#![allow(clippy::clone_on_copy)]

use super::*;
use crate::parser::testhelp::*;
use crate::tests::*;
use ahash::AHashSet;
use num::BigInt;
use std::fmt::Write;
use std::rc::Rc;
use test_case::test_case;

mod insn {
    use super::*;
    use test_case::test_case;

    #[test_case(Insn::Nop => "NOP"; "Nop instruction")]
    #[test_case(Insn::String => "STRING"; "String instruction")]
    #[test_case(Insn::Resolve => "RESOLVE"; "Resolve instruction")]
    #[test_case(Insn::StrictResolve => "STRICT_RESOLVE"; "StrictResolve instruction")]
    #[test_case(Insn::This => "THIS"; "This instruction")]
    #[test_case(Insn::Null => "NULL"; "Null instruction")]
    #[test_case(Insn::True => "TRUE"; "True instruction")]
    #[test_case(Insn::False => "FALSE"; "False instruction")]
    #[test_case(Insn::Float => "FLOAT"; "Float instruction")]
    #[test_case(Insn::Bigint => "BIGINT"; "Bigint instruction")]
    #[test_case(Insn::GetValue => "GET_VALUE"; "GetValue instruction")]
    #[test_case(Insn::JumpIfAbrupt => "JUMP_IF_ABRUPT"; "JumpIfAbrupt instruction")]
    #[test_case(Insn::JumpIfFalse => "JUMP_IF_FALSE"; "JumpIfFalse instruction")]
    #[test_case(Insn::JumpIfTrue => "JUMP_IF_TRUE"; "JumpIfTrue instruction")]
    #[test_case(Insn::JumpIfNotNullish => "JUMP_NOT_NULLISH"; "JumpIfNotNullish instruction")]
    #[test_case(Insn::JumpIfNullish => "JUMP_NULLISH"; "JumpIfNullish instruction")]
    #[test_case(Insn::UpdateEmpty => "UPDATE_EMPTY"; "UpdateEmpty instruction")]
    #[test_case(Insn::Undefined => "UNDEFINED"; "Undefined instruction")]
    #[test_case(Insn::Zero => "ZERO"; "Zero instruction")]
    #[test_case(Insn::Empty => "EMPTY"; "Empty instruction")]
    #[test_case(Insn::EmptyIfNotError => "EMPTY_IF_NOT_ERR"; "EmptyIfNotError instruction")]
    #[test_case(Insn::PutValue => "PUT_VALUE"; "PutValue instruction")]
    #[test_case(Insn::FunctionPrototype => "FUNC_PROTO"; "FunctionPrototype instruction")]
    #[test_case(Insn::Jump => "JUMP"; "Jump instruction")]
    #[test_case(Insn::JumpIfNormal => "JUMP_IF_NORMAL"; "JumpIfNormal instruction")]
    #[test_case(Insn::Call => "CALL"; "Call instruction")]
    #[test_case(Insn::StrictCall => "CALL_STRICT"; "StrictCall instruction")]
    #[test_case(Insn::Swap => "SWAP"; "Swap instruction")]
    #[test_case(Insn::Pop => "POP"; "Pop instruction")]
    #[test_case(Insn::PopOrPanic => "POP_PANIC"; "PopOrPanic instruction")]
    #[test_case(Insn::Pop2Push3 => "POP2_PUSH3"; "Pop2Push3 instruction")]
    #[test_case(Insn::Dup => "DUP"; "Dup instruction")]
    #[test_case(Insn::DupAfterList => "DUP_AFTER_LIST"; "DupAfterList instruction")]
    #[test_case(Insn::RotateUp => "ROTATEUP"; "RotateUp instruction")]
    #[test_case(Insn::RotateDown => "ROTATEDOWN"; "RotateDown instruction")]
    #[test_case(Insn::RotateDownList => "ROTATEDOWN_LIST"; "RotateDownList instruction")]
    #[test_case(Insn::Unwind => "UNWIND"; "Unwind instruction")]
    #[test_case(Insn::UnwindIfAbrupt => "UNWIND_IF_ABRUPT"; "UnwindIfAbrupt instruction")]
    #[test_case(Insn::Ref => "REF"; "Ref instruction")]
    #[test_case(Insn::StrictRef => "STRICT_REF"; "StrictRef instruction")]
    #[test_case(Insn::InitializeReferencedBinding => "IRB"; "InitializeReferencedBinding instruction")]
    #[test_case(Insn::Object => "OBJECT"; "Object instruction")]
    #[test_case(Insn::Array => "ARRAY"; "Array instruction")]
    #[test_case(Insn::CreateDataProperty => "CR_PROP"; "CreateDataProperty instruction")]
    #[test_case(Insn::SetPrototype => "SET_PROTO"; "SetPrototype instruction")]
    #[test_case(Insn::ToPropertyKey => "TO_KEY"; "ToPropertyKey instruction")]
    #[test_case(Insn::CopyDataProps => "COPY_DATA_PROPS"; "CopyDataProps instruction")]
    #[test_case(Insn::CopyDataPropsWithExclusions => "COPY_DATAPROPS_WE"; "CopyDataPropsWithExclusions instruction")]
    #[test_case(Insn::ToString => "TO_STRING"; "ToString instruction")]
    #[test_case(Insn::ToNumeric => "TO_NUMERIC"; "ToNumeric instruction")]
    #[test_case(Insn::Increment => "INCREMENT"; "Increment instruction")]
    #[test_case(Insn::Decrement => "DECREMENT"; "Decrement instruction")]
    #[test_case(Insn::PreDecrement => "PRE_DECREMENT"; "PreDecrement instruction")]
    #[test_case(Insn::PreIncrement => "PRE_INCREMENT"; "PreIncrement instruction")]
    #[test_case(Insn::Delete => "DELETE"; "Delete instruction")]
    #[test_case(Insn::Void => "VOID"; "Void instruction")]
    #[test_case(Insn::TypeOf => "TYPEOF"; "TypeOf instruction")]
    #[test_case(Insn::UnaryPlus => "UNARY_PLUS"; "UnaryPlus instruction")]
    #[test_case(Insn::UnaryMinus => "UNARY_MINUS"; "UnaryMinus instruction")]
    #[test_case(Insn::UnaryComplement => "UNARY_COMPLEMENT"; "UnaryComplement instruction")]
    #[test_case(Insn::UnaryNot => "UNARY_NOT"; "UnaryNot instruction")]
    #[test_case(Insn::Exponentiate => "EXPONENTIATE"; "Exponentiate instruction")]
    #[test_case(Insn::Multiply => "MULTIPLY"; "Multiply instruction")]
    #[test_case(Insn::Divide => "DIVIDE"; "Divide instruction")]
    #[test_case(Insn::Modulo => "MODULO"; "Modulo instruction")]
    #[test_case(Insn::Add => "ADD"; "Add instruction")]
    #[test_case(Insn::Subtract => "SUBTRACT"; "Subtract instruction")]
    #[test_case(Insn::LeftShift => "LSH"; "LeftShift instruction")]
    #[test_case(Insn::UnsignedRightShift => "URSH"; "UnsignedRightShift instruction")]
    #[test_case(Insn::SignedRightShift => "SRSH"; "SignedRightShift instruction")]
    #[test_case(Insn::Throw => "THROW"; "Throw instruction")]
    #[test_case(Insn::Less => "LT"; "Less instruction")]
    #[test_case(Insn::Greater => "GT"; "Greater instruction")]
    #[test_case(Insn::LessEqual => "LE"; "LessEqual instruction")]
    #[test_case(Insn::GreaterEqual => "GE"; "GreaterEqual instruction")]
    #[test_case(Insn::InstanceOf => "INSTANCEOF"; "InstanceOf instruction")]
    #[test_case(Insn::In => "IN"; "In instruction")]
    #[test_case(Insn::Equal => "EQ"; "Equal instruction")]
    #[test_case(Insn::NotEqual => "NE"; "NotEqual instruction")]
    #[test_case(Insn::StrictEqual => "SEQ"; "StrictEqual instruction")]
    #[test_case(Insn::StrictNotEqual => "SNE"; "StrictNotEqual instruction")]
    #[test_case(Insn::BitwiseAnd => "AND"; "BitwiseAnd instruction")]
    #[test_case(Insn::BitwiseOr => "OR"; "BitwiseOr instruction")]
    #[test_case(Insn::BitwiseXor => "XOR"; "BitwiseXor instruction")]
    #[test_case(Insn::PushNewLexEnv => "PNLE"; "PushNewLexEnv instruction")]
    #[test_case(Insn::PopLexEnv => "PLE"; "PopLexEnv instruction")]
    #[test_case(Insn::CreateStrictImmutableLexBinding => "CSILB"; "CreateStrictImmutableLexBinding instruction")]
    #[test_case(Insn::CreatePermanentMutableLexBinding => "CPMLB"; "CreatePermanentMutableLexBinding instruction")]
    #[test_case(Insn::InitializeLexBinding => "ILB"; "InitializeLexBinding instruction")]
    #[test_case(Insn::CreatePerIterationEnvironment => "CPIE"; "CreatePerIterationEnvironment instruction")]
    #[test_case(Insn::JumpPopIfTrue => "JUMPPOP_TRUE"; "JumpPopIfTrue instruction")]
    #[test_case(Insn::JumpPopIfFalse => "JUMPPOP_FALSE"; "JumpPopIfFalse instruction")]
    #[test_case(Insn::HandleEmptyBreak => "HEB"; "HandleEmptyBreak instruction")]
    #[test_case(Insn::HandleTargetedBreak => "HTB"; "HandleTargetedBreak instruction")]
    #[test_case(Insn::CoalesceValue => "COALESCE"; "CoalesceValue instruction")]
    #[test_case(Insn::LoopContinues => "LOOP_CONT"; "LoopContinues instruction")]
    #[test_case(Insn::Continue => "CONTINUE"; "Continue instruction")]
    #[test_case(Insn::TargetedContinue => "CONTINUE_WITH"; "TargetedContinue instruction")]
    #[test_case(Insn::Break => "BREAK"; "Break instruction")]
    #[test_case(Insn::TargetedBreak => "BREAK_FROM"; "TargetedBreak instruction")]
    #[test_case(Insn::CreateUnmappedArguments => "CUA"; "CreateUnmappedArguments instruction")]
    #[test_case(Insn::CreateMappedArguments => "CMA"; "CreateMappedArguments instruction")]
    #[test_case(Insn::AddMappedArgument => "AMA"; "AddMappedArgument instruction")]
    #[test_case(Insn::ToDo => "TODO"; "ToDo instruction")]
    #[test_case(Insn::JumpIfNotUndef => "JUMP_NOT_UNDEF"; "JumpIfNotUndef instruction")]
    #[test_case(Insn::EndFunction => "END_FUNCTION"; "EndFunction instruction")]
    #[test_case(Insn::Return => "RETURN"; "Return instruction")]
    #[test_case(Insn::UnwindList => "UNWIND_LIST"; "UnwindList instruction")]
    #[test_case(Insn::AppendList => "APPEND_LIST"; "AppendList instruction")]
    #[test_case(Insn::SwapDeepList => "SWAP_DEEP_LIST"; "SwapDeepList instruction")]
    #[test_case(Insn::PopOutList => "POP_OUT_LIST"; "PopOutList instruction")]
    #[test_case(Insn::PushNewVarEnvFromLex => "PNVEFL"; "PushNewVarEnvFromLex instruction")]
    #[test_case(Insn::PushNewLexEnvFromVar => "PNLEFV"; "PushNewLexEnvFromVar instruction")]
    #[test_case(Insn::SetLexEnvToVarEnv => "SLETVE"; "SetLexEnvToVarEnv instruction")]
    #[test_case(Insn::CreateNonStrictImmutableLexBinding => "CNSILB"; "CreateNonStrictImmutableLexBinding instruction")]
    #[test_case(Insn::CreateInitializedPermanentMutableLexIfMissing => "CIPMLBM"; "CreateInitializedPermanentMutableLexIfMissing instruction")]
    #[test_case(Insn::CreatePermanentMutableLexIfMissing => "CPMLBM"; "CreatePermanentMutableLexIfMissing instruction")]
    #[test_case(Insn::CreatePermanentMutableVarBinding => "CPMVB"; "CreatePermanentMutableVarBinding instruction")]
    #[test_case(Insn::GetLexBinding => "GLB"; "GetLexBinding instruction")]
    #[test_case(Insn::InitializeVarBinding => "IVB"; "InitializeVarBinding instruction")]
    #[test_case(Insn::SetMutableVarBinding => "SMVB"; "SetMutableVarBinding instruction")]
    #[test_case(Insn::InstantiateIdFreeFunctionExpression => "FUNC_IIFE"; "InstantiateIdFreeFunctionExpression instruction")]
    #[test_case(Insn::InstantiateOrdinaryFunctionExpression => "FUNC_IOFE"; "InstantiateOrdinaryFunctionExpression instruction")]
    #[test_case(Insn::InstantiateArrowFunctionExpression => "FUNC_IAE"; "InstantiateArrowFunctionExpression instruction")]
    #[test_case(Insn::InstantiateOrdinaryFunctionObject => "FUNC_OBJ"; "InstantiateOrdinaryFunctionObject instruction")]
    #[test_case(Insn::ExtractArg => "EXTRACT_ARG"; "ExtractArg instruction")]
    #[test_case(Insn::FinishArgs => "FINISH_ARGS"; "FinishArgs instruction")]
    #[test_case(Insn::ExtractThrownValue => "EXTRACT_THROW"; "ExtractThrownValue instruction")]
    #[test_case(Insn::SwapList => "SWAP_LIST"; "SwapList instruction")]
    #[test_case(Insn::PopList => "POP_LIST"; "PopList instruction")]
    #[test_case(Insn::RequireConstructor => "REQ_CSTR"; "RequireConstructor instruction")]
    #[test_case(Insn::Construct => "CONSTRUCT"; "Construct instruction")]
    #[test_case(Insn::JumpNotThrow => "JUMP_NOT_THROW"; "JumpNotThrow instruction")]
    #[test_case(Insn::IteratorAccumulate => "ITERATOR_ACCUM"; "IteratorAccumulate instruction")]
    #[test_case(Insn::IterateArguments => "ITER_ARGS"; "IterateArguments instruction")]
    #[test_case(Insn::RequireCoercible => "REQ_COER"; "RequireCoercible instruction")]
    #[test_case(Insn::GetSyncIterator => "GET_SYNC_ITER"; "GetSyncIterator instruction")]
    #[test_case(Insn::IteratorCloseIfNotDone => "ITER_CLOSE_IF_NOT_DONE"; "IteratorCloseIfNotDone instruction")]
    #[test_case(Insn::GetV => "GETV"; "GetV instruction")]
    #[test_case(Insn::IteratorDAEElision => "IDAE_ELISION"; "IteratorDAEElision instruction")]
    #[test_case(Insn::EmbellishedIteratorStep => "ITER_STEP"; "EmbellishedIteratorStep instruction")]
    #[test_case(Insn::IteratorRest => "ITER_REST"; "IteratorRest instruction")]
    #[test_case(Insn::ToObject => "TO_OBJECT"; "ToObject instruction")]
    #[test_case(Insn::IteratorClose => "ITER_CLOSE"; "IteratorClose instruction")]
    #[test_case(Insn::IteratorNext => "ITER_NEXT"; "IteratorNext instruction")]
    #[test_case(Insn::IteratorResultComplete => "IRES_COMPLETE"; "IteratorResultComplete instruction")]
    #[test_case(Insn::IteratorResultToValue => "IRES_TOVAL"; "IteratorResultToValue instruction")]
    #[test_case(Insn::EnumerateObjectProperties => "ENUM_PROPS"; "EnumerateObjectProperties instruction")]
    #[test_case(Insn::PrivateIdLookup => "PRIV_ID_LOOKUP"; "PrivateIdLookup instruction")]
    #[test_case(Insn::EvaluateInitializedClassFieldDefinition => "EVAL_CLASS_FIELD_DEF"; "EvaluateInitializedClassFieldDefinition instruction")]
    #[test_case(Insn::EvaluateClassStaticBlockDefinition => "EVAL_CLASS_SBLK_DEF"; "EvaluateClassStaticBlockDefinition instruction")]
    #[test_case(Insn::DefineMethod => "DEFINE_METHOD"; "DefineMethod instruction")]
    #[test_case(Insn::SetFunctionName => "SET_FUNC_NAME"; "SetFunctionName instruction")]
    #[test_case(Insn::DefineMethodProperty => "DEF_METH_PROP"; "DefineMethodProperty instruction")]
    #[test_case(Insn::DefineGetter => "DEF_GETTER"; "DefineGetter instruction")]
    fn display(insn: Insn) -> String {
        format!("{insn}")
    }

    #[test]
    fn debug() {
        assert_ne!(format!("{:?}", Insn::Bigint), "");
    }

    #[test_case(Insn::Null, Insn::Null => true; "equality")]
    #[test_case(Insn::Null, Insn::Float => false; "not equal")]
    fn eq(left: Insn, right: Insn) -> bool {
        left == right
    }

    #[test]
    fn u16_from() {
        let n1 = u16::from(Insn::False);
        let n2 = u16::from(Insn::String);
        assert_ne!(n1, n2);
    }

    #[test]
    fn try_from() {
        let insns = vec![Insn::String, Insn::Resolve, Insn::StrictResolve, Insn::UpdateEmpty];
        let ids = insns.iter().map(|&i| u16::from(i)).collect::<Vec<u16>>();
        let recovered = ids.iter().map(|&id| Insn::try_from(id).unwrap()).collect::<Vec<_>>();
        assert_eq!(insns, recovered);
    }

    #[test]
    #[allow(clippy::clone_on_copy)]
    fn clone() {
        let insn = Insn::String;
        let i2 = insn.clone();
        assert_eq!(insn, i2);
    }
}

mod compiler_status_flags {
    use super::*;
    use test_case::test_case;

    #[test]
    fn default() {
        let csf = CompilerStatusFlags::default();
        assert_eq!(csf.can_be_reference, RefResult::Never);
        assert_eq!(csf.can_be_abrupt, AbruptResult::Never);
    }

    #[test]
    fn debug() {
        assert_ne!(format!("{:?}", CompilerStatusFlags::new()), "");
    }

    #[test]
    #[allow(clippy::clone_on_copy)]
    fn clone() {
        let csf1 = CompilerStatusFlags { can_be_abrupt: AbruptResult::Never, can_be_reference: RefResult::Maybe };
        let csf2 = csf1.clone();
        assert_eq!(csf1.can_be_abrupt, csf2.can_be_abrupt);
        assert_eq!(csf1.can_be_reference, csf2.can_be_reference);
    }

    #[test]
    fn new() {
        let csf1 = CompilerStatusFlags::default();
        let csf2 = CompilerStatusFlags::new();
        assert_eq!(csf1.can_be_abrupt, csf2.can_be_abrupt);
        assert_eq!(csf1.can_be_reference, csf2.can_be_reference);
    }

    #[test_case(true => true; "abrupt")]
    #[test_case(false => false; "not abrupt")]
    fn abrupt(a: bool) -> bool {
        let csf = CompilerStatusFlags::new().abrupt(a);
        csf.maybe_abrupt()
    }

    #[test_case(true => true; "maybe a ref")]
    #[test_case(false => false; "never a ref")]
    fn reference(r: bool) -> bool {
        let csf = CompilerStatusFlags::new().reference(r);
        csf.maybe_ref()
    }

    #[test_case(AlwaysAbruptResult{} => CompilerStatusFlags { can_be_abrupt: AbruptResult::Maybe, can_be_reference: RefResult::Never }; "AlwaysAbruptResult")]
    #[test_case(NeverAbruptRefResult{} => CompilerStatusFlags { can_be_abrupt: AbruptResult::Never, can_be_reference: RefResult::Never }; "NeverAbruptRefResult")]
    #[test_case(AbruptResult::Maybe => CompilerStatusFlags { can_be_abrupt: AbruptResult::Maybe, can_be_reference: RefResult::Never }; "AbruptResult::Maybe")]
    #[test_case(AbruptResult::Never => CompilerStatusFlags { can_be_abrupt: AbruptResult::Never, can_be_reference: RefResult::Never }; "AbruptResult::Never")]
    #[test_case(AlwaysAbruptRefResult{} => CompilerStatusFlags { can_be_abrupt: AbruptResult::Maybe, can_be_reference: RefResult::Maybe }; "AlwaysAbruptRefResult")]
    #[test_case(AlwaysRefResult{} => CompilerStatusFlags { can_be_abrupt: AbruptResult::Never, can_be_reference: RefResult::Maybe }; "AlwaysRefResult")]
    #[test_case(RefResult::Maybe => CompilerStatusFlags { can_be_abrupt: AbruptResult::Never, can_be_reference: RefResult::Maybe }; "RefResult::Maybe")]
    #[test_case(RefResult::Never => CompilerStatusFlags { can_be_abrupt: AbruptResult::Never, can_be_reference: RefResult::Never }; "RefResult::Never")]
    fn from(item: impl Into<CompilerStatusFlags>) -> CompilerStatusFlags {
        item.into()
    }

    #[test_case(CompilerStatusFlags { can_be_abrupt: AbruptResult::Maybe, can_be_reference: RefResult::Never },
        CompilerStatusFlags { can_be_abrupt: AbruptResult::Never, can_be_reference: RefResult::Maybe } => false; "ne")]
    #[test_case(CompilerStatusFlags { can_be_abrupt: AbruptResult::Maybe, can_be_reference: RefResult::Never },
        CompilerStatusFlags { can_be_abrupt: AbruptResult::Maybe, can_be_reference: RefResult::Never } => true; "eq")]
    fn eq(left: CompilerStatusFlags, right: CompilerStatusFlags) -> bool {
        left == right
    }

    #[test_case(CompilerStatusFlags { can_be_abrupt: AbruptResult::Maybe, can_be_reference: RefResult::Never },
        CompilerStatusFlags { can_be_abrupt: AbruptResult::Never, can_be_reference: RefResult::Maybe } => true; "ne")]
    #[test_case(CompilerStatusFlags { can_be_abrupt: AbruptResult::Maybe, can_be_reference: RefResult::Never },
        CompilerStatusFlags { can_be_abrupt: AbruptResult::Maybe, can_be_reference: RefResult::Never } => false; "eq")]
    fn ne(left: CompilerStatusFlags, right: CompilerStatusFlags) -> bool {
        left != right
    }
}

mod ref_result {
    use super::*;
    use test_case::test_case;

    #[test]
    fn default() {
        let result = RefResult::default();
        assert!(matches!(result, RefResult::Never));
    }

    #[test_case(true => RefResult::Maybe; "maybe")]
    #[test_case(false => RefResult::Never; "never")]
    fn from_bool(src: bool) -> RefResult {
        RefResult::from(src)
    }

    #[test]
    fn debug() {
        assert_ne!(format!("{:?}", RefResult::Maybe), "");
    }

    #[test_case(RefResult::Maybe, RefResult::Maybe => true; "eq")]
    #[test_case(RefResult::Never, RefResult::Maybe => false; "ne")]
    fn eq(left: RefResult, right: RefResult) -> bool {
        left == right
    }

    #[test]
    fn clone() {
        let rr1 = RefResult::Maybe;
        let rr2 = rr1.clone();

        assert_eq!(rr1, rr2);
    }
}

mod abrupt_result {
    use super::*;
    use test_case::test_case;

    #[test]
    fn debug() {
        assert_ne!(format!("{:?}", AbruptResult::Maybe), "");
    }

    #[test_case(AbruptResult::Maybe, AbruptResult::Maybe => true; "eq")]
    #[test_case(AbruptResult::Maybe, AbruptResult::Never => false; "ne")]
    fn eq(left: AbruptResult, right: AbruptResult) -> bool {
        left == right
    }

    #[test]
    fn clone() {
        let ar1 = AbruptResult::Maybe;
        let ar2 = ar1.clone();

        assert_eq!(ar1, ar2);
    }

    #[test]
    fn default() {
        assert_eq!(AbruptResult::default(), AbruptResult::Never);
    }

    #[test_case(AbruptResult::Maybe => true; "maybe")]
    #[test_case(AbruptResult::Never => false; "never")]
    fn maybe_abrupt(item: AbruptResult) -> bool {
        item.maybe_abrupt()
    }

    #[test_case(AbruptResult::Maybe => false; "maybe")]
    #[test_case(AbruptResult::Never => false; "never")]
    fn maybe_ref(item: AbruptResult) -> bool {
        item.maybe_ref()
    }

    #[test_case(NeverAbruptRefResult{} => AbruptResult::Never; "NeverAbruptRefResult")]
    #[test_case(true => AbruptResult::Maybe; "true val")]
    #[test_case(false => AbruptResult::Never; "false val")]
    #[test_case(AlwaysAbruptResult{} => AbruptResult::Maybe; "AlwaysAbruptResult")]
    #[test_case(CompilerStatusFlags::new() => AbruptResult::Never; "CSF; not abrupt")]
    #[test_case(CompilerStatusFlags::new().abrupt(true) => AbruptResult::Maybe; "CSF; maybe abrupt")]
    fn from(item: impl Into<AbruptResult>) -> AbruptResult {
        item.into()
    }
}

mod always_abrupt_result {
    use super::*;

    #[test]
    fn debug() {
        assert_ne!(format!("{:?}", AlwaysAbruptResult {}), "");
    }

    #[test]
    #[allow(clippy::clone_on_copy)]
    fn clone() {
        let item = AlwaysAbruptResult {};
        let cloned = item.clone();
        assert!(matches!(cloned, AlwaysAbruptResult {}));
    }

    #[test]
    fn maybe_ref() {
        let item = AlwaysAbruptResult {};
        assert!(!item.maybe_ref());
    }

    #[test]
    fn maybe_abrupt() {
        let item = AlwaysAbruptResult {};
        assert!(item.maybe_abrupt());
    }
}

mod always_ref_result {
    use super::*;

    #[test]
    fn debug() {
        assert_ne!(format!("{:?}", AlwaysRefResult {}), "");
    }

    #[test]
    #[allow(clippy::clone_on_copy)]
    fn clone() {
        let item = AlwaysRefResult {};
        let cloned = item.clone();
        assert!(matches!(cloned, AlwaysRefResult {}));
    }
}

mod always_abrupt_ref_result {
    use super::*;

    #[test]
    fn debug() {
        assert_ne!(format!("{:?}", AlwaysAbruptRefResult {}), "");
    }

    #[test]
    #[allow(clippy::clone_on_copy)]
    fn clone() {
        let item = AlwaysAbruptRefResult {};
        let cloned = item.clone();
        assert!(matches!(cloned, AlwaysAbruptRefResult {}));
    }
}

mod never_abrupt_ref_result {
    use super::*;

    #[test]
    fn debug() {
        assert_ne!(format!("{:?}", NeverAbruptRefResult {}), "");
    }

    #[test]
    fn maybe_ref() {
        let item = NeverAbruptRefResult {};
        assert!(!item.maybe_ref());
    }

    #[test]
    fn maybe_abrupt() {
        let item = NeverAbruptRefResult {};
        assert!(!item.maybe_abrupt());
    }

    #[test]
    #[allow(clippy::clone_on_copy)]
    fn clone() {
        let item = NeverAbruptRefResult {};
        let cloned = item.clone();
        assert!(matches!(cloned, NeverAbruptRefResult {}));
    }
}

mod nameable_production {
    use super::*;
    use test_case::test_case;

    #[test]
    fn debug() {
        let fd = Maker::new("function (){}").function_expression();
        let x = NameableProduction::Function(fd);
        assert_ne!(format!("{x:?}"), "");
    }

    #[test_case(&NameableProduction::Function(Maker::new("function(){}").function_expression()) => "function ( ) { }"; "Function")]
    #[test_case(&NameableProduction::Generator(Maker::new("function*(){}").generator_expression()) => "function * ( ) { }"; "Generator")]
    #[test_case(&NameableProduction::AsyncFunction(Maker::new("async function(){}").async_function_expression()) => "async function ( ) { }"; "AsyncFunction")]
    #[test_case(&NameableProduction::AsyncGenerator(Maker::new("async function*(){}").async_generator_expression()) => "async function * ( ) { }"; "AsyncGenerator")]
    #[test_case(&NameableProduction::Class(Maker::new("class{}").class_expression()) => "class { }"; "Class")]
    #[test_case(&NameableProduction::Arrow(Maker::new("x=>x").arrow_function()) => "x => x"; "Arrow")]
    #[test_case(&NameableProduction::AsyncArrow(Maker::new("async x=>x").async_arrow_function()) => "async x => x"; "AsyncArrow")]
    fn display(node: &NameableProduction) -> String {
        node.to_string()
    }

    #[test_case(Maker::new("=3").initializer() => serr("Production not nameable"); "Initializer, not nameable")]
    #[test_case(Maker::new("=function(){}").initializer() => sok("function ( ) { }"); "Initializer: nameable")]
    #[test_case(Maker::new("3").assignment_expression() => serr("Production not nameable"); "AssignmentExpression::FallThru not namable")]
    #[test_case(Maker::new("function (){}").assignment_expression() => sok("function ( ) { }"); "AssignmentExpression::FallThru nameable")]
    #[test_case(Maker::new("a=>a").assignment_expression() => sok("a => a"); "AssignmentExpression::ArrowFunction")]
    #[test_case(Maker::new("async a=>a").assignment_expression() => sok("async a => a"); "AssignmentExpression::AsyncArrowHead")]
    #[test_case(Maker::new("yield function(){}").assignment_expression() => serr("Production not nameable"); "AssignmentExpression::YieldExpression")]
    #[test_case(Maker::new("a = function(){}").assignment_expression() => serr("Production not nameable"); "AssignmentExpression::Assignment")]
    #[test_case(Maker::new("a += function(){}").assignment_expression() => serr("Production not nameable"); "AssignmentExpression::OpAssignment")]
    #[test_case(Maker::new("a &&= function(){}").assignment_expression() => serr("Production not nameable"); "AssignmentExpression::Land Assignment")]
    #[test_case(Maker::new("a ||= function(){}").assignment_expression() => serr("Production not nameable"); "AssignmentExpression::Lor Assignment")]
    #[test_case(Maker::new("a ??= function(){}").assignment_expression() => serr("Production not nameable"); "AssignmentExpression::Coalesce Assignment")]
    #[test_case(Maker::new("{a} = function(){}").assignment_expression() => serr("Production not nameable"); "AssignmentExpression::Destructuring Assignment")]
    #[test_case(Maker::new("3").conditional_expression() => serr("Production not nameable"); "ConditionalExpression::FallThru not namable")]
    #[test_case(Maker::new("function (){}").conditional_expression() => sok("function ( ) { }"); "ConditionalExpression::FallThru nameable")]
    #[test_case(Maker::new("function () {} ? a => a : a => 2*a").conditional_expression() => serr("Production not nameable"); "ConditionalExpression::Conditional")]
    #[test_case(Maker::new("3").short_circuit_expression() => serr("Production not nameable"); "ShortCircuitExpression::FallThru not namable")]
    #[test_case(Maker::new("function (){}").short_circuit_expression() => sok("function ( ) { }"); "ShortCircuitExpression::FallThru nameable")]
    #[test_case(Maker::new("function () {} ?? a => a").short_circuit_expression() => serr("Production not nameable"); "ShortCircuitExpression::Coalesce")]
    #[test_case(Maker::new("function() {} || x => x").logical_or_expression() => serr("Production not nameable"); "Logical Or Expression::Or")]
    #[test_case(Maker::new("function() {} && x => x").logical_and_expression() => serr("Production not nameable"); "Logical Or Expression::And")]
    #[test_case(Maker::new("function() {} | x => x").bitwise_or_expression() => serr("Production not nameable"); "Bitwise Or Expression: Or")]
    #[test_case(Maker::new("function() {} ^ x => x").bitwise_xor_expression() => serr("Production not nameable"); "Bitwise Xor Expression: Xor")]
    #[test_case(Maker::new("function() {} & x => x").bitwise_and_expression() => serr("Production not nameable"); "Bitwise And Expression: And")]
    #[test_case(Maker::new("function() {} == a").equality_expression() => serr("Production not nameable"); "Equality Expression: Eq")]
    #[test_case(Maker::new("function() {} != a").equality_expression() => serr("Production not nameable"); "Equality Expression: Ne")]
    #[test_case(Maker::new("function() {} === a").equality_expression() => serr("Production not nameable"); "Equality Expression: Seq")]
    #[test_case(Maker::new("function() {} !== a").equality_expression() => serr("Production not nameable"); "Equality Expression: Sne")]
    #[test_case(Maker::new("function(){} < 10").relational_expression() => serr("Production not nameable"); "Relational Expression: Less")]
    #[test_case(Maker::new("function(){} << 10").shift_expression() => serr("Production not nameable"); "Shift Expression: Left")]
    #[test_case(Maker::new("function(){} + 10").additive_expression() => serr("Production not nameable"); "Additive Expression: add")]
    #[test_case(Maker::new("function(){} * 10").multiplicative_expression() => serr("Production not nameable"); "Multiplicative Expression: multiply")]
    #[test_case(Maker::new("function(){} ** 10").exponentiation_expression() => serr("Production not nameable"); "Exponentiation Expressison")]
    #[test_case(Maker::new("function(){}").expression() => sok("function ( ) { }"); "expression fallthru")]
    #[test_case(Maker::new("function(){}, 10").expression() => serr("Production not nameable"); "comma expression")]
    #[test_case(Maker::new("(function(){})").parenthesized_expression() => sok("function ( ) { }"); "parenthesized")]
    #[test_case(Maker::new("new function(){}").new_expression() => serr("Production not nameable"); "new expr")]
    #[test_case(Maker::new("function() {} ++").update_expression() => serr("Production not nameable"); "post-increment")]
    #[test_case(Maker::new("-function() {}").unary_expression() => serr("Production not nameable"); "negate")]
    #[test_case(Maker::new("function(){}()").left_hand_side_expression() => serr("Production not nameable"); "call expression")]
    #[test_case(Maker::new("function(){}.name").member_expression() => serr("Production not nameable"); "member expression")]
    #[test_case(Maker::new("function(){}").primary_expression() => sok("function ( ) { }"); "primary function expr")]
    #[test_case(Maker::new("class {}").primary_expression() => sok("class { }"); "primary class expr")]
    #[test_case(Maker::new("function *(){}").primary_expression() => sok("function * ( ) { }"); "primary gen expr")]
    #[test_case(Maker::new("async function (){}").primary_expression() => sok("async function ( ) { }"); "primary async fun expr")]
    #[test_case(Maker::new("async function *(){}").primary_expression() => sok("async function * ( ) { }"); "primary async gen expr")]
    #[test_case(Maker::new("(3)").primary_expression() => serr("Production not nameable"); "primary parenthesized expr")]
    fn try_from(x: impl TryInto<NameableProduction, Error = anyhow::Error>) -> Result<String, String> {
        x.try_into().map_err(|err| err.to_string()).map(|node| node.to_string())
    }

    #[test_case("function(){}", true => Ok((svec(&["STRING 0 (my_function_name)", "FUNC_IIFE 0"]), true, false)); "function expression")]
    #[test_case("function *(){}", true => panics "not yet implemented"; "generator exprsesion")]
    #[test_case("async function(){}", true => panics "not yet implemented"; "async function expression")]
    #[test_case("async function*(){}", true => panics "not yet implemented"; "async generator expression")]
    #[test_case("class {}", true => panics "not yet implemented"; "class expression")]
    #[test_case("(x => x)", true => Ok((svec(&["STRING 0 (my_function_name)", "FUNC_IAE 0"]), true, false)); "arrow function")]
    #[test_case("(async x => x)", true => panics "not yet implemented"; "async arrow function")]
    fn compile_named_evaluation(src: &str, strict: bool) -> Result<(Vec<String>, bool, bool), String> {
        let node = NameableProduction::try_from(Maker::new(src).primary_expression()).unwrap();
        let mut c = Chunk::new("x");
        let id = c.add_to_string_pool("my_function_name".into()).unwrap();
        node.compile_named_evaluation(&mut c, strict, src, NameLoc::Index(id))
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }

    #[test_case("function (){}" => false; "function, unnamed")]
    #[test_case("function foo(){}" => true; "function, named")]
    #[test_case("(x => x)" => false; "arrow")]
    #[test_case("(async x => 0)" => false; "async arrow")]
    #[test_case("function *(){}" => false; "generator, unnamed")]
    #[test_case("function *foo(){}" => true; "generator, named")]
    #[test_case("async function (){}" => false; "async fn, unnamed")]
    #[test_case("async function foo (){}" => true; "async fn, named")]
    #[test_case("async function *(){}" => false; "async gen, unnamed")]
    #[test_case("async function *foo(){}" => true; "async gen, named")]
    #[test_case("class {}" => false; "class, unnamed")]
    #[test_case("class foo {}" => true; "class, named")]
    fn is_named_function(src: &str) -> bool {
        let node = NameableProduction::try_from(Maker::new(src).primary_expression()).unwrap();
        node.is_named_function()
    }

    #[test_case("function foo(a, b) { return 'I am groot'; }" => "Function: return 'I am groot' ;"; "function")]
    #[test_case("function *foo(a) { yield a; }" => "Generator: yield a ;"; "generator")]
    #[test_case("async function foo(x) { return x; }" => "AsyncFunction: return x ;"; "async function")]
    #[test_case("async function *foo(x) { yield x; }" => "AsyncGenerator: yield x ;"; "async generator")]
    #[test_case("x => x" => "ConciseBody: x"; "arrow function")]
    #[test_case("async x => x" => "AsyncConciseBody: x"; "async arrow")]
    #[test_case("class oops {}" => panics "Trying to get the body of a class"; "class")]
    fn body(src: &str) -> String {
        let node = NameableProduction::try_from(Maker::new(src).assignment_expression()).unwrap();
        let body = node.body();
        match body {
            BodySource::Function(node) => format!("Function: {node}"),
            BodySource::Generator(node) => format!("Generator: {node}"),
            BodySource::AsyncFunction(node) => format!("AsyncFunction: {node}"),
            BodySource::AsyncGenerator(node) => format!("AsyncGenerator: {node}"),
            BodySource::ConciseBody(node) => format!("ConciseBody: {node}"),
            BodySource::AsyncConciseBody(node) => format!("AsyncConciseBody: {node}"),
            BodySource::Initializer(node) => format!("Initializer: {node}"),
            BodySource::ClassStaticBlockBody(node) => format!("ClassStaticBlockBody: {node}"),
        }
    }

    #[test_case("function foo(a, b=20){}" => "FormalParameters: a , b = 20"; "function")]
    #[test_case("function *foo(a, b=20){}" => "FormalParameters: a , b = 20"; "generator")]
    #[test_case("async function foo(a, b=20){}" => "FormalParameters: a , b = 20"; "async function")]
    #[test_case("async function *foo(a, b=20){}" => "FormalParameters: a , b = 20"; "async generator")]
    #[test_case("class bob {}" => panics "Trying to get the parameters block from a class"; "class")]
    #[test_case("x => x" => "ArrowParameters: x"; "arrow")]
    #[test_case("async x => x" => "AsyncArrowBinding: x"; "async arrow")]
    fn params(src: &str) -> String {
        let node = NameableProduction::try_from(Maker::new(src).assignment_expression()).unwrap();
        let params = node.params();
        match params {
            ParamSource::FormalParameters(node) => format!("FormalParameters: {node}"),
            ParamSource::ArrowParameters(node) => format!("ArrowParameters: {node}"),
            ParamSource::AsyncArrowBinding(node) => format!("AsyncArrowBinding: {node}"),
            ParamSource::ArrowFormals(node) => format!("ArrowFormals: {node}"),
            ParamSource::UniqueFormalParameters(node) => format!("UniqueFormalParameters: {node}"),
            ParamSource::PropertySetParameterList(node) => format!("PropertySetParameterList: {node}"),
        }
    }
}

// A note about compile tests: These are really unit tests; they check that all the code paths are run, and that the
// opcodes output match what we wanted the opcodes to be. They do not test whether we chose the right opcodes to
// complete the requested task! Checking that really feels like a later stage integration task. (Both the compiler and
// the runtime executor need to be functioning. That's all probably based more or less on test-262.)

fn full_chunk(n: &str) -> Chunk {
    let mut c = Chunk::new(n);
    c.floats = vec![56_878_142.0; 65536];
    c.strings = Vec::with_capacity(65536);
    c.bigints = Vec::with_capacity(65536);
    for nbr in 0..65536 {
        c.strings.push(JSString::from(""));
        c.bigints.push(Rc::new(BigInt::from(nbr)));
    }
    c
}

fn almost_full_chunk(n: &str, slots_left: usize) -> Chunk {
    const LIMIT: usize = 65536;
    let mut c = Chunk::new(n);
    c.floats.resize(LIMIT - slots_left.min(LIMIT), 7_489_305.0);
    c.strings.resize(LIMIT - slots_left.min(LIMIT), JSString::from("filler"));
    c.bigints.resize(LIMIT - slots_left.min(LIMIT), Rc::new(BigInt::from(783)));
    let sample: AHashSet<JSString> = vec![JSString::from("jkalhoadf")].into_iter().collect();
    c.string_sets.resize(LIMIT - slots_left.min(LIMIT), sample);
    c
}

#[derive(Copy, Clone)]
enum Fillable {
    Float,
    String,
    BigInt,
    StringSet,
    FunctionStash,
}
fn complex_filled_chunk(name: &str, what: &[(Fillable, usize)]) -> Chunk {
    const LIMIT: usize = 65536;
    let mut c = Chunk::new(name);
    for &(section, slots_left) in what {
        match section {
            Fillable::Float => c.floats.resize(LIMIT - slots_left.min(LIMIT), 7_489_305.0),
            Fillable::String => c.strings.resize(LIMIT - slots_left.min(LIMIT), JSString::from("filler")),
            Fillable::BigInt => c.bigints.resize(LIMIT - slots_left.min(LIMIT), Rc::new(BigInt::from(783))),
            Fillable::StringSet => {
                let sample: AHashSet<JSString> = vec![JSString::from("jkalhoadf")].into_iter().collect();
                c.string_sets.resize(LIMIT - slots_left.min(LIMIT), sample);
            }
            Fillable::FunctionStash => {
                let src = "function (a, b) { return a + b; }";
                let func_def = Maker::new(src).function_declaration();
                let sample = StashedFunctionData {
                    source_text: src.to_string(),
                    params: func_def.params.clone().into(),
                    body: func_def.body.clone().into(),
                    to_compile: func_def.into(),
                    strict: true,
                    this_mode: ThisLexicality::NonLexicalThis,
                };
                c.function_object_data.resize(LIMIT - slots_left.min(LIMIT), sample);
            }
        }
    }
    c
}

mod identifier_reference {
    use super::*;

    mod compile {
        use super::*;
        use test_case::test_case;

        #[test_case("id", false => svec(&[
            "STRING 0 (id)",
            "RESOLVE"
        ]); "identifier, non-strict")]
        #[test_case("id", true => svec(&[
            "STRING 0 (id)",
            "STRICT_RESOLVE"
        ]); "identifier, strict")]
        #[test_case("yield", false => svec(&[
            "STRING 0 (yield)",
            "RESOLVE"
        ]); "yield, non-strict")]
        #[test_case("yield", true => svec(&[
            "STRING 0 (yield)",
            "STRICT_RESOLVE"
        ]); "yield, strict")]
        #[test_case("await", false => svec(&[
            "STRING 0 (await)",
            "RESOLVE"
        ]); "await, non-strict")]
        #[test_case("await", true => svec(&[
            "STRING 0 (await)",
            "STRICT_RESOLVE"
        ]); "await, strict")]
        fn normal(src: &str, strict: bool) -> Vec<String> {
            let node = Maker::new(src).yield_ok(false).await_ok(false).identifier_reference();
            let mut c = Chunk::new("identifier test");
            node.compile(&mut c, strict).unwrap();
            c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>()
        }

        #[test]
        fn error() {
            let node = Maker::new("id").identifier_reference();
            let mut c = full_chunk("name");
            let result = node.compile(&mut c, false).unwrap_err().to_string();
            assert_eq!(result, "Out of room for strings in this compilation unit");
        }
    }
}

mod primary_expression {
    use super::*;

    mod compile {
        use super::*;
        use test_case::test_case;

        #[test_case("id", true => svec(&[
            "STRING 0 (id)",
            "STRICT_RESOLVE"
        ]); "identifier ref, strict")]
        #[test_case("id", false => svec(&[
            "STRING 0 (id)",
            "RESOLVE"
        ]); "identifier ref, non strict")]
        #[test_case("this", true => svec(&[
            "THIS"
        ]); "this binding")]
        #[test_case("(id)", true => svec(&[
            "STRING 0 (id)",
            "STRICT_RESOLVE"
        ]); "strict parens")]
        #[test_case("(id)", false => svec(&[
            "STRING 0 (id)",
            "RESOLVE"
        ]); "non-strict parens")]
        #[test_case("true", true => svec(&[
            "TRUE"
        ]); "literal")]
        #[test_case("({})", true => svec(&["OBJECT"]); "object literal")]
        #[test_case("class {}", true => panics "not yet implemented"; "class expression")]
        #[test_case("[]", true => svec(&["ARRAY"]); "array literal")]
        #[test_case("``", true => svec(&["STRING 0 ()"]); "template literal")]
        #[test_case("function a(){}", true => svec(&["STRING 0 (a)", "FUNC_IOFE 0"]); "function expression")]
        #[test_case("function *(){}", true => panics "not yet implemented"; "generator expression")]
        #[test_case("async function (){}", true => panics "not yet implemented"; "async function expression")]
        #[test_case("async function *(){}", true => panics "not yet implemented"; "async generator expression")]
        #[test_case("/abcd/", true => panics "not yet implemented"; "regular expression")]
        fn normal(src: &str, strict: bool) -> Vec<String> {
            let node = Maker::new(src).primary_expression();
            let mut c = Chunk::new("pe");
            node.compile(&mut c, strict, src).unwrap();
            c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>()
        }
    }
}

mod literal {
    use super::*;

    mod compile {
        use super::*;
        use test_case::test_case;

        #[test_case("null" => svec(&["NULL"]); "null literal")]
        #[test_case("true" => svec(&["TRUE"]); "true literal")]
        #[test_case("false" => svec(&["FALSE"]); "false literal")]
        #[test_case("'apple'" => svec(&["STRING 0 (apple)"]); "string literal")]
        #[test_case("10" => svec(&["FLOAT 0 (10)"]); "number literal")]
        #[test_case("10n" => svec(&["BIGINT 0 (10)"]); "bigint literal")]
        fn normal(src: &str) -> Vec<String> {
            let node = Maker::new(src).literal();
            let mut c = Chunk::new("lit");
            node.compile(&mut c).unwrap();
            c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>()
        }

        #[test_case("'apple'" => "Out of room for strings in this compilation unit"; "string")]
        #[test_case("100" => "Out of room for floats in this compilation unit"; "float")]
        #[test_case("78907890n" => "Out of room for big ints in this compilation unit"; "bigint")]
        fn error(src: &str) -> String {
            let node = Maker::new(src).literal();
            let mut c = full_chunk("test");
            node.compile(&mut c).unwrap_err().to_string()
        }

        #[test]
        fn debug_lots_of_noops() {
            let node = Maker::new("@@@").literal();
            let mut c = Chunk::new("debug_lots_of_noops");
            node.compile(&mut c).unwrap();
            assert_eq!(c.opcodes.len(), 32769);
            assert_eq!(c.opcodes[0], Insn::Nop.into());
            for x in 1..32768 {
                assert_eq!(c.opcodes[x], c.opcodes[x - 1]);
            }
        }
        #[test]
        fn debug_many_noops() {
            let node = Maker::new("@@3").literal();
            let mut c = Chunk::new("debug_lots_of_noops");
            node.compile(&mut c).unwrap();
            assert_eq!(c.opcodes.len(), 32766);
            assert_eq!(c.opcodes[0], Insn::Nop.into());
            for x in 1..32765 {
                assert_eq!(c.opcodes[x], c.opcodes[x - 1]);
            }
        }

        #[test]
        fn filled_string_table() {
            let node = Maker::new("@@!").literal();
            let mut c = Chunk::new("filled_string_table");
            node.compile(&mut c).unwrap();
            // The point of this literal is to fill the string table -- such that the call to add a string to the table
            // will fail. So that's what we test.
            assert!(c.add_to_string_pool("test".into()).is_err());
        }

        #[test]
        fn filled_float_table() {
            let node = Maker::new("@@#").literal();
            let mut c = Chunk::new("filled_float_table");
            node.compile(&mut c).unwrap();
            assert!(c.add_to_float_pool(0.0).is_err());
        }

        #[test]
        fn filled_bigint_table() {
            let node = Maker::new("@@$").literal();
            let mut c = Chunk::new("filled_float_table");
            node.compile(&mut c).unwrap();
            assert!(c.add_to_bigint_pool(Rc::new(BigInt::from(882))).is_err());
        }

        #[test]
        fn mystery_debug() {
            let node = Maker::new("@@z").literal();
            let mut c = Chunk::new("mystery_debug");
            node.compile(&mut c).unwrap();
            assert!(c.opcodes.is_empty());
        }
    }
}

mod parenthesized_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("(id)", true => svec(&["STRING 0 (id)", "STRICT_RESOLVE"]); "strict")]
    #[test_case("(id)", false => svec(&["STRING 0 (id)", "RESOLVE"]); "non-strict")]
    fn compile(src: &str, strict: bool) -> Vec<String> {
        let node = Maker::new(src).parenthesized_expression();
        let mut c = Chunk::new("x");
        node.compile(&mut c, strict, src).unwrap();
        c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>()
    }
}

mod object_literal {
    use super::*;
    use test_case::test_case;

    #[test_case("{}", true => svec(&["OBJECT"]); "empty")]
    #[test_case("{a}", true => svec(&["OBJECT", "STRING 0 (a)", "STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "CR_PROP"]); "strict elements")]
    #[test_case("{a,}", false => svec(&["OBJECT", "STRING 0 (a)", "STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "CR_PROP"]); "non-strict elements")]
    fn compile(src: &str, strict: bool) -> Vec<String> {
        let node = Maker::new(src).object_literal();
        let mut c = Chunk::new("x");
        node.compile(&mut c, strict, src).unwrap();
        c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>()
    }
}

mod property_definition_list {
    use super::*;
    use test_case::test_case;

    #[test_case("a", true => Ok((svec(&[
        "STRING 0 (a)", "STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "CR_PROP"
    ]), true, false)); "one item, strict")]
    #[test_case("a", false => Ok((svec(&[
        "STRING 0 (a)", "STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "CR_PROP"
    ]), true, false)); "one item, non-strict")]
    #[test_case("b:1", false => Ok((svec(&[
        "STRING 0 (b)", "FLOAT 0 (1)", "CR_PROP"
    ]), false, false)); "one item, errorfree")]
    #[test_case("a,b", true => Ok((svec(&[
        "STRING 0 (a)", "STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "CR_PROP", "JUMP_IF_ABRUPT 13", "STRING 1 (b)", "STRING 1 (b)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "CR_PROP"
    ]), true, false)); "list, strict")]
    #[test_case("a,b", false => Ok((svec(&[
        "STRING 0 (a)", "STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "CR_PROP", "JUMP_IF_ABRUPT 13", "STRING 1 (b)", "STRING 1 (b)", "RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "CR_PROP"
    ]), true, false)); "list, non-strict")]
    #[test_case("a:1,b", false => Ok((svec(&[
        "STRING 0 (a)", "FLOAT 0 (1)", "CR_PROP", "STRING 1 (b)", "STRING 1 (b)", "RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "CR_PROP"
    ]), true, false)); "potential errors in first")]
    #[test_case("a,b:1", false => Ok((svec(&[
        "STRING 0 (a)", "STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "CR_PROP", "JUMP_IF_ABRUPT 5", "STRING 1 (b)", "FLOAT 0 (1)", "CR_PROP"
    ]), true, false)); "potential errors in second")]
    #[test_case("a:0,b:1", false => Ok((svec(&[
        "STRING 0 (a)", "FLOAT 0 (0)", "CR_PROP", "STRING 1 (b)", "FLOAT 1 (1)", "CR_PROP"
    ]), false, false)); "error free list")]
    #[test_case("a,b:@@@", false => serr("out of range integral type conversion attempted"); "jump fails")]
    #[test_case("[@@!]:0,a,b", false => serr("Out of room for strings in this compilation unit"); "filled string table")]
    fn compile(src: &str, strict: bool) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).property_definition_list();
        let mut c = Chunk::new("x");
        node.property_definition_evaluation(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod property_definition {
    use super::*;
    use test_case::test_case;

    #[test_case("a", true, &[] => Ok((svec(&[
        "STRING 0 (a)", "STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "CR_PROP"
    ]), true, false)); "id ref, strict")]
    #[test_case("a", false, &[] => Ok((svec(&[
        "STRING 0 (a)", "STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "CR_PROP"
    ]), true, false)); "id ref, non-strict")]
    #[test_case("a", false, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "first string has no room")]
    #[test_case("a=1", false, &[] => panics "unreachable"; "cover initialized name")]
    #[test_case("a:1", true, &[] => Ok((svec(&[
        "STRING 0 (a)", "FLOAT 0 (1)", "CR_PROP"
    ]), false, false)); "name:property; no possibility of error, strict")]
    #[test_case("[a]:1", true, &[] => Ok((svec(&[
        "STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 1", "TO_KEY", "JUMP_IF_NORMAL 4", "UNWIND 1", "JUMP 3", "FLOAT 0 (1)", "CR_PROP"
    ]), true, false)); "name:property; potential error in name, strict")]
    #[test_case("[a]:1", false, &[] => Ok((svec(&[
        "STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 1", "TO_KEY", "JUMP_IF_NORMAL 4", "UNWIND 1", "JUMP 3", "FLOAT 0 (1)", "CR_PROP"
    ]), true, false)); "name:property; potential error in name, non-strict")]
    #[test_case("[q]:33", false, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "pn compile errors out")]
    #[test_case("a: function () {}", true, &[] => Ok((svec(&[
        "STRING 0 (a)", "DUP", "FUNC_IIFE 0", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "CR_PROP"
    ]), true, false)); "anonymous function def")]
    #[test_case("a: function () {}", true, &[(Fillable::FunctionStash, 0)] => serr("Out of room for more functions!"); "function table full")]
    #[test_case("a:b", false, &[(Fillable::String, 1)] => serr("Out of room for strings in this compilation unit"); "ae compile errors out")]
    #[test_case("a:b", false, &[] => Ok((svec(&[
        "STRING 0 (a)", "STRING 1 (b)", "RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "CR_PROP"
    ]), true, false)); "name:value, ae can error; not-strict")]
    #[test_case("a:b", true, &[] => Ok((svec(&[
        "STRING 0 (a)", "STRING 1 (b)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "CR_PROP"
    ]), true, false)); "name:value, ae can error; strict")]
    #[test_case("[a]:@@@", true, &[] => serr("out of range integral type conversion attempted"); "jump too far")]
    #[test_case("__proto__:null", true, &[] => Ok((svec(&["NULL", "SET_PROTO"]), false, false)); "proto-setter")]
    #[test_case(
        "a(){}", true, &[]
        => Ok((
            svec(&[
                "DUP",
                "DUP",
                "FUNC_PROTO",
                "SWAP",
                "STRING 0 (a)",
                "DEFINE_METHOD 0",
                "JUMP_IF_ABRUPT 3",
                "SET_FUNC_NAME",
                "DEF_METH_PROP 1",
                "UNWIND_IF_ABRUPT 1",
                "JUMP_IF_ABRUPT 1",
                "POP",
                "UNWIND_IF_ABRUPT 1"
            ]),
            true,
            false
        ));
        "method def"
    )]
    #[test_case(
        "a(){}", true, &[(Fillable::FunctionStash, 0)]
        => serr("Out of room for more functions!");
        "method def fails"
    )]
    #[test_case("...a", true, &[] => Ok((svec(&[
        "STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 1", "JUMP 1", "COPY_DATA_PROPS"
    ]), true, false)); "rest object, strict")]
    #[test_case("...a", false, &[] => Ok((svec(&[
        "STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 1", "JUMP 1", "COPY_DATA_PROPS"
    ]), true, false)); "rest object, non-strict")]
    #[test_case("...a", false, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "rest object, ae errs")]
    #[test_case("...true", false, &[] => Ok((svec(&["TRUE", "COPY_DATA_PROPS"]), true, false)); "rest object, not reference")]
    fn compile(src: &str, strict: bool, what: &[(Fillable, usize)]) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).property_definition();
        let mut c = complex_filled_chunk("x", what);
        node.property_definition_evaluation(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod property_name {
    use super::*;
    use test_case::test_case;

    #[test_case("a", true, None => Ok((svec(&["STRING 0 (a)"]), false, false)); "literal property name")]
    #[test_case("[a]", true, None => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 1", "TO_KEY"]), true, false)); "computed property name; strict")]
    #[test_case("[a]", false, None => Ok((svec(&["STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 1", "TO_KEY"]), true, false)); "computed property name; non-strict")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).property_name();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }

    #[test_case("[a]" => false; "computed")]
    #[test_case("__proto__" => true; "literal, match")]
    #[test_case("a" => false; "literal, no match")]
    fn is_literal_proto(src: &str) -> bool {
        Maker::new(src).property_name().is_literal_proto()
    }
}

mod literal_property_name {
    use super::*;
    use test_case::test_case;

    #[test_case("__proto__" => true; "id proto")]
    #[test_case("a" => false; "id not")]
    #[test_case("'__proto__'" => true; "string proto")]
    #[test_case("'a'" => false; "string not")]
    #[test_case("1" => false; "number")]
    fn is_literal_proto(src: &str) -> bool {
        Maker::new(src).literal_property_name().is_literal_proto()
    }

    #[test_case("a", None => Ok((svec(&["STRING 0 (a)"]), false, false)); "id")]
    #[test_case("'a'", None => Ok((svec(&["STRING 0 (a)"]), false, false)); "string")]
    #[test_case("1", None => Ok((svec(&["STRING 0 (1)"]), false, false)); "number")]
    #[test_case("a", Some(0) => serr("Out of room for strings in this compilation unit"); "id; err")]
    #[test_case("'a'", Some(0) => serr("Out of room for strings in this compilation unit"); "string; err")]
    #[test_case("1", Some(0) => serr("Out of room for strings in this compilation unit"); "number; err")]
    fn compile(src: &str, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).literal_property_name();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod computed_property_name {
    use super::*;
    use test_case::test_case;

    #[test_case("[a]", true, None => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 1", "TO_KEY"]), true, false)); "reference expression; strict")]
    #[test_case("[a]", false, None => Ok((svec(&["STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 1", "TO_KEY"]), true, false)); "reference expression; non-strict")]
    #[test_case("[a]", true, Some(0) => serr("Out of room for strings in this compilation unit"); "err in expr")]
    #[test_case("[1]", true, None => Ok((svec(&["FLOAT 0 (1)", "TO_KEY"]), true, false)); "error-free expr")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).computed_property_name();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod member_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true, None => Ok((svec(&["STRING 0 (id)", "STRICT_RESOLVE"]), true, true)); "fall-thru strict")]
    #[test_case("id", false, None => Ok((svec(&["STRING 0 (id)", "RESOLVE"]), true, true)); "fall-thru non strict")]
    #[test_case("a.b", true, None => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "STRING 1 (b)",
        "STRICT_REF"
    ]), true, true)); "member property; strict")]
    #[test_case("a.b", false, None => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "STRING 1 (b)",
        "REF"
    ]), true, true)); "member property; non-strict")]
    #[test_case("a.b", true, Some(1) => serr("Out of room for strings in this compilation unit"); "no space for id")]
    #[test_case("a.b", true, Some(0) => serr("Out of room for strings in this compilation unit"); "no space for base")]
    #[test_case("(1).b", true, None => Ok((svec(&["FLOAT 0 (1)", "STRING 0 (b)", "STRICT_REF"]), false, true)); "error-free base")]
    #[test_case("a[b]", true, None => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 12",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 4",
        "TO_KEY",
        "JUMP_IF_ABRUPT 1",
        "STRICT_REF",
        "UNWIND_IF_ABRUPT 1"
    ]), true, true)); "strict member exp")]
    #[test_case(
        "a[b]", false, None
        => Ok((svec(&[
            "STRING 0 (a)",
            "RESOLVE",
            "GET_VALUE",
            "JUMP_IF_ABRUPT 12",
            "STRING 1 (b)",
            "RESOLVE",
            "GET_VALUE",
            "JUMP_IF_ABRUPT 4",
            "TO_KEY",
            "JUMP_IF_ABRUPT 1",
            "REF",
            "UNWIND_IF_ABRUPT 1"
        ]), true, true));
        "non-strict member exp"
    )]
    #[test_case("a[b]", true, Some(0) => serr("Out of room for strings in this compilation unit"); "no space for base (expression)")]
    #[test_case(
        "(1)[1]", true, None
        => Ok((svec(&[
            "FLOAT 0 (1)", "FLOAT 0 (1)", "TO_KEY", "JUMP_IF_ABRUPT 1", "STRICT_REF", "UNWIND_IF_ABRUPT 1"
        ]), true, true));
        "no errors in base (expression)"
    )]
    #[test_case("b[a]", true, Some(1) => serr("Out of room for strings in this compilation unit"); "no space for expression (expression)")]
    #[test_case("a[@@@]", true, None => serr("out of range integral type conversion attempted"); "bad jump (expression)")]
    #[test_case("a`${b}`", true, None => panics "not yet implemented"; "template")]
    #[test_case("super.a", true, None => panics "not yet implemented"; "super ref")]
    #[test_case("new.target", true, None => panics "not yet implemented"; "meta")]
    #[test_case("new a()", true, None => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 15",
        "DUP",
        "DUP",
        "FLOAT 0 (0)",
        "SWAP_LIST",
        "REQ_CSTR",
        "JUMP_IF_NORMAL 5",
        "UNWIND_LIST",
        "UNWIND 2",
        "JUMP 2",
        "POP",
        "CONSTRUCT"
    ]), true, false)); "new-args")]
    #[test_case("a.#pid", true, None => panics "not yet implemented"; "private")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).member_expression();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod new_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true => svec(&["STRING 0 (id)", "STRICT_RESOLVE"]); "fall-thru strict")]
    #[test_case("id", false => svec(&["STRING 0 (id)", "RESOLVE"]); "fall-thru non strict")]
    #[test_case("new a", true => svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 15",
        "DUP",
        "DUP",
        "FLOAT 0 (0)",
        "SWAP_LIST",
        "REQ_CSTR",
        "JUMP_IF_NORMAL 5",
        "UNWIND_LIST",
        "UNWIND 2",
        "JUMP 2",
        "POP",
        "CONSTRUCT"
    ]); "new exp")]
    fn compile(src: &str, strict: bool) -> Vec<String> {
        let node = Maker::new(src).new_expression();
        let mut c = Chunk::new("x");
        node.compile(&mut c, strict, src).unwrap();
        c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>()
    }
}

mod call_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("a()", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "DUP",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 3",
        "FLOAT 0 (0)",
        "CALL_STRICT"
    ]), true, false)); "call-expression; strict")]
    #[test_case("a()", false, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "DUP",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 3",
        "FLOAT 0 (0)",
        "CALL"
    ]), true, false)); "call-expression; non-strict")]
    #[test_case("super()", true, &[] => panics "not yet implemented"; "super call")]
    #[test_case("import(a)", true, &[] => panics "not yet implemented"; "import call")]
    #[test_case("a()()", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "DUP",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 3",
        "FLOAT 0 (0)",
        "CALL_STRICT",
        "DUP",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 5",
        "FLOAT 0 (0)",
        "CALL_STRICT",
        "JUMP 2",
        "UNWIND 1"
    ]), true, false)); "call-on-call")]
    #[test_case("a()()", false, &[(Fillable::String, 0)]
                => serr("Out of room for strings in this compilation unit")
                ; "call-expression-arguments: expression compile fail")]
    #[test_case("a()(1n)", false, &[(Fillable::BigInt, 0)]
                => serr("Out of room for big ints in this compilation unit")
                ; "call-expression-arguments: arguments compile fails")]
    #[test_case("a()(@@@)", false, &[]
                => serr("out of range integral type conversion attempted")
                ; "call-expression-arguments: unwind jump too far")]
    #[test_case("a()[b]", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "DUP",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 3",
        "FLOAT 0 (0)",
        "CALL_STRICT",
        "JUMP_IF_ABRUPT 12",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 4",
        "TO_KEY",
        "JUMP_IF_ABRUPT 1",
        "STRICT_REF",
        "UNWIND_IF_ABRUPT 1"
    ]), true, true)); "CallExpression: [ Expression ]")]
    #[test_case("a().b[c]", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "DUP",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 3",
        "FLOAT 0 (0)",
        "CALL_STRICT",
        "JUMP_IF_ABRUPT 3",
        "STRING 1 (b)",
        "STRICT_REF",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 12",
        "STRING 2 (c)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 4",
        "TO_KEY",
        "JUMP_IF_ABRUPT 1",
        "STRICT_REF",
        "UNWIND_IF_ABRUPT 1"
    ]), true, true)); "ce-exp; ce is ref")]
    #[test_case("a()[b]", true, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "ce-exp; ce fails compilation")]
    #[test_case("a()[8n]", true, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "ce-exp; exp fails compilation")]
    #[test_case("a().b", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "DUP",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 3",
        "FLOAT 0 (0)",
        "CALL_STRICT",
        "JUMP_IF_ABRUPT 3",
        "STRING 1 (b)",
        "STRICT_REF"
    ]), true, true)); "property-on-call")]
    #[test_case("a().b", true, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "ce-prop; ce fails compilation")]
    #[test_case("a().b.c", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "DUP",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 3",
        "FLOAT 0 (0)",
        "CALL_STRICT",
        "JUMP_IF_ABRUPT 3",
        "STRING 1 (b)",
        "STRICT_REF",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "STRING 2 (c)",
        "STRICT_REF"
    ]), true, true)); "ce-prop, ce is ref")]
    #[test_case("a().b", true, &[(Fillable::String, 1)] => serr("Out of room for strings in this compilation unit"); "ce-prop; id doesn't fit")]
    #[test_case("a()`${b}`", true, &[] => panics "not yet implemented"; "template-on-call")]
    #[test_case("a().#pid", true, &[] => panics "not yet implemented"; "private-on-call")]
    fn compile(src: &str, strict: bool, what: &[(Fillable, usize)]) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).call_expression();
        let mut c = complex_filled_chunk("x", what);
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod call_member_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("a()", true, None => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "DUP", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 1", "JUMP 3", "FLOAT 0 (0)", "CALL_STRICT"]), true, false)); "no args; strict")]
    #[test_case("a()", false, None => Ok((svec(&["STRING 0 (a)", "RESOLVE", "DUP", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 1", "JUMP 3", "FLOAT 0 (0)", "CALL"]), true, false)); "no args; non-strict")]
    #[test_case("a()", true, Some(0) => serr("Out of room for strings in this compilation unit"); "no space for ME")]
    #[test_case("(1)()", true, None => Ok((svec(&["FLOAT 0 (1)", "DUP", "FLOAT 1 (0)", "CALL_STRICT"]), true, false)); "me not reference")]
    #[test_case("a(b)", true, Some(1) => serr("Out of room for strings in this compilation unit"); "no space for args")]
    #[test_case("a(c)", true, None => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "DUP", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 1", "JUMP 15", "STRING 1 (c)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 2", "FLOAT 0 (1)", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "CALL_STRICT"]), true, false)); "args can error; strict")]
    #[test_case("a(c)", false, None => Ok((svec(&["STRING 0 (a)", "RESOLVE", "DUP", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 1", "JUMP 15", "STRING 1 (c)", "RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 2", "FLOAT 0 (1)", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "CALL"]), true, false)); "args can error; non-strict")]
    #[test_case("a(@@@)", true, None => serr("out of range integral type conversion attempted"); "bad jump (args too complex)")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).call_member_expression();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod left_hand_side_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true => svec(&["STRING 0 (id)", "STRICT_RESOLVE"]); "fall-thru strict")]
    #[test_case("id", false => svec(&["STRING 0 (id)", "RESOLVE"]); "fall-thru non strict")]
    #[test_case("a()", true => svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "DUP",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 3",
        "FLOAT 0 (0)",
        "CALL_STRICT"
    ]); "call strict")]
    #[test_case("a()", false => svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "DUP",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 3",
        "FLOAT 0 (0)",
        "CALL"
    ]); "call non-strict")]
    #[test_case("a?.b", true => svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 20",
        "DUP",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 14",
        "JUMP_NOT_NULLISH 5",
        "POP",
        "POP",
        "UNDEFINED",
        "JUMP 9",
        "UNWIND 1",
        "STRING 1 (b)",
        "STRICT_REF",
        "JUMP 2",
        "UNWIND 1"
    ]); "optional")]
    fn compile(src: &str, strict: bool) -> Vec<String> {
        let node = Maker::new(src).left_hand_side_expression();
        let mut c = Chunk::new("x");
        node.compile(&mut c, strict, src).unwrap();
        c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>()
    }
}

mod arguments {
    use super::*;
    use test_case::test_case;

    #[test_case("()", true, &[] => Ok((svec(&["FLOAT 0 (0)"]), false, false)); "empty")]
    #[test_case("()", true, &[(Fillable::Float, 0)] => serr("Out of room for floats in this compilation unit"); "empty; no space for args")]
    #[test_case("(1)", true, &[] => Ok((svec(&["FLOAT 0 (1)", "FLOAT 0 (1)"]), false, false)); "one arg; no errors")]
    #[test_case("(a)", true, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "no room for args")]
    #[test_case("(999)", true, &[(Fillable::Float, 1)] => serr("Out of room for floats in this compilation unit"); "no room for length")]
    #[test_case("(a,)", true, &[] => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 2", "FLOAT 0 (1)"]), true, false)); "error-able args; strict")]
    #[test_case("(a,)", false, &[] => Ok((svec(&["STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 2", "FLOAT 0 (1)"]), true, false)); "error-able args; non-strict")]
    #[test_case("(a,...b,c)", false, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 15",
        "FLOAT 0 (1)",
        "STRING 1 (b)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 6",
        "ITER_ARGS",
        "JUMP_IF_ABRUPT 3",
        "APPEND_LIST",
        "JUMP 1",
        "UNWIND_LIST",
        "JUMP_IF_ABRUPT 7",
        "STRING 2 (c)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 1",
        "UNWIND_LIST",
        "JUMP_IF_ABRUPT 5",
        "ROTATEUP 2",
        "FLOAT 0 (1)",
        "ADD"
    ]), true, false)); "with variable arg")]
    #[test_case("(true,true,true,true,true,true,...[],true)", false, &[(Fillable::Float, 1)] => serr("Out of room for floats in this compilation unit"); "varargs, out of float space")]
    #[test_case("(true,...[])", false, &[] => Ok((svec(&[
        "TRUE",
        "FLOAT 0 (1)",
        "ARRAY",
        "ITER_ARGS",
        "JUMP_IF_ABRUPT 3",
        "APPEND_LIST",
        "JUMP 1",
        "UNWIND_LIST"
    ]), true, false)); "no need to add more at end")]
    fn argument_list_evaluation(
        src: &str,
        strict: bool,
        what: &[(Fillable, usize)],
    ) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).arguments();
        let mut c = complex_filled_chunk("x", what);
        node.argument_list_evaluation(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod argument_list {
    use super::*;
    use test_case::test_case;

    #[test_case("a", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE"
        ]), 1, false, true, false)); "item/reference/strict")]
    #[test_case("a", false, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE"
        ]), 1, false, true, false)); "item/reference/non-strict")]
    #[test_case("true", true, &[] => Ok((svec(&[
        "TRUE"
        ]), 1, false, false, false)); "item/literal")]
    #[test_case("a", true, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "no room for item")]
    #[test_case("...a", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 1",
        "ITER_ARGS"
    ]), 0, true, true, false)); "...a -style object unpacking")]
    #[test_case("true, false", true, &[] => Ok((svec(&[
        "TRUE",
        "FALSE"
        ]), 2, false, false, false)); "list/noref")]
    #[test_case("a,b", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 8",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 2",
        "UNWIND 1"
        ]), 2, false, true, false)); "errable items, strict")]
    #[test_case("a,b", false, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 8",
        "STRING 1 (b)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 2",
        "UNWIND 1"
        ]), 2, false, true, false)); "errable items, non-strict")]
    #[test_case("a,b", true, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "no room for first list")]
    #[test_case("a,b", true, &[(Fillable::String, 1)] => serr("Out of room for strings in this compilation unit"); "no room for last item")]
    #[test_case("a,@@@", true, &[] => serr("out of range integral type conversion attempted"); "jump too far")]
    #[test_case("a,...b", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 15",
        "FLOAT 0 (1)",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 6",
        "ITER_ARGS",
        "JUMP_IF_ABRUPT 3",
        "APPEND_LIST",
        "JUMP 1",
        "UNWIND_LIST"
    ]), 0, true, true, false)); "list + rest")]
    #[test_case("...a,b", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 1",
        "ITER_ARGS",
        "JUMP_IF_ABRUPT 7",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 1",
        "UNWIND_LIST"
    ]), 1, true, true, false)); "spread + item")]
    #[test_case("...1", false, &[] => Ok((svec(&[
        "FLOAT 0 (1)",
        "ITER_ARGS"
    ]), 0, true, true, false)); "infallible spread")]
    #[test_case("...a", false, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "spread; ae compile fails")]
    #[test_case("a,...b", false, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "list-spread; list compile fails")]
    #[test_case("1,...b", false, &[] => Ok((svec(&[
        "FLOAT 0 (1)",
        "FLOAT 0 (1)",
        "STRING 0 (b)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 6",
        "ITER_ARGS",
        "JUMP_IF_ABRUPT 3",
        "APPEND_LIST",
        "JUMP 1",
        "UNWIND_LIST"
    ]), 0, true, true, false)); "list-spread; list infallible")]
    #[test_case("...a,56,...b", false, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 1",
        "ITER_ARGS",
        "JUMP_IF_ABRUPT 2",
        "FLOAT 0 (56)",
        "JUMP_IF_ABRUPT 18",
        "ROTATEUP 2",
        "FLOAT 1 (1)",
        "ADD",
        "STRING 1 (b)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 6",
        "ITER_ARGS",
        "JUMP_IF_ABRUPT 3",
        "APPEND_LIST",
        "JUMP 1",
        "UNWIND_LIST"
    ]), 0, true, true, false)); "list-spread; list is spread followed by direct")]
    #[test_case("...a,'a',...b", false, &[(Fillable::Float, 0)] => serr("Out of room for floats in this compilation unit"); "list-spread; no room for list touchup")]
    #[test_case("...a,...b", false, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 1",
        "ITER_ARGS",
        "JUMP_IF_ABRUPT 13",
        "STRING 1 (b)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 6",
        "ITER_ARGS",
        "JUMP_IF_ABRUPT 3",
        "APPEND_LIST",
        "JUMP 1",
        "UNWIND_LIST"
    ]), 0, true, true, false)); "list-spread; list is var with no direct")]
    #[test_case("'a',...b", false, &[(Fillable::Float, 0)] => serr("Out of room for floats in this compilation unit"); "list-spread, direct, no room for math")]
    #[test_case("1,...a", false, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "list-spread; spread compile fails")]
    #[test_case("1,...9", false, &[] =>  Ok((svec(&[
        "FLOAT 0 (1)",
        "FLOAT 0 (1)",
        "FLOAT 1 (9)",
        "ITER_ARGS",
        "JUMP_IF_ABRUPT 3",
        "APPEND_LIST",
        "JUMP 1",
        "UNWIND_LIST"
    ]), 0, true, true, false)); "list-spread; infallible")]
    #[test_case("...a,...@@@", false, &[] => serr("out of range integral type conversion attempted"); "list-spread; jump too far A")]
    fn argument_list_evaluation(
        src: &str,
        strict: bool,
        what: &[(Fillable, usize)],
    ) -> Result<(Vec<String>, u16, bool, bool, bool), String> {
        let node = Maker::new(src).argument_list();
        let mut c = complex_filled_chunk("x", what);
        node.argument_list_evaluation(&mut c, strict, src)
            .map(|(ArgListSizeHint { fixed_len: count, has_variable }, status)| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    count,
                    has_variable,
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod update_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true, None => Ok((svec(&["STRING 0 (id)", "STRICT_RESOLVE"]), true, true)); "fall-thru strict")]
    #[test_case("id", false, None => Ok((svec(&["STRING 0 (id)", "RESOLVE"]), true, true)); "fall-thru non strict")]
    #[test_case("a++", true, None => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "DUP",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "SWAP",
        "POP",
        "JUMP 11",
        "TO_NUMERIC",
        "JUMP_IF_NORMAL 4",
        "SWAP",
        "POP",
        "JUMP 4",
        "POP2_PUSH3",
        "INCREMENT",
        "PUT_VALUE",
        "UPDATE_EMPTY",
    ]), true, false)); "post-increment, strict")]
    #[test_case("a++", false, None => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "DUP",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "SWAP",
        "POP",
        "JUMP 11",
        "TO_NUMERIC",
        "JUMP_IF_NORMAL 4",
        "SWAP",
        "POP",
        "JUMP 4",
        "POP2_PUSH3",
        "INCREMENT",
        "PUT_VALUE",
        "UPDATE_EMPTY",
    ]), true, false)); "post-increment, non-strict")]
    #[test_case("a--", true, None => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "DUP",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "SWAP",
        "POP",
        "JUMP 11",
        "TO_NUMERIC",
        "JUMP_IF_NORMAL 4",
        "SWAP",
        "POP",
        "JUMP 4",
        "POP2_PUSH3",
        "DECREMENT",
        "PUT_VALUE",
        "UPDATE_EMPTY",
    ]), true, false)); "post-decrement, strict")]
    #[test_case("a--", false, None => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "DUP",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "SWAP",
        "POP",
        "JUMP 11",
        "TO_NUMERIC",
        "JUMP_IF_NORMAL 4",
        "SWAP",
        "POP",
        "JUMP 4",
        "POP2_PUSH3",
        "DECREMENT",
        "PUT_VALUE",
        "UPDATE_EMPTY",
    ]), true, false)); "post-decrement, non-strict")]
    #[test_case("++a", true, None => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "PRE_INCREMENT"]), true, false)); "pre-increment, strict")]
    #[test_case("++a", false, None => Ok((svec(&["STRING 0 (a)", "RESOLVE", "PRE_INCREMENT"]), true, false)); "pre-increment, non-strict")]
    #[test_case("--a", true, None => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "PRE_DECREMENT"]), true, false)); "pre-decrement, strict")]
    #[test_case("--a", false, None => Ok((svec(&["STRING 0 (a)", "RESOLVE", "PRE_DECREMENT"]), true, false)); "pre-decrement, non-strict")]
    #[test_case("++a", true, Some(0) => serr("Out of room for strings in this compilation unit"); "pre-op, err in subexpr")]
    #[test_case("a++", true, Some(0) => serr("Out of room for strings in this compilation unit"); "post-op, err in subexpr")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).update_expression();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod unary_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true, None => Ok((svec(&["STRING 0 (id)", "STRICT_RESOLVE"]), true, true)); "fall-thru strict")]
    #[test_case("id", false, None => Ok((svec(&["STRING 0 (id)", "RESOLVE"]), true, true)); "fall-thru non strict")]
    #[test_case("-a", true, None => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "UNARY_MINUS"]), true, false)); "negate; strict")]
    #[test_case("-a", false, None => Ok((svec(&["STRING 0 (a)", "RESOLVE", "UNARY_MINUS"]), true, false)); "negate; non-strict")]
    #[test_case("-a", false, Some(0) => serr("Out of room for strings in this compilation unit"); "negate; compile fail")]
    #[test_case("+a", true, None => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "UNARY_PLUS"]), true, false)); "tonumber; strict")]
    #[test_case("+a", false, None => Ok((svec(&["STRING 0 (a)", "RESOLVE", "UNARY_PLUS"]), true, false)); "tonumber; non-strict")]
    #[test_case("+a", false, Some(0) => serr("Out of room for strings in this compilation unit"); "tonumber; compile fail")]
    #[test_case("!a", true, None => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "UNARY_NOT"]), true, false)); "not; strict")]
    #[test_case("!a", false, None => Ok((svec(&["STRING 0 (a)", "RESOLVE", "UNARY_NOT"]), true, false)); "not; non-strict")]
    #[test_case("!a", false, Some(0) => serr("Out of room for strings in this compilation unit"); "not; compile fail")]
    #[test_case("~a", true, None => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "UNARY_COMPLEMENT"]), true, false)); "complement; strict")]
    #[test_case("~a", false, None => Ok((svec(&["STRING 0 (a)", "RESOLVE", "UNARY_COMPLEMENT"]), true, false)); "complement; non-strict")]
    #[test_case("~a", false, Some(0) => serr("Out of room for strings in this compilation unit"); "complement; compile fail")]
    #[test_case("void a", true, None => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "VOID"]), true, false)); "void; strict")]
    #[test_case("void a", false, None => Ok((svec(&["STRING 0 (a)", "RESOLVE", "VOID"]), true, false)); "void; non-strict")]
    #[test_case("void a", false, Some(0) => serr("Out of room for strings in this compilation unit"); "void; compile fail")]
    #[test_case("typeof a", true, None => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "TYPEOF"]), true, false)); "typeof; strict")]
    #[test_case("typeof a", false, None => Ok((svec(&["STRING 0 (a)", "RESOLVE", "TYPEOF"]), true, false)); "typeof; non-strict")]
    #[test_case("typeof a", false, Some(0) => serr("Out of room for strings in this compilation unit"); "typeof; compile fail")]
    #[test_case("delete a", true, None => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "DELETE"]), true, false)); "delete; strict")]
    #[test_case("delete a", false, None => Ok((svec(&["STRING 0 (a)", "RESOLVE", "DELETE"]), true, false)); "delete; non-strict")]
    #[test_case("delete a", false, Some(0) => serr("Out of room for strings in this compilation unit"); "delete; compile fail")]
    #[test_case("await a", true, None => panics "not yet implemented"; "await; strict")]
    #[test_case("await a", false, None => panics "not yet implemented"; "await; non-strict")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).unary_expression();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod exponentiation_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true, &[] => Ok((svec(&["STRING 0 (id)", "STRICT_RESOLVE"]), true, true)); "fall-thru strict")]
    #[test_case("id", false, &[] => Ok((svec(&["STRING 0 (id)", "RESOLVE"]), true, true)); "fall-thru non strict")]
    #[test_case(
        "a**8", true, &[]
        => Ok((
            svec(&["STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 3", "FLOAT 0 (8)", "EXPONENTIATE"]),
            true,
            false
        ));
        "exponent expr; non-strict"
    )]
    #[test_case(
        "2**b", false, &[]
        => Ok((
            svec(&[
                "FLOAT 0 (2)",
                "STRING 0 (b)",
                "RESOLVE",
                "GET_VALUE",
                "JUMP_IF_NORMAL 4",
                "UNWIND 1",
                "JUMP 1",
                "EXPONENTIATE"
                ]),
            true,
            false
        ));
        "left infallible; right fallible"
    )]
    #[test_case(
        "8n**2", false, &[(Fillable::BigInt, 0)]
        => serr("Out of room for big ints in this compilation unit");
        "left compilation fails"
    )]
    #[test_case(
        "2**7n", false, &[(Fillable::BigInt, 0)]
        => serr("Out of room for big ints in this compilation unit");
        "right compilation fails"
    )]
    fn compile(src: &str, strict: bool, slots_left: &[(Fillable, usize)]) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).exponentiation_expression();
        let mut c = complex_filled_chunk("x", slots_left);
        node.compile(&mut c, strict, src)
            .as_ref()
            .map(|flags| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    flags.maybe_abrupt(),
                    flags.maybe_ref(),
                )
            })
            .map_err(ToString::to_string)
    }
}

mod multiplicative_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true, &[] => Ok((svec(&["STRING 0 (id)", "STRICT_RESOLVE"]), true, true)); "fall-thru strict")]
    #[test_case("id", false, &[] => Ok((svec(&["STRING 0 (id)", "RESOLVE"]), true, true)); "fall-thru non strict")]
    #[test_case(
        "a*8", true, &[]
        => Ok((
            svec(&["STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 3", "FLOAT 0 (8)", "MULTIPLY"]),
            true,
            false
        ));
        "multiply expr; strict"
    )]
    #[test_case(
        "a/8", true, &[]
        => Ok((
            svec(&["STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 3", "FLOAT 0 (8)", "DIVIDE"]),
            true,
            false
        ));
        "divide expr; strict"
    )]
    #[test_case(
        "a%8", true, &[]
        => Ok((
            svec(&["STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 3", "FLOAT 0 (8)", "MODULO"]),
            true,
            false
        ));
        "modulo expr; strict"
    )]
    #[test_case(
        "8*a", true, &[]
        => Ok((
            svec(&[
                "FLOAT 0 (8)",
                "STRING 0 (a)",
                "STRICT_RESOLVE",
                "GET_VALUE",
                "JUMP_IF_NORMAL 4",
                "UNWIND 1",
                "JUMP 1",
                "MULTIPLY"
            ]),
            true,
            false
        ));
        "left infallible; right fallible"
    )]
    #[test_case(
        "8n*1", false, &[(Fillable::BigInt, 0)]
        => serr("Out of room for big ints in this compilation unit");
        "left compile fails"
    )]
    #[test_case(
        "1*8n", false, &[(Fillable::BigInt, 0)]
        => serr("Out of room for big ints in this compilation unit");
        "right compile fails"
    )]
    fn compile(src: &str, strict: bool, slots_left: &[(Fillable, usize)]) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).multiplicative_expression();
        let mut c = complex_filled_chunk("x", slots_left);
        node.compile(&mut c, strict, src)
            .as_ref()
            .map(|flags| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    flags.maybe_abrupt(),
                    flags.maybe_ref(),
                )
            })
            .map_err(ToString::to_string)
    }
}

mod additive_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true, &[] => Ok((svec(&["STRING 0 (id)", "STRICT_RESOLVE"]), true, true)); "fall-thru strict")]
    #[test_case("id", false, &[] => Ok((svec(&["STRING 0 (id)", "RESOLVE"]), true, true)); "fall-thru non strict")]
    #[test_case("a+3", true, &[] => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 3", "FLOAT 0 (3)", "ADD"]), true, false)); "add expr")]
    #[test_case("a-3", true, &[] => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 3", "FLOAT 0 (3)", "SUBTRACT"]), true, false)); "sub expr")]
    #[test_case("3+a", true, &[] => Ok((svec(&["FLOAT 0 (3)", "STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 1", "JUMP 1", "ADD"]), true, false)); "left infallible, right fallible")]
    #[test_case("8n+a", true, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "left compile fails")]
    #[test_case("a+8n", true, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "right compile fails")]
    fn compile(src: &str, strict: bool, slot_data: &[(Fillable, usize)]) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).additive_expression();
        let mut c = complex_filled_chunk("x", slot_data);
        node.compile(&mut c, strict, src)
            .as_ref()
            .map(|flags| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    flags.maybe_abrupt(),
                    flags.maybe_ref(),
                )
            })
            .map_err(ToString::to_string)
    }
}

mod shift_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true, &[] => Ok((svec(&["STRING 0 (id)", "STRICT_RESOLVE"]), true, true)); "fall-thru strict")]
    #[test_case("id", false, &[] => Ok((svec(&["STRING 0 (id)", "RESOLVE"]), true, true)); "fall-thru non strict")]
    #[test_case("a<<2", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "FLOAT 0 (2)",
        "LSH"
    ]), true, false)); "left shift, strict")]
    #[test_case("a<<2", false, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "FLOAT 0 (2)",
        "LSH"
    ]), true, false)); "left shift, non-strict")]
    #[test_case("a>>2", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "FLOAT 0 (2)",
        "SRSH"
    ]), true, false)); "signed right shift, strict")]
    #[test_case("a>>2", false, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "FLOAT 0 (2)",
        "SRSH"
    ]), true, false)); "signed right shift, non-strict")]
    #[test_case("a>>>2", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "FLOAT 0 (2)",
        "URSH"
    ]), true, false)); "unsigned right shift, strict")]
    #[test_case("a>>>2", false, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "FLOAT 0 (2)",
        "URSH"
    ]), true, false)); "unsigned right shift, non-strict")]
    #[test_case("2>>4", true, &[] => Ok((svec(&[
        "FLOAT 0 (2)",
        "FLOAT 1 (4)",
        "SRSH"
    ]), true, false)); "nothing that can throw")]
    #[test_case("2>>a", true, &[] => Ok((svec(&[
        "FLOAT 0 (2)",
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 1",
        "SRSH"
    ]), true, false)); "can throw from right")]
    fn compile(src: &str, strict: bool, slot_data: &[(Fillable, usize)]) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).shift_expression();
        let mut c = complex_filled_chunk("x", slot_data);
        node.compile(&mut c, strict, src)
            .as_ref()
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(ToString::to_string)
    }
}

mod relational_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true, &[] => Ok((svec(&["STRING 0 (id)", "STRICT_RESOLVE"]), true, true)); "fall-thru strict")]
    #[test_case("id", false, &[] => Ok((svec(&["STRING 0 (id)", "RESOLVE"]), true, true)); "fall-thru non strict")]
    #[test_case("a<10", true, &[] => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 3", "FLOAT 0 (10)", "LT"]), true, false)); "less than/strict")]
    #[test_case("a>10", true, &[] => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 3", "FLOAT 0 (10)", "GT"]), true, false)); "greater than/strict")]
    #[test_case("a<=10", true, &[] => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 3", "FLOAT 0 (10)", "LE"]), true, false)); "less equal/strict")]
    #[test_case("a>=10", true, &[] => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 3", "FLOAT 0 (10)", "GE"]), true, false)); "greater equal/strict")]
    #[test_case("a in b", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 11",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 1",
        "IN"
    ]), true, false)); "in/strict")]
    #[test_case("a instanceof c", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 11",
        "STRING 1 (c)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 1",
        "INSTANCEOF"
    ]), true, false)); "instanceof/strict")]
    #[test_case("#blue in gray", true, &[] => panics "not yet implemented"; "privateid in")]
    #[test_case("a<10", false, &[] => Ok((svec(&["STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 3", "FLOAT 0 (10)", "LT"]), true, false)); "less than/non-strict")]
    #[test_case("a>10", false, &[] => Ok((svec(&["STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 3", "FLOAT 0 (10)", "GT"]), true, false)); "greater than/non-strict")]
    #[test_case("a<=10", false, &[] => Ok((svec(&["STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 3", "FLOAT 0 (10)", "LE"]), true, false)); "less equal/non-strict")]
    #[test_case("a>=10", false, &[] => Ok((svec(&["STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 3", "FLOAT 0 (10)", "GE"]), true, false)); "greater equal/non-strict")]
    #[test_case("a in b", false, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 11",
        "STRING 1 (b)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 1",
        "IN"
    ]), true, false)); "in/non-strict")]
    #[test_case("a instanceof c", false, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 11",
        "STRING 1 (c)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 1",
        "INSTANCEOF"
    ]), true, false)); "instanceof/non-strict")]
    fn compile(src: &str, strict: bool, slot_data: &[(Fillable, usize)]) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).relational_expression();
        let mut c = complex_filled_chunk("x", slot_data);
        node.compile(&mut c, strict, src)
            .as_ref()
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(ToString::to_string)
    }

    #[test_case("a" => serr("RelationalExpression has no binary instruction"); "fall thru")]
    #[test_case("a < b" => Ok(Insn::Less); "less")]
    #[test_case("a <= b" => Ok(Insn::LessEqual); "less-equal")]
    #[test_case("a > b" => Ok(Insn::Greater); "greater")]
    #[test_case("a >= b" => Ok(Insn::GreaterEqual); "greater-equal")]
    #[test_case("a in b" => Ok(Insn::In); "op-in")]
    #[test_case("a instanceof b" => Ok(Insn::InstanceOf); "instance of")]
    fn insn(src: &str) -> Result<Insn, String> {
        let node = Maker::new(src).relational_expression();
        node.insn().map_err(|e| e.to_string())
    }
}

mod equality_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true, &[] => Ok((svec(&["STRING 0 (id)", "STRICT_RESOLVE"]), true, true)); "fall-thru strict")]
    #[test_case("id", false, &[] => Ok((svec(&["STRING 0 (id)", "RESOLVE"]), true, true)); "fall-thru non strict")]
    #[test_case("a==43", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "FLOAT 0 (43)",
        "EQ"
    ]), true, false)); "loosely eq/strict")]
    #[test_case("a===43", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "FLOAT 0 (43)",
        "SEQ"
    ]), true, false)); "strict eq/strict")]
    #[test_case("a!=43", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "FLOAT 0 (43)",
        "NE"
    ]), true, false)); "loosely not eq/strict")]
    #[test_case("a!==43", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "FLOAT 0 (43)",
        "SNE"
    ]), true, false)); "strict not eq/strict")]
    #[test_case("a==43", false, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "FLOAT 0 (43)",
        "EQ"
    ]), true, false)); "loosely eq/non-strict")]
    #[test_case("a===43", false, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "FLOAT 0 (43)",
        "SEQ"
    ]), true, false)); "strict eq/non-strict")]
    #[test_case("a!=43", false, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "FLOAT 0 (43)",
        "NE"
    ]), true, false)); "loosely not eq/non-strict")]
    #[test_case("a!==43", false, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "FLOAT 0 (43)",
        "SNE"
    ]), true, false)); "strict not eq/non-strict")]
    #[test_case(
        "5==b", false, &[]
        => Ok((
            svec(&[
                "FLOAT 0 (5)", "STRING 0 (b)", "RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 1", "JUMP 1", "EQ"
                ]),
            true,
            false
        ));
        "left infallible; right fallible"
    )]
    #[test_case(
        "5n==a", false, &[(Fillable::BigInt, 0)]
        => serr("Out of room for big ints in this compilation unit");
        "left compile fails"
    )]
    #[test_case(
        "a==5n", false, &[(Fillable::BigInt, 0)]
        => serr("Out of room for big ints in this compilation unit");
        "right compile fails"
    )]
    fn compile(src: &str, strict: bool, slot_data: &[(Fillable, usize)]) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).equality_expression();
        let mut c = complex_filled_chunk("x", slot_data);
        node.compile(&mut c, strict, src)
            .as_ref()
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(ToString::to_string)
    }
}

mod bitwise_and_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true, &[] => Ok((svec(&["STRING 0 (id)", "STRICT_RESOLVE"]), true, true)); "fall-thru strict")]
    #[test_case("id", false, &[] => Ok((svec(&["STRING 0 (id)", "RESOLVE"]), true, true)); "fall-thru non strict")]
    #[test_case("a&43", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "FLOAT 0 (43)",
        "AND"
    ]), true, false)); "bit and expr/strict")]
    #[test_case("a&43", false, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "FLOAT 0 (43)",
        "AND"
    ]), true, false)); "bit and expr/non-strict")]
    #[test_case(
        "43&a", false, &[]
        => Ok((
            svec(&[
                "FLOAT 0 (43)", "STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 1", "JUMP 1", "AND"
                ]),
            true,
            false
        ));
        "infallible left, fallible right"
    )]
    #[test_case(
        "43n&a", false, &[(Fillable::BigInt, 0)]
        => serr("Out of room for big ints in this compilation unit");
        "left compile fails"
    )]
    #[test_case(
        "a&43n", false, &[(Fillable::BigInt, 0)]
        => serr("Out of room for big ints in this compilation unit");
        "right compile fails"
    )]
    fn compile(src: &str, strict: bool, slot_data: &[(Fillable, usize)]) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).bitwise_and_expression();
        let mut c = complex_filled_chunk("x", slot_data);
        node.compile(&mut c, strict, src)
            .as_ref()
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(ToString::to_string)
    }
}

mod bitwise_xor_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true, &[] => Ok((svec(&["STRING 0 (id)", "STRICT_RESOLVE"]), true, true)); "fall-thru strict")]
    #[test_case("id", false, &[] => Ok((svec(&["STRING 0 (id)", "RESOLVE"]), true, true)); "fall-thru non strict")]
    #[test_case("a^43", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "FLOAT 0 (43)",
        "XOR"
    ]), true, false)); "bit xor expr/strict")]
    #[test_case("a^43", false, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "FLOAT 0 (43)",
        "XOR"
    ]), true, false)); "bit xor expr/non-strict")]
    #[test_case(
        "2^x", false, &[]
        => Ok((
            svec(&[
                "FLOAT 0 (2)", "STRING 0 (x)", "RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 1", "JUMP 1", "XOR"
                ]),
            true,
            false
        ));
        "left infallible, right fallible"
    )]
    #[test_case(
        "2n^a", false, &[(Fillable::BigInt, 0)]
        => serr("Out of room for big ints in this compilation unit");
        "left compile fails"
    )]
    #[test_case(
        "a^2n", false, &[(Fillable::BigInt, 0)]
        => serr("Out of room for big ints in this compilation unit");
        "right compile fails"
    )]
    fn compile(src: &str, strict: bool, slot_data: &[(Fillable, usize)]) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).bitwise_xor_expression();
        let mut c = complex_filled_chunk("x", slot_data);
        node.compile(&mut c, strict, src)
            .as_ref()
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(ToString::to_string)
    }
}

mod bitwise_or_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true, &[] => Ok((svec(&["STRING 0 (id)", "STRICT_RESOLVE"]), true, true)); "fall-thru strict")]
    #[test_case("id", false, &[] => Ok((svec(&["STRING 0 (id)", "RESOLVE"]), true, true)); "fall-thru non strict")]
    #[test_case("a|43", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "FLOAT 0 (43)",
        "OR"
    ]), true, false)); "bit or expr/strict")]
    #[test_case("a|43", false, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "FLOAT 0 (43)",
        "OR"
    ]), true, false)); "bit or expr/non-strict")]
    #[test_case(
        "43|a", false, &[]
        => Ok((
            svec(&[
                "FLOAT 0 (43)", "STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 1", "JUMP 1", "OR"
            ]),
            true,
            false
        ));
        "left infallible, right fallible"
    )]
    #[test_case(
        "2n|a", false, &[(Fillable::BigInt, 0)]
        => serr("Out of room for big ints in this compilation unit");
        "left compile fails"
    )]
    #[test_case(
        "a|2n", false, &[(Fillable::BigInt, 0)]
        => serr("Out of room for big ints in this compilation unit");
        "right compile fails"
    )]
    fn compile(src: &str, strict: bool, slot_data: &[(Fillable, usize)]) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).bitwise_or_expression();
        let mut c = complex_filled_chunk("x", slot_data);
        node.compile(&mut c, strict, src)
            .as_ref()
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(ToString::to_string)
    }
}

mod logical_and_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true, None => Ok((svec(&["STRING 0 (id)", "STRICT_RESOLVE"]), true, true)); "fall-thru strict")]
    #[test_case("id", false, None => Ok((svec(&["STRING 0 (id)", "RESOLVE"]), true, true)); "fall-thru non strict")]
    #[test_case("a&&b", true, None => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 7",
        "JUMP_IF_FALSE 5",
        "POP",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE"
    ]), true, false)); "land expr/strict")]
    #[test_case("a&&b", false, None => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 7",
        "JUMP_IF_FALSE 5",
        "POP",
        "STRING 1 (b)",
        "RESOLVE",
        "GET_VALUE"
    ]), true, false)); "land expr/non-strict")]
    #[test_case("a&&b", true, Some(0) => serr("Out of room for strings in this compilation unit"); "left expression error")]
    #[test_case("true&&false", true, None => Ok((svec(&["TRUE", "JUMP_IF_FALSE 2", "POP", "FALSE"]), false, false)); "just constants")]
    #[test_case("a&&b", true, Some(1) => serr("Out of room for strings in this compilation unit"); "right expression error")]
    #[test_case("a&&@@@", true, None => serr("out of range integral type conversion attempted"); "jump-if-abrupt too far")]
    #[test_case("true&&@@@", true, None => serr("out of range integral type conversion attempted"); "jump-if-false too far")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).logical_and_expression();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod logical_or_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true, None => Ok((svec(&["STRING 0 (id)", "STRICT_RESOLVE"]), true, true)); "fall-thru strict")]
    #[test_case("id", false, None => Ok((svec(&["STRING 0 (id)", "RESOLVE"]), true, true)); "fall-thru non strict")]
    #[test_case("a||b", true, None => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 7",
        "JUMP_IF_TRUE 5",
        "POP",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE"
    ]), true, false)); "lor expr/strict")]
    #[test_case("a||b", false, None => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 7",
        "JUMP_IF_TRUE 5",
        "POP",
        "STRING 1 (b)",
        "RESOLVE",
        "GET_VALUE"
    ]), true, false)); "lor expr/non-strict")]
    #[test_case("a||b", true, Some(0) => serr("Out of room for strings in this compilation unit"); "left expression error")]
    #[test_case("true||false", true, None => Ok((svec(&["TRUE", "JUMP_IF_TRUE 2", "POP", "FALSE"]), false, false)); "just constants")]
    #[test_case("a||b", true, Some(1) => serr("Out of room for strings in this compilation unit"); "right expression error")]
    #[test_case("a||@@@", true, None => serr("out of range integral type conversion attempted"); "jump-if-abrupt too far")]
    #[test_case("true||@@@", true, None => serr("out of range integral type conversion attempted"); "jump-if-true too far")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).logical_or_expression();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod coalesce_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("a??b", true, None => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 7",
        "JUMP_NOT_NULLISH 5",
        "POP",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE"
    ]), true, false)); "coal expr/strict")]
    #[test_case("a??b", false, None => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 7",
        "JUMP_NOT_NULLISH 5",
        "POP",
        "STRING 1 (b)",
        "RESOLVE",
        "GET_VALUE"
    ]), true, false)); "coal expr/non-strict")]
    #[test_case("a??b", true, Some(0) => serr("Out of room for strings in this compilation unit"); "left expression error")]
    #[test_case("true??false", true, None => Ok((svec(&["TRUE", "JUMP_NOT_NULLISH 2", "POP", "FALSE"]), false, false)); "just constants")]
    #[test_case("a??b", true, Some(1) => serr("Out of room for strings in this compilation unit"); "right expression error")]
    #[test_case("a??@@@", true, None => serr("out of range integral type conversion attempted"); "jump-if-abrupt too far")]
    #[test_case("true??@@@", true, None => serr("out of range integral type conversion attempted"); "jump-not-nullish too far")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).coalesce_expression();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod coalesce_expression_head {
    use super::*;
    use test_case::test_case;

    #[test_case("a??b", true, None => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE"]), true, true)); "bitwise or/strict")]
    #[test_case("a??b??c", true, None => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 7",
        "JUMP_NOT_NULLISH 5",
        "POP",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE"
    ]), true, false)); "coalesce/strict")]
    #[test_case("a??b", false, None => Ok((svec(&["STRING 0 (a)", "RESOLVE"]), true, true)); "bitwise or/non-strict")]
    #[test_case("a??b??c", false, None => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 7",
        "JUMP_NOT_NULLISH 5",
        "POP",
        "STRING 1 (b)",
        "RESOLVE",
        "GET_VALUE"
    ]), true, false)); "coalesce/non-strict")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).coalesce_expression_head();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod short_circuit_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true, None => Ok((svec(&["STRING 0 (id)", "STRICT_RESOLVE"]), true, true)); "fall-thru strict")]
    #[test_case("id", false, None => Ok((svec(&["STRING 0 (id)", "RESOLVE"]), true, true)); "fall-thru non strict")]
    #[test_case("a??b", true, None => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 7",
        "JUMP_NOT_NULLISH 5",
        "POP",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE"
    ]), true, false)); "coal expr/strict")]
    #[test_case("a??b", false, None => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 7",
        "JUMP_NOT_NULLISH 5",
        "POP",
        "STRING 1 (b)",
        "RESOLVE",
        "GET_VALUE"
    ]), true, false)); "coal expr/non-strict")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).short_circuit_expression();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod conditional_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true, None => Ok((svec(&["STRING 0 (id)", "STRICT_RESOLVE"]), true, true)); "fall-thru strict")]
    #[test_case("id", false, None => Ok((svec(&["STRING 0 (id)", "RESOLVE"]), true, true)); "fall-thru non strict")]
    #[test_case("a?b:c", true, None => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 14",
        "JUMP_IF_FALSE 7",
        "POP",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP 5",
        "POP",
        "STRING 2 (c)",
        "STRICT_RESOLVE",
        "GET_VALUE"
    ]), true, false)); "conditional expr / strict")]
    #[test_case("a?b:c", false, None => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 14",
        "JUMP_IF_FALSE 7",
        "POP",
        "STRING 1 (b)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP 5",
        "POP",
        "STRING 2 (c)",
        "RESOLVE",
        "GET_VALUE"
    ]), true, false)); "conditional expr / non-strict")]
    #[test_case("a?0:0", true, Some(0) => serr("Out of room for strings in this compilation unit"); "expr compile fails")]
    #[test_case("true?a:0", true, Some(0) => serr("Out of room for strings in this compilation unit"); "truthy compile fails")]
    #[test_case("true?false:a", true, Some(0) => serr("Out of room for strings in this compilation unit"); "falsey compile fails")]
    #[test_case("false?'a':'b'", true, None => Ok((svec(&["FALSE", "JUMP_IF_FALSE 5", "POP", "STRING 0 (a)", "JUMP 3", "POP", "STRING 1 (b)"]), false, false)); "only constants")]
    #[test_case("true?@@@:1", true, None => serr("out of range integral type conversion attempted"); "truthy too big")]
    #[test_case("true?0:@@@", true, None => serr("out of range integral type conversion attempted"); "falsey too big")]
    #[test_case("a?0:@@@", true, None => serr("out of range integral type conversion attempted"); "err jump too far")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).conditional_expression();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod assignment_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true, &[] => Ok((svec(&["STRING 0 (id)", "STRICT_RESOLVE"]), true, true)); "fall-thru strict")]
    #[test_case("id", false, &[] => Ok((svec(&["STRING 0 (id)", "RESOLVE"]), true, true)); "fall-thru non strict")]
    #[test_case("a=6", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 5",
        "FLOAT 0 (6)",
        "POP2_PUSH3",
        "PUT_VALUE",
        "UPDATE_EMPTY"
    ]), true, false)); "strict assignment expr")]
    #[test_case("a=6", false, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 5",
        "FLOAT 0 (6)",
        "POP2_PUSH3",
        "PUT_VALUE",
        "UPDATE_EMPTY"
    ]), true, false)); "non-strict assignment expr")]
    #[test_case("a=1", true, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "lhse errs")]
    #[test_case("a=function(){}", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 13",
        "STRING 0 (a)",
        "FUNC_IIFE 0",
        "JUMP_IF_NORMAL 4",
        "SWAP",
        "POP",
        "JUMP 3",
        "POP2_PUSH3",
        "PUT_VALUE",
        "UPDATE_EMPTY"
    ]), true, false)); "anonymous func/strict")]
    #[test_case("a=function(){}", false, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 13",
        "STRING 0 (a)",
        "FUNC_IIFE 0",
        "JUMP_IF_NORMAL 4",
        "SWAP",
        "POP",
        "JUMP 3",
        "POP2_PUSH3",
        "PUT_VALUE",
        "UPDATE_EMPTY"
    ]), true, false)); "anonymous func/non-strict")]
    #[test_case("a=function(){}", true, &[(Fillable::FunctionStash, 0)] => serr("Out of room for more functions!"); "full function table")]
    #[test_case("a[1]=function(){}", false, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 8",
        "FLOAT 0 (1)",
        "TO_KEY",
        "JUMP_IF_ABRUPT 1",
        "REF",
        "UNWIND_IF_ABRUPT 1",
        "JUMP_IF_ABRUPT 13",
        "STRING 1 ()",
        "FUNC_IIFE 0",
        "JUMP_IF_NORMAL 4",
        "SWAP",
        "POP",
        "JUMP 3",
        "POP2_PUSH3",
        "PUT_VALUE",
        "UPDATE_EMPTY"
    ]), true, false)); "non-id lref in anon func")]
    #[test_case("a=b", true, &[(Fillable::String, 1)] => serr("Out of room for strings in this compilation unit"); "ae errs")]
    #[test_case("a=b", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 13",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "SWAP",
        "POP",
        "JUMP 3",
        "POP2_PUSH3",
        "PUT_VALUE",
        "UPDATE_EMPTY"
    ]), true, false)); "ae is reference; strict")]
    #[test_case("a=b", false, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 13",
        "STRING 1 (b)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "SWAP",
        "POP",
        "JUMP 3",
        "POP2_PUSH3",
        "PUT_VALUE",
        "UPDATE_EMPTY"
    ]), true, false)); "ae is reference; non-strict")]
    #[test_case("a+=3", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 26",
        "DUP",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 18",
        "FLOAT 0 (3)",
        "ADD",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 9",
        "POP2_PUSH3",
        "PUT_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 1",
        "POP"
    ]), true, false)); "mutating assignment")]
    #[test_case("a+=9", true, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "op lval err")]
    #[test_case("1+=0", true, &[] => Ok((svec(&[
        "FLOAT 0 (1)",
        "DUP",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 18",
        "FLOAT 1 (0)",
        "ADD",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 9",
        "POP2_PUSH3",
        "PUT_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 1",
        "POP"
    ]), true, false)); "op lhse not abrupt")]
    #[test_case("a/=b", true, &[(Fillable::String, 1)] => serr("Out of room for strings in this compilation unit"); "op rhse compilation fail")]
    #[test_case("a*=b", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 34",
        "DUP",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 26",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 2",
        "JUMP 16",
        "MULTIPLY",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 9",
        "POP2_PUSH3",
        "PUT_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 1",
        "POP"
    ]), true, false)); "op rhse maybe abrupt")]
    #[test_case("a+=@@@", true, &[] => serr("out of range integral type conversion attempted"); "op rhs too big")]
    #[test_case("0+=@@@", true, &[] => serr("out of range integral type conversion attempted"); "op other rhs too big")]
    #[test_case("a/=1", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 26",
        "DUP",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 18",
        "FLOAT 0 (1)",
        "DIVIDE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 9",
        "POP2_PUSH3",
        "PUT_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 1",
        "POP"
    ]), true, false)); "op divide")]
    #[test_case("a%=1", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 26",
        "DUP",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 18",
        "FLOAT 0 (1)",
        "MODULO",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 9",
        "POP2_PUSH3",
        "PUT_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 1",
        "POP"
    ]), true, false)); "op modulo")]
    #[test_case("a-=1", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 26",
        "DUP",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 18",
        "FLOAT 0 (1)",
        "SUBTRACT",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 9",
        "POP2_PUSH3",
        "PUT_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 1",
        "POP"
    ]), true, false)); "op subtract")]
    #[test_case("a<<=1", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 26",
        "DUP",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 18",
        "FLOAT 0 (1)",
        "LSH",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 9",
        "POP2_PUSH3",
        "PUT_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 1",
        "POP"
    ]), true, false)); "op left-shift")]
    #[test_case("a>>=1", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 26",
        "DUP",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 18",
        "FLOAT 0 (1)",
        "SRSH",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 9",
        "POP2_PUSH3",
        "PUT_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 1",
        "POP"
    ]), true, false)); "op right-shift")]
    #[test_case("a>>>=1", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 26",
        "DUP",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 18",
        "FLOAT 0 (1)",
        "URSH",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 9",
        "POP2_PUSH3",
        "PUT_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 1",
        "POP"
    ]), true, false)); "op unsigned right shift")]
    #[test_case("a&=1", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 26",
        "DUP",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 18",
        "FLOAT 0 (1)",
        "AND",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 9",
        "POP2_PUSH3",
        "PUT_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 1",
        "POP"
    ]), true, false)); "op binary and")]
    #[test_case("a|=1", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 26",
        "DUP",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 18",
        "FLOAT 0 (1)",
        "OR",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 9",
        "POP2_PUSH3",
        "PUT_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 1",
        "POP"
    ]), true, false)); "op binary or")]
    #[test_case("a^=1", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 26",
        "DUP",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 18",
        "FLOAT 0 (1)",
        "XOR",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 9",
        "POP2_PUSH3",
        "PUT_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 1",
        "POP"
    ]), true, false)); "op binary xor")]
    #[test_case("a**=1", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 26",
        "DUP",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 18",
        "FLOAT 0 (1)",
        "EXPONENTIATE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 9",
        "POP2_PUSH3",
        "PUT_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 1",
        "POP"
    ]), true, false)); "op exponentiate")]
    #[test_case("a=@@@", true, &[] => serr("out of range integral type conversion attempted"); "ae is too big")]
    #[test_case("1=0", true, &[] => Ok((svec(&[
        "FLOAT 0 (1)",
        "FLOAT 1 (0)",
        "POP2_PUSH3",
        "PUT_VALUE",
        "UPDATE_EMPTY"
    ]), true, false)); "lhse not abrupt")]
    #[test_case("x => 0", true, &[] => Ok((svec(&["STRING 0 ()", "FUNC_IAE 0"]), true, false)); "arrow function")]
    #[test_case("yield 1", true, &[] => panics "not yet implemented"; "yield expr")]
    #[test_case("async x => x", true, &[] => panics "not yet implemented"; "async arrow")]
    #[test_case(
        "a &&= b", true, &[]
        => Ok((
            svec(&[
                "STRING 0 (a)",
                "STRICT_RESOLVE",
                "JUMP_IF_ABRUPT 24",
                "DUP",
                "GET_VALUE",
                "JUMP_IF_ABRUPT 18",
                "JUMP_IF_FALSE 16",
                "POP",
                "STRING 1 (b)",
                "STRICT_RESOLVE",
                "GET_VALUE",
                "JUMP_IF_ABRUPT 9",
                "DUP",
                "ROTATEDOWN 3",
                "PUT_VALUE",
                "JUMP_IF_ABRUPT 3",
                "POP",
                "JUMP 2",
                "UNWIND 1"
            ]),
            true,
            false,
        ));
        "logical and assignment"
    )]
    #[test_case("a ||= b", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 24",
        "DUP",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 18",
        "JUMP_IF_TRUE 16",
        "POP",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 9",
        "DUP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 3",
        "POP",
        "JUMP 2",
        "UNWIND 1"
    ]), true, false)); "logical or assignment")]
    #[test_case("c ??= b", true, &[] => Ok((svec(&[
        "STRING 0 (c)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 24",
        "DUP",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 18",
        "JUMP_NOT_NULLISH 16",
        "POP",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 9",
        "DUP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 3",
        "POP",
        "JUMP 2",
        "UNWIND 1"
    ]), true, false)); "coalesce assignment")]
    #[test_case("{a} = b", true, &[] => Ok((svec(&[
        "STRING 0 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 34",
        "REQ_COER",
        "JUMP_IF_ABRUPT 31",
        "STRING 1 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 19",
        "SWAP",
        "POP2_PUSH3",
        "STRING 1 (a)",
        "GETV",
        "JUMP_IF_ABRUPT 10",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 9",
        "POP",
        "STRING 1 (a)",
        "FLOAT 0 (1)",
        "JUMP 2",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 3",
        "POP_LIST",
        "JUMP 2",
        "UNWIND 1"]), true, false)); "destructuring assignment - simple")]
    #[test_case("{a}=3n", true, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "destructuring - bad expr")]
    #[test_case("{a}=1", true, &[] => Ok((svec(&[
        "FLOAT 0 (1)",
        "REQ_COER",
        "JUMP_IF_ABRUPT 31",
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 19",
        "SWAP",
        "POP2_PUSH3",
        "STRING 0 (a)",
        "GETV",
        "JUMP_IF_ABRUPT 10",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 9",
        "POP",
        "STRING 0 (a)",
        "FLOAT 0 (1)",
        "JUMP 2",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 3",
        "POP_LIST",
        "JUMP 2",
        "UNWIND 1"
    ]), true, false)); "destructuring - not ref")]
    #[test_case("{a=3n}=b", true, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "destructuring - bad pattern")]
    #[test_case("{a=@@(38)}=b", true, &[] => serr("out of range integral type conversion attempted"); "destructuring - jump too far")]
    fn compile(src: &str, strict: bool, slots_left: &[(Fillable, usize)]) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).assignment_expression();
        let mut c = complex_filled_chunk("x", slots_left);
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true, None => Ok((svec(&["STRING 0 (id)", "STRICT_RESOLVE"]), true, true)); "fall-thru strict")]
    #[test_case("id", false, None => Ok((svec(&["STRING 0 (id)", "RESOLVE"]), true, true)); "fall-thru non strict")]
    #[test_case("a,9", true, None => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 3", "POP", "FLOAT 0 (9)"]), true, false)); "comma expr")]
    #[test_case("a,0", true, Some(0) => serr("Out of room for strings in this compilation unit"); "first expr compile fail")]
    #[test_case("0,a", true, None => Ok((svec(&["FLOAT 0 (0)", "POP", "STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE"]), true, false)); "first expr literal")]
    #[test_case("true,false", true, None => Ok((svec(&["TRUE", "POP", "FALSE"]), false, false)); "nothing but literals")]
    #[test_case("true,a", true, Some(0) => serr("Out of room for strings in this compilation unit"); "second expr compile fail")]
    #[test_case("a,@@@", true, None => serr("out of range integral type conversion attempted"); "jump too far")]
    #[test_case("a,b", false, None => Ok((svec(&["STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 5", "POP", "STRING 1 (b)", "RESOLVE", "GET_VALUE"]), true, false)); "not strict")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).expression();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod expression_statement {
    use super::*;

    mod compile {
        use super::*;
        use test_case::test_case;

        #[test_case("id;", true => svec(&["STRING 0 (id)", "STRICT_RESOLVE", "GET_VALUE"]); "strict")]
        #[test_case("id;", false => svec(&["STRING 0 (id)", "RESOLVE", "GET_VALUE"]); "non strict")]
        #[test_case("3;", false => svec(&["FLOAT 0 (3)"]); "literal")]
        fn normal(src: &str, strict: bool) -> Vec<String> {
            let node = Maker::new(src).expression_statement();
            let mut c = Chunk::new("x");
            node.compile(&mut c, strict, src).unwrap();
            c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>()
        }

        #[test_case("a;" => "Out of room for strings in this compilation unit"; "no room")]
        fn error(src: &str) -> String {
            let node = Maker::new(src).expression_statement();
            let mut c = full_chunk("x");
            node.compile(&mut c, true, src).unwrap_err().to_string()
        }
    }
}

mod statement_list {
    use super::*;
    use test_case::test_case;

    #[test_case("id;", true, None => Ok((svec(&["STRING 0 (id)", "STRICT_RESOLVE", "GET_VALUE"]), true, false)); "id-strict")]
    #[test_case("id;", false, None => Ok((svec(&["STRING 0 (id)", "RESOLVE", "GET_VALUE"]), true, false)); "id-non-strict")]
    #[test_case("a; b;", true, None => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 5",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "UPDATE_EMPTY"
    ]), true, false)); "strict list")]
    #[test_case("a; b;", false, None => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 5",
        "STRING 1 (b)",
        "RESOLVE",
        "GET_VALUE",
        "UPDATE_EMPTY"
    ]), true, false)); "non-strict list")]
    #[test_case("a;3;", true, Some(0) => serr("Out of room for strings in this compilation unit"); "err in list")]
    #[test_case("true;b;", true, Some(0) => serr("Out of room for strings in this compilation unit"); "err in item")]
    #[test_case("a;@@@;", true, None => serr("out of range integral type conversion attempted"); "item is too big")]
    #[test_case("1;b;", true, None => Ok((svec(&[
        "FLOAT 0 (1)",
        "STRING 0 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "UPDATE_EMPTY"
    ]), true, false)); "list can't return abruptly")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).statement_list();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod statement_list_item {
    use super::*;
    use test_case::test_case;

    #[test_case("a;", true => svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
    ]); "strict stmt")]
    #[test_case("a;", false => svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
    ]); "non-strict stmt")]
    #[test_case("let a;", true => svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "UNDEFINED",
        "IRB",
        "JUMP_IF_ABRUPT 2",
        "POP",
        "EMPTY",
    ]); "strict decl")]
    #[test_case("let a;", false => svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "UNDEFINED",
        "IRB",
        "JUMP_IF_ABRUPT 2",
        "POP",
        "EMPTY",
    ]); "non-strict decl")]
    fn compile(src: &str, strict: bool) -> Vec<String> {
        let node = Maker::new(src).statement_list_item();
        let mut c = Chunk::new("x");
        node.compile(&mut c, strict, src).unwrap();
        c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>()
    }
}

mod statement {
    use super::*;
    use test_case::test_case;

    #[test_case("a;", true => svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
    ]); "strict expr stmt")]
    #[test_case("a;", false => svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
    ]); "non-strict expr stmt")]
    #[test_case("{}", true => svec(&["EMPTY"]); "block")]
    #[test_case("var a=b;", true => svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 11",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 1",
        "PUT_VALUE",
    ]); "strict var stmt")]
    #[test_case("var a=b;", false => svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 11",
        "STRING 1 (b)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 1",
        "PUT_VALUE",
    ]); "non-strict var stmt")]
    #[test_case(";", true => svec(&["EMPTY"]); "empty")]
    #[test_case("if (true) true;", true => svec(&["TRUE", "JUMPPOP_FALSE 3", "TRUE", "JUMP 1", "UNDEFINED", "UNDEFINED", "SWAP", "UPDATE_EMPTY"]); "if statement")]
    #[test_case("do ; while (false);", true => svec(&["UNDEFINED", "EMPTY", "COALESCE", "FALSE", "JUMPPOP_TRUE -5"]); "breakable statement")]
    #[test_case("continue;", true => svec(&["CONTINUE"]); "continue statement")]
    #[test_case("break;", true => svec(&["BREAK"]); "break statement")]
    #[test_case("return;", true => svec(&["UNDEFINED", "RETURN"]); "return statement")]
    #[test_case("with (a) {}", true => panics "not yet implemented"; "with statement")]
    #[test_case("a: true;", true => svec(&["TRUE"]); "labelled statement")]
    #[test_case("throw a;", true => svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 1",
        "THROW"
    ]); "strict throw")]
    #[test_case("throw a;", false => svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 1",
        "THROW"
    ]); "non-strict throw")]
    #[test_case("try {} catch {}", true => svec(&["UNDEFINED", "EMPTY", "UPDATE_EMPTY"]); "try statement")]
    #[test_case("debugger;", true => panics "not yet implemented"; "debugger")]
    fn compile(src: &str, strict: bool) -> Vec<String> {
        let node = Maker::new(src).statement();
        let mut c = Chunk::new("x");
        node.compile(&mut c, strict, src).unwrap();
        c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>()
    }

    #[test_case("do break alpha; while (false)", &["alpha", "beta"], true, None => Ok((svec(&[
        "UNDEFINED",
        "BREAK_FROM 0 (alpha)",
        "LOOP_CONT [alpha, beta]",
        "JUMPPOP_FALSE 6",
        "COALESCE",
        "FALSE",
        "JUMPPOP_TRUE -10",
        "JUMP 1",
        "UPDATE_EMPTY",
        "HEB"
    ]), true, false)); "breakable stmt")]
    #[test_case("true;", &["alpha", "beta"], true, None => Ok((svec(&["TRUE"]), false, false)); "simple statement")]
    #[test_case("gamma: do break alpha; while(false)", &["alpha", "beta"], true, None => Ok((svec(&[
        "UNDEFINED",
        "BREAK_FROM 0 (alpha)",
        "LOOP_CONT [alpha, beta, gamma]",
        "JUMPPOP_FALSE 6",
        "COALESCE",
        "FALSE",
        "JUMPPOP_TRUE -10",
        "JUMP 1",
        "UPDATE_EMPTY",
        "HEB",
        "HTB 1 (gamma)",
    ]), true, false)); "labelled statement")]
    fn labelled_compile(
        src: &str,
        labels: &[&str],
        strict: bool,
        spots_avail: Option<usize>,
    ) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).statement();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        let label_set = labels.iter().copied().map(JSString::from).collect::<Vec<JSString>>();
        node.labelled_compile(&mut c, strict, src, &label_set)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod declaration {
    use super::*;
    use test_case::test_case;

    #[test_case("class bob{}", true => panics "not yet implemented"; "class decl")]
    #[test_case("function bob(){}", true => svec(&["EMPTY"]); "hoistable")]
    #[test_case("const a=0;", true => svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "FLOAT 0 (0)",
        "IRB",
        "JUMP_IF_ABRUPT 2",
        "POP",
        "EMPTY",
    ]); "strict lexical")]
    #[test_case("const a=0;", false => svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "FLOAT 0 (0)",
        "IRB",
        "JUMP_IF_ABRUPT 2",
        "POP",
        "EMPTY",
    ]); "non-strict lexical")]
    fn compile(src: &str, strict: bool) -> Vec<String> {
        let node = Maker::new(src).declaration();
        let mut c = Chunk::new("x");
        node.compile(&mut c, strict, src).unwrap();
        c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>()
    }
}

mod lexical_declaration {
    use super::*;
    use test_case::test_case;

    #[test_case("let a;", true, None => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "UNDEFINED",
        "IRB",
        "JUMP_IF_ABRUPT 2",
        "POP",
        "EMPTY"
    ]), true, false)); "strict; typical")]
    #[test_case("let a;", false, None => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "UNDEFINED",
        "IRB",
        "JUMP_IF_ABRUPT 2",
        "POP",
        "EMPTY"
    ]), true, false)); "non-strict; typical")]
    #[test_case("let a;", true, Some(0) => serr("Out of room for strings in this compilation unit"); "full string table")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).lexical_declaration();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod binding_list {
    use super::*;
    use test_case::test_case;

    #[test_case("a", true, None =>  Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "UNDEFINED",
        "IRB",
    ]), true, false)); "strict item")]
    #[test_case("a", false, None =>  Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "UNDEFINED",
        "IRB",
    ]), true, false)); "non-strict item")]
    #[test_case("a,b", true, None =>  Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "UNDEFINED",
        "IRB",
        "JUMP_IF_ABRUPT 6",
        "POP",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "UNDEFINED",
        "IRB",
    ]), true, false)); "strict list")]
    #[test_case("a,b", false, None =>  Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "UNDEFINED",
        "IRB",
        "JUMP_IF_ABRUPT 6",
        "POP",
        "STRING 1 (b)",
        "RESOLVE",
        "UNDEFINED",
        "IRB",
    ]), true, false)); "non-strict list")]
    #[test_case("a,b", true, Some(0) => serr("Out of room for strings in this compilation unit"); "no room on list")]
    #[test_case("a,b", true, Some(1) => serr("Out of room for strings in this compilation unit"); "no room on item")]
    #[test_case("a,b=@@@", true, None => serr("out of range integral type conversion attempted"); "branch too far")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).binding_list();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod lexical_binding {
    use super::*;
    use test_case::test_case;

    #[test_case("a", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "UNDEFINED",
        "IRB",
    ]), true, false)); "strict, no initializer")]
    #[test_case("a", false, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "UNDEFINED",
        "IRB",
    ]), true, false)); "non-strict, no initializer")]
    #[test_case("a", true, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "string table full")]
    #[test_case("a=function (){}", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "STRING 0 (a)",
        "FUNC_IIFE 0",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 1",
        "IRB"
    ]), true, false)); "anonymous function; strict")]
    #[test_case("a=function (){}", false, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "STRING 0 (a)",
        "FUNC_IIFE 0",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 1",
        "IRB"
    ]), true, false)); "anonymous function; non-strict")]
    #[test_case("a=function(){}", true, &[(Fillable::FunctionStash, 0)] => serr("Out of room for more functions!"); "function table full")]
    #[test_case("a=b", true, &[(Fillable::String, 1)] => serr("Out of room for strings in this compilation unit"); "string table full in initializer")]
    #[test_case("a=b", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 1",
        "IRB"
    ]), true, false)); "resolvable initializer; strict")]
    #[test_case("a=b", false, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "STRING 1 (b)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 1",
        "IRB"
    ]), true, false)); "resolvable initializer; non-strict")]
    #[test_case("a=0", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "FLOAT 0 (0)",
        "IRB"
    ]), true, false)); "literal initializer; strict")]
    #[test_case("a=0", false, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "FLOAT 0 (0)",
        "IRB"
    ]), true, false)); "literal initializer; non-strict")]
    #[test_case(
        "{a}=b", true, &[]
        => Ok((
            svec(&[
                "STRING 0 (b)",
                "STRICT_RESOLVE",
                "GET_VALUE",
                "JUMP_IF_ABRUPT 33",
                "REQ_COER",
                "JUMP_IF_ABRUPT 30",
                "STRING 1 (a)",
                "STRING 1 (a)",
                "STRICT_RESOLVE",
                "JUMP_IF_ABRUPT 8",
                "ROTATEDOWN 3",
                "GETV",
                "JUMP_IF_ABRUPT 5",
                "IRB",
                "JUMP 4",
                "UNWIND 1",
                "UNWIND 1",
                "JUMP_IF_ABRUPT 5",
                "POP",
                "STRING 1 (a)",
                "FLOAT 0 (1)",
                "JUMP_IF_ABRUPT 2",
                "POP_LIST",
                "EMPTY"
            ]),
            true,
            false
        ));
        "pattern binding"
    )]
    fn compile(src: &str, strict: bool, what: &[(Fillable, usize)]) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).lexical_binding();
        let mut c = complex_filled_chunk("x", what);
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod block_statement {
    use super::*;
    use test_case::test_case;

    #[test_case("{ a; }", true, None => Ok((svec(&["PNLE", "STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "PLE"]), true, false)); "no decl/strict")]
    #[test_case("{ a; }", false, None => Ok((svec(&["PNLE", "STRING 0 (a)", "RESOLVE", "GET_VALUE", "PLE"]), true, false)); "no decl/non-strict")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).block_statement();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod block {
    use super::*;
    use test_case::test_case;

    #[test_case("{}", true, &[] => Ok((svec(&["EMPTY"]), false, false)); "empty block")]
    #[test_case("{ 1; }", true, &[] => Ok((svec(&["PNLE", "FLOAT 0 (1)", "PLE"]), false, false)); "all literal")]
    #[test_case("{ const zero=0; let one=1; }", true, &[] => Ok((svec(&[
        "PNLE",
        "CSILB 0 (zero)",
        "CPMLB 1 (one)",
        "STRING 0 (zero)",
        "STRICT_RESOLVE",
        "FLOAT 0 (0)",
        "IRB",
        "JUMP_IF_ABRUPT 2",
        "POP",
        "EMPTY",
        "JUMP_IF_ABRUPT 11",
        "STRING 1 (one)",
        "STRICT_RESOLVE",
        "FLOAT 1 (1)",
        "IRB",
        "JUMP_IF_ABRUPT 2",
        "POP",
        "EMPTY",
        "UPDATE_EMPTY",
        "PLE"
    ]), true, false)); "decls/strict")]
    #[test_case("{ const zero=0; let one=1; }", false, &[] => Ok((svec(&[
        "PNLE",
        "CSILB 0 (zero)",
        "CPMLB 1 (one)",
        "STRING 0 (zero)",
        "RESOLVE",
        "FLOAT 0 (0)",
        "IRB",
        "JUMP_IF_ABRUPT 2",
        "POP",
        "EMPTY",
        "JUMP_IF_ABRUPT 11",
        "STRING 1 (one)",
        "RESOLVE",
        "FLOAT 1 (1)",
        "IRB",
        "JUMP_IF_ABRUPT 2",
        "POP",
        "EMPTY",
        "UPDATE_EMPTY",
        "PLE"
    ]), true, false)); "decls/non-strict")]
    #[test_case("{ let a; }", true, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "error in decl formation")]
    #[test_case("{ function a() {} }", true, &[] => Ok((svec(&["PNLE", "CPMLB 0 (a)", "FUNC_OBJ 0 a", "ILB 0 (a)", "EMPTY", "PLE"]), false, false)); "function def")]
    #[test_case("{ function a() {} }", true, &[(Fillable::FunctionStash, 0)] => serr("Out of room for more functions!"); "function table full")]
    #[test_case("{ a; }", true, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "error in statement compilation")]
    fn compile(src: &str, strict: bool, what: &[(Fillable, usize)]) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).block();
        let mut c = complex_filled_chunk("x", what);
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod initializer {
    use super::*;
    use test_case::test_case;

    #[test_case("=a=b", true, None => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 13",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "SWAP",
        "POP",
        "JUMP 3",
        "POP2_PUSH3",
        "PUT_VALUE",
        "UPDATE_EMPTY"
    ]), true, false)); "assignment expression as initializer; strict")]
    #[test_case("=a=b", false, None => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 13",
        "STRING 1 (b)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "SWAP",
        "POP",
        "JUMP 3",
        "POP2_PUSH3",
        "PUT_VALUE",
        "UPDATE_EMPTY"
    ]), true, false)); "assignment expression as initializer; non-strict")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).initializer();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod variable_statement {
    use super::*;
    use test_case::test_case;

    #[test_case("var a;", true, None => Ok((svec(&["EMPTY"]), false, false)); "just one")]
    #[test_case("var x, y=67+b, c='hi';", true, None => Ok((svec(&[
        "EMPTY",
        "POP",
        "STRING 0 (y)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 20",
        "FLOAT 0 (67)",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 1",
        "ADD",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 1",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 9",
        "POP",
        "STRING 2 (c)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 3",
        "STRING 3 (hi)",
        "PUT_VALUE"
    ]), true, false)); "something complex")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).variable_statement();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod variable_declaration_list {
    use super::*;
    use test_case::test_case;

    #[test_case("a", true, None => Ok((svec(&["EMPTY"]), false, false)); "id/strict")]
    #[test_case("a, b", true, None => Ok((svec(&["EMPTY", "POP", "EMPTY"]), false, false)); "2 ids/strict")]
    #[test_case("a=1, b", true, Some(0) => serr("Out of room for strings in this compilation unit"); "first item compile fail")]
    #[test_case("a, b=1", true, Some(0) => serr("Out of room for strings in this compilation unit"); "second item compile fail")]
    #[test_case("a=b,c=d", true, None => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 11",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 1",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 17",
        "POP",
        "STRING 2 (c)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 11",
        "STRING 3 (d)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 1",
        "PUT_VALUE"
    ]), true, false)); "list with refs")]
    #[test_case("a=c, b=@@3", true, None => serr("out of range integral type conversion attempted"); "second item too large")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).variable_declaration_list();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod variable_declaration {
    use super::*;
    use test_case::test_case;

    #[test_case("a", true, &[] => Ok((svec(&["EMPTY"]), false, false)); "id/strict")]
    #[test_case("a=3", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 3",
        "FLOAT 0 (3)",
        "PUT_VALUE",
    ]), true, false)); "id lit init/strict")]
    #[test_case("a=3", false, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 3",
        "FLOAT 0 (3)",
        "PUT_VALUE",
    ]), true, false)); "id lit init/non-strict")]
    #[test_case("a=b", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 11",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 1",
        "PUT_VALUE",
    ]), true, false)); "id ref init/strict")]
    #[test_case("a=0", true, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "string exhaustion")]
    #[test_case("a=function(){}", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 11",
        "STRING 0 (a)",
        "FUNC_IIFE 0",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 1",
        "PUT_VALUE"
    ]), true, false)); "anonymous func; strict")]
    #[test_case("a=function(){}", false, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 11",
        "STRING 0 (a)",
        "FUNC_IIFE 0",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 1",
        "PUT_VALUE"
    ]), true, false)); "anonymous func; non-strict")]
    #[test_case("a=function(){}", true, &[(Fillable::FunctionStash, 0)] => serr("Out of room for more functions!"); "function table full")]
    #[test_case("a=b", true, &[(Fillable::String, 1)] => serr("Out of room for strings in this compilation unit"); "izer compilation fails")]
    #[test_case("a=@@@", true, &[] => serr("out of range integral type conversion attempted"); "izer too big")]
    #[test_case("[a]=b", true, &[] => Ok((svec(&[
        "STRING 0 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 26",
        "GET_SYNC_ITER",
        "JUMP_IF_ABRUPT 23",
        "DUP",
        "STRING 1 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 13",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 9",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 3",
        "POP",
        "JUMP 2",
        "UNWIND 1",
        "EMPTY_IF_NOT_ERR",
        "ITER_CLOSE_IF_NOT_DONE"
    ]), true, false)); "pattern assignment")]
    #[test_case("{a}={}", true, &[] => Ok((svec(&[
        "OBJECT",
        "REQ_COER",
        "JUMP_IF_ABRUPT 30",
        "STRING 0 (a)",
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 8",
        "ROTATEDOWN 3",
        "GETV",
        "JUMP_IF_ABRUPT 5",
        "PUT_VALUE",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 5",
        "POP",
        "STRING 0 (a)",
        "FLOAT 0 (1)",
        "JUMP_IF_ABRUPT 2",
        "POP_LIST",
        "EMPTY"
    ]), true, false)); "pattern; infallible init")]
    #[test_case("{a}=8n", true, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "init compile fails")]
    #[test_case("{a=8n}=b", true, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "pattern compile fails")]
    #[test_case("{a=@@(37)}=b", true, &[] => serr("out of range integral type conversion attempted"); "pattern: exit fixup too large")]
    fn compile(src: &str, strict: bool, what: &[(Fillable, usize)]) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).variable_declaration();
        let mut c = complex_filled_chunk("x", what);
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod throw_statement {
    use super::*;
    use test_case::test_case;

    #[test_case("throw 'a';", true, None => Ok((svec(&[
        "STRING 0 (a)",
        "THROW"
    ]), true, false)); "Throw literal")]
    #[test_case("throw a;", true, None => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 1",
        "THROW"
    ]), true, false)); "Throw reference")]
    #[test_case("throw a;", false, None => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 1",
        "THROW"
    ]), true, false)); "Throw ref/non-strict")]
    #[test_case("throw a;", true, Some(0) => serr("Out of room for strings in this compilation unit"); "exp compile fail")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).throw_statement();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod script {
    use super::*;
    use test_case::test_case;

    #[test_case("a;" => svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
    ]); "stmt")]
    #[test_case("" => svec(&[]); "empty")]
    fn compile(src: &str) -> Vec<String> {
        let node = Maker::new(src).script();
        let mut c = Chunk::new("x");
        node.compile(&mut c, false, src).unwrap();
        c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>()
    }
}

mod script_body {
    use super::*;
    use test_case::test_case;

    #[test_case("a;" => svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
    ]); "stmt")]
    #[test_case("'use strict'; a;" => svec(&[
        "STRING 0 (use strict)",
        "STRING 1 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "UPDATE_EMPTY",
    ]); "use-strict added")]
    fn compile(src: &str) -> Vec<String> {
        let node = Maker::new(src).script_body();
        let mut c = Chunk::new("x");
        node.compile(&mut c, false, src).unwrap();
        c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>()
    }
}

mod fcn_def {
    use super::*;
    use test_case::test_case;

    fn fcndecl() -> (FcnDef, String) {
        let src = "function a() {}";
        (FcnDef::Function(Maker::new(src).function_declaration()), src.to_string())
    }
    fn gendecl() -> (FcnDef, String) {
        let src = "function *a() {}";
        (FcnDef::Generator(Maker::new(src).generator_declaration()), src.to_string())
    }
    fn afcndecl() -> (FcnDef, String) {
        let src = "async function a() {}";
        (FcnDef::AsyncFun(Maker::new(src).async_function_declaration()), src.to_string())
    }
    fn agendecl() -> (FcnDef, String) {
        let src = "async function *a() {}";
        (FcnDef::AsyncGen(Maker::new(src).async_generator_declaration()), src.to_string())
    }

    #[test_case(fcndecl, true => Ok((svec(&["FUNC_OBJ 0 a"]), true, false)); "function decl")]
    #[test_case(gendecl, true => Ok((svec(&["TODO"]), false, false)); "generator decl")]
    #[test_case(afcndecl, true => Ok((svec(&["TODO"]), false, false)); "async function decl")]
    #[test_case(agendecl, true => Ok((svec(&["TODO"]), false, false)); "async generator decl")]
    fn compile_fo_instantiation(
        maker: fn() -> (FcnDef, String),
        strict: bool,
    ) -> Result<(Vec<String>, bool, bool), String> {
        let (part, src) = maker();
        let mut c = Chunk::new("x");

        part.compile_fo_instantiation(&mut c, strict, &src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod empty_statement {
    use super::*;
    use test_case::test_case;

    #[test_case(None => (svec(&["EMPTY"]), false, false); "typical")]
    fn compile(spots_avail: Option<usize>) -> (Vec<String>, bool, bool) {
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        let status = EmptyStatement::compile(&mut c);
        (
            c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
            status.maybe_abrupt(),
            status.maybe_ref(),
        )
    }
}

mod if_statement {
    use super::*;
    use test_case::test_case;

    #[test_case("if (1) 2;", true, None => Ok((svec(&[
        "FLOAT 0 (1)",
        "JUMPPOP_FALSE 4",
        "FLOAT 1 (2)",
        "JUMP 1",
        "UNDEFINED",
        "UNDEFINED",
        "SWAP",
        "UPDATE_EMPTY"
    ]), false, false)); "no else; only literals")]
    #[test_case("if (a) b; else c;", true, None => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 19",
        "JUMPPOP_FALSE 8",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 11",
        "JUMP 6",
        "STRING 2 (c)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "UNDEFINED",
        "SWAP",
        "UPDATE_EMPTY"
    ]), true, false)); "with else; all potentially fail")]
    #[test_case("if (a) 1;", true, Some(0) => serr("Out of room for strings in this compilation unit"); "expr compile fail")]
    #[test_case("if (true) a;", true, Some(0) => serr("Out of room for strings in this compilation unit"); "s1 compile fail")]
    #[test_case("if (true) false; else a;", true, Some(0) => serr("Out of room for strings in this compilation unit"); "s2 compile fail")]
    #[test_case("if (true) @@@; else false;", true, None => serr("out of range integral type conversion attempted"); "true path too large")]
    #[test_case("if (true) false; else @@@;", true, None => serr("out of range integral type conversion attempted"); "false path too large")]
    #[test_case("if (a) false; else @@3;", true, None => serr("out of range integral type conversion attempted"); "expr err exit jump too far")]
    #[test_case("if (true) a; else @@3;", true, None => serr("out of range integral type conversion attempted"); "s1 err exit jump too far")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).if_statement();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod breakable_statement {
    use super::*;
    use test_case::test_case;

    #[test_case("do ; while (a);", true, None => Ok((svec(&[
        "UNDEFINED",
        "EMPTY",
        "COALESCE",
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 2",
        "JUMPPOP_TRUE -14",
        "HEB",
    ]), true, false)); "dowhile/strict")]
    #[test_case("do ; while (a);", false, None => Ok((svec(&[
        "UNDEFINED",
        "EMPTY",
        "COALESCE",
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 2",
        "JUMPPOP_TRUE -14",
        "HEB",
    ]), true, false)); "dowhile/non-strict")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).breakable_statement();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }

    #[test_case("do a; while (false);", &["alpha"], true, None => Ok((svec(&[
        "UNDEFINED",
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "LOOP_CONT [alpha]",
        "JUMPPOP_FALSE 6",
        "COALESCE",
        "FALSE",
        "JUMPPOP_TRUE -12",
        "JUMP 1",
        "UPDATE_EMPTY",
        "HEB"
    ]), true, false)); "with HEB/strict")]
    #[test_case("do a; while (false);", &["alpha"], false, None => Ok((svec(&[
        "UNDEFINED",
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "LOOP_CONT [alpha]",
        "JUMPPOP_FALSE 6",
        "COALESCE",
        "FALSE",
        "JUMPPOP_TRUE -12",
        "JUMP 1",
        "UPDATE_EMPTY",
        "HEB"
    ]), true, false)); "with HEB/non-strict")]
    #[test_case("do ; while (false);", &["alpha"], true, None => Ok((svec(&[
        "UNDEFINED",
        "EMPTY",
        "COALESCE",
        "FALSE",
        "JUMPPOP_TRUE -5"
    ]), false, false)); "witout HEB")]
    #[test_case(
        "switch (a) { case 1: break; }", &["alpha"], true, None
        => Ok((
            svec(&[
                "STRING 0 (a)",
                "STRICT_RESOLVE",
                "GET_VALUE",
                "JUMP_IF_ABRUPT 24",
                "PNLE",
                "UNDEFINED",
                "SWAP",
                "FALSE",
                "JUMP_IF_TRUE 8",
                "POP",
                "DUP",
                "FLOAT 0 (1)",
                "SEQ",
                "JUMP_IF_FALSE 7",
                "POP",
                "POP",
                "BREAK",
                "UPDATE_EMPTY",
                "JUMP_IF_ABRUPT 4",
                "TRUE",
                "JUMPPOP_TRUE 1",
                "POP",
                "PLE",
                "HEB"
            ]),
            true,
            false
        ));
        "switch/strict"
    )]
    #[test_case("do a; while (false);", &["alpha"], true, Some(0) => serr("Out of room for strings in this compilation unit"); "compilation fails")]
    fn labelled_compile(
        src: &str,
        labels: &[&str],
        strict: bool,
        spots_avail: Option<usize>,
    ) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).breakable_statement();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        let label_set = labels.iter().copied().map(JSString::from).collect::<Vec<JSString>>();
        node.labelled_compile(&mut c, strict, src, &label_set)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod iteration_statement {
    use super::*;
    use test_case::test_case;

    #[test_case("do break beta; while(false);", &["beta", "green", "chocolate"], true, None => Ok((svec(&[
        "UNDEFINED",
        "BREAK_FROM 0 (beta)",
        "LOOP_CONT [beta, chocolate, green]",
        "JUMPPOP_FALSE 6",
        "COALESCE",
        "FALSE",
        "JUMPPOP_TRUE -10",
        "JUMP 1",
        "UPDATE_EMPTY"
    ]), true, false)); "dowhile/strict")]
    #[test_case("do break beta; while(false);", &["beta", "piano", "guitar"], false, None => Ok((svec(&[
        "UNDEFINED",
        "BREAK_FROM 0 (beta)",
        "LOOP_CONT [beta, guitar, piano]",
        "JUMPPOP_FALSE 6",
        "COALESCE",
        "FALSE",
        "JUMPPOP_TRUE -10",
        "JUMP 1",
        "UPDATE_EMPTY"
    ]), true, false)); "dowhile/nonstrict")]
    #[test_case("while(false);", &[], true, None => Ok((svec(&[
        "UNDEFINED",
        "FALSE",
        "JUMPPOP_FALSE 5",
        "EMPTY",
        "COALESCE",
        "JUMP -7",
        "UPDATE_EMPTY"
    ]), false, false)); "while stmt")]
    #[test_case("for(;;);", &[], true, None => Ok((svec(&[
        "UNDEFINED",
        "EMPTY",
        "COALESCE",
        "JUMP -4"
    ]), false, false)); "for stmt")]
    #[test_case("for(a in b);", &[], true, None => Ok((svec(&[
        "STRING 0 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 8",
        "JUMP_NULLISH 4",
        "TO_OBJECT",
        "ENUM_PROPS",
        "JUMP 2",
        "POP",
        "BREAK",
        "JUMP_IF_ABRUPT 51",
        "UNDEFINED",
        "SWAP",
        "ITER_NEXT",
        "JUMP_IF_ABRUPT 44",
        "IRES_COMPLETE",
        "JUMP_IF_ABRUPT 39",
        "JUMPPOP_TRUE 33",
        "IRES_TOVAL",
        "JUMP_IF_ABRUPT 36",
        "STRING 1 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 6",
        "SWAP",
        "PUT_VALUE",
        "JUMP_IF_NORMAL 6",
        "JUMP 25",
        "UNWIND 1",
        "JUMP -8",
        "POP",
        "SWAP",
        "EMPTY",
        "LOOP_CONT []",
        "JUMPPOP_FALSE 3",
        "COALESCE",
        "JUMP -37",
        "UPDATE_EMPTY",
        "UNWIND 1",
        "JUMP 8",
        "POP",
        "POP",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 2"
    ]), true, false)); "for-in stmt")]
    fn loop_compile(
        src: &str,
        labels: &[&str],
        strict: bool,
        spots_avail: Option<usize>,
    ) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).iteration_statement();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        let label_set = labels.iter().copied().map(JSString::from).collect::<Vec<JSString>>();
        node.loop_compile(&mut c, strict, src, &label_set)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod do_while_statement {
    use super::*;
    use test_case::test_case;

    #[test_case("do ; while(false);", &[], true, None => Ok((svec(&[
        "UNDEFINED",
        "EMPTY",
        "COALESCE",
        "FALSE",
        "JUMPPOP_TRUE -5",
    ]), false, false)); "literals only")]
    #[test_case("do a; while(false);", &[], true, Some(0) => serr("Out of room for strings in this compilation unit"); "stmt compile fails")]
    #[test_case("do break; while(false);", &["a"], true, None => Ok((svec(&[
        "UNDEFINED",
        "BREAK",
        "LOOP_CONT [a]",
        "JUMPPOP_FALSE 6",
        "COALESCE",
        "FALSE",
        "JUMPPOP_TRUE -9",
        "JUMP 1",
        "UPDATE_EMPTY",
    ]), true, false)); "abrubt statement possible")]
    #[test_case("do break; while(false);", &["a"], true, Some(0) => serr("Out of room for string sets in this compilation unit"); "label store fails")]
    #[test_case("do ; while(a);", &[], true, Some(0) => serr("Out of room for strings in this compilation unit"); "expr compilation fails")]
    #[test_case("do p; while(a);", &[], false, None => Ok((svec(&[
        "UNDEFINED",
        "STRING 0 (p)",
        "RESOLVE",
        "GET_VALUE",
        "LOOP_CONT []",
        "JUMPPOP_FALSE 15",
        "COALESCE",
        "STRING 1 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 5",
        "JUMPPOP_TRUE -21",
        "JUMP 1",
        "UPDATE_EMPTY",
    ]), true, false)); "stmt/expr fallible; nonstrict")]
    #[test_case("do p; while(a);", &[], true, None => Ok((svec(&[
        "UNDEFINED",
        "STRING 0 (p)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "LOOP_CONT []",
        "JUMPPOP_FALSE 15",
        "COALESCE",
        "STRING 1 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 5",
        "JUMPPOP_TRUE -21",
        "JUMP 1",
        "UPDATE_EMPTY",
    ]), true, false)); "stmt/expr fallible; strict")]
    #[test_case("do break; while(@@@);", &[], true, None => serr("out of range integral type conversion attempted"); "expr too large")]
    fn do_while_loop_compile(
        src: &str,
        labels: &[&str],
        strict: bool,
        spots_avail: Option<usize>,
    ) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).do_while_statement();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        let label_set = labels.iter().copied().map(JSString::from).collect::<Vec<JSString>>();
        node.do_while_loop_compile(&mut c, strict, src, &label_set)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod while_statement {
    use super::*;
    use test_case::test_case;

    #[test_case("while(false);", &[], false, &[] => Ok((svec(&["UNDEFINED", "FALSE", "JUMPPOP_FALSE 5", "EMPTY", "COALESCE", "JUMP -7", "UPDATE_EMPTY"]), false)); "simplest")]
    #[test_case("while(a);", &[], false, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "expr error")]
    #[test_case("while(a);", &[], false, &[] => Ok((svec(&["UNDEFINED", "STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 1", "JUMP 7", "JUMPPOP_FALSE 5", "EMPTY", "COALESCE", "JUMP -16", "UPDATE_EMPTY"]), true)); "expr reference")]
    #[test_case("while(a)a=!a;", &[], false, &[] => Ok((svec(&["UNDEFINED", "STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 1", "JUMP 28", "JUMPPOP_FALSE 26", "STRING 0 (a)", "RESOLVE", "JUMP_IF_ABRUPT 13", "STRING 0 (a)", "RESOLVE", "UNARY_NOT", "JUMP_IF_NORMAL 4", "SWAP", "POP", "JUMP 3", "POP2_PUSH3", "PUT_VALUE", "UPDATE_EMPTY", "LOOP_CONT []", "JUMPPOP_FALSE 3", "COALESCE", "JUMP -37", "UPDATE_EMPTY"]), true)); "stmt fallible")]
    #[test_case("while(false)a;", &[], false, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "stmt error")]
    #[test_case("while(true) break bob;", &["bob"], false, &[(Fillable::StringSet, 0)] => serr("Out of room for string sets in this compilation unit"); "lblset error")]
    #[test_case("while(false) @@@;", &[], false, &[] => serr("out of range integral type conversion attempted"); "loop body too big to jump over")]
    fn while_loop_compile(
        src: &str,
        labels: &[&str],
        strict: bool,
        what: &[(Fillable, usize)],
    ) -> Result<(Vec<String>, bool), String> {
        let node = Maker::new(src).while_statement();
        let mut c = complex_filled_chunk("x", what);
        let label_set = labels.iter().copied().map(JSString::from).collect::<Vec<JSString>>();
        node.while_loop_compile(&mut c, strict, src, &label_set)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod for_statement {
    use super::*;
    use test_case::test_case;

    #[test_case("for(;;);", &[], false, &[] => Ok((svec(&[
        "UNDEFINED",
        "EMPTY",
        "COALESCE",
        "JUMP -4"
    ]), false)); "for, no exprs")]
    #[test_case("for(1;;);", &[], false, &[] => Ok((svec(&[
        "FLOAT 0 (1)",
        "POP",
        "UNDEFINED",
        "EMPTY",
        "COALESCE",
        "JUMP -4"
    ]), false)); "for, infinite, infallible init")]
    #[test_case("for(a;;);", &[], false, &[(Fillable::String, 0)]
                => serr("Out of room for strings in this compilation unit")
                ; "for, init compile fail")]
    #[test_case("for(a;;);", &[], false, &[]
                => Ok((svec(&[
                    "STRING 0 (a)",
                    "RESOLVE",
                    "GET_VALUE",
                    "JUMP_IF_ABRUPT 6",
                    "POP",
                    "UNDEFINED",
                    "EMPTY",
                    "COALESCE",
                    "JUMP -4"
                ]), true))
                ; "for, init fallible")]
    #[test_case("for(;;)a;", &[], false, &[(Fillable::String, 0)]
                => serr("Out of room for strings in this compilation unit")
                ; "for, stmt compile fail")]
    #[test_case("for(;;)break;", &[], false, &[]
                => Ok((svec(&[
                    "UNDEFINED",
                    "BREAK",
                    "LOOP_CONT []",
                    "JUMPPOP_FALSE 3",
                    "COALESCE",
                    "JUMP -8",
                    "UPDATE_EMPTY"
                ]), true))
                ; "for, stmt fallible")]
    #[test_case("for(a;;)@@4;", &[], false, &[]
                => serr("out of range integral type conversion attempted")
                ; "for, abort from init too long")]
    #[test_case("for(var a;;);", &[], false, &[]
                => Ok((svec(&[
                    "EMPTY",
                    "POP",
                    "UNDEFINED",
                    "EMPTY",
                    "COALESCE",
                    "JUMP -4"
                ]), false))
                ; "for-var")]
    #[test_case("for(var a=b;;);", &[], false, &[(Fillable::String, 0)]
                => serr("Out of room for strings in this compilation unit")
                ; "for-var, vdl compile fails")]
    #[test_case("for(var a=b;;);", &[], false, &[]
                => Ok((svec(&[
                    "STRING 0 (a)",
                    "RESOLVE",
                    "JUMP_IF_ABRUPT 11",
                    "STRING 1 (b)",
                    "RESOLVE",
                    "GET_VALUE",
                    "JUMP_IF_NORMAL 4",
                    "UNWIND 1",
                    "JUMP 1",
                    "PUT_VALUE",
                    "JUMP_IF_ABRUPT 6",
                    "POP",
                    "UNDEFINED",
                    "EMPTY",
                    "COALESCE",
                    "JUMP -4"
                ]), true))
                ; "for-var, vdl fallible")]
    #[test_case("for(var a;;)101;", &[], false, &[(Fillable::Float, 0)]
                => serr("Out of room for floats in this compilation unit")
                ; "for-var, loop body compile fails")]
    #[test_case("for(var a;;)break;", &[], false, &[]
                => Ok((svec(&[
                    "EMPTY",
                    "POP",
                    "UNDEFINED",
                    "BREAK",
                    "LOOP_CONT []",
                    "JUMPPOP_FALSE 3",
                    "COALESCE",
                    "JUMP -8",
                    "UPDATE_EMPTY"
                ]), true))
                ; "for-var, fallible body")]
    #[test_case("for(var a=b;;)@@4;", &[], false, &[]
                => serr("out of range integral type conversion attempted")
                ; "for-var init escape too far")]
    #[test_case("for(let i=0;;);", &[], false, &[]
                => Ok((svec(&[
                    "PNLE",
                    "CPMLB 0 (i)",
                    "STRING 0 (i)",
                    "RESOLVE",
                    "FLOAT 0 (0)",
                    "IRB",
                    "JUMP_IF_ABRUPT 2",
                    "POP",
                    "EMPTY",
                    "JUMP_IF_ABRUPT 18",
                    "POP",
                    "UNDEFINED",
                    "CPIE [i]",
                    "JUMP_IF_ABRUPT 10",
                    "POP",
                    "EMPTY",
                    "COALESCE",
                    "CPIE [i]",
                    "JUMP_IF_ABRUPT 3",
                    "POP",
                    "JUMP -9",
                    "UNWIND 1",
                    "PLE"
                ]), true))
                ; "for-let")]
    #[test_case("for(let i=0;;);", &[], false, &[(Fillable::String, 0)]
                => serr("Out of room for strings in this compilation unit")
                ; "for-let; initial string stash fails")]
    #[test_case("for(const a=1;;);", &[], false, &[]
                => Ok((svec(&[
                    "PNLE",
                    "CSILB 0 (a)",
                    "STRING 0 (a)",
                    "RESOLVE",
                    "FLOAT 0 (1)",
                    "IRB",
                    "JUMP_IF_ABRUPT 2",
                    "POP",
                    "EMPTY",
                    "JUMP_IF_ABRUPT 6",
                    "POP",
                    "UNDEFINED",
                    "EMPTY",
                    "COALESCE",
                    "JUMP -4",
                    "PLE"
                ]), true));
                "for-let const form")]
    #[test_case("for(const a=1;;);", &[], false, &[(Fillable::Float, 0)]
                => serr("Out of room for floats in this compilation unit")
                ; "for-let init compile fails")]
    #[test_case("for(let a=0;;)8n;", &[], false, &[(Fillable::BigInt, 0)]
                => serr("Out of room for big ints in this compilation unit")
                ; "for-let body compile fails")]
    #[test_case("for(const a=0;;)@@4;", &[], false, &[]
                => serr("out of range integral type conversion attempted")
                ; "for-let init error jump too big")]
    #[test_case("for(let a=0;;);", &[], false, &[(Fillable::StringSet, 0)]
                => serr("Out of room for string sets in this compilation unit")
                ; "body-set-pool filled")]
    #[test_case("for(;true;);", &[], false, &[]
                => Ok((svec(&[
                    "UNDEFINED",
                    "TRUE",
                    "JUMPPOP_FALSE 4",
                    "EMPTY",
                    "COALESCE",
                    "JUMP -7"
                ]), false))
                ; "body: infallible test")]
    #[test_case("for(;a;);", &[], false, &[(Fillable::String, 0)]
                => serr("Out of room for strings in this compilation unit")
                ; "body test compilation fails")]
    #[test_case("for(;a;);", &[], false, &[]
                => Ok((svec(&[
                    "UNDEFINED",
                    "STRING 0 (a)",
                    "RESOLVE",
                    "GET_VALUE",
                    "JUMP_IF_ABRUPT 6",
                    "JUMPPOP_FALSE 6",
                    "EMPTY",
                    "COALESCE",
                    "JUMP -12",
                    "UNWIND 1"
                ]), true))
                ; "body test is ref")]
    #[test_case("for(;;)break;", &["lbl"], false, &[(Fillable::StringSet, 0)]
                => serr("Out of room for string sets in this compilation unit")
                ; "body no room for label sets")]
    #[test_case("for(;;a);", &[], false, &[]
                => Ok((svec(&[
                    "UNDEFINED",
                    "EMPTY",
                    "COALESCE",
                    "STRING 0 (a)",
                    "RESOLVE",
                    "GET_VALUE",
                    "JUMP_IF_ABRUPT 3",
                    "POP",
                    "JUMP -11",
                    "UNWIND 1"
                ]), true))
                ; "body - increment")]
    #[test_case("for(;;a);", &[], false, &[(Fillable::String, 0)]
                => serr("Out of room for strings in this compilation unit")
                ; "body - increment compilation fails")]
    #[test_case("for(;;a++);", &[], false, &[]
                => Ok((svec(&[
                    "UNDEFINED",
                    "EMPTY",
                    "COALESCE",
                    "STRING 0 (a)",
                    "RESOLVE",
                    "DUP",
                    "GET_VALUE",
                    "JUMP_IF_NORMAL 4",
                    "SWAP",
                    "POP",
                    "JUMP 11",
                    "TO_NUMERIC",
                    "JUMP_IF_NORMAL 4",
                    "SWAP",
                    "POP",
                    "JUMP 4",
                    "POP2_PUSH3",
                    "INCREMENT",
                    "PUT_VALUE",
                    "UPDATE_EMPTY",
                    "JUMP_IF_ABRUPT 3",
                    "POP",
                    "JUMP -29",
                    "UNWIND 1"
                ]), true))
                ; "body - increment not a ref")]
    #[test_case("for(;;true);", &[], false, &[]
                => Ok((svec(&[
                    "UNDEFINED",
                    "EMPTY",
                    "COALESCE",
                    "TRUE",
                    "POP",
                    "JUMP -6"
                ]), false))
                ; "body - increment infallible")]
    #[test_case("for(;;@@@);", &[], false, &[]
                => serr("out of range integral type conversion attempted")
                ; "body - loop back too large")]
    #[test_case("for(let x=0; x<10; x++) { @@9; }", &[], false, &[]
                => serr("out of range integral type conversion attempted")
                ; "body - unwind jump too large")]
    #[test_case("for(let x=0; x < 10; x++) { break able; }", &[], false, &[]
                => Ok((svec(&[
                    "PNLE",
                    "CPMLB 0 (x)",
                    "STRING 0 (x)",
                    "RESOLVE",
                    "FLOAT 0 (0)",
                    "IRB",
                    "JUMP_IF_ABRUPT 2",
                    "POP",
                    "EMPTY",
                    "JUMP_IF_ABRUPT 66",
                    "POP",
                    "UNDEFINED",
                    "CPIE [x]",
                    "JUMP_IF_ABRUPT 55",
                    "POP",
                    "STRING 0 (x)",
                    "RESOLVE",
                    "GET_VALUE",
                    "JUMP_IF_ABRUPT 3",
                    "FLOAT 1 (10)",
                    "LT",
                    "JUMP_IF_ABRUPT 43",
                    "JUMPPOP_FALSE 46",
                    "PNLE",
                    "BREAK_FROM 1 (able)",
                    "PLE",
                    "LOOP_CONT []",
                    "JUMPPOP_FALSE 37",
                    "COALESCE",
                    "CPIE [x]",
                    "JUMP_IF_ABRUPT 28",
                    "POP",
                    "STRING 0 (x)",
                    "RESOLVE",
                    "DUP",
                    "GET_VALUE",
                    "JUMP_IF_NORMAL 4",
                    "SWAP",
                    "POP",
                    "JUMP 11",
                    "TO_NUMERIC",
                    "JUMP_IF_NORMAL 4",
                    "SWAP",
                    "POP",
                    "JUMP 4",
                    "POP2_PUSH3",
                    "INCREMENT",
                    "PUT_VALUE",
                    "UPDATE_EMPTY",
                    "JUMP_IF_ABRUPT 3",
                    "POP",
                    "JUMP -54",
                    "UNWIND 1",
                    "JUMP 1",
                    "UPDATE_EMPTY",
                    "PLE"
                ]), true))
                ; "body - all the exit types")]
    fn compile(
        src: &str,
        labels: &[&str],
        strict: bool,
        what: &[(Fillable, usize)],
    ) -> Result<(Vec<String>, bool), String> {
        let node = Maker::new(src).for_statement();
        let mut c = complex_filled_chunk("x", what);
        let label_set = labels.iter().copied().map(JSString::from).collect::<Vec<JSString>>();
        node.compile_for_loop(&mut c, strict, src, &label_set)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                )
            })
            .map_err(|e| e.to_string())
    }
}
mod continue_statement {
    use super::*;
    use test_case::test_case;

    #[test_case("continue;", None => Ok((svec(&["CONTINUE"]), true, false)); "bare")]
    #[test_case("continue lbl;", None => Ok((svec(&["CONTINUE_WITH 0 (lbl)"]), true, false)); "targeted")]
    #[test_case("continue lbl;", Some(0) => serr("Out of room for strings in this compilation unit"); "no room for label")]
    fn compile(src: &str, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).continue_statement();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod break_statement {
    use super::*;
    use test_case::test_case;

    #[test_case("break;", None => Ok((svec(&["BREAK"]), true, false)); "bare")]
    #[test_case("break lbl;", None => Ok((svec(&["BREAK_FROM 0 (lbl)"]), true, false)); "targeted")]
    #[test_case("break lbl;", Some(0) => serr("Out of room for strings in this compilation unit"); "no room for label")]
    fn compile(src: &str, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).break_statement();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod switch_statement {
    use super::*;
    use test_case::test_case;

    #[test_case(
        "switch(a){case 1:@@(22);}", true, &[]
        => serr("out of range integral type conversion attempted");
        "case block too large"
    )]
    #[test_case(
        "switch(null){case 1n:}", true, &[(Fillable::BigInt, 0)]
        => serr("Out of room for big ints in this compilation unit");
        "case block compile fails"
    )]
    #[test_case(
        "switch(null){case 'a': let x;}", true, &[(Fillable::String, 0)]
        => serr("Out of room for strings in this compilation unit");
        "block instantiation fails"
    )]
    #[test_case(
        "switch(null){}", false, &[]
        => Ok((svec(&["NULL", "PNLE", "POP", "UNDEFINED", "PLE"]), false));
        "expr not ref, not fallible"
    )]
    #[test_case(
        "switch (1n) {}", false, &[(Fillable::BigInt, 0)]
        => serr("Out of room for big ints in this compilation unit");
        "expr compile fails"
    )]
    #[test_case(
        "switch (expr) {}", true, &[]
        => Ok((
            svec(&[
                "STRING 0 (expr)",
                "STRICT_RESOLVE",
                "GET_VALUE",
                "JUMP_IF_ABRUPT 4",
                "PNLE",
                "POP",
                "UNDEFINED",
                "PLE"
                ]),
            true,
        ));
        "typical"
    )]
    fn compile(src: &str, strict: bool, what: &[(Fillable, usize)]) -> Result<(Vec<String>, bool), String> {
        let node = Maker::new(src).switch_statement();
        let mut c = complex_filled_chunk("x", what);
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod function_declaration {
    use super::*;
    use test_case::test_case;

    #[test_case(None => svec(&["EMPTY"]); "typical")]
    fn compile(spots_avail: Option<usize>) -> Vec<String> {
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        FunctionDeclaration::compile(&mut c);
        c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>()
    }

    #[test_case("function named(){}", true, &[] => Ok((svec(&["FUNC_OBJ 0 named"]), true, false)); "named function")]
    #[test_case("function (){}", true, &[] => Ok((svec(&["FUNC_OBJ 0 default"]), true, false)); "unnamed function")]
    #[test_case("function (){}", true, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "no room for strings")]
    #[test_case("function (){}", true, &[(Fillable::FunctionStash, 0)] => serr("Out of room for more functions!"); "no room for functions")]
    fn compile_fo_instantiation(
        src: &str,
        strict: bool,
        slots_left: &[(Fillable, usize)],
    ) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).function_declaration();
        let mut c = complex_filled_chunk("x", slots_left);
        node.compile_fo_instantiation(&mut c, strict, src, node.clone())
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod function_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("function foo(){}", true, &[] => Ok((svec(&["STRING 0 (foo)", "FUNC_IOFE 0"]), true, false)); "typical")]
    fn compile(src: &str, strict: bool, what: &[(Fillable, usize)]) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).function_expression();
        let mut c = complex_filled_chunk("x", what);
        node.compile(&mut c, strict, src, node.clone())
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }

    #[test_case("function (){}", true, &[] => Ok((svec(&["FUNC_IIFE 0"]), true, false)); "typical")]
    fn compile_named_evaluation(
        src: &str,
        strict: bool,
        what: &[(Fillable, usize)],
    ) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).function_expression();
        let mut c = complex_filled_chunk("x", what);
        node.compile_named_evaluation(&mut c, strict, src, node.clone(), NameLoc::OnStack)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
    #[derive(Copy, Clone)]
    enum TestLoc {
        None,
        Stack,
        Index,
    }

    #[test_case("function (){}", TestLoc::None, true, &[] => Ok((svec(&["STRING 0 ()", "FUNC_IIFE 0"]), true, false)); "nameless")]
    #[test_case("function (){}", TestLoc::Stack, true, &[] => Ok((svec(&["FUNC_IIFE 0"]), true, false)); "name on stack")]
    #[test_case("function (){}", TestLoc::Index, true, &[] => Ok((svec(&["STRING 0 (myname)", "FUNC_IIFE 0"]), true, false)); "named")]
    #[test_case("function (){}", TestLoc::None, true, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "string table full")]
    #[test_case("function (){}", TestLoc::Stack, true, &[(Fillable::FunctionStash, 0)] => serr("Out of room for more functions!"); "function table full")]
    #[test_case("function a(){}", TestLoc::None, true, &[] => Ok((svec(&["STRING 0 (a)", "FUNC_IOFE 0"]), true, false)); "has ident")]
    #[test_case("function a(){}", TestLoc::None, true, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "ident + string table full")]
    #[test_case("function a(){}", TestLoc::None, true, &[(Fillable::FunctionStash, 0)] => serr("Out of room for more functions!"); "ident + function table full")]
    fn instantiate_ordinary_function_expression(
        src: &str,
        name: TestLoc,
        strict: bool,
        what: &[(Fillable, usize)],
    ) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).function_expression();
        let mut c = complex_filled_chunk("x", what);
        let name = match name {
            TestLoc::None => NameLoc::None,
            TestLoc::Stack => NameLoc::OnStack,
            TestLoc::Index => NameLoc::Index(c.add_to_string_pool("myname".into()).unwrap()),
        };
        node.instantiate_ordinary_function_expression(&mut c, strict, name, src, node.clone())
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod labelled_item {
    use super::*;
    use test_case::test_case;

    #[test_case("function b(){}", &[], true, None => Ok((svec(&["EMPTY"]), false, false)); "function def")]
    #[test_case("do x; while (true);", &["b"], true, None => Ok((svec(&[
        "UNDEFINED",
        "STRING 0 (x)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "LOOP_CONT [b]",
        "JUMPPOP_FALSE 6",
        "COALESCE",
        "TRUE",
        "JUMPPOP_TRUE -12",
        "JUMP 1",
        "UPDATE_EMPTY",
        "HEB",
    ]), true, false)); "statement/strict")]
    #[test_case("do x; while (true);", &["b"], false, None => Ok((svec(&[
        "UNDEFINED",
        "STRING 0 (x)",
        "RESOLVE",
        "GET_VALUE",
        "LOOP_CONT [b]",
        "JUMPPOP_FALSE 6",
        "COALESCE",
        "TRUE",
        "JUMPPOP_TRUE -12",
        "JUMP 1",
        "UPDATE_EMPTY",
        "HEB",
    ]), true, false)); "statement/nonstrict")]
    fn labelled_compile(
        src: &str,
        labels: &[&str],
        strict: bool,
        spots_avail: Option<usize>,
    ) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).labelled_item();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        let label_set = labels.iter().copied().map(JSString::from).collect::<Vec<JSString>>();
        node.labelled_compile(&mut c, strict, src, &label_set)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod labelled_statement {
    use super::*;
    use test_case::test_case;

    #[test_case("a:do x; while (true);", &["b"], true, None => Ok((svec(&[
        "UNDEFINED",
        "STRING 0 (x)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "LOOP_CONT [a, b]",
        "JUMPPOP_FALSE 6",
        "COALESCE",
        "TRUE",
        "JUMPPOP_TRUE -12",
        "JUMP 1",
        "UPDATE_EMPTY",
        "HEB",
        "HTB 1 (a)"
    ]), true, false)); "statement/strict")]
    #[test_case("a:do x; while (true);", &["b"], false, None => Ok((svec(&[
        "UNDEFINED",
        "STRING 0 (x)",
        "RESOLVE",
        "GET_VALUE",
        "LOOP_CONT [a, b]",
        "JUMPPOP_FALSE 6",
        "COALESCE",
        "TRUE",
        "JUMPPOP_TRUE -12",
        "JUMP 1",
        "UPDATE_EMPTY",
        "HEB",
        "HTB 1 (a)"
    ]), true, false)); "statement/nonstrict")]
    #[test_case("a:;", &["b"], true, None => Ok((svec(&["EMPTY"]), false, false)); "non-abrupt statement")]
    #[test_case("a:p;", &["b"], true, Some(0) => serr("Out of room for strings in this compilation unit"); "stmt compile fail")]
    #[test_case("a:break;", &["b"], true, Some(0) => serr("Out of room for strings in this compilation unit"); "label store fail")]
    fn labelled_compile(
        src: &str,
        labels: &[&str],
        strict: bool,
        spots_avail: Option<usize>,
    ) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).labelled_statement();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        let label_set = labels.iter().copied().map(JSString::from).collect::<Vec<JSString>>();
        node.labelled_compile(&mut c, strict, src, &label_set)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }

    #[test_case("a:do x; while(true);", true, None => Ok((svec(&[
        "UNDEFINED",
        "STRING 0 (x)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "LOOP_CONT [a]",
        "JUMPPOP_FALSE 6",
        "COALESCE",
        "TRUE",
        "JUMPPOP_TRUE -12",
        "JUMP 1",
        "UPDATE_EMPTY",
        "HEB",
        "HTB 1 (a)"
    ]), true, false)); "stmt/strict")]
    #[test_case("a:do x; while(true);", false, None => Ok((svec(&[
        "UNDEFINED",
        "STRING 0 (x)",
        "RESOLVE",
        "GET_VALUE",
        "LOOP_CONT [a]",
        "JUMPPOP_FALSE 6",
        "COALESCE",
        "TRUE",
        "JUMPPOP_TRUE -12",
        "JUMP 1",
        "UPDATE_EMPTY",
        "HEB",
        "HTB 1 (a)"
    ]), true, false)); "stmt/nonstrict")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).labelled_statement();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod binding_identifier {
    use super::*;
    use test_case::test_case;

    #[test_case("alpha", true, EnvUsage::UsePutValue, None => Ok((svec(&["STRING 0 (alpha)", "STRICT_RESOLVE", "SWAP", "PUT_VALUE", "POP_PANIC"]), false, false)); "strict/env/normal")]
    #[test_case("alpha", false, EnvUsage::UsePutValue, None => Ok((svec(&["STRING 0 (alpha)", "RESOLVE", "SWAP", "PUT_VALUE", "POP_PANIC"]), false, false)); "non-strict/env/normal")]
    #[test_case("alpha", true, EnvUsage::UseCurrentLexical, None => Ok((svec(&["ILB 0 (alpha)"]), false, false)); "strict/no_dupes/normal")]
    #[test_case("alpha", false, EnvUsage::UseCurrentLexical, None => Ok((svec(&["ILB 0 (alpha)"]), false, false)); "non-strict/no_dupes/normal")]
    #[test_case("yield", true, EnvUsage::UseCurrentLexical, None => Ok((svec(&["ILB 0 (yield)"]), false, false)); "strict/no_dupes/yield")]
    #[test_case("await", true, EnvUsage::UseCurrentLexical, None => Ok((svec(&["ILB 0 (await)"]), false, false)); "strict/no_dupes/await")]
    #[test_case("alpha", true, EnvUsage::UseCurrentLexical, Some(0) => serr("Out of room for strings in this compilation unit"); "no space left")]
    fn compile_binding_initialization(
        src: &str,
        strict: bool,
        env: EnvUsage,
        spots_avail: Option<usize>,
    ) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).yield_ok(false).await_ok(false).binding_identifier();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile_binding_initialization(&mut c, strict, env)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }

    #[test_case("a", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE"
    ]), true, true)); "strict identifier")]
    #[test_case("a", false, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE"
    ]), true, true)); "non-strict identifier")]
    #[test_case("yield", false, &[] => Ok((svec(&[
        "STRING 0 (yield)",
        "RESOLVE"
    ]), true, true)); "non-strict yield")]
    #[test_case("await", false, &[] => Ok((svec(&[
        "STRING 0 (await)",
        "RESOLVE"
    ]), true, true)); "non-strict await")]
    #[test_case("a", true, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "string table full")]
    fn compile(src: &str, strict: bool, what: &[(Fillable, usize)]) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).yield_ok(false).await_ok(false).binding_identifier();
        let mut c = complex_filled_chunk("x", what);
        node.compile(&mut c, strict)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod binding_element {
    use super::*;
    use test_case::test_case;

    #[test_case("alpha", true, EnvUsage::UseCurrentLexical, None => Ok((svec(&[
        "EXTRACT_ARG",
        "STRING 0 (alpha)",
        "STRICT_RESOLVE",
        "SWAP",
        "IRB",
        "POP"
    ]), false, false)); "single name/strict")]
    #[test_case("{alpha}", true, EnvUsage::UseCurrentLexical, None => Ok((svec(&[
        "EXTRACT_ARG",
        "REQ_COER",
        "JUMP_IF_ABRUPT 30",
        "STRING 0 (alpha)",
        "STRING 0 (alpha)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 8",
        "ROTATEDOWN 3",
        "GETV",
        "JUMP_IF_ABRUPT 5",
        "IRB",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 5",
        "POP",
        "STRING 0 (alpha)",
        "FLOAT 0 (1)",
        "JUMP_IF_ABRUPT 2",
        "POP_LIST",
        "EMPTY",
        "JUMP_IF_ABRUPT 3",
        "POP",
        "JUMP 1",
        "UNWIND_LIST"
    ]), true, false)); "no-init pattern")]
    #[test_case("{a}", true, EnvUsage::UseCurrentLexical, Some(0) => serr("Out of room for strings in this compilation unit"); "string table full, no init")]
    #[test_case("{alpha}=beta", true, EnvUsage::UseCurrentLexical, None => Ok((svec(&[
        "EXTRACT_ARG",
        "JUMP_NOT_UNDEF 7",
        "POP",
        "STRING 0 (beta)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 35",
        "REQ_COER",
        "JUMP_IF_ABRUPT 30",
        "STRING 1 (alpha)",
        "STRING 1 (alpha)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 8",
        "ROTATEDOWN 3",
        "GETV",
        "JUMP_IF_ABRUPT 5",
        "IRB",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 5",
        "POP",
        "STRING 1 (alpha)",
        "FLOAT 0 (1)",
        "JUMP_IF_ABRUPT 2",
        "POP_LIST",
        "EMPTY",
        "JUMP_IF_NORMAL 1",
        "UNWIND_LIST"
    ]), true, false)); "init pattern")]
    #[test_case("{alpha}=3", true, EnvUsage::UseCurrentLexical, None => Ok((svec(&[
        "EXTRACT_ARG",
        "JUMP_NOT_UNDEF 3",
        "POP",
        "FLOAT 0 (3)",
        "REQ_COER",
        "JUMP_IF_ABRUPT 30",
        "STRING 0 (alpha)",
        "STRING 0 (alpha)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 8",
        "ROTATEDOWN 3",
        "GETV",
        "JUMP_IF_ABRUPT 5",
        "IRB",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 5",
        "POP",
        "STRING 0 (alpha)",
        "FLOAT 1 (1)",
        "JUMP_IF_ABRUPT 2",
        "POP_LIST",
        "EMPTY",
        "JUMP_IF_NORMAL 1",
        "UNWIND_LIST"
    ]), true, false)); "init by errorfree")]
    #[test_case("{alpha}=beta", false, EnvUsage::UseCurrentLexical, Some(0) => serr("Out of room for strings in this compilation unit"); "no room")]
    #[test_case("{alhpa}=@@@", false, EnvUsage::UseCurrentLexical, None => serr("out of range integral type conversion attempted"); "initializer too large")]
    #[test_case("{alpha}=xxx", false, EnvUsage::UseCurrentLexical, Some(1) => serr("Out of room for strings in this compilation unit"); "almost no room")]
    #[test_case("{alhpa=@@(39)}=a", false, EnvUsage::UsePutValue, None => serr("out of range integral type conversion attempted"); "pattern too large")]
    fn compile_binding_initialization(
        src: &str,
        strict: bool,
        env: EnvUsage,
        spots_avail: Option<usize>,
    ) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).binding_element();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile_binding_initialization(&mut c, strict, src, env)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }

    #[test_case("{a}", false, EnvUsage::UsePutValue, &[] => Ok(svec(&[
        "ITER_STEP",
        "JUMP_IF_ABRUPT 40",
        "REQ_COER",
        "JUMP_IF_ABRUPT 30",
        "STRING 0 (a)",
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 8",
        "ROTATEDOWN 3",
        "GETV",
        "JUMP_IF_ABRUPT 5",
        "PUT_VALUE",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 5",
        "POP",
        "STRING 0 (a)",
        "FLOAT 0 (1)",
        "JUMP_IF_ABRUPT 2",
        "POP_LIST",
        "EMPTY",
        "JUMP_IF_ABRUPT 3",
        "POP",
        "JUMP 2",
        "UNWIND 1"
    ])); "non-strict/putvalue/has_izer")]
    #[test_case("{a}", false, EnvUsage::UseCurrentLexical, &[] => Ok(svec(&[
        "ITER_STEP",
        "JUMP_IF_ABRUPT 40",
        "REQ_COER",
        "JUMP_IF_ABRUPT 30",
        "STRING 0 (a)",
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 8",
        "ROTATEDOWN 3",
        "GETV",
        "JUMP_IF_ABRUPT 5",
        "IRB",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 5",
        "POP",
        "STRING 0 (a)",
        "FLOAT 0 (1)",
        "JUMP_IF_ABRUPT 2",
        "POP_LIST",
        "EMPTY",
        "JUMP_IF_ABRUPT 3",
        "POP",
        "JUMP 2",
        "UNWIND 1"
    ])); "non-strict/currentlex/has_izer")]
    #[test_case("{a}", true, EnvUsage::UsePutValue, &[] => Ok(svec(&[
        "ITER_STEP",
        "JUMP_IF_ABRUPT 40",
        "REQ_COER",
        "JUMP_IF_ABRUPT 30",
        "STRING 0 (a)",
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 8",
        "ROTATEDOWN 3",
        "GETV",
        "JUMP_IF_ABRUPT 5",
        "PUT_VALUE",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 5",
        "POP",
        "STRING 0 (a)",
        "FLOAT 0 (1)",
        "JUMP_IF_ABRUPT 2",
        "POP_LIST",
        "EMPTY",
        "JUMP_IF_ABRUPT 3",
        "POP",
        "JUMP 2",
        "UNWIND 1"
    ])); "strict/putvalue/has_izer")]
    #[test_case("{a}", true, EnvUsage::UseCurrentLexical, &[] => Ok(svec(&[
        "ITER_STEP",
        "JUMP_IF_ABRUPT 40",
        "REQ_COER",
        "JUMP_IF_ABRUPT 30",
        "STRING 0 (a)",
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 8",
        "ROTATEDOWN 3",
        "GETV",
        "JUMP_IF_ABRUPT 5",
        "IRB",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 5",
        "POP",
        "STRING 0 (a)",
        "FLOAT 0 (1)",
        "JUMP_IF_ABRUPT 2",
        "POP_LIST",
        "EMPTY",
        "JUMP_IF_ABRUPT 3",
        "POP",
        "JUMP 2",
        "UNWIND 1"
    ])); "strict/currentlex/has_izer")]
    #[test_case("a", false, EnvUsage::UsePutValue, &[] => Ok(svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 13",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 9",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 3",
        "POP",
        "JUMP 2",
        "UNWIND 1"
    ])); "single-name fall-thru")]
    #[test_case("[a]=[]", false, EnvUsage::UsePutValue, &[] => Ok(svec(&[
        "ITER_STEP",
        "JUMP_IF_ABRUPT 37",
        "JUMP_NOT_UNDEF 2",
        "POP",
        "ARRAY",
        "GET_SYNC_ITER",
        "JUMP_IF_ABRUPT 23",
        "DUP",
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 13",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 9",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 3",
        "POP",
        "JUMP 2",
        "UNWIND 1",
        "EMPTY_IF_NOT_ERR",
        "ITER_CLOSE_IF_NOT_DONE",
        "JUMP_IF_ABRUPT 3",
        "POP",
        "JUMP 2",
        "UNWIND 1"
    ])); "pattern; infallible initializer")]
    #[test_case("[a]=8n", false, EnvUsage::UsePutValue, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "izer compile fails")]
    #[test_case("[a]=b", false, EnvUsage::UsePutValue, &[] => Ok(svec(&[
        "ITER_STEP",
        "JUMP_IF_ABRUPT 42",
        "JUMP_NOT_UNDEF 7",
        "POP",
        "STRING 0 (b)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 31",
        "GET_SYNC_ITER",
        "JUMP_IF_ABRUPT 23",
        "DUP",
        "STRING 1 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 13",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 9",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 3",
        "POP",
        "JUMP 2",
        "UNWIND 1",
        "EMPTY_IF_NOT_ERR",
        "ITER_CLOSE_IF_NOT_DONE",
        "JUMP_IF_ABRUPT 3",
        "POP",
        "JUMP 2",
        "UNWIND 1"
    ])); "izer is ref")]
    #[test_case("[a]=@@@", false, EnvUsage::UsePutValue, &[] => serr("out of range integral type conversion attempted"); "izer jump too far")]
    #[test_case("[a=8n]=b", false, EnvUsage::UsePutValue, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "pattern compile fails")]
    #[test_case("[a=@@(34)]=b", false, EnvUsage::UsePutValue, &[] => serr("out of range integral type conversion attempted"); "pattern too big")]
    #[test_case("[a]=@@(36)", false, EnvUsage::UsePutValue, &[] => serr("out of range integral type conversion attempted"); "overall too big")]
    fn iterator_binding_initialization(
        src: &str,
        strict: bool,
        env: EnvUsage,
        what: &[(Fillable, usize)],
    ) -> Result<Vec<String>, String> {
        let node = Maker::new(src).binding_element();
        let mut c = complex_filled_chunk("x", what);
        node.iterator_binding_initialization(&mut c, strict, src, env)
            .map(|_| c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>())
            .map_err(|e| e.to_string())
    }

    #[test_case("a", false, EnvUsage::UsePutValue, &[] => Ok(svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 8",
        "ROTATEDOWN 3",
        "GETV",
        "JUMP_IF_ABRUPT 5",
        "PUT_VALUE",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 1"
    ])); "single-name fallthru")]
    #[test_case("[a]", false, EnvUsage::UsePutValue, &[] => Ok(svec(&[
        "GETV",
        "JUMP_IF_ABRUPT 26",
        "GET_SYNC_ITER",
        "JUMP_IF_ABRUPT 23",
        "DUP",
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 13",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 9",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 3",
        "POP",
        "JUMP 2",
        "UNWIND 1",
        "EMPTY_IF_NOT_ERR",
        "ITER_CLOSE_IF_NOT_DONE"
    ])); "no-init/non-strict/putvalue")]
    #[test_case("[a]=b", true, EnvUsage::UseCurrentLexical, &[] => Ok(svec(&[
        "GETV",
        "JUMP_IF_ABRUPT 35",
        "JUMP_NOT_UNDEF 7",
        "POP",
        "STRING 0 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 26",
        "GET_SYNC_ITER",
        "JUMP_IF_ABRUPT 23",
        "DUP",
        "STRING 1 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 13",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 9",
        "SWAP",
        "ROTATEDOWN 3",
        "IRB",
        "JUMP_IF_ABRUPT 3",
        "POP",
        "JUMP 2",
        "UNWIND 1",
        "EMPTY_IF_NOT_ERR",
        "ITER_CLOSE_IF_NOT_DONE"
    ])); "initializer ref")]
    #[test_case("[a]=9n", false, EnvUsage::UsePutValue, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "izer compile fails")]
    #[test_case("[a]=0", false, EnvUsage::UsePutValue, &[] => Ok(svec(&[
        "GETV",
        "JUMP_IF_ABRUPT 31",
        "JUMP_NOT_UNDEF 3",
        "POP",
        "FLOAT 0 (0)",
        "GET_SYNC_ITER",
        "JUMP_IF_ABRUPT 23",
        "DUP",
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 13",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 9",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 3",
        "POP",
        "JUMP 2",
        "UNWIND 1",
        "EMPTY_IF_NOT_ERR",
        "ITER_CLOSE_IF_NOT_DONE"
    ])); "infallibe izer")]
    #[test_case("[a]=@@@", false, EnvUsage::UsePutValue, &[] => serr("out of range integral type conversion attempted"); "vok branch too large")]
    #[test_case("[a=9n]=b", false, EnvUsage::UsePutValue, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "pattern compile fails")]
    #[test_case("[a=@@(29)]", false, EnvUsage::UsePutValue, &[] => serr("out of range integral type conversion attempted"); "exit branch too large")]
    fn keyed_binding_initialization(
        src: &str,
        strict: bool,
        env: EnvUsage,
        what: &[(Fillable, usize)],
    ) -> Result<Vec<String>, String> {
        let node = Maker::new(src).binding_element();
        let mut c = complex_filled_chunk("x", what);
        node.keyed_binding_initialization(&mut c, strict, src, env)
            .map(|_| c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>())
            .map_err(|e| e.to_string())
    }
}

mod binding_property {
    use super::*;
    use test_case::test_case;

    #[test_case("a", false, EnvUsage::UsePutValue, &[] => Ok(svec(&[
        "STRING 0 (a)",
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 8",
        "ROTATEDOWN 3",
        "GETV",
        "JUMP_IF_ABRUPT 5",
        "PUT_VALUE",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 5",
        "POP",
        "STRING 0 (a)",
        "FLOAT 0 (1)"
    ])); "simple name")]
    #[test_case("a", false, EnvUsage::UsePutValue, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "single name: string table full")]
    #[test_case("a", false, EnvUsage::UsePutValue, &[(Fillable::Float, 0)] => serr("Out of room for floats in this compilation unit"); "single name: float table full")]
    #[test_case("a=9n", false, EnvUsage::UsePutValue, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "single name: binding init fails")]
    #[test_case("a:b", false, EnvUsage::UseCurrentLexical, &[] => Ok(svec(&[
        "STRING 0 (a)",
        "POP2_PUSH3",
        "STRING 1 (b)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 8",
        "ROTATEDOWN 3",
        "GETV",
        "JUMP_IF_ABRUPT 5",
        "IRB",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 5",
        "POP",
        "FLOAT 0 (1)",
        "JUMP 2",
        "UNWIND 1"
    ])); "property; simple element")]
    #[test_case("a:b", false, EnvUsage::UseCurrentLexical, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "propname compile fails")]
    #[test_case("[a]:b", true, EnvUsage::UsePutValue, &[] => Ok(svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 1",
        "TO_KEY",
        "JUMP_IF_ABRUPT 25",
        "POP2_PUSH3",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 8",
        "ROTATEDOWN 3",
        "GETV",
        "JUMP_IF_ABRUPT 5",
        "PUT_VALUE",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 5",
        "POP",
        "FLOAT 0 (1)",
        "JUMP 2",
        "UNWIND 1"
    ])); "fallible propname")]
    #[test_case("a:b=8n", true, EnvUsage::UsePutValue, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "element compile fail")]
    #[test_case("a:b", false, EnvUsage::UsePutValue, &[(Fillable::Float, 0)] => serr("Out of room for floats in this compilation unit"); "propstyle: float table full")]
    #[test_case("[a]:b=@@(29)", false, EnvUsage::UsePutValue, &[] => serr("out of range integral type conversion attempted"); "unwind jump too far")]
    fn property_binding_initialization(
        src: &str,
        strict: bool,
        env: EnvUsage,
        what: &[(Fillable, usize)],
    ) -> Result<Vec<String>, String> {
        let node = Maker::new(src).binding_property();
        let mut c = complex_filled_chunk("x", what);
        node.property_binding_initialization(&mut c, strict, src, env)
            .map(|_| c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>())
            .map_err(|e| e.to_string())
    }
}

mod binding_pattern {
    use super::*;
    use test_case::test_case;

    #[test_case("{a}", true, EnvUsage::UseCurrentLexical, &[] => Ok(svec(&[
        "REQ_COER",
        "JUMP_IF_ABRUPT 30",
        "STRING 0 (a)",
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 8",
        "ROTATEDOWN 3",
        "GETV",
        "JUMP_IF_ABRUPT 5",
        "IRB",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 5",
        "POP",
        "STRING 0 (a)",
        "FLOAT 0 (1)",
        "JUMP_IF_ABRUPT 2",
        "POP_LIST",
        "EMPTY"
    ])); "simple")]
    #[test_case("{a}", true, EnvUsage::UseCurrentLexical, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "no space")]
    #[test_case("{a=@@(34)}", true, EnvUsage::UseCurrentLexical, &[] => serr("out of range integral type conversion attempted"); "exit jump too far")]
    #[test_case("[a]", true, EnvUsage::UseCurrentLexical, &[] => Ok(svec(&[
        "GET_SYNC_ITER",
        "JUMP_IF_ABRUPT 23",
        "DUP",
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 13",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 9",
        "SWAP",
        "ROTATEDOWN 3",
        "IRB",
        "JUMP_IF_ABRUPT 3",
        "POP",
        "JUMP 2",
        "UNWIND 1",
        "EMPTY_IF_NOT_ERR",
        "ITER_CLOSE_IF_NOT_DONE"
    ])); "array pattern; typical")]
    #[test_case("[a=1n]", false, EnvUsage::UsePutValue, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "pattern compile fails")]
    #[test_case("[a=@@(20)]", false, EnvUsage::UsePutValue, &[] => serr("out of range integral type conversion attempted"); "array: exit jump too far")]
    fn compile_binding_initialization(
        src: &str,
        strict: bool,
        env: EnvUsage,
        what: &[(Fillable, usize)],
    ) -> Result<Vec<String>, String> {
        let node = Maker::new(src).binding_pattern();
        let mut c = complex_filled_chunk("x", what);
        node.compile_binding_initialization(&mut c, strict, src, env)
            .map(|_| c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>())
            .map_err(|e| e.to_string())
    }
}

mod return_statement {
    use super::*;
    use test_case::test_case;

    #[test_case("return;", true, &[] => Ok((svec(&["UNDEFINED", "RETURN"]), true, false)); "bare return")]
    #[test_case("return 3;", true, &[] => Ok((svec(&["FLOAT 0 (3)", "RETURN"]), true, false)); "literal return")]
    #[test_case("return a;", true, &[] => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 1", "RETURN"]), true, false)); "exp return; strict")]
    #[test_case("return a;", false, &[] => Ok((svec(&["STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 1", "RETURN"]), true, false)); "exp return; non-strict")]
    #[test_case("return a;", true, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "expr compilation fails")]
    fn compile(src: &str, strict: bool, what: &[(Fillable, usize)]) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).return_statement();
        let mut c = complex_filled_chunk("x", what);
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod compile_fdi {
    use super::*;
    use test_case::test_case;

    fn function(src: &str, strict: bool) -> (StashedFunctionData, String) {
        let fd = Maker::new(src).function_declaration();
        let span = fd.location().span;
        let source_text = src[span.starting_index..(span.starting_index + span.length)].to_string();

        let stash = StashedFunctionData {
            source_text,
            params: fd.params.clone().into(),
            body: fd.body.clone().into(),
            to_compile: fd.into(),
            strict,
            this_mode: ThisLexicality::NonLexicalThis,
        };
        (stash, src.into())
    }

    fn insane(strict: bool) -> (StashedFunctionData, String) {
        // lots of variables in one statement, to overflow jumps
        let header = "function insane(a=b){var v0";
        let trailer = ";}";
        let mut src = String::with_capacity(6554 * 6) + header;
        for idx in 1..6554 {
            write!(src, ",v{idx}").unwrap();
        }
        src += trailer;
        function(&src, strict)
    }

    #[test_case(|s| function("function a(){}", s), true, &[] => Ok((svec(&["CUA", "CNSILB 0 (arguments)", "ILB 0 (arguments)", "FINISH_ARGS"]), false, false)); "simplest/strict")]
    #[test_case(|s| function("function a(){}", s), false, &[] => Ok((svec(&["CMA", "CPMLB 0 (arguments)", "ILB 0 (arguments)", "FINISH_ARGS", "PNLE"]), false, false)); "simplest/non-strict")]
    #[test_case(|s| function("function a(x){ function one() { return 1; } function two() { return 2; } function one() { return 42; } }", s), true, &[] => Ok((svec(&[
        "CPMLBM 0 (x)",
        "CUA",
        "CNSILB 1 (arguments)",
        "ILB 1 (arguments)",
        "EXTRACT_ARG",
        "STRING 0 (x)",
        "STRICT_RESOLVE",
        "SWAP",
        "IRB",
        "POP",
        "FINISH_ARGS",
        "CPMLB 2 (one)",
        "UNDEFINED",
        "ILB 2 (one)",
        "CPMLB 3 (two)",
        "UNDEFINED",
        "ILB 3 (two)",
        "FUNC_OBJ 0 two",
        "SMVB 3 (two)",
        "FUNC_OBJ 1 one",
        "SMVB 2 (one)"
    ]), false, false)); "multiple inner functions")]
    #[test_case(|s| function("function(x=blue()){}", s), false, &[] => Ok((svec(&[
        "PNLE",
        "CPMLBM 0 (x)",
        "CUA",
        "CPMLB 1 (arguments)",
        "ILB 1 (arguments)",
        "EXTRACT_ARG",
        "STRING 0 (x)",
        "RESOLVE",
        "SWAP",
        "JUMP_NOT_UNDEF 22",
        "POP",
        "STRING 2 (blue)",
        "RESOLVE",
        "DUP",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 3",
        "FLOAT 0 (0)",
        "CALL",
        "JUMP_IF_NORMAL 5",
        "UNWIND 1",
        "UNWIND_LIST",
        "JUMP 2",
        "IRB",
        "POP",
        "JUMP_IF_ABRUPT 3",
        "FINISH_ARGS",
        "PNVEFL",
        "PNLEFV"
    ]), true, false)); "has_parameter_expressions")]
    #[test_case(|s| function("function a(x=0, x=3){}", s), false, &[] => Ok((svec(&[
        "PNLE",
        "CIPMLBM 0 (x)",
        "CIPMLBM 0 (x)",
        "CUA",
        "CPMLB 1 (arguments)",
        "ILB 1 (arguments)",
        "EXTRACT_ARG",
        "STRING 0 (x)",
        "RESOLVE",
        "SWAP",
        "JUMP_NOT_UNDEF 3",
        "POP",
        "FLOAT 0 (0)",
        "PUT_VALUE",
        "POP",
        "EXTRACT_ARG",
        "STRING 0 (x)",
        "RESOLVE",
        "SWAP",
        "JUMP_NOT_UNDEF 3",
        "POP",
        "FLOAT 1 (3)",
        "PUT_VALUE",
        "POP",
        "FINISH_ARGS",
        "PNVEFL",
        "PNLEFV"
    ]), false, false)); "duplicates in parameters")]
    #[test_case(|s| function("function x(a){}", s), false, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "string table full (parameters)")]
    #[test_case(|s| function("function x(){}", s), false, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "string table full (args obj)")]
    #[test_case(|s| function("function a(){let arguments = null;}", s), false, &[] => Ok((svec(&["FINISH_ARGS", "PNLE", "CPMLB 0 (arguments)"]), false, false)); "no args object")]
    #[test_case(|s| function("function a(first=canfail(), b=@@@){}", s), false, &[] => serr("out of range integral type conversion attempted"); "failed parameter compilation")]
    #[test_case(|s| function("function a(){var b;}", s), true, &[(Fillable::String, 1)] => serr("Out of room for strings in this compilation unit"); "string table full (var instantiation)")]
    #[test_case(|s| function("function a(x=0){var a, x; var a;}", s), true, &[] => Ok((svec(&[
        "CPMLBM 0 (x)",
        "CUA",
        "CNSILB 1 (arguments)",
        "ILB 1 (arguments)",
        "EXTRACT_ARG",
        "STRING 0 (x)",
        "STRICT_RESOLVE",
        "SWAP",
        "JUMP_NOT_UNDEF 3",
        "POP",
        "FLOAT 0 (0)",
        "IRB",
        "POP",
        "FINISH_ARGS",
        "PNVEFL",
        "CPMVB 2 (a)",
        "UNDEFINED",
        "IVB 2 (a)",
        "CPMVB 0 (x)",
        "GLB 0 (x)",
        "IVB 0 (x)",
        "SLETVE"
    ]), false, false)); "var instantiation")]
    #[test_case(|s| function("function a(x=0){var a, x; var a;}", s), false, &[(Fillable::String, 2)] => serr("Out of room for strings in this compilation unit"); "string table full (var binding)")]
    #[test_case(|s| function("function a(){let x; const y=1;}", s), false, &[] => Ok((svec(&[
        "CMA", "CPMLB 0 (arguments)", "ILB 0 (arguments)", "FINISH_ARGS", "PNLE", "CPMLB 1 (x)", "CSILB 2 (y)"
    ]), false, false)); "lexical instantiation")]
    #[test_case(|s| function("function a(){let x; const y=1;}", s), false, &[(Fillable::String, 1)] => serr("Out of room for strings in this compilation unit"); "string table full (lexical instantiation)")]
    #[test_case(|s| function("function a(){function b(){}}", s), false, &[(Fillable::FunctionStash, 0)] => serr("Out of room for more functions!"); "function table full (function initialization)")]
    #[test_case(insane, false, &[] => serr("out of range integral type conversion attempted"); "branch too far")]
    fn compile_fdi(
        make_function: impl FnOnce(bool) -> (StashedFunctionData, String),
        strict: bool,
        what: &[(Fillable, usize)],
    ) -> Result<(Vec<String>, bool, bool), String> {
        let (info, text) = make_function(strict);
        let mut c = complex_filled_chunk("compile-fdi-test", what);
        super::compile_fdi(&mut c, &text, &info)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod arrow_function {
    use super::*;
    use test_case::test_case;
    #[derive(Copy, Clone)]
    enum TestLoc {
        None,
        Stack,
        Index,
    }

    #[test_case(TestLoc::None, &[] => Ok((svec(&["STRING 0 ()", "FUNC_IAE 0"]), true, false)); "nameless")]
    #[test_case(TestLoc::Stack, &[] => Ok((svec(&["FUNC_IAE 0"]), true, false)); "name on stack")]
    #[test_case(TestLoc::Index, &[] => Ok((svec(&["STRING 0 (myname)", "FUNC_IAE 0"]), true, false)); "named")]
    #[test_case(TestLoc::None, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "string table full")]
    #[test_case(TestLoc::Stack, &[(Fillable::FunctionStash, 0)] => serr("Out of room for more functions!"); "function table full")]
    fn instantiate_arrow_function_expression(
        name: TestLoc,
        what: &[(Fillable, usize)],
    ) -> Result<(Vec<String>, bool, bool), String> {
        let src = "x=>x";
        let strict = true;
        let node = Maker::new(src).arrow_function();
        let mut c = complex_filled_chunk("x", what);
        let name = match name {
            TestLoc::None => NameLoc::None,
            TestLoc::Stack => NameLoc::OnStack,
            TestLoc::Index => NameLoc::Index(c.add_to_string_pool("myname".into()).unwrap()),
        };
        node.instantiate_arrow_function_expression(&mut c, strict, src, name, node.clone())
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }

    #[test_case("x => x", &[] => Ok((svec(&["STRING 0 ()", "FUNC_IAE 0"]), true, false)); "typical")]
    fn compile(src: &str, what: &[(Fillable, usize)]) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).arrow_function();
        let mut c = complex_filled_chunk("x", what);
        node.compile(&mut c, true, src, node.clone())
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }

    #[test_case("x => x", &[] => Ok((svec(&["FUNC_IAE 0"]), true, false)); "typical")]
    fn compile_named_evaluation(src: &str, what: &[(Fillable, usize)]) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).arrow_function();
        let mut c = complex_filled_chunk("x", what);
        node.compile_named_evaluation(&mut c, true, src, node.clone(), NameLoc::OnStack)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod concise_body {
    use super::*;
    use test_case::test_case;

    #[test_case("x => x * 2", true, &[] => Ok((svec(&[
        "CPMLBM 0 (x)",
        "EXTRACT_ARG",
        "ILB 0 (x)",
        "FINISH_ARGS",
        "STRING 0 (x)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "FLOAT 0 (2)",
        "MULTIPLY",
        "JUMP_IF_ABRUPT 1",
        "RETURN",
        "END_FUNCTION"
    ]), true, false)); "simple expr function")]
    #[test_case("x => { return x; }", true, &[] => Ok((svec(&[
        "CPMLBM 0 (x)",
        "EXTRACT_ARG",
        "ILB 0 (x)",
        "FINISH_ARGS",
        "STRING 0 (x)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 1",
        "RETURN",
        "END_FUNCTION"
    ]), true, false)); "function body")]
    #[test_case("x => x", true, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "instantiation compile fails")]
    #[test_case("(x=b) => x", true, &[] => Ok((svec(&[
        "CPMLBM 0 (x)",
        "EXTRACT_ARG",
        "STRING 0 (x)",
        "STRICT_RESOLVE",
        "SWAP",
        "JUMP_NOT_UNDEF 12",
        "POP",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 5",
        "UNWIND 1",
        "UNWIND_LIST",
        "JUMP 2",
        "IRB",
        "POP",
        "JUMP_IF_ABRUPT 3",
        "FINISH_ARGS",
        "PNVEFL",
        "SLETVE",
        "JUMP_IF_ABRUPT 7",
        "STRING 0 (x)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 1",
        "RETURN",
        "END_FUNCTION"
    ]), true, false)); "fallible initializers")]
    #[test_case("x=>x", false, &[] => Ok((svec(&[
        "CPMLBM 0 (x)",
        "EXTRACT_ARG",
        "ILB 0 (x)",
        "FINISH_ARGS",
        "PNLE",
        "STRING 0 (x)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 1",
        "RETURN",
        "END_FUNCTION"
    ]), true, false)); "non-strict/simple")]
    #[test_case("x=>b", true, &[(Fillable::String, 1)] => serr("Out of room for strings in this compilation unit"); "expr compile fails")]
    #[test_case("(x=a)=>@@@", true, &[] => serr("out of range integral type conversion attempted"); "expr too big")]
    fn compile_body(src: &str, strict: bool, what: &[(Fillable, usize)]) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).arrow_function();
        let mut c = complex_filled_chunk("x", what);
        let data = StashedFunctionData {
            source_text: src.to_string(),
            params: node.parameters.clone().into(),
            body: node.body.clone().into(),
            to_compile: node.clone().into(),
            strict,
            this_mode: ThisLexicality::LexicalThis,
        };

        node.body
            .compile_body(&mut c, src, &data)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod expression_body {
    use super::*;
    use test_case::test_case;

    #[test_case("true", true, &[] => Ok((svec(&["TRUE", "RETURN"]), true, false)); "literal only")]
    #[test_case("a", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 1",
        "RETURN"
    ]), true, false)); "fallible expression")]
    #[test_case("a", false, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 1",
        "RETURN"
    ]), true, false)); "fallible/non-strict")]
    #[test_case(
        "a",
        true,
        &[(Fillable::String, 0)]
        => serr("Out of room for strings in this compilation unit");
        "expr compile fails"
    )]
    fn compile(src: &str, strict: bool, what: &[(Fillable, usize)]) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).expression_body();
        let mut c = complex_filled_chunk("x", what);
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod param_source {
    use super::*;
    use test_case::test_case;
    #[derive(Copy, Clone)]
    enum Kind {
        Formal,
        Arrow,
        AsyncArrowBinding,
        ArrowFormals,
        UniqueFormals,
    }

    #[test_case("a", Kind::Formal, true, EnvUsage::UsePutValue, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "PUT_VALUE", "POP"]), false, false)); "strict/dups/formal")]
    #[test_case("a", Kind::Formal, false, EnvUsage::UsePutValue, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "PUT_VALUE", "POP"]), false, false)); "non-strict/dups/formal")]
    #[test_case("a", Kind::Formal, true, EnvUsage::UseCurrentLexical, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "IRB", "POP"]), false, false)); "strict/no-dups/formal")]
    #[test_case("a", Kind::Formal, false, EnvUsage::UseCurrentLexical, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "IRB", "POP"]), false, false)); "non-strict/no-dups/formal")]
    #[test_case("a", Kind::Arrow, true, EnvUsage::UsePutValue, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "PUT_VALUE", "POP_PANIC"]), false, false)); "strict/dups/arrow")]
    #[test_case("a", Kind::Arrow, false, EnvUsage::UsePutValue, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "PUT_VALUE", "POP_PANIC"]), false, false)); "non-strict/dups/arrow")]
    #[test_case("a", Kind::Arrow, true, EnvUsage::UseCurrentLexical, &[] => Ok((svec(&["EXTRACT_ARG", "ILB 0 (a)"]), false, false)); "strict/no-dups/arrow")]
    #[test_case("a", Kind::Arrow, false, EnvUsage::UseCurrentLexical, &[] => Ok((svec(&["EXTRACT_ARG", "ILB 0 (a)"]), false, false)); "non-strict/no-dups/arrow")]
    #[test_case("a", Kind::AsyncArrowBinding, false, EnvUsage::UseCurrentLexical, &[] => panics "not yet implemented"; "async arrow binding")]
    #[test_case("(a)", Kind::ArrowFormals, false, EnvUsage::UseCurrentLexical, &[] => panics "not yet implemented"; "arrow formals")]
    #[test_case("a", Kind::UniqueFormals, false, EnvUsage::UseCurrentLexical, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "IRB", "POP"]), false, false)); "unique formal params")]
    fn compile_binding_initialization(
        src: &str,
        which: Kind,
        strict: bool,
        env: EnvUsage,
        what: &[(Fillable, usize)],
    ) -> Result<(Vec<String>, bool, bool), String> {
        let node = match which {
            Kind::Formal => ParamSource::FormalParameters(Maker::new(src).formal_parameters()),
            Kind::Arrow => ParamSource::ArrowParameters(Maker::new(src).arrow_parameters()),
            Kind::AsyncArrowBinding => ParamSource::AsyncArrowBinding(Maker::new(src).async_arrow_binding_identifier()),
            Kind::ArrowFormals => ParamSource::ArrowFormals(Maker::new(src).arrow_formal_parameters()),
            Kind::UniqueFormals => ParamSource::UniqueFormalParameters(Maker::new(src).unique_formal_parameters()),
        };
        let mut c = complex_filled_chunk("x", what);
        node.compile_binding_initialization(&mut c, strict, src, env)
            .as_ref()
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(ToString::to_string)
    }
}

mod formal_parameters {
    use super::*;
    use test_case::test_case;

    #[test_case("a", true, EnvUsage::UseCurrentLexical, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "IRB", "POP"]), false, false)); "simple/strict/no-dup")]
    #[test_case("a", true, EnvUsage::UsePutValue, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "PUT_VALUE", "POP"]), false, false)); "simple/strict/dup")]
    #[test_case("a", false, EnvUsage::UseCurrentLexical, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "IRB", "POP"]), false, false)); "simple/non-strict/no-dup")]
    #[test_case("a", false, EnvUsage::UsePutValue, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "PUT_VALUE", "POP"]), false, false)); "simple/non-strict/dup")]
    #[test_case("", false, EnvUsage::UseCurrentLexical, &[] => Ok((svec(&[]), false, false)); "empty/non-strict/no-dup")]
    #[test_case("...a", true, EnvUsage::UseCurrentLexical, &[] => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "JUMP_IF_ABRUPT 10", "ROTATEDOWN_LIST 0", "LIST_TO_ARRAY", "IRB", "JUMP_IF_ABRUPT 5", "POP", "ZERO", "JUMP 1", "UNWIND_LIST"]), true, false)); "rest/strict/no-dup")]
    #[test_case("a,", true, EnvUsage::UseCurrentLexical, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "IRB", "POP"]), false, false)); "comma/strict/no-dup")]
    #[test_case("a,...b", true, EnvUsage::UseCurrentLexical, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "IRB", "POP", "STRING 1 (b)", "STRICT_RESOLVE", "JUMP_IF_ABRUPT 10", "ROTATEDOWN_LIST 0", "LIST_TO_ARRAY", "IRB", "JUMP_IF_ABRUPT 5", "POP", "ZERO", "JUMP 1", "UNWIND_LIST"]), true, false)); "list+rest/strict/no-dup")]
    #[test_case("a,...b", true, EnvUsage::UseCurrentLexical, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "list+rest/string table full")]
    fn compile_binding_initialization(
        src: &str,
        strict: bool,
        env: EnvUsage,
        what: &[(Fillable, usize)],
    ) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).formal_parameters();
        let mut c = complex_filled_chunk("x", what);
        node.compile_binding_initialization(&mut c, strict, src, env)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod arrow_parameters {
    use super::*;
    use test_case::test_case;

    #[test_case("a", true, EnvUsage::UsePutValue, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "PUT_VALUE", "POP_PANIC"]), false, false)); "id/strict/dups")]
    #[test_case("a", true, EnvUsage::UseCurrentLexical, &[] => Ok((svec(&["EXTRACT_ARG", "ILB 0 (a)"]), false, false)); "id/strict/no-dups")]
    #[test_case("a", false, EnvUsage::UsePutValue, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "PUT_VALUE", "POP_PANIC"]), false, false)); "id/non-strict/dups")]
    #[test_case("a", false, EnvUsage::UseCurrentLexical, &[] => Ok((svec(&["EXTRACT_ARG", "ILB 0 (a)"]), false, false)); "id/non-strict/no-dups")]
    #[test_case("(a)", true, EnvUsage::UsePutValue, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "PUT_VALUE", "POP"]), false, false)); "afp/strict/dups")]
    #[test_case("(a)", true, EnvUsage::UseCurrentLexical, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "IRB", "POP"]), false, false)); "afp/strict/no-dups")]
    #[test_case("(a)", false, EnvUsage::UsePutValue, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "PUT_VALUE", "POP"]), false, false)); "afp/non-strict/dups")]
    #[test_case("(a)", false, EnvUsage::UseCurrentLexical, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "IRB", "POP"]), false, false)); "afp/non-strict/no-dups")]
    fn compile_binding_initialization(
        src: &str,
        strict: bool,
        env: EnvUsage,
        what: &[(Fillable, usize)],
    ) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).arrow_parameters();
        let mut c = complex_filled_chunk("x", what);
        node.compile_binding_initialization(&mut c, strict, src, env)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod arrow_formal_parameters {
    use super::*;
    use test_case::test_case;

    #[test_case("(a)", true, EnvUsage::UsePutValue, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "PUT_VALUE", "POP"]), false, false)); "afp/strict/dups")]
    #[test_case("(a)", true, EnvUsage::UseCurrentLexical, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "IRB", "POP"]), false, false)); "afp/strict/no-dups")]
    #[test_case("(a)", false, EnvUsage::UsePutValue, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "PUT_VALUE", "POP"]), false, false)); "afp/non-strict/dups")]
    #[test_case("(a)", false, EnvUsage::UseCurrentLexical, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "IRB", "POP"]), false, false)); "afp/non-strict/no-dups")]
    fn compile_binding_initialization(
        src: &str,
        strict: bool,
        env: EnvUsage,
        what: &[(Fillable, usize)],
    ) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).arrow_formal_parameters();
        let mut c = complex_filled_chunk("x", what);
        node.compile_binding_initialization(&mut c, strict, src, env)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod unique_formal_parameters {
    use super::*;
    use test_case::test_case;

    #[test_case("a", true, EnvUsage::UsePutValue, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "PUT_VALUE", "POP"]), false, false)); "ufp/strict/dups")]
    #[test_case("a", true, EnvUsage::UseCurrentLexical, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "IRB", "POP"]), false, false)); "ufp/strict/no-dups")]
    #[test_case("a", false, EnvUsage::UsePutValue, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "PUT_VALUE", "POP"]), false, false)); "ufp/non-strict/dups")]
    #[test_case("a", false, EnvUsage::UseCurrentLexical, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "IRB", "POP"]), false, false)); "ufp/non-strict/no-dups")]
    fn compile_binding_initialization(
        src: &str,
        strict: bool,
        env: EnvUsage,
        what: &[(Fillable, usize)],
    ) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).unique_formal_parameters();
        let mut c = complex_filled_chunk("x", what);
        node.compile_binding_initialization(&mut c, strict, src, env)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

#[test_case(true, EnvUsage::UsePutValue => svec(&["STRING 0 (simply_fascinating)", "STRICT_RESOLVE", "SWAP", "PUT_VALUE", "POP_PANIC"]); "strict/dups")]
#[test_case(true, EnvUsage::UseCurrentLexical => svec(&["ILB 0 (simply_fascinating)"]); "strict/no-dups")]
#[test_case(false, EnvUsage::UsePutValue => svec(&["STRING 0 (simply_fascinating)", "RESOLVE", "SWAP", "PUT_VALUE", "POP_PANIC"]); "non-strict/dups")]
#[test_case(false, EnvUsage::UseCurrentLexical => svec(&["ILB 0 (simply_fascinating)"]); "non-strict/no-dups")]
fn compile_initialize_bound_name(strict: bool, env: EnvUsage) -> Vec<String> {
    let mut c = Chunk::new("cibn");
    let string_idx = c.add_to_string_pool("simply_fascinating".into()).unwrap();
    super::compile_initialize_bound_name(&mut c, strict, env, string_idx);
    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect()
}

mod formal_parameter_list {
    use super::*;
    use test_case::test_case;

    #[test_case("a", true, EnvUsage::UsePutValue, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "PUT_VALUE", "POP"]), false, false)); "strict/dups")]
    #[test_case("a", true, EnvUsage::UseCurrentLexical, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "IRB", "POP"]), false, false)); "strict/no-dups")]
    #[test_case("a", false, EnvUsage::UsePutValue, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "PUT_VALUE", "POP"]), false, false)); "non-strict/dups")]
    #[test_case("a", false, EnvUsage::UseCurrentLexical, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "IRB", "POP"]), false, false)); "non-strict/no-dups")]
    #[test_case("a,b", true, EnvUsage::UsePutValue, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "PUT_VALUE", "POP", "EXTRACT_ARG", "STRING 1 (b)", "STRICT_RESOLVE", "SWAP", "PUT_VALUE", "POP"]), false, false)); "list - strict/dups")]
    #[test_case("a,b", true, EnvUsage::UseCurrentLexical, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "IRB", "POP", "EXTRACT_ARG", "STRING 1 (b)", "STRICT_RESOLVE", "SWAP", "IRB", "POP"]), false, false)); "list - strict/no-dups")]
    #[test_case("a,b", false, EnvUsage::UsePutValue, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "PUT_VALUE", "POP", "EXTRACT_ARG", "STRING 1 (b)", "RESOLVE", "SWAP", "PUT_VALUE", "POP"]), false, false)); "list - non-strict/dups")]
    #[test_case("a,b", false, EnvUsage::UseCurrentLexical, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "IRB", "POP", "EXTRACT_ARG", "STRING 1 (b)", "RESOLVE", "SWAP", "IRB", "POP"]), false, false)); "list - non-strict/no-dups")]
    #[test_case("a=x,b", true, EnvUsage::UseCurrentLexical, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "JUMP_NOT_UNDEF 12", "POP", "STRING 1 (x)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 5", "UNWIND 1", "UNWIND_LIST", "JUMP 2", "IRB", "POP", "JUMP_IF_ABRUPT 7", "EXTRACT_ARG", "STRING 2 (b)", "STRICT_RESOLVE", "SWAP", "IRB", "POP"]), true, false)); "list - left:fallible")]
    #[test_case("a,b=x", true, EnvUsage::UseCurrentLexical, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "IRB", "POP", "EXTRACT_ARG", "STRING 1 (b)", "STRICT_RESOLVE", "SWAP", "JUMP_NOT_UNDEF 12", "POP", "STRING 2 (x)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 5", "UNWIND 1", "UNWIND_LIST", "JUMP 2", "IRB", "POP"]), true, false)); "list - right:fallible")]
    #[test_case("a=c,b=@@(11)", true, EnvUsage::UseCurrentLexical, &[] => serr("out of range integral type conversion attempted"); "second init too big")]
    #[test_case("a,b", false, EnvUsage::UseCurrentLexical, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "left: compilation fails")]
    #[test_case("a,b", false, EnvUsage::UseCurrentLexical, &[(Fillable::String, 1)] => serr("Out of room for strings in this compilation unit"); "right: compilation fails")]
    fn compile_binding_initialization(
        src: &str,
        strict: bool,
        env: EnvUsage,
        what: &[(Fillable, usize)],
    ) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).formal_parameter_list();
        let mut c = complex_filled_chunk("x", what);
        node.compile_binding_initialization(&mut c, strict, src, env)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod formal_parameter {
    use super::*;
    use test_case::test_case;

    #[test_case("a", true, EnvUsage::UsePutValue, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "PUT_VALUE", "POP"]), false, false)); "strict/dups")]
    #[test_case("a", true, EnvUsage::UseCurrentLexical, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "IRB", "POP"]), false, false)); "strict/no-dups")]
    #[test_case("a", false, EnvUsage::UsePutValue, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "PUT_VALUE", "POP"]), false, false)); "non-strict/dups")]
    #[test_case("a", false, EnvUsage::UseCurrentLexical, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "IRB", "POP"]), false, false)); "non-strict/no-dups")]
    fn compile_binding_initialization(
        src: &str,
        strict: bool,
        env: EnvUsage,
        what: &[(Fillable, usize)],
    ) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).formal_parameter();
        let mut c = complex_filled_chunk("x", what);
        node.compile_binding_initialization(&mut c, strict, src, env)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod single_name_binding {
    use super::*;
    use test_case::test_case;

    #[test_case("a", true, EnvUsage::UsePutValue, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "PUT_VALUE", "POP"]), false, false)); "strict/dups")]
    #[test_case("a", true, EnvUsage::UseCurrentLexical, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "IRB", "POP"]), false, false)); "strict/no-dups")]
    #[test_case("a", false, EnvUsage::UsePutValue, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "PUT_VALUE", "POP"]), false, false)); "non-strict/dups")]
    #[test_case("a", false, EnvUsage::UseCurrentLexical, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "IRB", "POP"]), false, false)); "non-strict/no-dups")]
    #[test_case("a", false, EnvUsage::UseCurrentLexical, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "string table full")]
    #[test_case("a=0", false, EnvUsage::UseCurrentLexical, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "JUMP_NOT_UNDEF 3", "POP", "FLOAT 0 (0)", "IRB", "POP"]), false, false)); "non-strict/no-dupes/simple initializer")]
    #[test_case("a=b", false, EnvUsage::UseCurrentLexical, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "JUMP_NOT_UNDEF 12", "POP", "STRING 1 (b)", "RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 5", "UNWIND 1", "UNWIND_LIST", "JUMP 2", "IRB", "POP"]), true, false)); "non-strict/no-dupes/fallible initializer")]
    #[test_case("a=function(){}", false, EnvUsage::UseCurrentLexical, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "JUMP_NOT_UNDEF 12", "POP", "STRING 0 (a)", "FUNC_IIFE 0", "JUMP_IF_NORMAL 5", "UNWIND 1", "UNWIND_LIST", "JUMP 2", "IRB", "POP"]), true, false)); "non-strict/no-dupes/anonymous fcn")]
    #[test_case("a=function(){}", false, EnvUsage::UseCurrentLexical, &[(Fillable::FunctionStash, 0)] => serr("Out of room for more functions!"); "fcn comp fails")]
    #[test_case("a=b", false, EnvUsage::UseCurrentLexical, &[(Fillable::String, 1)] => serr("Out of room for strings in this compilation unit"); "initializer comp fails")]
    #[test_case("a=@@@", false, EnvUsage::UseCurrentLexical, &[] => serr("out of range integral type conversion attempted"); "value-present branch too far")]
    fn compile_binding_initialization(
        src: &str,
        strict: bool,
        env: EnvUsage,
        what: &[(Fillable, usize)],
    ) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).single_name_binding();
        let mut c = complex_filled_chunk("x", what);
        node.compile_binding_initialization(&mut c, strict, src, env)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }

    #[test_case("a", false, EnvUsage::UsePutValue, &[] => Ok(svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 8",
        "ROTATEDOWN 3",
        "GETV",
        "JUMP_IF_ABRUPT 5",
        "PUT_VALUE",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 1"
    ])); "non-strict/putvalue/simple")]
    #[test_case("a", true, EnvUsage::UsePutValue, &[] => Ok(svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 8",
        "ROTATEDOWN 3",
        "GETV",
        "JUMP_IF_ABRUPT 5",
        "PUT_VALUE",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 1"
    ])); "strict/putvalue/simple")]
    #[test_case("a", false, EnvUsage::UseCurrentLexical, &[] => Ok(svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 8",
        "ROTATEDOWN 3",
        "GETV",
        "JUMP_IF_ABRUPT 5",
        "IRB",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 1"
    ])); "non-strict/uselex/simple")]
    #[test_case("a", true, EnvUsage::UseCurrentLexical, &[] => Ok(svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 8",
        "ROTATEDOWN 3",
        "GETV",
        "JUMP_IF_ABRUPT 5",
        "IRB",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 1"
    ])); "strict/uselex/simple")]
    #[test_case("a", false, EnvUsage::UseCurrentLexical, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "string table full")]
    #[test_case("a=0", false, EnvUsage::UseCurrentLexical, &[] => Ok(svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 13",
        "ROTATEDOWN 3",
        "GETV",
        "JUMP_IF_ABRUPT 10",
        "JUMP_NOT_UNDEF 3",
        "POP",
        "FLOAT 0 (0)",
        "IRB",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 1"
    ])); "infallible initializer")]
    #[test_case("a=8n", false, EnvUsage::UseCurrentLexical, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "izer compile fails")]
    #[test_case("a=() => 0", true, EnvUsage::UsePutValue, &[] => Ok(svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 17",
        "ROTATEDOWN 3",
        "GETV",
        "JUMP_IF_ABRUPT 14",
        "JUMP_NOT_UNDEF 7",
        "POP",
        "STRING 0 (a)",
        "FUNC_IAE 0",
        "JUMP_IF_ABRUPT 5",
        "PUT_VALUE",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 1"
    ])); "named evaluation")]
    #[test_case("a=() => 0", true, EnvUsage::UsePutValue, &[(Fillable::FunctionStash, 0)] => serr("Out of room for more functions!"); "named compile fails")]
    #[test_case("a=b", false, EnvUsage::UsePutValue, &[] => Ok(svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 17",
        "ROTATEDOWN 3",
        "GETV",
        "JUMP_IF_ABRUPT 14",
        "JUMP_NOT_UNDEF 7",
        "POP",
        "STRING 1 (b)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 5",
        "PUT_VALUE",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 1"
    ])); "izer is a ref")]
    #[test_case("a=@@@", false, EnvUsage::UsePutValue, &[] => serr("out of range integral type conversion attempted"); "izer too big")]
    #[test_case("a=@@(12)", false, EnvUsage::UsePutValue, &[] => serr("out of range integral type conversion attempted"); "unwind2 jumps too far")]
    fn keyed_binding_initialization(
        src: &str,
        strict: bool,
        env: EnvUsage,
        what: &[(Fillable, usize)],
    ) -> Result<Vec<String>, String> {
        let node = Maker::new(src).single_name_binding();
        let mut c = complex_filled_chunk("x", what);
        node.keyed_binding_initialization(&mut c, strict, src, env)
            .map(|_| c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>())
            .map_err(|e| e.to_string())
    }

    #[test_case("a", false, EnvUsage::UsePutValue, &[] => Ok(svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 13",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 9",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 3",
        "POP",
        "JUMP 2",
        "UNWIND 1"
    ])); "non-strict/putvalue/no-initializer")]
    #[test_case("a", true, EnvUsage::UsePutValue, &[] => Ok(svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 13",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 9",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 3",
        "POP",
        "JUMP 2",
        "UNWIND 1"
    ])); "strict/putvalue/no-initializer")]
    #[test_case("a", false, EnvUsage::UseCurrentLexical, &[] => Ok(svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 13",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 9",
        "SWAP",
        "ROTATEDOWN 3",
        "IRB",
        "JUMP_IF_ABRUPT 3",
        "POP",
        "JUMP 2",
        "UNWIND 1"
    ])); "non-strict/currentlex/no-initializer")]
    #[test_case("a", true, EnvUsage::UseCurrentLexical, &[] => Ok(svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 13",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 9",
        "SWAP",
        "ROTATEDOWN 3",
        "IRB",
        "JUMP_IF_ABRUPT 3",
        "POP",
        "JUMP 2",
        "UNWIND 1"
    ])); "strict/currentlex/no-initializer")]
    #[test_case("a", false, EnvUsage::UseCurrentLexical, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "string table full")]
    #[test_case("a=0", false, EnvUsage::UsePutValue, &[] => Ok(svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 18",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 14",
        "JUMP_NOT_UNDEF 3",
        "POP",
        "FLOAT 0 (0)",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 3",
        "POP",
        "JUMP 2",
        "UNWIND 1"
    ])); "infallible initializer")]
    #[test_case("a=() => 0", false, EnvUsage::UsePutValue, &[] => Ok(svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 24",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 20",
        "JUMP_NOT_UNDEF 7",
        "POP",
        "STRING 0 (a)",
        "FUNC_IAE 0",
        "JUMP_IF_ABRUPT 9",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 5",
        "POP",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 1"
    ])); "named evaluation")]
    #[test_case("a=b", false, EnvUsage::UsePutValue, &[] => Ok(svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 24",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 20",
        "JUMP_NOT_UNDEF 7",
        "POP",
        "STRING 1 (b)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 9",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 5",
        "POP",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 1"
    ])); "izer ref")]
    #[test_case("a=()=>0", false, EnvUsage::UsePutValue, &[(Fillable::FunctionStash, 0)] => serr("Out of room for more functions!"); "function stash full")]
    #[test_case("a=1n", false, EnvUsage::UsePutValue, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "izer compile fails")]
    #[test_case("a=@@@", false, EnvUsage::UsePutValue, &[] => serr("out of range integral type conversion attempted"); "izer jump too far")]
    #[test_case("a=''+@@(20)", false, EnvUsage::UsePutValue, &[] => serr("out of range integral type conversion attempted"); "unwind1 jump too far")]
    fn iterator_binding_initialization(
        src: &str,
        strict: bool,
        env: EnvUsage,
        what: &[(Fillable, usize)],
    ) -> Result<Vec<String>, String> {
        let node = Maker::new(src).single_name_binding();
        let mut c = complex_filled_chunk("x", what);
        node.iterator_binding_initialization(&mut c, strict, src, env)
            .map(|_| c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>())
            .map_err(|e| e.to_string())
    }
}

mod function_rest_parameter {
    use super::*;
    use test_case::test_case;

    #[test_case("...a", false, EnvUsage::UseCurrentLexical, &[] => Ok((svec(&["STRING 0 (a)", "RESOLVE", "JUMP_IF_ABRUPT 10", "ROTATEDOWN_LIST 0", "LIST_TO_ARRAY", "IRB", "JUMP_IF_ABRUPT 5", "POP", "ZERO", "JUMP 1", "UNWIND_LIST"]), true, false)); "frp")]
    fn compile_binding_initialization(
        src: &str,
        strict: bool,
        env: EnvUsage,
        what: &[(Fillable, usize)],
    ) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).function_rest_parameter();
        let mut c = complex_filled_chunk("x", what);
        node.compile_binding_initialization(&mut c, strict, src, env)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod function_body {
    use super::*;
    use test_case::test_case;

    #[test_case("function a(){}", true, &[] => Ok((svec(&[
        "CUA",
        "CNSILB 0 (arguments)",
        "ILB 0 (arguments)",
        "FINISH_ARGS",
        "UNDEFINED",
        "END_FUNCTION"
    ]), false, false)); "typical/empty params")]
    #[test_case("function a(q){}", true, &[] => Ok((svec(&[
        "CPMLBM 0 (q)",
        "CUA",
        "CNSILB 1 (arguments)",
        "ILB 1 (arguments)",
        "EXTRACT_ARG",
        "STRING 0 (q)",
        "STRICT_RESOLVE",
        "SWAP",
        "IRB",
        "POP",
        "FINISH_ARGS",
        "UNDEFINED",
        "END_FUNCTION"
    ]), false, false)); "typical/simple params/strict")]
    #[test_case("function a(q){}", false, &[] => Ok((svec(&[
        "CPMLBM 0 (q)",
        "CMA",
        "CPMLB 1 (arguments)",
        "ILB 1 (arguments)",
        "EXTRACT_ARG",
        "STRING 0 (q)",
        "RESOLVE",
        "SWAP",
        "IRB",
        "POP",
        "FINISH_ARGS",
        "PNLE",
        "UNDEFINED",
        "END_FUNCTION"
    ]), false, false)); "typical/simple params/non-strict")]
    #[test_case("function a(q){'use strict';}", false, &[] => Ok((svec(&[
        "CPMLBM 0 (q)",
        "CUA",
        "CNSILB 1 (arguments)",
        "ILB 1 (arguments)",
        "EXTRACT_ARG",
        "STRING 0 (q)",
        "STRICT_RESOLVE",
        "SWAP",
        "IRB",
        "POP",
        "FINISH_ARGS",
        "STRING 2 (use strict)",
        "END_FUNCTION"
    ]), false, false)); "typical/simple params/directive")]
    #[test_case("function a(q=b){}", true, &[] => Ok((svec(&[
        "CPMLBM 0 (q)",
        "CUA",
        "CNSILB 1 (arguments)",
        "ILB 1 (arguments)",
        "EXTRACT_ARG",
        "STRING 0 (q)",
        "STRICT_RESOLVE",
        "SWAP",
        "JUMP_NOT_UNDEF 12",
        "POP",
        "STRING 2 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 5",
        "UNWIND 1",
        "UNWIND_LIST",
        "JUMP 2",
        "IRB",
        "POP",
        "JUMP_IF_ABRUPT 3",
        "FINISH_ARGS",
        "PNVEFL",
        "SLETVE",
        "JUMP_IF_ABRUPT 1",
        "UNDEFINED",
        "END_FUNCTION"
    ]), true, false)); "fallible instantiation")]
    #[test_case("function a(q){}", true, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "instantiation fails")]
    #[test_case("function a(){b;}", true, &[(Fillable::String, 1)] => serr("Out of room for strings in this compilation unit"); "statements fail")]
    #[test_case("function a(q=b){@@@;}", true, &[] => serr("out of range integral type conversion attempted"); "function too large")]
    fn compile_body(src: &str, strict: bool, what: &[(Fillable, usize)]) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).function_declaration();
        let mut c = complex_filled_chunk("x", what);
        let data = StashedFunctionData {
            source_text: src.to_string(),
            params: node.params.clone().into(),
            body: node.body.clone().into(),
            to_compile: node.clone().into(),
            strict,
            this_mode: ThisLexicality::NonLexicalThis,
        };

        node.body
            .compile_body(&mut c, src, &data)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod function_statement_list {
    use super::*;
    use test_case::test_case;

    #[test_case("", true, &[] => Ok((svec(&["UNDEFINED"]), false, false)); "empty")]
    #[test_case("a;", true, &[] => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE"]), true, false)); "fallible statement list/strict")]
    #[test_case("a;", false, &[] => Ok((svec(&["STRING 0 (a)", "RESOLVE", "GET_VALUE"]), true, false)); "fallible statement list/non-strict")]
    fn compile(src: &str, strict: bool, what: &[(Fillable, usize)]) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).function_statement_list();
        let mut c = complex_filled_chunk("x", what);
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod async_arrow_function {
    use super::*;
    use test_case::test_case;

    #[test_case("async x => 2*x" => "2 * x"; "ident only")]
    #[test_case("async (a, b) => a + b" => "a + b"; "formals")]
    fn body(src: &str) -> String {
        Maker::new(src).async_arrow_function().body().to_string()
    }

    #[test_case("async x => 2 * x" => "x"; "ident only")]
    #[test_case("async (a, b) => a + b" => "( a , b )"; "formals")]
    fn params(src: &str) -> String {
        Maker::new(src).async_arrow_function().params().to_string()
    }
}

mod construct_expr {
    use super::*;
    use test_case::test_case;

    #[test]
    fn debug() {
        assert_ne!(format!("{:?}", ConstructExpr::New(Maker::new("new Boolean").new_expression())), "");
    }

    #[test_case("new Boolean", |s| ConstructExpr::New(Maker::new(s).new_expression()), true, &[] => Ok((svec(&[
        "STRING 0 (Boolean)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 15",
        "DUP",
        "DUP",
        "FLOAT 0 (0)",
        "SWAP_LIST",
        "REQ_CSTR",
        "JUMP_IF_NORMAL 5",
        "UNWIND_LIST",
        "UNWIND 2",
        "JUMP 2",
        "POP",
        "CONSTRUCT"
    ]), true, false)); "new expr")]
    #[test_case("new Boolean()", |s| ConstructExpr::Member(Maker::new(s).member_expression()), true, &[] => Ok((svec(&[
        "STRING 0 (Boolean)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 15",
        "DUP",
        "DUP",
        "FLOAT 0 (0)",
        "SWAP_LIST",
        "REQ_CSTR",
        "JUMP_IF_NORMAL 5",
        "UNWIND_LIST",
        "UNWIND 2",
        "JUMP 2",
        "POP",
        "CONSTRUCT"
    ]), true, false)); "member expression")]
    fn compile(
        src: &str,
        make_node: impl FnOnce(&str) -> ConstructExpr,
        strict: bool,
        what: &[(Fillable, usize)],
    ) -> Result<(Vec<String>, bool, bool), String> {
        let node = make_node(src);
        let mut c = complex_filled_chunk("x", what);
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

#[test_case("new Symbol", true, &[] => Ok((svec(&[
    "STRING 0 (Symbol)",
    "STRICT_RESOLVE",
    "GET_VALUE",
    "JUMP_IF_ABRUPT 15",
    "DUP",
    "DUP",
    "FLOAT 0 (0)",
    "SWAP_LIST",
    "REQ_CSTR",
    "JUMP_IF_NORMAL 5",
    "UNWIND_LIST",
    "UNWIND 2",
    "JUMP 2",
    "POP",
    "CONSTRUCT"
]), true, false)); "no args/strict")]
#[test_case("new Symbol", true, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "expr compile error")]
#[test_case("new 3", true, &[] => Ok((svec(&[
    "FLOAT 0 (3)",
    "DUP",
    "DUP",
    "FLOAT 1 (0)",
    "SWAP_LIST",
    "REQ_CSTR",
    "JUMP_IF_NORMAL 5",
    "UNWIND_LIST",
    "UNWIND 2",
    "JUMP 2",
    "POP",
    "CONSTRUCT"
]), true, false)); "expr not a ref")]
#[test_case("new a", true, &[(Fillable::Float, 0)] => serr("Out of room for floats in this compilation unit"); "float table full, no args")]
#[test_case("new a(0)", true, &[] => Ok((svec(&[
    "STRING 0 (a)",
    "STRICT_RESOLVE",
    "GET_VALUE",
    "JUMP_IF_ABRUPT 17",
    "DUP",
    "DUP",
    "FLOAT 0 (0)",
    "FLOAT 1 (1)",
    "SWAP_LIST",
    "REQ_CSTR",
    "JUMP_IF_NORMAL 5",
    "UNWIND_LIST",
    "UNWIND 2",
    "JUMP 2",
    "POP",
    "CONSTRUCT"
]), true, false)); "with infallible args/strict")]
#[test_case("new a(0)", true, &[(Fillable::Float, 0)] => serr("Out of room for floats in this compilation unit"); "argeval fails")]
#[test_case("new a(b)", true, &[] => Ok((svec(&[
    "STRING 0 (a)",
    "STRICT_RESOLVE",
    "GET_VALUE",
    "JUMP_IF_ABRUPT 27",
    "DUP",
    "DUP",
    "STRING 1 (b)",
    "STRICT_RESOLVE",
    "GET_VALUE",
    "JUMP_IF_ABRUPT 2",
    "FLOAT 0 (1)",
    "JUMP_IF_NORMAL 4",
    "UNWIND 3",
    "JUMP 11",
    "SWAP_LIST",
    "REQ_CSTR",
    "JUMP_IF_NORMAL 5",
    "UNWIND_LIST",
    "UNWIND 2",
    "JUMP 2",
    "POP",
    "CONSTRUCT"
]), true, false)); "with fallible args/strict")]
#[test_case("new a(b)", false, &[] => Ok((svec(&[
    "STRING 0 (a)",
    "RESOLVE",
    "GET_VALUE",
    "JUMP_IF_ABRUPT 27",
    "DUP",
    "DUP",
    "STRING 1 (b)",
    "RESOLVE",
    "GET_VALUE",
    "JUMP_IF_ABRUPT 2",
    "FLOAT 0 (1)",
    "JUMP_IF_NORMAL 4",
    "UNWIND 3",
    "JUMP 11",
    "SWAP_LIST",
    "REQ_CSTR",
    "JUMP_IF_NORMAL 5",
    "UNWIND_LIST",
    "UNWIND 2",
    "JUMP 2",
    "POP",
    "CONSTRUCT"
]), true, false)); "with fallible args/non-strict")]
#[test_case("new a(@@@)", true, &[] => serr("out of range integral type conversion attempted"); "arguments too large")]
fn compile_new_evaluator(
    src: &str,
    strict: bool,
    what: &[(Fillable, usize)],
) -> Result<(Vec<String>, bool, bool), String> {
    let node = Maker::new(src).new_expression();
    let (constructor_expression, potential_arguments) = match &*node {
        NewExpression::MemberExpression(me) => match &**me {
            MemberExpression::NewArguments(me, args, _) => (ConstructExpr::Member(me.clone()), Some(args.clone())),
            _ => panic!("Invalid test case. Please don't get fancy."),
        },
        NewExpression::NewExpression(ne, _) => (ConstructExpr::New(ne.clone()), None),
    };
    let mut c = complex_filled_chunk("x", what);

    super::compile_new_evaluator(&mut c, strict, src, &constructor_expression, potential_arguments)
        .map(|status| {
            (
                c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                status.maybe_abrupt(),
                status.maybe_ref(),
            )
        })
        .map_err(|e| e.to_string())
}

mod catch_parameter {
    use super::*;
    use test_case::test_case;

    #[test_case("a", true, &[] => Ok((svec(&["ILB 0 (a)"]), false, false)); "ident")]
    #[test_case("{a}", true, &[] => Ok((svec(&[
        "REQ_COER",
        "JUMP_IF_ABRUPT 30",
        "STRING 0 (a)",
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 8",
        "ROTATEDOWN 3",
        "GETV",
        "JUMP_IF_ABRUPT 5",
        "IRB",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 5",
        "POP",
        "STRING 0 (a)",
        "FLOAT 0 (1)",
        "JUMP_IF_ABRUPT 2",
        "POP_LIST",
        "EMPTY"
    ]), true, false)); "pattern")]
    fn compile_binding_initialization(
        src: &str,
        strict: bool,
        what: &[(Fillable, usize)],
    ) -> Result<(Vec<String>, bool, bool), String> {
        let mut c = complex_filled_chunk("x", what);
        let node = Maker::new(src).catch_parameter();

        node.compile_binding_initialization(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod try_statement {
    use super::*;
    use test_case::test_case;

    #[test_case("try {} catch {}", true, &[] => Ok((svec(&[
        "UNDEFINED", "EMPTY", "UPDATE_EMPTY"
    ]), false, false)); "minimal (catch not even compiled)")]
    #[test_case("try {a;} catch {}", true, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "block compile fails")]
    #[test_case("try {a;} catch {b;}", true, &[] => Ok((svec(&[
        "UNDEFINED",
        "PNLE",
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "PLE",
        "JUMP_NOT_THROW 7",
        "POP",
        "PNLE",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "PLE",
        "UPDATE_EMPTY"
    ]), true, false)); "fallible try block/strict")]
    #[test_case("try {a;} catch {b;}", false, &[] => Ok((svec(&[
        "UNDEFINED",
        "PNLE",
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "PLE",
        "JUMP_NOT_THROW 7",
        "POP",
        "PNLE",
        "STRING 1 (b)",
        "RESOLVE",
        "GET_VALUE",
        "PLE",
        "UPDATE_EMPTY"
    ]), true, false)); "fallible try block/non-strict")]
    #[test_case("try {a;} catch {0;}", true, &[(Fillable::Float, 0)] => serr("Out of room for floats in this compilation unit"); "catch compile fails")]
    #[test_case("try {a;} catch {@@@;}", true, &[] => serr("out of range integral type conversion attempted"); "catch clause too large")]
    #[test_case("try {} finally {}", true, &[] => Ok((svec(&["UNDEFINED", "EMPTY", "EMPTY", "POP", "UPDATE_EMPTY"]), false, false)); "minimal finally")]
    #[test_case("try {a;} finally {}", true, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "try-finally: block compile fails")]
    #[test_case("try {a;} finally {0;}", true, &[(Fillable::Float, 0)] => serr("Out of room for floats in this compilation unit"); "try-finally: finally compile fails")]
    #[test_case("try {a;} finally {b;}", true, &[] => Ok((svec(&[
        "UNDEFINED",
        "PNLE",
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "PLE",
        "PNLE",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "PLE",
        "JUMP_IF_ABRUPT 4",
        "POP",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 2"
    ]), true, false)); "try-finally; finally is fallible")]
    #[test_case("try {a;} finally {b;}", false, &[] => Ok((svec(&[
        "UNDEFINED",
        "PNLE",
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "PLE",
        "PNLE",
        "STRING 1 (b)",
        "RESOLVE",
        "GET_VALUE",
        "PLE",
        "JUMP_IF_ABRUPT 4",
        "POP",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 2"
    ]), true, false)); "try-finally; finally is fallible; non-strict")]
    #[test_case("try{}catch{}finally{}", true, &[] => Ok((svec(&["UNDEFINED", "EMPTY", "EMPTY", "POP", "UPDATE_EMPTY"]), false, false)); "try-full/minimal")]
    #[test_case("try{a;}catch{b;}finally{c;}", true, &[] => Ok((svec(&[
        "UNDEFINED",
        "PNLE",
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "PLE",
        "JUMP_NOT_THROW 7",
        "POP",
        "PNLE",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "PLE",
        "PNLE",
        "STRING 2 (c)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "PLE",
        "JUMP_IF_ABRUPT 4",
        "POP",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 2"
    ]), true, false)); "try-full all fallible/strict")]
    #[test_case("try{a;}catch{b;}finally{c;}", false, &[] => Ok((svec(&[
        "UNDEFINED",
        "PNLE",
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "PLE",
        "JUMP_NOT_THROW 7",
        "POP",
        "PNLE",
        "STRING 1 (b)",
        "RESOLVE",
        "GET_VALUE",
        "PLE",
        "PNLE",
        "STRING 2 (c)",
        "RESOLVE",
        "GET_VALUE",
        "PLE",
        "JUMP_IF_ABRUPT 4",
        "POP",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 2"
    ]), true, false)); "try-full all fallible/non-strict")]
    #[test_case("try{a;}catch{}finally{}", true, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "try-full block fails")]
    #[test_case("try{a;}catch{0;}finally{}", true, &[(Fillable::Float, 0)] => serr("Out of room for floats in this compilation unit"); "try-full catch fails")]
    #[test_case("try{a;}catch{@@@;}finally{}", true, &[] => serr("out of range integral type conversion attempted"); "try-full catch too big")]
    #[test_case("try{a;}catch{b;}finally{0;}", true, &[(Fillable::Float, 0)] => serr("Out of room for floats in this compilation unit"); "try-full finally fails")]
    fn compile(src: &str, strict: bool, what: &[(Fillable, usize)]) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).try_statement();
        let mut c = complex_filled_chunk("x", what);
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod finally {
    use super::*;
    use test_case::test_case;

    #[test_case("finally{a;}", true, &[] => Ok((svec(&["PNLE", "STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "PLE"]), true, false)); "fallible/strict")]
    #[test_case("finally{a;}", false, &[] => Ok((svec(&["PNLE", "STRING 0 (a)", "RESOLVE", "GET_VALUE", "PLE"]), true, false)); "fallible/non-strict")]
    #[test_case("finally{}", true, &[] => Ok((svec(&["EMPTY"]), false, false)); "minimal")]
    fn compile(src: &str, strict: bool, what: &[(Fillable, usize)]) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).finally();
        let mut c = complex_filled_chunk("x", what);
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod catch {
    use super::*;
    use test_case::test_case;

    #[test_case("catch{}", true, &[] => Ok((svec(&["POP", "EMPTY"]), false, false)); "minimal")]
    #[test_case("catch{a;}", true, &[] => Ok((svec(&["POP", "PNLE", "STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "PLE"]), true, false)); "no-param/fallible/strict")]
    #[test_case("catch{a;}", false, &[] => Ok((svec(&["POP", "PNLE", "STRING 0 (a)", "RESOLVE", "GET_VALUE", "PLE"]), true, false)); "no-param/fallible/non-strict")]
    #[test_case("catch(e){e;}", true, &[] => Ok((svec(&["PNLE", "CPMLB 0 (e)", "EXTRACT_THROW", "ILB 0 (e)", "PNLE", "STRING 0 (e)", "STRICT_RESOLVE", "GET_VALUE", "PLE", "PLE"]), true, false)); "param/fallible/strict")]
    #[test_case("catch(e){e;}", false, &[] => Ok((svec(&["PNLE", "CPMLB 0 (e)", "EXTRACT_THROW", "ILB 0 (e)", "PNLE", "STRING 0 (e)", "RESOLVE", "GET_VALUE", "PLE", "PLE"]), true, false)); "param/fallible/non-strict")]
    #[test_case("catch(e){0;}", true, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "string table full")]
    #[test_case("catch({a}){x;}", true, &[] => panics "not yet implemented"; "binding maybe abrupt")]
    #[test_case("catch(e){0;}", true, &[(Fillable::Float, 0)] => serr("Out of room for floats in this compilation unit"); "block compile fail")]
    #[test_case("catch({e=9n}){}", true, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "binding init compile fails")]
    fn compile_catch_clause_evaluation(
        src: &str,
        strict: bool,
        what: &[(Fillable, usize)],
    ) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).catch();
        let mut c = complex_filled_chunk("x", what);
        node.compile_catch_clause_evaluation(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod elisions {
    use super::*;
    use test_case::test_case;

    #[test_case(",,", &[] => Ok((svec(&["FLOAT 0 (2)", "ADD", "SWAP", "DUP", "STRING 0 (length)", "STRICT_REF", "ROTATEUP 3", "POP2_PUSH3", "PUT_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "POP"]), true)); "normal")]
    #[test_case(",,", &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "string table full")]
    #[test_case(",,", &[(Fillable::Float, 0)] => serr("Out of room for floats in this compilation unit"); "float table full")]
    fn array_accumulation(src: &str, what: &[(Fillable, usize)]) -> Result<(Vec<String>, bool), String> {
        let node = Maker::new(src).elision();
        let mut c = complex_filled_chunk("x", what);
        node.array_accumulation(&mut c)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                )
            })
            .map_err(|e| e.to_string())
    }

    #[test_case(",,", &[] => Ok((svec(&["FLOAT 0 (2)", "IDAE_ELISION"]), true)); "normal")]
    #[test_case(",,", &[(Fillable::Float, 0)] => serr("Out of room for floats in this compilation unit"); "float table full")]
    fn iterator_destructuring_assignment_evaluation(
        src: &str,
        what: &[(Fillable, usize)],
    ) -> Result<(Vec<String>, bool), String> {
        let node = Maker::new(src).elision();
        let mut c = complex_filled_chunk("x", what);
        node.iterator_destructuring_assignment_evaluation(&mut c)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod element_list {
    use super::*;
    use test_case::test_case;

    #[test_case("0", false, &[] => Ok((svec(&["POP2_PUSH3", "TO_KEY", "FLOAT 0 (0)", "CR_PROP", "SWAP", "INCREMENT"]), false)); "one simple element")]
    #[test_case(",0", false, &[] => Ok((svec(&["FLOAT 0 (1)", "ADD", "SWAP", "DUP", "STRING 0 (length)", "STRICT_REF", "ROTATEUP 3", "POP2_PUSH3", "PUT_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "POP", "JUMP_IF_ABRUPT 7", "POP2_PUSH3", "TO_KEY", "FLOAT 1 (0)", "CR_PROP", "SWAP", "INCREMENT"]), true)); "one element, leading elision")]
    #[test_case(",0", false, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "leading elision compile fails")]
    #[test_case("a", false, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "one element, AE compile fails")]
    #[test_case("a", false, &[] => Ok((svec(&["POP2_PUSH3", "TO_KEY", "STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 3", "JUMP 3", "CR_PROP", "SWAP", "INCREMENT"]), true)); "one element, nonstrict ref")]
    #[test_case("a", true, &[] => Ok((svec(&["POP2_PUSH3", "TO_KEY", "STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 3", "JUMP 3", "CR_PROP", "SWAP", "INCREMENT"]), true)); "one element, strict ref")]
    #[test_case(",@@@", false, &[] => serr("out of range integral type conversion attempted"); "leading elision jump too far")]
    #[test_case("...a", false, &[] => Ok((svec(&["STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "ITERATOR_ACCUM"]), true)); "one spread element")]
    #[test_case(",...a", false, &[] => Ok((svec(&["FLOAT 0 (1)", "ADD", "SWAP", "DUP", "STRING 0 (length)", "STRICT_REF", "ROTATEUP 3", "POP2_PUSH3", "PUT_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "POP", "JUMP_IF_ABRUPT 11", "STRING 1 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "ITERATOR_ACCUM"]), true)); "elision then spread")]
    #[test_case(",...a", false, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "elision+spread; elision fails compilation")]
    #[test_case("...a", false, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "spread; fails compilation")]
    #[test_case(",...@@@", false, &[] => serr("out of range integral type conversion attempted"); "elision+spread: jump too large")]
    #[test_case("0,1", false, &[] => Ok((svec(&["POP2_PUSH3", "TO_KEY", "FLOAT 0 (0)", "CR_PROP", "SWAP", "INCREMENT", "POP2_PUSH3", "TO_KEY", "FLOAT 1 (1)", "CR_PROP", "SWAP", "INCREMENT"]), false)); "element list, simple")]
    #[test_case("a,1", false, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "element list, first fails")]
    #[test_case("a,1", false, &[] => Ok((svec(&["POP2_PUSH3", "TO_KEY", "STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 3", "JUMP 3", "CR_PROP", "SWAP", "INCREMENT", "JUMP_IF_ABRUPT 7", "POP2_PUSH3", "TO_KEY", "FLOAT 0 (1)", "CR_PROP", "SWAP", "INCREMENT"]), true)); "element list, first is fallible")]
    #[test_case("0,,1", false, &[] => Ok((svec(&["POP2_PUSH3", "TO_KEY", "FLOAT 0 (0)", "CR_PROP", "SWAP", "INCREMENT", "FLOAT 1 (1)", "ADD", "SWAP", "DUP", "STRING 0 (length)", "STRICT_REF", "ROTATEUP 3", "POP2_PUSH3", "PUT_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "POP", "JUMP_IF_ABRUPT 7", "POP2_PUSH3", "TO_KEY", "FLOAT 1 (1)", "CR_PROP", "SWAP", "INCREMENT"]), true)); "element list, with elision")]
    #[test_case("0,,1", false, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "list form; elision fails")]
    #[test_case("0,a", false, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "list form; final index store fails")]
    #[test_case("0,a", false, &[] => Ok((svec(&["POP2_PUSH3", "TO_KEY", "FLOAT 0 (0)", "CR_PROP", "SWAP", "INCREMENT", "POP2_PUSH3", "TO_KEY", "STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 3", "JUMP 3", "CR_PROP", "SWAP", "INCREMENT"]), true)); "list form, final is reference")]
    #[test_case("a,@@@", false, &[] => serr("out of range integral type conversion attempted"); "can't jump over final expr")]
    #[test_case("0,...a", false, &[] => Ok((svec(&["POP2_PUSH3", "TO_KEY", "FLOAT 0 (0)", "CR_PROP", "SWAP", "INCREMENT", "STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "ITERATOR_ACCUM"]), true)); "list spread element")]
    #[test_case("0,...a", false, &[(Fillable::Float, 0)] => serr("Out of room for floats in this compilation unit"); "list+spread; list fails")]
    #[test_case("b,...a", false, &[] => Ok((svec(&["POP2_PUSH3", "TO_KEY", "STRING 0 (b)", "RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 3", "JUMP 3", "CR_PROP", "SWAP", "INCREMENT", "JUMP_IF_ABRUPT 11", "STRING 1 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "ITERATOR_ACCUM"]), true)); "list+spread; fallible list")]
    #[test_case("0,,...a", false, &[] => Ok((svec(&["POP2_PUSH3", "TO_KEY", "FLOAT 0 (0)", "CR_PROP", "SWAP", "INCREMENT", "FLOAT 1 (1)", "ADD", "SWAP", "DUP", "STRING 0 (length)", "STRICT_REF", "ROTATEUP 3", "POP2_PUSH3", "PUT_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "POP", "JUMP_IF_ABRUPT 11", "STRING 1 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "ITERATOR_ACCUM"]), true)); "list+elision+spread")]
    #[test_case("0,,...a", false, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "list+elision+spread; elision fails")]
    #[test_case("0,...a", false, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "list+spread; spread fails")]
    fn array_accumulation(src: &str, strict: bool, what: &[(Fillable, usize)]) -> Result<(Vec<String>, bool), String> {
        let node = Maker::new(src).element_list();
        let mut c = complex_filled_chunk("x", what);
        node.array_accumulation(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod spread_element {
    use super::*;
    use test_case::test_case;

    #[test_case("...a", false, &[] => Ok((svec(&["STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "ITERATOR_ACCUM"]), true)); "expression")]
    #[test_case("...a", false, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "expression; fails")]
    #[test_case("...0", false, &[] => Ok((svec(&["FLOAT 0 (0)", "ITERATOR_ACCUM"]), true)); "expression; infallible")]
    fn array_accumulation(src: &str, strict: bool, what: &[(Fillable, usize)]) -> Result<(Vec<String>, bool), String> {
        let node = Maker::new(src).spread_element();
        let mut c = complex_filled_chunk("x", what);
        node.array_accumulation(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod array_literal {
    use super::*;
    use test_case::test_case;

    #[test_case("[]", false, &[] => Ok((svec(&["ARRAY"]), false)); "empty array")]
    #[test_case("[,]", false, &[] => Ok((svec(&["ARRAY", "ZERO", "FLOAT 0 (1)", "ADD", "SWAP", "DUP", "STRING 0 (length)", "STRICT_REF", "ROTATEUP 3", "POP2_PUSH3", "PUT_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "POP", "JUMP_IF_ABRUPT 1", "POP"]), true)); "elision only")]
    #[test_case("[,]", false, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "elision only; elison compile fails")]
    #[test_case("[0]", false, &[] => Ok((svec(&["ARRAY", "ZERO", "POP2_PUSH3", "TO_KEY", "FLOAT 0 (0)", "CR_PROP", "SWAP", "INCREMENT", "POP"]), false)); "list, normal")]
    #[test_case("[a]", true, &[] => Ok((svec(&["ARRAY", "ZERO", "POP2_PUSH3", "TO_KEY", "STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 3", "JUMP 3", "CR_PROP", "SWAP", "INCREMENT", "JUMP_IF_ABRUPT 1", "POP"]), true)); "list, fallible")]
    #[test_case("[a]", false, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "list, compile err")]
    #[test_case("[0,,]", false, &[] => Ok((svec(&["ARRAY", "ZERO", "POP2_PUSH3", "TO_KEY", "FLOAT 0 (0)", "CR_PROP", "SWAP", "INCREMENT", "FLOAT 1 (1)", "ADD", "SWAP", "DUP", "STRING 0 (length)", "STRICT_REF", "ROTATEUP 3", "POP2_PUSH3", "PUT_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "POP", "JUMP_IF_ABRUPT 1", "POP"]), true)); "list-elision, normal")]
    #[test_case("[0,,]", false, &[(Fillable::Float, 0)] => serr("Out of room for floats in this compilation unit"); "list-elision, list-part fails")]
    #[test_case("[0,,]", false, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "list-elision, elision fails")]
    #[test_case("[0,]", false, &[] => Ok((svec(&["ARRAY", "ZERO", "POP2_PUSH3", "TO_KEY", "FLOAT 0 (0)", "CR_PROP", "SWAP", "INCREMENT", "POP"]), false)); "list-elision, without elision")]
    #[test_case("[a,]", false, &[] => Ok((svec(&["ARRAY", "ZERO", "POP2_PUSH3", "TO_KEY", "STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 3", "JUMP 3", "CR_PROP", "SWAP", "INCREMENT", "JUMP_IF_ABRUPT 1", "POP"]), true)); "list-elision, fallible")]
    fn compile(src: &str, strict: bool, what: &[(Fillable, usize)]) -> Result<(Vec<String>, bool), String> {
        let node = Maker::new(src).array_literal();
        let mut c = complex_filled_chunk("x", what);
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod template_middle_list {
    // } text ${ expression
    use super::*;
    use test_case::test_case;

    #[test_case("}text${1", false, &[] => Ok((svec(&[
        "STRING 0 (text)",
        "FLOAT 0 (1)",
        "TO_STRING",
        "JUMP_IF_ABRUPT 3",
        "ADD",
        "JUMP 2",
        "UNWIND 1"
    ]), true)); "tm-exp; normal")]
    #[test_case("}text${1", false, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "tm-exp; head doesn't fit")]
    #[test_case("}text${1", false, &[(Fillable::Float, 0)] => serr("Out of room for floats in this compilation unit"); "tm-exp; expression compile fail")]
    #[test_case("}${a", false, &[] => Ok((svec(&[
        "STRING 0 ()",
        "STRING 1 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 6",
        "TO_STRING",
        "JUMP_IF_ABRUPT 3",
        "ADD",
        "JUMP 2",
        "UNWIND 1"
    ]), true)); "tm-exp; expr is reference")]
    #[test_case("}${0}${1", false, &[] => Ok((svec(&[
        "STRING 0 ()",
        "FLOAT 0 (0)",
        "TO_STRING",
        "JUMP_IF_ABRUPT 3",
        "ADD",
        "JUMP 2",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 13",
        "STRING 0 ()",
        "ADD",
        "FLOAT 1 (1)",
        "TO_STRING",
        "JUMP_IF_ABRUPT 3",
        "ADD",
        "JUMP 2",
        "UNWIND 1"
    ]), true)); "list-tm-exp; infallible")]
    #[test_case("}${0}${1", false, &[(Fillable::Float, 0)] => serr("Out of room for floats in this compilation unit"); "list-tm-exp; list compile fails")]
    #[test_case("}${0}x${1", false, &[(Fillable::String, 1)] => serr("Out of room for strings in this compilation unit"); "list-tm-exp; no room for tm-string")]
    #[test_case("}${a}${0", false, &[(Fillable::Float, 0)] => serr("Out of room for floats in this compilation unit"); "list-tm-exp; exp compile fails")]
    #[test_case("}${0}${a", false, &[] => Ok((svec(&[
        "STRING 0 ()",
        "FLOAT 0 (0)",
        "TO_STRING",
        "JUMP_IF_ABRUPT 3",
        "ADD",
        "JUMP 2",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 17",
        "STRING 0 ()",
        "ADD",
        "STRING 1 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 6",
        "TO_STRING",
        "JUMP_IF_ABRUPT 3",
        "ADD",
        "JUMP 2",
        "UNWIND 1"
    ]), true)); "list-tm-exp; exp is ref")]
    #[test_case("}${a}${@@@", false, &[] => serr("out of range integral type conversion attempted"); "list-tm-exp; exp too large")]
    fn compile(src: &str, strict: bool, what: &[(Fillable, usize)]) -> Result<(Vec<String>, bool), String> {
        let node = Maker::new(src).template_middle_list();
        let mut c = complex_filled_chunk("x", what);
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod template_spans {
    use super::*;
    use test_case::test_case;

    #[test_case("}`", false, &[] => Ok((svec(&["STRING 0 ()"]), false)); "tail-only")]
    #[test_case("}xyx`", false, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "tail-only: no room for strings")]
    #[test_case("}${0}`", false, &[] => Ok((svec(&[
        "STRING 0 ()",
        "FLOAT 0 (0)",
        "TO_STRING",
        "JUMP_IF_ABRUPT 3",
        "ADD",
        "JUMP 2",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 3",
        "STRING 0 ()",
        "ADD"
    ]), true)); "list-tail")]
    #[test_case("}${0}`", false, &[(Fillable::Float, 0)] => serr("Out of room for floats in this compilation unit"); "list-tail; compile fail in list")]
    #[test_case("}aa${0}bb`", false, &[(Fillable::String, 1)] => serr("Out of room for strings in this compilation unit"); "list-tail; no room for tail string")]
    fn compile(src: &str, strict: bool, what: &[(Fillable, usize)]) -> Result<(Vec<String>, bool), String> {
        let node = Maker::new(src).template_spans();
        let mut c = complex_filled_chunk("x", what);
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod substitution_template {
    use super::*;
    use test_case::test_case;

    #[test_case("`${0}`", false, &[] => Ok((svec(&[
        "STRING 0 ()",
        "FLOAT 0 (0)",
        "TO_STRING",
        "JUMP_IF_ABRUPT 6",
        "ADD",
        "STRING 0 ()",
        "ADD",
        "JUMP 2",
        "UNWIND 1"
    ]), true)); "one item, infallible")]
    #[test_case("`head${0}tail`", false, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "no room for head")]
    #[test_case("`head${8n}tail`", false, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "expr compile fail")]
    #[test_case("`head${a}tail`", false, &[] => Ok((svec(&[
        "STRING 0 (head)",
        "STRING 1 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 9",
        "TO_STRING",
        "JUMP_IF_ABRUPT 6",
        "ADD",
        "STRING 2 (tail)",
        "ADD",
        "JUMP 2",
        "UNWIND 1"
    ]), true)); "fallible expression")]
    #[test_case("`head ${a} middle ${8n} tail`", false, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "spans compile fail")]
    #[test_case("`head ${a} middle ${b} tail`", false, &[] => Ok((svec(&[
        "STRING 0 (head )",
        "STRING 1 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 30",
        "TO_STRING",
        "JUMP_IF_ABRUPT 27",
        "ADD",
        "STRING 2 ( middle )",
        "STRING 3 (b)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 6",
        "TO_STRING",
        "JUMP_IF_ABRUPT 3",
        "ADD",
        "JUMP 2",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 3",
        "STRING 4 ( tail)",
        "ADD",
        "JUMP_IF_ABRUPT 3",
        "ADD",
        "JUMP 2",
        "UNWIND 1"
    ]), true)); "spans fallible")]
    #[test_case("`head ${a} middle ${@@@} tail`", false, &[] => serr("out of range integral type conversion attempted"); "jump too far")]
    fn compile(src: &str, strict: bool, what: &[(Fillable, usize)]) -> Result<(Vec<String>, bool), String> {
        let node = Maker::new(src).substitution_template();
        let mut c = complex_filled_chunk("x", what);
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod template_literal {
    use super::*;
    use test_case::test_case;

    #[test_case("`rust`", false, &[] => Ok((svec(&["STRING 0 (rust)"]), false)); "no-sub")]
    #[test_case("`rust`", false, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "no-sub, no room")]
    #[test_case("`${0}`", false, &[] => Ok((svec(&[
        "STRING 0 ()",
        "FLOAT 0 (0)",
        "TO_STRING",
        "JUMP_IF_ABRUPT 6",
        "ADD",
        "STRING 0 ()",
        "ADD",
        "JUMP 2",
        "UNWIND 1"
    ]), true)); "subsitution templ")]
    fn compile(src: &str, strict: bool, what: &[(Fillable, usize)]) -> Result<(Vec<String>, bool), String> {
        let node = Maker::new(src).template_literal();
        let mut c = complex_filled_chunk("x", what);
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod env_usage {
    use super::*;
    use test_case::test_case;

    #[test_case(EnvUsage::UsePutValue, EnvUsage::UsePutValue => true; "equal")]
    #[test_case(EnvUsage::UseCurrentLexical, EnvUsage::UsePutValue => false; "not equal")]
    fn eq(left: EnvUsage, right: EnvUsage) -> bool {
        left == right
    }

    #[test_case(EnvUsage::UsePutValue => with |a| assert_ne!(a, ""); "sample")]
    fn debug(item: EnvUsage) -> String {
        format!("{item:?}")
    }

    #[test_case(EnvUsage::UsePutValue => EnvUsage::UsePutValue)]
    #[allow(clippy::clone_on_copy)]
    fn clone(a: EnvUsage) -> EnvUsage {
        a.clone()
    }
}

mod arg_list_size_hint {
    use super::*;
    use test_case::test_case;

    #[test_case(ArgListSizeHint { fixed_len: 20, has_variable: true }, ArgListSizeHint { fixed_len: 20, has_variable: true } => true; "equal")]
    #[test_case(ArgListSizeHint { fixed_len: 23, has_variable: false }, ArgListSizeHint { fixed_len: 3, has_variable: true } => false; "not equal")]
    fn eq(left: ArgListSizeHint, right: ArgListSizeHint) -> bool {
        left == right
    }

    #[test_case(ArgListSizeHint { fixed_len: 12, has_variable: true } => with |a| assert_ne!(a, ""); "sample")]
    fn debug(item: ArgListSizeHint) -> String {
        format!("{item:?}")
    }

    #[test_case(ArgListSizeHint { fixed_len: 2, has_variable: false } => ArgListSizeHint { fixed_len: 2, has_variable: false })]
    #[allow(clippy::clone_on_copy)]
    fn clone(a: ArgListSizeHint) -> ArgListSizeHint {
        a.clone()
    }
}

mod binding_property_list {
    use super::*;
    use test_case::test_case;

    #[test_case("a", false, EnvUsage::UsePutValue, &[] => Ok(svec(&[
        "STRING 0 (a)",
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 8",
        "ROTATEDOWN 3",
        "GETV",
        "JUMP_IF_ABRUPT 5",
        "PUT_VALUE",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 5",
        "POP",
        "STRING 0 (a)",
        "FLOAT 0 (1)"
    ])); "item")]
    #[test_case("a,b", false, EnvUsage::UsePutValue, &[] => Ok(svec(&[
        "DUP",
        "STRING 0 (a)",
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 8",
        "ROTATEDOWN 3",
        "GETV",
        "JUMP_IF_ABRUPT 5",
        "PUT_VALUE",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 5",
        "POP",
        "STRING 0 (a)",
        "FLOAT 0 (1)",
        "JUMP_IF_ABRUPT 32",
        "SWAP_LIST",
        "STRING 1 (b)",
        "STRING 1 (b)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 8",
        "ROTATEDOWN 3",
        "GETV",
        "JUMP_IF_ABRUPT 5",
        "PUT_VALUE",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 5",
        "POP",
        "STRING 1 (b)",
        "FLOAT 0 (1)",
        "JUMP_IF_ABRUPT 7",
        "APPEND_LIST",
        "JUMP 5",
        "UNWIND 1",
        "JUMP 1",
        "UNWIND_LIST"
    ])); "list")]
    #[test_case("a=1n,b", false, EnvUsage::UseCurrentLexical, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "list compile fails")]
    #[test_case("a,b=1n", false, EnvUsage::UseCurrentLexical, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "item compile fails")]
    #[test_case("a,b=@@(36)", false, EnvUsage::UseCurrentLexical, &[] => serr("out of range integral type conversion attempted"); "unwind_value jump too long")]
    fn property_binding_initialization(
        src: &str,
        strict: bool,
        env: EnvUsage,
        what: &[(Fillable, usize)],
    ) -> Result<Vec<String>, String> {
        let node = Maker::new(src).binding_property_list();
        let mut c = complex_filled_chunk("x", what);
        node.property_binding_initialization(&mut c, strict, src, env)
            .map(|_| c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>())
            .map_err(|e| e.to_string())
    }
}

mod binding_rest_property {
    use super::*;
    use test_case::test_case;

    #[test_case("...a", false, EnvUsage::UseCurrentLexical, &[] => Ok(svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 11",
        "ROTATEDOWN_LIST 1",
        "OBJECT",
        "ROTATEDOWN_LIST 1",
        "COPY_DATAPROPS_WE",
        "JUMP_IF_ABRUPT 4",
        "IRB",
        "JUMP 3",
        "UNWIND_LIST",
        "UNWIND 1"
    ])); "non-strict/lexical")]
    #[test_case("...a", true, EnvUsage::UsePutValue, &[] => Ok(svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 11",
        "ROTATEDOWN_LIST 1",
        "OBJECT",
        "ROTATEDOWN_LIST 1",
        "COPY_DATAPROPS_WE",
        "JUMP_IF_ABRUPT 4",
        "PUT_VALUE",
        "JUMP 3",
        "UNWIND_LIST",
        "UNWIND 1"
    ])); "strict/putvalue")]
    #[test_case("...a", true, EnvUsage::UsePutValue, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "string table full")]
    fn rest_binding_initialization(
        src: &str,
        strict: bool,
        env: EnvUsage,
        what: &[(Fillable, usize)],
    ) -> Result<Vec<String>, String> {
        let node = Maker::new(src).binding_rest_property();
        let mut c = complex_filled_chunk("x", what);
        node.rest_binding_initialization(&mut c, strict, env)
            .map(|_| c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>())
            .map_err(|e| e.to_string())
    }
}

mod array_binding_pattern {
    use super::*;
    use test_case::test_case;

    #[test_case("[]", false, EnvUsage::UsePutValue, &[] => Ok((svec(&[]), false)); "empty array")]
    #[test_case("[,]", false, EnvUsage::UsePutValue, &[] => Ok((svec(&[
        "FLOAT 0 (1)",
        "IDAE_ELISION"
    ]), true)); "elision only")]
    #[test_case("[...a]", false, EnvUsage::UsePutValue, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 13",
        "SWAP",
        "ITER_REST",
        "JUMP_IF_ABRUPT 9",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 3",
        "POP",
        "JUMP 2",
        "UNWIND 1"
    ]), true)); "rest only; typical")]
    #[test_case("[,...a]", true, EnvUsage::UseCurrentLexical, &[] => Ok((svec(&[
        "FLOAT 0 (1)",
        "IDAE_ELISION",
        "JUMP_IF_ABRUPT 20",
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 13",
        "SWAP",
        "ITER_REST",
        "JUMP_IF_ABRUPT 9",
        "SWAP",
        "ROTATEDOWN 3",
        "IRB",
        "JUMP_IF_ABRUPT 3",
        "POP",
        "JUMP 2",
        "UNWIND 1"
    ]), true)); "elision + rest; typical")]
    #[test_case("[,...a]", true, EnvUsage::UsePutValue, &[(Fillable::Float, 0)] => serr("Out of room for floats in this compilation unit"); "elision compile fails")]
    #[test_case("[,...a]", true, EnvUsage::UsePutValue, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "rest compile fails")]
    #[test_case("[a]", false, EnvUsage::UsePutValue, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 13",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 9",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 3",
        "POP",
        "JUMP 2",
        "UNWIND 1"
    ]), true)); "list only")]
    #[test_case("[a,]", true, EnvUsage::UseCurrentLexical, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 13",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 9",
        "SWAP",
        "ROTATEDOWN 3",
        "IRB",
        "JUMP_IF_ABRUPT 3",
        "POP",
        "JUMP 2",
        "UNWIND 1"
    ]), true)); "list-comma only")]
    #[test_case("[a,,]", false, EnvUsage::UseCurrentLexical, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 13",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 9",
        "SWAP",
        "ROTATEDOWN 3",
        "IRB",
        "JUMP_IF_ABRUPT 3",
        "POP",
        "JUMP 2",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 3",
        "FLOAT 0 (1)",
        "IDAE_ELISION"
    ]), true)); "list-elision; valid")]
    #[test_case("[a,,]", false, EnvUsage::UsePutValue, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "list-elision; list compile fails")]
    #[test_case("[a,,]", false, EnvUsage::UsePutValue, &[(Fillable::Float, 0)] => serr("Out of room for floats in this compilation unit"); "list-elision; elision compile fails")]
    #[test_case("[a,...b]", false, EnvUsage::UsePutValue, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 13",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 9",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 3",
        "POP",
        "JUMP 2",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 20",
        "STRING 1 (b)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 13",
        "SWAP",
        "ITER_REST",
        "JUMP_IF_ABRUPT 9",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 3",
        "POP",
        "JUMP 2",
        "UNWIND 1"
    ]), true)); "list-rest; valid")]
    #[test_case("[a,,...b]", false, EnvUsage::UsePutValue, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 13",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 9",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 3",
        "POP",
        "JUMP 2",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 25",
        "FLOAT 0 (1)",
        "IDAE_ELISION",
        "JUMP_IF_ABRUPT 20",
        "STRING 1 (b)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 13",
        "SWAP",
        "ITER_REST",
        "JUMP_IF_ABRUPT 9",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 3",
        "POP",
        "JUMP 2",
        "UNWIND 1"
    ]), true)); "list-elision-rest; valid")]
    #[test_case("[a=9n,...b]", false, EnvUsage::UsePutValue, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "list-rest; list-compile fails")]
    #[test_case("[a,,...b]", false, EnvUsage::UsePutValue, &[(Fillable::Float, 0)] => serr("Out of room for floats in this compilation unit"); "list-elision-rest; elision compile fails")]
    #[test_case("[b,...a]", false, EnvUsage::UsePutValue, &[(Fillable::String, 1)] => serr("Out of room for strings in this compilation unit"); "list-rest; rest compile fails")]
    fn iterator_binding_initialization(
        src: &str,
        strict: bool,
        env: EnvUsage,
        what: &[(Fillable, usize)],
    ) -> Result<(Vec<String>, bool), String> {
        let node = Maker::new(src).array_binding_pattern();
        let mut c = complex_filled_chunk("x", what);
        node.iterator_binding_initialization(&mut c, strict, src, env)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod binding_rest_element {
    use super::*;
    use test_case::test_case;

    #[test_case("...a", false, EnvUsage::UsePutValue, &[] => Ok(svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 13",
        "SWAP",
        "ITER_REST",
        "JUMP_IF_ABRUPT 9",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 3",
        "POP",
        "JUMP 2",
        "UNWIND 1"
    ])); "id/non-strict/putvalue")]
    #[test_case("...a", true, EnvUsage::UseCurrentLexical, &[] => Ok(svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 13",
        "SWAP",
        "ITER_REST",
        "JUMP_IF_ABRUPT 9",
        "SWAP",
        "ROTATEDOWN 3",
        "IRB",
        "JUMP_IF_ABRUPT 3",
        "POP",
        "JUMP 2",
        "UNWIND 1"
    ])); "id/strict/currentlex")]
    #[test_case("...a", true, EnvUsage::UseCurrentLexical, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "id; string table full")]
    #[test_case("...{}", true, EnvUsage::UseCurrentLexical, &[] => Ok(svec(&[
        "ITER_REST",
        "JUMP_IF_ABRUPT 12",
        "REQ_COER",
        "JUMP_IF_ABRUPT 2",
        "POP",
        "EMPTY",
        "JUMP_IF_ABRUPT 3",
        "POP",
        "JUMP 2",
        "UNWIND 1"
    ])); "pattern; empty obj")]
    #[test_case("...[a=@@(37)]", false, EnvUsage::UsePutValue, &[] => serr("out of range integral type conversion attempted"); "exit jump too far")]
    #[test_case("...[a]", false, EnvUsage::UsePutValue, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "pattern compile fails")]
    fn iterator_binding_initialization(
        src: &str,
        strict: bool,
        env: EnvUsage,
        what: &[(Fillable, usize)],
    ) -> Result<Vec<String>, String> {
        let node = Maker::new(src).binding_rest_element();
        let mut c = complex_filled_chunk("x", what);
        node.iterator_binding_initialization(&mut c, strict, src, env)
            .map(|_| c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>())
            .map_err(|e| e.to_string())
    }
}

mod binding_elision_element {
    use super::*;
    use test_case::test_case;

    #[test_case("a", false, EnvUsage::UsePutValue, &[] => Ok(svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 13",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 9",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 3",
        "POP",
        "JUMP 2",
        "UNWIND 1"
    ])); "element-only; typical")]
    #[test_case(",a", false, EnvUsage::UsePutValue, &[] => Ok(svec(&[
        "FLOAT 0 (1)",
        "IDAE_ELISION",
        "JUMP_IF_ABRUPT 20",
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 13",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 9",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 3",
        "POP",
        "JUMP 2",
        "UNWIND 1"
    ])); "elision-element; typical")]
    #[test_case(",a", false, EnvUsage::UsePutValue, &[(Fillable::Float, 0)] => serr("Out of room for floats in this compilation unit"); "elision compile fails")]
    #[test_case(",a=9n", false, EnvUsage::UseCurrentLexical, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "binding element compile fails")]
    #[test_case(",a=@@(24)", false, EnvUsage::UsePutValue, &[] => serr("out of range integral type conversion attempted"); "exit jump too far")]
    fn iterator_binding_initialization(
        src: &str,
        strict: bool,
        env: EnvUsage,
        what: &[(Fillable, usize)],
    ) -> Result<Vec<String>, String> {
        let node = Maker::new(src).binding_elision_element();
        let mut c = complex_filled_chunk("x", what);
        node.iterator_binding_initialization(&mut c, strict, src, env)
            .map(|_| c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>())
            .map_err(|e| e.to_string())
    }
}

mod binding_element_list {
    use super::*;
    use test_case::test_case;

    #[test_case("a", false, EnvUsage::UsePutValue, &[] => Ok(svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 13",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 9",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 3",
        "POP",
        "JUMP 2",
        "UNWIND 1"
    ])); "item only; typical")]
    #[test_case("a,b", false, EnvUsage::UsePutValue, &[] => Ok(svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 13",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 9",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 3",
        "POP",
        "JUMP 2",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 20",
        "STRING 1 (b)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 13",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 9",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 3",
        "POP",
        "JUMP 2",
        "UNWIND 1"
    ])); "list, item; typical")]
    #[test_case("a=1n,b", false, EnvUsage::UsePutValue, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "list compile fails")]
    #[test_case("a,b=1n", false, EnvUsage::UsePutValue, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "item compile fails")]
    #[test_case("a,b=@@(24)", false, EnvUsage::UsePutValue, &[] => serr("out of range integral type conversion attempted"); "exit jump too far")]
    fn iterator_binding_initialization(
        src: &str,
        strict: bool,
        env: EnvUsage,
        what: &[(Fillable, usize)],
    ) -> Result<Vec<String>, String> {
        let node = Maker::new(src).binding_element_list();
        let mut c = complex_filled_chunk("x", what);
        node.iterator_binding_initialization(&mut c, strict, src, env)
            .map(|_| c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>())
            .map_err(|e| e.to_string())
    }
}

mod object_binding_pattern {
    use super::*;
    use test_case::test_case;

    #[test_case("{}", false, EnvUsage::UsePutValue, &[] => Ok(svec(&[
        "POP",
        "EMPTY"
    ])); "empty pattern")]
    #[test_case("{a}", false, EnvUsage::UsePutValue, &[] => Ok(svec(&[
        "STRING 0 (a)",
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 8",
        "ROTATEDOWN 3",
        "GETV",
        "JUMP_IF_ABRUPT 5",
        "PUT_VALUE",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 5",
        "POP",
        "STRING 0 (a)",
        "FLOAT 0 (1)",
        "JUMP_IF_ABRUPT 2",
        "POP_LIST",
        "EMPTY"
    ])); "list-only; typical")]
    #[test_case("{a,}", false, EnvUsage::UsePutValue, &[] => Ok(svec(&[
        "STRING 0 (a)",
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 8",
        "ROTATEDOWN 3",
        "GETV",
        "JUMP_IF_ABRUPT 5",
        "PUT_VALUE",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 5",
        "POP",
        "STRING 0 (a)",
        "FLOAT 0 (1)",
        "JUMP_IF_ABRUPT 2",
        "POP_LIST",
        "EMPTY"
    ])); "list-comma; typical")]
    #[test_case("{...a}", false, EnvUsage::UsePutValue, &[] => Ok(svec(&[
        "ZERO",
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 11",
        "ROTATEDOWN_LIST 1",
        "OBJECT",
        "ROTATEDOWN_LIST 1",
        "COPY_DATAPROPS_WE",
        "JUMP_IF_ABRUPT 4",
        "PUT_VALUE",
        "JUMP 3",
        "UNWIND_LIST",
        "UNWIND 1"
    ])); "rest-only; typical")]
    #[test_case("{a,...b}", false, EnvUsage::UsePutValue, &[] => Ok(svec(&[
        "DUP",
        "STRING 0 (a)",
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 8",
        "ROTATEDOWN 3",
        "GETV",
        "JUMP_IF_ABRUPT 5",
        "PUT_VALUE",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 5",
        "POP",
        "STRING 0 (a)",
        "FLOAT 0 (1)",
        "JUMP_IF_ABRUPT 21",
        "STRING 1 (b)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 11",
        "ROTATEDOWN_LIST 1",
        "OBJECT",
        "ROTATEDOWN_LIST 1",
        "COPY_DATAPROPS_WE",
        "JUMP_IF_ABRUPT 4",
        "PUT_VALUE",
        "JUMP 3",
        "UNWIND_LIST",
        "UNWIND 1",
        "JUMP 2",
        "UNWIND 1"
    ])); "list-rest; typical")]
    #[test_case("{a}", false, EnvUsage::UsePutValue, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "list-only; list compile fails")]
    #[test_case("{...a}", false, EnvUsage::UsePutValue, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "rest-only; rest compile fails")]
    #[test_case("{a=9n,...b}", false, EnvUsage::UsePutValue, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "list-rest; list compile fails")]
    #[test_case("{a,...b}", false, EnvUsage::UsePutValue, &[(Fillable::String, 1)] => serr("Out of room for strings in this compilation unit"); "list-rest; rest compile fails")]
    fn binding_initialization(
        src: &str,
        strict: bool,
        env: EnvUsage,
        what: &[(Fillable, usize)],
    ) -> Result<Vec<String>, String> {
        let node = Maker::new(src).object_binding_pattern();
        let mut c = complex_filled_chunk("x", what);
        node.compile_binding_initialization(&mut c, strict, src, env)
            .map(|_| c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>())
            .map_err(|e| e.to_string())
    }
}

mod for_declaration {
    use super::*;
    use test_case::test_case;

    #[test_case("let a", &[] => Ok(svec(&["CPMLB 0 (a)"])); "mutable")]
    #[test_case("const a", &[] => Ok(svec(&["CSILB 0 (a)"])); "immutable")]
    #[test_case("let [a,b,c]", &[] => Ok(svec(&["CPMLB 0 (a)", "CPMLB 1 (b)", "CPMLB 2 (c)"])); "multiple mutable")]
    #[test_case("let []", &[] => Ok(svec(&[])); "no identifiers at all")]
    #[test_case("let oops", &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "string table full")]
    fn for_declaration_binding_instantiation(src: &str, what: &[(Fillable, usize)]) -> Result<Vec<String>, String> {
        let node = Maker::new(src).for_declaration();
        let mut c = complex_filled_chunk("x", what);
        node.for_declaration_binding_instantiation(&mut c)
            .map(|_| c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>())
            .map_err(|e| e.to_string())
    }

    #[test_case("let [a,b,c]", true, &[] => Ok((svec(&["GET_SYNC_ITER", "JUMP_IF_ABRUPT 67", "DUP", "STRING 0 (a)", "STRICT_RESOLVE", "JUMP_IF_ABRUPT 13", "SWAP", "ITER_STEP", "JUMP_IF_ABRUPT 9", "SWAP", "ROTATEDOWN 3", "IRB", "JUMP_IF_ABRUPT 3", "POP", "JUMP 2", "UNWIND 1", "JUMP_IF_ABRUPT 20", "STRING 1 (b)", "STRICT_RESOLVE", "JUMP_IF_ABRUPT 13", "SWAP", "ITER_STEP", "JUMP_IF_ABRUPT 9", "SWAP", "ROTATEDOWN 3", "IRB", "JUMP_IF_ABRUPT 3", "POP", "JUMP 2", "UNWIND 1", "JUMP_IF_ABRUPT 20", "STRING 2 (c)", "STRICT_RESOLVE", "JUMP_IF_ABRUPT 13", "SWAP", "ITER_STEP", "JUMP_IF_ABRUPT 9", "SWAP", "ROTATEDOWN 3", "IRB", "JUMP_IF_ABRUPT 3", "POP", "JUMP 2", "UNWIND 1", "EMPTY_IF_NOT_ERR", "ITER_CLOSE_IF_NOT_DONE"]), true)); "pattern")]
    fn for_declaration_binding_initialization(
        src: &str,
        strict: bool,
        what: &[(Fillable, usize)],
    ) -> Result<(Vec<String>, bool), String> {
        let node = Maker::new(src).for_declaration();
        let mut c = complex_filled_chunk("x", what);
        node.for_declaration_binding_initialization(&mut c, strict, src, EnvUsage::UseCurrentLexical)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod for_binding {
    use super::*;
    use test_case::test_case;

    #[test_case("a", false, EnvUsage::UsePutValue, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "SWAP",
        "PUT_VALUE",
        "POP_PANIC"
    ]), false)); "identifier, nonstrict, putvalue")]
    #[test_case("a", true, EnvUsage::UseCurrentLexical, &[] => Ok((svec(&[
        "ILB 0 (a)"
    ]), false)); "identifier, strict, lexical")]
    #[test_case("[a,b]", true, EnvUsage::UseCurrentLexical, &[] => Ok((svec(&[
        "GET_SYNC_ITER",
        "JUMP_IF_ABRUPT 45",
        "DUP",
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 13",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 9",
        "SWAP",
        "ROTATEDOWN 3",
        "IRB",
        "JUMP_IF_ABRUPT 3",
        "POP",
        "JUMP 2",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 20",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 13",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 9",
        "SWAP",
        "ROTATEDOWN 3",
        "IRB",
        "JUMP_IF_ABRUPT 3",
        "POP",
        "JUMP 2",
        "UNWIND 1",
        "EMPTY_IF_NOT_ERR",
        "ITER_CLOSE_IF_NOT_DONE"
    ]), true)); "pattern, strict, lexical")]
    #[test_case("[a,b]", false, EnvUsage::UsePutValue, &[] => Ok((svec(&[
        "GET_SYNC_ITER",
        "JUMP_IF_ABRUPT 45",
        "DUP",
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 13",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 9",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 3",
        "POP",
        "JUMP 2",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 20",
        "STRING 1 (b)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 13",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 9",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 3",
        "POP",
        "JUMP 2",
        "UNWIND 1",
        "EMPTY_IF_NOT_ERR",
        "ITER_CLOSE_IF_NOT_DONE"
    ]), true)); "pattern, non-strict, putvalue")]
    #[test_case("a", true, EnvUsage::UsePutValue, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "err from id compile")]
    #[test_case("[a]", true, EnvUsage::UsePutValue, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "err from pattern compile")]
    fn binding_initialization(
        src: &str,
        strict: bool,
        env: EnvUsage,
        what: &[(Fillable, usize)],
    ) -> Result<(Vec<String>, bool), String> {
        let node = Maker::new(src).for_binding();
        let mut c = complex_filled_chunk("x", what);
        node.binding_initialization(&mut c, strict, src, env)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                )
            })
            .map_err(|e| e.to_string())
    }

    #[test_case("a", true, &[] => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE"]), true, true)); "strict ident")]
    #[test_case("a", false, &[] => Ok((svec(&["STRING 0 (a)", "RESOLVE"]), true, true)); "non-strict ident")]
    #[test_case("[a]", false, &[] => panics "Patterns not expected to compile."; "pattern")]
    #[test_case("a", true, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "id compile fails")]
    fn compile(src: &str, strict: bool, what: &[(Fillable, usize)]) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).for_binding();
        let mut c = complex_filled_chunk("x", what);
        node.compile(&mut c, strict)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod iteration_kind {
    use super::*;

    #[test]
    #[allow(clippy::clone_on_copy)]
    fn clone() {
        let item = IterationKind::Enumerate;
        let copy = item.clone();
        assert!(matches!(copy, IterationKind::Enumerate));
    }
}

mod for_in_of_expr {
    use super::*;
    use test_case::test_case;

    mod from {
        use super::*;

        #[test]
        fn assignment_expression() {
            let ae = Maker::new("a=10").assignment_expression();
            let fioe = ForInOfExpr::from(&ae);
            if let ForInOfExpr::AssignmentExpression(result) = fioe {
                assert!(Rc::ptr_eq(&ae, result));
            } else {
                panic!("Poorly formed ForInOfExpr");
            }
        }

        #[test]
        fn expression() {
            let exp = Maker::new("89").expression();
            let fioe = ForInOfExpr::from(&exp);
            if let ForInOfExpr::Expression(result) = fioe {
                assert!(Rc::ptr_eq(&exp, result));
            } else {
                panic!("Poorly formed ForInOfExpr");
            }
        }
    }

    #[test_case("90", &Maker::new("90").assignment_expression(), true, &[]
            => Ok((svec(&["FLOAT 0 (90)"]), false, false))
            ; "strict, non-fallible assignment expression")]
    #[test_case("a", &Maker::new("a").expression(), true, &[]
            => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE"]), true, true))
            ; "strict, fallible expression")]
    #[test_case("90", &Maker::new("90").assignment_expression(), true, &[(Fillable::Float, 0)]
            => serr("Out of room for floats in this compilation unit")
            ; "ae compile fails")]
    #[test_case("a", &Maker::new("a").expression(), true, &[(Fillable::String, 0)]
            => serr("Out of room for strings in this compilation unit")
            ; "expression compile fails")]
    fn compile<'a>(
        src: &str,
        item: impl Into<ForInOfExpr<'a>>,
        strict: bool,
        what: &[(Fillable, usize)],
    ) -> Result<(Vec<String>, bool, bool), String> {
        let node = item.into();
        let mut c = complex_filled_chunk("x", what);
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }

    #[test]
    fn clone() {
        let ae = Maker::new("a=3").assignment_expression();
        let expr = ForInOfExpr::AssignmentExpression(&ae);
        let cloned = expr.clone();
        assert!(matches!(cloned, ForInOfExpr::AssignmentExpression(x) if Rc::ptr_eq(x, &ae)));
    }
}

mod for_in_of_lhs_expr {
    use super::*;
    use test_case::test_case;

    mod from {
        use super::*;

        #[test]
        fn left_hand_side_expression() {
            let node = Maker::new("a").left_hand_side_expression();
            let fiole = ForInOfLHSExpr::from(&node);
            if let ForInOfLHSExpr::LeftHandSideExpression(lhs) = fiole {
                assert!(Rc::ptr_eq(lhs, &node));
            } else {
                panic!("Bad conversion to ForInOfLHSExpr");
            }
        }

        #[test]
        fn assignment_pattern() {
            let node = Maker::new("[a]").assignment_pattern();
            let fiole = ForInOfLHSExpr::from(&node);
            if let ForInOfLHSExpr::AssignmentPattern(ap) = fiole {
                assert!(Rc::ptr_eq(ap, &node));
            } else {
                panic!("Bad conversion to ForInOfLHSExpr");
            }
        }

        #[test]
        fn for_binding() {
            let node = Maker::new("[a]").for_binding();
            let fiole = ForInOfLHSExpr::from(&node);
            if let ForInOfLHSExpr::ForBinding(fb) = fiole {
                assert!(Rc::ptr_eq(fb, &node));
            } else {
                panic!("Bad conversion to ForInOfLHSExpr");
            }
        }

        #[test]
        fn for_declaration() {
            let node = Maker::new("const a").for_declaration();
            let fiole = ForInOfLHSExpr::from(&node);
            if let ForInOfLHSExpr::ForDeclaration(fd) = fiole {
                assert!(Rc::ptr_eq(fd, &node));
            } else {
                panic!("Bad conversion to ForInOfLHSExpr");
            }
        }
    }

    #[test_case(&Maker::new("a").left_hand_side_expression() => false; "lhs: not destructuring")]
    #[test_case(&Maker::new("{a}").left_hand_side_expression() => true; "lhs: destructuring")]
    #[test_case(&Maker::new("[a]").assignment_pattern() => true; "assignment pattern")]
    #[test_case(&Maker::new("a").for_binding() => false; "ForBinding; not destr")]
    #[test_case(&Maker::new("[a]").for_binding() => true; "ForBinding: destructuring")]
    #[test_case(&Maker::new("let a").for_declaration() => false; "ForDeclaration: not destructuring")]
    #[test_case(&Maker::new("let [a]").for_declaration() => true; "ForDeclaration: destructuring")]
    fn is_destructuring<'a>(item: impl Into<ForInOfLHSExpr<'a>>) -> bool {
        let node = item.into();
        node.is_destructuring()
    }

    #[test]
    fn clone() {
        let lhse = Maker::new("twelve").left_hand_side_expression();
        let expr = ForInOfLHSExpr::LeftHandSideExpression(&lhse);
        let cloned = expr.clone();
        assert!(matches!(cloned, ForInOfLHSExpr::LeftHandSideExpression(x) if Rc::ptr_eq(x, &lhse)));
    }
}

mod assignment_pattern {
    use super::*;
    use test_case::test_case;

    #[test_case("{a}", true, &[] => Ok(svec(&[
        "REQ_COER",
        "JUMP_IF_ABRUPT 31",
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 19",
        "SWAP",
        "POP2_PUSH3",
        "STRING 0 (a)",
        "GETV",
        "JUMP_IF_ABRUPT 10",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 9",
        "POP",
        "STRING 0 (a)",
        "FLOAT 0 (1)",
        "JUMP 2",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 3",
        "POP_LIST",
        "JUMP 2",
        "UNWIND 1"
    ])); "object pattern")]
    #[test_case("[a]", true, &[] => Ok(svec(&[
        "DUP",
        "GET_SYNC_ITER",
        "JUMP_IF_ABRUPT 24",
        "DUP",
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 11",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 7",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1",
        "EMPTY_IF_NOT_ERR",
        "ITER_CLOSE_IF_NOT_DONE",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1"
    ])); "array pattern")]
    fn destructuring_assignment_evaluation(
        src: &str,
        strict: bool,
        what: &[(Fillable, usize)],
    ) -> Result<Vec<String>, String> {
        let node = Maker::new(src).assignment_pattern();
        let mut c = complex_filled_chunk("x", what);
        node.destructuring_assignment_evaluation(&mut c, strict, src)
            .map(|_| c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>())
            .map_err(|e| e.to_string())
    }
}

mod assignment_property {
    use super::*;
    use test_case::test_case;

    #[test_case("a", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 19",
        "SWAP",
        "POP2_PUSH3",
        "STRING 0 (a)",
        "GETV",
        "JUMP_IF_ABRUPT 10",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 9",
        "POP",
        "STRING 0 (a)",
        "FLOAT 0 (1)",
        "JUMP 2",
        "UNWIND 1"
    ]), true)); "id: simple")]
    #[test_case("a", true, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "id: no string space")]
    #[test_case("a", false, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 19",
        "SWAP",
        "POP2_PUSH3",
        "STRING 0 (a)",
        "GETV",
        "JUMP_IF_ABRUPT 10",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 9",
        "POP",
        "STRING 0 (a)",
        "FLOAT 0 (1)",
        "JUMP 2",
        "UNWIND 1"
    ]), true)); "id: simple/non-strict")]
    #[test_case("a=null", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 23",
        "SWAP",
        "POP2_PUSH3",
        "STRING 0 (a)",
        "GETV",
        "JUMP_IF_ABRUPT 14",
        "JUMP_NOT_UNDEF 2",
        "POP",
        "NULL",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 9",
        "POP",
        "STRING 0 (a)",
        "FLOAT 0 (1)",
        "JUMP 2",
        "UNWIND 1"
    ]), true)); "id: izer")]
    #[test_case("a=()=>null", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 28",
        "SWAP",
        "POP2_PUSH3",
        "STRING 0 (a)",
        "GETV",
        "JUMP_IF_ABRUPT 19",
        "JUMP_NOT_UNDEF 7",
        "POP",
        "STRING 0 (a)",
        "FUNC_IAE 0",
        "JUMP_IF_ABRUPT 10",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 9",
        "POP",
        "STRING 0 (a)",
        "FLOAT 0 (1)",
        "JUMP 2",
        "UNWIND 1"
    ]), true)); "id: nameable")]
    #[test_case("a=()=>null", true, &[(Fillable::FunctionStash, 0)] => serr("Out of room for more functions!"); "id: nameable fail")]
    #[test_case("a=8n", true, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "id: izer fail")]
    #[test_case("a=b", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 28",
        "SWAP",
        "POP2_PUSH3",
        "STRING 0 (a)",
        "GETV",
        "JUMP_IF_ABRUPT 19",
        "JUMP_NOT_UNDEF 7",
        "POP",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 10",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 9",
        "POP",
        "STRING 0 (a)",
        "FLOAT 0 (1)",
        "JUMP 2",
        "UNWIND 1"
    ]), true)); "id: ref")]
    #[test_case("a=@@@", true, &[] => serr("out of range integral type conversion attempted"); "id: izer jump too far")]
    #[test_case("a", true, &[(Fillable::Float, 0)] => serr("Out of room for floats in this compilation unit"); "id: no float space")]
    #[test_case("a=@@(14)", true, &[] => serr("out of range integral type conversion attempted"); "id: get jump too far")]
    #[test_case("a=@@(23)", true, &[] => serr("out of range integral type conversion attempted"); "id: resolve jump too far")]
    #[test_case("a:b", true, &[] => Ok((svec(&[
        "DUP",
        "STRING 0 (a)",
        "POP2_PUSH3",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 8",
        "ROTATEDOWN 3",
        "GETV",
        "JUMP_IF_ABRUPT 5",
        "PUT_VALUE",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 5",
        "POP",
        "FLOAT 0 (1)",
        "JUMP 2",
        "UNWIND 1"
    ]), true)); "prop: ref only")]
    #[test_case("[8n]:a", true, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "prop: name fail")]
    #[test_case("[a]:b", true, &[] => Ok((svec(&[
        "DUP",
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 1",
        "TO_KEY",
        "JUMP_IF_ABRUPT 25",
        "POP2_PUSH3",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 8",
        "ROTATEDOWN 3",
        "GETV",
        "JUMP_IF_ABRUPT 5",
        "PUT_VALUE",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 5",
        "POP",
        "FLOAT 0 (1)",
        "JUMP 2",
        "UNWIND 1"
    ]), true)); "prop: fallible name")]
    #[test_case("a:b=8n", true, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "prop: expr fail")]
    #[test_case("a:null", true, &[] => Ok((svec(&[
        "DUP",
        "STRING 0 (a)",
        "POP2_PUSH3",
        "NULL",
        "ROTATEDOWN 3",
        "GETV",
        "JUMP_IF_ABRUPT 3",
        "PUT_VALUE",
        "JUMP 2",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 5",
        "POP",
        "FLOAT 0 (1)",
        "JUMP 2",
        "UNWIND 1"
    ]), true)); "prop: infallible expr")]
    #[test_case("a:b", true, &[(Fillable::Float, 0)] => serr("Out of room for floats in this compilation unit"); "prop: resource exhaustion")]
    #[test_case("[a]:b=@@(29)", true, &[] => serr("out of range integral type conversion attempted"); "prop: too big")]
    fn property_destructuring_assignment_evaluation(
        src: &str,
        strict: bool,
        what: &[(Fillable, usize)],
    ) -> Result<(Vec<String>, bool), String> {
        let node = Maker::new(src).assignment_property();
        let mut c = complex_filled_chunk("x", what);
        node.property_destructuring_assignment_evaluation(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod assignment_property_list {
    use super::*;
    use test_case::test_case;

    #[test_case("a", true, &[] => Ok(svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 19",
        "SWAP",
        "POP2_PUSH3",
        "STRING 0 (a)",
        "GETV",
        "JUMP_IF_ABRUPT 10",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 9",
        "POP",
        "STRING 0 (a)",
        "FLOAT 0 (1)",
        "JUMP 2",
        "UNWIND 1"
    ])); "item; normal")]
    #[test_case("a,b", true, &[] => Ok(svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 19",
        "SWAP",
        "POP2_PUSH3",
        "STRING 0 (a)",
        "GETV",
        "JUMP_IF_ABRUPT 10",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 9",
        "POP",
        "STRING 0 (a)",
        "FLOAT 0 (1)",
        "JUMP 2",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 33",
        "SWAP_LIST",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 19",
        "SWAP",
        "POP2_PUSH3",
        "STRING 1 (b)",
        "GETV",
        "JUMP_IF_ABRUPT 10",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 9",
        "POP",
        "STRING 1 (b)",
        "FLOAT 0 (1)",
        "JUMP 2",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 4",
        "SWAP_DEEP_LIST",
        "APPEND_LIST",
        "JUMP 2",
        "POP_OUT_LIST 3"
    ])); "list; normal")]
    #[test_case("a=1n,b", true, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "list; head fail")]
    #[test_case("a,b=1n", true, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "list; tail fail")]
    #[test_case("a,b=@@(37)", true, &[] => serr("out of range integral type conversion attempted"); "list; tail too large")]
    fn property_destructuring_assignment_evaluation(
        src: &str,
        strict: bool,
        what: &[(Fillable, usize)],
    ) -> Result<Vec<String>, String> {
        let node = Maker::new(src).assignment_property_list();
        let mut c = complex_filled_chunk("x", what);
        node.property_destructuring_assignment_evaluation(&mut c, strict, src)
            .map(|_| c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>())
            .map_err(|e| e.to_string())
    }
}

mod assignment_rest_property {
    use super::*;
    use test_case::test_case;

    #[test_case("...a", true, &[] => Ok(svec(&[
        "DUP_AFTER_LIST",
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 14",
        "ROTATEDOWN_LIST 1",
        "OBJECT",
        "ROTATEDOWN_LIST 1",
        "COPY_DATAPROPS_WE",
        "JUMP_IF_ABRUPT 7",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 6",
        "POP",
        "JUMP 5",
        "UNWIND_LIST",
        "UNWIND 1",
        "UNWIND 1"
    ])); "normal")]
    #[test_case("...a[1n]", true, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "rest fail")]
    #[test_case("...0", true, &[] => Ok(svec(&[
        "DUP_AFTER_LIST",
        "FLOAT 0 (0)",
        "ROTATEDOWN_LIST 1",
        "OBJECT",
        "ROTATEDOWN_LIST 1",
        "COPY_DATAPROPS_WE",
        "JUMP_IF_ABRUPT 6",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 5",
        "POP",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 1"
    ])); "infallible")]
    fn rest_destructuring_assignment_evaluation(
        src: &str,
        strict: bool,
        what: &[(Fillable, usize)],
    ) -> Result<Vec<String>, String> {
        let node = Maker::new(src).assignment_rest_property();
        let mut c = complex_filled_chunk("x", what);
        node.rest_destructuring_assignment_evaluation(&mut c, strict, src)
            .map(|_| c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>())
            .map_err(|e| e.to_string())
    }
}

mod assignment_element {
    use super::*;
    use test_case::test_case;

    #[test_case("a", true, &[] => Ok(svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 8",
        "ROTATEDOWN 3",
        "GETV",
        "JUMP_IF_ABRUPT 5",
        "PUT_VALUE",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 1"
    ])); "no-init; normal")]
    #[test_case("a[1n]", true, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "no-init; fail")]
    #[test_case("0", true, &[] => Ok(svec(&[
        "FLOAT 0 (0)",
        "ROTATEDOWN 3",
        "GETV",
        "JUMP_IF_ABRUPT 3",
        "PUT_VALUE",
        "JUMP 2",
        "UNWIND 1"
    ])); "no-init; infallible")]
    #[test_case("[a]", true, &[] => Ok(svec(&[
        "GETV",
        "JUMP_IF_ABRUPT 31",
        "DUP",
        "GET_SYNC_ITER",
        "JUMP_IF_ABRUPT 24",
        "DUP",
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 11",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 7",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1",
        "EMPTY_IF_NOT_ERR",
        "ITER_CLOSE_IF_NOT_DONE",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1",
        "EMPTY_IF_NOT_ERR"
    ])); "no-init; pattern; normal")]
    #[test_case("a=0", true, &[] => Ok(svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 13",
        "ROTATEDOWN 3",
        "GETV",
        "JUMP_IF_ABRUPT 10",
        "JUMP_NOT_UNDEF 3",
        "POP",
        "FLOAT 0 (0)",
        "PUT_VALUE",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 1"
    ])); "init; normal")]
    #[test_case("a=()=>null", true, &[] => Ok(svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 19",
        "ROTATEDOWN 3",
        "GETV",
        "JUMP_IF_ABRUPT 16",
        "JUMP_NOT_UNDEF 9",
        "POP",
        "STRING 0 (a)",
        "STRING 0 (a)",
        "FUNC_IAE 0",
        "JUMP_IF_ABRUPT 5",
        "PUT_VALUE",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 1"
    ])); "init with func; normal")]
    #[test_case("a=b", true, &[] => Ok(svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 17",
        "ROTATEDOWN 3",
        "GETV",
        "JUMP_IF_ABRUPT 14",
        "JUMP_NOT_UNDEF 7",
        "POP",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 5",
        "PUT_VALUE",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 1"
    ])); "init with ref; normal")]
    #[test_case("a=()=>null", true, &[(Fillable::FunctionStash, 0)] => serr("Out of room for more functions!"); "init with func; fail")]
    #[test_case("a=8n", true, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "init with expr; fail")]
    #[test_case("[a]=b", true, &[] => Ok(svec(&[
        "GETV",
        "JUMP_IF_ABRUPT 40",
        "JUMP_NOT_UNDEF 7",
        "POP",
        "STRING 0 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 31",
        "DUP",
        "GET_SYNC_ITER",
        "JUMP_IF_ABRUPT 24",
        "DUP",
        "STRING 1 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 11",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 7",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1",
        "EMPTY_IF_NOT_ERR",
        "ITER_CLOSE_IF_NOT_DONE",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1",
        "EMPTY_IF_NOT_ERR"
    ])); "pattern target; normal")]
    #[test_case("a=@@(2)", true, &[] => serr("out of range integral type conversion attempted"); "izer too big")]
    #[test_case("[a[8n]]", true, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "destructuring fail")]
    #[test_case("a=@@(12)", true, &[] => serr("out of range integral type conversion attempted"); "lhse init too big")]
    #[test_case("0=@@(7)", true, &[] => serr("out of range integral type conversion attempted"); "infallible lhse init too big")]
    #[test_case("[a]=@@(35)", true, &[] => serr("out of range integral type conversion attempted"); "dstr init too big")] // exit_a
    fn keyed_destructuring_assignment_evaluation(
        src: &str,
        strict: bool,
        what: &[(Fillable, usize)],
    ) -> Result<Vec<String>, String> {
        let node = Maker::new(src).assignment_element();
        let mut c = complex_filled_chunk("x", what);
        node.keyed_destructuring_assignment_evaluation(&mut c, strict, src)
            .map(|_| c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>())
            .map_err(|e| e.to_string())
    }

    #[test_case("0", true, &[] => Ok(svec(&[
        "FLOAT 0 (0)",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 7",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1"
    ])); "infallible lref")]
    #[test_case("a", true, &[] => Ok(svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 11",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 7",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1"
    ])); "lref; normal")]
    #[test_case("[a]", true, &[] => Ok(svec(&[
        "ITER_STEP",
        "JUMP_IF_ABRUPT 32",
        "DUP",
        "GET_SYNC_ITER",
        "JUMP_IF_ABRUPT 24",
        "DUP",
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 11",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 7",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1",
        "EMPTY_IF_NOT_ERR",
        "ITER_CLOSE_IF_NOT_DONE",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1",
        "EMPTY_IF_NOT_ERR",
        "UPDATE_EMPTY"
    ])); "dstr; normal")]
    #[test_case("a[8n]", true, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "lhse fail")]
    #[test_case("a=0", true, &[] => Ok(svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 16",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 12",
        "SWAP",
        "ROTATEDOWN 3",
        "JUMP_NOT_UNDEF 3",
        "POP",
        "FLOAT 0 (0)",
        "PUT_VALUE",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1"
    ])); "lhse with initializer")]
    #[test_case("a=b", true, &[] => Ok(svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 22",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 18",
        "SWAP",
        "ROTATEDOWN 3",
        "JUMP_NOT_UNDEF 7",
        "POP",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 4",
        "PUT_VALUE",
        "UPDATE_EMPTY",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 1"
    ])); "lhse with initializer ref")]
    #[test_case("a=()=>10", true, &[] => Ok(svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 22",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 18",
        "SWAP",
        "ROTATEDOWN 3",
        "JUMP_NOT_UNDEF 7",
        "POP",
        "STRING 0 (a)",
        "FUNC_IAE 0",
        "JUMP_IF_ABRUPT 4",
        "PUT_VALUE",
        "UPDATE_EMPTY",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 1"
    ])); "lhse with named function")]
    #[test_case("a=()=>10", true, &[(Fillable::FunctionStash, 0)] => serr("Out of room for more functions!"); "named compilation fails")]
    #[test_case("a=8n", true, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "izer fail")]
    #[test_case("[a]=b", true, &[] => Ok(svec(&[
        "ITER_STEP",
        "JUMP_IF_ABRUPT 45",
        "JUMP_NOT_UNDEF 7",
        "POP",
        "STRING 0 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 34",
        "DUP",
        "GET_SYNC_ITER",
        "JUMP_IF_ABRUPT 24",
        "DUP",
        "STRING 1 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 11",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 7",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1",
        "EMPTY_IF_NOT_ERR",
        "ITER_CLOSE_IF_NOT_DONE",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1",
        "EMPTY_IF_NOT_ERR",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1"
    ])); "dstr with initializer")]
    #[test_case("a=@@(2)", true, &[] => serr("out of range integral type conversion attempted"); "izer too big")]
    #[test_case("[a[1n]]", true, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "dstr fail")]
    #[test_case("a=@@(15)", true, &[] => serr("out of range integral type conversion attempted"); "izer too big v2")]
    #[test_case("0=@@(11)", true, &[] => serr("out of range integral type conversion attempted"); "izer too big v3")]
    #[test_case("[a[@@(44)]]=q", true, &[] => serr("out of range integral type conversion attempted"); "dstr too big")]
    #[test_case("[a[@@(42)]]", true, &[] => serr("out of range integral type conversion attempted"); "dstr too big v2")]
    fn iterator_destructuring_assignment_evaluation(
        src: &str,
        strict: bool,
        what: &[(Fillable, usize)],
    ) -> Result<Vec<String>, String> {
        let node = Maker::new(src).assignment_element();
        let mut c = complex_filled_chunk("x", what);
        node.iterator_destructuring_assignment_evaluation(&mut c, strict, src)
            .map(|_| c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>())
            .map_err(|e| e.to_string())
    }
}

mod assignment_elision_element {
    use super::*;
    use test_case::test_case;

    #[test_case("a", true, &[] => Ok(svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 11",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 7",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1"
    ])); "no elision; normal")]
    #[test_case(",a", true, &[] => Ok(svec(&[
        "FLOAT 0 (1)",
        "IDAE_ELISION",
        "JUMP_IF_ABRUPT 18",
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 11",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 7",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1"
    ])); "has elision; normal")]
    #[test_case(",,,,,,,a", true, &[(Fillable::Float, 0)] => serr("Out of room for floats in this compilation unit"); "elision fail")]
    #[test_case(",a[8n]", true, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "element fail")]
    #[test_case(",a[@@(28)]", true, &[] => serr("out of range integral type conversion attempted"); "elem too big")]
    fn iterator_destructuring_assignment_evaluation(
        src: &str,
        strict: bool,
        what: &[(Fillable, usize)],
    ) -> Result<Vec<String>, String> {
        let node = Maker::new(src).assignment_elision_element();
        let mut c = complex_filled_chunk("x", what);
        node.iterator_destructuring_assignment_evaluation(&mut c, strict, src)
            .map(|_| c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>())
            .map_err(|e| e.to_string())
    }
}

mod assignment_element_list {
    use super::*;
    use test_case::test_case;

    #[test_case("a", true, &[] => Ok(svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 11",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 7",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1"
    ])); "item; normal")]
    #[test_case("a,b", true, &[] => Ok(svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 11",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 7",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 18",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 11",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 7",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1"
    ])); "list; normal")]
    #[test_case("a[1n],b", true, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "list fail")]
    #[test_case("a,b[1n]", true, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "item fail")]
    #[test_case("a,b[@@(28)]", true, &[] => serr("out of range integral type conversion attempted"); "item too big")]
    fn iterator_destructuring_assignment_evaluation(
        src: &str,
        strict: bool,
        what: &[(Fillable, usize)],
    ) -> Result<Vec<String>, String> {
        let node = Maker::new(src).assignment_element_list();
        let mut c = complex_filled_chunk("x", what);
        node.iterator_destructuring_assignment_evaluation(&mut c, strict, src)
            .map(|_| c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>())
            .map_err(|e| e.to_string())
    }
}

mod assignment_rest_element {
    use super::*;
    use test_case::test_case;

    #[test_case("...a", true, &[] => Ok(svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 11",
        "SWAP",
        "ITER_REST",
        "JUMP_IF_ABRUPT 7",
        "ROTATEUP 3",
        "SWAP",
        "PUT_VALUE",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1"
    ])); "ref; normal")]
    #[test_case("...[a]", true, &[] => Ok(svec(&[
        "ITER_REST",
        "JUMP_IF_ABRUPT 34",
        "DUP",
        "GET_SYNC_ITER",
        "JUMP_IF_ABRUPT 24",
        "DUP",
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 11",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 7",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1",
        "EMPTY_IF_NOT_ERR",
        "ITER_CLOSE_IF_NOT_DONE",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1",
        "EMPTY_IF_NOT_ERR",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1"
    ])); "dstr; normal")]
    #[test_case("...0", true, &[] => Ok(svec(&[
        "FLOAT 0 (0)",
        "SWAP",
        "ITER_REST",
        "JUMP_IF_ABRUPT 7",
        "ROTATEUP 3",
        "SWAP",
        "PUT_VALUE",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1"
    ])); "infallible lhse")]
    #[test_case("...a[8n]", true, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "lref fail")]
    #[test_case("...[a[8n]]", true, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "dstr fail")]
    fn iterator_destructuring_assignment_evaluation(
        src: &str,
        strict: bool,
        what: &[(Fillable, usize)],
    ) -> Result<Vec<String>, String> {
        let node = Maker::new(src).assignment_rest_element();
        let mut c = complex_filled_chunk("x", what);
        node.iterator_destructuring_assignment_evaluation(&mut c, strict, src)
            .map(|_| c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>())
            .map_err(|e| e.to_string())
    }
}

mod array_assignment_pattern {
    use super::*;
    use test_case::test_case;

    #[test_case("[]", true, &[] => Ok(svec(&[
        "DUP",
        "GET_SYNC_ITER",
        "JUMP_IF_ABRUPT 7",
        "EMPTY",
        "ITER_CLOSE",
        "JUMP_IF_ABRUPT 3",
        "POP",
        "JUMP 2",
        "UNWIND 1"
    ])); "empty; normal")]
    #[test_case("[,]", true, &[] => Ok(svec(&[
        "DUP",
        "GET_SYNC_ITER",
        "JUMP_IF_ABRUPT 9",
        "DUP",
        "FLOAT 0 (1)",
        "IDAE_ELISION",
        "EMPTY_IF_NOT_ERR",
        "ITER_CLOSE_IF_NOT_DONE",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1"
    ])); "elision only; normal")]
    #[test_case("[,,,]", true, &[(Fillable::Float, 0)] => serr("Out of room for floats in this compilation unit"); "elision fail")] // 3790
    #[test_case("[,...a]", true, &[] => Ok(svec(&[
        "DUP",
        "GET_SYNC_ITER",
        "JUMP_IF_ABRUPT 31",
        "DUP",
        "FLOAT 0 (1)",
        "IDAE_ELISION",
        "JUMP_IF_ABRUPT 23",
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 11",
        "SWAP",
        "ITER_REST",
        "JUMP_IF_ABRUPT 7",
        "ROTATEUP 3",
        "SWAP",
        "PUT_VALUE",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1",
        "EMPTY_IF_NOT_ERR",
        "ITER_CLOSE_IF_NOT_DONE",
        "UPDATE_EMPTY",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 1"
    ])); "elision+rest; normal")]
    #[test_case("[,,,...a]", true, &[(Fillable::Float, 0)] => serr("Out of room for floats in this compilation unit"); "elision+rest; elision fail")]
    #[test_case("[,...a[@@(33)]]", true, &[] => serr("out of range integral type conversion attempted"); "elision+rest; rest too big")]
    #[test_case("[...a]", true, &[] => Ok(svec(&[
        "DUP",
        "GET_SYNC_ITER",
        "JUMP_IF_ABRUPT 24",
        "DUP",
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 11",
        "SWAP",
        "ITER_REST",
        "JUMP_IF_ABRUPT 7",
        "ROTATEUP 3",
        "SWAP",
        "PUT_VALUE",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1",
        "EMPTY_IF_NOT_ERR",
        "ITER_CLOSE_IF_NOT_DONE",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1"
    ])); "rest; normal")]
    #[test_case("[...a[1n]]", true, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "rest fail")]
    #[test_case("[...a[@@(34)]]", true, &[] => serr("out of range integral type conversion attempted"); "rest-only; too big")]
    #[test_case("[a]", true, &[] => Ok(svec(&[
        "DUP",
        "GET_SYNC_ITER",
        "JUMP_IF_ABRUPT 24",
        "DUP",
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 11",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 7",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1",
        "EMPTY_IF_NOT_ERR",
        "ITER_CLOSE_IF_NOT_DONE",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1"
    ])); "list; normal")]
    #[test_case("[a[1n]]", true, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "list fail")]
    #[test_case("[a[@@(34)]]", true, &[] => serr("out of range integral type conversion attempted"); "list-only; too big")]
    #[test_case("[a,]", true, &[] => Ok(svec(&[
        "DUP",
        "GET_SYNC_ITER",
        "JUMP_IF_ABRUPT 29",
        "DUP",
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 11",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 7",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1",
        "JUMP_IF_NORMAL 3",
        "ITER_CLOSE_IF_NOT_DONE",
        "JUMP 5",
        "EMPTY_IF_NOT_ERR",
        "ITER_CLOSE_IF_NOT_DONE",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1"
    ])); "list+comma; normal")]
    #[test_case("[a,...b]", true, &[] => Ok(svec(&[
        "DUP",
        "GET_SYNC_ITER",
        "JUMP_IF_ABRUPT 47",
        "DUP",
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 11",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 7",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1",
        "JUMP_IF_NORMAL 3",
        "ITER_CLOSE_IF_NOT_DONE",
        "JUMP 23",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 11",
        "SWAP",
        "ITER_REST",
        "JUMP_IF_ABRUPT 7",
        "ROTATEUP 3",
        "SWAP",
        "PUT_VALUE",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1",
        "EMPTY_IF_NOT_ERR",
        "ITER_CLOSE_IF_NOT_DONE",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1"
    ])); "list+rest; normal")]
    #[test_case("[a,,]", true, &[] => Ok(svec(&[
        "DUP",
        "GET_SYNC_ITER",
        "JUMP_IF_ABRUPT 36",
        "DUP",
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 11",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 7",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1",
        "JUMP_IF_NORMAL 3",
        "ITER_CLOSE_IF_NOT_DONE",
        "JUMP 12",
        "FLOAT 0 (1)",
        "IDAE_ELISION",
        "JUMP_IF_ABRUPT 5",
        "EMPTY_IF_NOT_ERR",
        "ITER_CLOSE_IF_NOT_DONE",
        "UPDATE_EMPTY",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 1"
    ])); "list+elision; normal")]
    #[test_case("[a,,...b]", true, &[] => Ok(svec(&[
        "DUP",
        "GET_SYNC_ITER",
        "JUMP_IF_ABRUPT 54",
        "DUP",
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 11",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 7",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1",
        "JUMP_IF_NORMAL 3",
        "ITER_CLOSE_IF_NOT_DONE",
        "JUMP 30",
        "FLOAT 0 (1)",
        "IDAE_ELISION",
        "JUMP_IF_ABRUPT 23",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 11",
        "SWAP",
        "ITER_REST",
        "JUMP_IF_ABRUPT 7",
        "ROTATEUP 3",
        "SWAP",
        "PUT_VALUE",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1",
        "EMPTY_IF_NOT_ERR",
        "ITER_CLOSE_IF_NOT_DONE",
        "UPDATE_EMPTY",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 1"
    ])); "list+elision+rest; normal")]
    #[test_case("[a[1n],]", true, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "list+e+r; list fail")]
    #[test_case("[a,,,]", true, &[(Fillable::Float, 0)] => serr("Out of room for floats in this compilation unit"); "l+e+r; elision fail")]
    #[test_case("[a,...b[1n]]", true, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "l+e+r; rest fail")]
    #[test_case("[a,,...b[@@(37)]]", true, &[] => serr("out of range integral type conversion attempted"); "l+e+r; rest too big")]
    #[test_case("[a,,...b[@@(33)]]", true, &[] => serr("out of range integral type conversion attempted"); "l+e+r; rest too big v2")]
    fn destructuring_assignment_evaluation(
        src: &str,
        strict: bool,
        what: &[(Fillable, usize)],
    ) -> Result<Vec<String>, String> {
        let node = Maker::new(src).array_assignment_pattern();
        let mut c = complex_filled_chunk("x", what);
        node.destructuring_assignment_evaluation(&mut c, strict, src)
            .map(|_| c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>())
            .map_err(|e| e.to_string())
    }
}

mod destructuring_assignment_target {
    use super::*;
    use test_case::test_case;

    #[test_case("a", true, &[] => serr("lhs cannot be converted to pattern"); "lhse")]
    #[test_case("[a]", true, &[] => Ok(svec(&[
        "DUP",
        "GET_SYNC_ITER",
        "JUMP_IF_ABRUPT 24",
        "DUP",
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 11",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 7",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1",
        "EMPTY_IF_NOT_ERR",
        "ITER_CLOSE_IF_NOT_DONE",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1"
    ])); "pattern")]
    fn destructuring_assignment_evaluation(
        src: &str,
        strict: bool,
        what: &[(Fillable, usize)],
    ) -> Result<Vec<String>, String> {
        let node = Maker::new(src).destructuring_assignment_target();
        let mut c = complex_filled_chunk("x", what);
        node.destructuring_assignment_evaluation(&mut c, strict, src)
            .map(|_| c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>())
            .map_err(|e| e.to_string())
    }

    #[test_case("a", true, &[] => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE"]), true, true)); "lhse")]
    #[test_case("[a]", true, &[] => panics "internal error: entered unreachable code"; "pattern")]
    fn compile(src: &str, strict: bool, what: &[(Fillable, usize)]) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).destructuring_assignment_target();
        let mut c = complex_filled_chunk("x", what);
        node.compile(&mut c, strict, src)
            .map(|flags| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    flags.maybe_abrupt(),
                    flags.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

#[allow(clippy::too_many_arguments)]
mod for_in_of_statement {
    use super::*;
    use test_case::test_case;
    #[derive(Copy, Clone)]
    enum ForInOfExprKind {
        Expression,
        AssignmentExpression,
    }
    enum ForInOfExprBinding {
        Expression(Rc<Expression>),
        AssignmentExpression(Rc<AssignmentExpression>),
    }

    #[test_case("i=0", true, &[], ForInOfExprKind::Expression, IterationKind::Iterate, &[] => Ok((svec(&[
        "STRING 0 (i)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 5",
        "FLOAT 0 (0)",
        "POP2_PUSH3",
        "PUT_VALUE",
        "UPDATE_EMPTY",
        "JUMP_IF_ABRUPT 1",
        "GET_SYNC_ITER"
    ]), true)); "expression/strict/iterate")]
    #[test_case("i", false, &[], ForInOfExprKind::AssignmentExpression, IterationKind::Enumerate, &[] => Ok((svec(&[
        "STRING 0 (i)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 8",
        "JUMP_NULLISH 4",
        "TO_OBJECT",
        "ENUM_PROPS",
        "JUMP 2",
        "POP",
        "BREAK"
    ]), true)); "ass-exp/non-strict/enumerate")]
    #[test_case("obj", false, &["item"], ForInOfExprKind::Expression, IterationKind::Enumerate, &[] => Ok((svec(&[
        "PNLE",
        "CPMLB 0 (item)",
        "STRING 1 (obj)",
        "RESOLVE",
        "PLE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 8",
        "JUMP_NULLISH 4",
        "TO_OBJECT",
        "ENUM_PROPS",
        "JUMP 2",
        "POP",
        "BREAK"
    ]), true)); "expression/non-strict/enumerate/has_ids")]
    #[test_case("obj", false, &["item"], ForInOfExprKind::Expression, IterationKind::Enumerate, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "has_ids + string table full")]
    #[test_case("9n", false, &["item"], ForInOfExprKind::Expression, IterationKind::Enumerate, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "has_ids + compile fails")]
    #[test_case("9n", false, &[], ForInOfExprKind::Expression, IterationKind::Enumerate, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "no ids + compile fails")]
    #[test_case("{}", false, &[], ForInOfExprKind::Expression, IterationKind::Iterate, &[] => Ok((svec(&["OBJECT", "GET_SYNC_ITER"]), true)); "infallible object/iterate")]
    #[test_case("{}", false, &[], ForInOfExprKind::Expression, IterationKind::Enumerate, &[] => Ok((svec(&["OBJECT", "JUMP_NULLISH 4", "TO_OBJECT", "ENUM_PROPS", "JUMP 2", "POP", "BREAK"]), false)); "infallible object/enumerate")]
    #[test_case("{}", false, &[], ForInOfExprKind::Expression, IterationKind::AsyncIterate, &[] => Ok((svec(&["OBJECT", "TODO"]), true)); "infallible object/async iterate")]
    fn for_in_of_head_evaluation(
        src: &str,
        strict: bool,
        names: &[&str],
        exp: ForInOfExprKind,
        iter: IterationKind,
        what: &[(Fillable, usize)],
    ) -> Result<(Vec<String>, bool), String> {
        let maker = Maker::new(src);
        let binding = match exp {
            ForInOfExprKind::Expression => ForInOfExprBinding::Expression(maker.expression()),
            ForInOfExprKind::AssignmentExpression => {
                ForInOfExprBinding::AssignmentExpression(maker.assignment_expression())
            }
        };
        let node = match &binding {
            ForInOfExprBinding::Expression(exp) => ForInOfExpr::from(exp),
            ForInOfExprBinding::AssignmentExpression(ae) => ForInOfExpr::from(ae),
        };
        let mut c = complex_filled_chunk("x", what);
        let uninitialized_bound_names = names.iter().map(|&s| JSString::from(s)).collect::<Vec<_>>();
        ForInOfStatement::for_in_of_head_evaluation(&mut c, strict, src, &uninitialized_bound_names, node, iter)
            .map(|status| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                )
            })
            .map_err(|e| e.to_string())
    }

    #[test_case("for (item in thing) ;", false, &[], &[] => Ok(svec(&[
        "STRING 0 (thing)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 8",
        "JUMP_NULLISH 4",
        "TO_OBJECT",
        "ENUM_PROPS",
        "JUMP 2",
        "POP",
        "BREAK",
        "JUMP_IF_ABRUPT 51",
        "UNDEFINED",
        "SWAP",
        "ITER_NEXT",
        "JUMP_IF_ABRUPT 44",
        "IRES_COMPLETE",
        "JUMP_IF_ABRUPT 39",
        "JUMPPOP_TRUE 33",
        "IRES_TOVAL",
        "JUMP_IF_ABRUPT 36",
        "STRING 1 (item)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 6",
        "SWAP",
        "PUT_VALUE",
        "JUMP_IF_NORMAL 6",
        "JUMP 25",
        "UNWIND 1",
        "JUMP -8",
        "POP",
        "SWAP",
        "EMPTY",
        "LOOP_CONT []",
        "JUMPPOP_FALSE 3",
        "COALESCE",
        "JUMP -37",
        "UPDATE_EMPTY",
        "UNWIND 1",
        "JUMP 8",
        "POP",
        "POP",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 2"
    ])); "assignment style/non-strict")]
    #[test_case("for (var item in thing) ;", true, &[], &[] => Ok(svec(&[
        "STRING 0 (thing)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 8",
        "JUMP_NULLISH 4",
        "TO_OBJECT",
        "ENUM_PROPS",
        "JUMP 2",
        "POP",
        "BREAK",
        "JUMP_IF_ABRUPT 51",
        "UNDEFINED",
        "SWAP",
        "ITER_NEXT",
        "JUMP_IF_ABRUPT 44",
        "IRES_COMPLETE",
        "JUMP_IF_ABRUPT 39",
        "JUMPPOP_TRUE 33",
        "IRES_TOVAL",
        "JUMP_IF_ABRUPT 36",
        "STRING 1 (item)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 6",
        "SWAP",
        "PUT_VALUE",
        "JUMP_IF_NORMAL 6",
        "JUMP 25",
        "UNWIND 1",
        "JUMP -8",
        "POP",
        "SWAP",
        "EMPTY",
        "LOOP_CONT []",
        "JUMPPOP_FALSE 3",
        "COALESCE",
        "JUMP -37",
        "UPDATE_EMPTY",
        "UNWIND 1",
        "JUMP 8",
        "POP",
        "POP",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 2"
        ])); "var binding style/strict")]
    #[test_case("for ([a,b,c] in thing) ;", false, &[], &[] => Ok(svec(&[
        "STRING 0 (thing)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 8",
        "JUMP_NULLISH 4",
        "TO_OBJECT",
        "ENUM_PROPS",
        "JUMP 2",
        "POP",
        "BREAK",
        "JUMP_IF_ABRUPT 110",
        "UNDEFINED",
        "SWAP",
        "ITER_NEXT",
        "JUMP_IF_ABRUPT 103",
        "IRES_COMPLETE",
        "JUMP_IF_ABRUPT 98",
        "JUMPPOP_TRUE 92",
        "IRES_TOVAL",
        "JUMP_IF_ABRUPT 95",
        "DUP",
        "GET_SYNC_ITER",
        "JUMP_IF_ABRUPT 64",
        "DUP",
        "STRING 1 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 11",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 7",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 18",
        "STRING 2 (b)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 11",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 7",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 18",
        "STRING 3 (c)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 11",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 7",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1",
        "EMPTY_IF_NOT_ERR",
        "ITER_CLOSE_IF_NOT_DONE",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1",
        "JUMP_IF_NORMAL 2",
        "JUMP 21",
        "POP",
        "SWAP",
        "EMPTY",
        "LOOP_CONT []",
        "JUMPPOP_FALSE 3",
        "COALESCE",
        "JUMP -96",
        "UPDATE_EMPTY",
        "UNWIND 1",
        "JUMP 8",
        "POP",
        "POP",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 2"
    ])); "destructuring style/non-strict")]
    #[test_case("for (let item in thing) ;", true, &[], &[] => Ok(svec(&[
        "PNLE",
        "CPMLB 0 (item)",
        "STRING 1 (thing)",
        "STRICT_RESOLVE",
        "PLE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 8",
        "JUMP_NULLISH 4",
        "TO_OBJECT",
        "ENUM_PROPS",
        "JUMP 2",
        "POP",
        "BREAK",
        "JUMP_IF_ABRUPT 50",
        "UNDEFINED",
        "SWAP",
        "ITER_NEXT",
        "JUMP_IF_ABRUPT 43",
        "IRES_COMPLETE",
        "JUMP_IF_ABRUPT 38",
        "JUMPPOP_TRUE 32",
        "IRES_TOVAL",
        "JUMP_IF_ABRUPT 35",
        "PNLE",
        "CPMLB 0 (item)",
        "STRING 0 (item)",
        "STRICT_RESOLVE",
        "SWAP",
        "IRB",
        "JUMP_IF_NORMAL 3",
        "PLE",
        "JUMP 22",
        "POP",
        "SWAP",
        "EMPTY",
        "PLE",
        "LOOP_CONT []",
        "JUMPPOP_FALSE 3",
        "COALESCE",
        "JUMP -36",
        "UPDATE_EMPTY",
        "UNWIND 1",
        "JUMP 8",
        "POP",
        "POP",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 2"
        ])); "let declaration style/strict")]
    #[test_case("for (item of thing) ;", false, &[], &[] => Ok(svec(&[
        "STRING 0 (thing)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 1",
        "GET_SYNC_ITER",
        "JUMP_IF_ABRUPT 54",
        "UNDEFINED",
        "SWAP",
        "ITER_NEXT",
        "JUMP_IF_ABRUPT 47",
        "IRES_COMPLETE",
        "JUMP_IF_ABRUPT 42",
        "JUMPPOP_TRUE 36",
        "IRES_TOVAL",
        "JUMP_IF_ABRUPT 39",
        "STRING 1 (item)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 10",
        "SWAP",
        "PUT_VALUE",
        "JUMP_IF_NORMAL 10",
        "ROTATEUP 3",
        "POP",
        "ITER_CLOSE",
        "JUMP 26",
        "UNWIND 1",
        "JUMP -12",
        "POP",
        "SWAP",
        "EMPTY",
        "LOOP_CONT []",
        "JUMPPOP_FALSE 3",
        "COALESCE",
        "JUMP -41",
        "UPDATE_EMPTY",
        "ITER_CLOSE",
        "JUMP 8",
        "POP",
        "POP",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 2"
        ])); "assignment style/non-strict/enumerate")]
    #[test_case("for ([item] of thing) ;", false, &[], &[] => Ok(svec(&[
        "STRING 0 (thing)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 1",
        "GET_SYNC_ITER",
        "JUMP_IF_ABRUPT 73",
        "UNDEFINED",
        "SWAP",
        "ITER_NEXT",
        "JUMP_IF_ABRUPT 66",
        "IRES_COMPLETE",
        "JUMP_IF_ABRUPT 61",
        "JUMPPOP_TRUE 55",
        "IRES_TOVAL",
        "JUMP_IF_ABRUPT 58",
        "DUP",
        "GET_SYNC_ITER",
        "JUMP_IF_ABRUPT 24",
        "DUP",
        "STRING 1 (item)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 11",
        "SWAP",
        "ITER_STEP",
        "JUMP_IF_ABRUPT 7",
        "SWAP",
        "ROTATEDOWN 3",
        "PUT_VALUE",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1",
        "EMPTY_IF_NOT_ERR",
        "ITER_CLOSE_IF_NOT_DONE",
        "UPDATE_EMPTY",
        "JUMP 2",
        "UNWIND 1",
        "JUMP_IF_NORMAL 6",
        "ROTATEUP 3",
        "POP",
        "ITER_CLOSE",
        "JUMP 22",
        "POP",
        "SWAP",
        "EMPTY",
        "LOOP_CONT []",
        "JUMPPOP_FALSE 3",
        "COALESCE",
        "JUMP -60",
        "UPDATE_EMPTY",
        "ITER_CLOSE",
        "JUMP 8",
        "POP",
        "POP",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 2"
    ])); "destructuring style/non-strict/enumerate")]
    #[test_case("for (var item of thing) ;", true, &[], &[] => Ok(svec(&[
        "STRING 0 (thing)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 1",
        "GET_SYNC_ITER",
        "JUMP_IF_ABRUPT 54",
        "UNDEFINED",
        "SWAP",
        "ITER_NEXT",
        "JUMP_IF_ABRUPT 47",
        "IRES_COMPLETE",
        "JUMP_IF_ABRUPT 42",
        "JUMPPOP_TRUE 36",
        "IRES_TOVAL",
        "JUMP_IF_ABRUPT 39",
        "STRING 1 (item)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 10",
        "SWAP",
        "PUT_VALUE",
        "JUMP_IF_NORMAL 10",
        "ROTATEUP 3",
        "POP",
        "ITER_CLOSE",
        "JUMP 26",
        "UNWIND 1",
        "JUMP -12",
        "POP",
        "SWAP",
        "EMPTY",
        "LOOP_CONT []",
        "JUMPPOP_FALSE 3",
        "COALESCE",
        "JUMP -41",
        "UPDATE_EMPTY",
        "ITER_CLOSE",
        "JUMP 8",
        "POP",
        "POP",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 2"
    ])); "var decl/strict/of-style")]
    #[test_case("for (let item of thing) ;", true, &[], &[] => Ok(svec(&[
        "PNLE",
        "CPMLB 0 (item)",
        "STRING 1 (thing)",
        "STRICT_RESOLVE",
        "PLE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 1",
        "GET_SYNC_ITER",
        "JUMP_IF_ABRUPT 53",
        "UNDEFINED",
        "SWAP",
        "ITER_NEXT",
        "JUMP_IF_ABRUPT 46",
        "IRES_COMPLETE",
        "JUMP_IF_ABRUPT 41",
        "JUMPPOP_TRUE 35",
        "IRES_TOVAL",
        "JUMP_IF_ABRUPT 38",
        "PNLE",
        "CPMLB 0 (item)",
        "STRING 0 (item)",
        "STRICT_RESOLVE",
        "SWAP",
        "IRB",
        "JUMP_IF_NORMAL 7",
        "PLE",
        "ROTATEUP 3",
        "POP",
        "ITER_CLOSE",
        "JUMP 23",
        "POP",
        "SWAP",
        "EMPTY",
        "PLE",
        "LOOP_CONT []",
        "JUMPPOP_FALSE 3",
        "COALESCE",
        "JUMP -40",
        "UPDATE_EMPTY",
        "ITER_CLOSE",
        "JUMP 8",
        "POP",
        "POP",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 2"
    ])); "let decl/strict/of-style")]
    #[test_case("for await (item of thing);", true, &[], &[] => panics "not yet implemented"; "await style: assignment")]
    #[test_case("for await ([item] of thing);", true, &[], &[] => panics "not yet implemented"; "await style: destructuring")]
    #[test_case("for await (var item of thing);", true, &[], &[] => panics "not yet implemented"; "await style: for binding")]
    #[test_case("for await (let item of thing);", true, &[], &[] => panics "not yet implemented"; "await style: for declaration")]
    #[test_case("for (x in 8n);", false, &[], &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "head compile fails")]
    #[test_case("for (x in thing) 8n;", false, &[], &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "body compile fails")]
    #[test_case("for (x in thing) @@(51);", false, &[], &[] => serr("out of range integral type conversion attempted"); "body too large")]
    #[test_case("for (let x in {});", false, &[], &[] => Ok(svec(&[
        "PNLE",
        "CPMLB 0 (x)",
        "OBJECT",
        "PLE",
        "JUMP_NULLISH 4",
        "TO_OBJECT",
        "ENUM_PROPS",
        "JUMP 2",
        "POP",
        "BREAK",
        "UNDEFINED",
        "SWAP",
        "ITER_NEXT",
        "JUMP_IF_ABRUPT 43",
        "IRES_COMPLETE",
        "JUMP_IF_ABRUPT 38",
        "JUMPPOP_TRUE 32",
        "IRES_TOVAL",
        "JUMP_IF_ABRUPT 35",
        "PNLE",
        "CPMLB 0 (x)",
        "STRING 0 (x)",
        "RESOLVE",
        "SWAP",
        "IRB",
        "JUMP_IF_NORMAL 3",
        "PLE",
        "JUMP 22",
        "POP",
        "SWAP",
        "EMPTY",
        "PLE",
        "LOOP_CONT []",
        "JUMPPOP_FALSE 3",
        "COALESCE",
        "JUMP -36",
        "UPDATE_EMPTY",
        "UNWIND 1",
        "JUMP 8",
        "POP",
        "POP",
        "JUMP 4",
        "UNWIND 1",
        "UNWIND 2"
    ])); "all infallible")]
    fn for_in_of_evaluation(
        src: &str,
        strict: bool,
        labels: &[&str],
        what: &[(Fillable, usize)],
    ) -> Result<Vec<String>, String> {
        let node = Maker::new(src).for_in_of_statement();
        let mut c = complex_filled_chunk("x", what);
        let label_set = labels.iter().map(|&s| JSString::from(s)).collect::<Vec<_>>();
        node.for_in_of_evaluation(&mut c, strict, src, &label_set)
            .map(|_| c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>())
            .map_err(|e| e.to_string())
    }
    #[derive(Copy, Clone)]
    enum LHSKind {
        Assignment,
        Destructuring,
        VarBinding,
        LexicalBinding,
    }
    enum LHSBinding {
        Assignment(Rc<LeftHandSideExpression>),
        Destructuring(Rc<AssignmentPattern>),
        VarBinding(Rc<ForBinding>),
        LexicalBinding(Rc<ForDeclaration>),
    }

    #[test_case("item", true, LHSKind::Assignment, ";", IterationKind::Iterate, &[], IteratorKind::Sync, &[]
            => Ok(svec(&[
                "UNDEFINED",
                "SWAP",
                "ITER_NEXT",
                "JUMP_IF_ABRUPT 47",
                "IRES_COMPLETE",
                "JUMP_IF_ABRUPT 42",
                "JUMPPOP_TRUE 36",
                "IRES_TOVAL",
                "JUMP_IF_ABRUPT 39",
                "STRING 0 (item)",
                "STRICT_RESOLVE",
                "JUMP_IF_ABRUPT 10",
                "SWAP",
                "PUT_VALUE",
                "JUMP_IF_NORMAL 10",
                "ROTATEUP 3",
                "POP",
                "ITER_CLOSE",
                "JUMP 26",
                "UNWIND 1",
                "JUMP -12",
                "POP",
                "SWAP",
                "EMPTY",
                "LOOP_CONT []",
                "JUMPPOP_FALSE 3",
                "COALESCE",
                "JUMP -41",
                "UPDATE_EMPTY",
                "ITER_CLOSE",
                "JUMP 8",
                "POP",
                "POP",
                "JUMP 4",
                "UNWIND 1",
                "UNWIND 2"
            ]))
            ; "for item of / strict / sync")]
    #[test_case("item", true, LHSKind::Assignment, ";", IterationKind::AsyncIterate, &[], IteratorKind::Async, &[]
            => Ok(svec(&[
                "UNDEFINED",
                "SWAP",
                "TODO",
                "JUMP_IF_ABRUPT 47",
                "IRES_COMPLETE",
                "JUMP_IF_ABRUPT 42",
                "JUMPPOP_TRUE 36",
                "IRES_TOVAL",
                "JUMP_IF_ABRUPT 39",
                "STRING 0 (item)",
                "STRICT_RESOLVE",
                "JUMP_IF_ABRUPT 10",
                "SWAP",
                "PUT_VALUE",
                "JUMP_IF_NORMAL 10",
                "ROTATEUP 3",
                "POP",
                "TODO",
                "JUMP 26",
                "UNWIND 1",
                "JUMP -12",
                "POP",
                "SWAP",
                "EMPTY",
                "LOOP_CONT []",
                "JUMPPOP_FALSE 3",
                "COALESCE",
                "JUMP -41",
                "UPDATE_EMPTY",
                "TODO",
                "JUMP 8",
                "POP",
                "POP",
                "JUMP 4",
                "UNWIND 1",
                "UNWIND 2"
            ]))
            ; "for await item of / strict / async")]
    #[test_case("[item]", true, LHSKind::Destructuring, ";", IterationKind::Enumerate, &[], IteratorKind::Sync, &[]
            => Ok(svec(&[
                "UNDEFINED",
                "SWAP",
                "ITER_NEXT",
                "JUMP_IF_ABRUPT 63",
                "IRES_COMPLETE",
                "JUMP_IF_ABRUPT 58",
                "JUMPPOP_TRUE 52",
                "IRES_TOVAL",
                "JUMP_IF_ABRUPT 55",
                "DUP",
                "GET_SYNC_ITER",
                "JUMP_IF_ABRUPT 24",
                "DUP",
                "STRING 0 (item)",
                "STRICT_RESOLVE",
                "JUMP_IF_ABRUPT 11",
                "SWAP",
                "ITER_STEP",
                "JUMP_IF_ABRUPT 7",
                "SWAP",
                "ROTATEDOWN 3",
                "PUT_VALUE",
                "UPDATE_EMPTY",
                "JUMP 2",
                "UNWIND 1",
                "EMPTY_IF_NOT_ERR",
                "ITER_CLOSE_IF_NOT_DONE",
                "UPDATE_EMPTY",
                "JUMP 2",
                "UNWIND 1",
                "JUMP_IF_NORMAL 2",
                "JUMP 21",
                "POP",
                "SWAP",
                "EMPTY",
                "LOOP_CONT []",
                "JUMPPOP_FALSE 3",
                "COALESCE",
                "JUMP -56",
                "UPDATE_EMPTY",
                "UNWIND 1",
                "JUMP 8",
                "POP",
                "POP",
                "JUMP 4",
                "UNWIND 1",
                "UNWIND 2"
            ]))
            ; "for [item] in / strict / sync")]
    #[test_case(
        "[item=9n]",
            true,
            LHSKind::Destructuring,
            ";",
            IterationKind::Enumerate,
            &[],
            IteratorKind::Sync,
            &[(Fillable::BigInt, 0)]
        => serr("Out of room for big ints in this compilation unit");
        "for [item] in / destructuring compile fails"
    )]
    #[test_case("item", true, LHSKind::VarBinding, ";", IterationKind::Enumerate, &[], IteratorKind::Sync, &[]
            => Ok(svec(&[
                "UNDEFINED",
                "SWAP",
                "ITER_NEXT",
                "JUMP_IF_ABRUPT 44",
                "IRES_COMPLETE",
                "JUMP_IF_ABRUPT 39",
                "JUMPPOP_TRUE 33",
                "IRES_TOVAL",
                "JUMP_IF_ABRUPT 36",
                "STRING 0 (item)",
                "STRICT_RESOLVE",
                "JUMP_IF_ABRUPT 6",
                "SWAP",
                "PUT_VALUE",
                "JUMP_IF_NORMAL 6",
                "JUMP 25",
                "UNWIND 1",
                "JUMP -8",
                "POP",
                "SWAP",
                "EMPTY",
                "LOOP_CONT []",
                "JUMPPOP_FALSE 3",
                "COALESCE",
                "JUMP -37",
                "UPDATE_EMPTY",
                "UNWIND 1",
                "JUMP 8",
                "POP",
                "POP",
                "JUMP 4",
                "UNWIND 1",
                "UNWIND 2"
            ]))
            ; "for var item in / strict / sync")]
    #[test_case("[item]", true, LHSKind::VarBinding, ";", IterationKind::Iterate, &[], IteratorKind::Sync, &[]
            => Ok(svec(&[
                "UNDEFINED",
                "SWAP",
                "ITER_NEXT",
                "JUMP_IF_ABRUPT 62",
                "IRES_COMPLETE",
                "JUMP_IF_ABRUPT 57",
                "JUMPPOP_TRUE 51",
                "IRES_TOVAL",
                "JUMP_IF_ABRUPT 54",
                "GET_SYNC_ITER",
                "JUMP_IF_ABRUPT 23",
                "DUP",
                "STRING 0 (item)",
                "STRICT_RESOLVE",
                "JUMP_IF_ABRUPT 13",
                "SWAP",
                "ITER_STEP",
                "JUMP_IF_ABRUPT 9",
                "SWAP",
                "ROTATEDOWN 3",
                "PUT_VALUE",
                "JUMP_IF_ABRUPT 3",
                "POP",
                "JUMP 2",
                "UNWIND 1",
                "EMPTY_IF_NOT_ERR",
                "ITER_CLOSE_IF_NOT_DONE",
                "JUMP_IF_NORMAL 6",
                "ROTATEUP 3",
                "POP",
                "ITER_CLOSE",
                "JUMP 22",
                "POP",
                "SWAP",
                "EMPTY",
                "LOOP_CONT []",
                "JUMPPOP_FALSE 3",
                "COALESCE",
                "JUMP -56",
                "UPDATE_EMPTY",
                "ITER_CLOSE",
                "JUMP 8",
                "POP",
                "POP",
                "JUMP 4",
                "UNWIND 1",
                "UNWIND 2"
            ]))
            ; "for var [item] of / strict / sync")]
    #[test_case("[item=8n]", true, LHSKind::VarBinding, ";", IterationKind::Iterate, &[], IteratorKind::Sync, &[(Fillable::BigInt, 0)]
            => serr("Out of room for big ints in this compilation unit")
            ; "for var [item] of / destructuring compile fails")]
    #[test_case("item", true, LHSKind::VarBinding, ";", IterationKind::Iterate, &[], IteratorKind::Sync, &[(Fillable::String, 0)]
            => serr("Out of room for strings in this compilation unit")
            ; "for var item of / binding compile fails")]
    #[test_case("item", true, LHSKind::Assignment, ";", IterationKind::Iterate, &[], IteratorKind::Sync, &[(Fillable::String, 0)]
            => serr("Out of room for strings in this compilation unit")
            ; "for item of / binding compile fails")]
    #[test_case("let item", true, LHSKind::LexicalBinding, ";", IterationKind::Iterate, &[], IteratorKind::Sync, &[]
            => Ok(svec(&[
                "UNDEFINED",
                "SWAP",
                "ITER_NEXT",
                "JUMP_IF_ABRUPT 46",
                "IRES_COMPLETE",
                "JUMP_IF_ABRUPT 41",
                "JUMPPOP_TRUE 35",
                "IRES_TOVAL",
                "JUMP_IF_ABRUPT 38",
                "PNLE",
                "CPMLB 0 (item)",
                "STRING 0 (item)",
                "STRICT_RESOLVE",
                "SWAP",
                "IRB",
                "JUMP_IF_NORMAL 7",
                "PLE",
                "ROTATEUP 3",
                "POP",
                "ITER_CLOSE",
                "JUMP 23",
                "POP",
                "SWAP",
                "EMPTY",
                "PLE",
                "LOOP_CONT []",
                "JUMPPOP_FALSE 3",
                "COALESCE",
                "JUMP -40",
                "UPDATE_EMPTY",
                "ITER_CLOSE",
                "JUMP 8",
                "POP",
                "POP",
                "JUMP 4",
                "UNWIND 1",
                "UNWIND 2"
            ]))
            ; "for let item of / strict / sync")]
    #[test_case("let item", true, LHSKind::LexicalBinding, ";", IterationKind::Iterate, &[], IteratorKind::Sync, &[(Fillable::String, 0)]
            => serr("Out of room for strings in this compilation unit")
            ; "for let item of / binding instantiation fails")]
    #[test_case("let [destructure]", true, LHSKind::LexicalBinding, ";", IterationKind::Iterate, &[], IteratorKind::Sync, &[]
            => Ok(svec(&["UNDEFINED", "SWAP", "ITER_NEXT", "JUMP_IF_ABRUPT 67", "IRES_COMPLETE", "JUMP_IF_ABRUPT 62", "JUMPPOP_TRUE 56", "IRES_TOVAL", "JUMP_IF_ABRUPT 59", "PNLE", "CPMLB 0 (destructure)", "GET_SYNC_ITER", "JUMP_IF_ABRUPT 23", "DUP", "STRING 0 (destructure)", "STRICT_RESOLVE", "JUMP_IF_ABRUPT 13", "SWAP", "ITER_STEP", "JUMP_IF_ABRUPT 9", "SWAP", "ROTATEDOWN 3", "IRB", "JUMP_IF_ABRUPT 3", "POP", "JUMP 2", "UNWIND 1", "EMPTY_IF_NOT_ERR", "ITER_CLOSE_IF_NOT_DONE", "JUMP_IF_NORMAL 7", "PLE", "ROTATEUP 3", "POP", "ITER_CLOSE", "JUMP 23", "POP", "SWAP", "EMPTY", "PLE", "LOOP_CONT []", "JUMPPOP_FALSE 3", "COALESCE", "JUMP -61", "UPDATE_EMPTY", "ITER_CLOSE", "JUMP 8", "POP", "POP", "JUMP 4", "UNWIND 1", "UNWIND 2"]))
            ; "for let [destructure] of / strict / sync ")]
    #[test_case("let [destructure]", true, LHSKind::LexicalBinding, ";", IterationKind::Iterate, &[], IteratorKind::Sync, &[(Fillable::String, 0)]
            => serr("Out of room for strings in this compilation unit")
            ; "for let [destructure] of / binding initialization fails ")] //4639
    #[test_case("let item", false, LHSKind::LexicalBinding, ";", IterationKind::Iterate, &[], IteratorKind::Sync, &[]
            => Ok(svec(&[
                "UNDEFINED",
                "SWAP",
                "ITER_NEXT",
                "JUMP_IF_ABRUPT 46",
                "IRES_COMPLETE",
                "JUMP_IF_ABRUPT 41",
                "JUMPPOP_TRUE 35",
                "IRES_TOVAL",
                "JUMP_IF_ABRUPT 38",
                "PNLE",
                "CPMLB 0 (item)",
                "STRING 0 (item)",
                "RESOLVE",
                "SWAP",
                "IRB",
                "JUMP_IF_NORMAL 7",
                "PLE",
                "ROTATEUP 3",
                "POP",
                "ITER_CLOSE",
                "JUMP 23",
                "POP",
                "SWAP",
                "EMPTY",
                "PLE",
                "LOOP_CONT []",
                "JUMPPOP_FALSE 3",
                "COALESCE",
                "JUMP -40",
                "UPDATE_EMPTY",
                "ITER_CLOSE",
                "JUMP 8",
                "POP",
                "POP",
                "JUMP 4",
                "UNWIND 1",
                "UNWIND 2"
            ]))
            ; "for let item of / non-strict / sync")]
    #[test_case("let item", true, LHSKind::LexicalBinding, "8n;", IterationKind::Iterate, &[], IteratorKind::Sync, &[(Fillable::BigInt, 0)]
            => serr("Out of room for big ints in this compilation unit")
            ; "statement compile fails")]
    #[test_case("let item", true, LHSKind::LexicalBinding, ";", IterationKind::Iterate, &["lbl1", "lbl2"], IteratorKind::Sync, &[]
            => Ok(svec(&[
                "UNDEFINED",
                "SWAP",
                "ITER_NEXT",
                "JUMP_IF_ABRUPT 46",
                "IRES_COMPLETE",
                "JUMP_IF_ABRUPT 41",
                "JUMPPOP_TRUE 35",
                "IRES_TOVAL",
                "JUMP_IF_ABRUPT 38",
                "PNLE",
                "CPMLB 0 (item)",
                "STRING 0 (item)",
                "STRICT_RESOLVE",
                "SWAP",
                "IRB",
                "JUMP_IF_NORMAL 7",
                "PLE",
                "ROTATEUP 3",
                "POP",
                "ITER_CLOSE",
                "JUMP 23",
                "POP",
                "SWAP",
                "EMPTY",
                "PLE",
                "LOOP_CONT [lbl1, lbl2]",
                "JUMPPOP_FALSE 3",
                "COALESCE",
                "JUMP -40",
                "UPDATE_EMPTY",
                "ITER_CLOSE",
                "JUMP 8",
                "POP",
                "POP",
                "JUMP 4",
                "UNWIND 1",
                "UNWIND 2"
            ]))
            ; "with legit label set")]
    #[test_case("let item", true, LHSKind::LexicalBinding, ";", IterationKind::Iterate, &["lbl1", "lbl2"], IteratorKind::Sync, &[(Fillable::StringSet, 0)]
            => serr("Out of room for string sets in this compilation unit")
            ; "string set table full")]
    #[test_case("let item", true, LHSKind::LexicalBinding, "@@@;", IterationKind::Iterate, &[], IteratorKind::Sync, &[]
            => serr("out of range integral type conversion attempted")
            ; "jump back too far")]
    #[test_case("let item", true, LHSKind::LexicalBinding, "@@(41);", IterationKind::Iterate, &[], IteratorKind::Sync, &[]
            => serr("out of range integral type conversion attempted")
            ; "unwind3 jump too far")]
    #[test_case("let item", true, LHSKind::LexicalBinding, "@@(46);", IterationKind::Iterate, &[], IteratorKind::Sync, &[]
            => serr("out of range integral type conversion attempted")
            ; "longest unwind2 jump too far")]
    fn for_in_of_body_evaluation(
        lhs_src: &str,
        strict: bool,
        lhs_kind: LHSKind,
        stmt_src: &str,
        iteration_kind: IterationKind,
        labels: &[&str],
        iterator_kind: IteratorKind,
        what: &[(Fillable, usize)],
    ) -> Result<Vec<String>, String> {
        let lhs_maker = Maker::new(lhs_src);
        let binding = match lhs_kind {
            LHSKind::Assignment => LHSBinding::Assignment(lhs_maker.left_hand_side_expression()),
            LHSKind::Destructuring => LHSBinding::Destructuring(lhs_maker.assignment_pattern()),
            LHSKind::VarBinding => LHSBinding::VarBinding(lhs_maker.for_binding()),
            LHSKind::LexicalBinding => LHSBinding::LexicalBinding(lhs_maker.for_declaration()),
        };
        let node = match &binding {
            LHSBinding::Assignment(item) => ForInOfLHSExpr::from(item),
            LHSBinding::Destructuring(item) => ForInOfLHSExpr::from(item),
            LHSBinding::VarBinding(item) => ForInOfLHSExpr::from(item),
            LHSBinding::LexicalBinding(item) => ForInOfLHSExpr::from(item),
        };
        let label_set = labels.iter().map(|&s| JSString::from(s)).collect::<Vec<_>>();
        let mut c = complex_filled_chunk("x", what);
        let stmt = Maker::new(stmt_src).statement();

        ForInOfStatement::for_in_of_body_evaluation(
            &mut c,
            strict,
            "",
            node,
            &stmt,
            iteration_kind,
            &label_set,
            iterator_kind,
        )
        .map(|_| c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>())
        .map_err(|e| e.to_string())
    }
}

mod object_assignment_pattern {
    use super::*;
    use test_case::test_case;

    #[test_case("{}", true, &[] => Ok(svec(&["REQ_COER"])); "empty")]
    #[test_case("{...a}", true, &[] => Ok(svec(&[
        "REQ_COER",
        "JUMP_IF_ABRUPT 26",
        "ZERO",
        "DUP_AFTER_LIST",
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 14",
        "ROTATEDOWN_LIST 1",
        "OBJECT",
        "ROTATEDOWN_LIST 1",
        "COPY_DATAPROPS_WE",
        "JUMP_IF_ABRUPT 7",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 6",
        "POP",
        "JUMP 5",
        "UNWIND_LIST",
        "UNWIND 1",
        "UNWIND 1"
    ])); "rest-only: normal")]
    #[test_case("{...b}", true, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "rest-only; fail")]
    #[test_case("{...b[@@(36)]}", true, &[] => serr("out of range integral type conversion attempted"); "rest-only; too large")]
    #[test_case("{a,b}", true, &[] => Ok(svec(&[
        "REQ_COER",
        "JUMP_IF_ABRUPT 66",
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 19",
        "SWAP",
        "POP2_PUSH3",
        "STRING 0 (a)",
        "GETV",
        "JUMP_IF_ABRUPT 10",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 9",
        "POP",
        "STRING 0 (a)",
        "FLOAT 0 (1)",
        "JUMP 2",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 33",
        "SWAP_LIST",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 19",
        "SWAP",
        "POP2_PUSH3",
        "STRING 1 (b)",
        "GETV",
        "JUMP_IF_ABRUPT 10",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 9",
        "POP",
        "STRING 1 (b)",
        "FLOAT 0 (1)",
        "JUMP 2",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 4",
        "SWAP_DEEP_LIST",
        "APPEND_LIST",
        "JUMP 2",
        "POP_OUT_LIST 3",
        "JUMP_IF_ABRUPT 3",
        "POP_LIST",
        "JUMP 2",
        "UNWIND 1"
    ])); "list-only; normal")]
    #[test_case("{a,b,}", true, &[] => Ok(svec(&[
        "REQ_COER",
        "JUMP_IF_ABRUPT 66",
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 19",
        "SWAP",
        "POP2_PUSH3",
        "STRING 0 (a)",
        "GETV",
        "JUMP_IF_ABRUPT 10",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 9",
        "POP",
        "STRING 0 (a)",
        "FLOAT 0 (1)",
        "JUMP 2",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 33",
        "SWAP_LIST",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 19",
        "SWAP",
        "POP2_PUSH3",
        "STRING 1 (b)",
        "GETV",
        "JUMP_IF_ABRUPT 10",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 9",
        "POP",
        "STRING 1 (b)",
        "FLOAT 0 (1)",
        "JUMP 2",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 4",
        "SWAP_DEEP_LIST",
        "APPEND_LIST",
        "JUMP 2",
        "POP_OUT_LIST 3",
        "JUMP_IF_ABRUPT 3",
        "POP_LIST",
        "JUMP 2",
        "UNWIND 1"
    ])); "list-rest/empty-rest; normal")]
    #[test_case("{a=1n,b}", true, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "list-only; fail")]
    #[test_case("{a=@@(70),b}", true, &[] => serr("out of range integral type conversion attempted"); "list-only; too big")]
    #[test_case("{a,...b}", true, &[] => Ok(svec(&[
        "REQ_COER",
        "JUMP_IF_ABRUPT 55",
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 19",
        "SWAP",
        "POP2_PUSH3",
        "STRING 0 (a)",
        "GETV",
        "JUMP_IF_ABRUPT 10",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 9",
        "POP",
        "STRING 0 (a)",
        "FLOAT 0 (1)",
        "JUMP 2",
        "UNWIND 1",
        "JUMP_IF_ABRUPT 27",
        "DUP_AFTER_LIST",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 14",
        "ROTATEDOWN_LIST 1",
        "OBJECT",
        "ROTATEDOWN_LIST 1",
        "COPY_DATAPROPS_WE",
        "JUMP_IF_ABRUPT 7",
        "PUT_VALUE",
        "JUMP_IF_ABRUPT 6",
        "POP",
        "JUMP 5",
        "UNWIND_LIST",
        "UNWIND 1",
        "UNWIND 1",
        "JUMP 2",
        "UNWIND 1"
    ])); "list-rest; normal")]
    #[test_case("{a=8n,...b}", true, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "list-rest; list-fail")]
    #[test_case("{a,...b[8n]}", true, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "list-rest; rest-fail")]
    #[test_case("{a,...b[@@(37)]}", true, &[] => serr("out of range integral type conversion attempted"); "list-rest; rest too big")]
    #[test_case("{a=@@(59),...b}", true, &[] => serr("out of range integral type conversion attempted"); "list-rest; list too big")]
    fn destructuring_assignment_evaluation(
        src: &str,
        strict: bool,
        what: &[(Fillable, usize)],
    ) -> Result<Vec<String>, String> {
        let node = Maker::new(src).object_assignment_pattern();
        let mut c = complex_filled_chunk("x", what);

        node.destructuring_assignment_evaluation(&mut c, strict, src)
            .map_err(|e| e.to_string())
            .map(|_| c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>())
    }
}

mod class_element_name {
    use super::*;
    use test_case::test_case;

    #[test_case("name", true, &[] => Ok((svec(&["STRING 0 (name)"]), false)); "property name")]
    #[test_case("#name", true, &[] => Ok((svec(&["PRIV_ID_LOOKUP 0 (#name)"]), false)); "private name")]
    #[test_case("#name", true, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "private name fail")]
    fn compile(src: &str, strict: bool, what: &[(Fillable, usize)]) -> Result<(Vec<String>, bool), String> {
        let node = Maker::new(src).class_element_name();
        let mut c = complex_filled_chunk("x", what);

        node.compile(&mut c, strict, src).map_err(|e| e.to_string()).map(|flags| {
            (
                c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                flags.maybe_abrupt(),
            )
        })
    }
}

mod field_definition {
    use super::*;
    use test_case::test_case;

    #[test_case("name", true, &[] => Ok((svec(&["STRING 0 (name)", "EMPTY"]), false)); "infallible; no init")]
    #[test_case("[a]", true, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 1",
        "TO_KEY",
        "JUMP_IF_ABRUPT 3",
        "EMPTY",
        "JUMP 2",
        "UNWIND 1"
    ]), true)); "fallible; no init")]
    #[test_case("a=10", true, &[] => Ok((svec(&["STRING 0 (a)", "EVAL_CLASS_FIELD_DEF 0"]), false)); "with init")]
    #[test_case("[9n]", true, &[(Fillable::BigInt, 0)] => serr("Out of room for big ints in this compilation unit"); "name fail")]
    #[test_case("a=23", true, &[(Fillable::FunctionStash, 0)] => serr("Out of room for more functions!"); "init fail")]
    fn class_field_definition_evaluation(
        src: &str,
        strict: bool,
        what: &[(Fillable, usize)],
    ) -> Result<(Vec<String>, bool), String> {
        let node = Maker::new(src).field_definition();
        let mut c = complex_filled_chunk("x", what);
        let node2 = node.clone();

        node.class_field_definition_evaluation(&mut c, strict, src, node2).map_err(|e| e.to_string()).map(|flags| {
            (
                c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                flags.maybe_abrupt(),
            )
        })
    }
}

mod class_static_block_statement_list {
    use super::*;
    use test_case::test_case;

    #[test_case("", true, &[] => Ok((svec(&["UNDEFINED"]), false)); "empty")]
    #[test_case("this.item = 3;", true, &[] => Ok((svec(&[
        "THIS",
        "JUMP_IF_ABRUPT 3",
        "STRING 0 (item)",
        "STRICT_REF",
        "JUMP_IF_ABRUPT 5",
        "FLOAT 0 (3)",
        "POP2_PUSH3",
        "PUT_VALUE",
        "UPDATE_EMPTY"
    ]), true)); "slist")]
    fn compile(src: &str, strict: bool, what: &[(Fillable, usize)]) -> Result<(Vec<String>, bool), String> {
        let node = Maker::new(src).class_static_block_statement_list();
        let mut c = complex_filled_chunk("x", what);

        node.compile(&mut c, strict, src).map_err(|e| e.to_string()).map(|flags| {
            (
                c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                flags.maybe_abrupt(),
            )
        })
    }
}

mod class_static_block_body {
    use super::*;
    use test_case::test_case;

    #[test_case("", true, &[] => Ok((svec(&["UNDEFINED"]), false)); "empty")]
    #[test_case("this.item = 3;", true, &[] => Ok((svec(&[
        "THIS",
        "JUMP_IF_ABRUPT 3",
        "STRING 0 (item)",
        "STRICT_REF",
        "JUMP_IF_ABRUPT 5",
        "FLOAT 0 (3)",
        "POP2_PUSH3",
        "PUT_VALUE",
        "UPDATE_EMPTY"
    ]), true)); "slist")]
    fn compile(src: &str, strict: bool, what: &[(Fillable, usize)]) -> Result<(Vec<String>, bool), String> {
        let node = Maker::new(src).class_static_block_body();
        let mut c = complex_filled_chunk("x", what);

        node.compile(&mut c, strict, src).map_err(|e| e.to_string()).map(|flags| {
            (
                c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                flags.maybe_abrupt(),
            )
        })
    }
}

mod class_static_block {
    use super::*;
    use test_case::test_case;

    #[test_case("static {}", true, &[] => Ok(svec(&["EVAL_CLASS_SBLK_DEF 0"])); "normal")]
    #[test_case("static {}", true, &[(Fillable::FunctionStash, 0)] => serr("Out of room for more functions!"); "compile error")]
    fn class_static_block_definition_evaluation(
        src: &str,
        strict: bool,
        what: &[(Fillable, usize)],
    ) -> Result<Vec<String>, String> {
        let node = Maker::new(src).class_static_block();
        let mut c = complex_filled_chunk("x", what);
        let node2 = node.clone();

        node.class_static_block_definition_evaluation(&mut c, strict, node2)
            .map_err(|e| e.to_string())
            .map(|_| c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>())
    }
}

mod optional_expression {
    use super::*;
    use test_case::test_case;

    #[test_case(
        "a?.b", true, &[]
        => Ok((svec(&[
            "STRING 0 (a)",
            "STRICT_RESOLVE",
            "JUMP_IF_ABRUPT 20",
            "DUP",
            "GET_VALUE",
            "JUMP_IF_ABRUPT 14",
            "JUMP_NOT_NULLISH 5",
            "POP",
            "POP",
            "UNDEFINED",
            "JUMP 9",
            "UNWIND 1",
            "STRING 1 (b)",
            "STRICT_REF",
            "JUMP 2",
            "UNWIND 1"
        ]), true, true));
        "oe: me oc; ok"
    )]
    #[test_case(
        "a[1n]?.b", false, &[(Fillable::BigInt, 0)]
        => serr("Out of room for big ints in this compilation unit");
        "oe: me oc; me compile fails"
    )]
    #[test_case(
        "({})?.b", false, &[]
        => Ok((svec(&[
            "OBJECT",
            "DUP",
            "JUMP_NOT_NULLISH 5",
            "POP",
            "POP",
            "UNDEFINED",
            "JUMP 5",
            "UNWIND 1",
            "STRING 0 (b)",
            "REF"
        ]), false, true));
        "oe: me oc; nonabrupt me"
    )]
    #[test_case(
        "({})?.[1n]", false, &[(Fillable::BigInt, 0)]
        => serr("Out of room for big ints in this compilation unit");
        "oe: me oc; oc compile fails"
    )]
    #[test_case(
        "a?.[@@@]", false, &[]
        => serr("out of range integral type conversion attempted");
        "oc: me oc; me is ref + oc too big"
    )]
    #[test_case(
        "({})?.[@@@]", false, &[]
        => serr("out of range integral type conversion attempted");
        "oc: me oc; me not ref + oc too big"
    )]
    #[test_case(
        "a?.[@@(24)]", false, &[]
        => serr("out of range integral type conversion attempted");
        "oc: me oc; me fallible + oc too big (but smaller)"
    )]
    #[test_case(
        "a()?.b", false, &[]
        => Ok((
            svec(&[
                "STRING 0 (a)",
                "RESOLVE",
                "DUP",
                "GET_VALUE",
                "JUMP_IF_NORMAL 4",
                "UNWIND 1",
                "JUMP 3",
                "FLOAT 0 (0)",
                "CALL",
                "JUMP_IF_ABRUPT 13",
                "DUP",
                "JUMP_NOT_NULLISH 5",
                "POP",
                "POP",
                "UNDEFINED",
                "JUMP 5",
                "UNWIND 1",
                "STRING 1 (b)",
                "REF"
            ]),
            true,
            true,
        ));
        "oc: ce oc; success"
    )]
    #[test_case(
        "a(1n)?.b", false, &[(Fillable::BigInt, 0)]
        => serr("Out of room for big ints in this compilation unit");
        "oc: ce oc; ce compile fails"
    )]
    #[test_case(
        "a?.b?.c", false, &[]
        => Ok((
            svec(&[
                "STRING 0 (a)",
                "RESOLVE",
                "JUMP_IF_ABRUPT 20",
                "DUP",
                "GET_VALUE",
                "JUMP_IF_ABRUPT 14",
                "JUMP_NOT_NULLISH 5",
                "POP",
                "POP",
                "UNDEFINED",
                "JUMP 9",
                "UNWIND 1",
                "STRING 1 (b)",
                "REF",
                "JUMP 2",
                "UNWIND 1",
                "JUMP_IF_ABRUPT 20",
                "DUP",
                "GET_VALUE",
                "JUMP_IF_ABRUPT 14",
                "JUMP_NOT_NULLISH 5",
                "POP",
                "POP",
                "UNDEFINED",
                "JUMP 9",
                "UNWIND 1",
                "STRING 2 (c)",
                "REF",
                "JUMP 2",
                "UNWIND 1"
            ]),
            true,
            true
        ));
        "oc: oe oc; success"
    )]
    #[test_case(
        "a(1n)?.b?.c", false, &[(Fillable::BigInt, 0)]
        => serr("Out of room for big ints in this compilation unit");
        "oc: oe oc; oe compile fails"
    )]
    fn compile(src: &str, strict: bool, what: &[(Fillable, usize)]) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).optional_expression();
        let mut c = complex_filled_chunk("x", what);

        node.compile(&mut c, strict, src).map_err(|e| e.to_string()).map(|flags| {
            (
                c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                flags.maybe_abrupt(),
                flags.maybe_ref(),
            )
        })
    }
}

mod optional_chain {
    use super::*;
    use test_case::test_case;

    #[test_case(
        "?.()", true, &[]
        => Ok((svec(&["FLOAT 0 (0)", "CALL_STRICT"]), true, false));
        "oc: args; success"
    )]
    #[test_case(
        "?.[0]", true, &[]
        => Ok(
            (svec(&[
                "UNWIND 1", "FLOAT 0 (0)", "TO_KEY", "JUMP_IF_ABRUPT 1", "STRICT_REF", "UNWIND_IF_ABRUPT 1"
            ]),
            true,
            true
        ));
        "oc: exp; success"
    )]
    #[test_case(
        "?.a", true, &[]
        => Ok((svec(&["UNWIND 1", "STRING 0 (a)", "STRICT_REF"]), false, true));
        "oc: id; success"
    )]
    #[test_case("?.``", true, &[] => panics "not yet implemented"; "oc: template")]
    #[test_case("?.#a", true, &[] => panics "not yet implemented"; "oc: privateid")]
    #[test_case(
        "?.a()", true, &[]
        => Ok((
            svec(&[
                "UNWIND 1",
                "STRING 0 (a)",
                "STRICT_REF",
                "DUP",
                "GET_VALUE",
                "JUMP_IF_ABRUPT 5",
                "FLOAT 0 (0)",
                "CALL_STRICT",
                "JUMP 2",
                "UNWIND 1"
            ]),
            true,
            false
        ));
        "oc: oc args; success, oc is ref"
    )]
    #[test_case(
        "?.()()", false, &[]
        => Ok((
            svec(&["FLOAT 0 (0)", "CALL", "JUMP_IF_ABRUPT 4", "DUP", "FLOAT 0 (0)", "CALL"]),
            true,
            false
        ));
        "oc: oc args; oc not ref"
    )]
    #[test_case(
        "?.(1n)()", false, &[(Fillable::BigInt, 0)]
        => serr("Out of room for big ints in this compilation unit");
        "oc: oc args; oc compile fails"
    )]
    #[test_case(
        "?.()(1n)", false, &[(Fillable::BigInt, 0)]
        => serr("Out of room for big ints in this compilation unit");
        "oc: oc args; args compile fails"
    )]
    #[test_case(
        "?.a(@@@)", false, &[]
        => serr("out of range integral type conversion attempted");
        "oc: oc args; args too big (after ref)"
    )]
    #[test_case(
        "?.()(@@@)", false, &[]
        => serr("out of range integral type conversion attempted");
        "oc: oc args; args too big (no ref)"
    )]
    #[test_case(
        "?.a[0]", false, &[]
        => Ok((
            svec(&[
                "UNWIND 1",
                "STRING 0 (a)",
                "REF",
                "GET_VALUE",
                "JUMP_IF_ABRUPT 8",
                "FLOAT 0 (0)",
                "TO_KEY",
                "JUMP_IF_ABRUPT 1",
                "REF",
                "UNWIND_IF_ABRUPT 1"
            ]),
            true,
            true
        ));
        "oc: oc exp; success"
    )]
    #[test_case(
        "?.[1n][0]", false, &[(Fillable::BigInt, 0)]
        => serr("Out of room for big ints in this compilation unit");
        "oc: oc exp; oc compile fails"
    )]
    #[test_case(
        "?.()[0]", false, &[]
        => Ok((
            svec(&[
                "FLOAT 0 (0)",
                "CALL",
                "JUMP_IF_ABRUPT 8",
                "FLOAT 0 (0)",
                "TO_KEY",
                "JUMP_IF_ABRUPT 1",
                "REF",
                "UNWIND_IF_ABRUPT 1"
            ]),
            true,
            true
        ));
        "oc: oc exp; not-ref success"
    )]
    #[test_case(
        "?.()[1n]", false, &[(Fillable::BigInt, 0)]
        => serr("Out of room for big ints in this compilation unit");
        "oc: oc exp; exp compile fails"
    )]
    #[test_case(
        "?.()[@@@]", false, &[]
        => serr("out of range integral type conversion attempted");
        "oc: oc exp; exp too big"
    )]
    #[test_case(
        "?.a.b", true, &[]
        => Ok((
            svec(&[
                "UNWIND 1",
                "STRING 0 (a)",
                "STRICT_REF",
                "GET_VALUE",
                "JUMP_IF_ABRUPT 3",
                "STRING 1 (b)",
                "STRICT_REF"
            ]),
            true,
            true
        ));
        "oc: oc id; success"
    )]
    #[test_case(
        "?.().x", false, &[]
        => Ok((
            svec(&["FLOAT 0 (0)", "CALL", "JUMP_IF_ABRUPT 3", "STRING 0 (x)", "REF"]),
            true,
            true
        ));
        "oc: oc id; success without ref"
    )]
    #[test_case(
        "?.(1n).x", false, &[(Fillable::BigInt, 0)]
        => serr("Out of room for big ints in this compilation unit");
        "oc: oc id; oc compile fails"
    )]
    #[test_case(
        "?.().x", false, &[(Fillable::String, 0)]
        => serr("Out of room for strings in this compilation unit");
        "oc: oc id; id compile fails"
    )]
    #[test_case("?.a``", false, &[] => panics "not yet implemented"; "oc: oc template")]
    #[test_case("?.a.#b", false, &[] => panics "not yet implemented"; "oc: oc privateid")]
    fn chain_evaluation(
        src: &str,
        strict: bool,
        what: &[(Fillable, usize)],
    ) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).optional_chain();
        let mut c = complex_filled_chunk("x", what);

        node.chain_evaluation(&mut c, strict, src).map_err(|e| e.to_string()).map(|flags| {
            (
                c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                flags.maybe_abrupt(),
                flags.maybe_ref(),
            )
        })
    }
}

mod default_clause {
    use super::*;
    use test_case::test_case;

    #[test_case("default:", true, &[] => Ok((svec(&["EMPTY"]), false, false)); "empty")]
    #[test_case("default:null;", true, &[] => Ok((svec(&["NULL"]), false, false)); "statement")]
    fn compile(src: &str, strict: bool, what: &[(Fillable, usize)]) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).default_clause();
        let mut c = complex_filled_chunk("x", what);

        node.compile(&mut c, strict, src).map_err(|e| e.to_string()).map(|flags| {
            (
                c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                flags.maybe_abrupt(),
                flags.maybe_ref(),
            )
        })
    }
}

mod case_clause {
    use super::*;
    use test_case::test_case;

    #[test_case("case 1:", true, &[] => Ok((svec(&["EMPTY"]), false)); "empty")]
    #[test_case(
        "case 1: doit(); break;", true, &[]
        => Ok((
            svec(&[
                "STRING 0 (doit)",
                "STRICT_RESOLVE",
                "DUP",
                "GET_VALUE",
                "JUMP_IF_NORMAL 4",
                "UNWIND 1",
                "JUMP 3",
                "FLOAT 0 (0)",
                "CALL_STRICT",
                "JUMP_IF_ABRUPT 2",
                "BREAK",
                "UPDATE_EMPTY"
            ]),
            true,
        ));
        "statements"
    )]
    fn compile(src: &str, strict: bool, what: &[(Fillable, usize)]) -> Result<(Vec<String>, bool), String> {
        let node = Maker::new(src).case_clause();
        let mut c = complex_filled_chunk("x", what);

        node.compile(&mut c, strict, src).map_err(|e| e.to_string()).map(|flags| {
            (
                c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                flags.maybe_abrupt(),
            )
        })
    }

    #[test_case("case 1:", true, &[] => Ok((svec(&["DUP", "FLOAT 0 (1)", "SEQ"]), false)); "infallible expr")]
    #[test_case(
        "case a:", true, &[]
        => Ok((
            svec(&[
                "DUP",
                "STRING 0 (a)",
                "STRICT_RESOLVE",
                "GET_VALUE",
                "JUMP_IF_ABRUPT 3",
                "SEQ",
                "JUMP 2",
                "UNWIND 1"
            ]),
            true
        ));
        "fallible ref"
    )]
    #[test_case(
        "case 1n:", true, &[(Fillable::BigInt, 0)]
        => serr("Out of room for big ints in this compilation unit");
        "expr compile fails"
    )]
    fn case_clause_is_selected(
        src: &str,
        strict: bool,
        what: &[(Fillable, usize)],
    ) -> Result<(Vec<String>, bool), String> {
        let node = Maker::new(src).case_clause();
        let mut c = complex_filled_chunk("x", what);

        node.case_clause_is_selected(&mut c, strict, src).map_err(|e| e.to_string()).map(|flags| {
            (
                c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                flags.maybe_abrupt(),
            )
        })
    }
}

mod case_block {
    use super::*;
    use test_case::test_case;

    #[test_case(
        "{default: @@@;}", true, &[]
        => serr("out of range integral type conversion attempted");
        "with default: default clause too large"
    )]
    #[test_case(
        "{case a: break; default: break; case b: @@(1000);}", true, &[]
        => serr("out of range integral type conversion attempted");
        "with default: unwind_2 too far"
    )]
    #[test_case(
        "{default:break;case null:}", false, &[]
        => Ok((
            svec(&[
                "UNDEFINED",
                "SWAP",
                "FALSE",
                "FALSE",
                "SWAP",
                "JUMPPOP_TRUE 14",
                "JUMP_IF_TRUE 6",
                "POP",
                "DUP",
                "NULL",
                "SEQ",
                "JUMP_IF_FALSE 6",
                "ROTATEUP 3",
                "EMPTY",
                "UPDATE_EMPTY",
                "ROTATEDOWN 3",
                "SWAP",
                "POP",
                "JUMPPOP_TRUE 6",
                "BREAK",
                "UPDATE_EMPTY",
                "JUMP_IF_ABRUPT 2",
                "EMPTY",
                "UPDATE_EMPTY"
            ]),
            true
        ));
        "with default; default case fallible"
    )]
    #[test_case(
        "{default:1n;case null:}", false, &[(Fillable::BigInt, 0)]
        => serr("Out of room for big ints in this compilation unit");
        "with default; default clause compile fails"
    )]
    #[test_case(
        "{case 'a':default:case 'b': @@(15);}", false, &[]
        => serr("out of range integral type conversion attempted");
        "with default; big jump over after clauses too large"
    )]
    #[test_case(
        "{default:case null:@@@;}", false, &[]
        => serr("out of range integral type conversion attempted");
        "with default: after clause too large"
    )]
    #[test_case(
        "{default:case null:1n;}", false, &[(Fillable::BigInt, 0)]
        => serr("Out of room for big ints in this compilation unit");
        "with default; after clause compilation fails"
    )]
    #[test_case(
        "{default:case @@@:}", false, &[]
        => serr("out of range integral type conversion attempted");
        "with default; after selector too large"
    )]
    #[test_case(
        "{default:case a:break;}", false, &[]
        => Ok((
            svec(&[
                "UNDEFINED",
                "SWAP",
                "FALSE",
                "FALSE",
                "SWAP",
                "JUMPPOP_TRUE 27",
                "JUMP_IF_TRUE 17",
                "POP",
                "DUP",
                "STRING 0 (a)",
                "RESOLVE",
                "GET_VALUE",
                "JUMP_IF_ABRUPT 3",
                "SEQ",
                "JUMP 2",
                "UNWIND 1",
                "JUMP_IF_ABRUPT 22",
                "JUMP_IF_FALSE 8",
                "ROTATEUP 3",
                "BREAK",
                "UPDATE_EMPTY",
                "JUMP_IF_ABRUPT 14",
                "ROTATEDOWN 3",
                "SWAP",
                "POP",
                "JUMPPOP_TRUE 10",
                "EMPTY",
                "UPDATE_EMPTY",
                "BREAK",
                "UPDATE_EMPTY",
                "JUMP_IF_ABRUPT 4",
                "JUMP 2",
                "UNWIND 2"
            ]),
            true
        ));
        "with default: after selector & block fallible" // 6722
    )]
    #[test_case(
        "{default:case null:}", false, &[]
        => Ok((
            svec(&[
                "UNDEFINED",
                "SWAP",
                "FALSE",
                "FALSE",
                "SWAP",
                "JUMPPOP_TRUE 14",
                "JUMP_IF_TRUE 6",
                "POP",
                "DUP",
                "NULL",
                "SEQ",
                "JUMP_IF_FALSE 6",
                "ROTATEUP 3",
                "EMPTY",
                "UPDATE_EMPTY",
                "ROTATEDOWN 3",
                "SWAP",
                "POP",
                "JUMPPOP_TRUE 4",
                "EMPTY",
                "UPDATE_EMPTY",
                "EMPTY",
                "UPDATE_EMPTY"
            ]),
            false
        ));
        "with default: after clause infallible"
    )]
    #[test_case(
        "{default:case 1n:}", false, &[(Fillable::BigInt, 0)]
        => serr("Out of room for big ints in this compilation unit");
        "with default; after clause selection compile fail" // 6720
    )]
    #[test_case(
        "{default:}", false, &[]
        => Ok((
            svec(&[
                "UNDEFINED",
                "SWAP",
                "FALSE",
                "FALSE",
                "SWAP",
                "POP",
                "SWAP",
                "POP",
                "JUMPPOP_TRUE 2",
                "EMPTY",
                "UPDATE_EMPTY"
            ]),
            false
        ));
        "with default (and nothing else)"
    )]
    #[test_case(
        "{case null:@@@;default:}", false, &[]
        => serr("out of range integral type conversion attempted");
        "with default; before clause too large"
    )]
    #[test_case(
        "{case null:break;default:}", false, &[]
        => Ok((
            svec(&[
                "UNDEFINED",
                "SWAP",
                "FALSE",
                "JUMP_IF_TRUE 6",
                "POP",
                "DUP",
                "NULL",
                "SEQ",
                "JUMP_IF_FALSE 8",
                "ROTATEUP 3",
                "BREAK",
                "UPDATE_EMPTY",
                "JUMP_IF_ABRUPT 13",
                "ROTATEDOWN 3",
                "FALSE",
                "SWAP",
                "POP",
                "SWAP",
                "POP",
                "JUMPPOP_TRUE 6",
                "EMPTY",
                "UPDATE_EMPTY",
                "JUMP 2",
                "UNWIND 2"
            ]),
            true
        ));
        "with default; before clause fallible" // 6707
    )]
    #[test_case(
        "{case null:1n;default:}", false, &[(Fillable::BigInt, 0)]
        => serr("Out of room for big ints in this compilation unit");
        "with default; before clause compilation fails"
    )]
    #[test_case(
        "{case @@@:default:}", false, &[]
        => serr("out of range integral type conversion attempted");
        "with default; before selector too large"
    )]
    #[test_case(
        "{case null:default:}", false, &[]
        => Ok((
            svec(&[
                "UNDEFINED",
                "SWAP",
                "FALSE",
                "JUMP_IF_TRUE 6",
                "POP",
                "DUP",
                "NULL",
                "SEQ",
                "JUMP_IF_FALSE 6",
                "ROTATEUP 3",
                "EMPTY",
                "UPDATE_EMPTY",
                "ROTATEDOWN 3",
                "FALSE",
                "SWAP",
                "POP",
                "SWAP",
                "POP",
                "JUMPPOP_TRUE 2",
                "EMPTY",
                "UPDATE_EMPTY"
            ]),
            false,
        ));
        "with default; infallible selector (before)"
    )]
    #[test_case(
        "{case a:default:}", false, &[]
        => Ok((
            svec(&[
                "UNDEFINED",
                "SWAP",
                "FALSE",
                "JUMP_IF_TRUE 17",
                "POP",
                "DUP",
                "STRING 0 (a)",
                "RESOLVE",
                "GET_VALUE",
                "JUMP_IF_ABRUPT 3",
                "SEQ",
                "JUMP 2",
                "UNWIND 1",
                "JUMP_IF_ABRUPT 19",
                "JUMP_IF_FALSE 6",
                "ROTATEUP 3",
                "EMPTY",
                "UPDATE_EMPTY",
                "ROTATEDOWN 3",
                "FALSE",
                "SWAP",
                "POP",
                "SWAP",
                "POP",
                "JUMPPOP_TRUE 6",
                "EMPTY",
                "UPDATE_EMPTY",
                "JUMP 2",
                "UNWIND 2"
            ]),
            true
        ));
        "with default; selection (before) is ref"
    )]
    #[test_case(
        "{case 1n:;default:;}", false, &[(Fillable::BigInt, 0)]
        => serr("Out of room for big ints in this compilation unit");
        "with default; selection (before) fails"
    )]
    #[test_case(
        "{case 1: break; case 2: @@(18);}", false, &[]
        => serr("out of range integral type conversion attempted");
        "no default; exit jump too far"
    )]
    #[test_case(
        "{case a: @@(12);}", false, &[]
        => serr("out of range integral type conversion attempted");
        "no default; unwind jump too far"
    )]
    #[test_case(
        "{case 'a': @@@;}", false, &[]
        => serr("out of range integral type conversion attempted");
        "no default; case clause too large"
    )]
    #[test_case(
        "{case 'a': 1n;}", false, &[(Fillable::BigInt, 0)]
        => serr("Out of room for big ints in this compilation unit");
        "no default; case clause compile fails"
    )]
    #[test_case(
        "{case @@@:}", false, &[]
        => serr("out of range integral type conversion attempted");
        "no default; case selector too big"
    )]
    #[test_case(
        "{case a:}", true, &[]
        => Ok((
            svec(&[
                "UNDEFINED",
                "SWAP",
                "FALSE",
                "JUMP_IF_TRUE 18",
                "POP",
                "DUP",
                "STRING 0 (a)",
                "STRICT_RESOLVE",
                "GET_VALUE",
                "JUMP_IF_ABRUPT 3",
                "SEQ",
                "JUMP 2",
                "UNWIND 1",
                "JUMP_IF_ABRUPT 12",
                "JUMP_IF_FALSE 5",
                "POP",
                "POP",
                "EMPTY",
                "UPDATE_EMPTY",
                "TRUE",
                "JUMPPOP_TRUE 5",
                "POP",
                "JUMP 2",
                "UNWIND 2"
            ]),
            true
        ));
        "no default, fallible case selection"
    )]
    #[test_case(
        "{case 1n:}", true, &[(Fillable::BigInt, 0)]
        => serr("Out of room for big ints in this compilation unit");
        "no default; clause selection compile fails"
    )]
    #[test_case(
        "{case 0: break; case 1: break; case 2: break;}", true, &[]
        => Ok((
            svec(&[
                "UNDEFINED",
                "SWAP",
                "FALSE",
                "JUMP_IF_TRUE 8",
                "POP",
                "DUP",
                "FLOAT 0 (0)",
                "SEQ",
                "JUMP_IF_FALSE 7",
                "POP",
                "POP",
                "BREAK",
                "UPDATE_EMPTY",
                "JUMP_IF_ABRUPT 36",
                "TRUE",
                "JUMP_IF_TRUE 8",
                "POP",
                "DUP",
                "FLOAT 1 (1)",
                "SEQ",
                "JUMP_IF_FALSE 7",
                "POP",
                "POP",
                "BREAK",
                "UPDATE_EMPTY",
                "JUMP_IF_ABRUPT 20",
                "TRUE",
                "JUMP_IF_TRUE 8",
                "POP",
                "DUP",
                "FLOAT 2 (2)",
                "SEQ",
                "JUMP_IF_FALSE 7",
                "POP",
                "POP",
                "BREAK",
                "UPDATE_EMPTY",
                "JUMP_IF_ABRUPT 4",
                "TRUE",
                "JUMPPOP_TRUE 1",
                "POP"
            ]),
            true,
        ));
        "no-default; potentially abrupt; multiple cases"
    )]
    #[test_case("{}", true, &[] => Ok((svec(&["POP", "UNDEFINED"]), false)); "empty")]
    #[test_case(
        "{case null:}", true, &[]
        => Ok((
            svec(&[
                "UNDEFINED",
                "SWAP",
                "FALSE",
                "JUMP_IF_TRUE 7",
                "POP",
                "DUP",
                "NULL",
                "SEQ",
                "JUMP_IF_FALSE 5",
                "POP",
                "POP",
                "EMPTY",
                "UPDATE_EMPTY",
                "TRUE",
                "JUMPPOP_TRUE 1",
                "POP"
            ]),
            false
        ));
        "no-default; just once"
    )]
    fn case_block_evaluation(
        src: &str,
        strict: bool,
        what: &[(Fillable, usize)],
    ) -> Result<(Vec<String>, bool), String> {
        let node = Maker::new(src).case_block();
        let mut c = complex_filled_chunk("x", what);

        node.case_block_evaluation(&mut c, strict, src).map_err(|e| e.to_string()).map(|flags| {
            (
                c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                flags.maybe_abrupt(),
            )
        })
    }
}

mod method_definition {
    use super::*;
    use test_case::test_case;

    #[test_case(
        "[1n] () {}", true, &[(Fillable::BigInt, 0)]
        => serr("Out of room for big ints in this compilation unit");
        "class element name compile fails"
    )]
    #[test_case(
        "[1n] () {}", true, &[]
        => Ok(svec(&["BIGINT 0 (1)", "TO_KEY", "JUMP_IF_ABRUPT 4", "DEFINE_METHOD 0", "JUMP 2", "UNWIND 2"]));
        "success; fallible name"
    )]
    #[test_case("a(){}", true, &[] => Ok(svec(&["STRING 0 (a)", "DEFINE_METHOD 0"])); "success; infallible name")]
    #[test_case(
        "a(){}", true, &[(Fillable::FunctionStash, 0)]
        => serr("Out of room for more functions!");
        "no room for functions"
    )]
    #[test_case("get a(){}", true, &[] => panics "entered unreachable code"; "not plain method")]
    fn define_method(src: &str, strict: bool, what: &[(Fillable, usize)]) -> Result<Vec<String>, String> {
        let node = Maker::new(src).method_definition();
        let mut c = complex_filled_chunk("x", what);

        node.define_method(&mut c, strict, src, &node)
            .map_err(|e| e.to_string())
            .map(|_| c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>())
    }

    #[test_case(
        "[1n](){}", true, &[(Fillable::BigInt, 0)], true
        => serr("Out of room for big ints in this compilation unit");
        "define_method fails"
    )]
    #[test_case(
        "a(){}", true, &[], false
        => Ok(svec(&[
            "DUP",
            "FUNC_PROTO",
            "SWAP",
            "STRING 0 (a)",
            "DEFINE_METHOD 0",
            "JUMP_IF_ABRUPT 3",
            "SET_FUNC_NAME",
            "DEF_METH_PROP 0",
            "UNWIND_IF_ABRUPT 1"
        ]));
        "success; not enumerable"
    )]
    #[test_case(
        "a(){}", true, &[], true
        => Ok(svec(&[
            "DUP",
            "FUNC_PROTO",
            "SWAP",
            "STRING 0 (a)",
            "DEFINE_METHOD 0",
            "JUMP_IF_ABRUPT 3",
            "SET_FUNC_NAME",
            "DEF_METH_PROP 1",
            "UNWIND_IF_ABRUPT 1"
        ]));
        "success; enumerable"
    )]
    #[test_case(
        "get a(){}", true, &[], true
        => Ok(svec(&["STRING 0 (a)", "DEF_GETTER 0 enumerable"]));
        "getter"
    )]
    #[test_case(
        "get [1n](){}", true, &[(Fillable::BigInt, 0)], true
        => serr("Out of room for big ints in this compilation unit");
        "getter; name compile fails"
    )]
    #[test_case(
        "get [a](){}", true, &[], true
        => Ok(svec(&[
            "STRING 0 (a)",
            "STRICT_RESOLVE",
            "GET_VALUE",
            "JUMP_IF_ABRUPT 1",
            "TO_KEY",
            "JUMP_IF_ABRUPT 5",
            "DEF_GETTER 0 enumerable",
            "JUMP 2",
            "UNWIND 1"
        ]));
        "getter; fallible name"
    )]
    #[test_case(
        "get a(){}", true, &[(Fillable::FunctionStash, 0)], true
        => serr("Out of room for more functions!");
        "getter; function doesn't fit"
    )]
    #[test_case(
        "get a(){}", true, &[], false => Ok(svec(&["STRING 0 (a)", "DEF_GETTER 0 hidden"])); "getter, not enumerable"
    )]
    #[test_case("set a(b){}", true, &[], true => Ok(svec(&["STRING 0 (a)", "DEF_SETTER 0 enumerable"])); "setter")]
    #[test_case("*a(){}", true, &[], true => panics "not yet implemented"; "generator")]
    #[test_case("async a(){}", true, &[], true => panics "not yet implemented"; "async function")]
    #[test_case("async *a(){}", true, &[], true => panics "not yet implemented"; "async generator")]
    fn method_definition_evaluation(
        src: &str,
        strict: bool,
        what: &[(Fillable, usize)],
        enumerable: bool,
    ) -> Result<Vec<String>, String> {
        let node = Maker::new(src).method_definition();
        let mut c = complex_filled_chunk("x", what);

        node.method_definition_evaluation(enumerable, &mut c, strict, src, &node)
            .map_err(|e| e.to_string())
            .map(|_| c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>())
    }
}

mod class_declaration {
    use super::*;
    use test_case::test_case;

    #[test_case("class a {}", true, &[] => panics "not yet implemented"; "todo, still")]
    fn compile(src: &str, strict: bool, what: &[(Fillable, usize)]) -> Result<(Vec<String>, bool), String> {
        let node = Maker::new(src).class_declaration();
        let mut c = complex_filled_chunk("x", what);
        node.compile(&mut c, strict, src)
            .as_ref()
            .map(|flags| {
                (
                    c.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
                    flags.maybe_abrupt(),
                )
            })
            .map_err(ToString::to_string)
    }
}

mod name_loc {
    use super::*;

    #[test]
    #[allow(clippy::clone_on_copy)]
    fn clone() {
        let item = NameLoc::OnStack;
        let copy = item.clone();
        assert!(matches!(copy, NameLoc::OnStack));
    }
}
