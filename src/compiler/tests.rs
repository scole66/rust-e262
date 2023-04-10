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
    #[test_case(Insn::UpdateEmpty => "UPDATE_EMPTY"; "UpdateEmpty instruction")]
    #[test_case(Insn::Undefined => "UNDEFINED"; "Undefined instruction")]
    #[test_case(Insn::Zero => "ZERO"; "Zero instruction")]
    #[test_case(Insn::Empty => "EMPTY"; "Empty instruction")]
    #[test_case(Insn::PutValue => "PUT_VALUE"; "PutValue instruction")]
    #[test_case(Insn::Jump => "JUMP"; "Jump instruction")]
    #[test_case(Insn::JumpIfNormal => "JUMP_IF_NORMAL"; "JumpIfNormal instruction")]
    #[test_case(Insn::Call => "CALL"; "Call instruction")]
    #[test_case(Insn::Swap => "SWAP"; "Swap instruction")]
    #[test_case(Insn::Pop => "POP"; "Pop instruction")]
    #[test_case(Insn::Pop2Push3 => "POP2_PUSH3"; "Pop2Push3 instruction")]
    #[test_case(Insn::Dup => "DUP"; "Dup instruction")]
    #[test_case(Insn::RotateUp => "ROTATEUP"; "RotateUp instruction")]
    #[test_case(Insn::RotateDown => "ROTATEDOWN"; "RotateDown instruction")]
    #[test_case(Insn::Unwind => "UNWIND"; "Unwind instruction")]
    #[test_case(Insn::Ref => "REF"; "Ref instruction")]
    #[test_case(Insn::StrictRef => "STRICT_REF"; "StrictRef instruction")]
    #[test_case(Insn::InitializeReferencedBinding => "IRB"; "InitializeReferencedBinding instruction")]
    #[test_case(Insn::Object => "OBJECT"; "Object instruction")]
    #[test_case(Insn::Array => "ARRAY"; "Array instruction")]
    #[test_case(Insn::CreateDataProperty => "CR_PROP"; "CreateDataProperty instruction")]
    #[test_case(Insn::SetPrototype => "SET_PROTO"; "SetPrototype instruction")]
    #[test_case(Insn::ToPropertyKey => "TO_KEY"; "ToPropertyKey instruction")]
    #[test_case(Insn::CopyDataProps => "COPY_DATA_PROPS"; "CopyDataProps instruction")]
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
    #[test_case(Insn::RequireConstructor => "REQ_CSTR"; "RequireConstructor instruction")]
    #[test_case(Insn::Construct => "CONSTRUCT"; "Construct instruction")]
    #[test_case(Insn::JumpNotThrow => "JUMP_NOT_THROW"; "JumpNotThrow instruction")]
    #[test_case(Insn::IteratorAccumulate => "ITERATOR_ACCUM"; "IteratorAccumulate instruction")]
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
}

mod always_abrupt_ref_result {
    use super::*;

    #[test]
    fn debug() {
        assert_ne!(format!("{:?}", AlwaysAbruptRefResult {}), "");
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
}

mod nameable_production {
    use super::*;
    use test_case::test_case;

    #[test]
    fn debug() {
        let fd = Maker::new("function (){}").function_expression();
        let x = NameableProduction::Function(fd);
        assert_ne!(format!("{:?}", x), "");
    }

    #[test_case(NameableProduction::Function(Maker::new("function(){}").function_expression()) => "function (  ) {  }"; "Function")]
    #[test_case(NameableProduction::Generator(Maker::new("function*(){}").generator_expression()) => "function * (  ) {  }"; "Generator")]
    #[test_case(NameableProduction::AsyncFunction(Maker::new("async function(){}").async_function_expression()) => "async function (  ) {  }"; "AsyncFunction")]
    #[test_case(NameableProduction::AsyncGenerator(Maker::new("async function*(){}").async_generator_expression()) => "async function * (  ) {  }"; "AsyncGenerator")]
    #[test_case(NameableProduction::Class(Maker::new("class{}").class_expression()) => "class { }"; "Class")]
    #[test_case(NameableProduction::Arrow(Maker::new("x=>x").arrow_function()) => "x => x"; "Arrow")]
    #[test_case(NameableProduction::AsyncArrow(Maker::new("async x=>x").async_arrow_function()) => "async x => x"; "AsyncArrow")]
    fn display(node: NameableProduction) -> String {
        node.to_string()
    }

    #[test_case(Maker::new("=3").initializer() => serr("Production not nameable"); "Initializer, not nameable")]
    #[test_case(Maker::new("=function(){}").initializer() => sok("function (  ) {  }"); "Initializer: nameable")]
    #[test_case(Maker::new("3").assignment_expression() => serr("Production not nameable"); "AssignmentExpression::FallThru not namable")]
    #[test_case(Maker::new("function (){}").assignment_expression() => sok("function (  ) {  }"); "AssignmentExpression::FallThru nameable")]
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
    #[test_case(Maker::new("function (){}").conditional_expression() => sok("function (  ) {  }"); "ConditionalExpression::FallThru nameable")]
    #[test_case(Maker::new("function () {} ? a => a : a => 2*a").conditional_expression() => serr("Production not nameable"); "ConditionalExpression::Conditional")]
    #[test_case(Maker::new("3").short_circuit_expression() => serr("Production not nameable"); "ShortCircuitExpression::FallThru not namable")]
    #[test_case(Maker::new("function (){}").short_circuit_expression() => sok("function (  ) {  }"); "ShortCircuitExpression::FallThru nameable")]
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
    #[test_case(Maker::new("function(){}").expression() => sok("function (  ) {  }"); "expression fallthru")]
    #[test_case(Maker::new("function(){}, 10").expression() => serr("Production not nameable"); "comma expression")]
    #[test_case(Maker::new("(function(){})").parenthesized_expression() => sok("function (  ) {  }"); "parenthesized")]
    #[test_case(Maker::new("new function(){}").new_expression() => serr("Production not nameable"); "new expr")]
    #[test_case(Maker::new("function() {} ++").update_expression() => serr("Production not nameable"); "post-increment")]
    #[test_case(Maker::new("-function() {}").unary_expression() => serr("Production not nameable"); "negate")]
    #[test_case(Maker::new("function(){}()").left_hand_side_expression() => serr("Production not nameable"); "call expression")]
    #[test_case(Maker::new("function(){}.name").member_expression() => serr("Production not nameable"); "member expression")]
    #[test_case(Maker::new("function(){}").primary_expression() => sok("function (  ) {  }"); "primary function expr")]
    #[test_case(Maker::new("class {}").primary_expression() => sok("class { }"); "primary class expr")]
    #[test_case(Maker::new("function *(){}").primary_expression() => sok("function * (  ) {  }"); "primary gen expr")]
    #[test_case(Maker::new("async function (){}").primary_expression() => sok("async function (  ) {  }"); "primary async fun expr")]
    #[test_case(Maker::new("async function *(){}").primary_expression() => sok("async function * (  ) {  }"); "primary async gen expr")]
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
        }
    }
}

// A note about compile tests: These are really unit tests; they check that all the code paths are run, and that the
// opcodes output match what we wanted the opcodes to be. They do not test whether we chose the right opcodes to
// complete the requested task! Checking that really feels like a later stage integration task. (Both the compiler and
// the runtime executor need to be functioning. That's all probably based more or less on test-262.)

fn full_chunk(n: &str) -> Chunk {
    let mut c = Chunk::new(n);
    c.floats = vec![56878142.0; 65536];
    c.strings = Vec::with_capacity(65536);
    c.bigints = Vec::with_capacity(65536);
    for nbr in 0..65536 {
        c.strings.push(JSString::from(""));
        c.bigints.push(Rc::new(BigInt::from(nbr)));
    }
    c
}

fn almost_full_chunk(n: &str, slots_left: usize) -> Chunk {
    let mut c = Chunk::new(n);
    const LIMIT: usize = 65536;
    c.floats.resize(LIMIT - slots_left.min(LIMIT), 7489305.0);
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
    let mut c = Chunk::new(name);
    const LIMIT: usize = 65536;
    for &(section, slots_left) in what {
        match section {
            Fillable::Float => c.floats.resize(LIMIT - slots_left.min(LIMIT), 7489305.0),
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
            c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
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
        #[test_case("``", true => panics "not yet implemented"; "template literal")]
        #[test_case("function a(){}", true => svec(&["STRING 0 (a)", "FUNC_IOFE 0"]); "function expression")]
        #[test_case("function *(){}", true => panics "not yet implemented"; "generator expression")]
        #[test_case("async function (){}", true => panics "not yet implemented"; "async function expression")]
        #[test_case("async function *(){}", true => panics "not yet implemented"; "async generator expression")]
        #[test_case("/abcd/", true => panics "not yet implemented"; "regular expression")]
        fn normal(src: &str, strict: bool) -> Vec<String> {
            let node = Maker::new(src).primary_expression();
            let mut c = Chunk::new("pe");
            node.compile(&mut c, strict, src).unwrap();
            c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
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
            c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
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
            assert!(c.add_to_string_pool("test".into()).is_err())
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
        c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
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
        c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
    #[test_case("a(){}", true, &[] => panics "not yet implemented"; "method def")]
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
        "JUMP_IF_ABRUPT 18",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 8",
        "TO_KEY",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 1",
        "STRICT_REF"
    ]), true, true)); "strict member exp")]
    #[test_case("a[b]", false, None => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 18",
        "STRING 1 (b)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 8",
        "TO_KEY",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 1",
        "REF"
    ]), true, true)); "non-strict member exp")]
    #[test_case("a[b]", true, Some(0) => serr("Out of room for strings in this compilation unit"); "no space for base (expression)")]
    #[test_case("(1)[1]", true, None => Ok((svec(&["FLOAT 0 (1)", "FLOAT 0 (1)", "TO_KEY", "JUMP_IF_NORMAL 4", "UNWIND 1", "JUMP 1", "STRICT_REF"]), true, true)); "no errors in base (expression)")]
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
        c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
    }
}

mod call_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("a()", true, None => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "DUP", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 1", "JUMP 3", "FLOAT 0 (0)", "CALL"]), true, false)); "call-expression; strict")]
    #[test_case("a()", false, None => Ok((svec(&["STRING 0 (a)", "RESOLVE", "DUP", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 1", "JUMP 3", "FLOAT 0 (0)", "CALL"]), true, false)); "call-expression; non-strict")]
    #[test_case("super()", true, None => panics "not yet implemented"; "super call")]
    #[test_case("import(a)", true, None => panics "not yet implemented"; "import call")]
    #[test_case("a()()", true, None => panics "not yet implemented"; "call-on-call")]
    #[test_case("a()[b]", true, None => panics "not yet implemented"; "expr-on-call")]
    #[test_case("a().b", true, None => panics "not yet implemented"; "property-on-call")]
    #[test_case("a()`${b}`", true, None => panics "not yet implemented"; "template-on-call")]
    #[test_case("a().#pid", true, None => panics "not yet implemented"; "private-on-call")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).call_expression();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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

    #[test_case("a()", true, None => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "DUP", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 1", "JUMP 3", "FLOAT 0 (0)", "CALL"]), true, false)); "no args; strict")]
    #[test_case("a()", false, None => Ok((svec(&["STRING 0 (a)", "RESOLVE", "DUP", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 1", "JUMP 3", "FLOAT 0 (0)", "CALL"]), true, false)); "no args; non-strict")]
    #[test_case("a()", true, Some(0) => serr("Out of room for strings in this compilation unit"); "no space for ME")]
    #[test_case("(1)()", true, None => Ok((svec(&["FLOAT 0 (1)", "DUP", "FLOAT 1 (0)", "CALL"]), true, false)); "me not reference")]
    #[test_case("a(b)", true, Some(1) => serr("Out of room for strings in this compilation unit"); "no space for args")]
    #[test_case("a(c)", true, None => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "DUP", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 1", "JUMP 15", "STRING 1 (c)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 2", "FLOAT 0 (1)", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "CALL"]), true, false)); "args can error; strict")]
    #[test_case("a(c)", false, None => Ok((svec(&["STRING 0 (a)", "RESOLVE", "DUP", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 1", "JUMP 15", "STRING 1 (c)", "RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 2", "FLOAT 0 (1)", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "CALL"]), true, false)); "args can error; non-strict")]
    #[test_case("a(@@@)", true, None => serr("out of range integral type conversion attempted"); "bad jump (args too complex)")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).call_member_expression();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
        "CALL"
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
    #[test_case("a?.b", true => panics "not yet implemented"; "optional")]
    fn compile(src: &str, strict: bool) -> Vec<String> {
        let node = Maker::new(src).left_hand_side_expression();
        let mut c = Chunk::new("x");
        node.compile(&mut c, strict, src).unwrap();
        c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
    }
}

mod arguments {
    use super::*;
    use test_case::test_case;

    #[test_case("()", true, None => Ok((svec(&["FLOAT 0 (0)"]), false, false)); "empty")]
    #[test_case("()", true, Some(0) => serr("Out of room for floats in this compilation unit"); "empty; no space for args")]
    #[test_case("(1)", true, None => Ok((svec(&["FLOAT 0 (1)", "FLOAT 0 (1)"]), false, false)); "one arg; no errors")]
    #[test_case("(a)", true, Some(0) => serr("Out of room for strings in this compilation unit"); "no room for args")]
    #[test_case("(999)", true, Some(1) => serr("Out of room for floats in this compilation unit"); "no room for length")]
    #[test_case("(a,)", true, None => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 2", "FLOAT 0 (1)"]), true, false)); "error-able args; strict")]
    #[test_case("(a,)", false, None => Ok((svec(&["STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 2", "FLOAT 0 (1)"]), true, false)); "error-able args; non-strict")]
    fn argument_list_evaluation(
        src: &str,
        strict: bool,
        spots_avail: Option<usize>,
    ) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).arguments();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.argument_list_evaluation(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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

    #[test_case("a", true, None => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE"]), 1, true, false)); "item/reference/strict")]
    #[test_case("a", false, None => Ok((svec(&["STRING 0 (a)", "RESOLVE", "GET_VALUE"]), 1, true, false)); "item/reference/non-strict")]
    #[test_case("true", true, None => Ok((svec(&["TRUE"]), 1, false, false)); "item/literal")]
    #[test_case("a", true, Some(0) => serr("Out of room for strings in this compilation unit"); "no room for item")]
    #[test_case("...a", true, None => panics "not yet implemented"; "...a -style object unpacking")]
    #[test_case("true, false", true, None => Ok((svec(&["TRUE", "FALSE"]), 2, false, false)); "list/noref")]
    #[test_case("a,b", true, None => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 8", "STRING 1 (b)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 2", "UNWIND 1"]), 2, true, false)); "errable items, strict")]
    #[test_case("a,b", false, None => Ok((svec(&["STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 8", "STRING 1 (b)", "RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 2", "UNWIND 1"]), 2, true, false)); "errable items, non-strict")]
    #[test_case("a,b", true, Some(0) => serr("Out of room for strings in this compilation unit"); "no room for first list")]
    #[test_case("a,b", true, Some(1) => serr("Out of room for strings in this compilation unit"); "no room for last item")]
    #[test_case("a,@@@", true, None => serr("out of range integral type conversion attempted"); "jump too far")]
    #[test_case("a,...b", true, None => panics "not yet implemented"; "list + rest")]
    fn argument_list_evaluation(
        src: &str,
        strict: bool,
        spots_avail: Option<usize>,
    ) -> Result<(Vec<String>, u16, bool, bool), String> {
        let node = Maker::new(src).argument_list();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.argument_list_evaluation(&mut c, strict, src)
            .map(|(count, status)| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
                    count,
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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

    #[test_case("id", true => svec(&["STRING 0 (id)", "STRICT_RESOLVE"]); "fall-thru strict")]
    #[test_case("id", false => svec(&["STRING 0 (id)", "RESOLVE"]); "fall-thru non strict")]
    #[test_case("a**8", true => svec(&["STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 3", "FLOAT 0 (8)", "EXPONENTIATE"]); "exponent expr; non-strict")]
    fn compile(src: &str, strict: bool) -> Vec<String> {
        let node = Maker::new(src).exponentiation_expression();
        let mut c = Chunk::new("x");
        node.compile(&mut c, strict, src).unwrap();
        c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
    }
}

mod multiplicative_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true => svec(&["STRING 0 (id)", "STRICT_RESOLVE"]); "fall-thru strict")]
    #[test_case("id", false => svec(&["STRING 0 (id)", "RESOLVE"]); "fall-thru non strict")]
    #[test_case("a*8", true => svec(&["STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 3", "FLOAT 0 (8)", "MULTIPLY"]); "multiply expr; strict")]
    #[test_case("a/8", true => svec(&["STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 3", "FLOAT 0 (8)", "DIVIDE"]); "divide expr; strict")]
    #[test_case("a%8", true => svec(&["STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 3", "FLOAT 0 (8)", "MODULO"]); "modulo expr; strict")]
    fn compile(src: &str, strict: bool) -> Vec<String> {
        let node = Maker::new(src).multiplicative_expression();
        let mut c = Chunk::new("x");
        node.compile(&mut c, strict, src).unwrap();
        c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
    }
}

mod additive_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true => svec(&["STRING 0 (id)", "STRICT_RESOLVE"]); "fall-thru strict")]
    #[test_case("id", false => svec(&["STRING 0 (id)", "RESOLVE"]); "fall-thru non strict")]
    #[test_case("a+3", true => svec(&["STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 3", "FLOAT 0 (3)", "ADD"]); "add expr")]
    #[test_case("a-3", true => svec(&["STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 3", "FLOAT 0 (3)", "SUBTRACT"]); "sub expr")]
    fn compile(src: &str, strict: bool) -> Vec<String> {
        let node = Maker::new(src).additive_expression();
        let mut c = Chunk::new("x");
        node.compile(&mut c, strict, src).unwrap();
        c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
    }
}

mod shift_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true, None => Ok((svec(&["STRING 0 (id)", "STRICT_RESOLVE"]), true, true)); "fall-thru strict")]
    #[test_case("id", false, None => Ok((svec(&["STRING 0 (id)", "RESOLVE"]), true, true)); "fall-thru non strict")]
    #[test_case("a<<2", true, None => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "FLOAT 0 (2)",
        "LSH"
    ]), true, false)); "left shift, strict")]
    #[test_case("a<<2", false, None => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "FLOAT 0 (2)",
        "LSH"
    ]), true, false)); "left shift, non-strict")]
    #[test_case("a>>2", true, None => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "FLOAT 0 (2)",
        "SRSH"
    ]), true, false)); "signed right shift, strict")]
    #[test_case("a>>2", false, None => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "FLOAT 0 (2)",
        "SRSH"
    ]), true, false)); "signed right shift, non-strict")]
    #[test_case("a>>>2", true, None => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "FLOAT 0 (2)",
        "URSH"
    ]), true, false)); "unsigned right shift, strict")]
    #[test_case("a>>>2", false, None => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "FLOAT 0 (2)",
        "URSH"
    ]), true, false)); "unsigned right shift, non-strict")]
    #[test_case("2>>4", true, None => Ok((svec(&[
        "FLOAT 0 (2)",
        "FLOAT 1 (4)",
        "SRSH"
    ]), true, false)); "nothing that can throw")]
    #[test_case("2>>a", true, None => Ok((svec(&[
        "FLOAT 0 (2)",
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 1",
        "SRSH"
    ]), true, false)); "can throw from right")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).shift_expression();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod relational_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true, None => Ok((svec(&["STRING 0 (id)", "STRICT_RESOLVE"]), true, true)); "fall-thru strict")]
    #[test_case("id", false, None => Ok((svec(&["STRING 0 (id)", "RESOLVE"]), true, true)); "fall-thru non strict")]
    #[test_case("a<10", true, None => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 3", "FLOAT 0 (10)", "LT"]), true, false)); "less than/strict")]
    #[test_case("a>10", true, None => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 3", "FLOAT 0 (10)", "GT"]), true, false)); "greater than/strict")]
    #[test_case("a<=10", true, None => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 3", "FLOAT 0 (10)", "LE"]), true, false)); "less equal/strict")]
    #[test_case("a>=10", true, None => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 3", "FLOAT 0 (10)", "GE"]), true, false)); "greater equal/strict")]
    #[test_case("a in b", true, None => Ok((svec(&[
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
    #[test_case("a instanceof c", true, None => Ok((svec(&[
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
    #[test_case("#blue in gray", true, None => panics "not yet implemented"; "privateid in")]
    #[test_case("a<10", false, None => Ok((svec(&["STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 3", "FLOAT 0 (10)", "LT"]), true, false)); "less than/non-strict")]
    #[test_case("a>10", false, None => Ok((svec(&["STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 3", "FLOAT 0 (10)", "GT"]), true, false)); "greater than/non-strict")]
    #[test_case("a<=10", false, None => Ok((svec(&["STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 3", "FLOAT 0 (10)", "LE"]), true, false)); "less equal/non-strict")]
    #[test_case("a>=10", false, None => Ok((svec(&["STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 3", "FLOAT 0 (10)", "GE"]), true, false)); "greater equal/non-strict")]
    #[test_case("a in b", false, None => Ok((svec(&[
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
    #[test_case("a instanceof c", false, None => Ok((svec(&[
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
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).relational_expression();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod equality_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true, None => Ok((svec(&["STRING 0 (id)", "STRICT_RESOLVE"]), true, true)); "fall-thru strict")]
    #[test_case("id", false, None => Ok((svec(&["STRING 0 (id)", "RESOLVE"]), true, true)); "fall-thru non strict")]
    #[test_case("a==43", true, None => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "FLOAT 0 (43)",
        "EQ"
    ]), true, false)); "loosely eq/strict")]
    #[test_case("a===43", true, None => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "FLOAT 0 (43)",
        "SEQ"
    ]), true, false)); "strict eq/strict")]
    #[test_case("a!=43", true, None => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "FLOAT 0 (43)",
        "NE"
    ]), true, false)); "loosely not eq/strict")]
    #[test_case("a!==43", true, None => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "FLOAT 0 (43)",
        "SNE"
    ]), true, false)); "strict not eq/strict")]
    #[test_case("a==43", false, None => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "FLOAT 0 (43)",
        "EQ"
    ]), true, false)); "loosely eq/non-strict")]
    #[test_case("a===43", false, None => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "FLOAT 0 (43)",
        "SEQ"
    ]), true, false)); "strict eq/non-strict")]
    #[test_case("a!=43", false, None => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "FLOAT 0 (43)",
        "NE"
    ]), true, false)); "loosely not eq/non-strict")]
    #[test_case("a!==43", false, None => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "FLOAT 0 (43)",
        "SNE"
    ]), true, false)); "strict not eq/non-strict")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).equality_expression();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod bitwise_and_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true, None => Ok((svec(&["STRING 0 (id)", "STRICT_RESOLVE"]), true, true)); "fall-thru strict")]
    #[test_case("id", false, None => Ok((svec(&["STRING 0 (id)", "RESOLVE"]), true, true)); "fall-thru non strict")]
    #[test_case("a&43", true, None => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "FLOAT 0 (43)",
        "AND"
    ]), true, false)); "bit and expr/strict")]
    #[test_case("a&43", false, None => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "FLOAT 0 (43)",
        "AND"
    ]), true, false)); "bit and expr/non-strict")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).bitwise_and_expression();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod bitwise_xor_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true, None => Ok((svec(&["STRING 0 (id)", "STRICT_RESOLVE"]), true, true)); "fall-thru strict")]
    #[test_case("id", false, None => Ok((svec(&["STRING 0 (id)", "RESOLVE"]), true, true)); "fall-thru non strict")]
    #[test_case("a^43", true, None => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "FLOAT 0 (43)",
        "XOR"
    ]), true, false)); "bit xor expr/strict")]
    #[test_case("a^43", false, None => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "FLOAT 0 (43)",
        "XOR"
    ]), true, false)); "bit xor expr/non-strict")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).bitwise_xor_expression();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod bitwise_or_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true, None => Ok((svec(&["STRING 0 (id)", "STRICT_RESOLVE"]), true, true)); "fall-thru strict")]
    #[test_case("id", false, None => Ok((svec(&["STRING 0 (id)", "RESOLVE"]), true, true)); "fall-thru non strict")]
    #[test_case("a|43", true, None => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "FLOAT 0 (43)",
        "OR"
    ]), true, false)); "bit or expr/strict")]
    #[test_case("a|43", false, None => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "FLOAT 0 (43)",
        "OR"
    ]), true, false)); "bit or expr/non-strict")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).bitwise_or_expression();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
        "JUMP_IF_ABRUPT 10",
        "FLOAT 0 (1)",
        "TO_KEY",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 1",
        "REF",
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
    #[test_case("a &&= b", true, &[] => panics "not yet implemented"; "logical and assignment")]
    #[test_case("a ||= b", true, &[] => panics "not yet implemented"; "logical or assignment")]
    #[test_case("c ??= b", true, &[] => panics "not yet implemented"; "coalesce assignment")]
    #[test_case("{a} = b", true, &[] => panics "not yet implemented"; "destructuring assignment")]
    fn compile(src: &str, strict: bool, slots_left: &[(Fillable, usize)]) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).assignment_expression();
        let mut c = complex_filled_chunk("x", slots_left);
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
            c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
        c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
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
        c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
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
        let label_set = labels.iter().cloned().map(JSString::from).collect::<Vec<JSString>>();
        node.labelled_compile(&mut c, strict, src, &label_set)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
        c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
    #[test_case("{a}=b", true, &[] => panics "not yet implemented"; "pattern binding")]
    fn compile(src: &str, strict: bool, what: &[(Fillable, usize)]) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).lexical_binding();
        let mut c = complex_filled_chunk("x", what);
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
    #[test_case("[a]=b", true, &[] => panics "not yet implemented"; "pattern assignment")]
    fn compile(src: &str, strict: bool, what: &[(Fillable, usize)]) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).variable_declaration();
        let mut c = complex_filled_chunk("x", what);
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
        node.compile(&mut c, src).unwrap();
        c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
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
        node.compile(&mut c, src).unwrap();
        c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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

    #[test_case(";", None => (svec(&["EMPTY"]), false, false); "typical")]
    fn compile(src: &str, spots_avail: Option<usize>) -> (Vec<String>, bool, bool) {
        let node = Maker::new(src).empty_statement();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        let status = node.compile(&mut c);
        (
            c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
    #[test_case("switch (a) { case 1: break; }", &["alpha"], true, None => panics "not yet implemented"; "switch/strict")]
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
        let label_set = labels.iter().cloned().map(JSString::from).collect::<Vec<JSString>>();
        node.labelled_compile(&mut c, strict, src, &label_set)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
    #[test_case("for(a in b);", &[], true, None => panics "not yet implemented"; "for-in stmt")]
    fn loop_compile(
        src: &str,
        labels: &[&str],
        strict: bool,
        spots_avail: Option<usize>,
    ) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).iteration_statement();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        let label_set = labels.iter().cloned().map(JSString::from).collect::<Vec<JSString>>();
        node.loop_compile(&mut c, strict, src, &label_set)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
        let label_set = labels.iter().cloned().map(JSString::from).collect::<Vec<JSString>>();
        node.do_while_loop_compile(&mut c, strict, src, &label_set)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
        let label_set = labels.iter().cloned().map(JSString::from).collect::<Vec<JSString>>();
        node.while_loop_compile(&mut c, strict, src, &label_set)
            .map(|status| {
                (c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(), status.maybe_abrupt())
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
    #[test_case("for(a;;);", &[], false, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "for, init compile fail")]
    #[test_case("for(a;;);", &[], false, &[] => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 6",
        "POP",
        "UNDEFINED",
        "EMPTY",
        "COALESCE",
        "JUMP -4"
    ]), true)); "for, init fallible")]
    #[test_case("for(;;)a;", &[], false, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "for, stmt compile fail")]
    #[test_case("for(;;)break;", &[], false, &[] => Ok((svec(&[
        "UNDEFINED",
        "BREAK",
        "LOOP_CONT []",
        "JUMPPOP_FALSE 3",
        "COALESCE",
        "JUMP -8",
        "UPDATE_EMPTY"
    ]), true)); "for, stmt fallible")]
    #[test_case("for(a;;)@@4;", &[], false, &[] => serr("out of range integral type conversion attempted"); "for, abort from init too long")]
    #[test_case("for(var a;;);", &[], false, &[] => Ok((svec(&[
        "EMPTY",
        "POP",
        "UNDEFINED",
        "EMPTY",
        "COALESCE",
        "JUMP -4"
    ]), false)); "for-var")]
    fn compile(
        src: &str,
        labels: &[&str],
        strict: bool,
        what: &[(Fillable, usize)],
    ) -> Result<(Vec<String>, bool), String> {
        let node = Maker::new(src).for_statement();
        let mut c = complex_filled_chunk("x", what);
        let label_set = labels.iter().cloned().map(JSString::from).collect::<Vec<JSString>>();
        node.compile_for_loop(&mut c, strict, src, &label_set)
            .map(|status| {
                (c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(), status.maybe_abrupt())
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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

    #[test_case("switch (expr) {}", true, None => panics "not yet implemented"; "typical")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).switch_statement();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict, src)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod function_declaration {
    use super::*;
    use test_case::test_case;

    #[test_case("function x(){}", None => Ok((svec(&["EMPTY"]), false, false)); "typical")]
    fn compile(src: &str, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).function_declaration();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }

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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
        let label_set = labels.iter().cloned().map(JSString::from).collect::<Vec<JSString>>();
        node.labelled_compile(&mut c, strict, src, &label_set)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
        let label_set = labels.iter().cloned().map(JSString::from).collect::<Vec<JSString>>();
        node.labelled_compile(&mut c, strict, src, &label_set)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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

    #[test_case("alpha", true, true, None => Ok((svec(&["STRING 0 (alpha)", "STRICT_RESOLVE", "SWAP", "PUT_VALUE", "POP"]), false, false)); "strict/has_dupes/normal")]
    #[test_case("alpha", false, true, None => Ok((svec(&["STRING 0 (alpha)", "RESOLVE", "SWAP", "PUT_VALUE", "POP"]), false, false)); "non-strict/has_dupes/normal")]
    #[test_case("alpha", true, false, None => Ok((svec(&["ILB 0 (alpha)"]), false, false)); "strict/no_dupes/normal")]
    #[test_case("alpha", false, false, None => Ok((svec(&["ILB 0 (alpha)"]), false, false)); "non-strict/no_dupes/normal")]
    #[test_case("yield", true, false, None => Ok((svec(&["ILB 0 (yield)"]), false, false)); "strict/no_dupes/yield")]
    #[test_case("await", true, false, None => Ok((svec(&["ILB 0 (await)"]), false, false)); "strict/no_dupes/await")]
    #[test_case("alpha", true, false, Some(0) => serr("Out of room for strings in this compilation unit"); "no space left")]
    fn compile_binding_initialization(
        src: &str,
        strict: bool,
        has_dupes: bool,
        spots_avail: Option<usize>,
    ) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).yield_ok(false).await_ok(false).binding_identifier();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile_binding_initialization(&mut c, strict, has_dupes)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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

    #[test_case("alpha", true, false, None => Ok((svec(&[
        "EXTRACT_ARG", "STRING 0 (alpha)", "STRICT_RESOLVE", "SWAP", "IRB", "POP"
    ]), false, false)); "single name/strict")]
    #[test_case("{alpha}", true, false, None => Ok((svec(&["EXTRACT_ARG", "TODO"]), true, false)); "no-init pattern")]
    #[test_case("{alpha}=beta", true, false, None => Ok((svec(&["EXTRACT_ARG", "JUMP_NOT_UNDEF 10", "POP", "STRING 0 (beta)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 3", "UNWIND_LIST", "JUMP 1", "TODO"]), true, false)); "init pattern")]
    #[test_case("{alpha}=3", true, false, None => Ok((svec(&["EXTRACT_ARG", "JUMP_NOT_UNDEF 3", "POP", "FLOAT 0 (3)", "TODO"]), true, false)); "init by errorfree")]
    #[test_case("{alpha}=beta", false, false, Some(0) => serr("Out of room for strings in this compilation unit"); "no room")]
    #[test_case("{alhpa}=@@@", false, false, None => serr("out of range integral type conversion attempted"); "initializer too large")]
    #[test_case("{alpha}=xxx", false, false, Some(1) => serr("Out of room for strings in this compilation unit"); "almost no room")]
    #[test_case("{alpha}=a", false, true, None => serr("out of range integral type conversion attempted"); "pattern too complex")]
    fn compile_binding_initialization(
        src: &str,
        strict: bool,
        has_dupes: bool,
        spots_avail: Option<usize>,
    ) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).binding_element();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile_binding_initialization(&mut c, strict, src, has_dupes)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod binding_pattern {
    use super::*;
    use test_case::test_case;

    #[test_case("{a}", true, false, None => Ok((svec(&["TODO"]), true, false)); "simple")]
    #[test_case("{a}", true, false, Some(0) => serr("Out of room for strings in this compilation unit"); "no space")]
    fn compile_binding_initialization(
        src: &str,
        strict: bool,
        has_dupes: bool,
        spots_avail: Option<usize>,
    ) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).binding_pattern();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile_binding_initialization(&mut c, strict, src, has_dupes)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }

    #[test]
    fn compile_binding_initialization_coverage_special() {
        let node = Maker::new("{a}").binding_pattern();
        let mut c = Chunk::new("name");
        node.compile_binding_initialization(&mut c, true, "{a}", true).unwrap();
        assert_eq!(c.opcodes[0], Insn::ToDo as u16);
        assert!(c.opcodes.len() >= 32768);
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 3",
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 3",
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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

    enum Kind {
        Formal,
        Arrow,
        AsyncArrowBinding,
        ArrowFormals,
    }

    #[test_case("a", Kind::Formal, true, true, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "PUT_VALUE", "POP"]), false, false)); "strict/dups/formal")]
    #[test_case("a", Kind::Formal, false, true, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "PUT_VALUE", "POP"]), false, false)); "non-strict/dups/formal")]
    #[test_case("a", Kind::Formal, true, false, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "IRB", "POP"]), false, false)); "strict/no-dups/formal")]
    #[test_case("a", Kind::Formal, false, false, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "IRB", "POP"]), false, false)); "non-strict/no-dups/formal")]
    #[test_case("a", Kind::Arrow, true, true, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "PUT_VALUE", "POP"]), false, false)); "strict/dups/arrow")]
    #[test_case("a", Kind::Arrow, false, true, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "PUT_VALUE", "POP"]), false, false)); "non-strict/dups/arrow")]
    #[test_case("a", Kind::Arrow, true, false, &[] => Ok((svec(&["EXTRACT_ARG", "ILB 0 (a)"]), false, false)); "strict/no-dups/arrow")]
    #[test_case("a", Kind::Arrow, false, false, &[] => Ok((svec(&["EXTRACT_ARG", "ILB 0 (a)"]), false, false)); "non-strict/no-dups/arrow")]
    #[test_case("a", Kind::AsyncArrowBinding, false, false, &[] => panics "not yet implemented"; "async arrow binding")]
    #[test_case("(a)", Kind::ArrowFormals, false, false, &[] => panics "not yet implemented"; "arrow formals")]
    fn compile_binding_initialization(
        src: &str,
        which: Kind,
        strict: bool,
        has_dups: bool,
        what: &[(Fillable, usize)],
    ) -> Result<(Vec<String>, bool, bool), String> {
        let node = match which {
            Kind::Formal => ParamSource::FormalParameters(Maker::new(src).formal_parameters()),
            Kind::Arrow => ParamSource::ArrowParameters(Maker::new(src).arrow_parameters()),
            Kind::AsyncArrowBinding => ParamSource::AsyncArrowBinding(Maker::new(src).async_arrow_binding_identifier()),
            Kind::ArrowFormals => ParamSource::ArrowFormals(Maker::new(src).arrow_formal_parameters()),
        };
        let mut c = complex_filled_chunk("x", what);
        node.compile_binding_initialization(&mut c, strict, src, has_dups)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod formal_parameters {
    use super::*;
    use test_case::test_case;

    #[test_case("a", true, false, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "IRB", "POP"]), false, false)); "simple/strict/no-dup")]
    #[test_case("a", true, true, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "PUT_VALUE", "POP"]), false, false)); "simple/strict/dup")]
    #[test_case("a", false, false, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "IRB", "POP"]), false, false)); "simple/non-strict/no-dup")]
    #[test_case("a", false, true, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "PUT_VALUE", "POP"]), false, false)); "simple/non-strict/dup")]
    #[test_case("", false, false, &[] => Ok((svec(&[]), false, false)); "empty/non-strict/no-dup")]
    #[test_case("...a", true, false, &[] => panics "not yet implemented"; "rest/strict/no-dup")]
    #[test_case("a,", true, false, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "IRB", "POP"]), false, false)); "comma/strict/no-dup")]
    #[test_case("a,...b", true, false, &[] => panics "not yet implemented"; "list+rest/strict/no-dup")]
    #[test_case("a,...b", true, false, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "list+rest/string table full")]
    fn compile_binding_initialization(
        src: &str,
        strict: bool,
        has_dups: bool,
        what: &[(Fillable, usize)],
    ) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).formal_parameters();
        let mut c = complex_filled_chunk("x", what);
        node.compile_binding_initialization(&mut c, strict, src, has_dups)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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

    #[test_case("a", true, true, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "PUT_VALUE", "POP"]), false, false)); "id/strict/dups")]
    #[test_case("a", true, false, &[] => Ok((svec(&["EXTRACT_ARG", "ILB 0 (a)"]), false, false)); "id/strict/no-dups")]
    #[test_case("a", false, true, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "PUT_VALUE", "POP"]), false, false)); "id/non-strict/dups")]
    #[test_case("a", false, false, &[] => Ok((svec(&["EXTRACT_ARG", "ILB 0 (a)"]), false, false)); "id/non-strict/no-dups")]
    #[test_case("(a)", true, true, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "PUT_VALUE", "POP"]), false, false)); "afp/strict/dups")]
    #[test_case("(a)", true, false, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "IRB", "POP"]), false, false)); "afp/strict/no-dups")]
    #[test_case("(a)", false, true, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "PUT_VALUE", "POP"]), false, false)); "afp/non-strict/dups")]
    #[test_case("(a)", false, false, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "IRB", "POP"]), false, false)); "afp/non-strict/no-dups")]
    fn compile_binding_initialization(
        src: &str,
        strict: bool,
        has_dups: bool,
        what: &[(Fillable, usize)],
    ) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).arrow_parameters();
        let mut c = complex_filled_chunk("x", what);
        node.compile_binding_initialization(&mut c, strict, src, has_dups)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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

    #[test_case("(a)", true, true, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "PUT_VALUE", "POP"]), false, false)); "afp/strict/dups")]
    #[test_case("(a)", true, false, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "IRB", "POP"]), false, false)); "afp/strict/no-dups")]
    #[test_case("(a)", false, true, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "PUT_VALUE", "POP"]), false, false)); "afp/non-strict/dups")]
    #[test_case("(a)", false, false, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "IRB", "POP"]), false, false)); "afp/non-strict/no-dups")]
    fn compile_binding_initialization(
        src: &str,
        strict: bool,
        has_dups: bool,
        what: &[(Fillable, usize)],
    ) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).arrow_formal_parameters();
        let mut c = complex_filled_chunk("x", what);
        node.compile_binding_initialization(&mut c, strict, src, has_dups)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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

    #[test_case("a", true, true, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "PUT_VALUE", "POP"]), false, false)); "ufp/strict/dups")]
    #[test_case("a", true, false, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "IRB", "POP"]), false, false)); "ufp/strict/no-dups")]
    #[test_case("a", false, true, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "PUT_VALUE", "POP"]), false, false)); "ufp/non-strict/dups")]
    #[test_case("a", false, false, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "IRB", "POP"]), false, false)); "ufp/non-strict/no-dups")]
    fn compile_binding_initialization(
        src: &str,
        strict: bool,
        has_dups: bool,
        what: &[(Fillable, usize)],
    ) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).unique_formal_parameters();
        let mut c = complex_filled_chunk("x", what);
        node.compile_binding_initialization(&mut c, strict, src, has_dups)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

#[test_case(true, true => svec(&["STRING 0 (simply_fascinating)", "STRICT_RESOLVE", "SWAP", "PUT_VALUE", "POP"]); "strict/dups")]
#[test_case(true, false => svec(&["ILB 0 (simply_fascinating)"]); "strict/no-dups")]
#[test_case(false, true => svec(&["STRING 0 (simply_fascinating)", "RESOLVE", "SWAP", "PUT_VALUE", "POP"]); "non-strict/dups")]
#[test_case(false, false => svec(&["ILB 0 (simply_fascinating)"]); "non-strict/no-dups")]
fn compile_initialize_bound_name(strict: bool, has_dups: bool) -> Vec<String> {
    let mut c = Chunk::new("cibn");
    let string_idx = c.add_to_string_pool("simply_fascinating".into()).unwrap();
    super::compile_initialize_bound_name(&mut c, strict, has_dups, string_idx);
    c.disassemble().into_iter().filter_map(disasm_filt).collect()
}

mod formal_parameter_list {
    use super::*;
    use test_case::test_case;

    #[test_case("a", true, true, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "PUT_VALUE", "POP"]), false, false)); "strict/dups")]
    #[test_case("a", true, false, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "IRB", "POP"]), false, false)); "strict/no-dups")]
    #[test_case("a", false, true, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "PUT_VALUE", "POP"]), false, false)); "non-strict/dups")]
    #[test_case("a", false, false, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "IRB", "POP"]), false, false)); "non-strict/no-dups")]
    #[test_case("a,b", true, true, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "PUT_VALUE", "POP", "EXTRACT_ARG", "STRING 1 (b)", "STRICT_RESOLVE", "SWAP", "PUT_VALUE", "POP"]), false, false)); "list - strict/dups")]
    #[test_case("a,b", true, false, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "IRB", "POP", "EXTRACT_ARG", "STRING 1 (b)", "STRICT_RESOLVE", "SWAP", "IRB", "POP"]), false, false)); "list - strict/no-dups")]
    #[test_case("a,b", false, true, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "PUT_VALUE", "POP", "EXTRACT_ARG", "STRING 1 (b)", "RESOLVE", "SWAP", "PUT_VALUE", "POP"]), false, false)); "list - non-strict/dups")]
    #[test_case("a,b", false, false, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "IRB", "POP", "EXTRACT_ARG", "STRING 1 (b)", "RESOLVE", "SWAP", "IRB", "POP"]), false, false)); "list - non-strict/no-dups")]
    #[test_case("a=x,b", true, false, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "JUMP_NOT_UNDEF 12", "POP", "STRING 1 (x)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 5", "UNWIND 1", "UNWIND_LIST", "JUMP 2", "IRB", "POP", "EXTRACT_ARG", "STRING 2 (b)", "STRICT_RESOLVE", "SWAP", "IRB", "POP"]), true, false)); "list - left:fallible")]
    #[test_case("a,b=x", true, false, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "IRB", "POP", "EXTRACT_ARG", "STRING 1 (b)", "STRICT_RESOLVE", "SWAP", "JUMP_NOT_UNDEF 12", "POP", "STRING 2 (x)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 5", "UNWIND 1", "UNWIND_LIST", "JUMP 2", "IRB", "POP"]), true, false)); "list - right:fallible")]
    #[test_case("a,b", false, false, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "left: compilation fails")]
    #[test_case("a,b", false, false, &[(Fillable::String, 1)] => serr("Out of room for strings in this compilation unit"); "right: compilation fails")]
    fn compile_binding_initialization(
        src: &str,
        strict: bool,
        has_dups: bool,
        what: &[(Fillable, usize)],
    ) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).formal_parameter_list();
        let mut c = complex_filled_chunk("x", what);
        node.compile_binding_initialization(&mut c, strict, src, has_dups)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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

    #[test_case("a", true, true, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "PUT_VALUE", "POP"]), false, false)); "strict/dups")]
    #[test_case("a", true, false, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "IRB", "POP"]), false, false)); "strict/no-dups")]
    #[test_case("a", false, true, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "PUT_VALUE", "POP"]), false, false)); "non-strict/dups")]
    #[test_case("a", false, false, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "IRB", "POP"]), false, false)); "non-strict/no-dups")]
    fn compile_binding_initialization(
        src: &str,
        strict: bool,
        has_dups: bool,
        what: &[(Fillable, usize)],
    ) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).formal_parameter();
        let mut c = complex_filled_chunk("x", what);
        node.compile_binding_initialization(&mut c, strict, src, has_dups)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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

    #[test_case("a", true, true, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "PUT_VALUE", "POP"]), false, false)); "strict/dups")]
    #[test_case("a", true, false, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "STRICT_RESOLVE", "SWAP", "IRB", "POP"]), false, false)); "strict/no-dups")]
    #[test_case("a", false, true, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "PUT_VALUE", "POP"]), false, false)); "non-strict/dups")]
    #[test_case("a", false, false, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "IRB", "POP"]), false, false)); "non-strict/no-dups")]
    #[test_case("a", false, false, &[(Fillable::String, 0)] => serr("Out of room for strings in this compilation unit"); "string table full")]
    #[test_case("a=0", false, false, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "JUMP_NOT_UNDEF 3", "POP", "FLOAT 0 (0)", "IRB", "POP"]), false, false)); "non-strict/no-dupes/simple initializer")]
    #[test_case("a=b", false, false, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "JUMP_NOT_UNDEF 12", "POP", "STRING 1 (b)", "RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 5", "UNWIND 1", "UNWIND_LIST", "JUMP 2", "IRB", "POP"]), true, false)); "non-strict/no-dupes/fallible initializer")]
    #[test_case("a=function(){}", false, false, &[] => Ok((svec(&["EXTRACT_ARG", "STRING 0 (a)", "RESOLVE", "SWAP", "JUMP_NOT_UNDEF 12", "POP", "STRING 0 (a)", "FUNC_IIFE 0", "JUMP_IF_NORMAL 5", "UNWIND 1", "UNWIND_LIST", "JUMP 2", "IRB", "POP"]), true, false)); "non-strict/no-dupes/anonymous fcn")]
    #[test_case("a=function(){}", false, false, &[(Fillable::FunctionStash, 0)] => serr("Out of room for more functions!"); "fcn comp fails")]
    #[test_case("a=b", false, false, &[(Fillable::String, 1)] => serr("Out of room for strings in this compilation unit"); "initializer comp fails")]
    #[test_case("a=@@@", false, false, &[] => serr("out of range integral type conversion attempted"); "value-present branch too far")]
    fn compile_binding_initialization(
        src: &str,
        strict: bool,
        has_dups: bool,
        what: &[(Fillable, usize)],
    ) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).single_name_binding();
        let mut c = complex_filled_chunk("x", what);
        node.compile_binding_initialization(&mut c, strict, src, has_dups)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.maybe_abrupt(),
                    status.maybe_ref(),
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod function_rest_parameter {
    use super::*;
    use test_case::test_case;

    #[test_case("...a", false, false, &[] => panics "not yet implemented"; "panic")]
    fn compile_binding_initialization(
        src: &str,
        strict: bool,
        has_dups: bool,
        what: &[(Fillable, usize)],
    ) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).function_rest_parameter();
        let mut c = complex_filled_chunk("x", what);
        node.compile_binding_initialization(&mut c, strict, src, has_dups)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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

    #[test_case("function a(){}", true, &[] => Ok((svec(&["CUA", "CNSILB 0 (arguments)", "ILB 0 (arguments)", "FINISH_ARGS", "UNDEFINED", "END_FUNCTION"]), false, false)); "typical/empty params")]
    #[test_case("function a(q){}", true, &[] => Ok((svec(&["CPMLBM 0 (q)", "CUA", "CNSILB 1 (arguments)", "ILB 1 (arguments)", "EXTRACT_ARG", "STRING 0 (q)", "STRICT_RESOLVE", "SWAP", "IRB", "POP", "FINISH_ARGS", "UNDEFINED", "END_FUNCTION"]), false, false)); "typical/simple params/strict")]
    #[test_case("function a(q){}", false, &[] => Ok((svec(&["CPMLBM 0 (q)", "CMA", "CPMLB 1 (arguments)", "ILB 1 (arguments)", "EXTRACT_ARG", "STRING 0 (q)", "RESOLVE", "SWAP", "IRB", "POP", "FINISH_ARGS", "PNLE", "UNDEFINED", "END_FUNCTION"]), false, false)); "typical/simple params/non-strict")]
    #[test_case("function a(q){'use strict';}", false, &[] => Ok((svec(&["CPMLBM 0 (q)", "CUA", "CNSILB 1 (arguments)", "ILB 1 (arguments)", "EXTRACT_ARG", "STRING 0 (q)", "STRICT_RESOLVE", "SWAP", "IRB", "POP", "FINISH_ARGS", "STRING 2 (use strict)", "END_FUNCTION"]), false, false)); "typical/simple params/directive")]
    #[test_case("function a(q=b){}", true, &[] => Ok((svec(&["CPMLBM 0 (q)", "CUA", "CNSILB 1 (arguments)", "ILB 1 (arguments)", "EXTRACT_ARG", "STRING 0 (q)", "STRICT_RESOLVE", "SWAP", "JUMP_NOT_UNDEF 12", "POP", "STRING 2 (b)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 5", "UNWIND 1", "UNWIND_LIST", "JUMP 2", "IRB", "POP", "JUMP_IF_NORMAL 4", "UNWIND 1", "JUMP 3", "FINISH_ARGS", "PNVEFL", "SLETVE", "JUMP_IF_ABRUPT 1", "UNDEFINED", "END_FUNCTION"]), true, false)); "fallible instantiation")]
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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

    super::compile_new_evaluator(&mut c, strict, src, constructor_expression, potential_arguments)
        .map(|status| {
            (
                c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
    #[test_case("{a}", true, &[] => Ok((svec(&["TODO"]), true, false)); "pattern")]
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
    #[test_case("catch({e}){}", true, &[(Fillable::String, 1)] => serr("Out of room for strings in this compilation unit"); "binding init compile fails")]
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
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
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
                (c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(), status.maybe_abrupt())
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
                (c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(), status.maybe_abrupt())
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
                (c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(), status.maybe_abrupt())
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
                (c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(), status.maybe_abrupt())
            })
            .map_err(|e| e.to_string())
    }
}
