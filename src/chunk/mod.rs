use super::*;
use ahash::AHashSet;
use anyhow::anyhow;
use itertools::Itertools;
use num::bigint::BigInt;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub struct StashedFunctionData {
    pub source_text: String,
    pub params: ParamSource,
    pub body: BodySource,
    pub to_compile: FunctionSource,
    pub strict: bool,
    pub this_mode: ThisLexicality,
}

pub struct ConciseChunk<'a>(pub &'a Chunk);
impl fmt::Debug for ConciseChunk<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Chunk {{ {} }}", self.0.name)
    }
}
/// A compilation unit
#[derive(Debug, Default)]
pub struct Chunk {
    pub name: String,
    pub strings: Vec<JSString>,
    pub opcodes: Vec<Opcode>,
    pub floats: Vec<f64>,
    pub bigints: Vec<Rc<BigInt>>,
    pub string_sets: Vec<AHashSet<JSString>>,
    pub function_object_data: Vec<StashedFunctionData>,
}

impl Chunk {
    pub fn new(name: impl Into<String>) -> Self {
        Self { name: name.into(), ..Default::default() }
    }

    pub fn dup_without_code(src: &Chunk, name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            strings: src.strings.clone(),
            floats: src.floats.clone(),
            bigints: src.bigints.clone(),
            string_sets: src.string_sets.clone(),
            //function_object_data: src.function_object_data.clone(),
            ..Default::default()
        }
    }

    pub fn add_to_string_pool(&mut self, s: JSString) -> anyhow::Result<u16> {
        Self::add_to_pool(&mut self.strings, s, "strings")
    }

    pub fn add_to_float_pool(&mut self, n: f64) -> anyhow::Result<u16> {
        Self::add_to_pool(&mut self.floats, n, "floats")
    }

    pub fn add_to_bigint_pool(&mut self, n: Rc<BigInt>) -> anyhow::Result<u16> {
        Self::add_to_pool(&mut self.bigints, n, "big ints")
    }

    pub fn add_to_string_set_pool(&mut self, strings: &[JSString]) -> anyhow::Result<u16> {
        let label_set = strings.iter().cloned().collect::<AHashSet<JSString>>();
        Self::add_to_pool(&mut self.string_sets, label_set, "string sets")
    }

    fn add_to_pool<Item>(collection: &mut Vec<Item>, item: Item, collection_name: &str) -> anyhow::Result<u16>
    where
        Item: PartialEq,
    {
        for (idx, existing) in collection.iter().enumerate() {
            if existing == &item {
                // This unwrap is safe, as we're returning something that was ok at least once before.
                return Ok(idx.try_into().unwrap());
            }
        }
        collection.push(item);
        (collection.len() - 1)
            .try_into()
            .map_err(|_| anyhow!("Out of room for {} in this compilation unit", collection_name))
    }

    pub fn add_to_func_stash(&mut self, fd: StashedFunctionData) -> anyhow::Result<u16> {
        let result = self.function_object_data.len();
        self.function_object_data.push(fd);
        result.try_into().map_err(|_| anyhow!("Out of room for more functions!"))
    }

    pub fn op(&mut self, opcode: Insn) {
        self.opcodes.push(opcode.into());
    }

    pub fn op_plus_arg(&mut self, opcode: Insn, arg: u16) {
        self.opcodes.push(opcode.into());
        self.opcodes.push(arg);
    }

    pub fn op_plus_two_args(&mut self, opcode: Insn, arg1: u16, arg2: u16) {
        self.opcodes.push(opcode.into());
        self.opcodes.push(arg1);
        self.opcodes.push(arg2);
    }

    pub fn op_jump(&mut self, opcode: Insn) -> usize {
        self.opcodes.push(opcode.into());
        self.opcodes.push(0);
        self.opcodes.len() - 1
    }

    #[expect(clippy::cast_sign_loss)]
    pub fn op_jump_back(&mut self, opcode: Insn, location: usize) -> anyhow::Result<()> {
        self.opcodes.push(opcode.into());
        let delta = isize::try_from(location).expect("a hope and a prayer")
            - isize::try_from(self.opcodes.len()).expect("should be ok")
            - 1;
        let offset = i16::try_from(delta)?;
        self.opcodes.push(offset as u16);
        Ok(())
    }

    #[expect(clippy::cast_sign_loss)]
    pub fn fixup(&mut self, mark: usize) -> anyhow::Result<()> {
        let len = self.opcodes.len();
        if mark >= len {
            anyhow::bail!("Fixup location out of range");
        }
        let offset = i16::try_from(len - mark - 1)?;
        self.opcodes[mark] = offset as u16;
        Ok(())
    }

    pub fn pos(&self) -> usize {
        self.opcodes.len()
    }

    #[expect(clippy::cast_possible_wrap)]
    pub fn insn_repr_at(&self, starting_idx: usize) -> (usize, String) {
        let mut idx = starting_idx;
        let insn = Insn::try_from(self.opcodes[idx]).unwrap();
        idx += 1;
        match insn {
            Insn::String
            | Insn::CreateStrictImmutableLexBinding
            | Insn::CreateNonStrictImmutableLexBinding
            | Insn::CreatePermanentMutableLexBinding
            | Insn::CreateInitializedPermanentMutableLexIfMissing
            | Insn::CreatePermanentMutableLexIfMissing
            | Insn::CreatePermanentMutableVarBinding
            | Insn::InitializeLexBinding
            | Insn::GetLexBinding
            | Insn::InitializeVarBinding
            | Insn::SetMutableVarBinding
            | Insn::CreatePrivateNameIfMissing
            | Insn::TargetedContinue
            | Insn::TargetedBreak
            | Insn::HandleTargetedBreak
            | Insn::PrivateIdLookup
            | Insn::AttachSourceText
            | Insn::MakePrivateReference => {
                let arg = self.opcodes[idx] as usize;
                (2, format!("    {:<24}{} ({})", insn, arg, String::from(&self.strings[arg]).escape_debug()))
            }
            Insn::Float => {
                let arg = self.opcodes[idx] as usize;
                (2, format!("    {:<24}{} ({})", insn, arg, self.floats[arg]))
            }
            Insn::Bigint => {
                let arg = self.opcodes[idx] as usize;
                (2, format!("    {:<24}{} ({})", insn, arg, self.bigints[arg]))
            }
            Insn::Unwind
            | Insn::UnwindIfAbrupt
            | Insn::RotateUp
            | Insn::RotateDown
            | Insn::RotateDownList
            | Insn::RotateListDown
            | Insn::RotateListUp
            | Insn::PopOutList
            | Insn::InstantiateIdFreeFunctionExpression
            | Insn::InstantiateArrowFunctionExpression
            | Insn::InstantiateGeneratorFunctionExpression
            | Insn::InstantiateOrdinaryFunctionExpression
            | Insn::EvaluateInitializedClassFieldDefinition
            | Insn::EvaluateInitializedClassStaticFieldDefinition
            | Insn::EvaluateClassStaticBlockDefinition
            | Insn::DefineMethod
            | Insn::DefineMethodProperty
            | Insn::AttachElements => {
                let arg = self.opcodes[idx] as usize;
                (2, format!("    {insn:<24}{arg}"))
            }
            Insn::Ref
            | Insn::StrictRef
            | Insn::Resolve
            | Insn::StrictResolve
            | Insn::Nop
            | Insn::ToDo
            | Insn::InitializeReferencedBinding
            | Insn::PopLexEnv
            | Insn::PushNewLexEnv
            | Insn::PushNewVarEnvFromLex
            | Insn::PushNewLexEnvFromVar
            | Insn::SetLexEnvToVarEnv
            | Insn::SetAsideLexEnv
            | Insn::RestoreLexEnv
            | Insn::PushNewPrivateEnv
            | Insn::PopPrivateEnv
            | Insn::PushWithEnv
            | Insn::CreateDataProperty
            | Insn::SetPrototype
            | Insn::ToPropertyKey
            | Insn::CopyDataProps
            | Insn::CopyDataPropsWithExclusions
            | Insn::This
            | Insn::Null
            | Insn::Undefined
            | Insn::True
            | Insn::False
            | Insn::Empty
            | Insn::EmptyIfNotError
            | Insn::UndefinedIfEmpty
            | Insn::Zero
            | Insn::GetValue
            | Insn::PutValue
            | Insn::FunctionPrototype
            | Insn::ObjectPrototype
            | Insn::Call
            | Insn::StrictCall
            | Insn::EndFunction
            | Insn::Return
            | Insn::UpdateEmpty
            | Insn::Swap
            | Insn::Pop
            | Insn::PopOrPanic
            | Insn::Pop2Push3
            | Insn::Dup
            | Insn::DupAfterList
            | Insn::ToString
            | Insn::ToNumeric
            | Insn::ToObject
            | Insn::Increment
            | Insn::Decrement
            | Insn::PreIncrement
            | Insn::PreDecrement
            | Insn::Delete
            | Insn::Void
            | Insn::TypeOf
            | Insn::UnaryPlus
            | Insn::UnaryMinus
            | Insn::UnaryComplement
            | Insn::UnaryNot
            | Insn::Exponentiate
            | Insn::Multiply
            | Insn::Divide
            | Insn::Modulo
            | Insn::Add
            | Insn::Subtract
            | Insn::LeftShift
            | Insn::SignedRightShift
            | Insn::UnsignedRightShift
            | Insn::Less
            | Insn::Greater
            | Insn::LessEqual
            | Insn::GreaterEqual
            | Insn::InstanceOf
            | Insn::In
            | Insn::Equal
            | Insn::NotEqual
            | Insn::StrictEqual
            | Insn::StrictNotEqual
            | Insn::BitwiseAnd
            | Insn::BitwiseOr
            | Insn::BitwiseXor
            | Insn::Throw
            | Insn::CreateUnmappedArguments
            | Insn::CreateMappedArguments
            | Insn::HandleEmptyBreak
            | Insn::CoalesceValue
            | Insn::Continue
            | Insn::Break
            | Insn::ExtractArg
            | Insn::FinishArgs
            | Insn::UnwindList
            | Insn::AppendList
            | Insn::ExtractThrownValue
            | Insn::SwapList
            | Insn::SwapDeepList
            | Insn::PopList
            | Insn::RequireConstructor
            | Insn::Construct
            | Insn::Object
            | Insn::ObjectWithProto
            | Insn::Array
            | Insn::IteratorAccumulate
            | Insn::IterateArguments
            | Insn::RequireCoercible
            | Insn::GetSyncIterator
            | Insn::IteratorClose
            | Insn::IteratorCloseIfNotDone
            | Insn::IteratorNext
            | Insn::IteratorResultComplete
            | Insn::IteratorResultToValue
            | Insn::GetV
            | Insn::IteratorDAEElision
            | Insn::EmbellishedIteratorStep
            | Insn::IteratorRest
            | Insn::EnumerateObjectProperties
            | Insn::ListToArray
            | Insn::SetFunctionName
            | Insn::GetParentsFromSuperclass
            | Insn::GeneratorStartFromFunction
            | Insn::Yield
            | Insn::CreateDefaultConstructor
            | Insn::MakeClassConstructorAndSetName
            | Insn::MakeConstructor
            | Insn::MakeConstructorWithProto
            | Insn::SetDerived
            | Insn::NameOnlyFieldRecord
            | Insn::NameOnlyStaticFieldRecord
            | Insn::GetNewTarget
            | Insn::GetSuperConstructor
            | Insn::ConstructorCheck
            | Insn::BindThisAndInit
            | Insn::StaticClassItem => (1, format!("    {insn}")),
            Insn::JumpIfAbrupt
            | Insn::Jump
            | Insn::JumpIfNormal
            | Insn::JumpIfFalse
            | Insn::JumpIfTrue
            | Insn::JumpPopIfFalse
            | Insn::JumpPopIfTrue
            | Insn::JumpIfNotNullish
            | Insn::JumpIfNullish
            | Insn::JumpIfNotUndef
            | Insn::JumpNotThrow => {
                let arg = self.opcodes[idx] as i16;
                (2, format!("    {insn:<24}{arg}"))
            }
            Insn::AddMappedArgument
            | Insn::InstantiateOrdinaryFunctionObject
            | Insn::InstantiateGeneratorFunctionObject => {
                let string_arg = self.opcodes[idx] as usize;
                let index_arg = self.opcodes[idx + 1] as usize;
                (3, format!("    {:<24}{} {}", insn, index_arg, self.strings[string_arg]))
            }
            Insn::RegExpCreate => {
                let pattern_arg = self.opcodes[idx] as usize;
                let flags_arg = self.opcodes[idx + 1] as usize;
                (3, format!("    {insn:<24}/{}/{}", self.strings[pattern_arg], self.strings[flags_arg]))
            }
            Insn::DefineGetter | Insn::DefineSetter | Insn::InstantiateGeneratorMethod => {
                let arg = self.opcodes[idx] as usize;
                let flag = self.opcodes[idx + 1] != 0;
                (3, format!("    {:<24}{} {}", insn, arg, if flag { "enumerable" } else { "hidden" }))
            }
            Insn::MakeSuperPropertyReference => {
                let flag = self.opcodes[idx] != 0;
                (2, format!("    {:<24}{}", insn, if flag { "strict" } else { "non-strict" }))
            }
            Insn::LoopContinues | Insn::CreatePerIterationEnvironment => {
                let string_set_idx = self.opcodes[idx] as usize;
                let mut string_set = self.string_sets[string_set_idx].iter().collect::<Vec<&JSString>>();
                string_set.sort();
                (2, format!("    {:<24}[{}]", insn, string_set.iter().join(", ")))
            }
        }
    }

    pub fn disassemble(&self) -> Vec<String> {
        let mut idx = 0;
        let mut result = vec![];
        result.push(format!("====== {} ======", self.name));
        while idx < self.opcodes.len() {
            let (inc, repr) = self.insn_repr_at(idx);
            idx += inc;
            result.push(repr);
        }
        result
    }

    pub fn repr_with_size(&self) -> Vec<(String, usize)> {
        let mut idx = 0;
        let mut result = vec![];
        while idx < self.opcodes.len() {
            let (inc, repr) = self.insn_repr_at(idx);
            idx += inc;
            result.push((repr, inc));
        }
        result
    }

    pub fn set_name(&mut self, name: &str) {
        self.name = String::from(name);
    }

    pub fn analyze_strictness(&self) -> Strictness {
        let mut idx = 0;
        let mut saw_strict = false;
        let mut saw_non_strict = false;
        while idx < self.opcodes.len() && (!saw_strict || !saw_non_strict) {
            let (inc, repr) = self.insn_repr_at(idx);
            idx += inc;

            let repr = repr.split_whitespace().next().unwrap();
            if ["CALL_STRICT", "STRICT_RESOLVE", "STRICT_REF"].contains(&repr) {
                saw_strict = true;
            } else if ["CALL", "RESOLVE", "REF"].contains(&repr) {
                saw_non_strict = true;
            }
        }
        match (saw_strict, saw_non_strict) {
            (true, true) => Strictness::Mixed,
            (true, false) => Strictness::Strict,
            (false, true) => Strictness::NonStrict,
            (false, false) => Strictness::Indeterminate,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Strictness {
    /// Has strict-style instructions, and no non-strict instructions
    Strict,
    /// Has non-strict instructions, and no strict instructions
    NonStrict,
    /// Has neither non-strict nor strict instructions
    Indeterminate,
    /// Has both strict and non-strict instructions
    Mixed,
}

#[cfg(test)]
mod tests;
