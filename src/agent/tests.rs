#![expect(clippy::bool_assert_comparison)]
use super::*;
use crate::parser::testhelp::*;
use crate::tests::*;
use ahash::AHashSet;
use num::BigInt;
use regex::Regex;
use std::cell::RefCell;
use std::rc::Rc;
use std::str::FromStr;
use std::sync::LazyLock;
use test_case::test_case;

mod agent {
    use super::*;
    use crate::values::tests::make_test_obj_uncallable;
    use test_case::test_case;

    #[test]
    fn new() {
        let agent = Agent::new();

        // New agent; no realm initialized.
        assert!(agent.execution_context_stack.borrow().is_empty());

        // All well-known symbols initialized, and different from one another.
        let symbols = [
            agent.symbols.async_iterator_,
            agent.symbols.has_instance_,
            agent.symbols.is_concat_spreadable_,
            agent.symbols.iterator_,
            agent.symbols.match_,
            agent.symbols.match_all_,
            agent.symbols.replace_,
            agent.symbols.search_,
            agent.symbols.species_,
            agent.symbols.split_,
            agent.symbols.to_primitive_,
            agent.symbols.to_string_tag_,
            agent.symbols.unscopables_,
        ];
        let num_symbols = symbols.len();
        let symbol_set = symbols.iter().collect::<AHashSet<_>>();
        assert_eq!(num_symbols, symbol_set.len());

        // ID trackers at reasonable spots
        assert_eq!(agent.obj_id.get(), 1);
        assert_eq!(agent.symbol_id.get(), num_symbols + 1);
    }

    #[test]
    fn default() {
        let agent = Agent::default();

        // New agent; no realm initialized.
        assert!(agent.execution_context_stack.borrow().is_empty());

        // All well-known symbols initialized, and different from one another.
        let symbols = [
            agent.symbols.async_iterator_,
            agent.symbols.has_instance_,
            agent.symbols.is_concat_spreadable_,
            agent.symbols.iterator_,
            agent.symbols.match_,
            agent.symbols.match_all_,
            agent.symbols.replace_,
            agent.symbols.search_,
            agent.symbols.species_,
            agent.symbols.split_,
            agent.symbols.to_primitive_,
            agent.symbols.to_string_tag_,
            agent.symbols.unscopables_,
        ];
        let num_symbols = symbols.len();
        let symbol_set = symbols.iter().collect::<AHashSet<_>>();
        assert_eq!(num_symbols, symbol_set.len());

        // ID trackers at reasonable spots
        assert_eq!(agent.obj_id.get(), 1);
        assert_eq!(agent.symbol_id.get(), num_symbols + 1);
    }

    #[test]
    fn pop_execution_context() {
        setup_test_agent();
        let realm_ref = current_realm_record().unwrap();
        // build a new EC, and add it to the EC stack
        let sr = ScriptRecord {
            realm: realm_ref.clone(),
            ecmascript_code: Maker::new("").script(),
            compiled: Rc::new(Chunk::new("test")),
            text: String::new(),
        };
        let test_ec = ExecutionContext::new(None, realm_ref, Some(ScriptOrModule::Script(Rc::new(sr))));
        super::push_execution_context(test_ec);
        // now pop it.
        super::pop_execution_context();
        // And verify the one on top has no script_or_module value
        AGENT.with(|agent| {
            let r = &agent.execution_context_stack.borrow()[agent.execution_context_stack.borrow().len() - 1];
            assert!(r.script_or_module.is_none());
        });
    }
    #[test]
    fn push_execution_context() {
        setup_test_agent();
        let realm_ref = current_realm_record().unwrap();
        AGENT.with(|agent| {
            let prior_length = agent.execution_context_stack.borrow().len();
            // build a new EC, and add it to the EC stack
            let sr = ScriptRecord {
                realm: realm_ref.clone(),
                ecmascript_code: Maker::new("").script(),
                compiled: Rc::new(Chunk::new("test")),
                text: String::new(),
            };
            let test_ec = ExecutionContext::new(None, realm_ref, Some(ScriptOrModule::Script(Rc::new(sr))));
            super::push_execution_context(test_ec);

            assert_eq!(agent.execution_context_stack.borrow().len(), prior_length + 1);
            let r = &agent.execution_context_stack.borrow()[agent.execution_context_stack.borrow().len() - 1];
            assert!(r.script_or_module.is_some());
        });
    }
    #[test]
    fn active_function_object() {
        setup_test_agent();
        AGENT.with(Agent::reset);
        // no Running Execution Context, so this should be None.
        let afo = super::active_function_object();
        assert!(afo.is_none());

        super::initialize_host_defined_realm(100, true);
        // Now there's an execution context, but still no active function, so this should still be None.
        let afo = super::active_function_object();
        assert!(afo.is_none());

        // Create a new EC that _does_ have a function object; push it, and then check the active function.
        let fo = intrinsic(IntrinsicId::ThrowTypeError);
        let realm = current_realm_record().unwrap();
        let function_ec = ExecutionContext::new(Some(fo.clone()), realm, None);
        super::push_execution_context(function_ec);

        let afo = super::active_function_object().unwrap();
        assert_eq!(afo, fo);
    }
    #[test]
    fn next_object_id() {
        setup_test_agent();
        AGENT.with(Agent::reset);
        // Starts at something, and then increases monotonically.
        let first = super::next_object_id();
        for x in 1..10 {
            assert_eq!(super::next_object_id(), x + first);
        }
    }
    #[test]
    fn next_symbol_id() {
        setup_test_agent();
        AGENT.with(Agent::reset);
        // Starts at something, and then increases monotonically.
        let first = super::next_symbol_id();
        for x in 1..10 {
            assert_eq!(super::next_symbol_id(), x + first);
        }
    }
    #[test]
    fn debug() {
        assert_ne!(format!("{:?}", Agent::new()), "");
    }

    #[test]
    fn global_symbol_registry() {
        setup_test_agent();
        AGENT.with(Agent::reset);
        let registry = Rc::new(RefCell::new(SymbolRegistry::new()));
        AGENT.with(|agent| agent.set_global_symbol_registry(registry.clone()));
        let gsr = super::global_symbol_registry();
        assert!(Rc::ptr_eq(&registry, &gsr));
    }
    #[test]
    #[should_panic(expected = "GSR: Attempted change after having already been set")]
    fn global_symbol_registry_panic() {
        setup_test_agent();
        AGENT.with(Agent::reset);
        let registry1 = Rc::new(RefCell::new(SymbolRegistry::new()));
        let registry2 = Rc::new(RefCell::new(SymbolRegistry::new()));
        AGENT.with(|agent| agent.set_global_symbol_registry(registry1.clone()));
        AGENT.with(|agent| agent.set_global_symbol_registry(registry2.clone()));
    }

    mod current_realm_record {
        use super::*;

        #[test]
        fn empty_ec_stack() {
            setup_test_agent();
            AGENT.with(Agent::reset);
            assert!(current_realm_record().is_none());
        }

        #[test]
        fn stacked() {
            setup_test_agent();

            let first_realm = create_named_realm("first");
            let first_context = ExecutionContext::new(None, first_realm, None);
            crate::push_execution_context(first_context);

            let second_realm = create_named_realm("second");
            let second_context = ExecutionContext::new(None, second_realm, None);
            crate::push_execution_context(second_context);

            assert_eq!(get_realm_name(), "second");

            crate::pop_execution_context();

            assert_eq!(get_realm_name(), "first");
        }
    }

    #[test_case(|| Ok(NormalCompletion::from(10)) => Ok(NormalCompletion::from(ECMAScriptValue::Undefined)); "value")]
    #[test_case(|| Err(create_type_error("Test Sentinel")) => serr("TypeError: Test Sentinel"); "pending type error")]
    #[test_case(|| {
            let env = current_realm_record().unwrap().borrow().global_env.clone().unwrap();
            Ok(NormalCompletion::from(Reference::new(Base::Environment(env), "debug_token", true, None)))
        } => Ok(NormalCompletion::from(ECMAScriptValue::Undefined)); "valid ref")]
    fn void_operator(make_expr: fn() -> FullCompletion) -> Result<NormalCompletion, String> {
        setup_test_agent();
        let expr = make_expr();
        super::void_operator(expr).map_err(unwind_any_error)
    }

    #[test_case(|| Ok(NormalCompletion::from(Reference::new(Base::Unresolvable, "not_here", true, None))) => Ok(NormalCompletion::from("undefined")); "unresolvable ref")]
    #[test_case(|| Err(create_type_error("Test Sentinel")) => serr("TypeError: Test Sentinel"); "pending type error")]
    #[test_case(|| Ok(NormalCompletion::from(ECMAScriptValue::Undefined)) => Ok(NormalCompletion::from("undefined")); "undefined value")]
    #[test_case(|| Ok(NormalCompletion::from(ECMAScriptValue::Null)) => Ok(NormalCompletion::from("object")); "null value")]
    #[test_case(|| Ok(NormalCompletion::from(true)) => Ok(NormalCompletion::from("boolean")); "bool value")]
    #[test_case(|| Ok(NormalCompletion::from("just a string")) => Ok(NormalCompletion::from("string")); "string value")]
    #[test_case(|| Ok(NormalCompletion::from(227)) => Ok(NormalCompletion::from("number")); "number value")]
    #[test_case(|| Ok(NormalCompletion::from(BigInt::from(102))) => Ok(NormalCompletion::from("bigint")); "bigint value")]
    #[test_case(|| {
        let symbol_constructor = intrinsic(IntrinsicId::Symbol);
        let reference = Reference::new(Base::Value(ECMAScriptValue::from(symbol_constructor)), "species", true, None);
        Ok(NormalCompletion::from(reference))
    } => Ok(NormalCompletion::from("symbol")); "typeof Symbol.species")]
    #[test_case(|| {
        let env = current_realm_record().unwrap().borrow().global_env.clone().unwrap();
        Ok(NormalCompletion::from(Reference::new(Base::Environment(env), "Boolean", true, None)))
    } => Ok(NormalCompletion::from("function")); "typeof Boolean")]
    #[test_case(|| {
        let bool_proto = intrinsic(IntrinsicId::BooleanPrototype);
        Ok(NormalCompletion::from(bool_proto))
    } => Ok(NormalCompletion::from("object")); "typeof Boolean.prototype")]
    fn typeof_operator(make_expr: fn() -> FullCompletion) -> Result<NormalCompletion, String> {
        setup_test_agent();
        let expr = make_expr();
        super::typeof_operator(expr).map_err(unwind_any_error)
    }

    #[expect(clippy::unnecessary_wraps)]
    fn superproperty() -> FullCompletion {
        // For example: ({method() { delete super.test_property; }}).method()
        // 1. Let F be OrdinaryFunctionCreate(intrinsics.[[%FunctionPrototype%]], source_text, ParameterList, Body, thisMode, env, privateenv).
        // 2. Let homeObject be OrdinaryObjectCreate(intrinsics.[[%ObjectPrototype%]]).
        // 2. Call MakeMethod(F, homeObject).
        // 3. Let fenv be NewFunctionEnvironment(F, undefined).
        // 4. Let actualThis be fenv.GetThisBinding().
        // 5. Return MakeSuperPropertyReference(actualThis, "test_property", true)
        let obj = ordinary_object_create(None);
        let copy = obj.clone();
        let myref = Reference::new(Base::Value(obj.into()), "item", true, Some(copy.into()));
        Ok(NormalCompletion::from(myref))
    }

    #[expect(clippy::unnecessary_wraps)]
    fn bool_proto_ref(strict: bool) -> FullCompletion {
        let bool_obj = intrinsic(IntrinsicId::Boolean);
        let myref = Reference::new(Base::Value(bool_obj.into()), "prototype", strict, None);
        Ok(NormalCompletion::from(myref))
    }
    fn strict_proto_ref() -> FullCompletion {
        bool_proto_ref(true)
    }
    fn nonstrict_proto_ref() -> FullCompletion {
        bool_proto_ref(false)
    }
    #[expect(clippy::unnecessary_wraps)]
    fn dead_ref() -> FullCompletion {
        let dead = DeadObject::object();
        Ok(NormalCompletion::from(Reference::new(Base::Value(dead.into()), "anything", true, None)))
    }
    #[expect(clippy::unnecessary_wraps)]
    fn ref_to_undefined() -> FullCompletion {
        let env = current_realm_record().unwrap().borrow().global_env.clone().unwrap();
        Ok(NormalCompletion::from(Reference::new(Base::Environment(env), "undefined", true, None)))
    }
    #[expect(clippy::unnecessary_wraps)]
    fn dead_env() -> FullCompletion {
        let outer = current_realm_record().unwrap().borrow().global_env.clone().unwrap();
        let dead = DeadObject::object();
        let obj_env = Rc::new(ObjectEnvironmentRecord::new(dead, false, Some(outer), "dead"));
        Ok(NormalCompletion::from(Reference::new(Base::Environment(obj_env), "anything", true, None)))
    }

    #[test_case(|| Ok(NormalCompletion::Empty) => Ok(NormalCompletion::from(true)); "empty -> true")]
    #[test_case(|| Ok(NormalCompletion::from("test sentinel")) => Ok(NormalCompletion::from(true)); "value -> true")]
    #[test_case(|| Err(create_type_error("Test Sentinel")) => serr("TypeError: Test Sentinel"); "pending type error")]
    #[test_case(|| Ok(NormalCompletion::from(Reference::new(Base::Unresolvable, "not_here", true, None))) => Ok(NormalCompletion::from(true)); "unresolvable ref")]
    #[test_case(superproperty => serr("ReferenceError: super properties not deletable"); "super prop")]
    #[test_case(|| Ok(NormalCompletion::from(Reference::new(Base::Value(ECMAScriptValue::Undefined), "x", true, None))) => serr("TypeError: Undefined and null cannot be converted to objects"); "Non-object ref base")]
    #[test_case(|| Ok(NormalCompletion::from(Reference::new(Base::Value(true.into()), "x", true, None))) => Ok(NormalCompletion::from(true)); "delete nonexistent")]
    #[test_case(strict_proto_ref => serr("TypeError: property not deletable"); "permanent property; strict")]
    #[test_case(nonstrict_proto_ref => Ok(NormalCompletion::from(false)); "permanent property; nonstrict")]
    #[test_case(dead_ref => serr("TypeError: delete called on DeadObject"); "property ref delete errs")]
    #[test_case(ref_to_undefined => Ok(NormalCompletion::from(false)); "undefined ref")]
    #[test_case(dead_env => serr("TypeError: delete called on DeadObject"); "env ref delete errors")]
    #[test_case(|| {
        let ir = create_list_iterator_record(vec![1.into()]);
        Ok(NormalCompletion::from(ir))
    } => Ok(NormalCompletion::from(true)); "iterator record")]
    fn delete_ref(make_expr: fn() -> FullCompletion) -> Result<NormalCompletion, String> {
        setup_test_agent();
        let expr = make_expr();
        super::delete_ref(expr).map_err(unwind_any_error)
    }

    #[test_case(|| ECMAScriptValue::from("left "),
                || ECMAScriptValue::from("right"),
                BinOp::Add
                => Ok(NormalCompletion::from("left right")); "string catentation")]
    #[test_case(|| ECMAScriptValue::from(make_toprimitive_throw_obj()),
                || ECMAScriptValue::from("a"),
                BinOp::Add
                => serr("TypeError: Test Sentinel"); "left toPrimitive error")]
    #[test_case(|| ECMAScriptValue::from("a"),
                || ECMAScriptValue::from(make_toprimitive_throw_obj()),
                BinOp::Add
                => serr("TypeError: Test Sentinel"); "right toPrimitive error")]
    #[test_case(|| ECMAScriptValue::from(10),
                || ECMAScriptValue::from("a"),
                BinOp::Add
                => Ok(NormalCompletion::from("10a")); "stringify from right")]
    #[test_case(|| ECMAScriptValue::from("a"),
                || ECMAScriptValue::from(10),
                BinOp::Add
                => Ok(NormalCompletion::from("a10")); "stringify from left")]
    #[test_case(|| ECMAScriptValue::from(super::super::wks(WksId::ToPrimitive)),
                || ECMAScriptValue::from("a"),
                BinOp::Add
                => serr("TypeError: Symbols may not be converted to strings"); "left tostring errs")]
    #[test_case(|| ECMAScriptValue::from("a"),
                || ECMAScriptValue::from(super::super::wks(WksId::ToPrimitive)),
                BinOp::Add
                => serr("TypeError: Symbols may not be converted to strings"); "right tostring errs")]
    #[test_case(|| ECMAScriptValue::from(super::super::wks(WksId::ToPrimitive)),
                || ECMAScriptValue::from(10),
                BinOp::Add
                => serr("TypeError: Symbol values cannot be converted to Number values"); "left tonumeric errs")]
    #[test_case(|| ECMAScriptValue::from(10),
                || ECMAScriptValue::from(super::super::wks(WksId::ToPrimitive)),
                BinOp::Add
                => serr("TypeError: Symbol values cannot be converted to Number values"); "right tonumeric errs")]
    #[test_case(|| ECMAScriptValue::from(2.0),
                || ECMAScriptValue::from(3.0),
                BinOp::Exponentiate
                => Ok(NormalCompletion::from(8)); "exponentiation")]
    #[test_case(|| ECMAScriptValue::from(2.0),
                || ECMAScriptValue::from(3.0),
                BinOp::Multiply
                => Ok(NormalCompletion::from(6)); "multiplication")]
    #[test_case(|| ECMAScriptValue::from(12.0),
                || ECMAScriptValue::from(3.0),
                BinOp::Divide
                => Ok(NormalCompletion::from(4)); "division")]
    #[test_case(|| ECMAScriptValue::from(26.0),
                || ECMAScriptValue::from(7.0),
                BinOp::Remainder
                => Ok(NormalCompletion::from(5)); "remainder")]
    #[test_case(|| ECMAScriptValue::from(2.0),
                || ECMAScriptValue::from(3.0),
                BinOp::Add
                => Ok(NormalCompletion::from(5)); "addition")]
    #[test_case(|| ECMAScriptValue::from(2.0),
                || ECMAScriptValue::from(3.0),
                BinOp::Subtract
                => Ok(NormalCompletion::from(-1)); "subtraction")]
    #[test_case(|| ECMAScriptValue::from(2.0),
                || ECMAScriptValue::from(3.0),
                BinOp::LeftShift
                => Ok(NormalCompletion::from(16.0)); "left shift")]
    #[test_case(|| ECMAScriptValue::from(16.0),
                || ECMAScriptValue::from(3.0),
                BinOp::SignedRightShift
                => Ok(NormalCompletion::from(2)); "signed right shift")]
    #[test_case(|| ECMAScriptValue::from(-16.0),
                || ECMAScriptValue::from(3.0),
                BinOp::SignedRightShift
                => Ok(NormalCompletion::from(-2)); "signed right shift (negative)")]
    #[test_case(|| ECMAScriptValue::from(16.0),
                || ECMAScriptValue::from(3.0),
                BinOp::UnsignedRightShift
                => Ok(NormalCompletion::from(2)); "unsigned right shift")]
    #[test_case(|| ECMAScriptValue::from(-16.0),
                || ECMAScriptValue::from(3.0),
                BinOp::UnsignedRightShift
                => Ok(NormalCompletion::from(536_870_910)); "unsigned right shift (negative)")]
    #[test_case(|| ECMAScriptValue::from(2.0),
                || ECMAScriptValue::from(3.0),
                BinOp::BitwiseAnd
                => Ok(NormalCompletion::from(2)); "bitwise and")]
    #[test_case(|| ECMAScriptValue::from(2.0),
                || ECMAScriptValue::from(3.0),
                BinOp::BitwiseOr
                => Ok(NormalCompletion::from(3)); "bitwise or")]
    #[test_case(|| ECMAScriptValue::from(2.0),
                || ECMAScriptValue::from(3.0),
                BinOp::BitwiseXor
                => Ok(NormalCompletion::from(1)); "bitwise xor")]
    #[test_case(|| ECMAScriptValue::from(BigInt::from(2)),
                || ECMAScriptValue::from(BigInt::from(3)),
                BinOp::Exponentiate
                => Ok(NormalCompletion::from(BigInt::from(8))); "exponentiation (bigint)")]
    #[test_case(|| ECMAScriptValue::from(BigInt::from(2)),
                || ECMAScriptValue::from(BigInt::from(-3)),
                BinOp::Exponentiate
                => serr("RangeError: Exponent must be positive"); "bad exponentiation (bigint)")]
    #[test_case(|| ECMAScriptValue::from(BigInt::from(2)),
                || ECMAScriptValue::from(BigInt::from(3)),
                BinOp::Multiply
                => Ok(NormalCompletion::from(BigInt::from(6))); "multiplication (bigint)")]
    #[test_case(|| ECMAScriptValue::from(BigInt::from(12)),
                || ECMAScriptValue::from(BigInt::from(3)),
                BinOp::Divide
                => Ok(NormalCompletion::from(BigInt::from(4))); "division (bigint)")]
    #[test_case(|| ECMAScriptValue::from(BigInt::from(12)),
                || ECMAScriptValue::from(BigInt::from(0)),
                BinOp::Divide
                =>serr("RangeError: Division by zero"); "zero division (bigint)")]
    #[test_case(|| ECMAScriptValue::from(BigInt::from(26)),
                || ECMAScriptValue::from(BigInt::from(7)),
                BinOp::Remainder
                => Ok(NormalCompletion::from(BigInt::from(5))); "remainder (bigint)")]
    #[test_case(|| ECMAScriptValue::from(BigInt::from(12)),
                || ECMAScriptValue::from(BigInt::from(0)),
                BinOp::Remainder
                =>serr("RangeError: Division by zero"); "zero remainder (bigint)")]
    #[test_case(|| ECMAScriptValue::from(BigInt::from(2)),
                || ECMAScriptValue::from(BigInt::from(3)),
                BinOp::Add
                => Ok(NormalCompletion::from(BigInt::from(5))); "addition (bigint)")]
    #[test_case(|| ECMAScriptValue::from(BigInt::from(2)),
                || ECMAScriptValue::from(BigInt::from(3)),
                BinOp::Subtract
                => Ok(NormalCompletion::from(BigInt::from(-1))); "subtraction (bigint)")]
    #[test_case(|| ECMAScriptValue::from(BigInt::from(2)),
                || ECMAScriptValue::from(BigInt::from(3)),
                BinOp::LeftShift
                => Ok(NormalCompletion::from(BigInt::from(16))); "left shift (bigint)")]
    #[test_case(|| ECMAScriptValue::from(BigInt::from(2)),
                || ECMAScriptValue::from(BigInt::from_str("689674891678594267895287496789").unwrap()),
                BinOp::LeftShift
                => serr("RangeError: out of range conversion regarding big integer attempted"); "left shift (bigint) (too big)")]
    #[test_case(|| ECMAScriptValue::from(BigInt::from(16)),
                || ECMAScriptValue::from(BigInt::from(3)),
                BinOp::SignedRightShift
                => Ok(NormalCompletion::from(BigInt::from(2))); "signed right shift (bigint)")]
    #[test_case(|| ECMAScriptValue::from(BigInt::from(16)),
                || ECMAScriptValue::from(BigInt::from_str("-3267891568973452345").unwrap()),
                BinOp::SignedRightShift
                => serr("RangeError: out of range conversion regarding big integer attempted"); "signed right shift (bigint) (too big)")]
    #[test_case(|| ECMAScriptValue::from(BigInt::from(8)),
                || ECMAScriptValue::from(BigInt::from(3)),
                BinOp::UnsignedRightShift
                => serr("TypeError: BigInts have no unsigned right shift, use >> instead"); "unsigned right shift (bigint)")]
    #[test_case(|| ECMAScriptValue::from(BigInt::from(2)),
                || ECMAScriptValue::from(BigInt::from(3)),
                BinOp::BitwiseAnd
                => Ok(NormalCompletion::from(ECMAScriptValue::from(BigInt::from(2)))); "bitwise and (bigint)")]
    #[test_case(|| ECMAScriptValue::from(BigInt::from(2)),
                || ECMAScriptValue::from(BigInt::from(3)),
                BinOp::BitwiseOr
                => Ok(NormalCompletion::from(ECMAScriptValue::from(BigInt::from(3)))); "bitwise or (bigint)")]
    #[test_case(|| ECMAScriptValue::from(BigInt::from(2)),
                || ECMAScriptValue::from(BigInt::from(3)),
                BinOp::BitwiseXor
                => Ok(NormalCompletion::from(ECMAScriptValue::from(BigInt::from(1)))); "bitwise xor (bigint)")]
    #[test_case(|| ECMAScriptValue::from(BigInt::from(12)),
                || ECMAScriptValue::from(3),
                BinOp::Remainder
                =>serr("TypeError: Cannot mix BigInt and other types, use explicit conversions"); "bigint type mix (left)")]
    #[test_case(|| ECMAScriptValue::from(12),
                || ECMAScriptValue::from(BigInt::from(1)),
                BinOp::Remainder
                =>serr("TypeError: Cannot mix BigInt and other types, use explicit conversions"); "bigint type mix (right)")]
    fn apply_string_or_numeric_binary_operator(
        make_left: fn() -> ECMAScriptValue,
        make_right: fn() -> ECMAScriptValue,
        op: BinOp,
    ) -> Result<NormalCompletion, String> {
        setup_test_agent();
        let left = make_left();
        let right = make_right();
        super::apply_string_or_numeric_binary_operator(left, right, op).map_err(unwind_any_error)
    }

    #[test_case(WksId::AsyncIterator => "Symbol.asyncIterator"; "Symbol.asyncIterator")]
    #[test_case(WksId::HasInstance => "Symbol.hasInstance"; "Symbol.hasInstance")]
    #[test_case(WksId::IsConcatSpreadable => "Symbol.isConcatSpreadable"; "Symbol.isConcatSpreadable")]
    #[test_case(WksId::Iterator => "Symbol.iterator"; "Symbol.iterator")]
    #[test_case(WksId::Match => "Symbol.match"; "Symbol.match")]
    #[test_case(WksId::MatchAll => "Symbol.matchAll"; "Symbol.matchAll")]
    #[test_case(WksId::Replace => "Symbol.replace"; "Symbol.replace")]
    #[test_case(WksId::Search => "Symbol.search"; "Symbol.search")]
    #[test_case(WksId::Species => "Symbol.species"; "Symbol.species")]
    #[test_case(WksId::Split => "Symbol.split"; "Symbol.split")]
    #[test_case(WksId::ToPrimitive => "Symbol.toPrimitive"; "Symbol.toPrimitive")]
    #[test_case(WksId::ToStringTag => "Symbol.toStringTag"; "Symbol.toStringTag")]
    #[test_case(WksId::Unscopables => "Symbol.unscopables"; "Symbol.unscopables")]
    fn wks(id: WksId) -> String {
        setup_test_agent();
        String::from(super::wks(id).description().unwrap())
    }

    fn no_primitive_val() -> ECMAScriptValue {
        make_test_obj_uncallable().into()
    }
    fn make_symbol() -> ECMAScriptValue {
        Symbol::new(None).into()
    }
    #[test_case(no_primitive_val, || ECMAScriptValue::from(10), true => serr("TypeError: Cannot convert object to primitive value"); "lf:true; first errs")]
    #[test_case(|| ECMAScriptValue::from(10), no_primitive_val, true => serr("TypeError: Cannot convert object to primitive value"); "lf:true; second errs")]
    #[test_case(no_primitive_val, || ECMAScriptValue::from(10), false => serr("TypeError: Cannot convert object to primitive value"); "lf:false; second errs")]
    #[test_case(|| ECMAScriptValue::from(10), no_primitive_val, false => serr("TypeError: Cannot convert object to primitive value"); "lf:false; first errs")]
    #[test_case(|| ECMAScriptValue::from("first"), || ECMAScriptValue::from("second"), true => Ok(Some(true)); "two strings; left first; true")]
    #[test_case(|| ECMAScriptValue::from("zfirst"), || ECMAScriptValue::from("second"), true => Ok(Some(false)); "two strings; left first; false")]
    #[test_case(|| ECMAScriptValue::from("10"), || ECMAScriptValue::from(BigInt::from(100)), true => Ok(Some(true)); "left string; right bigint; true")]
    #[test_case(|| ECMAScriptValue::from("1000"), || ECMAScriptValue::from(BigInt::from(100)), true => Ok(Some(false)); "left string; right bigint; false")]
    #[test_case(|| ECMAScriptValue::from("anchor"), || ECMAScriptValue::from(BigInt::from(100)), true => Ok(None); "left string; right bigint; none")]
    #[test_case(|| ECMAScriptValue::from(BigInt::from(100)), || ECMAScriptValue::from("10"), true => Ok(Some(false)); "left bigint; right string; false")]
    #[test_case(|| ECMAScriptValue::from(BigInt::from(100)), || ECMAScriptValue::from("1000"), true => Ok(Some(true)); "left bigint; right string; true")]
    #[test_case(|| ECMAScriptValue::from(BigInt::from(100)), || ECMAScriptValue::from("anchor"), true => Ok(None); "left bigint; right string; none")]
    #[test_case(make_symbol, || ECMAScriptValue::Undefined, true => serr("TypeError: Symbol values cannot be converted to Number values"); "left symbol")]
    #[test_case(|| ECMAScriptValue::Undefined, make_symbol, true => serr("TypeError: Symbol values cannot be converted to Number values"); "right symbol")]
    #[test_case(|| ECMAScriptValue::from(0), || ECMAScriptValue::Undefined, true => Ok(None); "right NaN")]
    #[test_case(|| ECMAScriptValue::Undefined, || ECMAScriptValue::from(0), true => Ok(None); "left NaN")]
    #[test_case(|| ECMAScriptValue::from(100), || ECMAScriptValue::from(0), false => Ok(Some(false)); "numbers: left bigger")]
    #[test_case(|| ECMAScriptValue::from(100), || ECMAScriptValue::from(7880), true => Ok(Some(true)); "numbers: left smaller")]
    #[test_case(|| ECMAScriptValue::Undefined, || ECMAScriptValue::from(BigInt::from(10)), true => Ok(None); "left NaN vs Bigint")]
    #[test_case(|| ECMAScriptValue::from(f64::NEG_INFINITY), || ECMAScriptValue::from(BigInt::from(10)), true => Ok(Some(true)); "left neg inf vs Bigint")]
    #[test_case(|| ECMAScriptValue::from(f64::INFINITY), || ECMAScriptValue::from(BigInt::from(10)), true => Ok(Some(false)); "left pos inf vs Bigint")]
    #[test_case(|| ECMAScriptValue::from(2.3e87), || ECMAScriptValue::from(BigInt::from(388)), true => Ok(Some(false)); "left big vs BigInt")]
    #[test_case(|| ECMAScriptValue::from(-2.3e87), || ECMAScriptValue::from(BigInt::from(388)), true => Ok(Some(true)); "left small vs BigInt")]
    #[test_case(|| ECMAScriptValue::from(BigInt::from(10)), || ECMAScriptValue::Undefined, true => Ok(None); "right NaN vs Bigint")]
    #[test_case(|| ECMAScriptValue::from(BigInt::from(10)), || ECMAScriptValue::from(f64::NEG_INFINITY), true => Ok(Some(false)); "right neg inf vs Bigint")]
    #[test_case(|| ECMAScriptValue::from(BigInt::from(10)), || ECMAScriptValue::from(f64::INFINITY), true => Ok(Some(true)); "right pos inf vs Bigint")]
    #[test_case(|| ECMAScriptValue::from(BigInt::from(388)), || ECMAScriptValue::from(2.3e87), true => Ok(Some(true)); "right big vs BigInt")]
    #[test_case(|| ECMAScriptValue::from(BigInt::from(388)), || ECMAScriptValue::from(-2.3e87), true => Ok(Some(false)); "right small vs BigInt")]
    #[test_case(|| ECMAScriptValue::from(BigInt::from(100)), || ECMAScriptValue::from(BigInt::from(7880)), true => Ok(Some(true)); "bigints: left smaller")]
    #[test_case(|| ECMAScriptValue::from(BigInt::from(100_999)), || ECMAScriptValue::from(BigInt::from(7880)), true => Ok(Some(false)); "bigints: right smaller")]
    fn is_less_than(
        make_x: fn() -> ECMAScriptValue,
        make_y: fn() -> ECMAScriptValue,
        left_first: bool,
    ) -> Result<Option<bool>, String> {
        setup_test_agent();
        let x = make_x();
        let y = make_y();
        x.is_less_than(&y, left_first).map_err(unwind_any_error)
    }

    mod create_unmapped_arguments_object {
        use super::*;
        use test_case::test_case;

        #[test_case(&["first".into(), "second".into(), 38.into(), true.into()]; "4 args")]
        #[test_case(&[]; "no args")]
        #[test_case(&[88.into()]; "one arg")]
        fn normal(values: &[ECMAScriptValue]) {
            setup_test_agent();
            let num_values = values.len();
            AGENT.with(|agent| {
                let index = agent.execution_context_stack.borrow().len() - 1;
                {
                    let top_ec = &mut agent.execution_context_stack.borrow_mut()[index];
                    let stack = &mut top_ec.stack;
                    for value in values {
                        stack.push(Ok(value.clone().into()));
                    }
                    stack.push(Ok(num_values.into()));
                }
            });

            super::insn_impl::create_unmapped_arguments_object().unwrap();

            let ao = AGENT.with(|agent| {
                let ec_ref = agent.execution_context_stack.borrow();
                let stack = &ec_ref.last().unwrap().stack;
                let stack_size = stack.len();

                // Assert arg vector is still in the right spot
                assert_eq!(stack[stack_size - 2].as_ref().unwrap(), &NormalCompletion::from(num_values));
                for (idx, val) in values.iter().enumerate() {
                    assert_eq!(
                        stack[stack_size - 2 - num_values + idx].as_ref().unwrap(),
                        &NormalCompletion::from(val.clone())
                    );
                }

                // Validate the arguments object.
                Object::try_from(ECMAScriptValue::try_from(stack[stack_size - 1].as_ref().unwrap().clone()).unwrap())
                    .unwrap()
            });
            assert_eq!(ao.get(&"length".into()).unwrap(), ECMAScriptValue::from(num_values));
            for (idx, val) in values.iter().enumerate() {
                assert_eq!(&ao.get(&idx.into()).unwrap(), val);
            }
            let args_iterator = intrinsic(IntrinsicId::ArrayPrototypeValues);
            let type_error_generator = intrinsic(IntrinsicId::ThrowTypeError);
            let iterator_sym = super::super::wks(WksId::Iterator);
            assert_eq!(ao.get(&iterator_sym.into()).unwrap(), ECMAScriptValue::from(args_iterator));
            let callee = ao.o.get_own_property(&"callee".into()).unwrap().unwrap();
            assert_eq!(
                callee.property,
                PropertyKind::Accessor(AccessorProperty {
                    get: ECMAScriptValue::from(type_error_generator.clone()),
                    set: ECMAScriptValue::from(type_error_generator)
                })
            );
            assert_eq!(callee.enumerable, false);
            assert_eq!(callee.configurable, false);

            assert!(ao.o.to_arguments_object().is_some());
        }

        #[test]
        fn panics_empty() {
            setup_test_agent();
            assert_eq!(
                super::insn_impl::create_unmapped_arguments_object().unwrap_err().to_string(),
                "runtime stack is empty"
            );
        }

        #[test]
        fn panics_short() {
            setup_test_agent();
            AGENT.with(|agent| {
                let index = agent.execution_context_stack.borrow().len() - 1;
                {
                    let top_ec = &mut agent.execution_context_stack.borrow_mut()[index];
                    let stack = &mut top_ec.stack;
                    stack.push(Ok(NormalCompletion::from(800)));
                }
            });
            let err = super::insn_impl::create_unmapped_arguments_object().unwrap_err().to_string();
            assert_eq!(err, "runtime stack is empty");
        }
    }

    mod create_mapped_arguments_object {
        use super::*;
        use test_case::test_case;

        #[test_case(&[], &[]; "empty")]
        #[test_case(&["x", "y"], &[10.into(), 20.into()]; "multiple")]
        fn normal(names: &[&str], values: &[ECMAScriptValue]) {
            setup_test_agent();
            let env = current_realm_record().unwrap().borrow().global_env.clone().unwrap();
            let lexenv = Rc::new(DeclarativeEnvironmentRecord::new(Some(env), "create_mapped_arguments_object test"))
                as Rc<dyn EnvironmentRecord>;
            super::set_lexical_environment(Some(lexenv.clone()));

            let func_obj = ordinary_object_create(None);

            for (idx, name) in names.iter().enumerate() {
                let value = values.get(idx).cloned().unwrap_or(ECMAScriptValue::Undefined);
                lexenv.set_mutable_binding(JSString::from(*name), value, false).unwrap();
            }

            let num_values = values.len();
            let index = AGENT.with(|agent| {
                let index = agent.execution_context_stack.borrow().len() - 1;
                let top_ec = &mut agent.execution_context_stack.borrow_mut()[index];
                let stack = &mut top_ec.stack;
                stack.push(Ok(func_obj.clone().into()));
                for value in values {
                    stack.push(Ok(value.clone().into()));
                }
                stack.push(Ok(num_values.into()));
                index
            });

            super::insn_impl::create_mapped_arguments_object().unwrap();

            let ao = AGENT.with(|agent| {
                let stack = &agent.execution_context_stack.borrow()[index].stack;
                let stack_size = stack.len();

                // Assert arg vector is still in the right spot
                assert_eq!(stack[stack_size - 2].as_ref().unwrap(), &NormalCompletion::from(num_values));
                for (idx, val) in values.iter().enumerate() {
                    assert_eq!(
                        stack[stack_size - 2 - num_values + idx].as_ref().unwrap(),
                        &NormalCompletion::from(val.clone())
                    );
                }
                assert_eq!(
                    stack[stack_size - 3 - values.len()].as_ref().unwrap(),
                    &NormalCompletion::from(func_obj.clone())
                );

                // Validate the arguments object.

                Object::try_from(ECMAScriptValue::try_from(stack[stack_size - 1].as_ref().unwrap().clone()).unwrap())
                    .unwrap()
            });
            assert_eq!(ao.get(&"length".into()).unwrap(), ECMAScriptValue::from(num_values));
            for (idx, val) in values.iter().enumerate() {
                assert_eq!(&ao.get(&idx.into()).unwrap(), val);
            }
            let args_iterator = intrinsic(IntrinsicId::ArrayPrototypeValues);
            let iterator_sym = super::super::wks(WksId::Iterator);
            assert_eq!(ao.get(&iterator_sym.into()).unwrap(), ECMAScriptValue::from(args_iterator));
            assert_eq!(ao.get(&"callee".into()).unwrap(), ECMAScriptValue::from(func_obj));
        }

        #[test]
        fn panics_empty() {
            setup_test_agent();
            let err = super::insn_impl::create_mapped_arguments_object().unwrap_err().to_string();
            assert_eq!(err, "runtime stack is empty");
        }

        #[test]
        fn panics_short() {
            setup_test_agent();
            AGENT.with(|agent| {
                let index = agent.execution_context_stack.borrow().len() - 1;
                {
                    let top_ec = &mut agent.execution_context_stack.borrow_mut()[index];
                    let stack = &mut top_ec.stack;
                    stack.push(Ok(NormalCompletion::from(800)));
                }
            });
            let err = super::insn_impl::create_mapped_arguments_object().unwrap_err().to_string();
            assert_eq!(err, "runtime stack is empty");
        }
    }
}

#[test]
fn prepare_for_execution() {
    setup_test_agent();
    let chunk = Rc::new(Chunk::new("test sentinel"));

    super::prepare_for_execution(0, Rc::clone(&chunk));

    AGENT.with(|agent| {
        assert_eq!(agent.execution_context_stack.borrow()[0].pc, 0);
        assert!(agent.execution_context_stack.borrow()[0].stack.is_empty());
        assert_eq!(agent.execution_context_stack.borrow()[0].chunk.as_ref().unwrap().name, "test sentinel");
    });
}

type ValueMaker = fn() -> ECMAScriptValue;
fn empty_object() -> ECMAScriptValue {
    let obj_proto = intrinsic(IntrinsicId::ObjectPrototype);
    ECMAScriptValue::from(ordinary_object_create(Some(obj_proto)))
}
fn bool_class() -> ECMAScriptValue {
    let boolean = intrinsic(IntrinsicId::Boolean);
    ECMAScriptValue::from(boolean)
}
fn undef() -> ECMAScriptValue {
    ECMAScriptValue::Undefined
}
fn number() -> ECMAScriptValue {
    ECMAScriptValue::from(10)
}
fn string() -> ECMAScriptValue {
    ECMAScriptValue::from("Test Sentinel")
}
fn dead_object() -> ECMAScriptValue {
    ECMAScriptValue::from(DeadObject::object())
}
fn test_has_instance(
    _: &ECMAScriptValue,
    _: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let mut args = FuncArgs::from(arguments);
    let thing_to_check = args.next_arg();
    // Let's say: all numbers are instances of our "class"
    // But that strings are a type error
    match thing_to_check {
        ECMAScriptValue::Number(_) => Ok(true.into()),
        ECMAScriptValue::String(s) => Err(create_type_error(s)),
        _ => Ok(false.into()),
    }
}
fn faux_class() -> ECMAScriptValue {
    let obj_proto = intrinsic(IntrinsicId::ObjectPrototype);
    let obj = ordinary_object_create(Some(obj_proto));
    let realm = current_realm_record();
    let function_prototype = intrinsic(IntrinsicId::FunctionPrototype);
    let has_instance = create_builtin_function(
        Box::new(test_has_instance),
        None,
        1_f64,
        PropertyKey::from("[Symbol.hasInstance]"),
        BUILTIN_FUNCTION_SLOTS,
        realm,
        Some(function_prototype),
        None,
    );
    let hi = super::wks(WksId::HasInstance);
    define_property_or_throw(
        &obj,
        hi,
        PotentialPropertyDescriptor::new().value(has_instance).writable(false).enumerable(false).configurable(true),
    )
    .unwrap();
    obj.into()
}

#[test_case(empty_object, undef => serr("TypeError: Right-hand side of 'instanceof' is not an object"); "class is not object")]
#[test_case(empty_object, dead_object => serr("TypeError: get called on DeadObject"); "GetMethod throws for class")]
#[test_case(empty_object, empty_object => serr("TypeError: Right-hand side of 'instanceof' is not callable"); "class is not callable")]
#[test_case(empty_object, bool_class => Ok(false); "defer to ordinary")]
#[test_case(empty_object, faux_class => Ok(false); "[Symbol.hasInstance] returns false")]
#[test_case(number, faux_class => Ok(true); "[Symbol.hasInstance] returns true")]
#[test_case(string, faux_class => serr("TypeError: Test Sentinel"); "[Symbol.hasInstance] throws")]
fn instanceof_operator(make_v: ValueMaker, make_target: ValueMaker) -> Result<bool, String> {
    setup_test_agent();
    let v = make_v();
    let target = make_target();

    super::instanceof_operator(v, &target).map_err(unwind_any_error)
}

#[test]
fn wksid_debug() {
    assert_ne!(format!("{:?}", WksId::ToStringTag), "");
}
#[test]
fn wksid_eq() {
    let w1 = WksId::Match;
    let w2 = WksId::Search;
    let w3 = WksId::Match;

    assert_eq!(w1 == w2, false);
    assert_eq!(w1 == w3, true);
    assert_eq!(w2 == w3, false);
}
#[test]
#[expect(clippy::clone_on_copy)]
fn wksid_clone() {
    let w1 = WksId::ToPrimitive;
    let w2 = w1.clone();

    assert_eq!(w1, w2);
}

mod well_known_symbols {
    use super::*;

    #[test]
    fn debug() {
        setup_test_agent();
        let s = AGENT.with(|agent| format!("{:?}", agent.symbols));
        assert_ne!(s, "");
    }
}

mod parse_script {
    use super::*;
    use test_case::test_case;

    #[test]
    fn happy() {
        setup_test_agent();
        let src = "/* hello! */ 'hello world';";
        let starting_realm = current_realm_record().unwrap();
        let ScriptRecord { realm, ecmascript_code, compiled, text } =
            super::parse_script(src, starting_realm.clone()).unwrap();
        assert!(Rc::ptr_eq(&realm, &starting_realm));
        assert_eq!(format!("{ecmascript_code}"), "'hello world' ;");
        assert_eq!(compiled.name, "top level script");
        assert_eq!(
            compiled.disassemble().iter().map(String::as_str).filter_map(disasm_filt).collect::<Vec<_>>(),
            svec(&["STRING 0 (hello world)",])
        );
        assert_eq!(text, src);
    }

    #[test_case("for [i=0, i<10, i++] {}" => sset(&["1:5: ‘(’ expected"]); "parse time syntax")]
    #[test_case("break lbl;" => sset(&["undefined break target detected"]); "early error syntax")]
    fn parse_error(src: &str) -> AHashSet<String> {
        setup_test_agent();
        let starting_realm = current_realm_record().unwrap();
        let errs = super::parse_script(src, starting_realm).unwrap_err();
        errs.iter().map(|err| unwind_syntax_error_object(&err.clone())).collect()
    }
}

mod top_level_lex_decl {
    use super::*;
    use test_case::test_case;

    type MakerResult = (Option<Rc<ClassDeclaration>>, Option<Rc<LexicalDeclaration>>, DeclPart);

    fn make_class_decl() -> MakerResult {
        let cd = Maker::new("class alice {}").class_declaration();
        (Some(cd.clone()), None, DeclPart::ClassDeclaration(cd))
    }
    fn make_lex_decl() -> MakerResult {
        let ld = Maker::new("let alice = 999;").lexical_declaration();
        (None, Some(ld.clone()), DeclPart::LexicalDeclaration(ld))
    }
    fn make_func_decl() -> MakerResult {
        let fd = Maker::new("function alice(bob) { return charlie(bob); }").function_declaration();
        (None, None, DeclPart::FunctionDeclaration(fd))
    }
    #[test_case(make_class_decl => Ok((true, false)); "class")]
    #[test_case(make_lex_decl => Ok((false, true)); "lexical")]
    #[test_case(make_func_decl => serr("Not a top-level lexical decl"); "function")]
    fn try_from(maker: fn() -> MakerResult) -> Result<(bool, bool), String> {
        let (maybe_class_declaration, maybe_lexical_declaration, dp) = maker();
        TopLevelLexDecl::try_from(dp)
            .map(|tlld| match (tlld, maybe_class_declaration, maybe_lexical_declaration) {
                (TopLevelLexDecl::Class(cd1), Some(cd2), _) => (Rc::ptr_eq(&cd1, &cd2), false),
                (TopLevelLexDecl::Lex(ld1), _, Some(ld2)) => (false, Rc::ptr_eq(&ld1, &ld2)),
                _ => (false, false),
            })
            .map_err(|err| err.to_string())
    }
}

mod fcn_def {
    use super::*;
    use test_case::test_case;

    #[test]
    fn debug() {
        let fd = Maker::new("function alice(x) { return x * 2; }").function_declaration();
        assert_ne!(format!("{:?}", FcnDef::Function(fd)), "");
    }

    #[test]
    fn clone() {
        let fd = Maker::new("function alice(x) { return x * 2; }").function_declaration();
        let fd1 = FcnDef::Function(fd);
        let fd2 = fd1.clone();

        match (fd1, fd2) {
            (FcnDef::Function(fd1), FcnDef::Function(fd2)) => {
                assert!(Rc::ptr_eq(&fd1, &fd2));
            }
            _ => unreachable!(),
        }
    }

    type MakerResult = (
        Option<Rc<FunctionDeclaration>>,
        Option<Rc<GeneratorDeclaration>>,
        Option<Rc<AsyncFunctionDeclaration>>,
        Option<Rc<AsyncGeneratorDeclaration>>,
        VarScopeDecl,
    );

    fn make_func_decl() -> MakerResult {
        let fd = Maker::new("function alice(bob) { return charlie(bob); }").function_declaration();
        (Some(fd.clone()), None, None, None, VarScopeDecl::FunctionDeclaration(fd))
    }
    fn make_gen_decl() -> MakerResult {
        let gd = Maker::new("function *alice(bob) { return charlie(bob); }").generator_declaration();
        (None, Some(gd.clone()), None, None, VarScopeDecl::GeneratorDeclaration(gd))
    }
    fn make_async_decl() -> MakerResult {
        let afd = Maker::new("async function alice(bob) { return charlie(bob); }").async_function_declaration();
        (None, None, Some(afd.clone()), None, VarScopeDecl::AsyncFunctionDeclaration(afd))
    }
    fn make_async_gen_decl() -> MakerResult {
        let agd = Maker::new("async function *alice(bob) { return charlie(bob); }").async_generator_declaration();
        (None, None, None, Some(agd.clone()), VarScopeDecl::AsyncGeneratorDeclaration(agd))
    }
    fn make_var_decl() -> MakerResult {
        let vd = Maker::new("alice").variable_declaration();
        (None, None, None, None, VarScopeDecl::VariableDeclaration(vd))
    }
    fn make_for_binding() -> MakerResult {
        let fb = Maker::new("alice").for_binding();
        (None, None, None, None, VarScopeDecl::ForBinding(fb))
    }
    #[test_case(make_func_decl => Ok(true); "Function decl")]
    #[test_case(make_gen_decl => Ok(true); "Generator decl")]
    #[test_case(make_async_decl => Ok(true); "Async Function decl")]
    #[test_case(make_async_gen_decl => Ok(true); "Async Generator decl")]
    #[test_case(make_var_decl => serr("Not a function def"); "Var decl")]
    #[test_case(make_for_binding => serr("Not a function def"); "for binding")]
    fn try_from(maker: fn() -> MakerResult) -> Result<bool, String> {
        let (opt_func_decl, opt_gen_decl, opt_async_func_decl, opt_async_gen_decl, vsd) = maker();
        FcnDef::try_from(vsd)
            .map(|fd| match (fd, opt_func_decl, opt_gen_decl, opt_async_func_decl, opt_async_gen_decl) {
                (FcnDef::Function(fd1), Some(fd2), _, _, _) => Rc::ptr_eq(&fd1, &fd2),
                (FcnDef::Generator(gd1), _, Some(gd2), _, _) => Rc::ptr_eq(&gd1, &gd2),
                (FcnDef::AsyncFun(afd1), _, _, Some(afd2), _) => Rc::ptr_eq(&afd1, &afd2),
                (FcnDef::AsyncGen(agd1), _, _, _, Some(agd2)) => Rc::ptr_eq(&agd1, &agd2),
                _ => false,
            })
            .map_err(|err| err.to_string())
    }

    type MakerDeclResult = (
        Option<Rc<FunctionDeclaration>>,
        Option<Rc<GeneratorDeclaration>>,
        Option<Rc<AsyncFunctionDeclaration>>,
        Option<Rc<AsyncGeneratorDeclaration>>,
        DeclPart,
    );
    fn decl_func() -> MakerDeclResult {
        let fd = Maker::new("function alice(bob) { return charlie(bob); }").function_declaration();
        (Some(fd.clone()), None, None, None, DeclPart::FunctionDeclaration(fd))
    }
    fn decl_gen() -> MakerDeclResult {
        let gd = Maker::new("function *alice(bob) { return charlie(bob); }").generator_declaration();
        (None, Some(gd.clone()), None, None, DeclPart::GeneratorDeclaration(gd))
    }
    fn decl_async() -> MakerDeclResult {
        let afd = Maker::new("async function alice(bob) { return charlie(bob); }").async_function_declaration();
        (None, None, Some(afd.clone()), None, DeclPart::AsyncFunctionDeclaration(afd))
    }
    fn decl_async_gen() -> MakerDeclResult {
        let agd = Maker::new("async function *alice(bob) { return charlie(bob); }").async_generator_declaration();
        (None, None, None, Some(agd.clone()), DeclPart::AsyncGeneratorDeclaration(agd))
    }
    fn decl_class() -> MakerDeclResult {
        let cls = Maker::new("class alice {}").class_declaration();
        (None, None, None, None, DeclPart::ClassDeclaration(cls))
    }
    fn decl_lex() -> MakerDeclResult {
        let ld = Maker::new("let alice;").lexical_declaration();
        (None, None, None, None, DeclPart::LexicalDeclaration(ld))
    }
    #[test_case(decl_func => Ok(true); "Function decl")]
    #[test_case(decl_gen => Ok(true); "Generator decl")]
    #[test_case(decl_async => Ok(true); "Async Function decl")]
    #[test_case(decl_async_gen => Ok(true); "Async Generator decl")]
    #[test_case(decl_class => serr("Not a function def"); "Class decl")]
    #[test_case(decl_lex => serr("Not a function def"); "Lex decl")]
    fn try_from_declpart(maker: fn() -> MakerDeclResult) -> Result<bool, String> {
        let (opt_func_decl, opt_gen_decl, opt_async_func_decl, opt_async_gen_decl, dp) = maker();
        FcnDef::try_from(dp)
            .map(|fd| match (fd, opt_func_decl, opt_gen_decl, opt_async_func_decl, opt_async_gen_decl) {
                (FcnDef::Function(fd1), Some(fd2), _, _, _) => Rc::ptr_eq(&fd1, &fd2),
                (FcnDef::Generator(gd1), _, Some(gd2), _, _) => Rc::ptr_eq(&gd1, &gd2),
                (FcnDef::AsyncFun(afd1), _, _, Some(afd2), _) => Rc::ptr_eq(&afd1, &afd2),
                (FcnDef::AsyncGen(agd1), _, _, _, Some(agd2)) => Rc::ptr_eq(&agd1, &agd2),
                _ => false,
            })
            .map_err(|err| err.to_string())
    }

    #[test_case(&FcnDef::Function(Maker::new("function fcn(){}").function_declaration()) => "fcn"; "function decl")]
    #[test_case(&FcnDef::Generator(Maker::new("function *fcn(){}").generator_declaration()) => "fcn"; "generator decl")]
    #[test_case(&FcnDef::AsyncFun(Maker::new("async function fcn(){}").async_function_declaration()) => "fcn"; "async function decl")]
    #[test_case(&FcnDef::AsyncGen(Maker::new("async function *fcn(){}").async_generator_declaration()) => "fcn"; "async generator decl")]
    fn bound_name(part: &FcnDef) -> String {
        part.bound_name().to_string()
    }

    #[test_case({ let src = "function red(){}"; (FcnDef::Function(Maker::new(src).function_declaration()), src.into()) } => sok("red"); "function decl")]
    #[test_case({ let src = "function *blue(){}"; (FcnDef::Generator(Maker::new(src).generator_declaration()), src.into()) } => sok("blue"); "generator decl")]
    #[test_case({ let src = "async function green(){}"; (FcnDef::AsyncFun(Maker::new(src).async_function_declaration()), src.into()) } => panics "not yet implemented"; "async function decl")]
    #[test_case({ let src = "async function *orange(){}"; (FcnDef::AsyncGen(Maker::new(src).async_generator_declaration()), src.into()) } => panics "not yet implemented"; "async generator decl")]
    fn instantiate_function_object(part: (FcnDef, String)) -> Result<String, String> {
        let (part, src) = part;
        setup_test_agent();
        let global_env = current_realm_record().unwrap().borrow().global_env.clone().unwrap();
        let env = global_env as Rc<dyn EnvironmentRecord>;

        part.instantiate_function_object(env, None, true, &src)
            .map_err(|err| err.to_string())
            .map(|value| value.get(&"name".into()).unwrap().to_string())
    }
}

mod top_level_var_decl {
    use super::*;
    use test_case::test_case;

    type MakerResult = (Option<Rc<VariableDeclaration>>, Option<Rc<ForBinding>>, VarScopeDecl);
    fn make_func_decl() -> MakerResult {
        let fd = Maker::new("function alice(bob) { return charlie(bob); }").function_declaration();
        (None, None, VarScopeDecl::FunctionDeclaration(fd))
    }
    fn make_gen_decl() -> MakerResult {
        let gd = Maker::new("function *alice(bob) { return charlie(bob); }").generator_declaration();
        (None, None, VarScopeDecl::GeneratorDeclaration(gd))
    }
    fn make_async_decl() -> MakerResult {
        let afd = Maker::new("async function alice(bob) { return charlie(bob); }").async_function_declaration();
        (None, None, VarScopeDecl::AsyncFunctionDeclaration(afd))
    }
    fn make_async_gen_decl() -> MakerResult {
        let agd = Maker::new("async function *alice(bob) { return charlie(bob); }").async_generator_declaration();
        (None, None, VarScopeDecl::AsyncGeneratorDeclaration(agd))
    }
    fn make_var_decl() -> MakerResult {
        let vd = Maker::new("alice").variable_declaration();
        (Some(vd.clone()), None, VarScopeDecl::VariableDeclaration(vd))
    }
    fn make_for_binding() -> MakerResult {
        let fb = Maker::new("alice").for_binding();
        (None, Some(fb.clone()), VarScopeDecl::ForBinding(fb))
    }

    #[test_case(make_func_decl => serr("FunctionDeclaration seen when top-level var decl expected"); "Function decl")]
    #[test_case(make_gen_decl => serr("GeneratorDeclaration seen when top-level var decl expected"); "Generator decl")]
    #[test_case(make_async_decl => serr("AsyncFunctionDeclaration seen when top-level var decl expected"); "Async Function decl")]
    #[test_case(make_async_gen_decl => serr("AsyncGeneratorDeclaration seen when top-level var decl expected"); "Async Generator decl")]
    #[test_case(make_var_decl => Ok(true); "Var decl")]
    #[test_case(make_for_binding => Ok(true); "for binding")]
    fn try_from(maker: fn() -> MakerResult) -> Result<bool, String> {
        let (maybe_vd, maybe_fb, vsd) = maker();
        TopLevelVarDecl::try_from(vsd)
            .map(|tlvd| match (tlvd, maybe_vd, maybe_fb) {
                (TopLevelVarDecl::VarDecl(vd1), Some(vd2), _) => Rc::ptr_eq(&vd1, &vd2),
                (TopLevelVarDecl::ForBinding(fb1), _, Some(fb2)) => Rc::ptr_eq(&fb1, &fb2),
                _ => false,
            })
            .map_err(|err| err.to_string())
    }
}

mod global_declaration_instantiation {
    use super::*;
    use test_case::test_case;

    #[test_case("var a" => Ok((sset(&["a"]), sset(&[]))); "one simple var-declared variable")]
    #[test_case("let already_var_declared;" => serr("SyntaxError: already_var_declared: already defined"); "existing var decl")]
    #[test_case("let existing_mutable;" => serr("SyntaxError: existing_mutable: already defined"); "existing lex decl")]
    #[test_case("let undefined;" => serr("SyntaxError: undefined is restricted and may not be used"); "restricted global")]
    #[test_case("var existing_mutable;" => serr("SyntaxError: existing_mutable: already defined"); "var dups lex")]
    #[test_case("function undefined(){}" => serr("TypeError: Cannot create global function undefined"); "function named undefined")]
    #[test_case("var a; let b; const c=0; for (var item in object) {}" => Ok((sset(&["a", "item"]), sset(&["b", "c"]))); "many")]
    #[test_case("class bob{}" => Ok((sset(&[]), sset(&["bob"]))); "a class")]
    #[test_case("function f(){}" => Ok((sset(&["f"]), sset(&[]))); "functions")]
    #[test_case("function *g(){}" => Ok((sset(&["g"]), sset(&[]))); "generators")]
    #[test_case("async function af(){}" => panics "not yet implemented"; "async functions")]
    #[test_case("async function *ag(){}" => panics "not yet implemented"; "async generators")]
    fn global_declaration_instantiation(src: &str) -> Result<(AHashSet<String>, AHashSet<String>), String> {
        setup_test_agent();
        let script = Maker::new(src).script();
        let global_env = current_realm_record().unwrap().borrow().global_env.clone().unwrap();
        global_env.create_global_var_binding("already_var_declared".into(), false).unwrap();
        global_env.create_mutable_binding("existing_mutable".into(), false).unwrap();

        let prior_vardecl = global_env.var_decls().into_iter().collect::<AHashSet<_>>();
        let prior_lexdecl = global_env.lex_decls().into_iter().collect::<AHashSet<_>>();

        let result = super::global_declaration_instantiation(&script, &global_env.clone(), false, src);

        result.map_err(unwind_any_error).map(|()| {
            let after_vardecl = global_env.var_decls().into_iter().collect::<AHashSet<_>>();
            let after_lexdecl = global_env.lex_decls().into_iter().collect::<AHashSet<_>>();

            let new_vardecl =
                after_vardecl.difference(&prior_vardecl).map(ToString::to_string).collect::<AHashSet<_>>();
            let new_lexdecl =
                after_lexdecl.difference(&prior_lexdecl).map(ToString::to_string).collect::<AHashSet<_>>();
            (new_vardecl, new_lexdecl)
        })
    }
}

mod script_evaluation {
    use super::*;
    use test_case::test_case;

    #[test_case("a=10;" => Ok(ECMAScriptValue::from(10.0)); "typical")]
    #[test_case("undeclared" => serr("ReferenceError: Unresolvable Reference"); "invalid")]
    #[test_case("" => Ok(ECMAScriptValue::Undefined); "empty")]
    fn script_evaluation(src: &str) -> Result<ECMAScriptValue, String> {
        setup_test_agent();
        let realm = current_realm_record().unwrap();
        let script_record = parse_script(src, realm).unwrap();

        super::script_evaluation(script_record).map_err(unwind_any_error)
    }
}

mod process_error {
    use super::*;
    use test_case::test_case;

    #[test]
    fn debug() {
        let s = format!("{:?}", ProcessError::InternalError { reason: "random reason".into() });
        assert_ne!(s, "");
    }

    fn internal_err() -> ProcessError {
        ProcessError::InternalError { reason: "blue".into() }
    }

    fn runtime_err_obj() -> ProcessError {
        let err = create_type_error_object("test sentinel");
        ProcessError::RuntimeError { error: err.into() }
    }
    fn runtime_err_value() -> ProcessError {
        let error = "test sentinel".into();
        ProcessError::RuntimeError { error }
    }
    fn runtime_err_non_err_obj() -> ProcessError {
        let error = ordinary_object_create(None).into();
        ProcessError::RuntimeError { error }
    }
    #[expect(clippy::needless_pass_by_value)]
    fn matches_object(s: String) {
        static MATCH: LazyLock<Regex> = LazyLock::new(|| Regex::new("^Thrown: <Object [0-9]+>$").expect("Valid regex"));
        assert!(MATCH.is_match(&s));
    }
    fn compiler_objs() -> ProcessError {
        ProcessError::CompileErrors {
            values: vec![
                create_syntax_error_object("Trouble in Paradise", None),
                create_reference_error_object("yeah, compiler errs are only syntax..."),
            ],
        }
    }
    #[test_case(internal_err => "blue"; "internal error")]
    #[test_case(runtime_err_obj => "Thrown: TypeError: test sentinel"; "error obj runtime")]
    #[test_case(runtime_err_value => "Thrown: test sentinel"; "error value runtime")]
    #[test_case(runtime_err_non_err_obj => using matches_object; "error obj but not error")]
    #[test_case(compiler_objs => "During compilation: [SyntaxError: Trouble in Paradise], [ReferenceError: yeah, compiler errs are only syntax...]"; "compiler err list")]
    fn display(make_error: fn() -> ProcessError) -> String {
        setup_test_agent();
        let err = make_error();
        format!("{err}")
    }

    #[test]
    fn display_err() {
        setup_test_agent();
        let err = compiler_objs();
        display_error_validate(err);
    }
}

mod process_ecmascript {
    use super::*;
    use test_case::test_case;

    #[test_case("1;" => Ok(ECMAScriptValue::from(1)); "normal result")]
    #[test_case("void" => serr("During compilation: [SyntaxError: 1:5: UnaryExpression expected]"); "syntax error")]
    #[test_case("a;" => serr("Thrown: ReferenceError: Unresolvable Reference"); "runtime error")]
    fn process_ecmascript(src: &str) -> Result<ECMAScriptValue, String> {
        setup_test_agent();
        let result = super::process_ecmascript(src);
        result.map_err(|e| format!("{e}"))
    }
}

#[test_case(&BigInt::from(10), &BigInt::from(-2) => Ok(BigInt::from(2)); "negative shift amt")]
#[test_case(&BigInt::from(10), &BigInt::from(4) => Ok(BigInt::from(160)); "positive shift amt")]
#[test_case(&BigInt::from(10), &BigInt::from(0xFFFF_FFFF_FFFF_u64) => serr("out of range conversion regarding big integer attempted"); "overflow")]
fn bigint_leftshift(left: &BigInt, right: &BigInt) -> Result<BigInt, String> {
    super::bigint_leftshift(left, right).map_err(|e| e.to_string())
}

#[test_case(&BigInt::from(10), &BigInt::from(-2) => Ok(BigInt::from(40)); "negative shift amt")]
#[test_case(&BigInt::from(10), &BigInt::from(2) => Ok(BigInt::from(2)); "positive shift amt")]
#[test_case(&BigInt::from(10), &BigInt::from(0xFF_FFFF_FFFF_u64) => Ok(BigInt::from(0)); "underflow")]
#[test_case(&BigInt::from(10), &BigInt::from(-0xFF_FFFF_FFFF_i64) => serr("out of range conversion regarding big integer attempted"); "overflow")]
fn bigint_rightshift(left: &BigInt, right: &BigInt) -> Result<BigInt, String> {
    super::bigint_rightshift(left, right).map_err(|e| e.to_string())
}

mod current_script_or_module {
    use super::*;

    #[test]
    fn empty() {
        setup_test_agent();
        let som = current_script_or_module();
        // Without any code, that should be None.
        assert!(som.is_none());
    }
    #[test]
    fn script() {
        setup_test_agent();
        // Add a script record; make sure it comes back...
        let realm_ref = current_realm_record().unwrap();
        let sr = ScriptRecord {
            realm: realm_ref.clone(),
            ecmascript_code: Maker::new("").script(),
            compiled: Rc::new(Chunk::new("test")),
            text: String::new(),
        };
        let test_ec = ExecutionContext::new(None, realm_ref, Some(ScriptOrModule::Script(Rc::new(sr))));
        push_execution_context(test_ec);

        let som = current_script_or_module();
        assert!(matches!(som, Some(ScriptOrModule::Script(_))));
    }
}

#[expect(clippy::type_complexity)]
mod create_per_iteration_environment {
    use super::*;
    use test_case::test_case;

    // So this function takes an existing DER and a set of names, makes a new DER with those names and the
    // values from the previous DER (which becomes the new DER's outer).
    //
    // Testing this means:
    //  * Set up an environment with some set of bindings.
    //  * Call the function with names
    //  * Validate that the environment coming back (the new "current lexical environment")
    //    * has the previous environment as its outer
    //    * that none of that outer's bindings has changed
    //    * has bindings according to the input names, with the same values as the outer
    //    * has no other bindings

    fn s(s: &str) -> String {
        s.to_string()
    }

    #[test_case(AHashSet::new
        => Ok((
            s("prior_env"),
            vec![(s("a"), s("alpha")), (s("b"), s("beta")), (s("c"), s("gamma")), (s("uninitialized"), s("error on get"))],
            s("outer_env"),
            vec![],
            s("prior_env"),
            vec![(s("a"), s("alpha")), (s("b"), s("beta")), (s("c"), s("gamma")), (s("uninitialized"), s("error on get"))],
        ))
        ; "empty")]
    #[test_case(|| {
            let mut set = AHashSet::new();
            set.insert(JSString::from("a"));
            set.insert(JSString::from("b"));
            set
        }
        => Ok((
            s("per-iter"),
            vec![(s("a"), s("alpha")), (s("b"), s("beta"))],
            s("outer_env"),
            vec![],
            s("prior_env"),
            vec![(s("a"), s("alpha")), (s("b"), s("beta")), (s("c"), s("gamma")), (s("uninitialized"), s("error on get"))],
        ))
        ; "reasonable")]
    #[test_case(|| {
            let mut set = AHashSet::new();
            set.insert(JSString::from("a"));
            set.insert(JSString::from("uninitialized"));
            set
        }
        => serr("ReferenceError: Binding not initialized")
        ; "asking for unitialized binding")]
    fn call(
        make_strings: impl FnOnce() -> AHashSet<JSString>,
    ) -> Result<(String, Vec<(String, String)>, String, Vec<(String, String)>, String, Vec<(String, String)>), String>
    {
        // value on output:
        // (name-of-env, Vec of (name, value) for env, name-of-outer, Vec of (name, value) for outer, name-of-prior, Vec of (name, value) for prior)
        // all vecs are sorted for test stability.
        setup_test_agent();
        let outer = DeclarativeEnvironmentRecord::new(None, "outer_env");
        let prior = DeclarativeEnvironmentRecord::new(Some(Rc::new(outer)), "prior_env");
        prior.create_mutable_binding("a".into(), true).unwrap();
        prior.initialize_binding(&"a".into(), "alpha".into()).unwrap();
        prior.create_mutable_binding("b".into(), true).unwrap();
        prior.initialize_binding(&"b".into(), "beta".into()).unwrap();
        prior.create_mutable_binding("c".into(), true).unwrap();
        prior.initialize_binding(&"c".into(), "gamma".into()).unwrap();
        prior.create_mutable_binding("uninitialized".into(), true).unwrap();
        let prior: Rc<dyn EnvironmentRecord> = Rc::new(prior);
        let prior_copy = prior.clone();
        AGENT.with(|agent| {
            let mut ec_stack = agent.execution_context_stack.borrow_mut();
            let running_execution_context = ec_stack.last_mut().unwrap();
            running_execution_context.lexical_environment = Some(prior_copy);
        });
        let strings = make_strings();

        create_per_iteration_environment(&strings).map_err(unwind_any_error)?;

        let current_lexical_env = AGENT.with(|agent| {
            let ec_stack = agent.execution_context_stack.borrow();
            let running_execution_context = ec_stack.last().unwrap();
            running_execution_context.lexical_environment.as_ref().unwrap().clone()
        });

        // (name-of-env, Vec of (name, value) for env, name-of-outer, Vec of (name, value) for outer)
        let name_of_env = current_lexical_env.name();
        let mut bindings = current_lexical_env
            .binding_names()
            .into_iter()
            .map(|name| {
                let value = String::from(
                    JSString::try_from(
                        current_lexical_env
                            .get_binding_value(&name, true)
                            .unwrap_or_else(|_| ECMAScriptValue::from("error on get")),
                    )
                    .unwrap(),
                );
                (String::from(name), value)
            })
            .collect::<Vec<_>>();
        bindings.sort_unstable();

        let outer = current_lexical_env.get_outer_env().clone();
        let outer_name = outer.as_ref().map_or_else(String::new, |e| e.name());
        let mut outer_bindings = outer.map_or_else(Vec::new, |e| {
            e.binding_names()
                .into_iter()
                .map(|name| {
                    let value = String::from(
                        JSString::try_from(
                            e.get_binding_value(&name, true).unwrap_or_else(|_| ECMAScriptValue::from("error on get")),
                        )
                        .unwrap(),
                    );
                    (String::from(name), value)
                })
                .collect::<Vec<_>>()
        });
        outer_bindings.sort_unstable();

        let prior_name = prior.name();
        let mut prior_bindings = prior
            .binding_names()
            .into_iter()
            .map(|name| {
                let value = String::from(
                    JSString::try_from(
                        prior.get_binding_value(&name, true).unwrap_or_else(|_| ECMAScriptValue::from("error on get")),
                    )
                    .unwrap(),
                );
                (String::from(name), value)
            })
            .collect::<Vec<_>>();
        prior_bindings.sort_unstable();

        Ok((name_of_env, bindings, outer_name, outer_bindings, prior_name, prior_bindings))
    }
}

mod ec_pop_list {
    use super::*;
    use test_case::test_case;

    #[test_case(|| () => serr("no execution context"); "no context")]
    #[test_case(setup_test_agent => serr("empty application stack"); "empty stack")]
    #[test_case(|| { setup_test_agent(); ec_push(Ok(0.into()))} => Ok(svec(&[])); "zero-length")]
    #[test_case(|| { setup_test_agent(); ec_push(Err(create_type_error("sentinel")))} => serr("Unexpected abrupt completion"); "top of stack error")]
    #[test_case(|| { setup_test_agent(); ec_push(Ok(NormalCompletion::Empty))} => serr("Not a language value!"); "not-language-value")]
    #[test_case(|| { setup_test_agent(); ec_push(Ok("abcd".into()))} => serr("Value not an f64"); "not a number")]
    #[test_case(|| { setup_test_agent(); ec_push(Ok(10.into()))} => serr("empty application stack"); "stack crash")]
    #[test_case(|| { setup_test_agent(); ec_push(Err(create_type_error("sentinel"))); ec_push(Ok(1.into())); } => serr("Unexpected abrupt completion:"); "err in values")]
    #[test_case(|| { setup_test_agent(); ec_push(Ok(NormalCompletion::Empty)); ec_push(Ok(1.into())); } => serr("Not a language value!"); "not-value in list")]
    #[test_case(|| { setup_test_agent(); ec_push(Ok("abcd".into())); ec_push(Ok("efgh".into())); ec_push(Ok("ijkl".into())); ec_push(Ok(3.into())); } => Ok(svec(&["ijkl", "efgh", "abcd"])); "3-item list")]
    fn call(setup: fn() -> ()) -> Result<Vec<String>, String> {
        setup();

        ec_pop_list()
            .as_ref()
            .map(|v| v.iter().map(ToString::to_string).collect::<Vec<_>>())
            .map_err(ToString::to_string)
    }
}

mod begin_call_evaluation {
    use super::*;
    use test_case::test_case;

    #[expect(clippy::unnecessary_wraps)]
    fn test_reporter(
        this_value: &ECMAScriptValue,
        _new_target: Option<&Object>,
        arguments: &[ECMAScriptValue],
    ) -> Completion<ECMAScriptValue> {
        let my_this = match this_value {
            ECMAScriptValue::Object(o) => o.get(&"marker".into()).unwrap(),
            _ => this_value.clone(),
        };
        let this_repr = String::from(to_string(my_this).unwrap());
        let args_repr = arguments.iter().cloned().map(|val| String::from(to_string(val).unwrap())).join(",");
        let total_repr = format!("this: {this_repr}; args: {args_repr}");
        Ok(ECMAScriptValue::from(total_repr))
    }

    fn good_func() -> ECMAScriptValue {
        ECMAScriptValue::from(create_builtin_function(
            Box::new(test_reporter),
            None,
            2.0,
            "test_reporter".into(),
            &[],
            None,
            None,
            None,
        ))
    }

    fn object_this() -> NormalCompletion {
        let obj = ordinary_object_create(None);
        obj.set("marker", "legitimate this", true).unwrap();
        let base = ECMAScriptValue::from(ordinary_object_create(None));
        let r = Reference::new(Base::Value(base), "callee", true, Some(ECMAScriptValue::from(obj)));
        NormalCompletion::from(r)
    }

    fn undef_this() -> NormalCompletion {
        true.into()
    }

    fn unresolvable() -> NormalCompletion {
        Reference::new(Base::Unresolvable, "callee", true, None).into()
    }

    fn with_env() -> NormalCompletion {
        let objproto = intrinsic(IntrinsicId::ObjectPrototype);
        let obj = ordinary_object_create(Some(objproto));
        obj.create_data_property_or_throw("marker", "with-object").unwrap();
        let we = ObjectEnvironmentRecord::new(obj, true, None, "with_env test");
        Reference::new(Base::Environment(Rc::new(we)), "test_report", true, None).into()
    }

    #[test_case(|| ECMAScriptValue::Undefined, || NormalCompletion::Empty => Err(InternalRuntimeError::ValueOrReferenceExpected); "empty reference")]
    #[test_case(good_func, object_this => Ok(Some("\"this: legitimate this; args: sentinel\"".into())); "this is object")]
    #[test_case(good_func, undef_this => Ok(Some("\"this: undefined; args: sentinel\"".into())); "this is undefined")]
    #[test_case(good_func, unresolvable => Err(InternalRuntimeError::UnresolvableReference); "unresolvable reference")]
    #[test_case(|| ECMAScriptValue::Undefined, object_this => Ok(Some("TypeError: not an object".into())); "non-object in function spot")]
    #[test_case(|| intrinsic(IntrinsicId::ObjectPrototype).into(), object_this => Ok(Some("TypeError: not a function".into())); "non-function object in function spot")]
    #[test_case(good_func, with_env => Ok(Some("\"this: with-object; args: sentinel\"".into())); "with-object")]
    fn call(
        make_func: fn() -> ECMAScriptValue,
        make_this: fn() -> NormalCompletion,
    ) -> Result<Option<String>, InternalRuntimeError> {
        setup_test_agent();
        let func = make_func();
        let reference = make_this();
        begin_call_evaluation(&func, &reference, &[ECMAScriptValue::from("sentinel")])?;

        Ok(ec_pop().map(|v| v.map_err(unwind_any_error).map_or_else(|err| err, |nc| format!("{nc}"))))
    }
}

mod for_in_iterator_object {
    use super::*;

    #[test]
    fn to_for_in_iterator() {
        setup_test_agent();
        let proto = intrinsic(IntrinsicId::ForInIteratorPrototype);
        let a = array_create(0.0, None).unwrap();
        let o = ForInIteratorObject::object(Some(proto), a);

        assert_eq!(o.o.to_for_in_iterator().unwrap().id(), o.o.id());
    }

    #[test]
    fn debug() {
        setup_test_agent();
        let proto = intrinsic(IntrinsicId::ForInIteratorPrototype);
        let a = array_create(0.0, None).unwrap();
        let o = ForInIteratorObject::object(Some(proto), a);
        assert_ne!(format!("{o:?}"), "");
    }

    fn make() -> Object {
        let proto = intrinsic(IntrinsicId::ForInIteratorPrototype);
        let a = array_create(0.0, None);
        let o = ForInIteratorObject::object(Some(proto), a.unwrap());
        let proto = o.o.get_prototype_of().unwrap().unwrap();
        proto.set("proto_sentinel", true, true).unwrap();
        o
    }

    default_uses_ordinary_get_prototype_of_test!();
    default_get_prototype_of_test!(ForInIteratorPrototype);
    default_set_prototype_of_test!();
    default_is_extensible_test!();
    default_prevent_extensions_test!();
    default_get_own_property_test!();
    default_define_own_property_test!();
    default_has_property_test!();
    default_set_test!();
    default_get_test!(|| PropertyKey::from("proto_sentinel"), ECMAScriptValue::from(true));
    default_delete_test!();
    default_own_property_keys_test!();
    default_id_test!();
    false_function!(is_array_object);
    false_function!(is_bigint_object);
    false_function!(is_callable_obj);
    false_function!(is_date_object);
    false_function!(is_generator_object);
    false_function!(is_plain_object);
    false_function!(is_proxy_object);
    false_function!(is_regexp_object);
    false_function!(is_string_object);
    false_function!(is_symbol_object);
    none_function!(to_arguments_object);
    none_function!(to_array_object);
    none_function!(to_bigint_object);
    none_function!(to_boolean_obj);
    none_function!(to_builtin_function_obj);
    none_function!(to_callable_obj);
    none_function!(to_constructable);
    none_function!(to_function_obj);
    none_function!(to_generator_object);
    none_function!(to_number_obj);
    none_function!(to_proxy_object);
    none_function!(to_string_obj);
    none_function!(to_symbol_obj);
}

mod for_in_iterator_internals {
    use super::*;

    #[test]
    fn debug() {
        setup_test_agent();
        let obj = ordinary_object_create(None);
        let item = ForInIteratorInternals {
            object: obj,
            object_was_visited: false,
            visited_keys: vec![],
            remaining_keys: vec![],
        };
        assert_ne!(format!("{item:?}"), "");
    }
}

#[test]
fn create_for_in_iterator() {
    setup_test_agent();
    let proto = intrinsic(IntrinsicId::ObjectPrototype);
    let obj = ordinary_object_create(Some(proto));

    let fii = super::create_for_in_iterator(obj.clone());
    let fii_obj = fii.o.to_for_in_iterator().expect("object should be f-i iterator");
    let internals = fii_obj.internals.borrow();

    assert_eq!(internals.object, obj);
    assert!(!internals.object_was_visited);
    assert!(internals.visited_keys.is_empty());
    assert!(internals.remaining_keys.is_empty());
}

#[test]
fn provision_for_in_iterator_prototype() {
    setup_test_agent();

    let fiip = intrinsic(IntrinsicId::ForInIteratorPrototype);
    let iterator_proto = intrinsic(IntrinsicId::IteratorPrototype);

    assert_eq!(fiip.o.get_prototype_of().unwrap().unwrap(), iterator_proto);
    func_validation(fiip.o.get_own_property(&"next".into()).unwrap().unwrap(), "next", 0);
}

mod for_in_iterator_prototype_next {
    use super::*;
    use test_case::test_case;

    fn make_busy_object() -> ECMAScriptValue {
        let object_proto = intrinsic(IntrinsicId::ObjectPrototype);
        object_proto.create_data_property_or_throw("proto_prop", 67).unwrap();
        object_proto.create_data_property_or_throw("masked", 0).unwrap();
        let object = ordinary_object_create(Some(object_proto));
        object.create_data_property_or_throw("on_object", 999).unwrap();
        object.create_data_property_or_throw(wks(WksId::ToStringTag), "TestObject").unwrap();
        object.create_data_property_or_throw("masked", 99).unwrap();
        crate::agent::create_for_in_iterator(object).into()
    }
    #[expect(clippy::unnecessary_wraps)]
    fn lying_ownprops(_: &AdaptableObject) -> Completion<Vec<PropertyKey>> {
        Ok(vec!["one".into(), "two".into(), "three".into()])
    }
    fn lyingkeys() -> ECMAScriptValue {
        let o = AdaptableObject::object(&AdaptableMethods {
            own_property_keys_override: Some(lying_ownprops),
            ..Default::default()
        });
        crate::agent::create_for_in_iterator(o).into()
    }
    fn gop_failure(this: &AdaptableObject, key: &PropertyKey) -> Completion<Option<PropertyDescriptor>> {
        if *key != "sequoia".into() || !to_boolean(this.get(&"primed".into(), &ECMAScriptValue::Undefined).unwrap()) {
            Ok(ordinary_get_own_property(this, key))
        } else {
            Err(create_type_error("[[GetOwnProperty]] fails"))
        }
    }
    fn get_own_property_failure() -> ECMAScriptValue {
        let o = AdaptableObject::object(&AdaptableMethods {
            get_own_property_override: Some(gop_failure),
            ..Default::default()
        });
        o.create_data_property_or_throw("sequoia", 0).unwrap();
        o.create_data_property_or_throw("primed", true).unwrap();
        crate::agent::create_for_in_iterator(o).into()
    }

    #[test_case(|| crate::agent::create_for_in_iterator(ordinary_object_create(None)).into() => Ok(vec![]); "empty object")]
    #[test_case(make_busy_object => Ok(vec!["on_object".into(), "masked".into(), "proto_prop".into()]); "busy object")]
    #[test_case(|| crate::agent::create_for_in_iterator(DeadObject::object()).into() => serr("TypeError: own_property_keys called on DeadObject"); "own_property_keys fails")]
    #[test_case(lyingkeys => Ok(vec![]); "own_property_keys lies")]
    #[test_case(get_own_property_failure => serr("TypeError: [[GetOwnProperty]] fails"); "get_own_property fails")]
    #[test_case(|| crate::agent::create_for_in_iterator(TestObject::object(&[FunctionId::GetPrototypeOf])).into() => serr("TypeError: [[GetPrototypeOf]] called on TestObject"); "getprototypeof fails")]
    fn call(make_this: fn() -> ECMAScriptValue) -> Result<Vec<ECMAScriptValue>, String> {
        setup_test_agent();
        let this = make_this();
        let mut result = vec![];
        loop {
            let ir = for_in_iterator_prototype_next(&this, None, &[]).map_err(unwind_any_error)?;
            let iro = Object::try_from(ir).unwrap();
            if iterator_complete(&iro).unwrap() {
                return Ok(result);
            }
            result.push(iterator_value(&iro).unwrap());
        }
    }
}

mod evaluate_initialized_class_field_definition {
    use super::*;

    #[test]
    fn call() {
        setup_test_agent();
        let env = current_realm_record().unwrap().borrow().global_env.clone().unwrap();
        let lexenv = Rc::new(DeclarativeEnvironmentRecord::new(Some(env), "create_mapped_arguments_object test"));
        set_lexical_environment(Some(lexenv as Rc<dyn EnvironmentRecord>));

        let src = "sum = 10+20";
        let fd = Maker::new(src).field_definition();
        let proto = intrinsic(IntrinsicId::ObjectPrototype);
        let home = ordinary_object_create(Some(proto));
        let name = Some(ClassName::from("class_name"));
        let info = StashedFunctionData {
            source_text: String::new(),
            params: Rc::new(FormalParameters::Empty(Location::default())).into(),
            body: fd.init.as_ref().unwrap().clone().into(),
            to_compile: fd.into(),
            strict: true,
            this_mode: ThisLexicality::NonLexicalThis,
        };

        let obj = evaluate_initialized_class_field_definition(&info, home.clone(), name, src).unwrap();

        let fdata = obj.o.to_function_obj().unwrap().function_data().borrow();
        assert_eq!(fdata.class_field_initializer_name, Some(ClassName::from("class_name")));
        assert_eq!(fdata.home_object.as_ref().unwrap().o.id(), home.o.id());
    }

    #[test]
    fn call_err() {
        setup_test_agent();
        let env = current_realm_record().unwrap().borrow().global_env.clone().unwrap();
        let lexenv = Rc::new(DeclarativeEnvironmentRecord::new(Some(env), "create_mapped_arguments_object test"));
        set_lexical_environment(Some(lexenv as Rc<dyn EnvironmentRecord>));

        let src = "sum = @@# + 10+20";
        let fd = Maker::new(src).field_definition();
        let proto = intrinsic(IntrinsicId::ObjectPrototype);
        let home = ordinary_object_create(Some(proto));
        let name = Some(ClassName::from("class_name"));
        let info = StashedFunctionData {
            source_text: String::new(),
            params: Rc::new(FormalParameters::Empty(Location::default())).into(),
            body: fd.init.as_ref().unwrap().clone().into(),
            to_compile: fd.into(),
            strict: true,
            this_mode: ThisLexicality::NonLexicalThis,
        };

        let res = evaluate_initialized_class_field_definition(&info, home.clone(), name, src);
        assert!(res.is_err());
    }
}

mod define_method_property {
    use super::*;
    use test_case::test_case;

    #[derive(Debug, PartialEq)]
    enum TestResult {
        PrivateLike { obj_desc: String, name: String, func: String },
        PropertyLike { obj_desc: String, writable: bool, enumerable: bool, configurable: bool },
    }

    fn ordinary() -> Object {
        ordinary_object_create(None)
    }

    #[test_case(
        ordinary, FunctionName::from(PropertyKey::from("bob")), || intrinsic(IntrinsicId::IsNaN), true
        => TestResult::PropertyLike {
            obj_desc: "bob:function isNaN".to_string(),
            writable: true,
            enumerable: true,
            configurable: true
        };
        "typical-enumerable"
    )]
    #[test_case(
        ordinary, FunctionName::from(PropertyKey::from("bob")), || intrinsic(IntrinsicId::IsNaN), false
        => TestResult::PropertyLike {
            obj_desc: "bob:function isNaN".to_string(),
            writable: true,
            enumerable: false,
            configurable: true
        };
        "typical-hidden"
    )]
    #[test_case(
        ordinary, FunctionName::from(PrivateName::new("private")), || intrinsic(IntrinsicId::IsNaN), true
        => TestResult::PrivateLike {
            obj_desc: String::new(),
            name: "private".to_string(),
            func: "length:1,name:isNaN".to_string()
        };
        "private"
    )]
    fn f(
        make_home_object: impl FnOnce() -> Object,
        key: FunctionName,
        make_closure: impl FnOnce() -> Object,
        enumerable: bool,
    ) -> TestResult {
        setup_test_agent();
        let home_object = make_home_object();
        let closure = make_closure();
        let opt_pe = define_method_property(&home_object, key.clone(), closure.clone(), enumerable).unwrap();
        match opt_pe {
            None => {
                let pk = PropertyKey::try_from(key).unwrap();
                let desc = DataDescriptor::try_from(home_object.o.get_own_property(&pk).unwrap().unwrap()).unwrap();
                TestResult::PropertyLike {
                    obj_desc: ECMAScriptValue::from(home_object).test_result_string(),
                    writable: desc.writable,
                    enumerable: desc.enumerable,
                    configurable: desc.configurable,
                }
            }
            Some(pe) => match pe {
                PrivateElement { key: PrivateName { description, .. }, kind: PrivateElementKind::Method { value } } => {
                    TestResult::PrivateLike {
                        obj_desc: ECMAScriptValue::from(home_object).test_result_string(),
                        name: String::from(description),
                        func: value.test_result_string(),
                    }
                }
                _ => {
                    panic!("Bad PE came back");
                }
            },
        }
    }
}

mod ec_peek {
    use super::*;
    use test_case::test_case;

    #[test_case(|| (), 1 => None; "stack shorter than lookback")]
    #[test_case(|| ec_push(Ok(NormalCompletion::from("blue"))), 0 => Some(sok("\"blue\"")); "successful peek")]
    #[test_case(
        || {
            ec_push(Ok(NormalCompletion::from("green")));
            ec_push(Ok(NormalCompletion::from("blue")));
        },
        1
        => Some(sok("\"green\""));
        "peek > 0"
    )]
    #[test_case(
        || AGENT.with(|agent| agent.execution_context_stack.borrow_mut().clear()),
        1
        => None;
        "No execution contexts"
    )]
    fn ec_peek(make_stack: impl FnOnce(), from_end: usize) -> Option<Result<String, String>> {
        setup_test_agent();
        make_stack();
        super::ec_peek(from_end).map(|item| item.map_err(unwind_any_error).map(|nc| nc.to_string()))
    }
}

#[test]
fn set_default_global_bindings() {
    setup_test_agent();
    // Coverage is easy here, all we need to is create the test agent. So let's validate, instead.

    let globj = get_global_object().unwrap();

    macro_rules! validate_obj_data_property {
        ( $name:expr_2021, $obj:expr_2021, $value:expr_2021, $writable:expr_2021, $enumerable:expr_2021, $configurable:expr_2021 ) => {
            let key = PropertyKey::from($name);
            let item = $obj.o.get_own_property(&key).unwrap();
            let expected = ECMAScriptValue::from($value);
            if let Some(PropertyDescriptor {
                property: PropertyKind::Data(DataProperty { value, writable: $writable }),
                enumerable: $enumerable,
                configurable: $configurable,
                spot: _,
            }) = item
            {
                assert!(
                    value.same_value(&expected),
                    "{}: Data value was incorrect: {} should have been {}",
                    $name,
                    value,
                    expected
                );
            } else {
                panic!(
                    "Incorrect property descriptor for {}:\ndesired: {:?}\nactual: {:?}",
                    $name,
                    PropertyDescriptor {
                        property: PropertyKind::Data(DataProperty { value: expected, writable: $writable }),
                        enumerable: $enumerable,
                        configurable: $configurable,
                        spot: 0
                    },
                    item
                );
            }
        };
    }

    macro_rules! validate_data_property {
        ( $name:expr_2021, $value:expr_2021, $writable:expr_2021, $enumerable:expr_2021, $configurable:expr_2021 ) => {
            validate_obj_data_property!($name, globj, $value, $writable, $enumerable, $configurable);
        };
    }

    let global_this = {
        let rc_realm = current_realm_record().unwrap();
        let realm_ref = rc_realm.borrow();
        realm_ref.global_env.as_ref().unwrap().get_this_binding().unwrap()
    };
    validate_data_property!("globalThis", global_this, true, false, true);
    validate_data_property!("Infinity", f64::INFINITY, false, false, false);
    validate_data_property!("NaN", f64::NAN, false, false, false);
    validate_data_property!("undefined", ECMAScriptValue::Undefined, false, false, false);

    macro_rules! validate_intrinsic_function {
        ( $name:expr_2021, $id:ident, $length:expr_2021 ) => {
            let value = intrinsic(IntrinsicId::$id);
            validate_data_property!($name, value.clone(), true, false, true);
            validate_obj_data_property!("name", value, $name, false, false, true);
            validate_obj_data_property!("length", value, $length, false, false, true);
        };
    }

    validate_intrinsic_function!("eval", Eval, 1);
    validate_intrinsic_function!("isFinite", IsFinite, 1);
    validate_intrinsic_function!("isNaN", IsNaN, 1);
    validate_intrinsic_function!("parseFloat", ParseFloat, 1);
    validate_intrinsic_function!("parseInt", ParseInt, 2);
    validate_intrinsic_function!("decodeURI", DecodeURI, 1);
    validate_intrinsic_function!("decodeURIComponent", DecodeURIComponent, 1);
    validate_intrinsic_function!("encodeURI", EncodeURI, 1);
    validate_intrinsic_function!("encodeURIComponent", EncodeURIComponent, 1);

    macro_rules! validate_intrinsic_constructor {
        ( $name:expr_2021, $id:ident, $length:expr_2021 ) => {
            validate_intrinsic_function!($name, $id, $length);
        };
    }

    //validate_intrinsic_constructor!("AggregateError", AggregateError, 2);
    validate_intrinsic_constructor!("Array", Array, 1);
    //validate_intrinsic_constructor!("ArrayBuffer", ArrayBuffer, 1);
    validate_intrinsic_constructor!("BigInt", BigInt, 1);
    //validate_intrinsic_constructor!("BigInt64Array", BigInt64Array, 3);
    //validate_intrinsic_constructor!("BigUint64Array", BigUint64Array, 3);
    validate_intrinsic_constructor!("Boolean", Boolean, 1);
    //validate_intrinsic_constructor!("DataView", DataView, 1);
    //validate_intrinsic_constructor!("Date", Date, 7);
    validate_intrinsic_constructor!("Error", Error, 1);
    validate_intrinsic_constructor!("EvalError", EvalError, 1);
    //validate_intrinsic_constructor!("FinalizationRegistry", FinalizationRegistry, 1);
    //validate_intrinsic_constructor!("Float32Array", Float32Array, 3);
    //validate_intrinsic_constructor!("Float64Array", Float64Array, 3);
    validate_intrinsic_constructor!("Function", Function, 1);
    //validate_intrinsic_constructor!("Int8Array", Int8Array, 3);
    //validate_intrinsic_constructor!("Int16Array", Int16Array, 3);
    //validate_intrinsic_constructor!("Int32Array", Int32Array, 3);
    //validate_intrinsic_constructor!("Map", Map, 0);
    validate_intrinsic_constructor!("Number", Number, 1);
    validate_intrinsic_constructor!("Object", Object, 1);
    //validate_intrinsic_constructor!("Promise", Promise, 1);
    validate_intrinsic_constructor!("Proxy", Proxy, 2);
    validate_intrinsic_constructor!("RangeError", RangeError, 1);
    validate_intrinsic_constructor!("ReferenceError", ReferenceError, 1);
    //validate_intrinsic_constructor!("RegExp", RegExp, 2);
    //validate_intrinsic_constructor!("Set", Set, 0);
    //validate_intrinsic_constructor!("SharedArrayBuffer", SharedArrayBuffer, 1);
    validate_intrinsic_constructor!("String", String, 1);
    validate_intrinsic_constructor!("Symbol", Symbol, 0);
    validate_intrinsic_constructor!("SyntaxError", SyntaxError, 1);
    validate_intrinsic_constructor!("TypeError", TypeError, 1);
    //validate_intrinsic_constructor!("Uint8Array", Uint8Array, 3);
    //validate_intrinsic_constructor!("Uint8ClampedArray", Uint8ClampedArray, 3);
    //validate_intrinsic_constructor!("Uint16Array", Uint16Array, 3);
    //validate_intrinsic_constructor!("Uint32Array", Uint32Array, 3);
    validate_intrinsic_constructor!("URIError", URIError, 1);
    //validate_intrinsic_constructor!("WeakMap", WeakMap, 0);
    //validate_intrinsic_constructor!("WeakRef", WeakRef, 1);
    //validate_intrinsic_constructor!("WeakSet", WeakSet, 0);

    macro_rules! validate_intrinsic_data {
        ( $name:expr_2021, $id:ident ) => {
            let value = intrinsic(IntrinsicId::$id);
            validate_data_property!($name, value, true, false, true);
        };
    }

    //validate_intrinsic_data!("Atomics", Atomics);
    //validate_intrinsic_data!("JSON", Json);
    validate_intrinsic_data!("Math", Math);
    validate_intrinsic_data!("Reflect", Reflect);
}

#[test_case(setup_test_agent => 1; "nothing happening yet")]
#[test_case(
    || {
        setup_test_agent();
        push_execution_context(ExecutionContext::new(None, current_realm_record().unwrap(), None));
    }
    => 2;
    "Something more on stack"
)]
fn execution_context_stack_len(setup: impl FnOnce()) -> usize {
    setup();
    super::execution_context_stack_len()
}
