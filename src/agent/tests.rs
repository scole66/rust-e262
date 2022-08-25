use super::*;
use crate::parser::testhelp::*;
use crate::tests::*;
use ahash::AHashSet;
use lazy_static::lazy_static;
use num::BigInt;
use regex::Regex;
use std::cell::RefCell;
use std::iter::zip;
use std::rc::Rc;
use std::str::FromStr;
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
        let symbols = vec![
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
        })
    }
    #[test]
    fn push_execution_context() {
        setup_test_agent();
        let realm_ref = current_realm_record().unwrap();
        let prior_length = agent.execution_context_stack.borrow().len();
        // build a new EC, and add it to the EC stack
        let sr = ScriptRecord {
            realm: realm_ref.clone(),
            ecmascript_code: Maker::new("").script(),
            compiled: Rc::new(Chunk::new("test")),
            text: String::new(),
        };
        let test_ec = ExecutionContext::new(None, realm_ref, Some(ScriptOrModule::Script(Rc::new(sr))));
        agent.push_execution_context(test_ec);

        assert_eq!(agent.execution_context_stack.borrow().len(), prior_length + 1);
        let r = &agent.execution_context_stack.borrow()[agent.execution_context_stack.borrow().len() - 1];
        assert!(r.script_or_module.is_some());
    }
    #[test]
    fn active_function_object() {
        let agent = Agent::new();
        // no Running Execution Context, so this should be None.
        let afo = agent.active_function_object();
        assert!(afo.is_none());

        agent.initialize_host_defined_realm(true);
        // Now there's an execution context, but still no active function, so this should still be None.
        let afo = agent.active_function_object();
        assert!(afo.is_none());

        // Create a new EC that _does_ have a function object; push it, and then check the active function.
        let fo = intrinsic(IntrinsicId::ThrowTypeError);
        let realm = current_realm_record().unwrap();
        let function_ec = ExecutionContext::new(Some(fo.clone()), realm, None);
        agent.push_execution_context(function_ec);

        let afo = agent.active_function_object().unwrap();
        assert_eq!(afo, fo);
    }
    #[test]
    fn next_object_id() {
        let agent = Agent::new();
        // Starts at something, and then increases monotonically.
        let first = agent.next_object_id();
        for x in 1..10 {
            assert_eq!(agent.next_object_id(), x + first);
        }
    }
    #[test]
    fn next_symbol_id() {
        let agent = Agent::new();
        // Starts at something, and then increases monotonically.
        let first = agent.next_symbol_id();
        for x in 1..10 {
            assert_eq!(agent.next_symbol_id(), x + first);
        }
    }
    #[test]
    fn debug() {
        assert_ne!(format!("{:?}", Agent::new()), "");
    }

    #[test]
    fn global_symbol_registry() {
        let registry = Rc::new(RefCell::new(SymbolRegistry::new()));
        let agent = Agent::new();
        agent.set_global_symbol_registry(registry.clone());
        let gsr = agent.global_symbol_registry();
        assert!(Rc::ptr_eq(&registry, &gsr));
    }

    mod current_realm_record {
        use super::*;

        #[test]
        fn empty_ec_stack() {
            let agent = Agent::new();
            assert!(current_realm_record().is_none());
        }

        #[test]
        fn stacked() {
            setup_test_agent();

            let first_realm = create_named_realm("first");
            let first_context = ExecutionContext::new(None, first_realm, None);
            a.push_execution_context(first_context);

            let second_realm = create_named_realm("second");
            let second_context = ExecutionContext::new(None, second_realm, None);
            a.push_execution_context(second_context);

            let current = current_realm_record().unwrap();
            assert_eq!(get_realm_name(&current.borrow()), "second");

            pop_execution_context();

            let current = current_realm_record().unwrap();
            assert_eq!(get_realm_name(&current.borrow()), "first");
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
        agent.void_operator(expr).map_err(|ac| unwind_any_error(ac))
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
        typeof_operator(expr).map_err(|ac| unwind_any_error(ac))
    }

    fn superproperty() -> FullCompletion {
        // For example: ({method() { delete super.test_property; }}).method()
        // 1. Let F be OrdinaryFunctionCreate(intrinsics.[[%FunctionPrototype%]], source_text, ParameterList, Body, thisMode, env, privateenv).
        // 2. Let homeObject be OrdinaryObjectCreate(intrinsics.[[%ObjectPrototype%]]).
        // 2. Call MakeMethod(F, homeObject).
        // 3. Let fenv be NewFunctionEnvironment(F, undefined).
        // 4. Let actualThis be fenv.GetThisBinding().
        // 5. Return MakeSuperPropertyReference(actualThis, "test_property", true)
        let obj = ordinary_object_create(None, &[]);
        let copy = obj.clone();
        let myref = Reference::new(Base::Value(obj.into()), "item", true, Some(copy.into()));
        Ok(NormalCompletion::from(myref))
    }

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
    fn dead_ref() -> FullCompletion {
        let dead = DeadObject::object();
        Ok(NormalCompletion::from(Reference::new(Base::Value(dead.into()), "anything", true, None)))
    }
    fn ref_to_undefined() -> FullCompletion {
        let env = current_realm_record().unwrap().borrow().global_env.clone().unwrap();
        Ok(NormalCompletion::from(Reference::new(Base::Environment(env), "undefined", true, None)))
    }
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
    fn delete_ref(make_expr: fn() -> FullCompletion) -> Result<NormalCompletion, String> {
        setup_test_agent();
        let expr = make_expr();
        delete_ref(expr).map_err(|ac| unwind_any_error(ac))
    }

    #[test_case(|| ECMAScriptValue::from("left "),
                || ECMAScriptValue::from("right"),
                BinOp::Add
                => Ok(NormalCompletion::from("left right")); "string catentation")]
    #[test_case(|agent| ECMAScriptValue::from(make_toprimitive_throw_obj(agent)),
                || ECMAScriptValue::from("a"),
                BinOp::Add
                => serr("TypeError: Test Sentinel"); "left toPrimitive error")]
    #[test_case(|| ECMAScriptValue::from("a"),
                |agent| ECMAScriptValue::from(make_toprimitive_throw_obj(agent)),
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
    #[test_case(|| ECMAScriptValue::from(wks(WksId::ToPrimitive)),
                || ECMAScriptValue::from("a"),
                BinOp::Add
                => serr("TypeError: Symbols may not be converted to strings"); "left tostring errs")]
    #[test_case(|| ECMAScriptValue::from("a"),
                || ECMAScriptValue::from(wks(WksId::ToPrimitive)),
                BinOp::Add
                => serr("TypeError: Symbols may not be converted to strings"); "right tostring errs")]
    #[test_case(|| ECMAScriptValue::from(wks(WksId::ToPrimitive)),
                || ECMAScriptValue::from(10),
                BinOp::Add
                => serr("TypeError: Symbol values cannot be converted to Number values"); "left tonumeric errs")]
    #[test_case(|| ECMAScriptValue::from(10),
                || ECMAScriptValue::from(wks(WksId::ToPrimitive)),
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
                => Ok(NormalCompletion::from(536870910)); "unsigned right shift (negative)")]
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
        make_lval: fn() -> ECMAScriptValue,
        make_rval: fn() -> ECMAScriptValue,
        op: BinOp,
    ) -> Result<NormalCompletion, String> {
        setup_test_agent();
        let lval = make_lval();
        let rval = make_rval();
        apply_string_or_numeric_binary_operator(lval, rval, op).map_err(|ac| unwind_any_error(ac))
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
        String::from(wks(id).description().unwrap())
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
        })
    }

    #[test]
    fn two_values() {
        setup_test_agent();
        let index = agent.execution_context_stack.borrow().len() - 1;
        agent.execution_context_stack.borrow_mut()[index].stack.push(Ok(NormalCompletion::from(ECMAScriptValue::Null)));
        agent.execution_context_stack.borrow_mut()[index]
            .stack
            .push(Ok(NormalCompletion::from(ECMAScriptValue::from("test"))));
        let (left, right) = agent.two_values(index);
        assert_eq!(left, ECMAScriptValue::Null);
        assert_eq!(right, ECMAScriptValue::from("test"));
    }

    fn no_primitive_val() -> ECMAScriptValue {
        make_test_obj_uncallable(agent).into()
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
    #[test_case(|| ECMAScriptValue::from(BigInt::from(100999)), || ECMAScriptValue::from(BigInt::from(7880)), true => Ok(Some(false)); "bigints: right smaller")]
    fn is_less_than(
        make_x: fn() -> ECMAScriptValue,
        make_y: fn() -> ECMAScriptValue,
        left_first: bool,
    ) -> Result<Option<bool>, String> {
        setup_test_agent();
        let x = make_x();
        let y = make_y();
        super::is_less_than(x, y, left_first).map_err(|completion| unwind_any_error(completion))
    }

    type ValueMaker = fn() -> ECMAScriptValue;
    fn empty_object() -> ECMAScriptValue {
        let obj_proto = intrinsic(IntrinsicId::ObjectPrototype);
        ECMAScriptValue::from(ordinary_object_create(Some(obj_proto), &[]))
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
        _: ECMAScriptValue,
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
        let obj = ordinary_object_create(Some(obj_proto), &[]);
        let realm = current_realm_record();
        let function_prototype = intrinsic(IntrinsicId::FunctionPrototype);
        let has_instance = create_builtin_function(
            test_has_instance,
            false,
            1_f64,
            PropertyKey::from("[Symbol.hasInstance]"),
            BUILTIN_FUNCTION_SLOTS,
            realm,
            Some(function_prototype),
            None,
        );
        let hi = wks(WksId::HasInstance);
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
    #[test_case(empty_object, bool_class => Ok(false.into()); "defer to ordinary")]
    #[test_case(empty_object, faux_class => Ok(false.into()); "[Symbol.hasInstance] returns false")]
    #[test_case(number, faux_class => Ok(true.into()); "[Symbol.hasInstance] returns true")]
    #[test_case(string, faux_class => serr("TypeError: Test Sentinel"); "[Symbol.hasInstance] throws")]
    fn instanceof_operator(make_v: ValueMaker, make_target: ValueMaker) -> Result<ECMAScriptValue, String> {
        setup_test_agent();
        let v = make_v();
        let target = make_target();

        super::instanceof_operator(v, target)
            .map_err(|completion| unwind_any_error(completion))
            .map(|nc| nc.try_into().unwrap())
    }

    mod create_unmapped_arguments_object {
        use super::*;
        use test_case::test_case;

        #[test_case(&["first".into(), "second".into(), 38.into(), true.into()]; "4 args")]
        #[test_case(&[]; "no args")]
        #[test_case(&[88.into()]; "one arg")]
        fn normal(values: &[ECMAScriptValue]) {
            setup_test_agent();
            let num_values = values.len() as u32;
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

            super::create_unmapped_arguments_object(index);

            let ao = AGENT.with(|agent| {
                let stack = &agent.execution_context_stack.borrow()[index].stack;
                let stack_size = stack.len();

                // Assert arg vector is still in the right spot
                assert_eq!(stack[stack_size - 2].as_ref().unwrap(), &NormalCompletion::from(num_values));
                for (idx, val) in values.iter().enumerate() {
                    assert_eq!(
                        stack[stack_size - 2 - num_values as usize + idx].as_ref().unwrap(),
                        &NormalCompletion::from(val.clone())
                    );
                }

                // Validate the arguments object.
                Object::try_from(ECMAScriptValue::try_from(stack[stack_size - 1].as_ref().unwrap().clone()).unwrap())
                    .unwrap()
            });
            assert_eq!(get(&ao, &"length".into()).unwrap(), ECMAScriptValue::from(num_values));
            for (idx, val) in values.iter().enumerate() {
                assert_eq!(&get(&ao, &idx.into()).unwrap(), val);
            }
            let args_iterator = intrinsic(IntrinsicId::ArrayPrototypeValues);
            let type_error_generator = intrinsic(IntrinsicId::ThrowTypeError);
            let iterator_sym = wks(WksId::Iterator);
            assert_eq!(get(&ao, &iterator_sym.into()).unwrap(), ECMAScriptValue::from(args_iterator));
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

            assert!(ao.o.is_arguments_object());
        }

        #[test]
        #[should_panic(expected = "Stack must not be empty")]
        fn panics_empty() {
            setup_test_agent();
            let index = agent.execution_context_stack.borrow().len() - 1;
            agent.create_unmapped_arguments_object(index);
        }

        #[test]
        #[should_panic(expected = "Stack too short to fit all the arguments")]
        fn panics_short() {
            setup_test_agent();
            let index = agent.execution_context_stack.borrow().len() - 1;
            {
                let top_ec = &mut agent.execution_context_stack.borrow_mut()[index];
                let stack = &mut top_ec.stack;
                stack.push(Ok(NormalCompletion::from(800)));
            }
            agent.create_unmapped_arguments_object(index);
        }
    }

    mod create_mapped_arguments_object {
        use super::*;
        use test_case::test_case;

        #[test_case(&[]; "empty")]
        #[test_case(&[10.into(), 20.into()]; "multiple")]
        fn normal(values: &[ECMAScriptValue]) {
            setup_test_agent();
            let env = current_realm_record().unwrap().borrow().global_env.clone().unwrap();
            let lexenv = Rc::new(DeclarativeEnvironmentRecord::new(Some(env), "create_mapped_arguments_object test"));
            super::set_lexical_environment(Some(lexenv as Rc<dyn EnvironmentRecord>));

            let func_obj = ordinary_object_create(None, &[]);

            let num_values = values.len() as u32;
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

            super::create_mapped_arguments_object(index);

            let ao = AGENT.with(|agent| {
                let stack = &agent.execution_context_stack.borrow()[index].stack;
                let stack_size = stack.len();

                // Assert arg vector is still in the right spot
                assert_eq!(stack[stack_size - 2].as_ref().unwrap(), &NormalCompletion::from(num_values));
                for (idx, val) in values.iter().enumerate() {
                    assert_eq!(
                        stack[stack_size - 2 - num_values as usize + idx].as_ref().unwrap(),
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
            assert_eq!(get(&ao, &"length".into()).unwrap(), ECMAScriptValue::from(num_values));
            for (idx, val) in values.iter().enumerate() {
                assert_eq!(&get(&ao, &idx.into()).unwrap(), val);
            }
            let args_iterator = intrinsic(IntrinsicId::ArrayPrototypeValues);
            let iterator_sym = wks(WksId::Iterator);
            assert_eq!(get(&ao, &iterator_sym.into()).unwrap(), ECMAScriptValue::from(args_iterator));
            assert_eq!(get(&ao, &"callee".into()).unwrap(), ECMAScriptValue::from(func_obj));
        }

        #[test]
        #[should_panic(expected = "Stack must not be empty")]
        fn panics_empty() {
            setup_test_agent();
            let index = agent.execution_context_stack.borrow().len() - 1;
            agent.create_mapped_arguments_object(index);
        }

        #[test]
        #[should_panic(expected = "Stack too short to fit all the arguments plus the function obj")]
        fn panics_short() {
            setup_test_agent();
            let index = agent.execution_context_stack.borrow().len() - 1;
            {
                let top_ec = &mut agent.execution_context_stack.borrow_mut()[index];
                let stack = &mut top_ec.stack;
                stack.push(Ok(NormalCompletion::from(800)));
            }
            agent.create_mapped_arguments_object(index);
        }
    }

    mod attach_mapped_arg {
        use super::*;
        use test_case::test_case;

        #[test_case(&[10.into(), "blue".into(), true.into()], &["number".into(), "string".into(), "boolean".into()]; "typical")]
        fn normal(values: &[ECMAScriptValue], names: &[JSString]) {
            setup_test_agent();
            let realm = current_realm_record().unwrap();
            let ge = realm.borrow().global_env.as_ref().unwrap().clone();
            let lex = Rc::new(DeclarativeEnvironmentRecord::new(Some(ge), "test lex"));
            let num_values = values.len() as u32;
            let index = agent.execution_context_stack.borrow().len() - 1;
            {
                let top_ec = &mut agent.execution_context_stack.borrow_mut()[index];
                top_ec.lexical_environment = Some(lex.clone());
                let stack = &mut top_ec.stack;
                stack.push(Ok(ECMAScriptValue::Null.into())); // faux function
                for value in values {
                    stack.push(Ok(value.clone().into()));
                }
                stack.push(Ok(num_values.into()));
            }
            agent.create_mapped_arguments_object(index);
            let ao = Object::try_from(
                ECMAScriptValue::try_from(
                    agent.execution_context_stack.borrow()[index].stack
                        [agent.execution_context_stack.borrow()[index].stack.len() - 1]
                        .clone()
                        .unwrap(),
                )
                .unwrap(),
            )
            .unwrap();

            for (idx, (name, value)) in zip(names, values).enumerate().rev() {
                lex.create_mutable_binding(name.clone(), false).unwrap();
                lex.initialize_binding(&agent, name, value.clone()).unwrap();
                agent.attach_mapped_arg(index, name, idx);
            }

            for (idx, (name, value)) in zip(names, values).enumerate() {
                let val = get(&agent, &ao, &idx.into()).unwrap();
                assert_eq!(&val, value);
                set(&agent, &ao, idx.into(), (idx as u32).into(), true).unwrap();
                let val = lex.get_binding_value(&agent, name, true).unwrap();
                assert_eq!(val, ECMAScriptValue::from(idx as u32));
            }
        }

        #[test]
        #[should_panic(expected = "stack must not be empty")]
        fn empty_stack() {
            setup_test_agent();
            let index = agent.execution_context_stack.borrow().len() - 1;
            agent.attach_mapped_arg(index, &"bbo".into(), 12);
        }
    }
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
#[allow(clippy::clone_on_copy)]
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
        let s = format!("{:?}", agent.0.symbols);
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
            super::parse_script(&agent, src, starting_realm.clone()).unwrap();
        assert!(Rc::ptr_eq(&realm, &starting_realm));
        assert_eq!(format!("{}", ecmascript_code), "'hello world' ;");
        assert_eq!(compiled.name, "top level script");
        assert_eq!(
            compiled.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
            svec(&["STRING 0 (hello world)",])
        );
        assert_eq!(text, src);
    }

    #[test_case("for [i=0, i<10, i++] {}" => sset(&["1:5: ‘(’ expected"]); "parse time syntax")]
    #[test_case("break lbl;" => sset(&["undefined break target detected"]); "early error syntax")]
    fn parse_error(src: &str) -> AHashSet<String> {
        setup_test_agent();
        let starting_realm = current_realm_record().unwrap();
        let errs = super::parse_script(&agent, src, starting_realm).unwrap_err();
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&agent, err.clone())))
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
        let (maybe_cd, maybe_ld, dp) = maker();
        TopLevelLexDecl::try_from(dp)
            .map(|tlld| match (tlld, maybe_cd, maybe_ld) {
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
        let (maybe_fd, maybe_gd, maybe_afd, maybe_agd, vsd) = maker();
        FcnDef::try_from(vsd)
            .map(|fd| match (fd, maybe_fd, maybe_gd, maybe_afd, maybe_agd) {
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
        let (maybe_fd, maybe_gd, maybe_afd, maybe_agd, dp) = maker();
        FcnDef::try_from(dp)
            .map(|fd| match (fd, maybe_fd, maybe_gd, maybe_afd, maybe_agd) {
                (FcnDef::Function(fd1), Some(fd2), _, _, _) => Rc::ptr_eq(&fd1, &fd2),
                (FcnDef::Generator(gd1), _, Some(gd2), _, _) => Rc::ptr_eq(&gd1, &gd2),
                (FcnDef::AsyncFun(afd1), _, _, Some(afd2), _) => Rc::ptr_eq(&afd1, &afd2),
                (FcnDef::AsyncGen(agd1), _, _, _, Some(agd2)) => Rc::ptr_eq(&agd1, &agd2),
                _ => false,
            })
            .map_err(|err| err.to_string())
    }

    #[test_case(FcnDef::Function(Maker::new("function fcn(){}").function_declaration()) => "fcn"; "function decl")]
    #[test_case(FcnDef::Generator(Maker::new("function *fcn(){}").generator_declaration()) => "fcn"; "generator decl")]
    #[test_case(FcnDef::AsyncFun(Maker::new("async function fcn(){}").async_function_declaration()) => "fcn"; "async function decl")]
    #[test_case(FcnDef::AsyncGen(Maker::new("async function *fcn(){}").async_generator_declaration()) => "fcn"; "async generator decl")]
    fn bound_name(part: FcnDef) -> String {
        part.bound_name().to_string()
    }

    #[test_case({ let src = "function red(){}"; (FcnDef::Function(Maker::new(src).function_declaration()), src.into()) } => sok("red"); "function decl")]
    #[test_case({ let src = "function *blue(){}"; (FcnDef::Generator(Maker::new(src).generator_declaration()), src.into()) } => panics "not yet implemented"; "generator decl")]
    #[test_case({ let src = "async function green(){}"; (FcnDef::AsyncFun(Maker::new(src).async_function_declaration()), src.into()) } => panics "not yet implemented"; "async function decl")]
    #[test_case({ let src = "async function *orange(){}"; (FcnDef::AsyncGen(Maker::new(src).async_generator_declaration()), src.into()) } => panics "not yet implemented"; "async generator decl")]
    fn instantiate_function_object(part: (FcnDef, String)) -> Result<String, String> {
        let (part, src) = part;
        setup_test_agent();
        let global_env = current_realm_record().unwrap().borrow().global_env.clone().unwrap();
        let env = global_env as Rc<dyn EnvironmentRecord>;

        part.instantiate_function_object(&agent, env, None, true, &src)
            .map_err(|err| err.to_string())
            .map(|value| getv(&agent, &value, &"name".into()).unwrap().to_string())
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
    #[test_case("function *g(){}" => panics "not yet implemented"; "generators")]
    #[test_case("async function af(){}" => panics "not yet implemented"; "async functions")]
    #[test_case("async function *ag(){}" => panics "not yet implemented"; "async generators")]
    fn global_declaration_instantiation(src: &str) -> Result<(AHashSet<String>, AHashSet<String>), String> {
        setup_test_agent();
        let script = Maker::new(src).script();
        let global_env = current_realm_record().unwrap().borrow().global_env.clone().unwrap();
        global_env.create_global_var_binding(&agent, "already_var_declared".into(), false).unwrap();
        global_env.create_mutable_binding(&agent, "existing_mutable".into(), false).unwrap();

        let prior_vardecl = global_env.var_decls().into_iter().collect::<AHashSet<_>>();
        let prior_lexdecl = global_env.lex_decls().into_iter().collect::<AHashSet<_>>();

        let result = super::global_declaration_instantiation(&agent, script, global_env.clone(), false, src);

        result.map_err(|err| unwind_any_error(err)).map(|| {
            let after_vardecl = global_env.var_decls().into_iter().collect::<AHashSet<_>>();
            let after_lexdecl = global_env.lex_decls().into_iter().collect::<AHashSet<_>>();

            let new_vardecl = after_vardecl.difference(&prior_vardecl).map(|s| s.to_string()).collect::<AHashSet<_>>();
            let new_lexdecl = after_lexdecl.difference(&prior_lexdecl).map(|s| s.to_string()).collect::<AHashSet<_>>();
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
        let script_record = parse_script(&agent, src, realm).unwrap();

        super::script_evaluation(&agent, script_record).map_err(|err| unwind_any_error(err))
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
        let err = create_type_error_object(agent, "test sentinel");
        ProcessError::RuntimeError { error: err.into() }
    }
    fn runtime_err_value() -> ProcessError {
        let error = "test sentinel".into();
        ProcessError::RuntimeError { error }
    }
    fn runtime_err_non_err_obj() -> ProcessError {
        let error = ordinary_object_create(agent, None, &[]).into();
        ProcessError::RuntimeError { error }
    }
    fn matches_object(s: String) {
        lazy_static! {
            static ref MATCH: Regex = Regex::new("^Thrown: <Object [0-9]+>$").expect("Valid regex");
        }
        assert!(MATCH.is_match(&s));
    }
    fn compiler_objs() -> ProcessError {
        ProcessError::CompileErrors {
            values: vec![
                create_syntax_error_object(agent, "Trouble in Paradise", None),
                create_reference_error_object(agent, "yeah, compiler errs are only syntax..."),
            ],
        }
    }
    #[test_case(internal_err => "blue"; "internal error")]
    #[test_case(runtime_err_obj => "Thrown: TypeError: test sentinel"; "error obj runtime")]
    #[test_case(runtime_err_value => "Thrown: test sentinel"; "error value runtime")]
    #[test_case(runtime_err_non_err_obj => using matches_object; "error obj but not error")]
    #[test_case(compiler_objs => "During compilation:\nSyntaxError: Trouble in Paradise\nReferenceError: yeah, compiler errs are only syntax...\n"; "compiler err list")]
    fn display(make_error: fn() -> ProcessError) -> String {
        setup_test_agent();
        let err = make_error(&agent);
        format!("{err}")
    }

    #[test]
    fn display_err() {
        setup_test_agent();
        let err = compiler_objs(&agent);
        display_error_validate(err);
    }
}

mod process_ecmascript {
    use super::*;
    use test_case::test_case;

    #[test_case("1;" => Ok(ECMAScriptValue::from(1)); "normal result")]
    #[test_case("void" => serr("During compilation:\nSyntaxError: 1:5: UnaryExpression expected\n"); "syntax error")]
    #[test_case("a;" => serr("Thrown: ReferenceError: Unresolvable Reference"); "runtime error")]
    fn process_ecmascript(src: &str) -> Result<ECMAScriptValue, String> {
        setup_test_agent();
        let result = super::process_ecmascript(&agent, src);
        result.map_err(|e| format!("{e}"))
    }
}

#[test_case(BigInt::from(10), BigInt::from(-2) => Ok(BigInt::from(2)); "negative shift amt")]
#[test_case(BigInt::from(10), BigInt::from(4) => Ok(BigInt::from(160)); "positive shift amt")]
#[test_case(BigInt::from(10), BigInt::from(0xFFFFFFFFFFFF_u64) => serr("out of range conversion regarding big integer attempted"); "overflow")]
fn bigint_leftshift(left: BigInt, right: BigInt) -> Result<BigInt, String> {
    super::bigint_leftshift(&left, &right).map_err(|e| e.to_string())
}

#[test_case(BigInt::from(10), BigInt::from(-2) => Ok(BigInt::from(40)); "negative shift amt")]
#[test_case(BigInt::from(10), BigInt::from(2) => Ok(BigInt::from(2)); "positive shift amt")]
#[test_case(BigInt::from(10), BigInt::from(0xFFFFFFFFFF_u64) => Ok(BigInt::from(0)); "underflow")]
#[test_case(BigInt::from(10), BigInt::from(-0xFFFFFFFFFF_i64) => serr("out of range conversion regarding big integer attempted"); "overflow")]
fn bigint_rightshift(left: BigInt, right: BigInt) -> Result<BigInt, String> {
    super::bigint_rightshift(&left, &right).map_err(|e| e.to_string())
}
