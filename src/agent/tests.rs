use super::*;
use crate::parser::testhelp::*;
use crate::tests::*;
use ahash::AHashSet;
use lazy_static::lazy_static;
use num::BigInt;
use regex::Regex;
use std::cell::RefCell;
use std::rc::Rc;

mod agent {
    use super::*;
    use test_case::test_case;

    #[test]
    fn new() {
        let agent = Agent::new(Rc::new(RefCell::new(SymbolRegistry::new())));

        // New agent; no realm initialized.
        assert!(agent.execution_context_stack.is_empty());

        // All well-known symbols initialized, and different from one another.
        let symbols = vec![
            agent.wks(WksId::AsyncIterator),
            agent.wks(WksId::HasInstance),
            agent.wks(WksId::IsConcatSpreadable),
            agent.wks(WksId::Iterator),
            agent.wks(WksId::Match),
            agent.wks(WksId::MatchAll),
            agent.wks(WksId::Replace),
            agent.wks(WksId::Search),
            agent.wks(WksId::Species),
            agent.wks(WksId::Split),
            agent.wks(WksId::ToPrimitive),
            agent.wks(WksId::ToStringTag),
            agent.wks(WksId::Unscopables),
        ];
        let num_symbols = symbols.len();
        let mut symbol_set = AHashSet::new();
        for sym in symbols.iter() {
            symbol_set.insert(sym);
        }
        assert_eq!(num_symbols, symbol_set.len());

        // ID trackers at reasonable spots
        assert_eq!(agent.obj_id, 1);
        assert_eq!(agent.symbol_id, num_symbols + 1);
    }

    #[test]
    fn pop_execution_context() {
        let mut agent = test_agent();
        let realm_ref = agent.current_realm_record().unwrap();
        // build a new EC, and add it to the EC stack
        let sr = ScriptRecord {
            realm: realm_ref.clone(),
            ecmascript_code: Maker::new("").script(),
            compiled: Rc::new(Chunk::new("test")),
        };
        let test_ec = ExecutionContext::new(None, realm_ref, Some(ScriptOrModule::Script(Rc::new(sr))));
        agent.push_execution_context(test_ec);
        // now pop it.
        agent.pop_execution_context();
        // And verify the one on top has no script_or_module value
        let r = &agent.execution_context_stack[agent.execution_context_stack.len() - 1];
        assert!(r.script_or_module.is_none());
    }
    #[test]
    fn push_execution_context() {
        let mut agent = test_agent();
        let realm_ref = agent.current_realm_record().unwrap();
        let prior_length = agent.execution_context_stack.len();
        // build a new EC, and add it to the EC stack
        let sr = ScriptRecord {
            realm: realm_ref.clone(),
            ecmascript_code: Maker::new("").script(),
            compiled: Rc::new(Chunk::new("test")),
        };
        let test_ec = ExecutionContext::new(None, realm_ref, Some(ScriptOrModule::Script(Rc::new(sr))));
        agent.push_execution_context(test_ec);

        assert_eq!(agent.execution_context_stack.len(), prior_length + 1);
        let r = &agent.execution_context_stack[agent.execution_context_stack.len() - 1];
        assert!(r.script_or_module.is_some());
    }
    #[test]
    fn active_function_object() {
        let mut agent = Agent::new(Rc::new(RefCell::new(SymbolRegistry::new())));
        // no Running Execution Context, so this should be None.
        let afo = agent.active_function_object();
        assert!(afo.is_none());

        agent.initialize_host_defined_realm(true);
        // Now there's an execution context, but still no active function, so this should still be None.
        let afo = agent.active_function_object();
        assert!(afo.is_none());

        // Create a new EC that _does_ have a function object; push it, and then check the active function.
        let fo = agent.intrinsic(IntrinsicId::ThrowTypeError);
        let realm = agent.current_realm_record().unwrap();
        let function_ec = ExecutionContext::new(Some(fo.clone()), realm, None);
        agent.push_execution_context(function_ec);

        let afo = agent.active_function_object().unwrap();
        assert_eq!(afo, fo);
    }
    #[test]
    fn next_object_id() {
        let mut agent = Agent::new(Rc::new(RefCell::new(SymbolRegistry::new())));
        // Starts at something, and then increases monotonically.
        let first = agent.next_object_id();
        for x in 1..10 {
            assert_eq!(agent.next_object_id(), x + first);
        }
    }
    #[test]
    fn next_symbol_id() {
        let mut agent = Agent::new(Rc::new(RefCell::new(SymbolRegistry::new())));
        // Starts at something, and then increases monotonically.
        let first = agent.next_symbol_id();
        for x in 1..10 {
            assert_eq!(agent.next_symbol_id(), x + first);
        }
    }
    #[test]
    fn debug() {
        assert_ne!(format!("{:?}", Agent::new(Rc::new(RefCell::new(SymbolRegistry::new())))), "");
    }

    #[test]
    fn global_symbol_registry() {
        let registry = Rc::new(RefCell::new(SymbolRegistry::new()));
        let agent = Agent::new(Rc::clone(&registry));
        let gsr = agent.global_symbol_registry();
        assert!(Rc::ptr_eq(&registry, &gsr));
    }

    mod current_realm_record {
        use super::*;

        #[test]
        fn empty_ec_stack() {
            let agent = Agent::new(Rc::new(RefCell::new(SymbolRegistry::new())));
            assert!(agent.current_realm_record().is_none());
        }

        #[test]
        fn multiple_ec_stack() {
            let mut agent = test_agent();
            let first_realm = agent.current_realm_record().unwrap();
            let second_realm = create_realm(&mut agent);
            // build a new EC, and add it to the EC stack
            let sr = ScriptRecord {
                realm: Rc::clone(&second_realm),
                ecmascript_code: Maker::new("").script(),
                compiled: Rc::new(Chunk::new("test")),
            };
            let test_ec =
                ExecutionContext::new(None, Rc::clone(&second_realm), Some(ScriptOrModule::Script(Rc::new(sr))));
            agent.push_execution_context(test_ec);

            let result = agent.current_realm_record().unwrap();
            assert!(Rc::ptr_eq(&result, &second_realm));
            assert!(!Rc::ptr_eq(&result, &first_realm));
        }
    }

    #[test_case(|_| Ok(NormalCompletion::from(10)) => Ok(NormalCompletion::from(ECMAScriptValue::Undefined)); "value")]
    #[test_case(|agent| Err(create_type_error(agent, "Test Sentinel")) => serr("TypeError: Test Sentinel"); "pending type error")]
    #[test_case(|agent| {
            let env = agent.current_realm_record().unwrap().borrow().global_env.clone().unwrap();
            Ok(NormalCompletion::from(Reference::new(Base::Environment(env), "debug_token", true, None)))
        } => Ok(NormalCompletion::from(ECMAScriptValue::Undefined)); "valid ref")]
    fn void_operator(make_expr: fn(&mut Agent) -> FullCompletion) -> Result<NormalCompletion, String> {
        let mut agent = test_agent();
        let expr = make_expr(&mut agent);
        agent.void_operator(expr).map_err(|ac| unwind_any_error(&mut agent, ac))
    }

    #[test_case(|_| Ok(NormalCompletion::from(Reference::new(Base::Unresolvable, "not_here", true, None))) => Ok(NormalCompletion::from("undefined")); "unresolvable ref")]
    #[test_case(|agent| Err(create_type_error(agent, "Test Sentinel")) => serr("TypeError: Test Sentinel"); "pending type error")]
    #[test_case(|_| Ok(NormalCompletion::from(ECMAScriptValue::Undefined)) => Ok(NormalCompletion::from("undefined")); "undefined value")]
    #[test_case(|_| Ok(NormalCompletion::from(ECMAScriptValue::Null)) => Ok(NormalCompletion::from("object")); "null value")]
    #[test_case(|_| Ok(NormalCompletion::from(true)) => Ok(NormalCompletion::from("boolean")); "bool value")]
    #[test_case(|_| Ok(NormalCompletion::from("just a string")) => Ok(NormalCompletion::from("string")); "string value")]
    #[test_case(|_| Ok(NormalCompletion::from(227)) => Ok(NormalCompletion::from("number")); "number value")]
    #[test_case(|_| Ok(NormalCompletion::from(BigInt::from(102))) => Ok(NormalCompletion::from("bigint")); "bigint value")]
    #[test_case(|agent| {
        let symbol_constructor = agent.intrinsic(IntrinsicId::Symbol);
        let reference = Reference::new(Base::Value(ECMAScriptValue::from(symbol_constructor)), "species", true, None);
        Ok(NormalCompletion::from(reference))
    } => Ok(NormalCompletion::from("symbol")); "typeof Symbol.species")]
    #[test_case(|agent| {
        let env = agent.current_realm_record().unwrap().borrow().global_env.clone().unwrap();
        Ok(NormalCompletion::from(Reference::new(Base::Environment(env), "Boolean", true, None)))
    } => Ok(NormalCompletion::from("function")); "typeof Boolean")]
    #[test_case(|agent| {
        let bool_proto = agent.intrinsic(IntrinsicId::BooleanPrototype);
        Ok(NormalCompletion::from(bool_proto))
    } => Ok(NormalCompletion::from("object")); "typeof Boolean.prototype")]
    fn typeof_operator(make_expr: fn(&mut Agent) -> FullCompletion) -> Result<NormalCompletion, String> {
        let mut agent = test_agent();
        let expr = make_expr(&mut agent);
        agent.typeof_operator(expr).map_err(|ac| unwind_any_error(&mut agent, ac))
    }

    fn superproperty(agent: &mut Agent) -> FullCompletion {
        // For example: ({method() { delete super.test_property; }}).method()
        // 1. Let F be OrdinaryFunctionCreate(intrinsics.[[%FunctionPrototype%]], source_text, ParameterList, Body, thisMode, env, privateenv).
        // 2. Let homeObject be OrdinaryObjectCreate(intrinsics.[[%ObjectPrototype%]]).
        // 2. Call MakeMethod(F, homeObject).
        // 3. Let fenv be NewFunctionEnvironment(F, undefined).
        // 4. Let actualThis be fenv.GetThisBinding().
        // 5. Return MakeSuperPropertyReference(actualThis, "test_property", true)
        let obj = ordinary_object_create(agent, None, &[]);
        let copy = obj.clone();
        let myref = Reference::new(Base::Value(obj.into()), "item", true, Some(copy.into()));
        Ok(NormalCompletion::from(myref))
    }

    fn bool_proto_ref(agent: &mut Agent, strict: bool) -> FullCompletion {
        let bool_obj = agent.intrinsic(IntrinsicId::Boolean);
        let myref = Reference::new(Base::Value(bool_obj.into()), "prototype", strict, None);
        Ok(NormalCompletion::from(myref))
    }
    fn strict_proto_ref(agent: &mut Agent) -> FullCompletion {
        bool_proto_ref(agent, true)
    }
    fn nonstrict_proto_ref(agent: &mut Agent) -> FullCompletion {
        bool_proto_ref(agent, false)
    }
    fn dead_ref(agent: &mut Agent) -> FullCompletion {
        let dead = DeadObject::object(agent);
        Ok(NormalCompletion::from(Reference::new(Base::Value(dead.into()), "anything", true, None)))
    }
    fn ref_to_undefined(agent: &mut Agent) -> FullCompletion {
        let env = agent.current_realm_record().unwrap().borrow().global_env.clone().unwrap();
        Ok(NormalCompletion::from(Reference::new(Base::Environment(env), "undefined", true, None)))
    }
    fn dead_env(agent: &mut Agent) -> FullCompletion {
        let outer = agent.current_realm_record().unwrap().borrow().global_env.clone().unwrap();
        let dead = DeadObject::object(agent);
        let obj_env = Rc::new(ObjectEnvironmentRecord::new(dead, false, Some(outer), "dead"));
        Ok(NormalCompletion::from(Reference::new(Base::Environment(obj_env), "anything", true, None)))
    }

    #[test_case(|_| Ok(NormalCompletion::Empty) => Ok(NormalCompletion::from(true)); "empty -> true")]
    #[test_case(|_| Ok(NormalCompletion::from("test sentinel")) => Ok(NormalCompletion::from(true)); "value -> true")]
    #[test_case(|agent| Err(create_type_error(agent, "Test Sentinel")) => serr("TypeError: Test Sentinel"); "pending type error")]
    #[test_case(|_| Ok(NormalCompletion::from(Reference::new(Base::Unresolvable, "not_here", true, None))) => Ok(NormalCompletion::from(true)); "unresolvable ref")]
    #[test_case(superproperty => serr("ReferenceError: super properties not deletable"); "super prop")]
    #[test_case(|_| Ok(NormalCompletion::from(Reference::new(Base::Value(ECMAScriptValue::Undefined), "x", true, None))) => serr("TypeError: Undefined and null cannot be converted to objects"); "Non-object ref base")]
    #[test_case(|_| Ok(NormalCompletion::from(Reference::new(Base::Value(true.into()), "x", true, None))) => Ok(NormalCompletion::from(true)); "delete nonexistent")]
    #[test_case(strict_proto_ref => serr("TypeError: property not deletable"); "permanent property; strict")]
    #[test_case(nonstrict_proto_ref => Ok(NormalCompletion::from(false)); "permanent property; nonstrict")]
    #[test_case(dead_ref => serr("TypeError: delete called on DeadObject"); "property ref delete errs")]
    #[test_case(ref_to_undefined => Ok(NormalCompletion::from(false)); "undefined ref")]
    #[test_case(dead_env => serr("TypeError: delete called on DeadObject"); "env ref delete errors")]
    fn delete_ref(make_expr: fn(&mut Agent) -> FullCompletion) -> Result<NormalCompletion, String> {
        let mut agent = test_agent();
        let expr = make_expr(&mut agent);
        agent.delete_ref(expr).map_err(|ac| unwind_any_error(&mut agent, ac))
    }

    #[test_case(|_| ECMAScriptValue::from("left "),
                |_| ECMAScriptValue::from("right"),
                BinOp::Add
                => Ok(NormalCompletion::from("left right")); "string catentation")]
    #[test_case(|agent| ECMAScriptValue::from(make_toprimitive_throw_obj(agent)),
                |_| ECMAScriptValue::from("a"),
                BinOp::Add
                => serr("TypeError: Test Sentinel"); "left toPrimitive error")]
    #[test_case(|_| ECMAScriptValue::from("a"),
                |agent| ECMAScriptValue::from(make_toprimitive_throw_obj(agent)),
                BinOp::Add
                => serr("TypeError: Test Sentinel"); "right toPrimitive error")]
    #[test_case(|_| ECMAScriptValue::from(10),
                |_| ECMAScriptValue::from("a"),
                BinOp::Add
                => Ok(NormalCompletion::from("10a")); "stringify from right")]
    #[test_case(|_| ECMAScriptValue::from("a"),
                |_| ECMAScriptValue::from(10),
                BinOp::Add
                => Ok(NormalCompletion::from("a10")); "stringify from left")]
    #[test_case(|agent| ECMAScriptValue::from(agent.wks(WksId::ToPrimitive)),
                |_| ECMAScriptValue::from("a"),
                BinOp::Add
                => serr("TypeError: Symbols may not be converted to strings"); "left tostring errs")]
    #[test_case(|_| ECMAScriptValue::from("a"),
                |agent| ECMAScriptValue::from(agent.wks(WksId::ToPrimitive)),
                BinOp::Add
                => serr("TypeError: Symbols may not be converted to strings"); "right tostring errs")]
    #[test_case(|agent| ECMAScriptValue::from(agent.wks(WksId::ToPrimitive)),
                |_| ECMAScriptValue::from(10),
                BinOp::Add
                => serr("TypeError: Symbol values cannot be converted to Number values"); "left tonumeric errs")]
    #[test_case(|_| ECMAScriptValue::from(10),
                |agent| ECMAScriptValue::from(agent.wks(WksId::ToPrimitive)),
                BinOp::Add
                => serr("TypeError: Symbol values cannot be converted to Number values"); "right tonumeric errs")]
    #[test_case(|_| ECMAScriptValue::from(2.0),
                |_| ECMAScriptValue::from(3.0),
                BinOp::Exponentiate
                => Ok(NormalCompletion::from(8)); "exponentiation")]
    #[test_case(|_| ECMAScriptValue::from(2.0),
                |_| ECMAScriptValue::from(3.0),
                BinOp::Multiply
                => Ok(NormalCompletion::from(6)); "multiplication")]
    #[test_case(|_| ECMAScriptValue::from(12.0),
                |_| ECMAScriptValue::from(3.0),
                BinOp::Divide
                => Ok(NormalCompletion::from(4)); "division")]
    #[test_case(|_| ECMAScriptValue::from(26.0),
                |_| ECMAScriptValue::from(7.0),
                BinOp::Remainder
                => Ok(NormalCompletion::from(5)); "remainder")]
    #[test_case(|_| ECMAScriptValue::from(2.0),
                |_| ECMAScriptValue::from(3.0),
                BinOp::Add
                => Ok(NormalCompletion::from(5)); "addition")]
    #[test_case(|_| ECMAScriptValue::from(2.0),
                |_| ECMAScriptValue::from(3.0),
                BinOp::Subtract
                => Ok(NormalCompletion::from(-1)); "subtraction")]
    #[test_case(|_| ECMAScriptValue::from(2.0),
                |_| ECMAScriptValue::from(3.0),
                BinOp::LeftShift
                => panics "not yet implemented"; "left shift")]
    #[test_case(|_| ECMAScriptValue::from(2.0),
                |_| ECMAScriptValue::from(3.0),
                BinOp::SignedRightShift
                => panics "not yet implemented"; "signed right shift")]
    #[test_case(|_| ECMAScriptValue::from(2.0),
                |_| ECMAScriptValue::from(3.0),
                BinOp::UnsignedRightShift
                => panics "not yet implemented"; "unsigned right shift")]
    #[test_case(|_| ECMAScriptValue::from(2.0),
                |_| ECMAScriptValue::from(3.0),
                BinOp::BitwiseAnd
                => panics "not yet implemented"; "bitwise and")]
    #[test_case(|_| ECMAScriptValue::from(2.0),
                |_| ECMAScriptValue::from(3.0),
                BinOp::BitwiseOr
                => panics "not yet implemented"; "bitwise or")]
    #[test_case(|_| ECMAScriptValue::from(2.0),
                |_| ECMAScriptValue::from(3.0),
                BinOp::BitwiseXor
                => panics "not yet implemented"; "bitwise xor")]
    #[test_case(|_| ECMAScriptValue::from(BigInt::from(2)),
                |_| ECMAScriptValue::from(BigInt::from(3)),
                BinOp::Exponentiate
                => Ok(NormalCompletion::from(BigInt::from(8))); "exponentiation (bigint)")]
    #[test_case(|_| ECMAScriptValue::from(BigInt::from(2)),
                |_| ECMAScriptValue::from(BigInt::from(-3)),
                BinOp::Exponentiate
                => serr("RangeError: Exponent must be positive"); "bad exponentiation (bigint)")]
    #[test_case(|_| ECMAScriptValue::from(BigInt::from(2)),
                |_| ECMAScriptValue::from(BigInt::from(3)),
                BinOp::Multiply
                => Ok(NormalCompletion::from(BigInt::from(6))); "multiplication (bigint)")]
    #[test_case(|_| ECMAScriptValue::from(BigInt::from(12)),
                |_| ECMAScriptValue::from(BigInt::from(3)),
                BinOp::Divide
                => Ok(NormalCompletion::from(BigInt::from(4))); "division (bigint)")]
    #[test_case(|_| ECMAScriptValue::from(BigInt::from(12)),
                |_| ECMAScriptValue::from(BigInt::from(0)),
                BinOp::Divide
                =>serr("RangeError: Division by zero"); "zero division (bigint)")]
    #[test_case(|_| ECMAScriptValue::from(BigInt::from(26)),
                |_| ECMAScriptValue::from(BigInt::from(7)),
                BinOp::Remainder
                => Ok(NormalCompletion::from(BigInt::from(5))); "remainder (bigint)")]
    #[test_case(|_| ECMAScriptValue::from(BigInt::from(12)),
                |_| ECMAScriptValue::from(BigInt::from(0)),
                BinOp::Remainder
                =>serr("RangeError: Division by zero"); "zero remainder (bigint)")]
    #[test_case(|_| ECMAScriptValue::from(BigInt::from(2)),
                |_| ECMAScriptValue::from(BigInt::from(3)),
                BinOp::Add
                => Ok(NormalCompletion::from(BigInt::from(5))); "addition (bigint)")]
    #[test_case(|_| ECMAScriptValue::from(BigInt::from(2)),
                |_| ECMAScriptValue::from(BigInt::from(3)),
                BinOp::Subtract
                => Ok(NormalCompletion::from(BigInt::from(-1))); "subtraction (bigint)")]
    #[test_case(|_| ECMAScriptValue::from(BigInt::from(2)),
                |_| ECMAScriptValue::from(BigInt::from(3)),
                BinOp::LeftShift
                => panics "not yet implemented"; "left shift (bigint)")]
    #[test_case(|_| ECMAScriptValue::from(BigInt::from(2)),
                |_| ECMAScriptValue::from(BigInt::from(3)),
                BinOp::SignedRightShift
                => panics "not yet implemented"; "signed right shift (bigint)")]
    #[test_case(|_| ECMAScriptValue::from(BigInt::from(2)),
                |_| ECMAScriptValue::from(BigInt::from(3)),
                BinOp::UnsignedRightShift
                => panics "not yet implemented"; "unsigned right shift (bigint)")]
    #[test_case(|_| ECMAScriptValue::from(BigInt::from(2)),
                |_| ECMAScriptValue::from(BigInt::from(3)),
                BinOp::BitwiseAnd
                => panics "not yet implemented"; "bitwise and (bigint)")]
    #[test_case(|_| ECMAScriptValue::from(BigInt::from(2)),
                |_| ECMAScriptValue::from(BigInt::from(3)),
                BinOp::BitwiseOr
                => panics "not yet implemented"; "bitwise or (bigint)")]
    #[test_case(|_| ECMAScriptValue::from(BigInt::from(2)),
                |_| ECMAScriptValue::from(BigInt::from(3)),
                BinOp::BitwiseXor
                => panics "not yet implemented"; "bitwise xor (bigint)")]
    #[test_case(|_| ECMAScriptValue::from(BigInt::from(12)),
                |_| ECMAScriptValue::from(3),
                BinOp::Remainder
                =>serr("TypeError: Cannot mix BigInt and other types, use explicit conversions"); "bigint type mix (left)")]
    #[test_case(|_| ECMAScriptValue::from(12),
                |_| ECMAScriptValue::from(BigInt::from(1)),
                BinOp::Remainder
                =>serr("TypeError: Cannot mix BigInt and other types, use explicit conversions"); "bigint type mix (right)")]
    fn apply_string_or_numeric_binary_operator(
        make_lval: fn(&mut Agent) -> ECMAScriptValue,
        make_rval: fn(&mut Agent) -> ECMAScriptValue,
        op: BinOp,
    ) -> Result<NormalCompletion, String> {
        let mut agent = test_agent();
        let lval = make_lval(&mut agent);
        let rval = make_rval(&mut agent);
        agent.apply_string_or_numeric_binary_operator(lval, rval, op).map_err(|ac| unwind_any_error(&mut agent, ac))
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

#[test]
fn wks_descriptions() {
    let agent = test_agent();
    let symbols = vec![
        WksId::AsyncIterator,
        WksId::HasInstance,
        WksId::IsConcatSpreadable,
        WksId::Iterator,
        WksId::Match,
        WksId::MatchAll,
        WksId::Replace,
        WksId::Search,
        WksId::Species,
        WksId::Split,
        WksId::ToPrimitive,
        WksId::ToStringTag,
        WksId::Unscopables,
    ];
    let descriptions = vec![
        "Symbol.asyncIterator",
        "Symbol.hasInstance",
        "Symbol.isConcatSpreadable",
        "Symbol.iterator",
        "Symbol.match",
        "Symbol.matchAll",
        "Symbol.replace",
        "Symbol.search",
        "Symbol.species",
        "Symbol.split",
        "Symbol.toPrimitive",
        "Symbol.toStringTag",
        "Symbol.unscopables",
    ];
    for (id, expected) in symbols.iter().zip(descriptions) {
        let desc = agent.wks(*id).description().unwrap();
        assert_eq!(desc, JSString::from(expected));
    }
}

mod current_realm_record {
    use super::*;

    #[test]
    fn empty() {
        let a = Agent::new(Rc::new(RefCell::new(SymbolRegistry::new())));
        let realm = a.current_realm_record();

        assert!(realm.is_none());
    }
    #[test]
    fn stacked() {
        let mut a = Agent::new(Rc::new(RefCell::new(SymbolRegistry::new())));
        let first_realm = create_named_realm(&mut a, "first");
        let first_context = ExecutionContext::new(None, first_realm, None);
        a.push_execution_context(first_context);

        let second_realm = create_named_realm(&mut a, "second");
        let second_context = ExecutionContext::new(None, second_realm, None);
        a.push_execution_context(second_context);

        let current = a.current_realm_record().unwrap();
        assert_eq!(get_realm_name(&mut a, &*current.borrow()), "second");

        a.pop_execution_context();

        let current = a.current_realm_record().unwrap();
        assert_eq!(get_realm_name(&mut a, &*current.borrow()), "first");
    }
}

mod well_known_symbols {
    use super::*;

    #[test]
    fn debug() {
        let agent = test_agent();
        let s = format!("{:?}", agent.symbols);
        assert_ne!(s, "");
    }
}

mod parse_script {
    use super::*;
    use test_case::test_case;

    #[test]
    fn happy() {
        let mut agent = test_agent();
        let src = "'hello world';";
        let starting_realm = agent.current_realm_record().unwrap();
        let ScriptRecord { realm, ecmascript_code, compiled } =
            super::parse_script(&mut agent, src, starting_realm.clone()).unwrap();
        assert!(Rc::ptr_eq(&realm, &starting_realm));
        assert_eq!(format!("{}", ecmascript_code), "'hello world' ;");
        assert_eq!(compiled.name, "top level script");
        assert_eq!(
            compiled.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
            svec(&["STRING 0 (hello world)",])
        );
    }

    #[test_case("for [i=0, i<10, i++] {}" => sset(&["1:5: ‘(’ expected"]); "parse time syntax")]
    #[test_case("break lbl;" => sset(&["undefined break target detected"]); "early error syntax")]
    fn parse_error(src: &str) -> AHashSet<String> {
        let mut agent = test_agent();
        let starting_realm = agent.current_realm_record().unwrap();
        let errs = super::parse_script(&mut agent, src, starting_realm).unwrap_err();
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
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

mod top_level_fcn_def {
    use super::*;
    use test_case::test_case;

    #[test]
    fn debug() {
        let fd = Maker::new("function alice(x) { return x * 2; }").function_declaration();
        assert_ne!(format!("{:?}", TopLevelFcnDef::Function(fd)), "");
    }

    #[test]
    fn clone() {
        let fd = Maker::new("function alice(x) { return x * 2; }").function_declaration();
        let tlfd1 = TopLevelFcnDef::Function(fd);
        let tlfd2 = tlfd1.clone();

        match (tlfd1, tlfd2) {
            (TopLevelFcnDef::Function(fd1), TopLevelFcnDef::Function(fd2)) => {
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
    #[test_case(make_var_decl => serr("Not a top-level function def"); "Var decl")]
    #[test_case(make_for_binding => serr("Not a top-level function def"); "for binding")]
    fn try_from(maker: fn() -> MakerResult) -> Result<bool, String> {
        let (maybe_fd, maybe_gd, maybe_afd, maybe_agd, vsd) = maker();
        TopLevelFcnDef::try_from(vsd)
            .map(|tlfd| match (tlfd, maybe_fd, maybe_gd, maybe_afd, maybe_agd) {
                (TopLevelFcnDef::Function(fd1), Some(fd2), _, _, _) => Rc::ptr_eq(&fd1, &fd2),
                (TopLevelFcnDef::Generator(gd1), _, Some(gd2), _, _) => Rc::ptr_eq(&gd1, &gd2),
                (TopLevelFcnDef::AsyncFun(afd1), _, _, Some(afd2), _) => Rc::ptr_eq(&afd1, &afd2),
                (TopLevelFcnDef::AsyncGen(agd1), _, _, _, Some(agd2)) => Rc::ptr_eq(&agd1, &agd2),
                _ => false,
            })
            .map_err(|err| err.to_string())
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
    #[test_case("function f(){}" => panics "not yet implemented"; "functions")]
    #[test_case("function *g(){}" => panics "not yet implemented"; "generators")]
    #[test_case("async function af(){}" => panics "not yet implemented"; "async functions")]
    #[test_case("async function *ag(){}" => panics "not yet implemented"; "async generators")]
    fn global_declaration_instantiation(src: &str) -> Result<(AHashSet<String>, AHashSet<String>), String> {
        let mut agent = test_agent();
        let script = Maker::new(src).script();
        let global_env = agent.current_realm_record().unwrap().borrow().global_env.clone().unwrap();
        global_env.create_global_var_binding(&mut agent, "already_var_declared".into(), false).unwrap();
        global_env.create_mutable_binding(&mut agent, "existing_mutable".into(), false).unwrap();

        let prior_vardecl = global_env.var_decls().into_iter().collect::<AHashSet<_>>();
        let prior_lexdecl = global_env.lex_decls().into_iter().collect::<AHashSet<_>>();

        let result = super::global_declaration_instantiation(&mut agent, script, global_env.clone());

        result.map_err(|err| unwind_any_error(&mut agent, err)).map(|_| {
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
        let mut agent = test_agent();
        let realm = agent.current_realm_record().unwrap();
        let script_record = parse_script(&mut agent, src, realm).unwrap();

        super::script_evaluation(&mut agent, script_record).map_err(|err| unwind_any_error(&mut agent, err))
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

    fn internal_err(_: &mut Agent) -> ProcessError {
        ProcessError::InternalError { reason: "blue".into() }
    }

    fn runtime_err_obj(agent: &mut Agent) -> ProcessError {
        let err = create_type_error_object(agent, "test sentinel");
        ProcessError::RuntimeError { error: err.into() }
    }
    fn runtime_err_value(_: &mut Agent) -> ProcessError {
        let error = "test sentinel".into();
        ProcessError::RuntimeError { error }
    }
    fn runtime_err_non_err_obj(agent: &mut Agent) -> ProcessError {
        let error = ordinary_object_create(agent, None, &[]).into();
        ProcessError::RuntimeError { error }
    }
    fn matches_object(s: String) {
        lazy_static! {
            static ref MATCH: Regex = Regex::new("^Thrown: <Object [0-9]+>$").expect("Valid regex");
        }
        assert!(MATCH.is_match(&s));
    }
    fn compiler_objs(agent: &mut Agent) -> ProcessError {
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
    fn display(make_error: fn(&mut Agent) -> ProcessError) -> String {
        let mut agent = test_agent();
        let err = make_error(&mut agent);
        format!("{err}")
    }

    #[test]
    fn display_err() {
        let mut agent = test_agent();
        let err = compiler_objs(&mut agent);
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
        let mut agent = test_agent();
        let result = super::process_ecmascript(&mut agent, src);
        result.map_err(|e| format!("{e}"))
    }
}
