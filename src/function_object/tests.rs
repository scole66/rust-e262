use super::*;
use crate::parser::testhelp::*;
use crate::tests::*;

mod func_args {
    use super::*;
    #[test]
    fn empty() {
        let arguments: &[ECMAScriptValue] = &[];
        let mut args = FuncArgs::from(arguments);

        assert_eq!(args.count(), 0);
        assert_eq!(args.next_arg(), ECMAScriptValue::Undefined);
    }

    #[test]
    fn list_of_two() {
        let arguments: &[ECMAScriptValue] = &[ECMAScriptValue::from(10), ECMAScriptValue::from(20)];
        let mut args = FuncArgs::from(arguments);

        assert_eq!(args.count(), 2);
        assert_eq!(args.next_arg(), ECMAScriptValue::from(10));
        assert_eq!(args.next_arg(), ECMAScriptValue::from(20));
        assert_eq!(args.next_arg(), ECMAScriptValue::Undefined);
        assert_eq!(args.next_arg(), ECMAScriptValue::Undefined);
        assert_eq!(args.count(), 2);
    }

    #[test]
    fn remaining() {
        let arguments: &[ECMAScriptValue] = &[
            ECMAScriptValue::from("first"),
            ECMAScriptValue::from(1),
            ECMAScriptValue::from(2),
            ECMAScriptValue::from(3),
        ];
        let mut args = FuncArgs::from(arguments);

        let first = args.next_arg();
        assert_eq!(first, ECMAScriptValue::from("first"));

        let rest: Vec<&ECMAScriptValue> = args.remaining().collect();
        assert_eq!(rest, &[&ECMAScriptValue::from(1), &ECMAScriptValue::from(2), &ECMAScriptValue::from(3)]);
    }
}

mod function_declaration {
    use super::*;

    mod instantiate_function_object {
        use super::*;
        use test_case::test_case;

        #[test_case("a" => "a"; "named")]
        #[test_case("" => "default"; "unnamed")]
        fn typical(name: &str) -> String {
            let src = format!("function {name}(){{}}");
            let fd = Maker::new(&src).function_declaration();
            setup_test_agent();
            let realm_rc = agent.current_realm_record().unwrap();
            let global_env = realm_rc.borrow().global_env.as_ref().unwrap().clone() as Rc<dyn EnvironmentRecord>;

            let fvalue =
                fd.instantiate_function_object(&agent, global_env.clone(), None, false, &src, fd.clone()).unwrap();
            let fobj = Object::try_from(fvalue).unwrap();

            let result = String::from(JSString::try_from(get(&agent, &fobj, &"name".into()).unwrap()).unwrap());

            let function = fobj.o.to_function_obj().unwrap().function_data().borrow();
            assert_eq!(function.environment.name(), global_env.name());
            assert!(function.private_environment.is_none());
            let params: Rc<FormalParameters> = function.formal_parameters.clone().try_into().unwrap();
            assert!(Rc::ptr_eq(&fd.params, &params));
            let body: Rc<FunctionBody> = function.ecmascript_code.clone().try_into().unwrap();
            assert!(Rc::ptr_eq(&fd.body, &body));
            assert_eq!(function.constructor_kind, ConstructorKind::Base);
            assert!(Rc::ptr_eq(&function.realm, &realm_rc));
            assert!(function.script_or_module.is_none()); // No scripts in test agents
            assert_eq!(function.this_mode, ThisMode::Global);
            assert_eq!(function.strict, false);
            assert!(function.home_object.is_none());
            assert_eq!(function.source_text, src);
            assert!(function.fields.is_empty());
            assert!(function.private_methods.is_empty());
            assert!(matches!(function.class_field_initializer_name, ClassName::Empty));
            assert!(!function.is_class_constructor);
            assert!(function.is_constructor);

            result
        }

        #[test]
        fn compile_error() {
            let src = "function a(){ if (true) { @@@; } return 3; }";
            let fd = Maker::new(src).function_declaration();
            setup_test_agent();
            let realm_rc = agent.current_realm_record().unwrap();
            let global_env = realm_rc.borrow().global_env.as_ref().unwrap().clone() as Rc<dyn EnvironmentRecord>;

            let fvalue =
                fd.instantiate_function_object(&agent, global_env.clone(), None, false, src, fd.clone()).unwrap_err();

            let msg = unwind_any_error(&agent, fvalue);
            assert_eq!(msg, "TypeError: out of range integral type conversion attempted");
        }
    }
}

mod generator_declaration {
    use super::*;
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn instantiate_function_object() {
        let src = "function *a(){}";
        let fd = Maker::new(src).generator_declaration();
        setup_test_agent();
        let global_env = {
            let realm_rc = agent.current_realm_record().unwrap();
            let realm = realm_rc.borrow();
            realm.global_env.as_ref().unwrap().clone() as Rc<dyn EnvironmentRecord>
        };

        fd.instantiate_function_object(&agent, global_env, None, false, src, fd.clone()).unwrap();
    }
}

mod async_function_declaration {
    use super::*;
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn instantiate_function_object() {
        let src = "async function a(){}";
        let fd = Maker::new(src).async_function_declaration();
        setup_test_agent();
        let global_env = {
            let realm_rc = agent.current_realm_record().unwrap();
            let realm = realm_rc.borrow();
            realm.global_env.as_ref().unwrap().clone() as Rc<dyn EnvironmentRecord>
        };

        fd.instantiate_function_object(&agent, global_env, None, false, src, fd.clone()).unwrap();
    }
}

mod async_generator_declaration {
    use super::*;
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn instantiate_function_object() {
        let src = "async function *a(){}";
        let fd = Maker::new(src).async_generator_declaration();
        setup_test_agent();
        let global_env = {
            let realm_rc = agent.current_realm_record().unwrap();
            let realm = realm_rc.borrow();
            realm.global_env.as_ref().unwrap().clone() as Rc<dyn EnvironmentRecord>
        };

        fd.instantiate_function_object(&agent, global_env, None, false, src, fd.clone()).unwrap();
    }
}

mod this_lexicality {
    use super::*;
    use test_case::test_case;

    #[test_case(ThisLexicality::LexicalThis => with |s| assert_ne!(s, ""); "lexical")]
    #[test_case(ThisLexicality::NonLexicalThis => with |s| assert_ne!(s, ""); "non-lexical")]
    fn debug(item: ThisLexicality) -> String {
        format!("{item:?}")
    }

    #[test_case(ThisLexicality::LexicalThis, ThisLexicality::LexicalThis => true; "equal")]
    #[test_case(ThisLexicality::NonLexicalThis, ThisLexicality::LexicalThis => false; "unequal")]
    fn eq(left: ThisLexicality, right: ThisLexicality) -> bool {
        left == right
    }

    #[test]
    #[allow(clippy::clone_on_copy)]
    fn clone() {
        let l1 = ThisLexicality::LexicalThis;
        let l2 = l1.clone();
        assert_eq!(l1, l2);
    }
}

mod constructor_kind {
    use super::*;
    use test_case::test_case;

    #[test_case(ConstructorKind::Derived => with |s| assert_ne!(s, ""); "derived")]
    #[test_case(ConstructorKind::Base => with |s| assert_ne!(s, ""); "base")]
    fn debug(item: ConstructorKind) -> String {
        format!("{item:?}")
    }

    #[test_case(ConstructorKind::Derived, ConstructorKind::Derived => true; "equal")]
    #[test_case(ConstructorKind::Base, ConstructorKind::Derived => false; "unequal")]
    fn eq(left: ConstructorKind, right: ConstructorKind) -> bool {
        left == right
    }

    #[test]
    #[allow(clippy::clone_on_copy)]
    fn clone() {
        let l1 = ConstructorKind::Derived;
        let l2 = l1.clone();
        assert_eq!(l1, l2);
    }
}

mod function_prototype_call {
    use super::*;
    use test_case::test_case;

    #[test_case(|_| ECMAScriptValue::Undefined, &[] => serr("TypeError: this isn't a function"); "bad this")]
    #[test_case(|agent| ECMAScriptValue::from(agent.intrinsic(IntrinsicId::Number)), &[ECMAScriptValue::Undefined, ECMAScriptValue::from(10)] => vok(10); "built-in function call")]
    fn function_prototype_call(
        get_this: impl FnOnce(&Agent) -> ECMAScriptValue,
        args: &[ECMAScriptValue],
    ) -> Result<ECMAScriptValue, String> {
        setup_test_agent();
        let this_value = get_this(&agent);
        super::function_prototype_call(&agent, this_value, None, args).map_err(|err| unwind_any_error(&agent, err))
    }
}
