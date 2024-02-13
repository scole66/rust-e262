use super::*;
use crate::tests::*;
use std::rc::Rc;

mod base {
    use super::*;
    use test_case::test_case;

    #[test_case(&Base::Value(ECMAScriptValue::from(33)) => with |s| assert_ne!(s, ""); "value")]
    fn debug(b: &Base) -> String {
        format!("{b:?}")
    }

    #[test_case(&Base::Unresolvable => Base::Unresolvable; "unresolvable")]
    #[test_case(&Base::Value(ECMAScriptValue::from("regurgitate")) => Base::Value(ECMAScriptValue::from("regurgitate")); "value")]
    fn clone(b: &Base) -> Base {
        #[allow(clippy::redundant_clone)]
        b.clone()
    }

    #[test]
    #[allow(clippy::redundant_clone)]
    fn clone_with_environment() {
        let env = Rc::new(DeclarativeEnvironmentRecord::new(None, "test"));
        let base = Base::Environment(env);
        let duplicate = base.clone();
        assert_eq!(&base, &duplicate);
    }

    #[test]
    fn eq_with_environment() {
        let env = Rc::new(DeclarativeEnvironmentRecord::new(None, "test"));
        let other_env = Rc::new(DeclarativeEnvironmentRecord::new(None, "test"));
        let base = Base::Environment(env.clone());
        let should_be_equal = Base::Environment(env);
        let shouldnt_be_equal = Base::Environment(other_env);
        assert!(base == should_be_equal);
        assert!(base != shouldnt_be_equal);
    }

    #[test_case(&Base::Unresolvable, &Base::Unresolvable => true; "unresolvable match")]
    #[test_case(&Base::Value(10.into()), &Base::Value(10.into()) => true; "value match")]
    #[test_case(&Base::Value(10.into()), &Base::Value(false.into()) => false; "value mismatch")]
    #[test_case(&Base::Unresolvable, &Base::Value("blue".into()) => false; "base type mismatch")]
    fn eq(left: &Base, right: &Base) -> bool {
        left == right
    }

    #[test_case(&Base::Unresolvable => "unresolvable"; "unresolvable")]
    #[test_case(&Base::Value(10.into()) => "10"; "value")]
    #[test_case(&Base::Environment(Rc::new(DeclarativeEnvironmentRecord::new(None, "test"))) => "DeclarativeEnvironmentRecord(test)"; "environment")]
    fn display(b: &Base) -> String {
        format!("{b}")
    }

    mod try_from {
        use super::*;
        use test_case::test_case;

        #[test_case(Base::Unresolvable => Err("Reference was not a Property Ref".to_string()); "unresolvable")]
        #[test_case(Base::Value(10.into()) => Ok(ECMAScriptValue::from(10)); "value")]
        #[test_case(Base::Environment(Rc::new(DeclarativeEnvironmentRecord::new(None, "test"))) => Err("Reference was not a Property Ref".to_string()); "environment")]
        fn value(b: Base) -> Result<ECMAScriptValue, String> {
            ECMAScriptValue::try_from(b).map_err(|e| e.to_string())
        }

        #[test_case(Base::Unresolvable => Err("Reference was not an environment ref".to_string()); "unresolvable")]
        #[test_case(Base::Value(true.into()) => Err("Reference was not an environment ref".to_string()); "value")]
        #[test_case(Base::Environment(Rc::new(DeclarativeEnvironmentRecord::new(None, "test"))) => Ok("DeclarativeEnvironmentRecord(test)".to_string()); "environment")]
        fn envrec(b: Base) -> Result<String, String> {
            let result: Result<Rc<dyn EnvironmentRecord>, String> =
                b.try_into().map_err(|e: anyhow::Error| e.to_string());
            result.map(|er| format!("{er:?}"))
        }
    }
}

mod referenced_name {
    use super::*;
    use test_case::test_case;

    #[test]
    fn debug() {
        let rn = ReferencedName::String(JSString::from("apple"));
        assert_ne!(format!("{rn:?}"), "");
    }

    #[test_case(&ReferencedName::from("popsicle") => ReferencedName::from("popsicle"); "string")]
    fn clone(rn: &ReferencedName) -> ReferencedName {
        #[allow(clippy::redundant_clone)]
        rn.clone()
    }

    mod partial_eq {
        use super::*;

        fn setup() -> (ReferencedName, ReferencedName, ReferencedName) {
            (
                ReferencedName::String(JSString::from("apple")),
                ReferencedName::PrivateName(PrivateName::new("apple")),
                ReferencedName::String(JSString::from("apple")),
            )
        }

        #[test]
        fn ne() {
            let (rn1, rn2, rn3) = setup();
            assert_eq!(rn1 != rn2, true);
            assert_eq!(rn1 != rn3, false);
            assert_eq!(rn2 != rn3, true);
        }
        #[test]
        fn eq() {
            let (rn1, rn2, rn3) = setup();
            assert_eq!(rn1 == rn2, false);
            assert_eq!(rn1 == rn3, true);
            assert_eq!(rn2 == rn3, false);
        }
    }

    mod from {
        use super::*;

        #[test]
        fn str_slice() {
            let rn = ReferencedName::from("orange");
            assert_eq!(rn, ReferencedName::String(JSString::from("orange")));
        }
        #[test]
        fn string() {
            let rn = ReferencedName::from(String::from("orange"));
            assert_eq!(rn, ReferencedName::String(JSString::from("orange")));
        }
        #[test]
        fn jsstring() {
            let rn = ReferencedName::from(JSString::from("orange"));
            assert_eq!(rn, ReferencedName::String(JSString::from("orange")));
        }
        #[test]
        fn symbol() {
            setup_test_agent();
            let sym = Symbol::new(None);
            let rn = ReferencedName::from(sym.clone());
            assert_eq!(rn, ReferencedName::Symbol(sym));
        }
        #[test]
        fn privatename() {
            let pn = PrivateName::new("a");
            let rn = ReferencedName::from(pn.clone());
            assert_eq!(rn, ReferencedName::PrivateName(pn));
        }
        mod property_key {
            use super::*;
            #[test]
            fn string() {
                let pk = PropertyKey::from("a");
                let rn = ReferencedName::from(pk);
                assert_eq!(rn, ReferencedName::String(JSString::from("a")));
            }
            #[test]
            fn symbol() {
                setup_test_agent();
                let sym = Symbol::new(Some(JSString::from("crazy")));
                let pk = PropertyKey::from(sym.clone());
                let rn = ReferencedName::from(pk);
                assert_eq!(rn, ReferencedName::Symbol(sym));
            }
        }
    }

    mod try_from {
        use super::*;

        mod jsstring {
            use super::*;
            #[test]
            fn string() {
                let rn = ReferencedName::from("a thing");
                let s: JSString = rn.try_into().unwrap();
                assert_eq!(s, JSString::from("a thing"));
            }
            #[test]
            fn symbol() {
                setup_test_agent();
                let sym = Symbol::new(None);
                let rn = ReferencedName::from(sym);
                let err = JSString::try_from(rn).unwrap_err();
                assert_eq!(err, "invalid string");
            }
            #[test]
            fn privatename() {
                let rn = ReferencedName::PrivateName(PrivateName::new("blue"));
                let err = JSString::try_from(rn).unwrap_err();
                assert_eq!(err, "invalid string");
            }
        }

        mod propertykey {
            use super::*;

            #[test]
            fn string() {
                let rn = ReferencedName::from("str");
                let pk: PropertyKey = rn.try_into().unwrap();
                assert_eq!(pk, PropertyKey::String(JSString::from("str")));
            }
            #[test]
            fn symbol() {
                setup_test_agent();
                let sym = Symbol::new(None);
                let rn = ReferencedName::from(sym.clone());
                let pk: PropertyKey = rn.try_into().unwrap();
                assert_eq!(pk, PropertyKey::Symbol(sym));
            }
            #[test]
            fn privatename() {
                let rn = ReferencedName::PrivateName(PrivateName::new("blue"));
                let err = PropertyKey::try_from(rn).unwrap_err();
                assert_eq!(err, "invalid property key");
            }
        }
    }

    mod display {
        use super::*;
        use test_case::test_case;

        #[test_case(&ReferencedName::from("string") => "string"; "string")]
        #[test_case(&ReferencedName::PrivateName(PrivateName::new("blue")) => "PN[blue]"; "private")]
        fn not_symbol(rn: &ReferencedName) -> String {
            format!("{rn}")
        }

        #[test]
        fn symbol() {
            setup_test_agent();
            let sym = Symbol::new(Some("symbol-test".into()));
            let rn = ReferencedName::from(sym);
            let result = format!("{rn}");
            assert_eq!(result, "Symbol(symbol-test)");
        }
    }
}

mod reference {
    use super::*;

    #[test]
    fn debug() {
        let r = Reference {
            base: Base::Unresolvable,
            referenced_name: ReferencedName::String(JSString::from("name")),
            strict: false,
            this_value: None,
        };
        assert_ne!(format!("{r:?}"), "");
    }

    #[test]
    #[allow(clippy::redundant_clone)]
    fn clone() {
        let r = Reference {
            base: Base::Unresolvable,
            referenced_name: ReferencedName::String(JSString::from("name")),
            strict: false,
            this_value: None,
        };
        let r2 = r.clone();
        assert_eq!(r, r2);
    }

    #[test]
    #[allow(clippy::redundant_clone)]
    fn eq() {
        let r1 = Reference {
            base: Base::Unresolvable,
            referenced_name: ReferencedName::String(JSString::from("name")),
            strict: false,
            this_value: None,
        };
        let r2 = Reference {
            base: Base::Value(true.into()),
            referenced_name: ReferencedName::String(JSString::from("name")),
            strict: false,
            this_value: None,
        };
        let r3 = r1.clone();

        assert!(r1 == r3);
        assert!(r1 != r2);
    }

    mod new {
        use super::*;
        #[test]
        fn string() {
            let r =
                Reference::new(Base::Unresolvable, JSString::from("anobject"), false, Some(ECMAScriptValue::from(999)));
            assert!(matches!(r.base, Base::Unresolvable));
            assert_eq!(r.referenced_name, ReferencedName::from("anobject"));
            assert_eq!(r.strict, false);
            assert_eq!(r.this_value, Some(ECMAScriptValue::from(999)));
        }
        #[test]
        fn key() {
            let r = Reference::new(
                Base::Unresolvable,
                PropertyKey::from("anobject"),
                false,
                Some(ECMAScriptValue::from(999)),
            );
            assert!(matches!(r.base, Base::Unresolvable));
            assert_eq!(r.referenced_name, ReferencedName::from("anobject"));
            assert_eq!(r.strict, false);
            assert_eq!(r.this_value, Some(ECMAScriptValue::from(999)));
        }
    }

    mod is_property_reference {
        use super::*;

        #[test]
        fn propref() {
            let r = Reference::new(Base::Value(ECMAScriptValue::from(10)), "blue", false, None);
            assert!(r.is_property_reference());
        }

        #[test]
        fn unresolved() {
            let r = Reference::new(Base::Unresolvable, "blue", false, None);
            assert!(!r.is_property_reference());
        }
    }

    mod is_unresolvable_reference {
        use super::*;

        #[test]
        fn unresolved() {
            assert!(Reference::new(Base::Unresolvable, "blue", false, None).is_unresolvable_reference());
        }
        #[test]
        fn propref() {
            assert!(!Reference::new(Base::Value(ECMAScriptValue::from(10)), "blue", false, None)
                .is_unresolvable_reference());
        }
    }

    mod is_super_reference {
        use super::*;

        #[test]
        fn has_this() {
            assert!(Reference::new(Base::Value(ECMAScriptValue::from(10)), "a", false, Some(ECMAScriptValue::from(1)))
                .is_super_reference());
        }
        #[test]
        fn no_this() {
            assert!(!Reference::new(Base::Value(ECMAScriptValue::from(10)), "a", false, None).is_super_reference());
        }
    }

    mod is_private_reference {
        use super::*;

        #[test]
        fn public() {
            assert!(!Reference::new(Base::Value(ECMAScriptValue::from(10)), "a", false, None).is_private_reference());
        }
        #[test]
        fn private() {
            assert!(Reference::new(Base::Value(ECMAScriptValue::from(10)), PrivateName::new("a"), false, None)
                .is_private_reference());
        }
    }

    mod get_this_value {
        use super::*;
        #[test]
        fn value_no_this() {
            let value = ECMAScriptValue::from("own men. Most of the confiden");
            let reference = Reference::new(Base::Value(value.clone()), "phrase", true, None);
            assert_eq!(reference.get_this_value(), value);
        }
        #[test]
        fn value_has_this() {
            setup_test_agent();
            let object_proto = intrinsic(IntrinsicId::ObjectPrototype);
            let normal_object = ordinary_object_create(Some(object_proto), &[]);
            let this_value = ECMAScriptValue::from(normal_object);
            let value = ECMAScriptValue::from("Gatsby turned out all right at the end");
            let reference = Reference::new(Base::Value(value), "phrase", true, Some(this_value.clone()));
            assert_eq!(reference.get_this_value(), this_value);
        }
        #[test]
        #[should_panic(expected = "unreachable code")]
        fn unresolvable() {
            Reference::new(Base::Unresolvable, "blurp", true, None).get_this_value();
        }
        #[test]
        #[should_panic(expected = "unreachable code")]
        fn environment() {
            Reference::new(
                Base::Environment(Rc::new(DeclarativeEnvironmentRecord::new(None, "test"))),
                "blurp",
                true,
                None,
            )
            .get_this_value();
        }
    }
}

mod get_value {
    use super::*;

    #[test]
    fn abrupt() {
        setup_test_agent();
        let err = create_type_error("Test Path");
        let result = get_value(Err(err)).unwrap_err();
        assert_eq!(unwind_type_error(result), "Test Path");
    }

    #[test]
    fn simple_value() {
        setup_test_agent();
        let val = ECMAScriptValue::from("a value");
        let result = get_value(Ok(NormalCompletion::from(val))).unwrap();
        assert_eq!(result, ECMAScriptValue::from("a value"));
    }

    #[test]
    fn unresolvable() {
        setup_test_agent();
        let badref = Reference::new(Base::Unresolvable, "no_ref", true, None);
        let result = get_value(Ok(NormalCompletion::from(badref))).unwrap_err();
        assert_eq!(unwind_reference_error(result), "Unresolvable Reference");
    }
    #[test]
    fn empty() {
        setup_test_agent();
        let val = Ok(NormalCompletion::Empty);
        let result = get_value(val).unwrap_err();
        assert_eq!(unwind_reference_error(result), "Unresolvable Reference");
    }
    #[test]
    fn value_base() {
        setup_test_agent();
        let object_proto = intrinsic(IntrinsicId::ObjectPrototype);
        let normal_object = ordinary_object_create(Some(object_proto), &[]);
        let value = ECMAScriptValue::from("value_base test value");
        let descriptor = PotentialPropertyDescriptor {
            writable: Some(true),
            enumerable: Some(true),
            configurable: Some(true),
            value: Some(value.clone()),
            ..Default::default()
        };
        define_property_or_throw(&normal_object, PropertyKey::from("test_value"), descriptor).unwrap();
        let reference = Reference::new(Base::Value(ECMAScriptValue::from(normal_object)), "test_value", true, None);

        let result = get_value(Ok(NormalCompletion::from(reference))).unwrap();
        assert_eq!(result, value);
    }
    #[test]
    fn to_object_err() {
        setup_test_agent();
        let reference = Reference::new(Base::Value(ECMAScriptValue::Undefined), "test_value", true, None);
        let result = get_value(Ok(NormalCompletion::from(reference))).unwrap_err();
        assert_eq!(unwind_type_error(result), "Undefined and null cannot be converted to objects");
    }
    #[test]
    fn private() {
        setup_test_agent();
        let pn = PrivateName::new("test name");
        let object_proto = intrinsic(IntrinsicId::ObjectPrototype);
        let normal_object = ordinary_object_create(Some(object_proto), &[]);
        let value = ECMAScriptValue::from("test value for private identifier");
        private_field_add(&normal_object, pn.clone(), value.clone()).unwrap();
        let reference = Reference::new(Base::Value(ECMAScriptValue::from(normal_object)), pn, true, None);
        let result = get_value(Ok(NormalCompletion::from(reference))).unwrap();
        assert_eq!(result, value);
    }
    #[test]
    fn environment() {
        setup_test_agent();
        let object_proto = intrinsic(IntrinsicId::ObjectPrototype);
        let global = ordinary_object_create(Some(object_proto), &[]);
        let this_obj = global.clone();
        let env = GlobalEnvironmentRecord::new(global, this_obj, "test-global");
        let value = ECMAScriptValue::from("sentinel string for environment test");
        env.create_immutable_binding(JSString::from("test_var"), true).unwrap();
        env.initialize_binding(&JSString::from("test_var"), value.clone()).unwrap();
        let reference = Reference::new(Base::Environment(Rc::new(env)), "test_var", true, None);

        let result = get_value(Ok(NormalCompletion::from(reference))).unwrap();
        assert_eq!(result, value);
    }

    #[test]
    #[should_panic(expected = "Bad completion type for get_value")]
    fn iter_record() {
        setup_test_agent();
        let ir = create_list_iterator_record(vec![1.into(), 2.into()]);
        let completion = Ok(NormalCompletion::from(ir));
        get_value(completion).unwrap();
    }
}

mod put_value {
    use super::*;
    use test_case::test_case;

    #[test]
    fn err_v() {
        setup_test_agent();
        let v = create_type_error("Error in V");
        let w = create_type_error("Error in W");
        let result = put_value(Err(v), Err(w)).unwrap_err();

        assert_eq!(unwind_type_error(result), "Error in V");
    }
    #[test]
    fn err_w() {
        setup_test_agent();
        let v = ECMAScriptValue::Undefined;
        let w = create_type_error("Error in W");
        let result = put_value(Ok(NormalCompletion::from(v)), Err(w)).unwrap_err();

        assert_eq!(unwind_type_error(result), "Error in W");
    }
    #[test]
    fn bad_lhs() {
        setup_test_agent();
        let result = put_value(Ok(NormalCompletion::from(ECMAScriptValue::Undefined)), Ok(ECMAScriptValue::Undefined))
            .unwrap_err();

        assert_eq!(unwind_reference_error(result), "Invalid Reference");
    }
    #[test]
    fn unresolvable_strict() {
        setup_test_agent();
        let reference = Reference::new(Base::Unresolvable, "blue", true, None);
        let result = put_value(Ok(NormalCompletion::from(reference)), Ok(ECMAScriptValue::Undefined)).unwrap_err();
        assert_eq!(unwind_reference_error(result), "Unknown reference");
    }
    #[test]
    fn unresolvable_nonstrict() {
        setup_test_agent();
        let value = ECMAScriptValue::from("Test Value for Unresolvable, non-strict writes");
        let reference = Reference::new(Base::Unresolvable, "blue", false, None);
        put_value(Ok(NormalCompletion::from(reference)), Ok(value.clone())).unwrap();
        let global = get_global_object().unwrap();
        let from_global = global.get(&PropertyKey::from("blue")).unwrap();
        assert_eq!(from_global, value);
    }
    #[test]
    fn unresolvable_throws() {
        setup_test_agent();
        let value = ECMAScriptValue::from("Test Value");
        let reference = Reference::new(Base::Unresolvable, "thrower", false, None);
        let thrower = ECMAScriptValue::from(intrinsic(IntrinsicId::ThrowTypeError));
        let global = get_global_object().unwrap();
        define_property_or_throw(
            &global,
            PropertyKey::from("thrower"),
            PotentialPropertyDescriptor {
                get: Some(thrower.clone()),
                set: Some(thrower),
                enumerable: Some(false),
                configurable: Some(true),
                ..Default::default()
            },
        )
        .unwrap();

        let result = put_value(Ok(NormalCompletion::from(reference)), Ok(value)).unwrap_err();
        assert_eq!(unwind_type_error(result), "Generic TypeError");
    }
    #[test]
    fn private() {
        setup_test_agent();
        let value = ECMAScriptValue::from("In my younger and more vulnerable years");
        let pn = PrivateName::new("test name");
        let object_proto = intrinsic(IntrinsicId::ObjectPrototype);
        let normal_object = ordinary_object_create(Some(object_proto), &[]);
        private_field_add(&normal_object, pn.clone(), ECMAScriptValue::Undefined).unwrap();
        let reference =
            Reference::new(Base::Value(ECMAScriptValue::from(normal_object.clone())), pn.clone(), true, None);

        put_value(Ok(NormalCompletion::from(reference)), Ok(value.clone())).unwrap();

        let from_private = private_get(&normal_object, &pn).unwrap();
        assert_eq!(from_private, value);
    }

    #[test]
    fn bad_value() {
        setup_test_agent();
        let reference = Reference::new(Base::Value(ECMAScriptValue::Undefined), "test", true, None);
        let result = put_value(Ok(NormalCompletion::from(reference)), Ok(ECMAScriptValue::Null)).unwrap_err();
        assert_eq!(unwind_type_error(result), "Undefined and null cannot be converted to objects");
    }
    #[test]
    fn ordinary() {
        setup_test_agent();
        let value = ECMAScriptValue::from("my father gave me some advice");
        let object_proto = intrinsic(IntrinsicId::ObjectPrototype);
        let normal_object = ordinary_object_create(Some(object_proto), &[]);
        let key = PropertyKey::from("phrase");
        let reference =
            Reference::new(Base::Value(ECMAScriptValue::from(normal_object.clone())), key.clone(), true, None);

        put_value(Ok(NormalCompletion::from(reference)), Ok(value.clone())).unwrap();

        let from_object = normal_object.get(&key).unwrap();
        assert_eq!(from_object, value);
    }
    #[test]
    fn object_throws() {
        setup_test_agent();
        let value = ECMAScriptValue::from("that I’ve been turning over in my mind ever since.");
        let thrower = ECMAScriptValue::from(intrinsic(IntrinsicId::ThrowTypeError));
        let object_proto = intrinsic(IntrinsicId::ObjectPrototype);
        let normal_object = ordinary_object_create(Some(object_proto), &[]);
        let key = PropertyKey::from("phrase");
        let reference =
            Reference::new(Base::Value(ECMAScriptValue::from(normal_object.clone())), key.clone(), true, None);
        define_property_or_throw(
            &normal_object,
            key,
            PotentialPropertyDescriptor {
                set: Some(thrower),
                enumerable: Some(false),
                configurable: Some(true),
                ..Default::default()
            },
        )
        .unwrap();

        let result = put_value(Ok(NormalCompletion::from(reference)), Ok(value)).unwrap_err();
        assert_eq!(unwind_type_error(result), "Generic TypeError");
    }
    #[test_case(false => Ok(()); "non-strict")]
    #[test_case(true => Err(String::from("Invalid Assignment Target")); "strict")]
    fn immutable(strict: bool) -> Result<(), String> {
        setup_test_agent();
        let value = ECMAScriptValue::from("“Whenever you feel like criticizing anyone,”");
        let object_proto = intrinsic(IntrinsicId::ObjectPrototype);
        let normal_object = ordinary_object_create(Some(object_proto), &[]);
        let key = PropertyKey::from("phrase");
        define_property_or_throw(
            &normal_object,
            key.clone(),
            PotentialPropertyDescriptor {
                value: Some(ECMAScriptValue::Undefined),
                writable: Some(false),
                enumerable: Some(true),
                configurable: Some(true),
                ..Default::default()
            },
        )
        .unwrap();
        let reference =
            Reference::new(Base::Value(ECMAScriptValue::from(normal_object.clone())), key.clone(), strict, None);

        let r = put_value(Ok(NormalCompletion::from(reference)), Ok(value)).map_err(unwind_type_error);

        let from_obj = normal_object.get(&key).unwrap();
        assert_eq!(from_obj, ECMAScriptValue::Undefined);

        r
    }
    #[test]
    fn environment() {
        setup_test_agent();
        let value = ECMAScriptValue::from(
            "he told me, “just remember that all the people in this world haven’t had the advantages that you’ve had.”",
        );
        let der = Rc::new(DeclarativeEnvironmentRecord::new(None, "test"));
        let key = JSString::from("env_test");
        der.create_mutable_binding(key.clone(), true).unwrap();
        der.initialize_binding(&key, ECMAScriptValue::Undefined).unwrap();
        let reference = Reference::new(Base::Environment(der.clone()), key.clone(), true, None);

        put_value(Ok(NormalCompletion::from(reference)), Ok(value.clone())).unwrap();

        let from_env = der.get_binding_value(&key, true).unwrap();
        assert_eq!(from_env, value);
    }

    #[test]
    #[should_panic(expected = "Bad completion type for put_value")]
    fn iter_record() {
        setup_test_agent();
        let ir = create_list_iterator_record(vec![1.into(), 2.into()]);
        let completion = Ok(NormalCompletion::from(ir));
        put_value(completion, Ok(ECMAScriptValue::Undefined)).unwrap();
    }
}

mod initialize_referenced_binding {
    use super::*;

    #[test]
    fn err_v() {
        setup_test_agent();
        let v = create_type_error("Error in V");
        let w = create_type_error("Error in W");
        let result = initialize_referenced_binding(Err(v), Err(w)).unwrap_err();

        assert_eq!(unwind_type_error(result), "Error in V");
    }
    #[test]
    fn err_w() {
        setup_test_agent();
        let v = ECMAScriptValue::Undefined;
        let w = create_type_error("Error in W");
        let result = initialize_referenced_binding(Ok(NormalCompletion::from(v)), Err(w)).unwrap_err();

        assert_eq!(unwind_type_error(result), "Error in W");
    }
    #[test]
    fn happy() {
        setup_test_agent();
        let key = JSString::from("variable");
        let env = Rc::new(DeclarativeEnvironmentRecord::new(None, "test"));
        env.create_mutable_binding(key.clone(), true).unwrap();
        let reference = Reference::new(Base::Environment(env.clone()), key.clone(), true, None);
        let value = ECMAScriptValue::from("There was so much to read, for one thing,");

        initialize_referenced_binding(Ok(NormalCompletion::from(reference)), Ok(value.clone())).unwrap();

        let from_env = env.get_binding_value(&key, true).unwrap();
        assert_eq!(from_env, value);
    }
    #[test]
    #[should_panic(expected = "unreachable code")]
    fn value_ref() {
        setup_test_agent();
        let reference = Reference::new(Base::Value(ECMAScriptValue::Undefined), "phrase", true, None);
        initialize_referenced_binding(Ok(NormalCompletion::from(reference)), Ok(ECMAScriptValue::Undefined)).unwrap();
    }
    #[test]
    #[should_panic(expected = "unreachable code")]
    fn unresolveable_ref() {
        setup_test_agent();
        let reference = Reference::new(Base::Unresolvable, "phrase", true, None);
        initialize_referenced_binding(Ok(NormalCompletion::from(reference)), Ok(ECMAScriptValue::Undefined)).unwrap();
    }
    #[test]
    #[should_panic(expected = "unreachable code")]
    fn value() {
        setup_test_agent();
        initialize_referenced_binding(
            Ok(NormalCompletion::from(ECMAScriptValue::Undefined)),
            Ok(ECMAScriptValue::Undefined),
        )
        .unwrap();
    }
}
