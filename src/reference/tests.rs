use super::*;
use crate::environment_record::{DeclarativeEnvironmentRecord, GlobalEnvironmentRecord};
use crate::object::{define_property_or_throw, get, ordinary_object_create, private_field_add, PotentialPropertyDescriptor};
use crate::realm::IntrinsicId;
use crate::tests::{test_agent, unwind_reference_error, unwind_type_error};
use crate::values::{PrivateName, PropertyKey};

mod base {
    use super::*;

    #[test]
    fn debug() {
        let b = Base::Value(ECMAScriptValue::from(33));
        assert_ne!(format!("{:?}", b), "");
    }
}

mod referenced_name {
    use super::*;

    #[test]
    fn debug() {
        let rn = ReferencedName::String(JSString::from("apple"));
        assert_ne!(format!("{:?}", rn), "");
    }

    mod partial_eq {
        use super::*;

        fn setup() -> (ReferencedName, ReferencedName, ReferencedName) {
            (ReferencedName::String(JSString::from("apple")), ReferencedName::PrivateName(PrivateName::new("apple")), ReferencedName::String(JSString::from("apple")))
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
            let mut agent = test_agent();
            let sym = Symbol::new(&mut agent, None);
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
                let mut agent = test_agent();
                let sym = Symbol::new(&mut agent, Some(JSString::from("crazy")));
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
                let mut agent = test_agent();
                let sym = Symbol::new(&mut agent, None);
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
                let mut agent = test_agent();
                let sym = Symbol::new(&mut agent, None);
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
}

mod reference {
    use super::*;

    #[test]
    fn debug() {
        let r = Reference { base: Base::Unresolvable, referenced_name: ReferencedName::String(JSString::from("name")), strict: false, this_value: None };
        assert_ne!(format!("{:?}", r), "");
    }

    #[test]
    fn new() {
        let r = Reference::new(Base::Unresolvable, JSString::from("anobject"), false, Some(ECMAScriptValue::from(999)));
        assert!(matches!(r.base, Base::Unresolvable));
        assert_eq!(r.referenced_name, ReferencedName::from("anobject"));
        assert_eq!(r.strict, false);
        assert_eq!(r.this_value, Some(ECMAScriptValue::from(999)));
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
            assert!(!Reference::new(Base::Value(ECMAScriptValue::from(10)), "blue", false, None).is_unresolvable_reference());
        }
    }

    mod is_super_reference {
        use super::*;

        #[test]
        fn has_this() {
            assert!(Reference::new(Base::Value(ECMAScriptValue::from(10)), "a", false, Some(ECMAScriptValue::from(1))).is_super_reference());
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
            assert!(Reference::new(Base::Value(ECMAScriptValue::from(10)), PrivateName::new("a"), false, None).is_private_reference());
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
            let mut agent = test_agent();
            let object_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
            let normal_object = ordinary_object_create(&mut agent, Some(object_proto), &[]);
            let this_value = ECMAScriptValue::from(normal_object);
            let value = ECMAScriptValue::from("Gatsby turned out all right at the end");
            let reference = Reference::new(Base::Value(value), "phrase", true, Some(this_value.clone()));
            assert_eq!(reference.get_this_value(), this_value);
        }
        #[test]
        #[should_panic]
        fn unresolvable() {
            Reference::new(Base::Unresolvable, "blurp", true, None).get_this_value();
        }
        #[test]
        #[should_panic]
        fn environment() {
            Reference::new(Base::Environment(Rc::new(DeclarativeEnvironmentRecord::new(None))), "blurp", true, None).get_this_value();
        }
    }
}

mod super_value {
    use super::*;

    #[test]
    fn debug() {
        let sv = SuperValue::Value(ECMAScriptValue::from(10));
        assert_ne!(format!("{:?}", sv), "");
    }

    mod from {
        use super::*;
        #[test]
        fn value() {
            let v = ECMAScriptValue::from("blue");
            let sv = SuperValue::from(v);
            match sv {
                SuperValue::Value(vv) => assert_eq!(vv, ECMAScriptValue::from("blue")),
                SuperValue::Reference(_) => unreachable!(),
            }
        }
        #[test]
        fn reference() {
            let r = Reference::new(Base::Unresolvable, "blue", false, None);
            let sv = SuperValue::from(r);
            match sv {
                SuperValue::Value(_) => unreachable!(),
                SuperValue::Reference(r) => {
                    assert!(matches!(r.base, Base::Unresolvable));
                    assert_eq!(r.referenced_name, ReferencedName::String(JSString::from("blue")));
                    assert_eq!(r.strict, false);
                    assert_eq!(r.this_value, None);
                }
            }
        }
    }
}

mod get_value {
    use super::*;

    #[test]
    fn abrupt() {
        let mut agent = test_agent();
        let err = create_type_error(&mut agent, "Test Path");
        let result = get_value(&mut agent, Err(err)).unwrap_err();
        assert_eq!(unwind_type_error(&mut agent, result), "Test Path");
    }

    #[test]
    fn simple_value() {
        let mut agent = test_agent();
        let val = ECMAScriptValue::from("a value");
        let result = get_value(&mut agent, Ok(SuperValue::from(val))).unwrap();
        assert_eq!(result, ECMAScriptValue::from("a value"));
    }

    #[test]
    fn unresolvable() {
        let mut agent = test_agent();
        let badref = Reference::new(Base::Unresolvable, "no_ref", true, None);
        let result = get_value(&mut agent, Ok(SuperValue::from(badref))).unwrap_err();
        assert_eq!(unwind_reference_error(&mut agent, result), "Unresolvable Reference");
    }
    #[test]
    fn value_base() {
        let mut agent = test_agent();
        let object_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let normal_object = ordinary_object_create(&mut agent, Some(object_proto), &[]);
        let value = ECMAScriptValue::from("value_base test value");
        let descriptor = PotentialPropertyDescriptor { writable: Some(true), enumerable: Some(true), configurable: Some(true), value: Some(value.clone()), ..Default::default() };
        define_property_or_throw(&mut agent, &normal_object, PropertyKey::from("test_value"), descriptor).unwrap();
        let reference = Reference::new(Base::Value(ECMAScriptValue::from(normal_object)), "test_value", true, None);

        let result = get_value(&mut agent, Ok(SuperValue::from(reference))).unwrap();
        assert_eq!(result, value);
    }
    #[test]
    fn to_object_err() {
        let mut agent = test_agent();
        let reference = Reference::new(Base::Value(ECMAScriptValue::Undefined), "test_value", true, None);
        let result = get_value(&mut agent, Ok(SuperValue::from(reference))).unwrap_err();
        assert_eq!(unwind_type_error(&mut agent, result), "Undefined and null cannot be converted to objects");
    }
    #[test]
    fn private() {
        let mut agent = test_agent();
        let pn = PrivateName::new("test name");
        let object_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let normal_object = ordinary_object_create(&mut agent, Some(object_proto), &[]);
        let value = ECMAScriptValue::from("test value for private identifier");
        private_field_add(&mut agent, &normal_object, pn.clone(), value.clone()).unwrap();
        let reference = Reference::new(Base::Value(ECMAScriptValue::from(normal_object)), pn, true, None);
        let result = get_value(&mut agent, Ok(SuperValue::from(reference))).unwrap();
        assert_eq!(result, value);
    }
    #[test]
    fn environment() {
        let mut agent = test_agent();
        let object_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let global = ordinary_object_create(&mut agent, Some(object_proto), &[]);
        let this_obj = global.clone();
        let env = GlobalEnvironmentRecord::new(global, this_obj);
        let value = ECMAScriptValue::from("sentinel string for environment test");
        env.create_immutable_binding(&mut agent, JSString::from("test_var"), true).unwrap();
        env.initialize_binding(&mut agent, &JSString::from("test_var"), value.clone()).unwrap();
        let reference = Reference::new(Base::Environment(Rc::new(env)), "test_var", true, None);

        let result = get_value(&mut agent, Ok(SuperValue::from(reference))).unwrap();
        assert_eq!(result, value);
    }
}

mod put_value {
    use super::*;
    use test_case::test_case;

    #[test]
    fn err_v() {
        let mut agent = test_agent();
        let v = create_type_error(&mut agent, "Error in V");
        let w = create_type_error(&mut agent, "Error in W");
        let result = put_value(&mut agent, Err(v), Err(w)).unwrap_err();

        assert_eq!(unwind_type_error(&mut agent, result), "Error in V");
    }
    #[test]
    fn err_w() {
        let mut agent = test_agent();
        let v = ECMAScriptValue::Undefined;
        let w = create_type_error(&mut agent, "Error in W");
        let result = put_value(&mut agent, Ok(SuperValue::from(v)), Err(w)).unwrap_err();

        assert_eq!(unwind_type_error(&mut agent, result), "Error in W");
    }
    #[test]
    fn bad_lhs() {
        let mut agent = test_agent();
        let result = put_value(&mut agent, Ok(SuperValue::from(ECMAScriptValue::Undefined)), Ok(ECMAScriptValue::Undefined)).unwrap_err();

        assert_eq!(unwind_reference_error(&mut agent, result), "Invalid Reference");
    }
    #[test]
    fn unresolvable_strict() {
        let mut agent = test_agent();
        let reference = Reference::new(Base::Unresolvable, "blue", true, None);
        let result = put_value(&mut agent, Ok(SuperValue::from(reference)), Ok(ECMAScriptValue::Undefined)).unwrap_err();
        assert_eq!(unwind_reference_error(&mut agent, result), "Unknown reference");
    }
    #[test]
    fn unresolvable_nonstrict() {
        let mut agent = test_agent();
        let value = ECMAScriptValue::from("Test Value for Unresolvable, non-strict writes");
        let reference = Reference::new(Base::Unresolvable, "blue", false, None);
        put_value(&mut agent, Ok(SuperValue::from(reference)), Ok(value.clone())).unwrap();
        let global = get_global_object(&mut agent).unwrap();
        let from_global = get(&mut agent, &global, &PropertyKey::from("blue")).unwrap();
        assert_eq!(from_global, value);
    }
    #[test]
    fn unresolvable_throws() {
        let mut agent = test_agent();
        let value = ECMAScriptValue::from("Test Value");
        let reference = Reference::new(Base::Unresolvable, "thrower", false, None);
        let thrower = ECMAScriptValue::from(agent.intrinsic(IntrinsicId::ThrowTypeError));
        let global = get_global_object(&mut agent).unwrap();
        define_property_or_throw(
            &mut agent,
            &global,
            PropertyKey::from("thrower"),
            PotentialPropertyDescriptor { get: Some(thrower.clone()), set: Some(thrower), enumerable: Some(false), configurable: Some(true), ..Default::default() },
        )
        .unwrap();

        let result = put_value(&mut agent, Ok(SuperValue::from(reference)), Ok(value)).unwrap_err();
        assert_eq!(unwind_type_error(&mut agent, result), "Generic TypeError");
    }
    #[test]
    fn private() {
        let mut agent = test_agent();
        let value = ECMAScriptValue::from("In my younger and more vulnerable years");
        let pn = PrivateName::new("test name");
        let object_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let normal_object = ordinary_object_create(&mut agent, Some(object_proto), &[]);
        private_field_add(&mut agent, &normal_object, pn.clone(), ECMAScriptValue::Undefined).unwrap();
        let reference = Reference::new(Base::Value(ECMAScriptValue::from(normal_object.clone())), pn.clone(), true, None);

        put_value(&mut agent, Ok(SuperValue::from(reference)), Ok(value.clone())).unwrap();

        let from_private = private_get(&mut agent, &normal_object, &pn).unwrap();
        assert_eq!(from_private, value);
    }

    #[test]
    fn bad_value() {
        let mut agent = test_agent();
        let reference = Reference::new(Base::Value(ECMAScriptValue::Undefined), "test", true, None);
        let result = put_value(&mut agent, Ok(SuperValue::from(reference)), Ok(ECMAScriptValue::Null)).unwrap_err();
        assert_eq!(unwind_type_error(&mut agent, result), "Undefined and null cannot be converted to objects");
    }
    #[test]
    fn ordinary() {
        let mut agent = test_agent();
        let value = ECMAScriptValue::from("my father gave me some advice");
        let object_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let normal_object = ordinary_object_create(&mut agent, Some(object_proto), &[]);
        let key = PropertyKey::from("phrase");
        let reference = Reference::new(Base::Value(ECMAScriptValue::from(normal_object.clone())), key.clone(), true, None);

        put_value(&mut agent, Ok(SuperValue::from(reference)), Ok(value.clone())).unwrap();

        let from_object = get(&mut agent, &normal_object, &key).unwrap();
        assert_eq!(from_object, value);
    }
    #[test]
    fn object_throws() {
        let mut agent = test_agent();
        let value = ECMAScriptValue::from("that I’ve been turning over in my mind ever since.");
        let thrower = ECMAScriptValue::from(agent.intrinsic(IntrinsicId::ThrowTypeError));
        let object_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let normal_object = ordinary_object_create(&mut agent, Some(object_proto), &[]);
        let key = PropertyKey::from("phrase");
        let reference = Reference::new(Base::Value(ECMAScriptValue::from(normal_object.clone())), key.clone(), true, None);
        define_property_or_throw(
            &mut agent,
            &normal_object,
            key,
            PotentialPropertyDescriptor { set: Some(thrower), enumerable: Some(false), configurable: Some(true), ..Default::default() },
        )
        .unwrap();

        let result = put_value(&mut agent, Ok(SuperValue::from(reference)), Ok(value)).unwrap_err();
        assert_eq!(unwind_type_error(&mut agent, result), "Generic TypeError");
    }
    #[test_case(false => Ok(()); "non-strict")]
    #[test_case(true => Err(String::from("Invalid Assignment Target")); "strict")]
    fn immutable(strict: bool) -> Result<(), String> {
        let mut agent = test_agent();
        let value = ECMAScriptValue::from("“Whenever you feel like criticizing anyone,”");
        let object_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let normal_object = ordinary_object_create(&mut agent, Some(object_proto), &[]);
        let key = PropertyKey::from("phrase");
        define_property_or_throw(
            &mut agent,
            &normal_object,
            key.clone(),
            PotentialPropertyDescriptor { value: Some(ECMAScriptValue::Undefined), writable: Some(false), enumerable: Some(true), configurable: Some(true), ..Default::default() },
        )
        .unwrap();
        let reference = Reference::new(Base::Value(ECMAScriptValue::from(normal_object.clone())), key.clone(), strict, None);

        let r = put_value(&mut agent, Ok(SuperValue::from(reference)), Ok(value)).map_err(|ac| unwind_type_error(&mut agent, ac));

        let from_obj = get(&mut agent, &normal_object, &key).unwrap();
        assert_eq!(from_obj, ECMAScriptValue::Undefined);

        r
    }
    #[test]
    fn environment() {
        let mut agent = test_agent();
        let value = ECMAScriptValue::from("he told me, “just remember that all the people in this world haven’t had the advantages that you’ve had.”");
        let der = Rc::new(DeclarativeEnvironmentRecord::new(None));
        let key = JSString::from("env_test");
        der.create_mutable_binding(&mut agent, key.clone(), true).unwrap();
        der.initialize_binding(&mut agent, &key, ECMAScriptValue::Undefined).unwrap();
        let reference = Reference::new(Base::Environment(der.clone()), key.clone(), true, None);

        put_value(&mut agent, Ok(SuperValue::from(reference)), Ok(value.clone())).unwrap();

        let from_env = der.get_binding_value(&mut agent, &key, true).unwrap();
        assert_eq!(from_env, value);
    }
}

mod initialize_referenced_binding {
    use super::*;

    #[test]
    fn err_v() {
        let mut agent = test_agent();
        let v = create_type_error(&mut agent, "Error in V");
        let w = create_type_error(&mut agent, "Error in W");
        let result = initialize_referenced_binding(&mut agent, Err(v), Err(w)).unwrap_err();

        assert_eq!(unwind_type_error(&mut agent, result), "Error in V");
    }
    #[test]
    fn err_w() {
        let mut agent = test_agent();
        let v = ECMAScriptValue::Undefined;
        let w = create_type_error(&mut agent, "Error in W");
        let result = initialize_referenced_binding(&mut agent, Ok(SuperValue::from(v)), Err(w)).unwrap_err();

        assert_eq!(unwind_type_error(&mut agent, result), "Error in W");
    }
    #[test]
    fn happy() {
        let mut agent = test_agent();
        let key = JSString::from("variable");
        let env = Rc::new(DeclarativeEnvironmentRecord::new(None));
        env.create_mutable_binding(&mut agent, key.clone(), true).unwrap();
        let reference = Reference::new(Base::Environment(env.clone()), key.clone(), true, None);
        let value = ECMAScriptValue::from("There was so much to read, for one thing,");

        initialize_referenced_binding(&mut agent, Ok(SuperValue::from(reference)), Ok(value.clone())).unwrap();

        let from_env = env.get_binding_value(&mut agent, &key, true).unwrap();
        assert_eq!(from_env, value);
    }
    #[test]
    #[should_panic]
    fn value_ref() {
        let mut agent = test_agent();
        let reference = Reference::new(Base::Value(ECMAScriptValue::Undefined), "phrase", true, None);
        initialize_referenced_binding(&mut agent, Ok(SuperValue::from(reference)), Ok(ECMAScriptValue::Undefined)).unwrap();
    }
    #[test]
    #[should_panic]
    fn unresolveable_ref() {
        let mut agent = test_agent();
        let reference = Reference::new(Base::Unresolvable, "phrase", true, None);
        initialize_referenced_binding(&mut agent, Ok(SuperValue::from(reference)), Ok(ECMAScriptValue::Undefined)).unwrap();
    }
    #[test]
    #[should_panic]
    fn value() {
        let mut agent = test_agent();
        initialize_referenced_binding(&mut agent, Ok(SuperValue::from(ECMAScriptValue::Undefined)), Ok(ECMAScriptValue::Undefined)).unwrap();
    }
}