use super::*;
use crate::parser::testhelp::*;
use crate::tests::*;

const ALL_REMOVABILITY: &[Removability] = &[Removability::Deletable, Removability::Permanent];

mod removability {
    use super::*;
    #[test]
    fn debug() {
        for val in ALL_REMOVABILITY {
            assert_ne!(format!("{val:?}"), "");
        }
    }
    #[test]
    fn eq() {
        for (right_idx, right_value) in ALL_REMOVABILITY.iter().enumerate() {
            for (left_idx, left_value) in ALL_REMOVABILITY.iter().enumerate() {
                assert_eq!(*left_value == *right_value, left_idx == right_idx);
            }
        }
    }
    #[test]
    fn from() {
        assert_eq!(Removability::from(true), Removability::Deletable);
        assert_eq!(Removability::from(false), Removability::Permanent);
    }
    #[test]
    #[allow(clippy::clone_on_copy)]
    fn clone() {
        let r1 = Removability::Deletable;
        let r2 = r1.clone();
        assert_eq!(r1, r2);
    }
}

const ALL_STRICTNESS: &[Strictness] = &[Strictness::Strict, Strictness::Sloppy];

mod strictness {
    use super::*;
    #[test]
    fn debug() {
        for val in ALL_STRICTNESS {
            assert_ne!(format!("{val:?}"), "");
        }
    }
    #[test]
    fn eq() {
        for (right_idx, right_value) in ALL_STRICTNESS.iter().enumerate() {
            for (left_idx, left_value) in ALL_STRICTNESS.iter().enumerate() {
                assert_eq!(*left_value == *right_value, left_idx == right_idx);
            }
        }
    }
    #[test]
    fn from() {
        assert_eq!(Strictness::from(true), Strictness::Strict);
        assert_eq!(Strictness::from(false), Strictness::Sloppy);
    }
    #[test]
    #[allow(clippy::clone_on_copy)]
    fn clone() {
        let s1 = Strictness::Sloppy;
        let s2 = s1.clone();
        assert_eq!(s1, s2);
    }
}

mod mutability {
    use super::*;
    #[test]
    fn debug() {
        for r in ALL_REMOVABILITY {
            assert_ne!(format!("{:?}", Mutability::Mutable(*r)), "");
        }
        for s in ALL_STRICTNESS {
            assert_ne!(format!("{:?}", Mutability::Immutable(*s)), "");
        }
    }
    #[test]
    fn eq() {
        for left in ALL_REMOVABILITY {
            for right in ALL_REMOVABILITY {
                assert_eq!(Mutability::Mutable(*left) == Mutability::Mutable(*right), left == right);
            }
            for right in ALL_STRICTNESS {
                assert_eq!(Mutability::Mutable(*left) == Mutability::Immutable(*right), false);
            }
        }
        for left in ALL_STRICTNESS {
            for right in ALL_REMOVABILITY {
                assert_eq!(Mutability::Immutable(*left) == Mutability::Mutable(*right), false);
            }
            for right in ALL_STRICTNESS {
                assert_eq!(Mutability::Immutable(*left) == Mutability::Immutable(*right), left == right);
            }
        }
    }
    #[test]
    fn ne() {
        for left in ALL_REMOVABILITY {
            for right in ALL_REMOVABILITY {
                assert_eq!(Mutability::Mutable(*left) != Mutability::Mutable(*right), left != right);
            }
            for right in ALL_STRICTNESS {
                assert_eq!(Mutability::Mutable(*left) != Mutability::Immutable(*right), true);
            }
        }
        for left in ALL_STRICTNESS {
            for right in ALL_REMOVABILITY {
                assert_eq!(Mutability::Immutable(*left) != Mutability::Mutable(*right), true);
            }
            for right in ALL_STRICTNESS {
                assert_eq!(Mutability::Immutable(*left) != Mutability::Immutable(*right), left != right);
            }
        }
    }
    #[test]
    #[allow(clippy::clone_on_copy)]
    fn clone() {
        let m1 = Mutability::Immutable(Strictness::Sloppy);
        let m2 = m1.clone();
        assert_eq!(m1, m2);
    }
}

mod binding {
    use super::*;
    #[test]
    fn debug() {
        assert_ne!(
            format!(
                "{:?}",
                Binding { value: Some(ECMAScriptValue::Null), mutability: Mutability::Immutable(Strictness::Sloppy) }
            ),
            ""
        );
    }
}

mod declarative_environment_record {
    use super::*;
    use test_case::test_case;

    #[test]
    fn debug() {
        let der = DeclarativeEnvironmentRecord::new(None, "test");
        assert_ne!(format!("{der:?}"), "");
    }
    #[test]
    fn fancy_debug() {
        let der = DeclarativeEnvironmentRecord::new(None, "test");
        assert_ne!(format!("{der:#?}"), "");
    }

    #[test_case("&str"; "string slice")]
    #[test_case(JSString::from("JSString"); "JSString")]
    fn new(name: impl Into<String> + Clone) {
        setup_test_agent();
        let name_dup: String = name.clone().into();
        let global_env = current_realm_record().unwrap().borrow().global_env.clone().unwrap();
        let der = DeclarativeEnvironmentRecord::new(Some(global_env.clone()), name);
        assert_eq!(der.outer_env.unwrap().name(), global_env.name());
        assert_eq!(der.name, name_dup);
        assert!(der.bindings.borrow().is_empty());
    }

    #[test_case("bob" => "bob"; "typical")]
    fn name(name: &str) -> String {
        let der = DeclarativeEnvironmentRecord::new(None, name);
        der.name()
    }

    #[test]
    fn has_binding() {
        setup_test_agent();
        let der = DeclarativeEnvironmentRecord::new(None, "test");
        der.create_mutable_binding(JSString::from("a"), true).unwrap();

        assert_eq!(der.has_binding(&JSString::from("a")).unwrap(), true);
        assert_eq!(der.has_binding(&JSString::from("b")).unwrap(), false);
    }

    #[test]
    fn create_mutable_binding() {
        setup_test_agent();
        let der = DeclarativeEnvironmentRecord::new(None, "test");

        der.create_mutable_binding(JSString::from("a"), true).unwrap();
        der.create_mutable_binding(JSString::from("b"), false).unwrap();

        // Poke in the internals
        let bindings = der.bindings.borrow();
        let binding = bindings.get(&JSString::from("a")).unwrap();
        assert!(binding.value.is_none());
        assert_eq!(binding.mutability, Mutability::Mutable(Removability::Deletable));
        let binding = bindings.get(&JSString::from("b")).unwrap();
        assert!(binding.value.is_none());
        assert_eq!(binding.mutability, Mutability::Mutable(Removability::Permanent));
    }
    #[test]
    fn create_immmutable_binding() {
        setup_test_agent();
        let der = DeclarativeEnvironmentRecord::new(None, "test");

        der.create_immutable_binding(JSString::from("a"), true).unwrap();
        der.create_immutable_binding(JSString::from("b"), false).unwrap();

        // Poke in the internals
        let bindings = der.bindings.borrow();
        let binding = bindings.get(&JSString::from("a")).unwrap();
        assert!(binding.value.is_none());
        assert_eq!(binding.mutability, Mutability::Immutable(Strictness::Strict));
        let binding = bindings.get(&JSString::from("b")).unwrap();
        assert!(binding.value.is_none());
        assert_eq!(binding.mutability, Mutability::Immutable(Strictness::Sloppy));
    }

    #[test]
    fn initialize_binding() {
        setup_test_agent();
        let der = DeclarativeEnvironmentRecord::new(None, "test");
        der.create_immutable_binding(JSString::from("a"), true).unwrap();
        der.create_mutable_binding(JSString::from("b"), true).unwrap();

        der.initialize_binding(&JSString::from("a"), ECMAScriptValue::from("value")).unwrap();
        der.initialize_binding(&JSString::from("b"), ECMAScriptValue::from("other")).unwrap();

        let bindings = der.bindings.borrow();
        let binding = bindings.get(&JSString::from("a")).unwrap();
        assert_eq!(binding.value, Some(ECMAScriptValue::from("value")));
        let binding = bindings.get(&JSString::from("b")).unwrap();
        assert_eq!(binding.value, Some(ECMAScriptValue::from("other")));
    }
    #[test]
    fn set_mutable_binding_01() {
        setup_test_agent();
        let der = DeclarativeEnvironmentRecord::new(None, "test");

        let err = der.set_mutable_binding(JSString::from("a"), ECMAScriptValue::from(10), true).unwrap_err();
        let msg = unwind_reference_error(err);
        assert_eq!(msg, "Identifier not defined");
    }
    #[test]
    fn set_mutable_binding_02() {
        setup_test_agent();
        let der = DeclarativeEnvironmentRecord::new(None, "test");

        der.set_mutable_binding(JSString::from("a"), ECMAScriptValue::from(10), false).unwrap();

        let bindings = der.bindings.borrow();
        let binding = bindings.get(&JSString::from("a")).unwrap();
        assert_eq!(binding.value, Some(ECMAScriptValue::from(10)));
        assert_eq!(binding.mutability, Mutability::Mutable(Removability::Deletable));
    }
    #[test]
    fn set_mutable_binding_03() {
        setup_test_agent();
        let der = DeclarativeEnvironmentRecord::new(None, "test");
        der.create_immutable_binding(JSString::from("a"), true).unwrap();

        let err = der.set_mutable_binding(JSString::from("a"), ECMAScriptValue::from(10), false).unwrap_err();
        let msg = unwind_reference_error(err);
        assert_eq!(msg, "Binding not initialized");
    }
    #[test]
    fn set_mutable_binding_04() {
        setup_test_agent();
        let der = DeclarativeEnvironmentRecord::new(None, "test");
        der.create_immutable_binding(JSString::from("a"), true).unwrap();
        der.initialize_binding(&JSString::from("a"), ECMAScriptValue::from(1)).unwrap();

        let err = der.set_mutable_binding(JSString::from("a"), ECMAScriptValue::from(10), false).unwrap_err();
        let msg = unwind_type_error(err);
        assert_eq!(msg, "Cannot change read-only value");
    }
    #[test]
    fn set_mutable_binding_05() {
        setup_test_agent();
        let der = DeclarativeEnvironmentRecord::new(None, "test");
        der.create_immutable_binding(JSString::from("a"), false).unwrap();
        der.initialize_binding(&JSString::from("a"), ECMAScriptValue::from(1)).unwrap();

        let err = der.set_mutable_binding(JSString::from("a"), ECMAScriptValue::from(10), true).unwrap_err();
        let msg = unwind_type_error(err);
        assert_eq!(msg, "Cannot change read-only value");
    }
    #[test]
    fn set_mutable_binding_06() {
        setup_test_agent();
        let der = DeclarativeEnvironmentRecord::new(None, "test");
        der.create_immutable_binding(JSString::from("a"), false).unwrap();
        der.initialize_binding(&JSString::from("a"), ECMAScriptValue::from(1)).unwrap();

        der.set_mutable_binding(JSString::from("a"), ECMAScriptValue::from(10), false).unwrap();

        let bindings = der.bindings.borrow();
        let binding = bindings.get(&JSString::from("a")).unwrap();
        assert_eq!(binding.value, Some(ECMAScriptValue::from(1)));
    }
    #[test]
    fn set_mutable_binding_07() {
        setup_test_agent();
        let der = DeclarativeEnvironmentRecord::new(None, "test");
        der.create_mutable_binding(JSString::from("a"), false).unwrap();
        der.initialize_binding(&JSString::from("a"), ECMAScriptValue::from(1)).unwrap();

        der.set_mutable_binding(JSString::from("a"), ECMAScriptValue::from(10), false).unwrap();

        let bindings = der.bindings.borrow();
        let binding = bindings.get(&JSString::from("a")).unwrap();
        assert_eq!(binding.value, Some(ECMAScriptValue::from(10)));
    }
    #[test]
    fn get_binding_value_01() {
        setup_test_agent();
        let der = DeclarativeEnvironmentRecord::new(None, "test");
        der.create_mutable_binding(JSString::from("a"), false).unwrap();
        der.initialize_binding(&JSString::from("a"), ECMAScriptValue::from(1)).unwrap();

        let result = der.get_binding_value(&JSString::from("a"), false).unwrap();

        assert_eq!(result, ECMAScriptValue::from(1));
    }
    #[test]
    fn get_binding_value_02() {
        setup_test_agent();
        let der = DeclarativeEnvironmentRecord::new(None, "test");
        der.create_mutable_binding(JSString::from("a"), false).unwrap();

        let result = der.get_binding_value(&JSString::from("a"), false).unwrap_err();
        let msg = unwind_reference_error(result);

        assert_eq!(msg, "Binding not initialized");
    }
    #[test]
    fn delete_binding_01() {
        setup_test_agent();
        let der = DeclarativeEnvironmentRecord::new(None, "test");
        der.create_mutable_binding(JSString::from("permanent"), false).unwrap();

        let result = der.delete_binding(&JSString::from("permanent")).unwrap();

        assert_eq!(result, false);
        assert!(der.has_binding(&JSString::from("permanent")).unwrap());
    }
    #[test]
    fn delete_binding_02() {
        setup_test_agent();
        let der = DeclarativeEnvironmentRecord::new(None, "test");
        der.create_mutable_binding(JSString::from("deletable"), true).unwrap();

        let result = der.delete_binding(&JSString::from("deletable")).unwrap();

        assert_eq!(result, true);
        assert!(!der.has_binding(&JSString::from("deletable")).unwrap());
    }
    #[test]
    fn delete_binding_03() {
        setup_test_agent();
        let der = DeclarativeEnvironmentRecord::new(None, "test");
        der.create_immutable_binding(JSString::from("immutable"), true).unwrap();

        let result = der.delete_binding(&JSString::from("immutable")).unwrap();

        assert_eq!(result, false);
        assert!(der.has_binding(&JSString::from("immutable")).unwrap());
    }
    #[test]
    fn has_this_binding() {
        let der = DeclarativeEnvironmentRecord::new(None, "test");

        assert_eq!(der.has_this_binding(), false);
    }
    #[test]
    fn has_super_binding() {
        let der = DeclarativeEnvironmentRecord::new(None, "test");

        assert_eq!(der.has_super_binding(), false);
    }
    #[test]
    fn with_base_object() {
        let der = DeclarativeEnvironmentRecord::new(None, "test");

        assert!(der.with_base_object().is_none());
    }
    #[test]
    fn get_outer_env() {
        setup_test_agent();
        let der = Rc::new(DeclarativeEnvironmentRecord::new(None, "test"));
        der.create_immutable_binding(JSString::from("sentinel"), true).unwrap();
        der.initialize_binding(&JSString::from("sentinel"), ECMAScriptValue::from("very unique string")).unwrap();
        let der2 = DeclarativeEnvironmentRecord::new(Some(der), "inner");

        let outer = der2.get_outer_env().unwrap();

        let val_from_outer = outer.get_binding_value(&JSString::from("sentinel"), true).unwrap();
        assert_eq!(val_from_outer, ECMAScriptValue::from("very unique string"));
    }

    #[test]
    #[should_panic(expected = "unreachable")]
    fn get_this_binding() {
        setup_test_agent();
        let der = DeclarativeEnvironmentRecord::new(None, "test");
        der.get_this_binding().unwrap();
    }

    #[test]
    #[should_panic(expected = "unreachable")]
    fn bind_this_value() {
        setup_test_agent();
        let der = DeclarativeEnvironmentRecord::new(None, "test");
        der.bind_this_value(ECMAScriptValue::Undefined).unwrap();
    }

    #[test]
    fn binding_names() {
        setup_test_agent();
        let der = DeclarativeEnvironmentRecord::new(None, "test");
        der.create_mutable_binding(JSString::from("a"), true).unwrap();
        der.create_mutable_binding(JSString::from("greasy"), true).unwrap();

        let mut names = der.binding_names();
        names.sort();
        assert_eq!(names, vec!["a", "greasy"]);
    }
}

mod object_environment_record {
    use super::*;
    #[test]
    fn debug() {
        setup_test_agent();
        let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let binding_object = ordinary_object_create(Some(object_prototype));
        let oer = ObjectEnvironmentRecord::new(binding_object, false, None, "test");

        assert_ne!(format!("{oer:#?}"), "");
        assert_ne!(format!("{oer:?}"), "");
    }
    #[test]
    fn has_binding_01() {
        setup_test_agent();
        let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let binding_object = ordinary_object_create(Some(object_prototype));
        let oer = ObjectEnvironmentRecord::new(binding_object, false, None, "test");

        let result = oer.has_binding(&JSString::from("not_here")).unwrap();
        assert_eq!(result, false);
    }
    #[test]
    fn has_binding_02() {
        setup_test_agent();
        let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let binding_object = ordinary_object_create(Some(object_prototype));
        define_property_or_throw(
            &binding_object,
            "exists",
            PotentialPropertyDescriptor {
                value: Some(ECMAScriptValue::from(true)),
                writable: Some(true),
                enumerable: Some(true),
                configurable: Some(true),
                ..Default::default()
            },
        )
        .unwrap();
        let oer = ObjectEnvironmentRecord::new(binding_object, false, None, "test");

        let result = oer.has_binding(&JSString::from("exists")).unwrap();
        assert_eq!(result, true);
    }
    #[test]
    fn has_binding_03() {
        setup_test_agent();
        let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let binding_object = ordinary_object_create(Some(object_prototype));
        define_property_or_throw(
            &binding_object,
            "exists",
            PotentialPropertyDescriptor {
                value: Some(ECMAScriptValue::from(true)),
                writable: Some(true),
                enumerable: Some(true),
                configurable: Some(true),
                ..Default::default()
            },
        )
        .unwrap();
        let oer = ObjectEnvironmentRecord::new(binding_object, true, None, "test");

        let result = oer.has_binding(&JSString::from("exists")).unwrap();

        assert_eq!(result, true);
    }
    #[test]
    fn has_binding_04() {
        setup_test_agent();
        let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
        // unscopables_obj = {
        //    hidden: 10,
        //    visible: false
        // }
        let unscopables_obj = ordinary_object_create(Some(object_prototype.clone()));
        define_property_or_throw(
            &unscopables_obj,
            "hidden",
            PotentialPropertyDescriptor {
                value: Some(ECMAScriptValue::from(10)),
                writable: Some(true),
                enumerable: Some(true),
                configurable: Some(true),
                ..Default::default()
            },
        )
        .unwrap();
        define_property_or_throw(
            &unscopables_obj,
            "visible",
            PotentialPropertyDescriptor {
                value: Some(ECMAScriptValue::from(false)),
                writable: Some(true),
                enumerable: Some(true),
                configurable: Some(true),
                ..Default::default()
            },
        )
        .unwrap();
        // binding_object = {
        //    [Symbol.unscopables]: unscopables_obj,
        //    visible: "This name is in the environment"
        //    hidden: "This name is not in the environment"
        //    also: "This one also visible"
        // }
        let binding_object = ordinary_object_create(Some(object_prototype));
        define_property_or_throw(
            &binding_object,
            "visible",
            PotentialPropertyDescriptor {
                value: Some(ECMAScriptValue::from("This name is in the environment")),
                writable: Some(true),
                enumerable: Some(true),
                configurable: Some(true),
                ..Default::default()
            },
        )
        .unwrap();
        define_property_or_throw(
            &binding_object,
            "hidden",
            PotentialPropertyDescriptor {
                value: Some(ECMAScriptValue::from("This name is not in the environment")),
                writable: Some(true),
                enumerable: Some(true),
                configurable: Some(true),
                ..Default::default()
            },
        )
        .unwrap();
        define_property_or_throw(
            &binding_object,
            "also",
            PotentialPropertyDescriptor {
                value: Some(ECMAScriptValue::from("This one also visible")),
                writable: Some(true),
                enumerable: Some(true),
                configurable: Some(true),
                ..Default::default()
            },
        )
        .unwrap();
        let unscopables_sym = wks(WksId::Unscopables);
        define_property_or_throw(
            &binding_object,
            unscopables_sym,
            PotentialPropertyDescriptor {
                value: Some(ECMAScriptValue::from(unscopables_obj)),
                writable: Some(true),
                enumerable: Some(true),
                configurable: Some(true),
                ..Default::default()
            },
        )
        .unwrap();
        let oer = ObjectEnvironmentRecord::new(binding_object, true, None, "test");

        assert!(oer.has_binding(&JSString::from("visible")).unwrap());
        assert!(!oer.has_binding(&JSString::from("hidden")).unwrap());
        assert!(oer.has_binding(&JSString::from("also")).unwrap());
    }
    #[test]
    fn has_binding_05() {
        // has_property returns an error
        setup_test_agent();
        let binding_object = DeadObject::object();
        let oer = ObjectEnvironmentRecord::new(binding_object, true, None, "test");

        let err = oer.has_binding(&JSString::from("random")).unwrap_err();
        let msg = unwind_type_error(err);
        assert_eq!(msg, "has_property called on DeadObject");
    }
    #[test]
    fn has_binding_06() {
        // binding_object.get(@@unscopables) fails
        setup_test_agent();
        let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let binding_object = ordinary_object_create(Some(object_prototype));
        // binding_object = {
        //    get [Symbol.unscopables] = %ThrowTypeError%
        //    field: true
        // }
        let tte = intrinsic(IntrinsicId::ThrowTypeError);
        let pk = PropertyKey::from(wks(WksId::Unscopables));
        let property = PotentialPropertyDescriptor {
            get: Some(ECMAScriptValue::from(tte)),
            enumerable: Some(true),
            configurable: Some(true),
            ..Default::default()
        };
        define_property_or_throw(&binding_object, pk, property).unwrap();
        let pk = PropertyKey::from("field");
        let property = PotentialPropertyDescriptor {
            value: Some(ECMAScriptValue::from(true)),
            writable: Some(true),
            enumerable: Some(true),
            configurable: Some(true),
            ..Default::default()
        };
        define_property_or_throw(&binding_object, pk, property).unwrap();
        let oer = ObjectEnvironmentRecord::new(binding_object, true, None, "test");

        let err = oer.has_binding(&JSString::from("field")).unwrap_err();

        let msg = unwind_type_error(err);
        assert_eq!(msg, "Generic TypeError");
    }
    #[test]
    fn has_binding_07() {
        // binding_object.@@unscopables.get(field) fails
        setup_test_agent();
        let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let binding_object = ordinary_object_create(Some(object_prototype.clone()));
        // binding_object = {
        //    [Symbol.unscopables] = {
        //        get field = %ThrowTypeError%
        //    }
        //    field: true
        // }
        let unscopables_obj = ordinary_object_create(Some(object_prototype));
        let tte = intrinsic(IntrinsicId::ThrowTypeError);
        let pk = PropertyKey::from("field");
        let property = PotentialPropertyDescriptor {
            get: Some(ECMAScriptValue::from(tte)),
            enumerable: Some(true),
            configurable: Some(true),
            ..Default::default()
        };
        define_property_or_throw(&unscopables_obj, pk.clone(), property).unwrap();
        let property = PotentialPropertyDescriptor {
            value: Some(ECMAScriptValue::from(true)),
            writable: Some(true),
            enumerable: Some(true),
            configurable: Some(true),
            ..Default::default()
        };
        define_property_or_throw(&binding_object, pk, property).unwrap();
        let pk = PropertyKey::from(wks(WksId::Unscopables));
        let property = PotentialPropertyDescriptor {
            value: Some(ECMAScriptValue::from(unscopables_obj)),
            writable: Some(true),
            enumerable: Some(true),
            configurable: Some(true),
            ..Default::default()
        };
        define_property_or_throw(&binding_object, pk, property).unwrap();
        let oer = ObjectEnvironmentRecord::new(binding_object, true, None, "test");

        let err = oer.has_binding(&JSString::from("field")).unwrap_err();

        let msg = unwind_type_error(err);
        assert_eq!(msg, "Generic TypeError");
    }

    #[test]
    fn create_mutable_binding() {
        setup_test_agent();
        let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let binding_object = ordinary_object_create(Some(object_prototype));
        let oer = ObjectEnvironmentRecord::new(binding_object.clone(), true, None, "test");

        oer.create_mutable_binding(JSString::from("can_delete"), true).unwrap();
        oer.create_mutable_binding(JSString::from("permanent"), false).unwrap();

        let can_delete_key = PropertyKey::from("can_delete");
        let cd_desc = binding_object.o.get_own_property(&can_delete_key).unwrap().unwrap();
        assert_eq!(cd_desc.enumerable, true);
        assert_eq!(cd_desc.configurable, true);
        assert!(cd_desc.is_data_descriptor());
        if let PropertyKind::Data(data) = cd_desc.property {
            assert_eq!(data.value, ECMAScriptValue::Undefined);
            assert_eq!(data.writable, true);
        }

        let permanent_key = PropertyKey::from("permanent");
        let perm_desc = binding_object.o.get_own_property(&permanent_key).unwrap().unwrap();
        assert_eq!(perm_desc.enumerable, true);
        assert_eq!(perm_desc.configurable, false);
        assert!(perm_desc.is_data_descriptor());
        if let PropertyKind::Data(data) = perm_desc.property {
            assert_eq!(data.value, ECMAScriptValue::Undefined);
            assert_eq!(data.writable, true);
        }
    }

    #[test]
    #[should_panic(expected = "unreachable code")]
    fn create_immutable_binding() {
        setup_test_agent();
        let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let binding_object = ordinary_object_create(Some(object_prototype));
        let oer = ObjectEnvironmentRecord::new(binding_object, true, None, "test");

        oer.create_immutable_binding(JSString::from("nothing"), true).unwrap();
    }

    #[test]
    fn initialize_binding() {
        setup_test_agent();
        let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let binding_object = ordinary_object_create(Some(object_prototype));
        let oer = ObjectEnvironmentRecord::new(binding_object.clone(), true, None, "test");
        let name = JSString::from("colorado");
        oer.create_mutable_binding(name.clone(), true).unwrap();

        oer.initialize_binding(&name, ECMAScriptValue::from(76)).unwrap();

        let key = PropertyKey::from(name);
        let desc = binding_object.o.get_own_property(&key).unwrap().unwrap();
        assert_eq!(desc.enumerable, true);
        assert_eq!(desc.configurable, true);
        assert!(desc.is_data_descriptor());
        if let PropertyKind::Data(data) = desc.property {
            assert_eq!(data.value, ECMAScriptValue::from(76));
            assert_eq!(data.writable, true);
        }
    }

    #[test]
    fn set_mutable_binding_01() {
        setup_test_agent();
        let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let binding_object = ordinary_object_create(Some(object_prototype));
        let oer = ObjectEnvironmentRecord::new(binding_object.clone(), true, None, "test");
        let name = JSString::from("vegetable");
        oer.create_mutable_binding(name.clone(), true).unwrap();
        oer.initialize_binding(&name, ECMAScriptValue::from(true)).unwrap();

        oer.set_mutable_binding(name.clone(), ECMAScriptValue::from(false), true).unwrap();

        let key = PropertyKey::from(name);
        let desc = binding_object.o.get_own_property(&key).unwrap().unwrap();
        assert_eq!(desc.enumerable, true);
        assert_eq!(desc.configurable, true);
        assert!(desc.is_data_descriptor());
        if let PropertyKind::Data(data) = desc.property {
            assert_eq!(data.value, ECMAScriptValue::from(false));
            assert_eq!(data.writable, true);
        }
    }
    #[test]
    fn set_mutable_binding_02() {
        // binding that's been deleted (or was never there)
        setup_test_agent();
        let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let binding_object = ordinary_object_create(Some(object_prototype));
        let oer = ObjectEnvironmentRecord::new(binding_object, true, None, "test");
        let name = JSString::from("vegetable");

        let err = oer.set_mutable_binding(name, ECMAScriptValue::Undefined, true).unwrap_err();

        let msg = unwind_reference_error(err);
        assert_eq!(msg, "Reference no longer exists");
    }
    #[test]
    fn set_mutable_binding_03() {
        // has_property throws
        setup_test_agent();
        let binding_object = DeadObject::object();
        let oer = ObjectEnvironmentRecord::new(binding_object, true, None, "test");
        let name = JSString::from("vegetable");

        let err = oer.set_mutable_binding(name, ECMAScriptValue::Undefined, true).unwrap_err();

        let msg = unwind_type_error(err);
        assert_eq!(msg, "has_property called on DeadObject");
    }
    #[test]
    fn set_mutable_binding_04() {
        // set throws
        setup_test_agent();
        let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let binding_object = ordinary_object_create(Some(object_prototype));
        let name = JSString::from("vegetable");
        let key = PropertyKey::from(name.clone());
        let tte = intrinsic(IntrinsicId::ThrowTypeError);
        let property = PotentialPropertyDescriptor {
            set: Some(ECMAScriptValue::from(tte)),
            enumerable: Some(true),
            configurable: Some(true),
            ..Default::default()
        };
        define_property_or_throw(&binding_object, key, property).unwrap();
        let oer = ObjectEnvironmentRecord::new(binding_object, true, None, "test");

        let err = oer.set_mutable_binding(name, ECMAScriptValue::Undefined, true).unwrap_err();

        let msg = unwind_type_error(err);
        assert_eq!(msg, "Generic TypeError");
    }

    #[test]
    fn get_binding_value_01() {
        setup_test_agent();
        let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let binding_object = ordinary_object_create(Some(object_prototype));
        let oer = ObjectEnvironmentRecord::new(binding_object, true, None, "test");
        let name = JSString::from("vegetable");
        oer.create_mutable_binding(name.clone(), true).unwrap();
        oer.initialize_binding(&name, ECMAScriptValue::from(true)).unwrap();
        oer.set_mutable_binding(name.clone(), ECMAScriptValue::from("squirrel"), true).unwrap();

        let result = oer.get_binding_value(&name, false).unwrap();
        assert_eq!(result, ECMAScriptValue::from("squirrel"));

        let result = oer.get_binding_value(&JSString::from("nothere"), false).unwrap();
        assert_eq!(result, ECMAScriptValue::Undefined);

        let result = oer.get_binding_value(&JSString::from("a"), true).unwrap_err();
        assert_eq!(unwind_reference_error(result), "Unresolvable reference");
    }
    #[test]
    fn get_binding_value_02() {
        // has_property throws
        setup_test_agent();
        let binding_object = DeadObject::object();
        let oer = ObjectEnvironmentRecord::new(binding_object, true, None, "test");
        let name = JSString::from("vegetable");

        let err = oer.get_binding_value(&name, true).unwrap_err();

        let msg = unwind_type_error(err);
        assert_eq!(msg, "has_property called on DeadObject");
    }

    #[test]
    fn delete() {
        setup_test_agent();
        let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let binding_object = ordinary_object_create(Some(object_prototype));
        let oer = ObjectEnvironmentRecord::new(binding_object, true, None, "test");
        let name = JSString::from("vegetable");
        oer.create_mutable_binding(name.clone(), true).unwrap();
        oer.initialize_binding(&name, ECMAScriptValue::from(true)).unwrap();
        oer.set_mutable_binding(name.clone(), ECMAScriptValue::from("squirrel"), true).unwrap();

        oer.delete_binding(&name).unwrap();
        assert!(!oer.has_binding(&name).unwrap());
    }

    #[test]
    fn object_environment_record_has_this_binding() {
        setup_test_agent();
        let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let binding_object = ordinary_object_create(Some(object_prototype));
        let oer = ObjectEnvironmentRecord::new(binding_object, true, None, "test");

        assert!(!oer.has_this_binding());
    }

    #[test]
    fn object_environment_record_has_super_binding() {
        setup_test_agent();
        let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let binding_object = ordinary_object_create(Some(object_prototype));
        let oer = ObjectEnvironmentRecord::new(binding_object, true, None, "test");

        assert!(!oer.has_super_binding());
    }

    #[test]
    fn object_environment_record_with_base_object_01() {
        setup_test_agent();
        let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let binding_object = ordinary_object_create(Some(object_prototype));
        let oer = ObjectEnvironmentRecord::new(binding_object.clone(), true, None, "test");

        assert_eq!(oer.with_base_object(), Some(binding_object));
    }

    #[test]
    fn object_environment_record_with_base_object_02() {
        setup_test_agent();
        let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let binding_object = ordinary_object_create(Some(object_prototype));
        let oer = ObjectEnvironmentRecord::new(binding_object, false, None, "test");

        assert!(oer.with_base_object().is_none());
    }

    #[test]
    fn object_environment_record_get_outer_env() {
        setup_test_agent();
        let declarative = Rc::new(DeclarativeEnvironmentRecord::new(None, "test"));
        declarative.create_immutable_binding(JSString::from("sentinel"), true).unwrap();
        declarative
            .initialize_binding(&JSString::from("sentinel"), ECMAScriptValue::from("very unique string"))
            .unwrap();
        let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let binding_object = ordinary_object_create(Some(object_prototype));
        let object_env = ObjectEnvironmentRecord::new(binding_object, false, Some(declarative), "test");

        let outer = object_env.get_outer_env().unwrap();

        let val_from_outer = outer.get_binding_value(&JSString::from("sentinel"), true).unwrap();
        assert_eq!(val_from_outer, ECMAScriptValue::from("very unique string"));
    }

    #[test]
    #[should_panic(expected = "unreachable")]
    fn get_this_binding() {
        setup_test_agent();
        let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let binding_object = ordinary_object_create(Some(object_prototype));
        let oer = ObjectEnvironmentRecord::new(binding_object, true, None, "test");
        oer.get_this_binding().unwrap();
    }

    #[test]
    #[should_panic(expected = "unreachable")]
    fn bind_this_value() {
        setup_test_agent();
        let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let binding_object = ordinary_object_create(Some(object_prototype));
        let oer = ObjectEnvironmentRecord::new(binding_object, true, None, "test");
        oer.bind_this_value(29.into()).unwrap();
    }

    #[test]
    fn name() {
        setup_test_agent();
        let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let binding_object = ordinary_object_create(Some(object_prototype));
        let oer = ObjectEnvironmentRecord::new(binding_object, true, None, "sentinel");
        assert_eq!(oer.name(), "sentinel");
    }

    #[test]
    fn binding_names() {
        setup_test_agent();
        let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let binding_object = ordinary_object_create(Some(object_prototype));
        let oer = ObjectEnvironmentRecord::new(binding_object, true, None, "sentinel");
        oer.create_mutable_binding("bill".into(), true).unwrap();
        oer.create_mutable_binding("alice".into(), true).unwrap();

        let bindings = oer.binding_names();
        assert_eq!(bindings.len(), 2);
        assert!(bindings.contains(&"bill".into()));
        assert!(bindings.contains(&"alice".into()));
    }
}

mod binding_status {
    use super::*;
    const ALL_BINDINGSTATUS: &[BindingStatus] =
        &[BindingStatus::Lexical, BindingStatus::Initialized, BindingStatus::Uninitialized];
    #[test]
    fn debug() {
        for val in ALL_BINDINGSTATUS {
            assert_ne!(format!("{val:?}"), "");
        }
    }
    #[test]
    fn eq() {
        for (right_idx, right_value) in ALL_BINDINGSTATUS.iter().enumerate() {
            for (left_idx, left_value) in ALL_BINDINGSTATUS.iter().enumerate() {
                assert_eq!(*left_value == *right_value, left_idx == right_idx);
            }
        }
    }
    #[test]
    #[allow(clippy::clone_on_copy)]
    fn clone() {
        let bs1 = BindingStatus::Lexical;
        let bs2 = bs1.clone();
        assert_eq!(bs1, bs2);
    }
}

mod function_environment_record {
    use super::*;
    use test_case::test_case;

    fn make_fer(src: &str, new_target: Option<Object>) -> (Object, FunctionEnvironmentRecord) {
        let ae = Maker::new(src).assignment_expression();
        let this_mode = if ae.contains(ParseNodeKind::ArrowFunction) || ae.contains(ParseNodeKind::AsyncArrowFunction) {
            ThisLexicality::LexicalThis
        } else {
            ThisLexicality::NonLexicalThis
        };
        let node = ae.function_definition().unwrap();
        let params = node.params();
        let body = node.body();

        let realm = current_realm_record().unwrap();
        let global_env = realm.borrow().global_env.clone().unwrap();
        let function_prototype = intrinsic(IntrinsicId::FunctionPrototype);
        let chunk = Rc::new(Chunk::new("empty"));
        let closure =
            ordinary_function_create(function_prototype, src, params, body, this_mode, global_env, None, true, chunk);
        (closure.clone(), FunctionEnvironmentRecord::new(closure, new_target, "environment_tag".to_string()))
    }

    #[test_case("function foo(left, right) { return left * right; }" => BindingStatus::Uninitialized; "non-lexical")]
    #[test_case("(left, right) => { return left * right; }" => BindingStatus::Lexical; "lexical")]
    fn new(source: &str) -> BindingStatus {
        setup_test_agent();

        let (closure, fer) = make_fer(source, None);

        assert_eq!(fer.name, "environment_tag");
        assert!(fer.new_target.is_none());
        assert_eq!(fer.function_object, closure);
        assert_eq!(&*fer.this_value.borrow(), &ECMAScriptValue::Undefined);
        assert!(fer.base.bindings.borrow().is_empty());

        fer.this_binding_status.get()
    }

    #[test]
    fn name() {
        setup_test_agent();
        let (_, fer) = make_fer("function a(){}", None);

        assert_eq!(fer.name(), "environment_tag");
    }

    #[test]
    fn create_immutable_binding() {
        setup_test_agent();
        let (_, fer) = make_fer("function a(){}", None);

        fer.create_immutable_binding("bob".into(), false).unwrap();
        assert_eq!(fer.binding_names(), &[JSString::from("bob")]);

        // But was it immutable?
        fer.initialize_binding(&"bob".into(), "initialized".into()).unwrap();
        fer.set_mutable_binding("bob".into(), "illegal".into(), true).expect_err("Should be immutable");
        let val = fer.get_binding_value(&"bob".into(), true).unwrap();
        assert_eq!(val, ECMAScriptValue::from("initialized"));
    }

    #[test]
    fn create_mutable_binding() {
        setup_test_agent();
        let (_, fer) = make_fer("function a(){}", None);

        fer.create_mutable_binding("bob".into(), true).unwrap();
        assert_eq!(fer.binding_names(), &[JSString::from("bob")]);

        // But was it mutable?
        fer.initialize_binding(&"bob".into(), "initialized".into()).unwrap();
        fer.set_mutable_binding("bob".into(), "altered".into(), true).unwrap();
        let val = fer.get_binding_value(&"bob".into(), true).unwrap();
        assert_eq!(val, ECMAScriptValue::from("altered"));
    }

    #[test]
    fn has_binding() {
        setup_test_agent();
        let (_, fer) = make_fer("function a(){}", None);

        fer.create_mutable_binding("bob".into(), true).unwrap();
        assert!(fer.has_binding(&"bob".into()).unwrap());
    }

    #[test]
    fn delete_binding() {
        setup_test_agent();
        let (_, fer) = make_fer("function a(){}", None);

        fer.create_mutable_binding("bob".into(), true).unwrap();
        fer.initialize_binding(&"bob".into(), "initialized".into()).unwrap();
        fer.create_mutable_binding("alice".into(), false).unwrap();
        fer.initialize_binding(&"alice".into(), "initialized".into()).unwrap();

        assert!(fer.delete_binding(&"bob".into()).unwrap());
        assert!(!fer.delete_binding(&"alice".into()).unwrap());
        assert!(fer.has_binding(&"alice".into()).unwrap());
        assert!(!fer.has_binding(&"bob".into()).unwrap());
    }

    #[test_case("function a(){}" => true; "non-lexical")]
    #[test_case("() => 1" => false; "lexical")]
    fn has_this_binding(src: &str) -> bool {
        setup_test_agent();
        let (_, fer) = make_fer(src, None);

        fer.has_this_binding()
    }

    fn make_lexical() -> FunctionEnvironmentRecord {
        make_fer("() => 3", None).1
    }
    fn make_uninit_this() -> FunctionEnvironmentRecord {
        make_fer("function a() {}", None).1
    }
    fn make_init_this() -> FunctionEnvironmentRecord {
        let (_, fer) = make_fer("function a(){}", None);
        fer.bind_this_value(ECMAScriptValue::from("sentinel")).unwrap();
        fer
    }
    #[test_case(make_lexical => panics "binding status may not be lexical"; "lexical")]
    #[test_case(make_uninit_this => sok("core"); "uninit")]
    #[test_case(make_init_this => serr("ReferenceError: This value already bound"); "already initialized")]
    fn bind_this_value(fer_maker: impl FnOnce() -> FunctionEnvironmentRecord) -> Result<String, String> {
        setup_test_agent();
        let fer = fer_maker();

        let value = ECMAScriptValue::from("core");

        fer.bind_this_value(value).map_err(unwind_any_error).map(|v| v.to_string())
    }

    #[test_case(make_lexical => panics "lexical functions never have a this binding"; "lexical")]
    #[test_case(make_uninit_this => serr("ReferenceError: This binding uninitialized"); "uninitialized")]
    #[test_case(make_init_this => Ok("sentinel".to_string()); "initialized")]
    fn get_this_binding(fer_maker: impl FnOnce() -> FunctionEnvironmentRecord) -> Result<String, String> {
        setup_test_agent();
        let fer = fer_maker();

        fer.get_this_binding().map_err(unwind_any_error).map(|v| to_string(v).unwrap().to_string())
    }

    fn make_super() -> FunctionEnvironmentRecord {
        let (f, fer) = make_fer("function a(){}", None);
        let home_object_prototype = Object::from("Home Object Prototype");
        let home_object = ordinary_object_create(Some(home_object_prototype));

        let fo = f.o.to_function_obj().unwrap();
        fo.function_data().borrow_mut().home_object = Some(home_object);
        fer
    }
    #[test_case(make_lexical => Ok(None); "no home object")]
    #[test_case(make_super => Ok(Some("Home Object Prototype".to_string())); "has home object")]
    fn get_super_base(fer_maker: impl FnOnce() -> FunctionEnvironmentRecord) -> Result<Option<String>, String> {
        setup_test_agent();
        let fer = fer_maker();

        fer.get_super_base()
            .map_err(unwind_any_error)
            .map(|opt_obj| opt_obj.map(|obj| String::from(obj.to_string().unwrap())))
    }

    #[test_case(make_lexical => false; "lexical - no super")]
    #[test_case(make_init_this => false; "non-lexical - no super")]
    #[test_case(make_super => true; "has super")]
    fn has_super_binding(fer_maker: impl FnOnce() -> FunctionEnvironmentRecord) -> bool {
        setup_test_agent();
        let fer = fer_maker();

        fer.has_super_binding()
    }

    #[test_case(make_lexical => None; "lexical")]
    fn with_base_object(fer_maker: impl FnOnce() -> FunctionEnvironmentRecord) -> Option<String> {
        setup_test_agent();
        let fer = fer_maker();

        fer.with_base_object().map(|obj| obj.get(&"sentinel".into()).unwrap().to_string())
    }
    #[test]
    fn debug_fmt() {
        setup_test_agent();
        let (_, fer) = make_fer("function a(){}", None);
        let s = format!("{fer:#?}");
        assert_ne!(s, "");
    }
    #[test]
    fn concise() {
        setup_test_agent();
        let (_, fer) = make_fer("function b(){}", None);
        let s = format!("{fer:?}");
        assert_ne!(s, "");
        assert!(!s.contains('\n'));
    }

    #[test]
    fn get_outer_env() {
        setup_test_agent();
        let (_, fer) = make_fer("() => 10", None);
        assert_eq!(fer.get_outer_env().map(|er| er.name()), Some("realm-global".to_string()));
    }
}

mod global_environment_record {
    use super::*;
    use test_case::test_case;

    fn setup() -> GlobalEnvironmentRecord {
        let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let global_object = ordinary_object_create(Some(object_prototype.clone()));
        let this_object = ordinary_object_create(Some(object_prototype));
        let ger = GlobalEnvironmentRecord::new(global_object, this_object, "test");
        let ld = JSString::from("lexical_deletable");
        // mutable, deletable lexical binding, named "lexical_deletable"
        ger.declarative_record.create_mutable_binding(ld.clone(), true).unwrap();
        ger.initialize_binding(&ld, ECMAScriptValue::from("LEXICAL DELETABLE")).unwrap();
        // mutable, permanent lexical binding, named "lexical_permanent"
        let lp = JSString::from("lexical_permanent");
        ger.declarative_record.create_mutable_binding(lp.clone(), false).unwrap();
        ger.initialize_binding(&lp, ECMAScriptValue::from("LEXICAL PERMANENT")).unwrap();
        // immutable, strict lexical binding, named "lexical_strict"
        let ls = JSString::from("lexical_strict");
        ger.declarative_record.create_immutable_binding(ls.clone(), true).unwrap();
        ger.initialize_binding(&ls, ECMAScriptValue::from("LEXICAL STRICT")).unwrap();
        // immutable, sloppy lexical binding, named "lexical_sloppy"
        let lslop = JSString::from("lexical_sloppy");
        ger.declarative_record.create_immutable_binding(lslop.clone(), false).unwrap();
        ger.initialize_binding(&lslop, ECMAScriptValue::from("LEXICAL SLOPPY")).unwrap();
        // configurable global var (in varnames), deletable, named "normal_var"
        ger.create_global_var_binding(JSString::from("normal_var"), true).unwrap();
        ger.object_record
            .set_mutable_binding(JSString::from("normal_var"), ECMAScriptValue::from("NORMAL VAR"), true)
            .unwrap();
        // param on global object that's not in varnames (like builtin props), named "non_config_var"
        let desc = PotentialPropertyDescriptor {
            value: Some(ECMAScriptValue::from("NON-CONFIG")),
            writable: Some(true),
            enumerable: Some(true),
            configurable: Some(false),
            ..Default::default()
        };
        ger.object_record.binding_object.o.define_own_property(JSString::from("non_config_var").into(), desc).unwrap();
        // Same thing, but not writable
        let desc = PotentialPropertyDescriptor {
            value: Some(ECMAScriptValue::from("CONST")),
            writable: Some(false),
            enumerable: Some(true),
            configurable: Some(false),
            ..Default::default()
        };
        ger.object_record
            .binding_object
            .o
            .define_own_property(JSString::from("non_config_permanent").into(), desc)
            .unwrap();
        // Same thing, but not enumerable
        let desc = PotentialPropertyDescriptor {
            value: Some(ECMAScriptValue::from("NO ENUM")),
            writable: Some(true),
            enumerable: Some(false),
            configurable: Some(false),
            ..Default::default()
        };
        ger.object_record
            .binding_object
            .o
            .define_own_property(JSString::from("non_config_unlisted").into(), desc)
            .unwrap();
        // Now a non-config accessor function
        let desc = PotentialPropertyDescriptor {
            get: Some(ECMAScriptValue::Undefined),
            set: Some(ECMAScriptValue::Undefined),
            enumerable: Some(true),
            configurable: Some(false),
            ..Default::default()
        };
        ger.object_record
            .binding_object
            .o
            .define_own_property(JSString::from("non_config_accessor").into(), desc)
            .unwrap();

        ger
    }

    #[test]
    fn debug() {
        setup_test_agent();
        let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let global_object = ordinary_object_create(Some(object_prototype.clone()));
        let this_object = ordinary_object_create(Some(object_prototype));
        let ger = GlobalEnvironmentRecord::new(global_object, this_object, "test");

        assert_ne!(format!("{ger:?}"), "");
    }

    #[test]
    fn fancy_debug() {
        setup_test_agent();
        let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let global_object = ordinary_object_create(Some(object_prototype.clone()));
        let this_object = ordinary_object_create(Some(object_prototype));
        let ger = GlobalEnvironmentRecord::new(global_object, this_object, "test");

        assert_ne!(format!("{ger:#?}"), "");
    }

    mod has_binding {
        use super::*;
        #[test]
        fn happy_path() {
            setup_test_agent();

            let in_object_name = JSString::from("in_object");
            let in_decl_name = JSString::from("in_decl");
            let nobody_name = JSString::from("nobody");
            let in_object_key = PropertyKey::from(in_object_name.clone());

            let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
            let global_object = ordinary_object_create(Some(object_prototype.clone()));
            let this_object = ordinary_object_create(Some(object_prototype));
            let in_object_property = PotentialPropertyDescriptor {
                value: Some(ECMAScriptValue::from(0)),
                writable: Some(true),
                enumerable: Some(true),
                configurable: Some(true),
                ..Default::default()
            };
            define_property_or_throw(&global_object, in_object_key, in_object_property).unwrap();
            let ger = GlobalEnvironmentRecord::new(global_object, this_object, "test");
            ger.create_mutable_binding(in_decl_name.clone(), true).unwrap();
            ger.initialize_binding(&in_decl_name, ECMAScriptValue::from(0)).unwrap();

            assert!(ger.has_binding(&in_decl_name).unwrap());
            assert!(ger.has_binding(&in_object_name).unwrap());
            assert!(!ger.has_binding(&nobody_name).unwrap());
        }
        #[test]
        fn error_path() {
            setup_test_agent();
            let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
            let global_object = DeadObject::object();
            let this_object = ordinary_object_create(Some(object_prototype));
            let ger = GlobalEnvironmentRecord::new(global_object, this_object, "test");

            let err = ger.has_binding(&JSString::from("a")).unwrap_err();
            let msg = unwind_type_error(err);
            assert_eq!(msg, "has_property called on DeadObject");
        }
    }

    mod create_mutable_binding {
        use super::*;
        use test_case::test_case;

        #[test_case(true; "Deletable")]
        #[test_case(false; "Permanent")]
        fn happy(deletable: bool) {
            // Setup
            setup_test_agent();
            let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
            let global_object = ordinary_object_create(Some(object_prototype.clone()));
            let this_object = ordinary_object_create(Some(object_prototype));
            let ger = GlobalEnvironmentRecord::new(global_object, this_object, "test");
            let test_name = JSString::from("test");

            // Exercise function
            ger.create_mutable_binding(test_name.clone(), deletable).unwrap();

            // Validate results
            let bindings = ger.declarative_record.bindings.borrow();
            // 1. Binding is in declarative record portion
            let binding = bindings.get(&test_name).unwrap();
            // 2. Binding is not yet initialized
            assert!(binding.value.is_none());
            // 3. Binding has correct deletable flag
            assert!(binding.mutability == Mutability::Mutable(Removability::from(deletable)));
            // 4. No additional bindings were added
            assert_eq!(bindings.len(), 1);
        }

        #[test]
        fn error() {
            // Setup
            setup_test_agent();
            let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
            let global_object = ordinary_object_create(Some(object_prototype.clone()));
            let this_object = ordinary_object_create(Some(object_prototype));
            let ger = GlobalEnvironmentRecord::new(global_object, this_object, "test");
            let test_name = JSString::from("test");
            ger.create_mutable_binding(test_name.clone(), true).unwrap();

            // Exercise function
            let result = ger.create_mutable_binding(test_name, true);

            // Validate result
            let err = result.unwrap_err();
            let msg = unwind_type_error(err);
            assert_eq!(msg, "Binding already exists");
        }
    }

    mod create_immutable_binding {
        use super::*;
        use test_case::test_case;

        #[test_case(true; "Strict")]
        #[test_case(false; "Sloppy")]
        fn happy(strict: bool) {
            // Setup
            setup_test_agent();
            let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
            let global_object = ordinary_object_create(Some(object_prototype.clone()));
            let this_object = ordinary_object_create(Some(object_prototype));
            let ger = GlobalEnvironmentRecord::new(global_object, this_object, "test");
            let test_name = JSString::from("test");

            // Exercise function
            ger.create_immutable_binding(test_name.clone(), strict).unwrap();

            // Validate
            let bindings = ger.declarative_record.bindings.borrow();
            // 1. Binding is in declarative record portion
            let binding = bindings.get(&test_name).unwrap();
            // 2. Binding is not yet initialized
            assert!(binding.value.is_none());
            // 3. Binding has correct strict flag
            assert!(binding.mutability == Mutability::Immutable(Strictness::from(strict)));
            // 4. No additional bindings were added
            assert_eq!(bindings.len(), 1);
        }

        #[test]
        fn error() {
            // Setup
            setup_test_agent();
            let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
            let global_object = ordinary_object_create(Some(object_prototype.clone()));
            let this_object = ordinary_object_create(Some(object_prototype));
            let ger = GlobalEnvironmentRecord::new(global_object, this_object, "test");
            let test_name = JSString::from("test");
            ger.create_mutable_binding(test_name.clone(), true).unwrap();

            // Exercise function
            let result = ger.create_immutable_binding(test_name, true);

            // Validate result
            let err = result.unwrap_err();
            let msg = unwind_type_error(err);
            assert_eq!(msg, "Binding already exists");
        }
    }

    mod initialize_binding {
        use super::*;

        #[test]
        fn decl() {
            // Setup
            setup_test_agent();
            let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
            let global_object = ordinary_object_create(Some(object_prototype.clone()));
            let this_object = ordinary_object_create(Some(object_prototype));
            let ger = GlobalEnvironmentRecord::new(global_object, this_object, "test");
            let test_name = JSString::from("test");
            ger.create_mutable_binding(test_name.clone(), true).unwrap();

            // Exercise function
            ger.initialize_binding(&test_name, ECMAScriptValue::from(527)).unwrap();

            let bindings = ger.declarative_record.bindings.borrow();
            // 1. Binding is in declarative record portion
            let binding = bindings.get(&test_name).unwrap();
            // 2. Binding has value 527
            assert_eq!(binding.value, Some(ECMAScriptValue::from(527)));
        }
        #[test]
        fn object() {
            // Setup
            setup_test_agent();
            let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
            let global_object = ordinary_object_create(Some(object_prototype.clone()));
            let this_object = ordinary_object_create(Some(object_prototype));
            let ger = GlobalEnvironmentRecord::new(global_object.clone(), this_object, "test");
            let test_name = JSString::from("test");
            ger.object_record.create_mutable_binding(test_name.clone(), true).unwrap();

            // Excersize function
            ger.initialize_binding(&test_name, ECMAScriptValue::from(223)).unwrap();

            // Validate
            let val = global_object.get(&PropertyKey::from(test_name)).unwrap();
            assert_eq!(val, ECMAScriptValue::from(223));
        }
    }

    mod set_mutable_binding {
        use super::*;
        #[test]
        fn decl() {
            // Setup
            setup_test_agent();
            let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
            let global_object = ordinary_object_create(Some(object_prototype.clone()));
            let this_object = ordinary_object_create(Some(object_prototype));
            let ger = GlobalEnvironmentRecord::new(global_object, this_object, "test");
            let test_name = JSString::from("test");
            ger.create_mutable_binding(test_name.clone(), true).unwrap();
            ger.initialize_binding(&test_name, ECMAScriptValue::from(527)).unwrap();

            // Exercise function
            ger.set_mutable_binding(test_name.clone(), ECMAScriptValue::from(10), false).unwrap();

            // Validate
            let val = ger.get_binding_value(&test_name, false).unwrap();
            assert_eq!(val, ECMAScriptValue::from(10));
            assert!(ger.declarative_record.has_binding(&test_name).unwrap());
            assert!(!ger.object_record.has_binding(&test_name).unwrap());
        }
        #[test]
        fn object() {
            // Setup
            setup_test_agent();
            let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
            let global_object = ordinary_object_create(Some(object_prototype.clone()));
            let this_object = ordinary_object_create(Some(object_prototype));
            let ger = GlobalEnvironmentRecord::new(global_object, this_object, "test");
            let test_name = JSString::from("test");
            ger.object_record.create_mutable_binding(test_name.clone(), true).unwrap();
            ger.initialize_binding(&test_name, ECMAScriptValue::from(527)).unwrap();

            // Exercise function
            ger.set_mutable_binding(test_name.clone(), ECMAScriptValue::from(9933), false).unwrap();

            // Validate
            let val = ger.get_binding_value(&test_name, false).unwrap();
            assert_eq!(val, ECMAScriptValue::from(9933));
            assert!(!ger.declarative_record.has_binding(&test_name).unwrap());
            assert!(ger.object_record.has_binding(&test_name).unwrap());
        }
    }

    mod get_binding_value {
        use super::*;

        #[test]
        fn decl() {
            // Setup
            setup_test_agent();
            let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
            let global_object = ordinary_object_create(Some(object_prototype.clone()));
            let this_object = ordinary_object_create(Some(object_prototype));
            let ger = GlobalEnvironmentRecord::new(global_object, this_object, "test");
            let test_name = JSString::from("test");
            ger.create_mutable_binding(test_name.clone(), true).unwrap();
            ger.initialize_binding(&test_name, ECMAScriptValue::from(527)).unwrap();

            // Exercise
            let result = ger.get_binding_value(&test_name, true).unwrap();

            // Validate
            assert_eq!(result, ECMAScriptValue::from(527));
        }
        #[test]
        fn object() {
            // Setup
            setup_test_agent();
            let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
            let global_object = ordinary_object_create(Some(object_prototype.clone()));
            let this_object = ordinary_object_create(Some(object_prototype));
            let ger = GlobalEnvironmentRecord::new(global_object, this_object, "test");
            let test_name = JSString::from("test");
            ger.object_record.create_mutable_binding(test_name.clone(), true).unwrap();
            ger.initialize_binding(&test_name, ECMAScriptValue::from(527)).unwrap();

            // Exercise
            let result = ger.get_binding_value(&test_name, true).unwrap();

            // Validate
            assert_eq!(result, ECMAScriptValue::from(527));
        }
        #[test]
        fn missing_sloppy() {
            // Setup
            setup_test_agent();
            let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
            let global_object = ordinary_object_create(Some(object_prototype.clone()));
            let this_object = ordinary_object_create(Some(object_prototype));
            let ger = GlobalEnvironmentRecord::new(global_object, this_object, "test");
            let test_name = JSString::from("test");

            // Exercise
            let result = ger.get_binding_value(&test_name, false).unwrap();

            // Validate
            assert_eq!(result, ECMAScriptValue::Undefined);
        }
        #[test]
        fn missing_strict() {
            // Setup
            setup_test_agent();
            let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
            let global_object = ordinary_object_create(Some(object_prototype.clone()));
            let this_object = ordinary_object_create(Some(object_prototype));
            let ger = GlobalEnvironmentRecord::new(global_object, this_object, "test");
            let test_name = JSString::from("test");

            // Exercise
            let result = ger.get_binding_value(&test_name, true);

            // Validate
            let err = result.unwrap_err();
            let msg = unwind_reference_error(err);
            assert_eq!(msg, "Unresolvable reference");
        }
    }

    mod delete_binding {
        use super::*;

        #[test]
        fn decl() {
            // Setup
            setup_test_agent();
            let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
            let global_object = ordinary_object_create(Some(object_prototype.clone()));
            let this_object = ordinary_object_create(Some(object_prototype));
            let ger = GlobalEnvironmentRecord::new(global_object, this_object, "test");
            let test_name = JSString::from("test");
            ger.create_mutable_binding(test_name.clone(), true).unwrap();
            ger.initialize_binding(&test_name, ECMAScriptValue::from(527)).unwrap();

            // Exercise function
            let result = ger.delete_binding(&test_name);

            // Validate
            assert!(result.unwrap());
            assert!(!ger.has_binding(&test_name).unwrap());
        }

        #[test]
        fn object_binding_in_varnames() {
            // Setup
            setup_test_agent();
            let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
            let global_object = ordinary_object_create(Some(object_prototype.clone()));
            let this_object = ordinary_object_create(Some(object_prototype));
            let ger = GlobalEnvironmentRecord::new(global_object, this_object, "test");
            let test_name = JSString::from("test");
            ger.object_record.create_mutable_binding(test_name.clone(), true).unwrap();
            ger.initialize_binding(&test_name, ECMAScriptValue::from(527)).unwrap();
            ger.var_names.borrow_mut().insert(test_name.clone());

            // Exercise
            let result = ger.delete_binding(&test_name);

            // Validate
            assert!(result.unwrap());
            assert!(!ger.has_binding(&test_name).unwrap());
            assert!(!ger.var_names.borrow().contains(&test_name));
        }
        #[test]
        fn object_binding_not_in_vn() {
            // Setup
            setup_test_agent();
            let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
            let global_object = ordinary_object_create(Some(object_prototype.clone()));
            let this_object = ordinary_object_create(Some(object_prototype));
            let ger = GlobalEnvironmentRecord::new(global_object, this_object, "test");
            let test_name = JSString::from("test");
            ger.object_record.create_mutable_binding(test_name.clone(), true).unwrap();
            ger.initialize_binding(&test_name, ECMAScriptValue::from(527)).unwrap();

            // Exercise
            let result = ger.delete_binding(&test_name);

            // Validate
            assert!(result.unwrap());
            assert!(!ger.has_binding(&test_name).unwrap());
            assert!(!ger.var_names.borrow().contains(&test_name));
        }
        #[test]
        fn object_binding_permanent() {
            // Setup
            setup_test_agent();
            let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
            let global_object = ordinary_object_create(Some(object_prototype.clone()));
            let this_object = ordinary_object_create(Some(object_prototype));
            let ger = GlobalEnvironmentRecord::new(global_object, this_object, "test");
            let test_name = JSString::from("test");
            ger.object_record.create_mutable_binding(test_name.clone(), false).unwrap();
            ger.initialize_binding(&test_name, ECMAScriptValue::from(527)).unwrap();
            ger.var_names.borrow_mut().insert(test_name.clone());

            // Exercise
            let result = ger.delete_binding(&test_name);

            // Validate
            assert!(!result.unwrap());
            assert!(ger.has_binding(&test_name).unwrap());
            assert!(ger.var_names.borrow().contains(&test_name));
        }
        #[test]
        fn no_binding() {
            // Setup
            setup_test_agent();
            let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
            let global_object = ordinary_object_create(Some(object_prototype.clone()));
            let this_object = ordinary_object_create(Some(object_prototype));
            let ger = GlobalEnvironmentRecord::new(global_object, this_object, "test");
            let test_name = JSString::from("test");

            // Exercise
            let result = ger.delete_binding(&test_name);

            // Validate
            assert!(result.unwrap());
        }
        #[test]
        fn has_property_error() {
            setup_test_agent();
            let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
            let global_object = DeadObject::object();
            let this_object = ordinary_object_create(Some(object_prototype));
            let ger = GlobalEnvironmentRecord::new(global_object, this_object, "test");

            let err = ger.delete_binding(&JSString::from("a")).unwrap_err();
            let msg = unwind_type_error(err);
            assert_eq!(msg, "get_own_property called on DeadObject");
        }
        #[test]
        fn delete_error() {
            // Setup
            setup_test_agent();
            let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
            let global_object = TestObject::object(&[FunctionId::Delete(None)]);
            let this_object = ordinary_object_create(Some(object_prototype));
            let ger = GlobalEnvironmentRecord::new(global_object, this_object, "test");
            let test_name = JSString::from("test");
            ger.object_record.create_mutable_binding(test_name.clone(), true).unwrap();
            ger.object_record.initialize_binding(&test_name, ECMAScriptValue::from(88)).unwrap();

            // Exercise
            let result = ger.delete_binding(&test_name);

            // Validate
            let err = result.unwrap_err();
            let msg = unwind_type_error(err);
            assert_eq!(msg, "[[Delete]] called on TestObject");
        }
    }

    #[test]
    fn has_this_binding() {
        // Setup
        setup_test_agent();
        let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let global_object = ordinary_object_create(Some(object_prototype.clone()));
        let this_object = ordinary_object_create(Some(object_prototype));
        let ger = GlobalEnvironmentRecord::new(global_object, this_object, "test");

        // Exercise
        let result = ger.has_this_binding();

        // Validate
        assert!(result);
    }
    #[test]
    fn has_super_binding() {
        // Setup
        setup_test_agent();
        let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let global_object = ordinary_object_create(Some(object_prototype.clone()));
        let this_object = ordinary_object_create(Some(object_prototype));
        let ger = GlobalEnvironmentRecord::new(global_object, this_object, "test");

        // Exercise
        let result = ger.has_super_binding();

        // Validate
        assert!(!result);
    }
    #[test]
    fn with_base_object() {
        // Setup
        setup_test_agent();
        let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let global_object = ordinary_object_create(Some(object_prototype.clone()));
        let this_object = ordinary_object_create(Some(object_prototype));
        let ger = GlobalEnvironmentRecord::new(global_object, this_object, "test");

        // Exercise
        let result = ger.with_base_object();

        // Validate
        assert!(result.is_none());
    }
    #[test]
    fn get_outer_env() {
        // Setup
        setup_test_agent();
        let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let global_object = ordinary_object_create(Some(object_prototype.clone()));
        let this_object = ordinary_object_create(Some(object_prototype));
        let ger = GlobalEnvironmentRecord::new(global_object, this_object, "test");

        // Exercise
        let result = ger.get_outer_env();

        // Validate
        assert!(result.is_none());
    }

    #[test]
    fn get_this_binding() {
        // Setup
        setup_test_agent();
        let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let global_object = ordinary_object_create(Some(object_prototype.clone()));
        let this_object = ordinary_object_create(Some(object_prototype));
        let ger = GlobalEnvironmentRecord::new(global_object, this_object.clone(), "test");

        // Exercise function
        let result = ger.get_this_binding().unwrap();

        // Validate
        assert_eq!(result, ECMAScriptValue::from(this_object));
    }

    #[test_case("varstyle" => true; "var")]
    #[test_case("lexical" => false; "lex")]
    fn has_var_declaration(prop_name: &str) -> bool {
        // Setup
        setup_test_agent();
        let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let global_object = ordinary_object_create(Some(object_prototype.clone()));
        let this_object = ordinary_object_create(Some(object_prototype));
        let ger = GlobalEnvironmentRecord::new(global_object, this_object, "test");
        let var_name = JSString::from("varstyle");
        ger.create_global_var_binding(var_name, true).unwrap();
        let lex_name = JSString::from("lexical");
        ger.create_mutable_binding(lex_name.clone(), true).unwrap();
        ger.initialize_binding(&lex_name, ECMAScriptValue::Undefined).unwrap();

        // Exercise
        ger.has_var_declaration(&JSString::from(prop_name))
    }
    #[test_case("varstyle" => false; "var")]
    #[test_case("lexical" => true; "lex")]
    fn has_lexical_declaration(prop_name: &str) -> bool {
        // Setup
        setup_test_agent();
        let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let global_object = ordinary_object_create(Some(object_prototype.clone()));
        let this_object = ordinary_object_create(Some(object_prototype));
        let ger = GlobalEnvironmentRecord::new(global_object, this_object, "test");
        let var_name = JSString::from("varstyle");
        ger.create_global_var_binding(var_name, true).unwrap();
        let lex_name = JSString::from("lexical");
        ger.create_mutable_binding(lex_name.clone(), true).unwrap();
        ger.initialize_binding(&lex_name, ECMAScriptValue::Undefined).unwrap();

        // Exercise
        ger.has_lexical_declaration(&JSString::from(prop_name))
    }

    mod has_restricted_global_property {
        use super::*;
        use test_case::test_case;

        #[test_case("not_present" => false; "property doesn't already exist")]
        #[test_case("normal_var" => false; "configurable var property")]
        #[test_case("non_config_var" => true; "non-configurable property on object")]
        fn happy(propname: &str) -> bool {
            setup_test_agent();
            let ger = setup();
            ger.has_restricted_global_property(&JSString::from(propname)).unwrap()
        }

        #[test]
        fn error() {
            setup_test_agent();
            let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
            let global_object = DeadObject::object();
            let this_object = ordinary_object_create(Some(object_prototype));
            let ger = GlobalEnvironmentRecord::new(global_object, this_object, "test");

            let err = ger.has_restricted_global_property(&JSString::from("test")).unwrap_err();
            let msg = unwind_type_error(err);
            assert_eq!(msg, "get_own_property called on DeadObject");
        }
    }

    mod can_declare_global_var {
        use super::*;
        use test_case::test_case;

        #[test_case("normal_var", true => true; "normal, extensible")]
        #[test_case("not_present", true => true; "not there, extensible")]
        #[test_case("normal_var", false => true; "normal, not extensible")]
        #[test_case("not_present", false => false; "not there, not extensible")]
        fn happy(name: &str, global_extensible: bool) -> bool {
            setup_test_agent();
            let ger = setup();
            if !global_extensible {
                ger.object_record.binding_object.o.prevent_extensions().unwrap();
            }

            ger.can_declare_global_var(&JSString::from(name)).unwrap()
        }

        #[test_case(FunctionId::GetOwnProperty(None) => "[[GetOwnProperty]] called on TestObject"; "GetOwnProperty")]
        #[test_case(FunctionId::IsExtensible => "[[IsExtensible]] called on TestObject"; "IsExtensible")]
        fn error(method: FunctionId) -> String {
            // Setup
            setup_test_agent();
            let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
            let global_object = TestObject::object(&[method]);
            let this_object = ordinary_object_create(Some(object_prototype));
            let ger = GlobalEnvironmentRecord::new(global_object, this_object, "test");

            let err = ger.can_declare_global_var(&JSString::from("anything")).unwrap_err();
            unwind_type_error(err)
        }
    }

    mod can_declare_global_function {
        use super::*;
        use test_case::test_case;

        #[test_case("not_present" => true)]
        #[test_case("normal_var" => true)]
        #[test_case("non_config_var" => true)]
        #[test_case("non_config_permanent" => false)]
        #[test_case("non_config_unlisted" => false)]
        #[test_case("non_config_accessor" => false)]
        fn happy_extensible(name: &str) -> bool {
            setup_test_agent();
            let ger = setup();
            let test_name = JSString::from(name);

            ger.can_declare_global_function(&test_name).unwrap()
        }
        #[test_case("not_present" => false)]
        #[test_case("normal_var" => true)]
        #[test_case("non_config_var" => true)]
        #[test_case("non_config_permanent" => false)]
        #[test_case("non_config_unlisted" => false)]
        #[test_case("non_config_accessor" => false)]
        fn happy_frozen(name: &str) -> bool {
            setup_test_agent();
            let ger = setup();
            ger.object_record.binding_object.o.prevent_extensions().unwrap();
            let test_name = JSString::from(name);

            ger.can_declare_global_function(&test_name).unwrap()
        }
        #[test_case(FunctionId::GetOwnProperty(None) => "[[GetOwnProperty]] called on TestObject"; "GetOwnProperty")]
        #[test_case(FunctionId::IsExtensible => "[[IsExtensible]] called on TestObject"; "IsExtensible")]
        fn error(method: FunctionId) -> String {
            // Setup
            setup_test_agent();
            let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
            let global_object = TestObject::object(&[method]);
            let this_object = ordinary_object_create(Some(object_prototype));
            let ger = GlobalEnvironmentRecord::new(global_object, this_object, "test");

            let err = ger.can_declare_global_function(&JSString::from("anything")).unwrap_err();
            unwind_type_error(err)
        }
    }

    mod create_global_var_binding {
        use super::*;
        use test_case::test_case;

        #[test_case("new_name", true => (ECMAScriptValue::Undefined, true); "new property; deletable")]
        #[test_case("new_name", false => (ECMAScriptValue::Undefined, false); "new property; permanent")]
        #[test_case("normal_var", true => (ECMAScriptValue::from("NORMAL VAR"), true); "existing prop; deletable")]
        #[test_case("normal_var", false => (ECMAScriptValue::from("NORMAL VAR"), true); "existing prop; permanent")]
        fn happy_extensible(name: &str, deletable: bool) -> (ECMAScriptValue, bool) {
            setup_test_agent();
            let ger = setup();
            let test_name = JSString::from(name);

            ger.create_global_var_binding(test_name.clone(), deletable).unwrap();

            assert!(ger.var_names.borrow().contains(&test_name));
            let desc =
                ger.object_record.binding_object.o.get_own_property(&PropertyKey::from(test_name)).unwrap().unwrap();
            assert!(matches!(desc.property, PropertyKind::Data(_)));
            if let PropertyKind::Data(data) = desc.property {
                (data.value, desc.configurable)
            } else {
                unreachable!()
            }
        }

        #[test_case("new_name", true => None; "new property; deletable")]
        #[test_case("new_name", false => None; "new property; permanent")]
        #[test_case("normal_var", true => Some((ECMAScriptValue::from("NORMAL VAR"), true)); "existing prop; deletable")]
        #[test_case("normal_var", false => Some((ECMAScriptValue::from("NORMAL VAR"), true)); "existing prop; permanent")]
        fn happy_frozen(name: &str, deletable: bool) -> Option<(ECMAScriptValue, bool)> {
            setup_test_agent();
            let ger = setup();
            ger.object_record.binding_object.o.prevent_extensions().unwrap();
            let test_name = JSString::from(name);

            ger.create_global_var_binding(test_name.clone(), deletable).unwrap();

            assert!(ger.var_names.borrow().contains(&test_name));
            let opt_desc = ger.object_record.binding_object.o.get_own_property(&PropertyKey::from(test_name)).unwrap();
            match opt_desc {
                None => None,
                Some(desc) => {
                    assert!(matches!(desc.property, PropertyKind::Data(_)));
                    if let PropertyKind::Data(data) = desc.property {
                        Some((data.value, desc.configurable))
                    } else {
                        unreachable!()
                    }
                }
            }
        }

        #[test_case(FunctionId::GetOwnProperty(None) => "[[GetOwnProperty]] called on TestObject"; "GetOwnProperty")]
        #[test_case(FunctionId::IsExtensible => "[[IsExtensible]] called on TestObject"; "IsExtensible")]
        #[test_case(FunctionId::DefineOwnProperty(None) => "[[DefineOwnProperty]] called on TestObject"; "DefineOwnProperty")]
        #[test_case(FunctionId::Set(None) => "[[Set]] called on TestObject"; "Set")]
        fn error(method: FunctionId) -> String {
            // Setup
            setup_test_agent();
            let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
            let global_object = TestObject::object(&[method]);
            let this_object = ordinary_object_create(Some(object_prototype));
            let ger = GlobalEnvironmentRecord::new(global_object, this_object, "test");

            let err = ger.create_global_var_binding(JSString::from("anything"), true).unwrap_err();
            unwind_type_error(err)
        }
    }

    mod create_global_function_binding {
        use super::*;
        use test_case::test_case;

        #[test_case("not_present", true => Some((ECMAScriptValue::from("unique"), true, true, true)); "not present; deletable")]
        #[test_case("not_present", false => Some((ECMAScriptValue::from("unique"), true, true, false)); "not present; permanent")]
        #[test_case("normal_var", true => Some((ECMAScriptValue::from("unique"), true, true, true)); "normal; deletable")]
        #[test_case("normal_var", false => Some((ECMAScriptValue::from("unique"), true, true, false)); "normal; permanent")]
        #[test_case("non_config_var", true => Some((ECMAScriptValue::from("unique"), true, true, false)); "not cfgable; deletable")]
        #[test_case("non_config_var", false => Some((ECMAScriptValue::from("unique"), true, true, false)); "not cfgable; permanent")]
        fn happy(name: &str, deletable: bool) -> Option<(ECMAScriptValue, bool, bool, bool)> {
            setup_test_agent();
            let ger = setup();
            let test_name = JSString::from(name);

            ger.create_global_function_binding(test_name.clone(), ECMAScriptValue::from("unique"), deletable).unwrap();

            assert!(ger.var_names.borrow().contains(&test_name));
            let opt_desc = ger.object_record.binding_object.o.get_own_property(&PropertyKey::from(test_name)).unwrap();
            match opt_desc {
                None => None,
                Some(desc) => {
                    if let PropertyKind::Data(data) = desc.property {
                        Some((data.value, data.writable, desc.enumerable, desc.configurable))
                    } else {
                        panic!("Expected data property, found an accessor property: {desc:?}");
                    }
                }
            }
        }

        #[test_case(FunctionId::GetOwnProperty(None) => "[[GetOwnProperty]] called on TestObject"; "GetOwnProperty")]
        #[test_case(FunctionId::DefineOwnProperty(None) => "[[DefineOwnProperty]] called on TestObject"; "DefineOwnProperty")]
        #[test_case(FunctionId::Set(None) => "[[Set]] called on TestObject"; "Set")]
        fn error(method: FunctionId) -> String {
            // Setup
            setup_test_agent();
            let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
            let global_object = TestObject::object(&[method]);
            let this_object = ordinary_object_create(Some(object_prototype));
            let ger = GlobalEnvironmentRecord::new(global_object, this_object, "test");

            let err = ger
                .create_global_function_binding(JSString::from("anything"), ECMAScriptValue::Undefined, true)
                .unwrap_err();
            unwind_type_error(err)
        }
    }

    #[test]
    fn new() {
        setup_test_agent();
        let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let global_object = ordinary_object_create(Some(object_prototype.clone()));
        let this_object = ordinary_object_create(Some(object_prototype));
        let ger = GlobalEnvironmentRecord::new(global_object.clone(), this_object.clone(), "test");

        assert_eq!(ger.object_record.binding_object, global_object);
        assert_eq!(ger.global_this_value, this_object);
        assert_eq!(ger.var_names.borrow().len(), 0);
    }

    #[test]
    #[should_panic(expected = "unreachable")]
    fn bind_this_value() {
        setup_test_agent();
        let ger = setup();
        ger.bind_this_value(ECMAScriptValue::Undefined).unwrap();
    }

    #[test]
    fn name() {
        setup_test_agent();
        let ger = setup();
        assert_eq!(ger.name(), "test");
    }

    #[test]
    fn var_decls() {
        setup_test_agent();
        let ger = setup();
        let vd_list = ger.var_decls();
        assert_eq!(vd_list.len(), 1);
        assert!(vd_list.contains(&JSString::from("normal_var")));
    }

    #[test]
    fn lex_decls() {
        setup_test_agent();
        let ger = setup();
        let lex_list = ger.lex_decls();
        assert_eq!(lex_list.len(), 4);
        assert!(lex_list.contains(&JSString::from("lexical_sloppy")));
        assert!(lex_list.contains(&JSString::from("lexical_permanent")));
        assert!(lex_list.contains(&JSString::from("lexical_strict")));
        assert!(lex_list.contains(&JSString::from("lexical_deletable")));
    }

    #[test]
    fn binding_names() {
        setup_test_agent();
        let ger = setup();
        let bindings = ger.binding_names();
        assert_eq!(bindings.len(), 5);
        assert!(bindings.contains(&JSString::from("lexical_sloppy")));
        assert!(bindings.contains(&JSString::from("lexical_permanent")));
        assert!(bindings.contains(&JSString::from("lexical_strict")));
        assert!(bindings.contains(&JSString::from("lexical_deletable")));
        assert!(bindings.contains(&JSString::from("normal_var")));
    }
}

mod get_identifier_reference {
    use super::*;
    use test_case::test_case;

    #[test_case("bob", true => (true, ReferencedName::from("bob"), true, None); "strict")]
    #[test_case("bob", false => (true, ReferencedName::from("bob"), false, None); "sloppy")]
    fn no_env(name: &str, strict: bool) -> (bool, ReferencedName, bool, Option<ECMAScriptValue>) {
        setup_test_agent();
        let reference = get_identifier_reference(None, JSString::from(name), strict).unwrap();
        (
            matches!(reference.base, Base::Unresolvable),
            reference.referenced_name,
            reference.strict,
            reference.this_value,
        )
    }

    #[derive(PartialEq, Eq, Debug)]
    enum EnvResult {
        Unresolvable, // Base::Unresolvable
        SelfEnv,      // Environment(e) where e is the arg
        ParentEnv,    // Environment(e) where e is arg's parent
    }

    #[test_case("bob", true => (EnvResult::Unresolvable, ReferencedName::from("bob"), true, None); "not-present; strict")]
    #[test_case("bob", false => (EnvResult::Unresolvable, ReferencedName::from("bob"), false, None); "not-present; sloppy")]
    #[test_case("present", true => (EnvResult::SelfEnv, ReferencedName::from("present"), true, None); "present; strict")]
    #[test_case("present", false => (EnvResult::SelfEnv, ReferencedName::from("present"), false, None); "present; sloppy")]
    #[test_case("parent", true => (EnvResult::ParentEnv, ReferencedName::from("parent"), true, None); "parent; strict")]
    #[test_case("parent", false => (EnvResult::ParentEnv, ReferencedName::from("parent"), false, None); "parent; sloppy")]
    fn some_env(name: &str, strict: bool) -> (EnvResult, ReferencedName, bool, Option<ECMAScriptValue>) {
        setup_test_agent();
        let parent = DeclarativeEnvironmentRecord::new(None, "test");
        parent.create_immutable_binding(JSString::from("parent"), true).unwrap();
        parent.initialize_binding(&JSString::from("parent"), ECMAScriptValue::from("testing")).unwrap();
        let rcparent: Rc<dyn EnvironmentRecord> = Rc::new(parent);
        let env = DeclarativeEnvironmentRecord::new(Some(Rc::clone(&rcparent)), "inner");
        env.create_immutable_binding(JSString::from("present"), true).unwrap();
        env.initialize_binding(&JSString::from("present"), ECMAScriptValue::from("testing")).unwrap();
        let rcenv: Rc<dyn EnvironmentRecord> = Rc::new(env);

        let result = get_identifier_reference(Some(Rc::clone(&rcenv)), JSString::from(name), strict).unwrap();
        (
            match &result.base {
                Base::Unresolvable => EnvResult::Unresolvable,
                Base::Environment(e) => {
                    if e.name() == "inner" {
                        EnvResult::SelfEnv
                    } else if e.name() == "test" {
                        EnvResult::ParentEnv
                    } else {
                        panic!("Strange environment came back")
                    }
                }
                Base::Value(_) => panic!("Variable base came back"),
            },
            result.referenced_name,
            result.strict,
            result.this_value,
        )
    }

    #[test]
    fn error() {
        setup_test_agent();
        let binding_object = TestObject::object(&[FunctionId::HasProperty(None)]);
        let env = ObjectEnvironmentRecord::new(binding_object, false, None, "test");
        let rcenv: Rc<dyn EnvironmentRecord> = Rc::new(env);

        let result = get_identifier_reference(Some(Rc::clone(&rcenv)), JSString::from("anything"), true);

        let err = result.unwrap_err();
        let msg = unwind_type_error(err);
        assert_eq!(msg, "[[HasProperty]] called on TestObject");
    }
}

mod private_environment_record {
    use super::*;

    #[test]
    fn debug() {
        let pe = PrivateEnvironmentRecord { outer_private_environment: None, names: vec![] };
        assert_ne!(format!("{pe:?}"), "");
    }

    #[test]
    fn new() {
        let pe = PrivateEnvironmentRecord::new(None);
        assert!(pe.outer_private_environment.is_none());
        assert!(pe.names.is_empty());
    }

    mod resolve_private_identifier {
        use super::*;

        fn setup() -> (Rc<RefCell<PrivateEnvironmentRecord>>, PrivateName, PrivateName) {
            let outer = Rc::new(RefCell::new(PrivateEnvironmentRecord::new(None)));
            let outer_name = PrivateName::new("outer");
            outer.borrow_mut().names.push(outer_name.clone());
            let inner = Rc::new(RefCell::new(PrivateEnvironmentRecord::new(Some(outer))));
            let inner_name = PrivateName::new("inner");
            inner.borrow_mut().names.push(inner_name.clone());
            (inner, outer_name, inner_name)
        }

        #[test]
        fn outer() {
            let (env, outer_name, _) = setup();
            let resolved = env.borrow().resolve_private_identifier(&JSString::from("outer"));
            assert_eq!(resolved, outer_name);
        }
        #[test]
        fn inner() {
            let (env, _, inner_name) = setup();
            let resolved = env.borrow().resolve_private_identifier(&JSString::from("inner"));
            assert_eq!(resolved, inner_name);
        }
    }
}

mod concisely_printed_environment_record {
    use super::*;

    #[test]
    fn debug() {
        setup_test_agent();
        let env = DeclarativeEnvironmentRecord::new(None, "test-sentinel");
        let cper = ConciselyPrintedEnvironmentRecord(Rc::new(env));
        let repr = format!("{cper:#?}");
        assert_eq!(repr.lines().collect::<Vec<_>>().len(), 1);
    }
}

mod concise_global_environment_record {
    use super::*;

    #[test]
    fn debug() {
        setup_test_agent();
        let realm = current_realm_record().unwrap();
        let ge = realm.borrow().global_env.as_ref().unwrap().clone();
        let cge = ConciseGlobalEnvironmentRecord(ge.as_ref());

        let repr = format!("{cge:?}");
        assert_eq!(repr.lines().collect::<Vec<_>>().len(), 1);
    }
}

mod concise_optional_global_environment_record {
    use super::*;

    #[test]
    fn debug_some() {
        setup_test_agent();
        let realm = current_realm_record().unwrap();
        let ge = realm.borrow().global_env.as_ref().unwrap().clone();
        let coge = ConciseOptionalGlobalEnvironmentRecord(Some(ge));

        let repr = format!("{coge:?}");
        assert_eq!(repr.lines().collect::<Vec<_>>().len(), 1);
    }

    #[test]
    fn debug_none() {
        let coge = ConciseOptionalGlobalEnvironmentRecord(None);

        let repr = format!("{coge:?}");
        assert_eq!(repr.lines().collect::<Vec<_>>().len(), 1);
    }
}
