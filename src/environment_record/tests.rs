use super::*;
use crate::agent::WksId;
use crate::object::{define_property_or_throw, ordinary_object_create, DeadObject, PotentialPropertyDescriptor, PropertyKind};
use crate::realm::IntrinsicId;
use crate::tests::{test_agent, unwind_reference_error, unwind_type_error};

const ALL_REMOVABILITY: [Removability; 2] = [Removability::Deletable, Removability::Permanent];

mod removability {
    use super::*;
    #[test]
    fn debug() {
        for val in ALL_REMOVABILITY {
            assert_ne!(format!("{:?}", val), "");
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

const ALL_STRICTNESS: [Strictness; 2] = [Strictness::Strict, Strictness::Sloppy];

mod strictness {
    use super::*;
    #[test]
    fn debug() {
        for val in ALL_STRICTNESS {
            assert_ne!(format!("{:?}", val), "");
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
            assert_ne!(format!("{:?}", Mutability::Mutable(r)), "");
        }
        for s in ALL_STRICTNESS {
            assert_ne!(format!("{:?}", Mutability::Immutable(s)), "");
        }
    }
    #[test]
    fn eq() {
        for left in ALL_REMOVABILITY {
            for right in ALL_REMOVABILITY {
                assert_eq!(Mutability::Mutable(left) == Mutability::Mutable(right), left == right);
            }
            for right in ALL_STRICTNESS {
                assert_eq!(Mutability::Mutable(left) == Mutability::Immutable(right), false);
            }
        }
        for left in ALL_STRICTNESS {
            for right in ALL_REMOVABILITY {
                assert_eq!(Mutability::Immutable(left) == Mutability::Mutable(right), false);
            }
            for right in ALL_STRICTNESS {
                assert_eq!(Mutability::Immutable(left) == Mutability::Immutable(right), left == right);
            }
        }
    }
    #[test]
    fn ne() {
        for left in ALL_REMOVABILITY {
            for right in ALL_REMOVABILITY {
                assert_eq!(Mutability::Mutable(left) != Mutability::Mutable(right), left != right);
            }
            for right in ALL_STRICTNESS {
                assert_eq!(Mutability::Mutable(left) != Mutability::Immutable(right), true);
            }
        }
        for left in ALL_STRICTNESS {
            for right in ALL_REMOVABILITY {
                assert_eq!(Mutability::Immutable(left) != Mutability::Mutable(right), true);
            }
            for right in ALL_STRICTNESS {
                assert_eq!(Mutability::Immutable(left) != Mutability::Immutable(right), left != right);
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
        assert_ne!(format!("{:?}", Binding { value: Some(ECMAScriptValue::Null), mutability: Mutability::Immutable(Strictness::Sloppy) }), "");
    }
}

mod declarative_environment_record {
    use super::*;
    #[test]
    fn debug() {
        let der = DeclarativeEnvironmentRecord::new(None);
        assert_ne!(format!("{:?}", der), "");
    }

    #[test]
    fn has_binding() {
        let mut agent = test_agent();
        let der = DeclarativeEnvironmentRecord::new(None);
        der.create_mutable_binding(&mut agent, JSString::from("a"), true).unwrap();

        assert_eq!(der.has_binding(&mut agent, &JSString::from("a")).unwrap(), true);
        assert_eq!(der.has_binding(&mut agent, &JSString::from("b")).unwrap(), false);
    }

    #[test]
    fn create_mutable_binding() {
        let mut agent = test_agent();
        let der = DeclarativeEnvironmentRecord::new(None);

        der.create_mutable_binding(&mut agent, JSString::from("a"), true).unwrap();
        der.create_mutable_binding(&mut agent, JSString::from("b"), false).unwrap();

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
        let mut agent = test_agent();
        let der = DeclarativeEnvironmentRecord::new(None);

        der.create_immutable_binding(&mut agent, JSString::from("a"), true).unwrap();
        der.create_immutable_binding(&mut agent, JSString::from("b"), false).unwrap();

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
        let mut agent = test_agent();
        let der = DeclarativeEnvironmentRecord::new(None);
        der.create_immutable_binding(&mut agent, JSString::from("a"), true).unwrap();
        der.create_mutable_binding(&mut agent, JSString::from("b"), true).unwrap();

        der.initialize_binding(&mut agent, &JSString::from("a"), ECMAScriptValue::from("value")).unwrap();
        der.initialize_binding(&mut agent, &JSString::from("b"), ECMAScriptValue::from("other")).unwrap();

        let bindings = der.bindings.borrow();
        let binding = bindings.get(&JSString::from("a")).unwrap();
        assert_eq!(binding.value, Some(ECMAScriptValue::from("value")));
        let binding = bindings.get(&JSString::from("b")).unwrap();
        assert_eq!(binding.value, Some(ECMAScriptValue::from("other")));
    }
    #[test]
    fn set_mutable_binding_01() {
        let mut agent = test_agent();
        let der = DeclarativeEnvironmentRecord::new(None);

        let err = der.set_mutable_binding(&mut agent, JSString::from("a"), ECMAScriptValue::from(10), true).unwrap_err();
        let msg = unwind_reference_error(&mut agent, err);
        assert_eq!(msg, "Identifier not defined");
    }
    #[test]
    fn set_mutable_binding_02() {
        let mut agent = test_agent();
        let der = DeclarativeEnvironmentRecord::new(None);

        der.set_mutable_binding(&mut agent, JSString::from("a"), ECMAScriptValue::from(10), false).unwrap();

        let bindings = der.bindings.borrow();
        let binding = bindings.get(&JSString::from("a")).unwrap();
        assert_eq!(binding.value, Some(ECMAScriptValue::from(10)));
        assert_eq!(binding.mutability, Mutability::Mutable(Removability::Deletable));
    }
    #[test]
    fn set_mutable_binding_03() {
        let mut agent = test_agent();
        let der = DeclarativeEnvironmentRecord::new(None);
        der.create_immutable_binding(&mut agent, JSString::from("a"), true).unwrap();

        let err = der.set_mutable_binding(&mut agent, JSString::from("a"), ECMAScriptValue::from(10), false).unwrap_err();
        let msg = unwind_reference_error(&mut agent, err);
        assert_eq!(msg, "Binding not initialized");
    }
    #[test]
    fn set_mutable_binding_04() {
        let mut agent = test_agent();
        let der = DeclarativeEnvironmentRecord::new(None);
        der.create_immutable_binding(&mut agent, JSString::from("a"), true).unwrap();
        der.initialize_binding(&mut agent, &JSString::from("a"), ECMAScriptValue::from(1)).unwrap();

        let err = der.set_mutable_binding(&mut agent, JSString::from("a"), ECMAScriptValue::from(10), false).unwrap_err();
        let msg = unwind_type_error(&mut agent, err);
        assert_eq!(msg, "Cannot change read-only value");
    }
    #[test]
    fn set_mutable_binding_05() {
        let mut agent = test_agent();
        let der = DeclarativeEnvironmentRecord::new(None);
        der.create_immutable_binding(&mut agent, JSString::from("a"), false).unwrap();
        der.initialize_binding(&mut agent, &JSString::from("a"), ECMAScriptValue::from(1)).unwrap();

        let err = der.set_mutable_binding(&mut agent, JSString::from("a"), ECMAScriptValue::from(10), true).unwrap_err();
        let msg = unwind_type_error(&mut agent, err);
        assert_eq!(msg, "Cannot change read-only value");
    }
    #[test]
    fn set_mutable_binding_06() {
        let mut agent = test_agent();
        let der = DeclarativeEnvironmentRecord::new(None);
        der.create_immutable_binding(&mut agent, JSString::from("a"), false).unwrap();
        der.initialize_binding(&mut agent, &JSString::from("a"), ECMAScriptValue::from(1)).unwrap();

        der.set_mutable_binding(&mut agent, JSString::from("a"), ECMAScriptValue::from(10), false).unwrap();

        let bindings = der.bindings.borrow();
        let binding = bindings.get(&JSString::from("a")).unwrap();
        assert_eq!(binding.value, Some(ECMAScriptValue::from(1)));
    }
    #[test]
    fn set_mutable_binding_07() {
        let mut agent = test_agent();
        let der = DeclarativeEnvironmentRecord::new(None);
        der.create_mutable_binding(&mut agent, JSString::from("a"), false).unwrap();
        der.initialize_binding(&mut agent, &JSString::from("a"), ECMAScriptValue::from(1)).unwrap();

        der.set_mutable_binding(&mut agent, JSString::from("a"), ECMAScriptValue::from(10), false).unwrap();

        let bindings = der.bindings.borrow();
        let binding = bindings.get(&JSString::from("a")).unwrap();
        assert_eq!(binding.value, Some(ECMAScriptValue::from(10)));
    }
    #[test]
    fn get_binding_value_01() {
        let mut agent = test_agent();
        let der = DeclarativeEnvironmentRecord::new(None);
        der.create_mutable_binding(&mut agent, JSString::from("a"), false).unwrap();
        der.initialize_binding(&mut agent, &JSString::from("a"), ECMAScriptValue::from(1)).unwrap();

        let result = der.get_binding_value(&mut agent, &JSString::from("a"), false).unwrap();

        assert_eq!(result, ECMAScriptValue::from(1));
    }
    #[test]
    fn get_binding_value_02() {
        let mut agent = test_agent();
        let der = DeclarativeEnvironmentRecord::new(None);
        der.create_mutable_binding(&mut agent, JSString::from("a"), false).unwrap();

        let result = der.get_binding_value(&mut agent, &JSString::from("a"), false).unwrap_err();
        let msg = unwind_reference_error(&mut agent, result);

        assert_eq!(msg, "Binding not initialized");
    }
    #[test]
    fn delete_binding_01() {
        let mut agent = test_agent();
        let der = DeclarativeEnvironmentRecord::new(None);
        der.create_mutable_binding(&mut agent, JSString::from("permanent"), false).unwrap();

        let result = der.delete_binding(&mut agent, &JSString::from("permanent")).unwrap();

        assert_eq!(result, false);
        assert!(der.has_binding(&mut agent, &JSString::from("permanent")).unwrap());
    }
    #[test]
    fn delete_binding_02() {
        let mut agent = test_agent();
        let der = DeclarativeEnvironmentRecord::new(None);
        der.create_mutable_binding(&mut agent, JSString::from("deletable"), true).unwrap();

        let result = der.delete_binding(&mut agent, &JSString::from("deletable")).unwrap();

        assert_eq!(result, true);
        assert!(!der.has_binding(&mut agent, &JSString::from("deletable")).unwrap());
    }
    #[test]
    fn delete_binding_03() {
        let mut agent = test_agent();
        let der = DeclarativeEnvironmentRecord::new(None);
        der.create_immutable_binding(&mut agent, JSString::from("immutable"), true).unwrap();

        let result = der.delete_binding(&mut agent, &JSString::from("immutable")).unwrap();

        assert_eq!(result, false);
        assert!(der.has_binding(&mut agent, &JSString::from("immutable")).unwrap());
    }
    #[test]
    fn has_this_binding() {
        let der = DeclarativeEnvironmentRecord::new(None);

        assert_eq!(der.has_this_binding(), false);
    }
    #[test]
    fn has_super_binding() {
        let der = DeclarativeEnvironmentRecord::new(None);

        assert_eq!(der.has_super_binding(), false);
    }
    #[test]
    fn with_base_object() {
        let der = DeclarativeEnvironmentRecord::new(None);

        assert!(der.with_base_object().is_none());
    }
    #[test]
    fn get_outer_env() {
        let mut agent = test_agent();
        let der = Rc::new(DeclarativeEnvironmentRecord::new(None));
        der.create_immutable_binding(&mut agent, JSString::from("sentinel"), true).unwrap();
        der.initialize_binding(&mut agent, &JSString::from("sentinel"), ECMAScriptValue::from("very unique string")).unwrap();
        let der2 = DeclarativeEnvironmentRecord::new(Some(der));

        let outer = der2.get_outer_env().unwrap();

        let val_from_outer = outer.get_binding_value(&mut agent, &JSString::from("sentinel"), true).unwrap();
        assert_eq!(val_from_outer, ECMAScriptValue::from("very unique string"));
    }
}

mod object_environment_record {
    use super::*;
    #[test]
    fn object_environment_record_debug() {
        let mut agent = test_agent();
        let object_prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let binding_object = ordinary_object_create(&mut agent, Some(&object_prototype), &[]);
        let oer = ObjectEnvironmentRecord::new(binding_object, false, None);

        println!("{:#?}", oer);
        assert_ne!(format!("{:?}", oer), "");
    }
    #[test]
    fn object_environment_record_has_binding_01() {
        let mut agent = test_agent();
        let object_prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let binding_object = ordinary_object_create(&mut agent, Some(&object_prototype), &[]);
        let oer = ObjectEnvironmentRecord::new(binding_object, false, None);

        let result = oer.has_binding(&mut agent, &JSString::from("not_here")).unwrap();
        assert_eq!(result, false);
    }
    #[test]
    fn object_environment_record_has_binding_02() {
        let mut agent = test_agent();
        let object_prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let binding_object = ordinary_object_create(&mut agent, Some(&object_prototype), &[]);
        define_property_or_throw(
            &mut agent,
            &binding_object,
            PropertyKey::from("exists"),
            PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(true)), writable: Some(true), enumerable: Some(true), configurable: Some(true), ..Default::default() },
        )
        .unwrap();
        let oer = ObjectEnvironmentRecord::new(binding_object, false, None);

        let result = oer.has_binding(&mut agent, &JSString::from("exists")).unwrap();
        assert_eq!(result, true);
    }
    #[test]
    fn object_environment_record_has_binding_03() {
        let mut agent = test_agent();
        let object_prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let binding_object = ordinary_object_create(&mut agent, Some(&object_prototype), &[]);
        define_property_or_throw(
            &mut agent,
            &binding_object,
            PropertyKey::from("exists"),
            PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(true)), writable: Some(true), enumerable: Some(true), configurable: Some(true), ..Default::default() },
        )
        .unwrap();
        let oer = ObjectEnvironmentRecord::new(binding_object, true, None);

        let result = oer.has_binding(&mut agent, &JSString::from("exists")).unwrap();

        assert_eq!(result, true);
    }
    #[test]
    fn object_environment_record_has_binding_04() {
        let mut agent = test_agent();
        let object_prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        // unscopables_obj = {
        //    hidden: 10,
        //    visible: false
        // }
        let unscopables_obj = ordinary_object_create(&mut agent, Some(&object_prototype), &[]);
        define_property_or_throw(
            &mut agent,
            &unscopables_obj,
            PropertyKey::from("hidden"),
            PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(10)), writable: Some(true), enumerable: Some(true), configurable: Some(true), ..Default::default() },
        )
        .unwrap();
        define_property_or_throw(
            &mut agent,
            &unscopables_obj,
            PropertyKey::from("visible"),
            PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(false)), writable: Some(true), enumerable: Some(true), configurable: Some(true), ..Default::default() },
        )
        .unwrap();
        // binding_object = {
        //    [Symbol.unscopables]: unscopables_obj,
        //    visible: "This name is in the environment"
        //    hidden: "This name is not in the environment"
        //    also: "This one also visible"
        // }
        let binding_object = ordinary_object_create(&mut agent, Some(&object_prototype), &[]);
        define_property_or_throw(
            &mut agent,
            &binding_object,
            PropertyKey::from("visible"),
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
            &mut agent,
            &binding_object,
            PropertyKey::from("hidden"),
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
            &mut agent,
            &binding_object,
            PropertyKey::from("also"),
            PotentialPropertyDescriptor {
                value: Some(ECMAScriptValue::from("This one also visible")),
                writable: Some(true),
                enumerable: Some(true),
                configurable: Some(true),
                ..Default::default()
            },
        )
        .unwrap();
        let unscopables_sym = agent.wks(WksId::Unscopables);
        define_property_or_throw(
            &mut agent,
            &binding_object,
            PropertyKey::from(unscopables_sym),
            PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(unscopables_obj)), writable: Some(true), enumerable: Some(true), configurable: Some(true), ..Default::default() },
        )
        .unwrap();
        let oer = ObjectEnvironmentRecord::new(binding_object, true, None);

        assert!(oer.has_binding(&mut agent, &JSString::from("visible")).unwrap());
        assert!(!oer.has_binding(&mut agent, &JSString::from("hidden")).unwrap());
        assert!(oer.has_binding(&mut agent, &JSString::from("also")).unwrap());
    }
    #[test]
    fn object_environment_record_has_binding_05() {
        // has_property returns an error
        let mut agent = test_agent();
        let binding_object = DeadObject::object(&mut agent);
        let oer = ObjectEnvironmentRecord::new(binding_object, true, None);

        let err = oer.has_binding(&mut agent, &JSString::from("random")).unwrap_err();
        let msg = unwind_type_error(&mut agent, err);
        assert_eq!(msg, "has_property called on DeadObject");
    }
    #[test]
    fn object_environment_record_has_binding_06() {
        // binding_object.get(@@unscopables) fails
        let mut agent = test_agent();
        let object_prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let binding_object = ordinary_object_create(&mut agent, Some(&object_prototype), &[]);
        // binding_object = {
        //    get [Symbol.unscopables] = %ThrowTypeError%
        //    field: true
        // }
        let tte = agent.intrinsic(IntrinsicId::ThrowTypeError);
        let pk = PropertyKey::from(agent.wks(WksId::Unscopables));
        let property = PotentialPropertyDescriptor { get: Some(ECMAScriptValue::from(tte)), enumerable: Some(true), configurable: Some(true), ..Default::default() };
        define_property_or_throw(&mut agent, &binding_object, pk, property).unwrap();
        let pk = PropertyKey::from("field");
        let property = PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(true)), writable: Some(true), enumerable: Some(true), configurable: Some(true), ..Default::default() };
        define_property_or_throw(&mut agent, &binding_object, pk, property).unwrap();
        let oer = ObjectEnvironmentRecord::new(binding_object, true, None);

        let err = oer.has_binding(&mut agent, &JSString::from("field")).unwrap_err();

        let msg = unwind_type_error(&mut agent, err);
        assert_eq!(msg, "Generic TypeError");
    }
    #[test]
    fn object_environment_record_has_binding_07() {
        // binding_object.@@unscopables.get(field) fails
        let mut agent = test_agent();
        let object_prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let binding_object = ordinary_object_create(&mut agent, Some(&object_prototype), &[]);
        // binding_object = {
        //    [Symbol.unscopables] = {
        //        get field = %ThrowTypeError%
        //    }
        //    field: true
        // }
        let unscopables_obj = ordinary_object_create(&mut agent, Some(&object_prototype), &[]);
        let tte = agent.intrinsic(IntrinsicId::ThrowTypeError);
        let pk = PropertyKey::from("field");
        let property = PotentialPropertyDescriptor { get: Some(ECMAScriptValue::from(tte)), enumerable: Some(true), configurable: Some(true), ..Default::default() };
        define_property_or_throw(&mut agent, &unscopables_obj, pk.clone(), property).unwrap();
        let property = PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(true)), writable: Some(true), enumerable: Some(true), configurable: Some(true), ..Default::default() };
        define_property_or_throw(&mut agent, &binding_object, pk, property).unwrap();
        let pk = PropertyKey::from(agent.wks(WksId::Unscopables));
        let property =
            PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(unscopables_obj)), writable: Some(true), enumerable: Some(true), configurable: Some(true), ..Default::default() };
        define_property_or_throw(&mut agent, &binding_object, pk, property).unwrap();
        let oer = ObjectEnvironmentRecord::new(binding_object, true, None);

        let err = oer.has_binding(&mut agent, &JSString::from("field")).unwrap_err();

        let msg = unwind_type_error(&mut agent, err);
        assert_eq!(msg, "Generic TypeError");
    }

    #[test]
    fn object_environment_record_create_mutable_binding() {
        let mut agent = test_agent();
        let object_prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let binding_object = ordinary_object_create(&mut agent, Some(&object_prototype), &[]);
        let oer = ObjectEnvironmentRecord::new(binding_object.clone(), true, None);

        oer.create_mutable_binding(&mut agent, JSString::from("can_delete"), true).unwrap();
        oer.create_mutable_binding(&mut agent, JSString::from("permanent"), false).unwrap();

        let can_delete_key = PropertyKey::from("can_delete");
        let cd_desc = binding_object.o.get_own_property(&mut agent, &can_delete_key).unwrap().unwrap();
        assert_eq!(cd_desc.enumerable, true);
        assert_eq!(cd_desc.configurable, true);
        assert!(cd_desc.is_data_descriptor());
        if let PropertyKind::Data(data) = cd_desc.property {
            assert_eq!(data.value, ECMAScriptValue::Undefined);
            assert_eq!(data.writable, true);
        }

        let permanent_key = PropertyKey::from("permanent");
        let perm_desc = binding_object.o.get_own_property(&mut agent, &permanent_key).unwrap().unwrap();
        assert_eq!(perm_desc.enumerable, true);
        assert_eq!(perm_desc.configurable, false);
        assert!(perm_desc.is_data_descriptor());
        if let PropertyKind::Data(data) = perm_desc.property {
            assert_eq!(data.value, ECMAScriptValue::Undefined);
            assert_eq!(data.writable, true);
        }
    }

    #[test]
    #[should_panic]
    fn object_environment_record_create_immutable_binding() {
        let mut agent = test_agent();
        let object_prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let binding_object = ordinary_object_create(&mut agent, Some(&object_prototype), &[]);
        let oer = ObjectEnvironmentRecord::new(binding_object, true, None);

        oer.create_immutable_binding(&mut agent, JSString::from("nothing"), true).unwrap();
    }

    #[test]
    fn object_environment_record_initialize_binding() {
        let mut agent = test_agent();
        let object_prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let binding_object = ordinary_object_create(&mut agent, Some(&object_prototype), &[]);
        let oer = ObjectEnvironmentRecord::new(binding_object.clone(), true, None);
        let name = JSString::from("colorado");
        oer.create_mutable_binding(&mut agent, name.clone(), true).unwrap();

        oer.initialize_binding(&mut agent, &name, ECMAScriptValue::from(76)).unwrap();

        let key = PropertyKey::from(name);
        let desc = binding_object.o.get_own_property(&mut agent, &key).unwrap().unwrap();
        assert_eq!(desc.enumerable, true);
        assert_eq!(desc.configurable, true);
        assert!(desc.is_data_descriptor());
        if let PropertyKind::Data(data) = desc.property {
            assert_eq!(data.value, ECMAScriptValue::from(76));
            assert_eq!(data.writable, true);
        }
    }

    #[test]
    fn object_environment_record_set_mutable_binding_01() {
        let mut agent = test_agent();
        let object_prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let binding_object = ordinary_object_create(&mut agent, Some(&object_prototype), &[]);
        let oer = ObjectEnvironmentRecord::new(binding_object.clone(), true, None);
        let name = JSString::from("vegetable");
        oer.create_mutable_binding(&mut agent, name.clone(), true).unwrap();
        oer.initialize_binding(&mut agent, &name, ECMAScriptValue::from(true)).unwrap();

        oer.set_mutable_binding(&mut agent, name.clone(), ECMAScriptValue::from(false), true).unwrap();

        let key = PropertyKey::from(name);
        let desc = binding_object.o.get_own_property(&mut agent, &key).unwrap().unwrap();
        assert_eq!(desc.enumerable, true);
        assert_eq!(desc.configurable, true);
        assert!(desc.is_data_descriptor());
        if let PropertyKind::Data(data) = desc.property {
            assert_eq!(data.value, ECMAScriptValue::from(false));
            assert_eq!(data.writable, true);
        }
    }
    #[test]
    fn object_environment_record_set_mutable_binding_02() {
        // binding that's been deleted (or was never there)
        let mut agent = test_agent();
        let object_prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let binding_object = ordinary_object_create(&mut agent, Some(&object_prototype), &[]);
        let oer = ObjectEnvironmentRecord::new(binding_object, true, None);
        let name = JSString::from("vegetable");

        let err = oer.set_mutable_binding(&mut agent, name, ECMAScriptValue::Undefined, true).unwrap_err();

        let msg = unwind_reference_error(&mut agent, err);
        assert_eq!(msg, "Reference no longer exists");
    }
    #[test]
    fn object_environment_record_set_mutable_binding_03() {
        // has_property throws
        let mut agent = test_agent();
        let binding_object = DeadObject::object(&mut agent);
        let oer = ObjectEnvironmentRecord::new(binding_object, true, None);
        let name = JSString::from("vegetable");

        let err = oer.set_mutable_binding(&mut agent, name, ECMAScriptValue::Undefined, true).unwrap_err();

        let msg = unwind_type_error(&mut agent, err);
        assert_eq!(msg, "has_property called on DeadObject");
    }
    #[test]
    fn object_environment_record_set_mutable_binding_04() {
        // set throws
        let mut agent = test_agent();
        let object_prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let binding_object = ordinary_object_create(&mut agent, Some(&object_prototype), &[]);
        let name = JSString::from("vegetable");
        let key = PropertyKey::from(name.clone());
        let tte = agent.intrinsic(IntrinsicId::ThrowTypeError);
        let property = PotentialPropertyDescriptor { set: Some(ECMAScriptValue::from(tte)), enumerable: Some(true), configurable: Some(true), ..Default::default() };
        define_property_or_throw(&mut agent, &binding_object, key, property).unwrap();
        let oer = ObjectEnvironmentRecord::new(binding_object, true, None);

        let err = oer.set_mutable_binding(&mut agent, name, ECMAScriptValue::Undefined, true).unwrap_err();

        let msg = unwind_type_error(&mut agent, err);
        assert_eq!(msg, "Generic TypeError");
    }

    #[test]
    fn object_environment_record_get_binding_value_01() {
        let mut agent = test_agent();
        let object_prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let binding_object = ordinary_object_create(&mut agent, Some(&object_prototype), &[]);
        let oer = ObjectEnvironmentRecord::new(binding_object, true, None);
        let name = JSString::from("vegetable");
        oer.create_mutable_binding(&mut agent, name.clone(), true).unwrap();
        oer.initialize_binding(&mut agent, &name, ECMAScriptValue::from(true)).unwrap();
        oer.set_mutable_binding(&mut agent, name.clone(), ECMAScriptValue::from("squirrel"), true).unwrap();

        let result = oer.get_binding_value(&mut agent, &name, false).unwrap();
        assert_eq!(result, ECMAScriptValue::from("squirrel"));

        let result = oer.get_binding_value(&mut agent, &JSString::from("nothere"), false).unwrap();
        assert_eq!(result, ECMAScriptValue::Undefined);

        let result = oer.get_binding_value(&mut agent, &JSString::from("a"), true).unwrap_err();
        assert_eq!(unwind_reference_error(&mut agent, result), "Unresolvable reference");
    }
    #[test]
    fn object_environment_record_get_binding_value_02() {
        // has_property throws
        let mut agent = test_agent();
        let binding_object = DeadObject::object(&mut agent);
        let oer = ObjectEnvironmentRecord::new(binding_object, true, None);
        let name = JSString::from("vegetable");

        let err = oer.get_binding_value(&mut agent, &name, true).unwrap_err();

        let msg = unwind_type_error(&mut agent, err);
        assert_eq!(msg, "has_property called on DeadObject");
    }

    #[test]
    fn object_environment_record_delete() {
        let mut agent = test_agent();
        let object_prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let binding_object = ordinary_object_create(&mut agent, Some(&object_prototype), &[]);
        let oer = ObjectEnvironmentRecord::new(binding_object, true, None);
        let name = JSString::from("vegetable");
        oer.create_mutable_binding(&mut agent, name.clone(), true).unwrap();
        oer.initialize_binding(&mut agent, &name, ECMAScriptValue::from(true)).unwrap();
        oer.set_mutable_binding(&mut agent, name.clone(), ECMAScriptValue::from("squirrel"), true).unwrap();

        oer.delete_binding(&mut agent, &name).unwrap();
        assert!(!oer.has_binding(&mut agent, &name).unwrap());
    }

    #[test]
    fn object_environment_record_has_this_binding() {
        let mut agent = test_agent();
        let object_prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let binding_object = ordinary_object_create(&mut agent, Some(&object_prototype), &[]);
        let oer = ObjectEnvironmentRecord::new(binding_object, true, None);

        assert!(!oer.has_this_binding());
    }

    #[test]
    fn object_environment_record_has_super_binding() {
        let mut agent = test_agent();
        let object_prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let binding_object = ordinary_object_create(&mut agent, Some(&object_prototype), &[]);
        let oer = ObjectEnvironmentRecord::new(binding_object, true, None);

        assert!(!oer.has_super_binding());
    }

    #[test]
    fn object_environment_record_with_base_object_01() {
        let mut agent = test_agent();
        let object_prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let binding_object = ordinary_object_create(&mut agent, Some(&object_prototype), &[]);
        let oer = ObjectEnvironmentRecord::new(binding_object.clone(), true, None);

        assert_eq!(oer.with_base_object(), Some(binding_object));
    }

    #[test]
    fn object_environment_record_with_base_object_02() {
        let mut agent = test_agent();
        let object_prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let binding_object = ordinary_object_create(&mut agent, Some(&object_prototype), &[]);
        let oer = ObjectEnvironmentRecord::new(binding_object, false, None);

        assert!(oer.with_base_object().is_none());
    }

    #[test]
    fn object_environment_record_get_outer_env() {
        let mut agent = test_agent();
        let der = Rc::new(DeclarativeEnvironmentRecord::new(None));
        der.create_immutable_binding(&mut agent, JSString::from("sentinel"), true).unwrap();
        der.initialize_binding(&mut agent, &JSString::from("sentinel"), ECMAScriptValue::from("very unique string")).unwrap();
        let object_prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let binding_object = ordinary_object_create(&mut agent, Some(&object_prototype), &[]);
        let oer = ObjectEnvironmentRecord::new(binding_object, false, Some(der));

        let outer = oer.get_outer_env().unwrap();

        let val_from_outer = outer.get_binding_value(&mut agent, &JSString::from("sentinel"), true).unwrap();
        assert_eq!(val_from_outer, ECMAScriptValue::from("very unique string"));
    }
}

mod binding_status {
    use super::*;
    const ALL_BINDINGSTATUS: [BindingStatus; 3] = [BindingStatus::Lexical, BindingStatus::Initialized, BindingStatus::Uninitialized];
    #[test]
    fn debug() {
        for val in ALL_BINDINGSTATUS {
            assert_ne!(format!("{:?}", val), "");
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
}

mod global_environment_record {
    use super::*;
    #[test]
    fn debug() {
        let mut agent = test_agent();
        let object_prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let global_object = ordinary_object_create(&mut agent, Some(&object_prototype), &[]);
        let this_object = ordinary_object_create(&mut agent, Some(&object_prototype), &[]);
        let ger = GlobalEnvironmentRecord::new(global_object, this_object);

        assert_ne!(format!("{:?}", ger), "");
    }

    mod has_binding {
        use super::*;
        #[test]
        fn happy_path() {
            let mut agent = test_agent();

            let in_object_name = JSString::from("in_object");
            let in_decl_name = JSString::from("in_decl");
            let nobody_name = JSString::from("nobody");
            let in_object_key = PropertyKey::from(in_object_name.clone());

            let object_prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
            let global_object = ordinary_object_create(&mut agent, Some(&object_prototype), &[]);
            let this_object = ordinary_object_create(&mut agent, Some(&object_prototype), &[]);
            let in_object_property =
                PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(0)), writable: Some(true), enumerable: Some(true), configurable: Some(true), ..Default::default() };
            define_property_or_throw(&mut agent, &global_object, in_object_key, in_object_property).unwrap();
            let ger = GlobalEnvironmentRecord::new(global_object, this_object);
            ger.create_mutable_binding(&mut agent, in_decl_name.clone(), true).unwrap();
            ger.initialize_binding(&mut agent, &in_decl_name, ECMAScriptValue::from(0)).unwrap();

            assert!(ger.has_binding(&mut agent, &in_decl_name).unwrap());
            assert!(ger.has_binding(&mut agent, &in_object_name).unwrap());
            assert!(!ger.has_binding(&mut agent, &nobody_name).unwrap());
        }
        #[test]
        fn error_path() {
            let mut agent = test_agent();
            let object_prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
            let global_object = DeadObject::object(&mut agent);
            let this_object = ordinary_object_create(&mut agent, Some(&object_prototype), &[]);
            let ger = GlobalEnvironmentRecord::new(global_object, this_object);

            let err = ger.has_binding(&mut agent, &JSString::from("a")).unwrap_err();
            let msg = unwind_type_error(&mut agent, err);
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
            let mut agent = test_agent();
            let object_prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
            let global_object = ordinary_object_create(&mut agent, Some(&object_prototype), &[]);
            let this_object = ordinary_object_create(&mut agent, Some(&object_prototype), &[]);
            let ger = GlobalEnvironmentRecord::new(global_object, this_object);
            let test_name = JSString::from("test");

            // Exercise function
            ger.create_mutable_binding(&mut agent, test_name.clone(), deletable).unwrap();

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
            let mut agent = test_agent();
            let object_prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
            let global_object = ordinary_object_create(&mut agent, Some(&object_prototype), &[]);
            let this_object = ordinary_object_create(&mut agent, Some(&object_prototype), &[]);
            let ger = GlobalEnvironmentRecord::new(global_object, this_object);
            let test_name = JSString::from("test");
            ger.create_mutable_binding(&mut agent, test_name.clone(), true).unwrap();

            // Exercise function
            let result = ger.create_mutable_binding(&mut agent, test_name, true);

            // Validate result
            let err = result.unwrap_err();
            let msg = unwind_type_error(&mut agent, err);
            assert_eq!(msg, "Binding already exists");
        }
    }
}
