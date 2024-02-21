use super::*;
use crate::tests::*;

mod prototype {
    use super::*;
    use test_case::test_case;

    mod valueof {
        use super::*;

        #[test]
        fn happy() {
            setup_test_agent();
            let value = ECMAScriptValue::from(10);

            let result = object_prototype_value_of(&value, None, &[]).unwrap();
            match &result {
                ECMAScriptValue::Object(obj) => {
                    assert!(obj.o.is_number_object());
                    assert_eq!(to_number(result).unwrap(), 10.0);
                }
                _ => {
                    panic!("Object.prototype.valueOf did not return an object. (Got: {result:?})");
                }
            }
        }
        #[test]
        fn err() {
            setup_test_agent();
            let result = object_prototype_value_of(&ECMAScriptValue::Undefined, None, &[]).unwrap_err();
            assert_eq!(unwind_type_error(result), "Undefined and null cannot be converted to objects");
        }
    }

    mod to_string {
        use super::*;
        use test_case::test_case;

        fn greasy() -> ECMAScriptValue {
            // Return an object whose @@toStringTag property has the value "Grease"
            let to_string_tag_symbol = wks(WksId::ToStringTag);
            let obj = ordinary_object_create(None, &[]);
            obj.set(PropertyKey::from(to_string_tag_symbol), ECMAScriptValue::from("Grease"), false).unwrap();
            ECMAScriptValue::from(obj)
        }

        #[test_case(|| ECMAScriptValue::Undefined => "[object Undefined]"; "undefined")]
        #[test_case(|| ECMAScriptValue::Null => "[object Null]"; "null")]
        #[test_case(|| ECMAScriptValue::from(99) => "[object Number]"; "number")]
        #[test_case(|| ECMAScriptValue::from(true) => "[object Boolean]"; "boolean")]
        #[test_case(|| ECMAScriptValue::from(create_type_error_object("test_error")) => "[object Error]"; "error object")]
        #[test_case(|| ECMAScriptValue::from(intrinsic(IntrinsicId::Boolean)) => "[object Function]"; "callable object")]
        #[test_case(|| ECMAScriptValue::from(ordinary_object_create(None, &[])) => "[object Object]"; "ordinary object")]
        #[test_case(greasy => "[object Grease]"; "to-string-tag")]
        #[test_case(|| ECMAScriptValue::from(DeadObject::object()) => "get called on DeadObject"; "throw getting tag")]
        fn f(make: fn() -> ECMAScriptValue) -> String {
            setup_test_agent();
            let value = make();
            match object_prototype_to_string(&value, None, &[]) {
                Ok(ok) => match ok {
                    ECMAScriptValue::String(s) => String::from(s),
                    _ => panic!("Object.prototype.toString did not return a string. (Got: {ok:?})"),
                },
                Err(err) => unwind_type_error(err),
            }
        }
    }

    #[test_case(
        || ECMAScriptValue::Undefined,
        || {
            fn behavior(
                _: &ECMAScriptValue,
                _: Option<&Object>,
                _: &[ECMAScriptValue],
            ) -> Completion<ECMAScriptValue> {
                Err(create_type_error("toPrimitive throws"))
            }
            let obj = ordinary_object_create(None, &[]);
            let to_primitive_method =
                create_builtin_function(
                    behavior,
                    false,
                    0.0,
                    "f".into(),
                    BUILTIN_FUNCTION_SLOTS,
                    current_realm_record(),
                    Some(intrinsic(IntrinsicId::FunctionPrototype)),
                    None,
                );
            let ppd = PotentialPropertyDescriptor::new().value(to_primitive_method);
            define_property_or_throw(&obj, wks(WksId::ToPrimitive), ppd).unwrap();
            obj.into()
        }
        => serr("TypeError: toPrimitive throws");
        "toPrimitive throws"
    )]
    #[test_case(
        || ECMAScriptValue::Undefined,
        || 0.into()
        => serr("TypeError: Undefined and null cannot be converted to objects");
        "toObject throws"
    )]
    #[test_case(
        || intrinsic(IntrinsicId::ObjectPrototype).into(),
        || "hasOwnProperty".into()
        => sok("true");
        "standard success"
    )]
    fn has_own_property(
        make_this: impl FnOnce() -> ECMAScriptValue,
        make_key: impl FnOnce() -> ECMAScriptValue,
    ) -> Result<String, String> {
        setup_test_agent();
        let this = make_this();
        let key = make_key();
        object_prototype_has_own_property(&this, None, &[key]).map_err(unwind_any_error).map(|v| v.test_result_string())
    }

    #[test_case(
        || {
            let proto = intrinsic(IntrinsicId::ObjectPrototype);
            let obj = ordinary_object_create(Some(proto), &[]);
            obj.into()
        }
        => sok("[object Object]");
        "success"
    )]
    fn to_locale_string(make_this: impl FnOnce() -> ECMAScriptValue) -> Result<String, String> {
        setup_test_agent();
        let this = make_this();
        object_prototype_to_locale_string(&this, None, &[]).map_err(unwind_any_error).map(|v| v.test_result_string())
    }

    #[test_case(
        || (ECMAScriptValue::Undefined, 0.into())
        => sok("false");
        "v not object, even if this is bad"
    )]
    #[test_case(
        || (ECMAScriptValue::Undefined, ordinary_object_create(None, &[]).into())
        => serr("TypeError: Undefined and null cannot be converted to objects");
        "bad this"
    )]
    #[test_case(
        || (
            ordinary_object_create(None, &[]).into(),
            TestObject::object(&[FunctionId::GetPrototypeOf]).into()
        )
        => serr("TypeError: [[GetPrototypeOf]] called on TestObject");
        "GetPrototypeOf throws"
    )]
    #[test_case(
        || (ordinary_object_create(None, &[]).into(), ordinary_object_create(None, &[]).into())
        => sok("false");
        "No shared prototypes"
    )]
    #[test_case(
        || (intrinsic(IntrinsicId::ArrayPrototype).into(), create_array_from_list(&["bob".into()]).into())
        => sok("true");
        "in chain"
    )]
    #[test_case(
        || (
            intrinsic(IntrinsicId::ObjectPrototype).into(),
            {
                let proto = intrinsic(IntrinsicId::ObjectPrototype);
                let parent = ordinary_object_create(Some(proto), &[]);
                let child = ordinary_object_create(Some(parent), &[]);
                child.into()
            }
        )
        => sok("true");
        "deep in chain"
    )]
    fn is_prototype_of(make_items: impl FnOnce() -> (ECMAScriptValue, ECMAScriptValue)) -> Result<String, String> {
        setup_test_agent();
        let (this, val) = make_items();
        object_prototype_is_prototype_of(&this, None, &[val]).map_err(unwind_any_error).map(|v| v.test_result_string())
    }

    #[test_case(
        || ECMAScriptValue::Undefined,
        || {
            fn behavior(
                _: &ECMAScriptValue,
                _: Option<&Object>,
                _: &[ECMAScriptValue],
            ) -> Completion<ECMAScriptValue> {
                Err(create_type_error("toPrimitive throws"))
            }
            let obj = ordinary_object_create(None, &[]);
            let to_primitive_method =
                create_builtin_function(
                    behavior,
                    false,
                    0.0,
                    "f".into(),
                    BUILTIN_FUNCTION_SLOTS,
                    current_realm_record(),
                    Some(intrinsic(IntrinsicId::FunctionPrototype)),
                    None,
                );
            let ppd = PotentialPropertyDescriptor::new().value(to_primitive_method);
            define_property_or_throw(&obj, wks(WksId::ToPrimitive), ppd).unwrap();
            obj.into()
        }
        => serr("TypeError: toPrimitive throws");
        "ToPropertyKey throws"
    )]
    #[test_case(
        || ECMAScriptValue::Undefined, || 0.into()
        => serr("TypeError: Undefined and null cannot be converted to objects");
        "ToObject throws"
    )]
    #[test_case(
        || TestObject::object(&[FunctionId::GetOwnProperty(Some("prop".into()))]).into(),
        || "prop".into()
        => serr("TypeError: [[GetOwnProperty]] called on TestObject");
        "GetOwnProperty throws"
    )]
    #[test_case(
        || ordinary_object_create(None, &[]).into(),
        || 0.into()
        => sok("false");
        "success; no property"
    )]
    #[test_case(
        || {
            let obj = ordinary_object_create(None, &[]);
            obj.create_data_property_or_throw("prop", 10).unwrap();
            obj.into()
        },
        || "prop".into()
        => sok("true");
        "success; prop match"
    )]
    #[test_case(
        || {
            let obj = ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)), &[]);
            let ppd = PotentialPropertyDescriptor::new().value(10).enumerable(false);
            define_property_or_throw(&obj, "prop", ppd).unwrap();
            obj.into()
        },
        || "prop".into()
        => sok("false");
        "success; prop is hidden"
    )]
    fn property_is_enumerable(
        make_this: impl FnOnce() -> ECMAScriptValue,
        make_val: impl FnOnce() -> ECMAScriptValue,
    ) -> Result<String, String> {
        setup_test_agent();
        let this = make_this();
        let val = make_val();
        object_prototype_property_is_enumerable(&this, None, &[val])
            .map_err(unwind_any_error)
            .map(|v| v.test_result_string())
    }
}

mod constructor {
    use super::*;
    use test_case::test_case;

    #[test_case(|| Some(ordinary_object_create(None, &[])), &[ECMAScriptValue::from(10)] => "10"; "new target but no active function")]
    #[test_case(|| {
        let obj = ordinary_object_create(None, &[]);
        let realm = current_realm_record().unwrap();
        push_execution_context(ExecutionContext::new(Some(obj.clone()), realm, None));
        Some(obj)
    }, &[ECMAScriptValue::from(11)] => "11"; "related new target")]
    #[test_case(|| {
        let obj = ordinary_object_create(None, &[]);
        let realm = current_realm_record().unwrap();
        push_execution_context(ExecutionContext::new(Some(obj), realm, None));
        Some(ordinary_object_create(None, &[]))
    }, &[ECMAScriptValue::from(12)] => "[object Object]"; "unrelated new target")]
    #[test_case(|| None, &[ECMAScriptValue::Null] => "[object Object]"; "null value")]
    #[test_case(|| None, &[ECMAScriptValue::Undefined] => "[object Object]"; "undefined value")]
    #[test_case(|| {
        let obj = ordinary_object_create(None, &[]);
        let realm = current_realm_record().unwrap();
        push_execution_context(ExecutionContext::new(Some(obj), realm, None));
        let nt = TestObject::object(&[FunctionId::Get(None)]);
        Some(nt)
    }, &[] => "[[Get]] called on TestObject"; "ordinary_create_from_constructor throws")]
    fn function(new_target: fn() -> Option<Object>, args: &[ECMAScriptValue]) -> String {
        setup_test_agent();
        let nt = new_target();

        match object_constructor_function(&ECMAScriptValue::Undefined, nt.as_ref(), args) {
            Ok(ok) => match ok {
                ECMAScriptValue::Object(obj) => String::from(to_string(obj).unwrap()),
                _ => panic!("Object() did not return an object. (Got: {ok:?})"),
            },
            Err(err) => unwind_type_error(err),
        }
    }

    mod assign {
        use super::*;
        use test_case::test_case;

        #[allow(clippy::unnecessary_wraps)]
        fn fake_keys(_this: &AdaptableObject) -> Completion<Vec<PropertyKey>> {
            Ok(vec![PropertyKey::from("once"), PropertyKey::from("twice")])
        }

        #[test]
        fn happy() {
            setup_test_agent();
            let object_proto = intrinsic(IntrinsicId::ObjectPrototype);
            let target = ordinary_object_create(Some(object_proto.clone()), &[]);
            let fruits = ordinary_object_create(Some(object_proto.clone()), &[]);
            fruits.create_data_property_or_throw("round", "apple").unwrap();
            fruits.create_data_property_or_throw("long", "banana").unwrap();
            fruits.create_data_property_or_throw("bunch", "grapes").unwrap();
            let limbs = ordinary_object_create(Some(object_proto), &[]);
            limbs.create_data_property_or_throw("spider", 8).unwrap();
            limbs.create_data_property_or_throw("bee", 6).unwrap();
            limbs.create_data_property_or_throw("dog", 4).unwrap();
            limbs.create_data_property_or_throw("worm", 0).unwrap();
            define_property_or_throw(
                &limbs,
                PropertyKey::from("not_visible"),
                PotentialPropertyDescriptor {
                    value: Some(ECMAScriptValue::Null),
                    enumerable: Some(false),
                    writable: Some(true),
                    configurable: Some(true),
                    ..Default::default()
                },
            )
            .unwrap();
            let keys_not_props = ECMAScriptValue::from(&AdaptableObject::object(&AdaptableMethods {
                own_property_keys_override: Some(fake_keys),
                ..Default::default()
            }));

            let result = object_assign(
                &ECMAScriptValue::Undefined,
                None,
                &[
                    ECMAScriptValue::from(target.clone()),
                    ECMAScriptValue::Undefined,
                    ECMAScriptValue::from(fruits),
                    ECMAScriptValue::Null,
                    ECMAScriptValue::from(limbs),
                    keys_not_props,
                ],
            )
            .unwrap();

            match result {
                ECMAScriptValue::Object(to) => {
                    assert_eq!(to, target);
                    assert_eq!(to.get(&PropertyKey::from("round")).unwrap(), ECMAScriptValue::from("apple"));
                    assert_eq!(to.get(&PropertyKey::from("long")).unwrap(), ECMAScriptValue::from("banana"));
                    assert_eq!(to.get(&PropertyKey::from("bunch")).unwrap(), ECMAScriptValue::from("grapes"));
                    assert_eq!(to.get(&PropertyKey::from("spider")).unwrap(), ECMAScriptValue::from(8));
                    assert_eq!(to.get(&PropertyKey::from("bee")).unwrap(), ECMAScriptValue::from(6));
                    assert_eq!(to.get(&PropertyKey::from("dog")).unwrap(), ECMAScriptValue::from(4));
                    assert_eq!(to.get(&PropertyKey::from("worm")).unwrap(), ECMAScriptValue::from(0));
                    assert!(!to.has_own_property(&PropertyKey::from("not_visible")).unwrap());
                    assert!(!to.has_own_property(&PropertyKey::from("once")).unwrap());
                    assert!(!to.has_own_property(&PropertyKey::from("twice")).unwrap());
                }
                _ => {
                    panic!("Got a non-object back: {result:?}");
                }
            }
        }

        fn ordinary_obj() -> ECMAScriptValue {
            let proto = intrinsic(IntrinsicId::ObjectPrototype);
            ECMAScriptValue::from(ordinary_object_create(Some(proto), &[]))
        }
        fn own_property_keys_throws() -> ECMAScriptValue {
            ECMAScriptValue::from(TestObject::object(&[FunctionId::OwnPropertyKeys]))
        }
        #[allow(clippy::unnecessary_wraps)]
        fn own_prop_keys(_: &AdaptableObject) -> Completion<Vec<PropertyKey>> {
            Ok(vec![PropertyKey::from("prop")])
        }
        fn get_own_prop_err(_: &AdaptableObject, _: &PropertyKey) -> Completion<Option<PropertyDescriptor>> {
            Err(create_type_error("Test Sentinel"))
        }
        fn get_own_property_throws() -> ECMAScriptValue {
            let obj = AdaptableObject::object(&AdaptableMethods {
                own_property_keys_override: Some(own_prop_keys),
                get_own_property_override: Some(get_own_prop_err),
                ..Default::default()
            });
            ECMAScriptValue::from(obj)
        }
        fn set_throws() -> ECMAScriptValue {
            ECMAScriptValue::from(TestObject::object(&[FunctionId::Set(None)]))
        }
        fn obj_with_item() -> ECMAScriptValue {
            let proto = intrinsic(IntrinsicId::ObjectPrototype);
            let obj = ordinary_object_create(Some(proto), &[]);
            obj.create_data_property_or_throw("something", 782).unwrap();
            ECMAScriptValue::from(obj)
        }
        #[allow(clippy::unnecessary_wraps)]
        fn get_own_prop_ok(_: &AdaptableObject, _: &PropertyKey) -> Completion<Option<PropertyDescriptor>> {
            Ok(Some(PropertyDescriptor {
                property: PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(22), writable: true }),
                enumerable: true,
                configurable: true,
                ..Default::default()
            }))
        }
        fn get_err(_: &AdaptableObject, _: &PropertyKey, _: &ECMAScriptValue) -> Completion<ECMAScriptValue> {
            Err(create_type_error("[[Get]] throws from AdaptableObject"))
        }
        fn get_throws() -> ECMAScriptValue {
            let obj = AdaptableObject::object(&AdaptableMethods {
                own_property_keys_override: Some(own_prop_keys),
                get_own_property_override: Some(get_own_prop_ok),
                get_override: Some(get_err),
                ..Default::default()
            });
            ECMAScriptValue::from(obj)
        }

        #[test_case(|| ECMAScriptValue::Undefined, || ECMAScriptValue::Undefined => "Undefined and null cannot be converted to objects"; "to undefined")]
        #[test_case(ordinary_obj, own_property_keys_throws => "[[OwnPropertyKeys]] called on TestObject"; "OwnPropertyKeys throws")]
        #[test_case(ordinary_obj, get_own_property_throws => "Test Sentinel"; "GetOwnProperty throws")]
        #[test_case(set_throws, obj_with_item => "[[Set]] called on TestObject"; "Set method throws")]
        #[test_case(ordinary_obj, get_throws => "[[Get]] throws from AdaptableObject"; "Get method throws")]
        fn error(create_to: fn() -> ECMAScriptValue, create_from: fn() -> ECMAScriptValue) -> String {
            setup_test_agent();
            let to = create_to();
            let from = create_from();

            let err = object_assign(&ECMAScriptValue::Undefined, None, &[to, from]).unwrap_err();
            unwind_type_error(err)
        }
    }

    mod define_properties_helper {
        use super::*;
        use test_case::test_case;

        fn normal_obj() -> Object {
            ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)), &[])
        }
        fn normal_params() -> ECMAScriptValue {
            let obj = ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)), &[]);
            let emotion_descriptor = ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)), &[]);
            emotion_descriptor.create_data_property_or_throw("value", "happy").unwrap();
            emotion_descriptor.create_data_property_or_throw("writable", true).unwrap();
            emotion_descriptor.create_data_property_or_throw("enumerable", true).unwrap();
            emotion_descriptor.create_data_property_or_throw("configurable", true).unwrap();
            obj.create_data_property_or_throw("emotion", emotion_descriptor).unwrap();
            let age_descriptor = ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)), &[]);
            age_descriptor.create_data_property_or_throw("value", 27).unwrap();
            age_descriptor.create_data_property_or_throw("writable", true).unwrap();
            age_descriptor.create_data_property_or_throw("enumerable", true).unwrap();
            age_descriptor.create_data_property_or_throw("configurable", true).unwrap();
            obj.create_data_property_or_throw("age", age_descriptor).unwrap();
            let favorite_descriptor = ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)), &[]);
            favorite_descriptor.create_data_property_or_throw("value", "banana").unwrap();
            favorite_descriptor.create_data_property_or_throw("writable", false).unwrap();
            favorite_descriptor.create_data_property_or_throw("enumerable", true).unwrap();
            favorite_descriptor.create_data_property_or_throw("configurable", true).unwrap();
            obj.create_data_property_or_throw("favorite_fruit", favorite_descriptor).unwrap();
            define_property_or_throw(
                &obj,
                PropertyKey::from("hidden"),
                PotentialPropertyDescriptor {
                    value: Some(ECMAScriptValue::from(true)),
                    writable: Some(true),
                    enumerable: Some(false),
                    configurable: Some(false),
                    ..Default::default()
                },
            )
            .unwrap();

            obj.into()
        }
        #[test_case(normal_obj, normal_params => Ok(vec![
            PropertyInfo { name: PropertyKey::from("emotion"), enumerable: true, configurable: true, kind: PropertyInfoKind::Data{ value: ECMAScriptValue::from("happy"), writable: true } },
            PropertyInfo { name: PropertyKey::from("age"), enumerable: true, configurable: true, kind: PropertyInfoKind::Data { value: ECMAScriptValue::from(27.0), writable: true } },
            PropertyInfo { name: PropertyKey::from("favorite_fruit"), enumerable: true, configurable: true, kind: PropertyInfoKind::Data { value: ECMAScriptValue::from("banana"), writable: false } }
        ]); "happy")]
        #[test_case(normal_obj, || ECMAScriptValue::Undefined => Err(String::from("Undefined and null cannot be converted to objects")); "undefined props")]
        #[test_case(normal_obj, || ECMAScriptValue::from(TestObject::object(&[FunctionId::OwnPropertyKeys])) => Err(String::from("[[OwnPropertyKeys]] called on TestObject")); "own_property_keys throws")]
        #[test_case(|| TestObject::object(&[FunctionId::DefineOwnProperty(None)]), normal_params => Err(String::from("[[DefineOwnProperty]] called on TestObject")); "define_property_or_throw throws")]
        #[test_case(normal_obj,
                    || ECMAScriptValue::from(&AdaptableObject::object(&AdaptableMethods { own_property_keys_override: Some(|_| Ok(vec![PropertyKey::from("something")])),
                    ..Default::default() })) =>
                    Ok(vec![]); "prop, but not")]
        #[test_case(normal_obj,
                    || ECMAScriptValue::from(&AdaptableObject::object(&AdaptableMethods {
                        own_property_keys_override: Some(|_| Ok(vec![PropertyKey::from("something")])),
                        get_own_property_override: Some(|_, _| Err(create_type_error("[[GetOwnProperty]] throws from AdaptableObject"))),
                        ..Default::default()
                    })) =>
                    Err(String::from("[[GetOwnProperty]] throws from AdaptableObject")); "get_own_property throws")]
        #[test_case(normal_obj, || {
                        let obj = TestObject::object(&[FunctionId::Get(None)]);
                        obj.create_data_property_or_throw("key", "blue").unwrap();
                        ECMAScriptValue::from(obj)
                    } => Err(String::from("[[Get]] called on TestObject")); "get throws")]
        #[test_case(normal_obj, || {
                        let obj = ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)), &[]);
                        obj.create_data_property_or_throw("key", "blue").unwrap();
                        ECMAScriptValue::from(obj)
                    } => Err(String::from("Must be an object")); "to_property_descriptor throws")]
        fn f(
            create_target: fn() -> Object,
            create_params: fn() -> ECMAScriptValue,
        ) -> Result<Vec<PropertyInfo>, String> {
            setup_test_agent();
            let target = create_target();
            let params = create_params();

            match object_define_properties_helper(target.clone(), params) {
                Ok(val) => match val {
                    ECMAScriptValue::Object(o) => {
                        assert_eq!(o, target);
                        Ok(o.o.common_object_data().borrow().propdump())
                    }
                    _ => panic!("Non-object came back from object_define_properties_helper(): {val:?}"),
                },
                Err(err) => Err(unwind_type_error(err)),
            }
        }
    }

    mod create {
        use super::*;
        use test_case::test_case;

        #[test_case(|| ECMAScriptValue::from(intrinsic(IntrinsicId::ObjectPrototype)), || ECMAScriptValue::Undefined => Ok(vec![]); "object proto; no props")]
        #[test_case(|| ECMAScriptValue::Null, || ECMAScriptValue::Undefined => Ok(vec![]); "null proto; no props")]
        #[test_case(|| ECMAScriptValue::from(22), || ECMAScriptValue::Undefined => Err(String::from("Prototype argument for Object.create must be an Object or null.")); "bad proto")]
        #[test_case(|| ECMAScriptValue::from(intrinsic(IntrinsicId::ObjectPrototype)),
                    || {
                        let obj = ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)), &[]);
                        let emotion_descriptor = ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)), &[]);
                        emotion_descriptor.create_data_property_or_throw("value", "happy").unwrap();
                        emotion_descriptor.create_data_property_or_throw("writable", true).unwrap();
                        emotion_descriptor.create_data_property_or_throw("enumerable", true).unwrap();
                        emotion_descriptor.create_data_property_or_throw("configurable", true).unwrap();
                        obj.create_data_property_or_throw("emotion", emotion_descriptor).unwrap();
                        ECMAScriptValue::from(obj)
                    } => Ok(vec![PropertyInfo { name: PropertyKey::from("emotion"), enumerable: true, configurable: true, kind: PropertyInfoKind::Data{ value: ECMAScriptValue::from("happy"), writable: true } },]); "with props")]
        fn f(
            make_proto: fn() -> ECMAScriptValue,
            make_props: fn() -> ECMAScriptValue,
        ) -> Result<Vec<PropertyInfo>, String> {
            setup_test_agent();
            let proto = make_proto();
            let props = make_props();
            match object_create(&ECMAScriptValue::Undefined, None, &[proto.clone(), props]) {
                Ok(val) => match val {
                    ECMAScriptValue::Object(o) => {
                        assert_eq!(
                            o.o.get_prototype_of(),
                            match &proto {
                                ECMAScriptValue::Null => Ok(None),
                                ECMAScriptValue::Object(o) => Ok(Some(o.clone())),
                                _ => panic!("Bad input to test function"),
                            }
                        );
                        Ok(o.o.common_object_data().borrow().propdump())
                    }
                    _ => panic!("Object.create returned a non-object: {val:?}"),
                },
                Err(err) => Err(unwind_type_error(err)),
            }
        }
    }

    mod define_properties {
        use super::*;
        use test_case::test_case;

        #[test_case(|| ECMAScriptValue::Undefined, || ECMAScriptValue::Undefined => Err("Object.defineProperties called on non-object".to_string()); "non-object")]
        #[test_case(|| ECMAScriptValue::from(ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)), &[])),
                    || {
                        let obj = ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)), &[]);
                        let emotion_descriptor = ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)), &[]);
                        emotion_descriptor.create_data_property_or_throw("value", "happy").unwrap();
                        emotion_descriptor.create_data_property_or_throw("writable", true).unwrap();
                        emotion_descriptor.create_data_property_or_throw("enumerable", true).unwrap();
                        emotion_descriptor.create_data_property_or_throw("configurable", true).unwrap();
                        obj.create_data_property_or_throw("emotion", emotion_descriptor).unwrap();
                        ECMAScriptValue::from(obj)
                    } => Ok(vec![PropertyInfo { name: PropertyKey::from("emotion"), enumerable: true, configurable: true, kind: PropertyInfoKind::Data{ value: ECMAScriptValue::from("happy"), writable: true } },]); "with props")]
        #[test_case(|| ECMAScriptValue::from(ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)), &[])),
                    || {
                        let obj = ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)), &[]);
                        obj.create_data_property_or_throw("key", "blue").unwrap();
                        ECMAScriptValue::from(obj)
                    } => Err("Must be an object".to_string()); "bad props")]
        fn f(
            make_obj: fn() -> ECMAScriptValue,
            make_props: fn() -> ECMAScriptValue,
        ) -> Result<Vec<PropertyInfo>, String> {
            setup_test_agent();
            let obj = make_obj();
            let props = make_props();
            match object_define_properties(&ECMAScriptValue::Undefined, None, &[obj.clone(), props]) {
                Ok(val) => match &val {
                    ECMAScriptValue::Object(o) => {
                        assert_eq!(val, obj);
                        Ok(o.o.common_object_data().borrow().propdump())
                    }
                    _ => panic!("Got a non-object back from Object.defineProperties: {val:?}"),
                },
                Err(err) => Err(unwind_type_error(err)),
            }
        }
    }

    mod define_property {
        use super::*;
        use test_case::test_case;

        fn plain_obj() -> ECMAScriptValue {
            ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)), &[]).into()
        }
        fn faux_errors(_: &ECMAScriptValue, _: Option<&Object>, _: &[ECMAScriptValue]) -> Completion<ECMAScriptValue> {
            Err(create_type_error("Test Sentinel"))
        }
        fn make_bad_property_key() -> ECMAScriptValue {
            let obj = ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)), &[]);
            let tostring_func = create_builtin_function(
                faux_errors,
                false,
                0.0,
                "toString".into(),
                BUILTIN_FUNCTION_SLOTS,
                current_realm_record(),
                Some(intrinsic(IntrinsicId::FunctionPrototype)),
                None,
            );
            define_property_or_throw(
                &obj,
                "toString",
                PotentialPropertyDescriptor {
                    value: Some(tostring_func.into()),
                    writable: Some(true),
                    enumerable: Some(true),
                    configurable: Some(true),
                    ..Default::default()
                },
            )
            .unwrap();
            obj.into()
        }
        fn undefined() -> ECMAScriptValue {
            ECMAScriptValue::Undefined
        }
        fn frozen_obj() -> ECMAScriptValue {
            let obj = ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)), &[]);
            obj.o.prevent_extensions().unwrap();
            obj.into()
        }
        fn attrs() -> ECMAScriptValue {
            let obj = ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)), &[]);
            obj.create_data_property_or_throw("value", 99).unwrap();
            obj.create_data_property_or_throw("writable", true).unwrap();
            obj.create_data_property_or_throw("enumerable", true).unwrap();
            obj.create_data_property_or_throw("configurable", true).unwrap();
            obj.into()
        }

        #[test_case(undefined, undefined, undefined => Err("Object.defineProperty called on non-object".to_string()); "undefined obj")]
        #[test_case(plain_obj, make_bad_property_key, undefined => Err("Test Sentinel".to_string()); "bad property key")]
        #[test_case(plain_obj, undefined, undefined => Err("Must be an object".to_string()); "bad descriptor")]
        #[test_case(frozen_obj, undefined, attrs => Err("Property cannot be assigned to".to_string()); "frozen starting object")]
        #[test_case(plain_obj, undefined, attrs => Ok(vec![PropertyInfo { name: PropertyKey::from("undefined"), enumerable: true, configurable: true, kind: PropertyInfoKind::Data{ value: ECMAScriptValue::from(99), writable: true } },]); "success")]
        fn f(
            make_obj: fn() -> ECMAScriptValue,
            make_key: fn() -> ECMAScriptValue,
            make_attrs: fn() -> ECMAScriptValue,
        ) -> Result<Vec<PropertyInfo>, String> {
            setup_test_agent();
            let obj = make_obj();
            let key = make_key();
            let attrs = make_attrs();

            let result = object_define_property(&ECMAScriptValue::Undefined, None, &[obj.clone(), key, attrs]);
            match result {
                Ok(val) => match &val {
                    ECMAScriptValue::Object(o) => {
                        assert_eq!(obj, val);
                        Ok(o.o.common_object_data().borrow().propdump())
                    }
                    _ => panic!("Got a non-object back from Object.defineProperty: {val:?}"),
                },
                Err(err) => Err(unwind_type_error(err)),
            }
        }
    }

    mod entries {
        use super::*;
        use test_case::test_case;

        const fn undef() -> ECMAScriptValue {
            ECMAScriptValue::Undefined
        }
        fn dead() -> ECMAScriptValue {
            DeadObject::object().into()
        }

        #[test_case(undef => Err("TypeError: Undefined and null cannot be converted to objects".to_string()); "undefined")]
        #[test_case(dead => Err("TypeError: own_property_keys called on DeadObject".to_string()); "own_property throws")]
        fn errs(make_arg: fn() -> ECMAScriptValue) -> Result<ECMAScriptValue, String> {
            setup_test_agent();
            let arg = make_arg();
            object_entries(&ECMAScriptValue::Undefined, None, &[arg]).map_err(unwind_any_error)
        }

        #[test]
        fn normal() {
            setup_test_agent();
            let proto = intrinsic(IntrinsicId::ObjectPrototype);
            let obj = ordinary_object_create(Some(proto), &[]);
            obj.create_data_property_or_throw("one", 1.0).unwrap();
            obj.create_data_property_or_throw("favorite", "spaghetti").unwrap();

            let result = object_entries(&ECMAScriptValue::Undefined, None, &[obj.into()]).unwrap();
            let entries: Object = result.try_into().unwrap();
            assert!(entries.is_array().unwrap());
            assert_eq!(entries.get(&"length".into()).unwrap(), 2.0.into());
            let first: Object = entries.get(&"0".into()).unwrap().try_into().unwrap();
            assert!(first.is_array().unwrap());
            assert_eq!(first.get(&"length".into()).unwrap(), 2.0.into());
            assert_eq!(first.get(&"0".into()).unwrap(), "one".into());
            assert_eq!(first.get(&"1".into()).unwrap(), 1.0.into());
            let second: Object = entries.get(&"1".into()).unwrap().try_into().unwrap();
            assert!(second.is_array().unwrap());
            assert_eq!(second.get(&"length".into()).unwrap(), 2.0.into());
            assert_eq!(second.get(&"0".into()).unwrap(), "favorite".into());
            assert_eq!(second.get(&"1".into()).unwrap(), "spaghetti".into());
        }
    }

    mod freeze {
        use super::*;

        #[test]
        fn no_args() {
            setup_test_agent();
            assert_eq!(object_freeze(&ECMAScriptValue::Undefined, None, &[]).unwrap(), ECMAScriptValue::Undefined);
        }
        #[test]
        fn number() {
            setup_test_agent();
            assert_eq!(
                object_freeze(&ECMAScriptValue::Undefined, None, &[2003.25.into()]).unwrap(),
                ECMAScriptValue::from(2003.25)
            );
        }
        #[test]
        fn dead() {
            setup_test_agent();
            let arg: ECMAScriptValue = DeadObject::object().into();
            let result = object_freeze(&ECMAScriptValue::Undefined, None, &[arg]).unwrap_err();
            assert_eq!(unwind_any_error(result), "TypeError: prevent_extensions called on DeadObject");
        }
        #[test]
        fn ok() {
            setup_test_agent();
            let obj = ordinary_object_create(None, &[]);
            obj.create_data_property_or_throw("property", "holiday").unwrap();
            let result: Object =
                object_freeze(&ECMAScriptValue::Undefined, None, &[obj.into()]).unwrap().try_into().unwrap();
            assert_eq!(
                result.o.common_object_data().borrow().propdump(),
                vec![PropertyInfo {
                    name: "property".into(),
                    kind: PropertyInfoKind::Data { value: "holiday".into(), writable: false },
                    enumerable: true,
                    configurable: false
                }]
            );
        }
        #[test]
        fn prevention_prevented() {
            setup_test_agent();
            let obj = AdaptableObject::object(&AdaptableMethods {
                prevent_extensions_override: Some(|_| Ok(false)),
                ..Default::default()
            });
            let result = object_freeze(&ECMAScriptValue::Undefined, None, &[obj.into()]).unwrap_err();
            assert_eq!(unwind_any_error(result), "TypeError: Object cannot be frozen");
        }
    }

    mod from_entries {
        use super::*;
        use test_case::test_case;

        #[test_case(Vec::new => serr("TypeError: Undefined and null are not allowed in this context"); "fails coercible check")]
        #[test_case(|| vec![ECMAScriptValue::from(10)] => serr("TypeError: object is not iterable"); "not a key/value iterator")]
        #[test_case(|| {
            let first = create_array_from_list(&[ECMAScriptValue::from("first"), ECMAScriptValue::from(true)]);
            let second = create_array_from_list(&[ECMAScriptValue::from("second"), ECMAScriptValue::from(998)]);
            let entries = create_array_from_list(&[ECMAScriptValue::from(first), ECMAScriptValue::from(second)]);
            vec![ECMAScriptValue::from(entries)]
        } => sok("first:true,second:998"); "typical")]
        #[test_case(|| {
            let key = ordinary_object_create(None, &[]); // no prototype means no toString
            let pair = create_array_from_list(&[ECMAScriptValue::from(key), ECMAScriptValue::from("item")]);
            let entries = create_array_from_list(&[ECMAScriptValue::from(pair)]);
            vec![ECMAScriptValue::from(entries)]
        } => serr("TypeError: Cannot convert object to primitive value"); "to_property_key throws")]
        fn call(make_args: impl FnOnce() -> Vec<ECMAScriptValue>) -> Result<String, String> {
            setup_test_agent();
            let args = make_args();

            let res =
                object_from_entries(&ECMAScriptValue::Undefined, None, args.as_slice()).map_err(unwind_any_error)?;

            Ok(res.test_result_string())
        }
    }

    #[test_case(|| create_array_from_list(&[ECMAScriptValue::from("bob"), ECMAScriptValue::from("charlie")]).into(), || ECMAScriptValue::from("0") => sok("value:bob,writable:true,enumerable:true,configurable:true"); "get 0 from array")]
    #[test_case(|| create_array_from_list(&[ECMAScriptValue::from("bob"), ECMAScriptValue::from("charlie")]).into(), || ECMAScriptValue::from("10") => sok("undefined"); "prop not found")]
    #[test_case(|| ECMAScriptValue::Undefined, || ECMAScriptValue::Undefined => serr("TypeError: Undefined and null cannot be converted to objects"); "to_object throws")]
    #[test_case(|| ECMAScriptValue::from(true), || ordinary_object_create(None, &[]).into() => serr("TypeError: Cannot convert object to primitive value"); "to_property_key throws")]
    #[test_case(|| DeadObject::object().into(), || ECMAScriptValue::from("prop") => serr("TypeError: get_own_property called on DeadObject"); "get_own_property throws")]
    fn get_own_property_descriptor(
        make_o: impl FnOnce() -> ECMAScriptValue,
        make_p: impl FnOnce() -> ECMAScriptValue,
    ) -> Result<String, String> {
        setup_test_agent();
        let o = make_o();
        let p = make_p();
        let result =
            object_get_own_property_descriptor(&ECMAScriptValue::Undefined, None, &[o, p]).map_err(unwind_any_error)?;
        Ok(result.test_result_string())
    }

    mod get_own_property_descriptors {
        use super::*;
        use test_case::test_case;

        #[allow(clippy::unnecessary_wraps)]
        fn lying_ownprops(_: &AdaptableObject) -> Completion<Vec<PropertyKey>> {
            Ok(vec!["one".into(), "two".into(), "three".into()])
        }
        fn lyingkeys() -> ECMAScriptValue {
            let obj = AdaptableObject::object(&AdaptableMethods {
                own_property_keys_override: Some(lying_ownprops),
                ..Default::default()
            });
            obj.into()
        }
        fn kaboom_gop(ao: &AdaptableObject, key: &PropertyKey) -> Completion<Option<PropertyDescriptor>> {
            if *key == PropertyKey::from("kaboom") {
                let state = ao.something.get();
                if state == 0 {
                    ao.something.set(1);
                } else {
                    return Err(create_type_error("[[GetOwnProperty]] called on kaboom"));
                }
            }
            Ok(ordinary_get_own_property(ao, key))
        }
        fn kaboom() -> ECMAScriptValue {
            let obj = AdaptableObject::object(&AdaptableMethods {
                get_own_property_override: Some(kaboom_gop),
                ..Default::default()
            });
            obj.create_data_property_or_throw("kaboom", 67).unwrap();
            obj.into()
        }

        #[test_case(|| ECMAScriptValue::Undefined => serr("TypeError: Undefined and null cannot be converted to objects"); "to_object throws")]
        #[test_case(|| DeadObject::object().into() => serr("TypeError: own_property_keys called on DeadObject"); "own_property_keys throws")]
        #[test_case(|| create_array_from_list(&[ECMAScriptValue::from(true)]).into() => sok("0:{value:true,writable:true,enumerable:true,configurable:true},length:{value:1,writable:true,enumerable:false,configurable:false}"); "typical")]
        #[test_case(lyingkeys => sok(""); "keys but not props")]
        #[test_case(kaboom => serr("TypeError: [[GetOwnProperty]] called on kaboom"); "get_own_property throws")]
        fn call(make_o: impl FnOnce() -> ECMAScriptValue) -> Result<String, String> {
            setup_test_agent();
            let o = make_o();
            let info = Object::try_from(
                object_get_own_property_descriptors(&ECMAScriptValue::Undefined, None, &[o])
                    .map_err(unwind_any_error)?,
            )
            .unwrap();

            let keys = ordinary_own_property_keys(&info);
            let mut r = String::new();
            let mut first = true;
            for key in keys {
                let value = info.get(&key).unwrap();
                if first {
                    first = false;
                } else {
                    r.push(',');
                }
                let value_str = value.test_result_string();
                r.push_str(&format!("{key}:{{{value_str}}}"));
            }
            Ok(r)
        }
    }

    #[test_case(|| ECMAScriptValue::Undefined => serr("TypeError: Undefined and null cannot be converted to objects"); "get_own_property_keys throws")]
    #[test_case(|| create_array_from_list(&[ECMAScriptValue::from(true)]).into() => sok("0:0,1:length,length:2"); "called on one-element array")]
    fn get_own_property_names(make_o: impl FnOnce() -> ECMAScriptValue) -> Result<String, String> {
        setup_test_agent();
        let o = make_o();
        let item = object_get_own_property_names(&ECMAScriptValue::Undefined, None, &[o]).map_err(unwind_any_error)?;
        Ok(item.test_result_string())
    }

    #[test_case(|| ECMAScriptValue::Undefined => serr("TypeError: Undefined and null cannot be converted to objects"); "get_own_property_keys throws")]
    #[test_case(|| {
        let obj = ordinary_object_create(None, &[]);
        obj.create_data_property_or_throw(wks(WksId::ToStringTag), "test_value").unwrap();
        obj.into()
    } => sok("0:Symbol(Symbol.toStringTag),length:1"); "one-symbol-prop")]
    fn get_own_property_symbols(make_o: impl FnOnce() -> ECMAScriptValue) -> Result<String, String> {
        setup_test_agent();
        let o = make_o();
        let item =
            object_get_own_property_symbols(&ECMAScriptValue::Undefined, None, &[o]).map_err(unwind_any_error)?;
        Ok(item.test_result_string())
    }

    mod get_prototype_of {
        use super::*;
        use test_case::test_case;

        fn object_with_std_proto() -> ECMAScriptValue {
            let key = global_symbol("gpo_test".into());
            let objproto = intrinsic(IntrinsicId::ObjectPrototype);
            objproto.create_data_property_or_throw(key, "%ObjectPrototype%").unwrap();
            ordinary_object_create(Some(objproto), &[]).into()
        }

        #[test_case(object_with_std_proto => vok("%ObjectPrototype%"); "normal object chain")]
        #[test_case(|| ECMAScriptValue::Undefined => serr("TypeError: Undefined and null cannot be converted to objects"); "to_object throws")]
        #[test_case(|| DeadObject::object().into() => serr("TypeError: get_prototype_of called on DeadObject"); "get_prototype_of throws")]
        #[test_case(|| ordinary_object_create(None, &[]).into() => vok(ECMAScriptValue::Undefined); "no prototype")]
        fn call(make_o: impl FnOnce() -> ECMAScriptValue) -> Result<ECMAScriptValue, String> {
            setup_test_agent();
            let o = make_o();
            let result = object_get_prototype_of(&ECMAScriptValue::Undefined, None, &[o]).map_err(unwind_any_error)?;
            match result {
                ECMAScriptValue::Object(obj) => {
                    let key = global_symbol("gpo_test".into());
                    let val = obj.get(&key.into()).unwrap();
                    if val.is_string() {
                        Ok(val)
                    } else {
                        Ok(obj.into())
                    }
                }
                _ => Ok(result),
            }
        }
    }

    #[test_case(||ECMAScriptValue::Undefined, ||ECMAScriptValue::Undefined => serr("TypeError: Undefined and null cannot be converted to objects"); "to_object throws")]
    #[test_case(||DeadObject::object().into(), ||DeadObject::object().into() => serr("TypeError: get called on DeadObject"); "to_property_key throws")]
    #[test_case(||create_array_from_list(&[ECMAScriptValue::from(10)]).into(), ||ECMAScriptValue::from("length") => vok(true); "typical")]
    fn has_own(
        make_o: impl FnOnce() -> ECMAScriptValue,
        make_p: impl FnOnce() -> ECMAScriptValue,
    ) -> Result<ECMAScriptValue, String> {
        setup_test_agent();
        let o = make_o();
        let p = make_p();

        object_has_own(&ECMAScriptValue::Undefined, None, &[o, p]).map_err(unwind_any_error)
    }

    // Don't need to get fancy here, same_value is already tested.
    #[test_case(ECMAScriptValue::from(10), ECMAScriptValue::from(10) => vok(true); "equal")]
    #[test_case(ECMAScriptValue::from("bob"), ECMAScriptValue::Null => vok(false); "not equal")]
    fn is(left: ECMAScriptValue, right: ECMAScriptValue) -> Result<ECMAScriptValue, String> {
        setup_test_agent();
        object_is(&ECMAScriptValue::Undefined, None, &[left, right]).map_err(unwind_any_error)
    }

    #[test_case(|| ECMAScriptValue::from("hello") => vok(false); "non-object")]
    #[test_case(|| ordinary_object_create(None, &[]).into() => vok(true); "object")]
    fn is_extensible(make_o: impl FnOnce() -> ECMAScriptValue) -> Result<ECMAScriptValue, String> {
        setup_test_agent();
        let o = make_o();
        object_is_extensible(&ECMAScriptValue::Undefined, None, &[o]).map_err(unwind_any_error)
    }

    #[test_case(|| ECMAScriptValue::from("hello") => vok(true); "non-object")]
    #[test_case(|| ordinary_object_create(None, &[]).into() => vok(false); "object")]
    fn is_frozen(make_o: impl FnOnce() -> ECMAScriptValue) -> Result<ECMAScriptValue, String> {
        setup_test_agent();
        let o = make_o();
        object_is_frozen(&ECMAScriptValue::Undefined, None, &[o]).map_err(unwind_any_error)
    }

    #[test_case(|| ECMAScriptValue::from("hello") => vok(true); "non-object")]
    #[test_case(|| ordinary_object_create(None, &[]).into() => vok(false); "object")]
    fn is_sealed(make_o: impl FnOnce() -> ECMAScriptValue) -> Result<ECMAScriptValue, String> {
        setup_test_agent();
        let o = make_o();
        object_is_sealed(&ECMAScriptValue::Undefined, None, &[o]).map_err(unwind_any_error)
    }

    #[test_case(|| ECMAScriptValue::Undefined => serr("TypeError: Undefined and null cannot be converted to objects"); "to_object throws")]
    #[test_case(|| DeadObject::object().into() => serr("TypeError: own_property_keys called on DeadObject"); "enumerable_own_properties throws")]
    #[test_case(|| create_array_from_list(&[ECMAScriptValue::from(10), ECMAScriptValue::from(3)]).into() => Ok(vec!["0".into(), "1".into()]); "array object")]
    fn keys(make_o: impl FnOnce() -> ECMAScriptValue) -> Result<Vec<ECMAScriptValue>, String> {
        setup_test_agent();
        let o = make_o();
        object_keys(&ECMAScriptValue::Undefined, None, &[o]).map_err(unwind_any_error).map(|v| match v {
            ECMAScriptValue::Undefined
            | ECMAScriptValue::Null
            | ECMAScriptValue::Boolean(_)
            | ECMAScriptValue::String(_)
            | ECMAScriptValue::Number(_)
            | ECMAScriptValue::BigInt(_)
            | ECMAScriptValue::Symbol(_) => vec![v],
            ECMAScriptValue::Object(o) => {
                if o.o.has_property(&"length".into()).unwrap() {
                    let length = to_usize(f64::try_from(o.get(&"length".into()).unwrap()).unwrap()).unwrap();
                    let mut result = vec![];
                    for idx in 0..length {
                        let val = o.get(&idx.into()).unwrap();
                        result.push(val);
                    }
                    result
                } else {
                    vec![o.into()]
                }
            }
        })
    }

    mod prevent_extensions {
        use super::*;
        use test_case::test_case;

        #[allow(clippy::unnecessary_wraps)]
        fn never_locks(_: &AdaptableObject) -> Completion<bool> {
            Ok(false)
        }

        fn unlockable() -> ECMAScriptValue {
            let obj = AdaptableObject::object(&AdaptableMethods {
                prevent_extensions_override: Some(never_locks),
                ..Default::default()
            });
            obj.into()
        }

        fn ordinary() -> ECMAScriptValue {
            let obj = ordinary_object_create(None, &[]);
            obj.create_data_property_or_throw("sentinel", "tomato").unwrap();
            obj.into()
        }

        #[test_case(|| ECMAScriptValue::from("potato") => vok("potato"); "not-object")]
        #[test_case(|| DeadObject::object().into() => serr("TypeError: prevent_extensions called on DeadObject"); "prevent_extensions throws")]
        #[test_case(ordinary => vok("tomato"); "typical")]
        #[test_case(unlockable => serr("TypeError: cannot prevent extensions for this object"); "always-open object")]
        fn call(make_o: impl FnOnce() -> ECMAScriptValue) -> Result<ECMAScriptValue, String> {
            setup_test_agent();
            let o = make_o();
            let res = object_prevent_extensions(&ECMAScriptValue::Undefined, None, &[o]).map_err(unwind_any_error)?;
            match Object::try_from(res.clone()) {
                Err(_) => Ok(res),
                Ok(o) => Ok(o.get(&"sentinel".into()).unwrap()),
            }
        }
    }

    mod seal {
        use super::*;
        use test_case::test_case;

        #[allow(clippy::unnecessary_wraps)]
        fn never_locks(_: &AdaptableObject) -> Completion<bool> {
            Ok(false)
        }

        fn unlockable() -> ECMAScriptValue {
            let obj = AdaptableObject::object(&AdaptableMethods {
                prevent_extensions_override: Some(never_locks),
                ..Default::default()
            });
            obj.into()
        }

        fn ordinary() -> ECMAScriptValue {
            let obj = ordinary_object_create(None, &[]);
            obj.create_data_property_or_throw("sentinel", "tomato").unwrap();
            obj.into()
        }

        #[test_case(|| ECMAScriptValue::from("potato") => vok("potato"); "not-object")]
        #[test_case(|| DeadObject::object().into() => serr("TypeError: prevent_extensions called on DeadObject"); "set_integrity_level throws")]
        #[test_case(ordinary => vok("tomato"); "typical")]
        #[test_case(unlockable => serr("TypeError: cannot seal this object"); "always-open object")]
        fn call(make_o: impl FnOnce() -> ECMAScriptValue) -> Result<ECMAScriptValue, String> {
            setup_test_agent();
            let o = make_o();
            let res = object_seal(&ECMAScriptValue::Undefined, None, &[o]).map_err(unwind_any_error)?;
            match Object::try_from(res.clone()) {
                Err(_) => Ok(res),
                Ok(o) => Ok(o.get(&"sentinel".into()).unwrap()),
            }
        }
    }

    mod set_prototype_of {
        use super::*;
        use test_case::test_case;

        fn sentinel_obj() -> ECMAScriptValue {
            let objproto = intrinsic(IntrinsicId::ObjectPrototype);
            let obj = ordinary_object_create(Some(objproto), &[]);
            obj.create_data_property_or_throw("sentinel", "turtle").unwrap();
            obj.into()
        }

        fn ordinary() -> ECMAScriptValue {
            ordinary_object_create(None, &[]).into()
        }

        fn sentinelized() -> ECMAScriptValue {
            let proto = Object::try_from(sentinel_obj()).unwrap();
            ordinary_object_create(Some(proto), &[]).into()
        }

        fn immutable() -> ECMAScriptValue {
            let objproto = intrinsic(IntrinsicId::ObjectPrototype);
            immutable_prototype_exotic_object_create(Some(&objproto)).into()
        }

        #[test_case(ordinary, sentinel_obj => vok("turtle"); "typical")]
        #[test_case(|| ECMAScriptValue::Undefined, sentinel_obj => serr("TypeError: Undefined and null are not allowed in this context"); "not coercible")]
        #[test_case(ordinary, || ECMAScriptValue::from(true) => serr("TypeError: Prototype must be an object or null"); "bad proto")]
        #[test_case(|| ECMAScriptValue::from("dog"), sentinel_obj => vok("dog"); "not object")]
        #[test_case(sentinelized, || ECMAScriptValue::Null => vok(ECMAScriptValue::Undefined); "proto erasure")]
        #[test_case(|| DeadObject::object().into(), || ECMAScriptValue::Null => serr("TypeError: set_prototype_of called on DeadObject"); "set_prototype_of throws")]
        #[test_case(immutable, || ECMAScriptValue::Null => serr("TypeError: Prototype setting failed"); "immutable proto")]
        fn call(
            make_o: impl FnOnce() -> ECMAScriptValue,
            make_p: impl FnOnce() -> ECMAScriptValue,
        ) -> Result<ECMAScriptValue, String> {
            setup_test_agent();
            let o = make_o();
            let proto = make_p();
            let res =
                object_set_prototype_of(&ECMAScriptValue::Undefined, None, &[o, proto]).map_err(unwind_any_error)?;
            match Object::try_from(res.clone()) {
                Err(_) => Ok(res),
                Ok(o) => Ok(o.get(&"sentinel".into()).unwrap()),
            }
        }
    }

    #[test_case(|| ECMAScriptValue::Undefined => serr("TypeError: Undefined and null cannot be converted to objects"); "to_object throws")]
    #[test_case(|| DeadObject::object().into() => serr("TypeError: own_property_keys called on DeadObject"); "enumerable_own_properties throws")]
    #[test_case(|| create_array_from_list(&[ECMAScriptValue::from(10), ECMAScriptValue::from(3)]).into() => Ok(vec![10.into(), 3.into()]); "array object")]
    fn values(make_o: impl FnOnce() -> ECMAScriptValue) -> Result<Vec<ECMAScriptValue>, String> {
        setup_test_agent();
        let o = make_o();
        object_values(&ECMAScriptValue::Undefined, None, &[o]).map_err(unwind_any_error).map(|v| match v {
            ECMAScriptValue::Undefined
            | ECMAScriptValue::Null
            | ECMAScriptValue::Boolean(_)
            | ECMAScriptValue::String(_)
            | ECMAScriptValue::Number(_)
            | ECMAScriptValue::BigInt(_)
            | ECMAScriptValue::Symbol(_) => vec![v],
            ECMAScriptValue::Object(o) => {
                if o.o.has_property(&"length".into()).unwrap() {
                    let length = to_usize(f64::try_from(o.get(&"length".into()).unwrap()).unwrap()).unwrap();
                    let mut result = vec![];
                    for idx in 0..length {
                        let val = o.get(&idx.into()).unwrap();
                        result.push(val);
                    }
                    result
                } else {
                    vec![o.into()]
                }
            }
        })
    }
}

mod get_own_property_keys {
    use super::*;
    use itertools::Itertools;
    use test_case::test_case;

    fn test_obj() -> ECMAScriptValue {
        let obj_proto = intrinsic(IntrinsicId::ObjectPrototype);
        let obj = ordinary_object_create(Some(obj_proto), &[]);
        obj.create_data_property_or_throw("string_key", "blue").unwrap();
        obj.create_data_property_or_throw("other_key", 892).unwrap();
        obj.create_data_property_or_throw(wks(WksId::ToStringTag), "awkward").unwrap();
        obj.into()
    }

    #[test_case(|| ECMAScriptValue::Undefined, KeyType::String => serr("TypeError: Undefined and null cannot be converted to objects"); "uncoerceable")]
    #[test_case(|| create_array_from_list(&[ECMAScriptValue::from("bob")]).into(), KeyType::String => sok("0, length"); "one-elem array; strings")]
    #[test_case(test_obj, KeyType::String => sok("string_key, other_key"); "test obj; strings")]
    #[test_case(test_obj, KeyType::Symbol => sok("Symbol(Symbol.toStringTag)"); "test obj; symbols")]
    #[test_case(|| DeadObject::object().into(), KeyType::String => serr("TypeError: own_property_keys called on DeadObject"); "own_property_keys throws")]
    fn call(make_val: impl FnOnce() -> ECMAScriptValue, typ: KeyType) -> Result<String, String> {
        setup_test_agent();
        let value = make_val();

        let result = get_own_property_keys(value, typ).map_err(unwind_any_error)?;
        Ok(result.into_iter().map(|v| format!("{v}")).join(", "))
    }
}
