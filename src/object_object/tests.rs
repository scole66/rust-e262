use super::*;
use crate::tests::*;

mod prototype {
    use super::*;

    mod valueof {
        use super::*;

        #[test]
        fn happy() {
            setup_test_agent();
            let value = ECMAScriptValue::from(10);

            let result = object_prototype_value_of(value, None, &[]).unwrap();
            match &result {
                ECMAScriptValue::Object(obj) => {
                    assert!(obj.o.is_number_object());
                    assert_eq!(to_number(result).unwrap(), 10.0);
                }
                _ => {
                    panic!("Object.prototype.valueOf did not return an object. (Got: {:?})", result);
                }
            }
        }
        #[test]
        fn err() {
            setup_test_agent();
            let result = object_prototype_value_of(ECMAScriptValue::Undefined, None, &[]).unwrap_err();
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
            set(&obj, PropertyKey::from(to_string_tag_symbol), ECMAScriptValue::from("Grease"), false).unwrap();
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
            match object_prototype_to_string(value, None, &[]) {
                Ok(ok) => match ok {
                    ECMAScriptValue::String(s) => String::from(s),
                    _ => panic!("Object.prototype.toString did not return a string. (Got: {:?})", ok),
                },
                Err(err) => unwind_type_error(err),
            }
        }
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

        match object_constructor_function(ECMAScriptValue::Undefined, nt.as_ref(), args) {
            Ok(ok) => match ok {
                ECMAScriptValue::Object(obj) => String::from(to_string(obj).unwrap()),
                _ => panic!("Object() did not return an object. (Got: {:?})", ok),
            },
            Err(err) => unwind_type_error(err),
        }
    }

    mod assign {
        use super::*;
        use test_case::test_case;

        fn fake_keys(_this: &AdaptableObject) -> Completion<Vec<PropertyKey>> {
            Ok(vec![PropertyKey::from("once"), PropertyKey::from("twice")])
        }

        #[test]
        fn happy() {
            setup_test_agent();
            let object_proto = intrinsic(IntrinsicId::ObjectPrototype);
            let target = ordinary_object_create(Some(object_proto.clone()), &[]);
            let fruits = ordinary_object_create(Some(object_proto.clone()), &[]);
            create_data_property_or_throw(&fruits, "round", "apple").unwrap();
            create_data_property_or_throw(&fruits, "long", "banana").unwrap();
            create_data_property_or_throw(&fruits, "bunch", "grapes").unwrap();
            let limbs = ordinary_object_create(Some(object_proto), &[]);
            create_data_property_or_throw(&limbs, "spider", 8).unwrap();
            create_data_property_or_throw(&limbs, "bee", 6).unwrap();
            create_data_property_or_throw(&limbs, "dog", 4).unwrap();
            create_data_property_or_throw(&limbs, "worm", 0).unwrap();
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
            let keys_not_props = ECMAScriptValue::from(AdaptableObject::object(AdaptableMethods {
                own_property_keys_override: Some(fake_keys),
                ..Default::default()
            }));

            let result = object_assign(
                ECMAScriptValue::Undefined,
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
                    assert_eq!(get(&to, &PropertyKey::from("round")).unwrap(), ECMAScriptValue::from("apple"));
                    assert_eq!(get(&to, &PropertyKey::from("long")).unwrap(), ECMAScriptValue::from("banana"));
                    assert_eq!(get(&to, &PropertyKey::from("bunch")).unwrap(), ECMAScriptValue::from("grapes"));
                    assert_eq!(get(&to, &PropertyKey::from("spider")).unwrap(), ECMAScriptValue::from(8));
                    assert_eq!(get(&to, &PropertyKey::from("bee")).unwrap(), ECMAScriptValue::from(6));
                    assert_eq!(get(&to, &PropertyKey::from("dog")).unwrap(), ECMAScriptValue::from(4));
                    assert_eq!(get(&to, &PropertyKey::from("worm")).unwrap(), ECMAScriptValue::from(0));
                    assert!(!has_own_property(&to, &PropertyKey::from("not_visible")).unwrap());
                    assert!(!has_own_property(&to, &PropertyKey::from("once")).unwrap());
                    assert!(!has_own_property(&to, &PropertyKey::from("twice")).unwrap());
                }
                _ => {
                    panic!("Got a non-object back: {:?}", result);
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
        fn own_prop_keys(_: &AdaptableObject) -> Completion<Vec<PropertyKey>> {
            Ok(vec![PropertyKey::from("prop")])
        }
        fn get_own_prop_err(_: &AdaptableObject, _: &PropertyKey) -> Completion<Option<PropertyDescriptor>> {
            Err(create_type_error("Test Sentinel"))
        }
        fn get_own_property_throws() -> ECMAScriptValue {
            let obj = AdaptableObject::object(AdaptableMethods {
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
            create_data_property_or_throw(&obj, "something", 782).unwrap();
            ECMAScriptValue::from(obj)
        }
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
            let obj = AdaptableObject::object(AdaptableMethods {
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

            let err = object_assign(ECMAScriptValue::Undefined, None, &[to, from]).unwrap_err();
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
            create_data_property_or_throw(&emotion_descriptor, "value", "happy").unwrap();
            create_data_property_or_throw(&emotion_descriptor, "writable", true).unwrap();
            create_data_property_or_throw(&emotion_descriptor, "enumerable", true).unwrap();
            create_data_property_or_throw(&emotion_descriptor, "configurable", true).unwrap();
            create_data_property_or_throw(&obj, "emotion", emotion_descriptor).unwrap();
            let age_descriptor = ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)), &[]);
            create_data_property_or_throw(&age_descriptor, "value", 27).unwrap();
            create_data_property_or_throw(&age_descriptor, "writable", true).unwrap();
            create_data_property_or_throw(&age_descriptor, "enumerable", true).unwrap();
            create_data_property_or_throw(&age_descriptor, "configurable", true).unwrap();
            create_data_property_or_throw(&obj, "age", age_descriptor).unwrap();
            let favorite_descriptor = ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)), &[]);
            create_data_property_or_throw(&favorite_descriptor, "value", "banana").unwrap();
            create_data_property_or_throw(&favorite_descriptor, "writable", false).unwrap();
            create_data_property_or_throw(&favorite_descriptor, "enumerable", true).unwrap();
            create_data_property_or_throw(&favorite_descriptor, "configurable", true).unwrap();
            create_data_property_or_throw(&obj, "favorite_fruit", favorite_descriptor).unwrap();
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
                    || ECMAScriptValue::from(AdaptableObject::object(AdaptableMethods { own_property_keys_override: Some(|_| Ok(vec![PropertyKey::from("something")])),
                    ..Default::default() })) =>
                    Ok(vec![]); "prop, but not")]
        #[test_case(normal_obj,
                    || ECMAScriptValue::from(AdaptableObject::object(AdaptableMethods {
                        own_property_keys_override: Some(|_| Ok(vec![PropertyKey::from("something")])),
                        get_own_property_override: Some(|_, _| Err(create_type_error("[[GetOwnProperty]] throws from AdaptableObject"))),
                        ..Default::default()
                    })) =>
                    Err(String::from("[[GetOwnProperty]] throws from AdaptableObject")); "get_own_property throws")]
        #[test_case(normal_obj, || {
                        let obj = TestObject::object(&[FunctionId::Get(None)]);
                        create_data_property_or_throw(&obj, "key", "blue").unwrap();
                        ECMAScriptValue::from(obj)
                    } => Err(String::from("[[Get]] called on TestObject")); "get throws")]
        #[test_case(normal_obj, || {
                        let obj = ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)), &[]);
                        create_data_property_or_throw(&obj, "key", "blue").unwrap();
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
                    _ => panic!("Non-object came back from object_define_properties_helper(): {:?}", val),
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
                        create_data_property_or_throw(&emotion_descriptor, "value", "happy").unwrap();
                        create_data_property_or_throw(&emotion_descriptor, "writable", true).unwrap();
                        create_data_property_or_throw(&emotion_descriptor, "enumerable", true).unwrap();
                        create_data_property_or_throw(&emotion_descriptor, "configurable", true).unwrap();
                        create_data_property_or_throw(&obj, "emotion", emotion_descriptor).unwrap();
                        ECMAScriptValue::from(obj)
                    } => Ok(vec![PropertyInfo { name: PropertyKey::from("emotion"), enumerable: true, configurable: true, kind: PropertyInfoKind::Data{ value: ECMAScriptValue::from("happy"), writable: true } },]); "with props")]
        fn f(
            make_proto: fn() -> ECMAScriptValue,
            make_props: fn() -> ECMAScriptValue,
        ) -> Result<Vec<PropertyInfo>, String> {
            setup_test_agent();
            let proto = make_proto();
            let props = make_props();
            match object_create(ECMAScriptValue::Undefined, None, &[proto.clone(), props]) {
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
                    _ => panic!("Object.create returned a non-object: {:?}", val),
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
                        create_data_property_or_throw(&emotion_descriptor, "value", "happy").unwrap();
                        create_data_property_or_throw(&emotion_descriptor, "writable", true).unwrap();
                        create_data_property_or_throw(&emotion_descriptor, "enumerable", true).unwrap();
                        create_data_property_or_throw(&emotion_descriptor, "configurable", true).unwrap();
                        create_data_property_or_throw(&obj, "emotion", emotion_descriptor).unwrap();
                        ECMAScriptValue::from(obj)
                    } => Ok(vec![PropertyInfo { name: PropertyKey::from("emotion"), enumerable: true, configurable: true, kind: PropertyInfoKind::Data{ value: ECMAScriptValue::from("happy"), writable: true } },]); "with props")]
        #[test_case(|| ECMAScriptValue::from(ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)), &[])),
                    || {
                        let obj = ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)), &[]);
                        create_data_property_or_throw(&obj, "key", "blue").unwrap();
                        ECMAScriptValue::from(obj)
                    } => Err("Must be an object".to_string()); "bad props")]
        fn f(
            make_obj: fn() -> ECMAScriptValue,
            make_props: fn() -> ECMAScriptValue,
        ) -> Result<Vec<PropertyInfo>, String> {
            setup_test_agent();
            let obj = make_obj();
            let props = make_props();
            match object_define_properties(ECMAScriptValue::Undefined, None, &[obj.clone(), props]) {
                Ok(val) => match &val {
                    ECMAScriptValue::Object(o) => {
                        assert_eq!(val, obj);
                        Ok(o.o.common_object_data().borrow().propdump())
                    }
                    _ => panic!("Got a non-object back from Object.defineProperties: {:?}", val),
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
        fn faux_errors(_: ECMAScriptValue, _: Option<&Object>, _: &[ECMAScriptValue]) -> Completion<ECMAScriptValue> {
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
            create_data_property_or_throw(&obj, "value", 99).unwrap();
            create_data_property_or_throw(&obj, "writable", true).unwrap();
            create_data_property_or_throw(&obj, "enumerable", true).unwrap();
            create_data_property_or_throw(&obj, "configurable", true).unwrap();
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

            let result = object_define_property(ECMAScriptValue::Undefined, None, &[obj.clone(), key, attrs]);
            match result {
                Ok(val) => match &val {
                    ECMAScriptValue::Object(o) => {
                        assert_eq!(obj, val);
                        Ok(o.o.common_object_data().borrow().propdump())
                    }
                    _ => panic!("Got a non-object back from Object.defineProperty: {:?}", val),
                },
                Err(err) => Err(unwind_type_error(err)),
            }
        }
    }

    mod entries {
        use super::*;
        use test_case::test_case;

        fn undef() -> ECMAScriptValue {
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
            object_entries(ECMAScriptValue::Undefined, None, &[arg]).map_err(unwind_any_error)
        }

        #[test]
        fn normal() {
            setup_test_agent();
            let proto = intrinsic(IntrinsicId::ObjectPrototype);
            let obj = ordinary_object_create(Some(proto), &[]);
            create_data_property_or_throw(&obj, "one", 1.0).unwrap();
            create_data_property_or_throw(&obj, "favorite", "spaghetti").unwrap();

            let result = object_entries(ECMAScriptValue::Undefined, None, &[obj.into()]).unwrap();
            let entries: Object = result.try_into().unwrap();
            assert!(entries.is_array().unwrap());
            assert_eq!(get(&entries, &"length".into()).unwrap(), 2.0.into());
            let first: Object = get(&entries, &"0".into()).unwrap().try_into().unwrap();
            assert!(first.is_array().unwrap());
            assert_eq!(get(&first, &"length".into()).unwrap(), 2.0.into());
            assert_eq!(get(&first, &"0".into()).unwrap(), "one".into());
            assert_eq!(get(&first, &"1".into()).unwrap(), 1.0.into());
            let second: Object = get(&entries, &"1".into()).unwrap().try_into().unwrap();
            assert!(second.is_array().unwrap());
            assert_eq!(get(&second, &"length".into()).unwrap(), 2.0.into());
            assert_eq!(get(&second, &"0".into()).unwrap(), "favorite".into());
            assert_eq!(get(&second, &"1".into()).unwrap(), "spaghetti".into());
        }
    }

    mod freeze {
        use super::*;

        #[test]
        fn no_args() {
            setup_test_agent();
            assert_eq!(object_freeze(ECMAScriptValue::Undefined, None, &[]).unwrap(), ECMAScriptValue::Undefined);
        }
        #[test]
        fn number() {
            setup_test_agent();
            assert_eq!(
                object_freeze(ECMAScriptValue::Undefined, None, &[2003.25.into()]).unwrap(),
                ECMAScriptValue::from(2003.25)
            );
        }
        #[test]
        fn dead() {
            setup_test_agent();
            let arg: ECMAScriptValue = DeadObject::object().into();
            let result = object_freeze(ECMAScriptValue::Undefined, None, &[arg]).unwrap_err();
            assert_eq!(unwind_any_error(result), "TypeError: prevent_extensions called on DeadObject");
        }
        #[test]
        fn ok() {
            setup_test_agent();
            let obj = ordinary_object_create(None, &[]);
            create_data_property_or_throw(&obj, "property", "holiday").unwrap();
            let result: Object =
                object_freeze(ECMAScriptValue::Undefined, None, &[obj.into()]).unwrap().try_into().unwrap();
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
            let obj = AdaptableObject::object(AdaptableMethods {
                prevent_extensions_override: Some(|_| Ok(false)),
                ..Default::default()
            });
            let result = object_freeze(ECMAScriptValue::Undefined, None, &[obj.into()]).unwrap_err();
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
                object_from_entries(ECMAScriptValue::Undefined, None, args.as_slice()).map_err(unwind_any_error)?;

            let repr = res.test_result_string();
            // let repr = match res {
            //     ECMAScriptValue::Undefined
            //     | ECMAScriptValue::Null
            //     | ECMAScriptValue::Boolean(_)
            //     | ECMAScriptValue::String(_)
            //     | ECMAScriptValue::Number(_)
            //     | ECMAScriptValue::BigInt(_)
            //     | ECMAScriptValue::Symbol(_) => format!("{res:?}"),
            //     ECMAScriptValue::Object(o) => {
            //         let keys = ordinary_own_property_keys(&o);
            //         let mut r = String::new();
            //         let mut first = true;
            //         for key in keys {
            //             let value = get(&o, &key).map_err(unwind_any_error)?;
            //             if !first {
            //                 r.push(',');
            //             } else {
            //                 first = false;
            //             }
            //             r.push_str(&format!("{key}:{value}"));
            //         }
            //         r
            //     }
            // };
            Ok(repr)
        }
    }

    mod get_own_property_descriptor {
        use super::*;
        use test_case::test_case;

        #[test_case(|| create_array_from_list(&[ECMAScriptValue::from("bob"), ECMAScriptValue::from("charlie")]).into(), || ECMAScriptValue::from("0") => sok("value:bob,writable:true,enumerable:true,configurable:true"); "get 0 from array")]
        #[test_case(|| create_array_from_list(&[ECMAScriptValue::from("bob"), ECMAScriptValue::from("charlie")]).into(), || ECMAScriptValue::from("10") => sok("undefined"); "prop not found")]
        #[test_case(|| ECMAScriptValue::Undefined, || ECMAScriptValue::Undefined => serr("TypeError: Undefined and null cannot be converted to objects"); "to_object throws")]
        #[test_case(|| ECMAScriptValue::from(true), || ordinary_object_create(None, &[]).into() => serr("TypeError: Cannot convert object to primitive value"); "to_property_key throws")]
        #[test_case(|| DeadObject::object().into(), || ECMAScriptValue::from("prop") => serr("TypeError: get_own_property called on DeadObject"); "get_own_property throws")]
        fn call(
            make_o: impl FnOnce() -> ECMAScriptValue,
            make_p: impl FnOnce() -> ECMAScriptValue,
        ) -> Result<String, String> {
            setup_test_agent();
            let o = make_o();
            let p = make_p();
            let result = object_get_own_property_descriptor(ECMAScriptValue::Undefined, None, &[o, p])
                .map_err(unwind_any_error)?;
            Ok(result.test_result_string())
        }
    }
}

mod get_own_property_keys {
    use super::*;
    use test_case::test_case;

    fn test_obj() -> ECMAScriptValue {
        let obj_proto = intrinsic(IntrinsicId::ObjectPrototype);
        let obj = ordinary_object_create(Some(obj_proto), &[]);
        create_data_property_or_throw(&obj, "string_key", "blue").unwrap();
        create_data_property_or_throw(&obj, "other_key", 892).unwrap();
        create_data_property_or_throw(&obj, wks(WksId::ToStringTag), "awkward").unwrap();
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
