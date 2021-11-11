use super::*;
use crate::cr::AltCompletion;
use crate::errors::{create_type_error, create_type_error_object};
use crate::object::{
    create_data_property_or_throw, has_own_property, ordinary_object_create, set, DataProperty, DeadObject, PropertyDescriptor, PropertyInfo, PropertyInfoKind, PropertyKind,
};
use crate::realm::IntrinsicId;
use crate::tests::{test_agent, unwind_any_error, unwind_type_error, AdaptableMethods, AdaptableObject, FunctionId, TestObject};
use crate::values::{to_number, to_string};

mod prototype {
    use super::*;

    mod valueof {
        use super::*;

        #[test]
        fn happy() {
            let mut agent = test_agent();
            let value = ECMAScriptValue::from(10);

            let result = object_prototype_value_of(&mut agent, value, None, &[]).unwrap();
            match &result {
                ECMAScriptValue::Object(obj) => {
                    assert!(obj.o.is_number_object());
                    assert_eq!(to_number(&mut agent, result).unwrap(), 10.0);
                }
                _ => {
                    panic!("Object.prototype.valueOf did not return an object. (Got: {:?})", result);
                }
            }
        }
        #[test]
        fn err() {
            let mut agent = test_agent();
            let result = object_prototype_value_of(&mut agent, ECMAScriptValue::Undefined, None, &[]).unwrap_err();
            assert_eq!(unwind_type_error(&mut agent, result), "Undefined and null cannot be converted to objects");
        }
    }

    mod to_string {
        use super::*;
        use test_case::test_case;

        fn greasy(agent: &mut Agent) -> ECMAScriptValue {
            // Return an object whose @@toStringTag property has the value "Grease"
            let to_string_tag_symbol = agent.wks(WksId::ToStringTag);
            let obj = ordinary_object_create(agent, None, &[]);
            set(agent, &obj, PropertyKey::from(to_string_tag_symbol), ECMAScriptValue::from("Grease"), false).unwrap();
            ECMAScriptValue::from(obj)
        }

        #[test_case(|_| ECMAScriptValue::Undefined => "[object Undefined]"; "undefined")]
        #[test_case(|_| ECMAScriptValue::Null => "[object Null]"; "null")]
        #[test_case(|_| ECMAScriptValue::from(99) => "[object Number]"; "number")]
        #[test_case(|_| ECMAScriptValue::from(true) => "[object Boolean]"; "boolean")]
        #[test_case(|agent| ECMAScriptValue::from(create_type_error_object(agent, "test_error")) => "[object Error]"; "error object")]
        #[test_case(|agent| ECMAScriptValue::from(agent.intrinsic(IntrinsicId::Boolean)) => "[object Function]"; "callable object")]
        #[test_case(|agent| ECMAScriptValue::from(ordinary_object_create(agent, None, &[])) => "[object Object]"; "ordinary object")]
        #[test_case(greasy => "[object Grease]"; "to-string-tag")]
        #[test_case(|agent| ECMAScriptValue::from(DeadObject::object(agent)) => "get called on DeadObject"; "throw getting tag")]
        fn f(make: fn(agent: &mut Agent) -> ECMAScriptValue) -> String {
            let mut agent = test_agent();
            let value = make(&mut agent);
            match object_prototype_to_string(&mut agent, value, None, &[]) {
                Ok(ok) => match ok {
                    ECMAScriptValue::String(s) => String::from(s),
                    _ => panic!("Object.prototype.toString did not return a string. (Got: {:?})", ok),
                },
                Err(err) => unwind_type_error(&mut agent, err),
            }
        }
    }
}

mod constructor {
    use super::*;
    use test_case::test_case;

    #[test_case(|a| Some(ordinary_object_create(a, None, &[])), &[ECMAScriptValue::from(10)] => "10"; "new target but no active function")]
    #[test_case(|a| {
        let obj = ordinary_object_create(a, None, &[]);
        a.running_execution_context_mut().unwrap().function = Some(obj.clone());
        Some(obj)
    }, &[ECMAScriptValue::from(11)] => "11"; "related new target")]
    #[test_case(|a| {
        let obj = ordinary_object_create(a, None, &[]);
        a.running_execution_context_mut().unwrap().function = Some(obj);
        Some(ordinary_object_create(a, None, &[]))
    }, &[ECMAScriptValue::from(12)] => "[object Object]"; "unrelated new target")]
    #[test_case(|_| None, &[ECMAScriptValue::Null] => "[object Object]"; "null value")]
    #[test_case(|_| None, &[ECMAScriptValue::Undefined] => "[object Object]"; "undefined value")]
    #[test_case(|a| {
        let obj = ordinary_object_create(a, None, &[]);
        a.running_execution_context_mut().unwrap().function = Some(obj);
        let nt = TestObject::object(a, &[FunctionId::Get(None)]);
        Some(nt)
    }, &[] => "[[Get]] called on TestObject"; "ordinary_create_from_constructor throws")]
    fn function(new_target: fn(&mut Agent) -> Option<Object>, args: &[ECMAScriptValue]) -> String {
        let mut agent = test_agent();
        let nt = new_target(&mut agent);

        match object_constructor_function(&mut agent, ECMAScriptValue::Undefined, nt.as_ref(), args) {
            Ok(ok) => match ok {
                ECMAScriptValue::Object(obj) => String::from(to_string(&mut agent, obj).unwrap()),
                _ => panic!("Object() did not return an object. (Got: {:?})", ok),
            },
            Err(err) => unwind_type_error(&mut agent, err),
        }
    }

    mod assign {
        use super::*;
        use test_case::test_case;

        fn fake_keys(_agent: &mut Agent, _this: &AdaptableObject) -> AltCompletion<Vec<PropertyKey>> {
            Ok(vec![PropertyKey::from("once"), PropertyKey::from("twice")])
        }

        #[test]
        fn happy() {
            let mut agent = test_agent();
            let object_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
            let target = ordinary_object_create(&mut agent, Some(object_proto.clone()), &[]);
            let fruits = ordinary_object_create(&mut agent, Some(object_proto.clone()), &[]);
            create_data_property_or_throw(&mut agent, &fruits, "round", "apple").unwrap();
            create_data_property_or_throw(&mut agent, &fruits, "long", "banana").unwrap();
            create_data_property_or_throw(&mut agent, &fruits, "bunch", "grapes").unwrap();
            let limbs = ordinary_object_create(&mut agent, Some(object_proto), &[]);
            create_data_property_or_throw(&mut agent, &limbs, "spider", 8).unwrap();
            create_data_property_or_throw(&mut agent, &limbs, "bee", 6).unwrap();
            create_data_property_or_throw(&mut agent, &limbs, "dog", 4).unwrap();
            create_data_property_or_throw(&mut agent, &limbs, "worm", 0).unwrap();
            define_property_or_throw(
                &mut agent,
                &limbs,
                PropertyKey::from("not_visible"),
                PotentialPropertyDescriptor { value: Some(ECMAScriptValue::Null), enumerable: Some(false), writable: Some(true), configurable: Some(true), ..Default::default() },
            )
            .unwrap();
            let keys_not_props = ECMAScriptValue::from(AdaptableObject::object(&mut agent, AdaptableMethods { own_property_keys_override: Some(fake_keys), ..Default::default() }));

            let result = object_assign(
                &mut agent,
                ECMAScriptValue::Undefined,
                None,
                &[ECMAScriptValue::from(target.clone()), ECMAScriptValue::Undefined, ECMAScriptValue::from(fruits), ECMAScriptValue::Null, ECMAScriptValue::from(limbs), keys_not_props],
            )
            .unwrap();

            match result {
                ECMAScriptValue::Object(to) => {
                    assert_eq!(to, target);
                    assert_eq!(get(&mut agent, &to, &PropertyKey::from("round")).unwrap(), ECMAScriptValue::from("apple"));
                    assert_eq!(get(&mut agent, &to, &PropertyKey::from("long")).unwrap(), ECMAScriptValue::from("banana"));
                    assert_eq!(get(&mut agent, &to, &PropertyKey::from("bunch")).unwrap(), ECMAScriptValue::from("grapes"));
                    assert_eq!(get(&mut agent, &to, &PropertyKey::from("spider")).unwrap(), ECMAScriptValue::from(8));
                    assert_eq!(get(&mut agent, &to, &PropertyKey::from("bee")).unwrap(), ECMAScriptValue::from(6));
                    assert_eq!(get(&mut agent, &to, &PropertyKey::from("dog")).unwrap(), ECMAScriptValue::from(4));
                    assert_eq!(get(&mut agent, &to, &PropertyKey::from("worm")).unwrap(), ECMAScriptValue::from(0));
                    assert!(!has_own_property(&mut agent, &to, &PropertyKey::from("not_visible")).unwrap());
                    assert!(!has_own_property(&mut agent, &to, &PropertyKey::from("once")).unwrap());
                    assert!(!has_own_property(&mut agent, &to, &PropertyKey::from("twice")).unwrap());
                }
                _ => {
                    panic!("Got a non-object back: {:?}", result);
                }
            }
        }

        fn ordinary_obj(agent: &mut Agent) -> ECMAScriptValue {
            let proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
            ECMAScriptValue::from(ordinary_object_create(agent, Some(proto), &[]))
        }
        fn own_property_keys_throws(agent: &mut Agent) -> ECMAScriptValue {
            ECMAScriptValue::from(TestObject::object(agent, &[FunctionId::OwnPropertyKeys]))
        }
        fn own_prop_keys(_: &mut Agent, _: &AdaptableObject) -> AltCompletion<Vec<PropertyKey>> {
            Ok(vec![PropertyKey::from("prop")])
        }
        fn get_own_prop_err(agent: &mut Agent, _: &AdaptableObject, _: &PropertyKey) -> AltCompletion<Option<PropertyDescriptor>> {
            Err(create_type_error(agent, "Test Sentinel"))
        }
        fn get_own_property_throws(agent: &mut Agent) -> ECMAScriptValue {
            let obj =
                AdaptableObject::object(agent, AdaptableMethods { own_property_keys_override: Some(own_prop_keys), get_own_property_override: Some(get_own_prop_err), ..Default::default() });
            ECMAScriptValue::from(obj)
        }
        fn set_throws(agent: &mut Agent) -> ECMAScriptValue {
            ECMAScriptValue::from(TestObject::object(agent, &[FunctionId::Set(None)]))
        }
        fn obj_with_item(agent: &mut Agent) -> ECMAScriptValue {
            let proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
            let obj = ordinary_object_create(agent, Some(proto), &[]);
            create_data_property_or_throw(agent, &obj, "something", 782).unwrap();
            ECMAScriptValue::from(obj)
        }
        fn get_own_prop_ok(_: &mut Agent, _: &AdaptableObject, _: &PropertyKey) -> AltCompletion<Option<PropertyDescriptor>> {
            Ok(Some(PropertyDescriptor {
                property: PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(22), writable: true }),
                enumerable: true,
                configurable: true,
                ..Default::default()
            }))
        }
        fn get_err(agent: &mut Agent, _: &AdaptableObject, _: &PropertyKey, _: &ECMAScriptValue) -> Completion {
            Err(create_type_error(agent, "[[Get]] throws from AdaptableObject"))
        }
        fn get_throws(agent: &mut Agent) -> ECMAScriptValue {
            let obj = AdaptableObject::object(
                agent,
                AdaptableMethods { own_property_keys_override: Some(own_prop_keys), get_own_property_override: Some(get_own_prop_ok), get_override: Some(get_err), ..Default::default() },
            );
            ECMAScriptValue::from(obj)
        }

        #[test_case(|_| ECMAScriptValue::Undefined, |_| ECMAScriptValue::Undefined => "Undefined and null cannot be converted to objects"; "to undefined")]
        #[test_case(ordinary_obj, own_property_keys_throws => "[[OwnPropertyKeys]] called on TestObject"; "OwnPropertyKeys throws")]
        #[test_case(ordinary_obj, get_own_property_throws => "Test Sentinel"; "GetOwnProperty throws")]
        #[test_case(set_throws, obj_with_item => "[[Set]] called on TestObject"; "Set method throws")]
        #[test_case(ordinary_obj, get_throws => "[[Get]] throws from AdaptableObject"; "Get method throws")]
        fn error(create_to: fn(&mut Agent) -> ECMAScriptValue, create_from: fn(&mut Agent) -> ECMAScriptValue) -> String {
            let mut agent = test_agent();
            let to = create_to(&mut agent);
            let from = create_from(&mut agent);

            let err = object_assign(&mut agent, ECMAScriptValue::Undefined, None, &[to, from]).unwrap_err();
            unwind_type_error(&mut agent, err)
        }
    }

    mod define_properties_helper {
        use super::*;
        use test_case::test_case;

        fn normal_obj(agent: &mut Agent) -> Object {
            ordinary_object_create(agent, Some(agent.intrinsic(IntrinsicId::ObjectPrototype)), &[])
        }
        fn normal_params(agent: &mut Agent) -> ECMAScriptValue {
            let obj = ordinary_object_create(agent, Some(agent.intrinsic(IntrinsicId::ObjectPrototype)), &[]);
            let emotion_descriptor = ordinary_object_create(agent, Some(agent.intrinsic(IntrinsicId::ObjectPrototype)), &[]);
            create_data_property_or_throw(agent, &emotion_descriptor, "value", "happy").unwrap();
            create_data_property_or_throw(agent, &emotion_descriptor, "writable", true).unwrap();
            create_data_property_or_throw(agent, &emotion_descriptor, "enumerable", true).unwrap();
            create_data_property_or_throw(agent, &emotion_descriptor, "configurable", true).unwrap();
            create_data_property_or_throw(agent, &obj, "emotion", emotion_descriptor).unwrap();
            let age_descriptor = ordinary_object_create(agent, Some(agent.intrinsic(IntrinsicId::ObjectPrototype)), &[]);
            create_data_property_or_throw(agent, &age_descriptor, "value", 27).unwrap();
            create_data_property_or_throw(agent, &age_descriptor, "writable", true).unwrap();
            create_data_property_or_throw(agent, &age_descriptor, "enumerable", true).unwrap();
            create_data_property_or_throw(agent, &age_descriptor, "configurable", true).unwrap();
            create_data_property_or_throw(agent, &obj, "age", age_descriptor).unwrap();
            let favorite_descriptor = ordinary_object_create(agent, Some(agent.intrinsic(IntrinsicId::ObjectPrototype)), &[]);
            create_data_property_or_throw(agent, &favorite_descriptor, "value", "banana").unwrap();
            create_data_property_or_throw(agent, &favorite_descriptor, "writable", false).unwrap();
            create_data_property_or_throw(agent, &favorite_descriptor, "enumerable", true).unwrap();
            create_data_property_or_throw(agent, &favorite_descriptor, "configurable", true).unwrap();
            create_data_property_or_throw(agent, &obj, "favorite_fruit", favorite_descriptor).unwrap();
            define_property_or_throw(
                agent,
                &obj,
                PropertyKey::from("hidden"),
                PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(true)), writable: Some(true), enumerable: Some(false), configurable: Some(false), ..Default::default() },
            )
            .unwrap();

            obj.into()
        }
        #[test_case(normal_obj, normal_params => Ok(vec![
            PropertyInfo { name: PropertyKey::from("emotion"), enumerable: true, configurable: true, kind: PropertyInfoKind::Data{ value: ECMAScriptValue::from("happy"), writable: true } },
            PropertyInfo { name: PropertyKey::from("age"), enumerable: true, configurable: true, kind: PropertyInfoKind::Data { value: ECMAScriptValue::from(27.0), writable: true } },
            PropertyInfo { name: PropertyKey::from("favorite_fruit"), enumerable: true, configurable: true, kind: PropertyInfoKind::Data { value: ECMAScriptValue::from("banana"), writable: false } }
        ]); "happy")]
        #[test_case(normal_obj, |_| ECMAScriptValue::Undefined => Err(String::from("Undefined and null cannot be converted to objects")); "undefined props")]
        #[test_case(normal_obj, |a| ECMAScriptValue::from(TestObject::object(a, &[FunctionId::OwnPropertyKeys])) => Err(String::from("[[OwnPropertyKeys]] called on TestObject")); "own_property_keys throws")]
        #[test_case(|a| TestObject::object(a, &[FunctionId::DefineOwnProperty(None)]), normal_params => Err(String::from("[[DefineOwnProperty]] called on TestObject")); "define_property_or_throw throws")]
        #[test_case(normal_obj,
                    |a| ECMAScriptValue::from(AdaptableObject::object(a, AdaptableMethods { own_property_keys_override: Some(|_, _| Ok(vec![PropertyKey::from("something")])),
                    ..Default::default() })) =>
                    Ok(vec![]); "prop, but not")]
        #[test_case(normal_obj,
                    |a| ECMAScriptValue::from(AdaptableObject::object(a, AdaptableMethods {
                        own_property_keys_override: Some(|_, _| Ok(vec![PropertyKey::from("something")])),
                        get_own_property_override: Some(|a,_,_| Err(create_type_error(a, "[[GetOwnProperty]] throws from AdaptableObject"))),
                        ..Default::default()
                    })) =>
                    Err(String::from("[[GetOwnProperty]] throws from AdaptableObject")); "get_own_property throws")]
        #[test_case(normal_obj, |a| {
                        let obj = TestObject::object(a, &[FunctionId::Get(None)]);
                        create_data_property_or_throw(a, &obj, "key", "blue").unwrap();
                        ECMAScriptValue::from(obj)
                    } => Err(String::from("[[Get]] called on TestObject")); "get throws")]
        #[test_case(normal_obj, |a| {
                        let obj = ordinary_object_create(a, Some(a.intrinsic(IntrinsicId::ObjectPrototype)), &[]);
                        create_data_property_or_throw(a, &obj, "key", "blue").unwrap();
                        ECMAScriptValue::from(obj)
                    } => Err(String::from("Must be an object")); "to_property_descriptor throws")]
        fn f(create_target: fn(&mut Agent) -> Object, create_params: fn(&mut Agent) -> ECMAScriptValue) -> Result<Vec<PropertyInfo>, String> {
            let mut agent = test_agent();
            let target = create_target(&mut agent);
            let params = create_params(&mut agent);

            match object_define_properties_helper(&mut agent, target.clone(), params) {
                Ok(val) => match val {
                    ECMAScriptValue::Object(o) => {
                        assert_eq!(o, target);
                        Ok(o.o.common_object_data().borrow().propdump())
                    }
                    _ => panic!("Non-object came back from object_define_properties_helper(): {:?}", val),
                },
                Err(err) => Err(unwind_type_error(&mut agent, err)),
            }
        }
    }

    mod create {
        use super::*;
        use test_case::test_case;

        #[test_case(|a| ECMAScriptValue::from(a.intrinsic(IntrinsicId::ObjectPrototype)), |_| ECMAScriptValue::Undefined => Ok(vec![]); "object proto; no props")]
        #[test_case(|_| ECMAScriptValue::Null, |_| ECMAScriptValue::Undefined => Ok(vec![]); "null proto; no props")]
        #[test_case(|_| ECMAScriptValue::from(22), |_| ECMAScriptValue::Undefined => Err(String::from("Prototype argument for Object.create must be an Object or null.")); "bad proto")]
        #[test_case(|a| ECMAScriptValue::from(a.intrinsic(IntrinsicId::ObjectPrototype)),
                    |a| {
                        let obj = ordinary_object_create(a, Some(a.intrinsic(IntrinsicId::ObjectPrototype)), &[]);
                        let emotion_descriptor = ordinary_object_create(a, Some(a.intrinsic(IntrinsicId::ObjectPrototype)), &[]);
                        create_data_property_or_throw(a, &emotion_descriptor, "value", "happy").unwrap();
                        create_data_property_or_throw(a, &emotion_descriptor, "writable", true).unwrap();
                        create_data_property_or_throw(a, &emotion_descriptor, "enumerable", true).unwrap();
                        create_data_property_or_throw(a, &emotion_descriptor, "configurable", true).unwrap();
                        create_data_property_or_throw(a, &obj, "emotion", emotion_descriptor).unwrap();
                        ECMAScriptValue::from(obj)
                    } => Ok(vec![PropertyInfo { name: PropertyKey::from("emotion"), enumerable: true, configurable: true, kind: PropertyInfoKind::Data{ value: ECMAScriptValue::from("happy"), writable: true } },]); "with props")]
        fn f(make_proto: fn(&mut Agent) -> ECMAScriptValue, make_props: fn(&mut Agent) -> ECMAScriptValue) -> Result<Vec<PropertyInfo>, String> {
            let mut agent = test_agent();
            let proto = make_proto(&mut agent);
            let props = make_props(&mut agent);
            match object_create(&mut agent, ECMAScriptValue::Undefined, None, &[proto.clone(), props]) {
                Ok(val) => match val {
                    ECMAScriptValue::Object(o) => {
                        assert_eq!(
                            o.o.get_prototype_of(&mut agent),
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
                Err(err) => Err(unwind_type_error(&mut agent, err)),
            }
        }
    }

    mod define_properties {
        use super::*;
        use test_case::test_case;

        #[test_case(|_| ECMAScriptValue::Undefined, |_| ECMAScriptValue::Undefined => Err("Object.defineProperties called on non-object".to_string()); "non-object")]
        #[test_case(|a| ECMAScriptValue::from(ordinary_object_create(a, Some(a.intrinsic(IntrinsicId::ObjectPrototype)), &[])),
                    |a| {
                        let obj = ordinary_object_create(a, Some(a.intrinsic(IntrinsicId::ObjectPrototype)), &[]);
                        let emotion_descriptor = ordinary_object_create(a, Some(a.intrinsic(IntrinsicId::ObjectPrototype)), &[]);
                        create_data_property_or_throw(a, &emotion_descriptor, "value", "happy").unwrap();
                        create_data_property_or_throw(a, &emotion_descriptor, "writable", true).unwrap();
                        create_data_property_or_throw(a, &emotion_descriptor, "enumerable", true).unwrap();
                        create_data_property_or_throw(a, &emotion_descriptor, "configurable", true).unwrap();
                        create_data_property_or_throw(a, &obj, "emotion", emotion_descriptor).unwrap();
                        ECMAScriptValue::from(obj)
                    } => Ok(vec![PropertyInfo { name: PropertyKey::from("emotion"), enumerable: true, configurable: true, kind: PropertyInfoKind::Data{ value: ECMAScriptValue::from("happy"), writable: true } },]); "with props")]
        #[test_case(|a| ECMAScriptValue::from(ordinary_object_create(a, Some(a.intrinsic(IntrinsicId::ObjectPrototype)), &[])),
                    |a| {
                        let obj = ordinary_object_create(a, Some(a.intrinsic(IntrinsicId::ObjectPrototype)), &[]);
                        create_data_property_or_throw(a, &obj, "key", "blue").unwrap();
                        ECMAScriptValue::from(obj)
                    } => Err("Must be an object".to_string()); "bad props")]
        fn f(make_obj: fn(&mut Agent) -> ECMAScriptValue, make_props: fn(&mut Agent) -> ECMAScriptValue) -> Result<Vec<PropertyInfo>, String> {
            let mut agent = test_agent();
            let obj = make_obj(&mut agent);
            let props = make_props(&mut agent);
            match object_define_properties(&mut agent, ECMAScriptValue::Undefined, None, &[obj.clone(), props]) {
                Ok(val) => match &val {
                    ECMAScriptValue::Object(o) => {
                        assert_eq!(val, obj);
                        Ok(o.o.common_object_data().borrow().propdump())
                    }
                    _ => panic!("Got a non-object back from Object.defineProperties: {:?}", val),
                },
                Err(err) => Err(unwind_type_error(&mut agent, err)),
            }
        }
    }

    mod define_property {
        use super::*;
        use test_case::test_case;

        fn plain_obj(agent: &mut Agent) -> ECMAScriptValue {
            ordinary_object_create(agent, Some(agent.intrinsic(IntrinsicId::ObjectPrototype)), &[]).into()
        }
        fn faux_errors(agent: &mut Agent, _: ECMAScriptValue, _: Option<&Object>, _: &[ECMAScriptValue]) -> Completion {
            Err(create_type_error(agent, "Test Sentinel"))
        }
        fn make_bad_property_key(agent: &mut Agent) -> ECMAScriptValue {
            let obj = ordinary_object_create(agent, Some(agent.intrinsic(IntrinsicId::ObjectPrototype)), &[]);
            let tostring_func = create_builtin_function(
                agent,
                faux_errors,
                false,
                0.0,
                "toString".into(),
                &BUILTIN_FUNCTION_SLOTS,
                Some(agent.running_execution_context().unwrap().realm.clone()),
                Some(agent.intrinsic(IntrinsicId::FunctionPrototype)),
                None,
            );
            define_property_or_throw(
                agent,
                &obj,
                "toString".into(),
                PotentialPropertyDescriptor { value: Some(tostring_func.into()), writable: Some(true), enumerable: Some(true), configurable: Some(true), ..Default::default() },
            )
            .unwrap();
            obj.into()
        }
        fn undefined(_: &mut Agent) -> ECMAScriptValue {
            ECMAScriptValue::Undefined
        }
        fn frozen_obj(agent: &mut Agent) -> ECMAScriptValue {
            let obj = ordinary_object_create(agent, Some(agent.intrinsic(IntrinsicId::ObjectPrototype)), &[]);
            obj.o.prevent_extensions(agent).unwrap();
            obj.into()
        }
        fn attrs(agent: &mut Agent) -> ECMAScriptValue {
            let obj = ordinary_object_create(agent, Some(agent.intrinsic(IntrinsicId::ObjectPrototype)), &[]);
            create_data_property_or_throw(agent, &obj, "value", 99).unwrap();
            create_data_property_or_throw(agent, &obj, "writable", true).unwrap();
            create_data_property_or_throw(agent, &obj, "enumerable", true).unwrap();
            create_data_property_or_throw(agent, &obj, "configurable", true).unwrap();
            obj.into()
        }

        #[test_case(undefined, undefined, undefined => Err("Object.defineProperty called on non-object".to_string()); "undefined obj")]
        #[test_case(plain_obj, make_bad_property_key, undefined => Err("Test Sentinel".to_string()); "bad property key")]
        #[test_case(plain_obj, undefined, undefined => Err("Must be an object".to_string()); "bad descriptor")]
        #[test_case(frozen_obj, undefined, attrs => Err("Property cannot be assigned to".to_string()); "frozen starting object")]
        #[test_case(plain_obj, undefined, attrs => Ok(vec![PropertyInfo { name: PropertyKey::from("undefined"), enumerable: true, configurable: true, kind: PropertyInfoKind::Data{ value: ECMAScriptValue::from(99), writable: true } },]); "success")]
        fn f(make_obj: fn(&mut Agent) -> ECMAScriptValue, make_key: fn(&mut Agent) -> ECMAScriptValue, make_attrs: fn(&mut Agent) -> ECMAScriptValue) -> Result<Vec<PropertyInfo>, String> {
            let mut agent = test_agent();
            let obj = make_obj(&mut agent);
            let key = make_key(&mut agent);
            let attrs = make_attrs(&mut agent);

            let result = object_define_property(&mut agent, ECMAScriptValue::Undefined, None, &[obj.clone(), key, attrs]);
            match result {
                Ok(val) => match &val {
                    ECMAScriptValue::Object(o) => {
                        assert_eq!(obj, val);
                        Ok(o.o.common_object_data().borrow().propdump())
                    }
                    _ => panic!("Got a non-object back from Object.defineProperty: {:?}", val),
                },
                Err(err) => Err(unwind_type_error(&mut agent, err)),
            }
        }
    }

    mod entries {
        use super::*;
        use test_case::test_case;

        fn undef(_: &mut Agent) -> ECMAScriptValue {
            ECMAScriptValue::Undefined
        }
        fn dead(agent: &mut Agent) -> ECMAScriptValue {
            DeadObject::object(agent).into()
        }

        #[test_case(undef => Err("TypeError: Undefined and null cannot be converted to objects".to_string()); "undefined")]
        #[test_case(dead => Err("TypeError: own_property_keys called on DeadObject".to_string()); "own_property throws")]
        fn errs(make_arg: fn(&mut Agent) -> ECMAScriptValue) -> Result<ECMAScriptValue, String> {
            let mut agent = test_agent();
            let arg = make_arg(&mut agent);
            object_entries(&mut agent, ECMAScriptValue::Undefined, None, &[arg]).map_err(|err| unwind_any_error(&mut agent, err))
        }

        #[test]
        fn normal() {
            let mut agent = test_agent();
            let proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
            let obj = ordinary_object_create(&mut agent, Some(proto), &[]);
            create_data_property_or_throw(&mut agent, &obj, "one", 1.0).unwrap();
            create_data_property_or_throw(&mut agent, &obj, "favorite", "spaghetti").unwrap();

            let result = object_entries(&mut agent, ECMAScriptValue::Undefined, None, &[obj.into()]).unwrap();
            let entries: Object = result.try_into().unwrap();
            assert!(entries.is_array(&mut agent).unwrap());
            assert_eq!(get(&mut agent, &entries, &"length".into()).unwrap(), 2.0.into());
            let first: Object = get(&mut agent, &entries, &"0".into()).unwrap().try_into().unwrap();
            assert!(first.is_array(&mut agent).unwrap());
            assert_eq!(get(&mut agent, &first, &"length".into()).unwrap(), 2.0.into());
            assert_eq!(get(&mut agent, &first, &"0".into()).unwrap(), "one".into());
            assert_eq!(get(&mut agent, &first, &"1".into()).unwrap(), 1.0.into());
            let second: Object = get(&mut agent, &entries, &"1".into()).unwrap().try_into().unwrap();
            assert!(second.is_array(&mut agent).unwrap());
            assert_eq!(get(&mut agent, &second, &"length".into()).unwrap(), 2.0.into());
            assert_eq!(get(&mut agent, &second, &"0".into()).unwrap(), "favorite".into());
            assert_eq!(get(&mut agent, &second, &"1".into()).unwrap(), "spaghetti".into());
        }
    }
}
