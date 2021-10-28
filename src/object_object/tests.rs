use super::*;
use crate::cr::AltCompletion;
use crate::errors::{create_type_error, create_type_error_object};
use crate::object::{create_data_property_or_throw, has_own_property, ordinary_object_create, set, DataProperty, DeadObject, PropertyDescriptor, PropertyKind};
use crate::realm::IntrinsicId;
use crate::tests::{test_agent, unwind_type_error, AdaptableMethods, AdaptableObject, FunctionId, TestObject};
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
                ECMAScriptValue::Object(obj) => String::from(to_string(&mut agent, obj.into()).unwrap()),
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
            let target = ordinary_object_create(&mut agent, Some(&object_proto), &[]);
            let fruits = ordinary_object_create(&mut agent, Some(&object_proto), &[]);
            create_data_property_or_throw(&mut agent, &fruits, "round", "apple").unwrap();
            create_data_property_or_throw(&mut agent, &fruits, "long", "banana").unwrap();
            create_data_property_or_throw(&mut agent, &fruits, "bunch", "grapes").unwrap();
            let limbs = ordinary_object_create(&mut agent, Some(&object_proto), &[]);
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
            ECMAScriptValue::from(ordinary_object_create(agent, Some(&proto), &[]))
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
            let obj = ordinary_object_create(agent, Some(&proto), &[]);
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
}
