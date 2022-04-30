use super::*;
use crate::object::{ordinary_object_create, set, Object, PropertyInfo, PropertyInfoKind};
use crate::tests::{faux_errors, test_agent, unwind_any_error};
use test_case::test_case;

mod array_object {
    use super::*;

    mod create {
        use super::*;
        use test_case::test_case;

        #[test_case(0 => Ok(vec![
            PropertyInfo { name: PropertyKey::from("length"), enumerable: false, configurable: false, kind: PropertyInfoKind::Data { value: ECMAScriptValue::from(0.0), writable: true}}
        ]); "zero length")]
        #[test_case(100 => Ok(vec![
            PropertyInfo { name: PropertyKey::from("length"), enumerable: false, configurable: false, kind: PropertyInfoKind::Data { value: ECMAScriptValue::from(100.0), writable: true}}
        ]); "hundred length")]
        #[test_case(7294967295 => Err("RangeError: Array lengths greater than 4294967295 are not allowed".to_string()); "over limit")]
        fn normal(length: u64) -> Result<Vec<PropertyInfo>, String> {
            let mut agent = test_agent();

            let result = ArrayObject::create(&mut agent, length, None);
            match result {
                Err(err) => Err(unwind_any_error(&mut agent, err)),
                Ok(obj) => {
                    assert!(obj.is_array(&mut agent).unwrap());
                    assert_eq!(obj.o.get_prototype_of(&mut agent).unwrap(), Some(agent.intrinsic(IntrinsicId::ArrayPrototype)));
                    Ok(obj.o.common_object_data().borrow().propdump())
                }
            }
        }

        #[test]
        fn proto_specified() {
            let mut agent = test_agent();
            let object_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);

            let obj = ArrayObject::create(&mut agent, 600, Some(object_proto.clone())).unwrap();
            assert!(obj.is_array(&mut agent).unwrap());
            assert_eq!(obj.o.get_prototype_of(&mut agent).unwrap(), Some(object_proto));
        }
    }

    #[test]
    fn debug() {
        let mut agent = test_agent();
        let a = ArrayObject::create(&mut agent, 0, None).unwrap();
        assert_ne!(format!("{:?}", a), "");
    }

    #[test]
    fn is_ordinary() {
        let mut agent = test_agent();
        let a = ArrayObject::create(&mut agent, 0, None).unwrap();
        assert_eq!(a.o.is_ordinary(), true);
    }

    #[test]
    fn get_prototype_of() {
        let mut agent = test_agent();
        let a = ArrayObject::create(&mut agent, 0, None).unwrap();
        let a_proto = a.o.get_prototype_of(&mut agent).unwrap().unwrap();
        assert_eq!(a_proto, agent.intrinsic(IntrinsicId::ArrayPrototype));
    }

    #[test]
    fn set_prototype_of() {
        let mut agent = test_agent();
        let a = ArrayObject::create(&mut agent, 0, None).unwrap();
        let obj_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let success = a.o.set_prototype_of(&mut agent, Some(obj_proto.clone())).unwrap();
        assert!(success);
        assert_eq!(obj_proto, a.o.get_prototype_of(&mut agent).unwrap().unwrap());
    }

    #[test]
    fn is_extensible() {
        let mut agent = test_agent();
        let a = ArrayObject::create(&mut agent, 0, None).unwrap();
        assert!(a.o.is_extensible(&mut agent).unwrap());
    }
    #[test]
    fn prevent_extensions() {
        let mut agent = test_agent();
        let a = ArrayObject::create(&mut agent, 0, None).unwrap();
        assert!(a.o.prevent_extensions(&mut agent).unwrap());
        assert!(!a.o.is_extensible(&mut agent).unwrap());
    }
    #[test]
    fn get_own_property() {
        let mut agent = test_agent();
        let a = ArrayObject::create(&mut agent, 0, None).unwrap();
        let desc: DataDescriptor = a.o.get_own_property(&mut agent, &"length".into()).unwrap().unwrap().try_into().unwrap();

        assert_eq!(desc.configurable, false);
        assert_eq!(desc.enumerable, false);
        assert_eq!(desc.value, 0.0.into());
        assert_eq!(desc.writable, true);
    }

    mod define_own_property {
        use super::*;
        use test_case::test_case;

        #[test_case("length", 10, true, false, false => Ok((true, vec![
            PropertyInfo { name: PropertyKey::from("length"), enumerable: false, configurable: false, kind: PropertyInfoKind::Data { value: ECMAScriptValue::from(10.0), writable: true}}
        ])); "length")]
        #[test_case("name", "bob", true, true, true => Ok((true, vec![
            PropertyInfo { name: PropertyKey::from("length"), enumerable: false, configurable: false, kind: PropertyInfoKind::Data { value: ECMAScriptValue::from(0.0), writable: true}},
            PropertyInfo { name: PropertyKey::from("name"), enumerable: true, configurable: true, kind: PropertyInfoKind::Data { value: ECMAScriptValue::from("bob"), writable: true}}
        ])); "non-numeric")]
        #[test_case("1003", "upthere", true, true, true => Ok((true, vec![
            PropertyInfo { name: PropertyKey::from("length"), enumerable: false, configurable: false, kind: PropertyInfoKind::Data { value: ECMAScriptValue::from(1004.0), writable: true}},
            PropertyInfo { name: PropertyKey::from("1003"), enumerable: true, configurable: true, kind: PropertyInfoKind::Data { value: ECMAScriptValue::from("upthere"), writable: true}}
        ])); "big number")]
        #[test_case("0", "zero", true, true, true => Ok((true, vec![
            PropertyInfo { name: PropertyKey::from("length"), enumerable: false, configurable: false, kind: PropertyInfoKind::Data { value: ECMAScriptValue::from(1.0), writable: true}},
            PropertyInfo { name: PropertyKey::from("0"), enumerable: true, configurable: true, kind: PropertyInfoKind::Data { value: ECMAScriptValue::from("zero"), writable: true}}
        ])); "zero")]
        fn normal(key: &str, val: impl Into<ECMAScriptValue>, writable: bool, enumerable: bool, configurable: bool) -> Result<(bool, Vec<PropertyInfo>), String> {
            let mut agent = test_agent();
            let a = ArrayObject::create(&mut agent, 0, None).unwrap();
            let result = a.o.define_own_property(
                &mut agent,
                key.into(),
                PotentialPropertyDescriptor { value: Some(val.into()), writable: Some(writable), enumerable: Some(enumerable), configurable: Some(configurable), ..Default::default() },
            );
            match result {
                Err(err) => Err(unwind_any_error(&mut agent, err)),
                Ok(success) => Ok((success, a.o.common_object_data().borrow().propdump())),
            }
        }

        #[test_case("10", true, true, true, true => Ok((true, vec![
            PropertyInfo { name: PropertyKey::from("length"), enumerable: false, configurable: false, kind: PropertyInfoKind::Data { value: ECMAScriptValue::from(100.0), writable: false}},
            PropertyInfo { name: PropertyKey::from("10"), enumerable: true, configurable: true, kind: PropertyInfoKind::Data { value: ECMAScriptValue::from(true), writable: true}}
        ])); "no change to length")]
        #[test_case("900", true, true, true, true => Ok((false, vec![
            PropertyInfo { name: PropertyKey::from("length"), enumerable: false, configurable: false, kind: PropertyInfoKind::Data { value: ECMAScriptValue::from(100.0), writable: false}},
        ])); "attempted length change")]
        fn read_only_length(key: &str, val: impl Into<ECMAScriptValue>, writable: bool, enumerable: bool, configurable: bool) -> Result<(bool, Vec<PropertyInfo>), String> {
            let mut agent = test_agent();
            let a = ArrayObject::create(&mut agent, 100, None).unwrap();
            // Make the length property read-only
            a.o.define_own_property(&mut agent, "length".into(), PotentialPropertyDescriptor { writable: Some(false), ..Default::default() }).unwrap();
            // Exercise function under test
            let result = a.o.define_own_property(
                &mut agent,
                key.into(),
                PotentialPropertyDescriptor { value: Some(val.into()), writable: Some(writable), enumerable: Some(enumerable), configurable: Some(configurable), ..Default::default() },
            );
            match result {
                Err(err) => Err(unwind_any_error(&mut agent, err)),
                Ok(success) => Ok((success, a.o.common_object_data().borrow().propdump())),
            }
        }

        #[test]
        fn read_only_elem() {
            let mut agent = test_agent();
            let a = ArrayObject::create(&mut agent, 100, None).unwrap();
            // Make a read-only property with an array index
            a.o.define_own_property(
                &mut agent,
                "30".into(),
                PotentialPropertyDescriptor { value: Some("blue".into()), writable: Some(false), enumerable: Some(true), configurable: Some(false), ..Default::default() },
            )
            .unwrap();
            // Now exercise: try to overwrite that property
            let result =
                a.o.define_own_property(
                    &mut agent,
                    "30".into(),
                    PotentialPropertyDescriptor { value: Some("green".into()), writable: Some(false), enumerable: Some(true), configurable: Some(false), ..Default::default() },
                )
                .unwrap();

            assert!(!result);
            assert_eq!(
                a.o.common_object_data().borrow().propdump(),
                vec![
                    PropertyInfo {
                        name: PropertyKey::from("length"),
                        enumerable: false,
                        configurable: false,
                        kind: PropertyInfoKind::Data { value: ECMAScriptValue::from(100.0), writable: true }
                    },
                    PropertyInfo {
                        name: PropertyKey::from("30"),
                        enumerable: true,
                        configurable: false,
                        kind: PropertyInfoKind::Data { value: ECMAScriptValue::from("blue"), writable: false }
                    }
                ]
            );
        }
    }

    #[test]
    fn has_property() {
        let mut agent = test_agent();
        let a = ArrayObject::create(&mut agent, 0, None).unwrap();
        assert!(a.o.has_property(&mut agent, &"length".into()).unwrap());
    }
    #[test]
    fn get() {
        let mut agent = test_agent();
        let a = ArrayObject::create(&mut agent, 0, None).unwrap();
        let receiver: ECMAScriptValue = a.clone().into();
        let val = a.o.get(&mut agent, &"length".into(), &receiver).unwrap();
        assert_eq!(val, 0.into());
    }
    #[test]
    fn set_() {
        let mut agent = test_agent();
        let a = ArrayObject::create(&mut agent, 0, None).unwrap();
        let receiver: ECMAScriptValue = a.clone().into();
        let success = a.o.set(&mut agent, "length".into(), 100.into(), &receiver).unwrap();
        assert!(success);
        assert_eq!(a.o.get(&mut agent, &"length".into(), &receiver).unwrap(), 100.into());
    }
    #[test]
    fn delete() {
        let mut agent = test_agent();
        let a = ArrayObject::create(&mut agent, 0, None).unwrap();
        let success = a.o.delete(&mut agent, &"length".into()).unwrap();
        assert!(!success);
    }
    #[test]
    fn own_property_keys() {
        let mut agent = test_agent();
        let a = ArrayObject::create(&mut agent, 0, None).unwrap();
        let list = a.o.own_property_keys(&mut agent).unwrap();
        assert_eq!(list, vec!["length".into()]);
    }
    #[test]
    fn is_array_object() {
        let mut agent = test_agent();
        let a = ArrayObject::create(&mut agent, 0, None).unwrap();
        assert!(a.o.is_array_object());
    }

    mod set_length {
        use super::*;
        use test_case::test_case;

        fn value_just_once(agent: &mut Agent, this_value: ECMAScriptValue, _: Option<&Object>, _: &[ECMAScriptValue]) -> Completion<ECMAScriptValue> {
            // The value 320 the first time, errors thrown all other times.
            let this: Object = this_value.try_into().unwrap();
            let previous = get(agent, &this, &"has_already_run".into()).unwrap();
            match previous {
                ECMAScriptValue::Undefined => {
                    set(agent, &this, "has_already_run".into(), true.into(), true).unwrap();
                    Ok(320.0.into())
                }
                _ => Err(create_type_error(agent, "valueOf called too many times")),
            }
        }
        fn screwy_get_value(agent: &mut Agent) -> PotentialPropertyDescriptor {
            let object_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
            let function_proto = agent.intrinsic(IntrinsicId::FunctionPrototype);
            let obj = ordinary_object_create(agent, Some(object_proto), &[]);
            let value_of = create_builtin_function(agent, value_just_once, false, 0.0, "valueOf".into(), &[], None, Some(function_proto), None);
            define_property_or_throw(
                agent,
                &obj,
                "valueOf",
                PotentialPropertyDescriptor { value: Some(value_of.into()), writable: Some(false), enumerable: Some(false), configurable: Some(true), ..Default::default() },
            )
            .unwrap();
            PotentialPropertyDescriptor { value: Some(obj.into()), ..Default::default() }
        }
        fn readonly(_: &mut Agent) -> PotentialPropertyDescriptor {
            PotentialPropertyDescriptor { writable: Some(false), ..Default::default() }
        }
        fn fraction(_: &mut Agent) -> PotentialPropertyDescriptor {
            PotentialPropertyDescriptor { value: Some(1.5.into()), ..Default::default() }
        }
        fn symbol(a: &mut Agent) -> PotentialPropertyDescriptor {
            let sym = a.wks(WksId::Species);
            PotentialPropertyDescriptor { value: Some(sym.into()), ..Default::default() }
        }
        fn bigger(_: &mut Agent) -> PotentialPropertyDescriptor {
            PotentialPropertyDescriptor { value: Some(7000.into()), ..Default::default() }
        }
        fn configurable_400(_: &mut Agent) -> PotentialPropertyDescriptor {
            PotentialPropertyDescriptor { value: Some(400.into()), configurable: Some(true), ..Default::default() }
        }
        fn writable_700(_: &mut Agent) -> PotentialPropertyDescriptor {
            PotentialPropertyDescriptor { value: Some(700.0.into()), writable: Some(true), ..Default::default() }
        }
        fn readonly_0(_: &mut Agent) -> PotentialPropertyDescriptor {
            PotentialPropertyDescriptor { value: Some(0.into()), writable: Some(false), ..Default::default() }
        }
        fn fifty(_: &mut Agent) -> PotentialPropertyDescriptor {
            PotentialPropertyDescriptor { value: Some(50.0.into()), ..Default::default() }
        }

        #[test_case(readonly =>
            Ok((true, vec![
                PropertyInfo {
                    name: PropertyKey::from("length"),
                    enumerable: false,
                    configurable: false,
                    kind: PropertyInfoKind::Data { value: ECMAScriptValue::from(0.0), writable: false },
                }
            ])); "no value")]
        #[test_case(fraction => Err("RangeError: Invalid array length".to_string()); "bad length")]
        #[test_case(symbol => Err("TypeError: Symbol values cannot be converted to Number values".to_string()); "to_uint32 throws")]
        #[test_case(screwy_get_value => Err("TypeError: valueOf called too many times".to_string()); "to_number throws")]
        #[test_case(bigger =>
            Ok((true, vec![
                PropertyInfo {
                    name: PropertyKey::from("length"),
                    enumerable: false,
                    configurable: false,
                    kind: PropertyInfoKind::Data { value: ECMAScriptValue::from(7000.0), writable: true },
                }
            ])); "length increase")]
        fn zero_elements(make_desc: fn(&mut Agent) -> PotentialPropertyDescriptor) -> Result<(bool, Vec<PropertyInfo>), String> {
            let mut agent = test_agent();
            let aobj = ArrayObject::create(&mut agent, 0, None).unwrap();
            let a = aobj.o.to_array_object().unwrap();
            let desc = make_desc(&mut agent);

            a.set_length(&mut agent, desc).map(|success| (success, a.common.borrow().propdump())).map_err(|err| unwind_any_error(&mut agent, err))
        }

        #[test]
        fn readonly_length() {
            let mut agent = test_agent();
            let aobj = ArrayObject::create(&mut agent, 9000, None).unwrap();
            define_property_or_throw(&mut agent, &aobj, "length", PotentialPropertyDescriptor { writable: Some(false), ..Default::default() }).unwrap();
            let a = aobj.o.to_array_object().unwrap();

            let result = a
                .set_length(&mut agent, PotentialPropertyDescriptor { value: Some(1000.0.into()), ..Default::default() })
                .map(|success| (success, a.common.borrow().propdump()))
                .map_err(|err| unwind_any_error(&mut agent, err));
            assert_eq!(
                result,
                Ok((
                    false,
                    vec![PropertyInfo {
                        name: PropertyKey::from("length"),
                        enumerable: false,
                        configurable: false,
                        kind: PropertyInfoKind::Data { value: ECMAScriptValue::from(9000.0), writable: false },
                    }]
                ))
            );
        }

        #[test_case(configurable_400 => Ok((false, vec![
            PropertyInfo {
                name: PropertyKey::from("length"),
                enumerable: false,
                configurable: false,
                kind: PropertyInfoKind::Data { value: ECMAScriptValue::from(9000.0), writable: true },
            },
            PropertyInfo {
                name: PropertyKey::from("0"),
                enumerable: true,
                configurable: true,
                kind: PropertyInfoKind::Data { value: ECMAScriptValue::from("blue"), writable: true },
            },
            PropertyInfo {
                name: PropertyKey::from("100"),
                enumerable: true,
                configurable: true,
                kind: PropertyInfoKind::Data { value: ECMAScriptValue::from("green"), writable: true },
            },
            PropertyInfo {
                name: PropertyKey::from("500"),
                enumerable: true,
                configurable: true,
                kind: PropertyInfoKind::Data { value: ECMAScriptValue::from("red"), writable: true },
            },
        ])); "set fails cleanly")]
        #[test_case(writable_700 => Ok((true, vec![
            PropertyInfo {
                name: PropertyKey::from("length"),
                enumerable: false,
                configurable: false,
                kind: PropertyInfoKind::Data { value: ECMAScriptValue::from(700.0), writable: true },
            },
            PropertyInfo {
                name: PropertyKey::from("0"),
                enumerable: true,
                configurable: true,
                kind: PropertyInfoKind::Data { value: ECMAScriptValue::from("blue"), writable: true },
            },
            PropertyInfo {
                name: PropertyKey::from("100"),
                enumerable: true,
                configurable: true,
                kind: PropertyInfoKind::Data { value: ECMAScriptValue::from("green"), writable: true },
            },
            PropertyInfo {
                name: PropertyKey::from("500"),
                enumerable: true,
                configurable: true,
                kind: PropertyInfoKind::Data { value: ECMAScriptValue::from("red"), writable: true },
            },
        ])); "no deletion")]
        #[test_case(readonly_0 => Ok((true, vec![
            PropertyInfo {
                name: PropertyKey::from("length"),
                enumerable: false,
                configurable: false,
                kind: PropertyInfoKind::Data { value: ECMAScriptValue::from(0), writable: false },
            },
        ])); "delete them all and lock")]
        fn three_elements(make_desc: fn(&mut Agent) -> PotentialPropertyDescriptor) -> Result<(bool, Vec<PropertyInfo>), String> {
            let mut agent = test_agent();
            let aobj = ArrayObject::create(&mut agent, 9000, None).unwrap();
            set(&mut agent, &aobj, "0".into(), "blue".into(), true).unwrap();
            set(&mut agent, &aobj, "100".into(), "green".into(), true).unwrap();
            set(&mut agent, &aobj, "500".into(), "red".into(), true).unwrap();
            let a = aobj.o.to_array_object().unwrap();
            let desc = make_desc(&mut agent);

            a.set_length(&mut agent, desc).map(|success| (success, a.common.borrow().propdump())).map_err(|err| unwind_any_error(&mut agent, err))
        }

        #[test_case(fifty => Ok((false, vec![
            PropertyInfo {
                name: PropertyKey::from("length"),
                enumerable: false,
                configurable: false,
                kind: PropertyInfoKind::Data { value: ECMAScriptValue::from(101.0), writable: true },
            },
            PropertyInfo {
                name: PropertyKey::from("0"),
                enumerable: true,
                configurable: true,
                kind: PropertyInfoKind::Data { value: ECMAScriptValue::from("blue"), writable: true },
            },
            PropertyInfo {
                name: PropertyKey::from("100"),
                enumerable: true,
                configurable: false,
                kind: PropertyInfoKind::Data { value: ECMAScriptValue::from("green"), writable: true },
            },
        ])); "aborting shorten")]
        #[test_case(readonly_0 => Ok((false, vec![
            PropertyInfo {
                name: PropertyKey::from("length"),
                enumerable: false,
                configurable: false,
                kind: PropertyInfoKind::Data { value: ECMAScriptValue::from(101.0), writable: false },
            },
            PropertyInfo {
                name: PropertyKey::from("0"),
                enumerable: true,
                configurable: true,
                kind: PropertyInfoKind::Data { value: ECMAScriptValue::from("blue"), writable: true },
            },
            PropertyInfo {
                name: PropertyKey::from("100"),
                enumerable: true,
                configurable: false,
                kind: PropertyInfoKind::Data { value: ECMAScriptValue::from("green"), writable: true },
            },
        ])); "abort then freeze")]
        fn frozen_middle(make_desc: fn(&mut Agent) -> PotentialPropertyDescriptor) -> Result<(bool, Vec<PropertyInfo>), String> {
            let mut agent = test_agent();
            let aobj = ArrayObject::create(&mut agent, 9000, None).unwrap();
            set(&mut agent, &aobj, "0".into(), "blue".into(), true).unwrap();
            define_property_or_throw(
                &mut agent,
                &aobj,
                "100",
                PotentialPropertyDescriptor { value: Some("green".into()), writable: Some(true), enumerable: Some(true), configurable: Some(false), ..Default::default() },
            )
            .unwrap();
            set(&mut agent, &aobj, "500".into(), "red".into(), true).unwrap();
            let a = aobj.o.to_array_object().unwrap();
            let desc = make_desc(&mut agent);

            a.set_length(&mut agent, desc).map(|success| (success, a.common.borrow().propdump())).map_err(|err| unwind_any_error(&mut agent, err))
        }
    }
}

#[test]
fn array_create() {
    let mut agent = test_agent();
    let array_proto = agent.intrinsic(IntrinsicId::ArrayPrototype);
    let custom_proto = ordinary_object_create(&mut agent, Some(array_proto), &[]);
    let aobj = super::array_create(&mut agent, 231, Some(custom_proto.clone())).unwrap();
    assert_eq!(aobj.o.get_prototype_of(&mut agent).unwrap(), Some(custom_proto));
    assert_eq!(get(&mut agent, &aobj, &"length".into()).unwrap(), ECMAScriptValue::from(231.0));
    assert!(aobj.is_array(&mut agent).unwrap())
}

fn make_ordinary_object(agent: &mut Agent) -> ECMAScriptValue {
    let proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
    ordinary_object_create(agent, Some(proto), &[]).into()
}
fn make_array_object(agent: &mut Agent) -> ECMAScriptValue {
    super::array_create(agent, 10, None).unwrap().into()
}
#[test_case(make_ordinary_object => Ok(false); "ordinary object")]
#[test_case(make_array_object => Ok(true); "array object")]
#[test_case(|_| ECMAScriptValue::Undefined => Ok(false); "undefined")]
#[test_case(|_| ECMAScriptValue::Null => Ok(false); "null")]
#[test_case(|_| ECMAScriptValue::from(true) => Ok(false); "boolean")]
#[test_case(|_| ECMAScriptValue::from(33.2) => Ok(false); "number")]
fn is_array(make_arg: fn(&mut Agent) -> ECMAScriptValue) -> Result<bool, String> {
    let mut agent = test_agent();
    let arg = make_arg(&mut agent);

    super::is_array(&mut agent, &arg).map_err(|err| unwind_any_error(&mut agent, err))
}

mod array_species_create {
    use super::*;
    use test_case::test_case;

    fn make_ordinary(agent: &mut Agent) -> Object {
        let proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
        ordinary_object_create(agent, Some(proto), &[])
    }

    fn make_throwing_constructor_prop(agent: &mut Agent) -> Object {
        let proto = agent.intrinsic(IntrinsicId::ArrayPrototype);
        let function_proto = agent.intrinsic(IntrinsicId::FunctionPrototype);
        let obj = super::super::array_create(agent, 0, Some(proto)).unwrap();
        let constructor_getter = create_builtin_function(agent, faux_errors, false, 0.0, "constructor".into(), &[], None, Some(function_proto), Some("get".into()));
        define_property_or_throw(
            agent,
            &obj,
            "constructor",
            PotentialPropertyDescriptor { get: Some(constructor_getter.into()), set: None, enumerable: Some(true), configurable: Some(true), ..Default::default() },
        )
        .unwrap();
        obj
    }
    fn make_undefined_constructor_prop(agent: &mut Agent) -> Object {
        let proto = agent.intrinsic(IntrinsicId::ArrayPrototype);
        let obj = super::super::array_create(agent, 0, Some(proto)).unwrap();
        define_property_or_throw(
            agent,
            &obj,
            "constructor",
            PotentialPropertyDescriptor { value: Some(ECMAScriptValue::Undefined), writable: Some(true), enumerable: Some(true), configurable: Some(true), ..Default::default() },
        )
        .unwrap();
        obj
    }
    fn make_plain_array(agent: &mut Agent) -> Object {
        let proto = agent.intrinsic(IntrinsicId::ArrayPrototype);
        super::super::array_create(agent, 5, Some(proto)).unwrap()
    }
    fn make_primitive_constructor_prop(agent: &mut Agent) -> Object {
        let proto = agent.intrinsic(IntrinsicId::ArrayPrototype);
        let obj = super::super::array_create(agent, 0, Some(proto)).unwrap();
        define_property_or_throw(
            agent,
            &obj,
            "constructor",
            PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(false)), writable: Some(true), enumerable: Some(true), configurable: Some(true), ..Default::default() },
        )
        .unwrap();
        obj
    }

    #[test_case(make_ordinary, 10 => Ok(vec![
        PropertyInfo {
            name: PropertyKey::from("length"),
            enumerable: false,
            configurable: false,
            kind: PropertyInfoKind::Data { value: ECMAScriptValue::from(10.0), writable: true },
        },
    ]); "not array")]
    #[test_case(make_ordinary, 42949672950 => Err("RangeError: Array lengths greater than 4294967295 are not allowed".to_string()); "bad length")]
    #[test_case(make_throwing_constructor_prop, 200 => Err("TypeError: Test Sentinel".to_string()); "get(constructor) throws")]
    #[test_case(make_undefined_constructor_prop, 0 => Ok(vec![
        PropertyInfo {
            name: PropertyKey::from("length"),
            enumerable: false,
            configurable: false,
            kind: PropertyInfoKind::Data { value: ECMAScriptValue::from(0.0), writable: true },
        },
    ]); "undefined constructor")]
    #[test_case(make_undefined_constructor_prop, 42949672950 => Err("RangeError: Array lengths greater than 4294967295 are not allowed".to_string()); "undefined constructor plus bad length")]
    #[test_case(make_plain_array, 542 => Ok(vec![
        PropertyInfo {
            name: PropertyKey::from("length"),
            enumerable: false,
            configurable: false,
            kind: PropertyInfoKind::Data { value: ECMAScriptValue::from(542.0), writable: true },
        },
    ]); "plain array")]
    #[test_case(make_primitive_constructor_prop, 10 => Err("TypeError: Array species constructor invalid".to_string()); "primitive in constructor")]
    fn f(make_original: fn(&mut Agent) -> Object, length: u64) -> Result<Vec<PropertyInfo>, String> {
        let mut agent = test_agent();
        let original = make_original(&mut agent);
        array_species_create(&mut agent, &original, length)
            .map(|val| Object::try_from(val).unwrap().o.common_object_data().borrow().propdump())
            .map_err(|err| unwind_any_error(&mut agent, err))
    }

    // todo!(): More tests want to be here to cover other code paths, but those code paths require:
    // * Proxy Objects
    // * Bound Function Objects
    // * An Array constructor that does something besides a todo-panic
    // * Species fields on other objects
}

#[test]
fn defaults() {
    // These don't really test anything except the default implementations, but it does clear a fair few instantiations
    // out of the "uncovered" set.
    let mut agent = test_agent();
    let a = super::array_create(&mut agent, 10, None).unwrap();
    assert_eq!(a.o.is_date_object(), false);
    assert!(a.o.to_function_obj().is_none());
    assert!(a.o.to_error_obj().is_none());
    assert_eq!(a.o.is_arguments_object(), false);
    assert_eq!(a.o.is_number_object(), false);
    assert!(a.o.to_builtin_function_obj().is_none());
    assert!(a.o.to_callable_obj().is_none());
    assert!(a.o.to_number_obj().is_none());
    assert_eq!(a.o.is_callable_obj(), false);
    assert_eq!(a.o.is_string_object(), false);
    assert_eq!(a.o.is_regexp_object(), false);
    assert!(a.o.to_constructable().is_none());
    assert_eq!(a.o.is_boolean_object(), false);
    assert!(a.o.to_boolean_obj().is_none());
    assert_eq!(a.o.is_proxy_object(), false);
    assert_eq!(a.o.is_error_object(), false);
}

#[test_case(super::array_constructor_function => panics; "array_constructor_function")]
#[test_case(super::array_from => panics; "array_from")]
#[test_case(super::array_is_array => panics; "array_is_array")]
#[test_case(super::array_of => panics; "array_of")]
#[test_case(super::array_prototype_at => panics; "array_prototype_at")]
#[test_case(super::array_prototype_concat => panics; "array_prototype_concat")]
#[test_case(super::array_prototype_copy_within => panics; "array_prototype_copy_within")]
#[test_case(super::array_prototype_entries => panics; "array_prototype_entries")]
#[test_case(super::array_prototype_every => panics; "array_prototype_every")]
#[test_case(super::array_prototype_fill => panics; "array_prototype_fill")]
#[test_case(super::array_prototype_filter => panics; "array_prototype_filter")]
#[test_case(super::array_prototype_find => panics; "array_prototype_find")]
#[test_case(super::array_prototype_find_index => panics; "array_prototype_find_index")]
#[test_case(super::array_prototype_flat => panics; "array_prototype_flat")]
#[test_case(super::array_prototype_flat_map => panics; "array_prototype_flat_map")]
#[test_case(super::array_prototype_for_each => panics; "array_prototype_for_each")]
#[test_case(super::array_prototype_includes => panics; "array_prototype_includes")]
#[test_case(super::array_prototype_index_of => panics; "array_prototype_index_of")]
#[test_case(super::array_prototype_join => panics; "array_prototype_join")]
#[test_case(super::array_prototype_keys => panics; "array_prototype_keys")]
#[test_case(super::array_prototype_last_index_of => panics; "array_prototype_last_index_of")]
#[test_case(super::array_prototype_map => panics; "array_prototype_map")]
#[test_case(super::array_prototype_pop => panics; "array_prototype_pop")]
#[test_case(super::array_prototype_push => panics; "array_prototype_push")]
#[test_case(super::array_prototype_reduce => panics; "array_prototype_reduce")]
#[test_case(super::array_prototype_reduce_right => panics; "array_prototype_reduce_right")]
#[test_case(super::array_prototype_reverse => panics; "array_prototype_reverse")]
#[test_case(super::array_prototype_shift => panics; "array_prototype_shift")]
#[test_case(super::array_prototype_slice => panics; "array_prototype_slice")]
#[test_case(super::array_prototype_some => panics; "array_prototype_some")]
#[test_case(super::array_prototype_sort => panics; "array_prototype_sort")]
#[test_case(super::array_prototype_splice => panics; "array_prototype_splice")]
#[test_case(super::array_prototype_to_locale_string => panics; "array_prototype_to_locale_string")]
#[test_case(super::array_prototype_to_string => panics; "array_prototype_to_string")]
#[test_case(super::array_prototype_unshift => panics; "array_prototype_unshift")]
#[test_case(super::array_prototype_values => panics; "array_prototype_values")]
fn todo(f: fn(&mut Agent, ECMAScriptValue, Option<&Object>, &[ECMAScriptValue]) -> Completion<ECMAScriptValue>) {
    let mut agent = test_agent();
    f(&mut agent, ECMAScriptValue::Undefined, None, &[]).unwrap();
}
