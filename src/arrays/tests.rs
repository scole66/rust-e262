use super::*;
use crate::tests::*;
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
            setup_test_agent();

            let result = ArrayObject::create(length, None);
            match result {
                Err(err) => Err(unwind_any_error(err)),
                Ok(obj) => {
                    assert!(obj.is_array().unwrap());
                    assert_eq!(obj.o.get_prototype_of().unwrap(), Some(intrinsic(IntrinsicId::ArrayPrototype)));
                    Ok(obj.o.common_object_data().borrow().propdump())
                }
            }
        }

        #[test]
        fn proto_specified() {
            setup_test_agent();
            let object_proto = intrinsic(IntrinsicId::ObjectPrototype);

            let obj = ArrayObject::create(600, Some(object_proto.clone())).unwrap();
            assert!(obj.is_array().unwrap());
            assert_eq!(obj.o.get_prototype_of().unwrap(), Some(object_proto));
        }
    }

    #[test]
    fn debug() {
        setup_test_agent();
        let a = ArrayObject::create(0, None).unwrap();
        assert_ne!(format!("{:?}", a), "");
    }

    #[test]
    fn is_ordinary() {
        setup_test_agent();
        let a = ArrayObject::create(0, None).unwrap();
        assert_eq!(a.o.is_ordinary(), true);
    }

    #[test]
    fn get_prototype_of() {
        setup_test_agent();
        let a = ArrayObject::create(0, None).unwrap();
        let a_proto = a.o.get_prototype_of().unwrap().unwrap();
        assert_eq!(a_proto, intrinsic(IntrinsicId::ArrayPrototype));
    }

    #[test]
    fn set_prototype_of() {
        setup_test_agent();
        let a = ArrayObject::create(0, None).unwrap();
        let obj_proto = intrinsic(IntrinsicId::ObjectPrototype);
        let success = a.o.set_prototype_of(Some(obj_proto.clone())).unwrap();
        assert!(success);
        assert_eq!(obj_proto, a.o.get_prototype_of().unwrap().unwrap());
    }

    #[test]
    fn is_extensible() {
        setup_test_agent();
        let a = ArrayObject::create(0, None).unwrap();
        assert!(a.o.is_extensible().unwrap());
    }
    #[test]
    fn prevent_extensions() {
        setup_test_agent();
        let a = ArrayObject::create(0, None).unwrap();
        assert!(a.o.prevent_extensions().unwrap());
        assert!(!a.o.is_extensible().unwrap());
    }
    #[test]
    fn get_own_property() {
        setup_test_agent();
        let a = ArrayObject::create(0, None).unwrap();
        let desc: DataDescriptor = a.o.get_own_property(&"length".into()).unwrap().unwrap().try_into().unwrap();

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
        fn normal(
            key: &str,
            val: impl Into<ECMAScriptValue>,
            writable: bool,
            enumerable: bool,
            configurable: bool,
        ) -> Result<(bool, Vec<PropertyInfo>), String> {
            setup_test_agent();
            let a = ArrayObject::create(0, None).unwrap();
            let result = a.o.define_own_property(
                key.into(),
                PotentialPropertyDescriptor {
                    value: Some(val.into()),
                    writable: Some(writable),
                    enumerable: Some(enumerable),
                    configurable: Some(configurable),
                    ..Default::default()
                },
            );
            match result {
                Err(err) => Err(unwind_any_error(err)),
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
        fn read_only_length(
            key: &str,
            val: impl Into<ECMAScriptValue>,
            writable: bool,
            enumerable: bool,
            configurable: bool,
        ) -> Result<(bool, Vec<PropertyInfo>), String> {
            setup_test_agent();
            let a = ArrayObject::create(100, None).unwrap();
            // Make the length property read-only
            a.o.define_own_property(
                "length".into(),
                PotentialPropertyDescriptor { writable: Some(false), ..Default::default() },
            )
            .unwrap();
            // Exercise function under test
            let result = a.o.define_own_property(
                key.into(),
                PotentialPropertyDescriptor {
                    value: Some(val.into()),
                    writable: Some(writable),
                    enumerable: Some(enumerable),
                    configurable: Some(configurable),
                    ..Default::default()
                },
            );
            match result {
                Err(err) => Err(unwind_any_error(err)),
                Ok(success) => Ok((success, a.o.common_object_data().borrow().propdump())),
            }
        }

        #[test]
        fn read_only_elem() {
            setup_test_agent();
            let a = ArrayObject::create(100, None).unwrap();
            // Make a read-only property with an array index
            a.o.define_own_property(
                "30".into(),
                PotentialPropertyDescriptor {
                    value: Some("blue".into()),
                    writable: Some(false),
                    enumerable: Some(true),
                    configurable: Some(false),
                    ..Default::default()
                },
            )
            .unwrap();
            // Now exercise: try to overwrite that property
            let result =
                a.o.define_own_property(
                    "30".into(),
                    PotentialPropertyDescriptor {
                        value: Some("green".into()),
                        writable: Some(false),
                        enumerable: Some(true),
                        configurable: Some(false),
                        ..Default::default()
                    },
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
        setup_test_agent();
        let a = ArrayObject::create(0, None).unwrap();
        assert!(a.o.has_property(&"length".into()).unwrap());
    }
    #[test]
    fn get() {
        setup_test_agent();
        let a = ArrayObject::create(0, None).unwrap();
        let receiver: ECMAScriptValue = a.clone().into();
        let val = a.o.get(&"length".into(), &receiver).unwrap();
        assert_eq!(val, 0.into());
    }
    #[test]
    fn set_() {
        setup_test_agent();
        let a = ArrayObject::create(0, None).unwrap();
        let receiver: ECMAScriptValue = a.clone().into();
        let success = a.o.set("length".into(), 100.into(), &receiver).unwrap();
        assert!(success);
        assert_eq!(a.o.get(&"length".into(), &receiver).unwrap(), 100.into());
    }
    #[test]
    fn delete() {
        setup_test_agent();
        let a = ArrayObject::create(0, None).unwrap();
        let success = a.o.delete(&"length".into()).unwrap();
        assert!(!success);
    }
    #[test]
    fn own_property_keys() {
        setup_test_agent();
        let a = ArrayObject::create(0, None).unwrap();
        let list = a.o.own_property_keys().unwrap();
        assert_eq!(list, vec!["length".into()]);
    }
    #[test]
    fn is_array_object() {
        setup_test_agent();
        let a = ArrayObject::create(0, None).unwrap();
        assert!(a.o.is_array_object());
    }

    mod set_length {
        use super::*;
        use test_case::test_case;

        fn value_just_once(
            this_value: ECMAScriptValue,
            _: Option<&Object>,
            _: &[ECMAScriptValue],
        ) -> Completion<ECMAScriptValue> {
            // The value 320 the first time, errors thrown all other times.
            let this: Object = this_value.try_into().unwrap();
            let previous = get(&this, &"has_already_run".into()).unwrap();
            match previous {
                ECMAScriptValue::Undefined => {
                    set(&this, "has_already_run".into(), true.into(), true).unwrap();
                    Ok(320.0.into())
                }
                _ => Err(create_type_error("valueOf called too many times")),
            }
        }
        fn screwy_get_value() -> PotentialPropertyDescriptor {
            let object_proto = intrinsic(IntrinsicId::ObjectPrototype);
            let function_proto = intrinsic(IntrinsicId::FunctionPrototype);
            let obj = ordinary_object_create(Some(object_proto), &[]);
            let value_of = create_builtin_function(
                value_just_once,
                false,
                0.0,
                "valueOf".into(),
                &[],
                None,
                Some(function_proto),
                None,
            );
            define_property_or_throw(
                &obj,
                "valueOf",
                PotentialPropertyDescriptor {
                    value: Some(value_of.into()),
                    writable: Some(false),
                    enumerable: Some(false),
                    configurable: Some(true),
                    ..Default::default()
                },
            )
            .unwrap();
            PotentialPropertyDescriptor { value: Some(obj.into()), ..Default::default() }
        }
        fn readonly() -> PotentialPropertyDescriptor {
            PotentialPropertyDescriptor { writable: Some(false), ..Default::default() }
        }
        fn fraction() -> PotentialPropertyDescriptor {
            PotentialPropertyDescriptor { value: Some(1.5.into()), ..Default::default() }
        }
        fn symbol() -> PotentialPropertyDescriptor {
            let sym = wks(WksId::Species);
            PotentialPropertyDescriptor { value: Some(sym.into()), ..Default::default() }
        }
        fn bigger() -> PotentialPropertyDescriptor {
            PotentialPropertyDescriptor { value: Some(7000.into()), ..Default::default() }
        }
        fn configurable_400() -> PotentialPropertyDescriptor {
            PotentialPropertyDescriptor { value: Some(400.into()), configurable: Some(true), ..Default::default() }
        }
        fn writable_700() -> PotentialPropertyDescriptor {
            PotentialPropertyDescriptor { value: Some(700.0.into()), writable: Some(true), ..Default::default() }
        }
        fn readonly_0() -> PotentialPropertyDescriptor {
            PotentialPropertyDescriptor { value: Some(0.into()), writable: Some(false), ..Default::default() }
        }
        fn fifty() -> PotentialPropertyDescriptor {
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
        fn zero_elements(make_desc: fn() -> PotentialPropertyDescriptor) -> Result<(bool, Vec<PropertyInfo>), String> {
            setup_test_agent();
            let aobj = ArrayObject::create(0, None).unwrap();
            let a = aobj.o.to_array_object().unwrap();
            let desc = make_desc();

            a.set_length(desc).map(|success| (success, a.common.borrow().propdump())).map_err(unwind_any_error)
        }

        #[test]
        fn readonly_length() {
            setup_test_agent();
            let aobj = ArrayObject::create(9000, None).unwrap();
            define_property_or_throw(
                &aobj,
                "length",
                PotentialPropertyDescriptor { writable: Some(false), ..Default::default() },
            )
            .unwrap();
            let a = aobj.o.to_array_object().unwrap();

            let result = a
                .set_length(PotentialPropertyDescriptor { value: Some(1000.0.into()), ..Default::default() })
                .map(|success| (success, a.common.borrow().propdump()))
                .map_err(unwind_any_error);
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
        fn three_elements(make_desc: fn() -> PotentialPropertyDescriptor) -> Result<(bool, Vec<PropertyInfo>), String> {
            setup_test_agent();
            let aobj = ArrayObject::create(9000, None).unwrap();
            set(&aobj, "0".into(), "blue".into(), true).unwrap();
            set(&aobj, "100".into(), "green".into(), true).unwrap();
            set(&aobj, "500".into(), "red".into(), true).unwrap();
            let a = aobj.o.to_array_object().unwrap();
            let desc = make_desc();

            a.set_length(desc).map(|success| (success, a.common.borrow().propdump())).map_err(unwind_any_error)
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
        fn frozen_middle(make_desc: fn() -> PotentialPropertyDescriptor) -> Result<(bool, Vec<PropertyInfo>), String> {
            setup_test_agent();
            let aobj = ArrayObject::create(9000, None).unwrap();
            set(&aobj, "0".into(), "blue".into(), true).unwrap();
            define_property_or_throw(
                &aobj,
                "100",
                PotentialPropertyDescriptor {
                    value: Some("green".into()),
                    writable: Some(true),
                    enumerable: Some(true),
                    configurable: Some(false),
                    ..Default::default()
                },
            )
            .unwrap();
            set(&aobj, "500".into(), "red".into(), true).unwrap();
            let a = aobj.o.to_array_object().unwrap();
            let desc = make_desc();

            a.set_length(desc).map(|success| (success, a.common.borrow().propdump())).map_err(unwind_any_error)
        }
    }
}

#[test]
fn array_create() {
    setup_test_agent();
    let array_proto = intrinsic(IntrinsicId::ArrayPrototype);
    let custom_proto = ordinary_object_create(Some(array_proto), &[]);
    let aobj = super::array_create(231, Some(custom_proto.clone())).unwrap();
    assert_eq!(aobj.o.get_prototype_of().unwrap(), Some(custom_proto));
    assert_eq!(get(&aobj, &"length".into()).unwrap(), ECMAScriptValue::from(231.0));
    assert!(aobj.is_array().unwrap())
}

fn make_ordinary_object() -> ECMAScriptValue {
    let proto = intrinsic(IntrinsicId::ObjectPrototype);
    ordinary_object_create(Some(proto), &[]).into()
}
fn make_array_object() -> ECMAScriptValue {
    super::array_create(10, None).unwrap().into()
}
#[test_case(make_ordinary_object => Ok(false); "ordinary object")]
#[test_case(make_array_object => Ok(true); "array object")]
#[test_case(|| ECMAScriptValue::Undefined => Ok(false); "undefined")]
#[test_case(|| ECMAScriptValue::Null => Ok(false); "null")]
#[test_case(|| ECMAScriptValue::from(true) => Ok(false); "boolean")]
#[test_case(|| ECMAScriptValue::from(33.2) => Ok(false); "number")]
fn is_array(make_arg: fn() -> ECMAScriptValue) -> Result<bool, String> {
    setup_test_agent();
    let arg = make_arg();

    super::is_array(&arg).map_err(unwind_any_error)
}

mod array_species_create {
    use super::*;
    use test_case::test_case;

    fn make_ordinary() -> Object {
        let proto = intrinsic(IntrinsicId::ObjectPrototype);
        ordinary_object_create(Some(proto), &[])
    }

    fn make_throwing_constructor_prop() -> Object {
        let proto = intrinsic(IntrinsicId::ArrayPrototype);
        let function_proto = intrinsic(IntrinsicId::FunctionPrototype);
        let obj = super::super::array_create(0, Some(proto)).unwrap();
        let constructor_getter = create_builtin_function(
            faux_errors,
            false,
            0.0,
            "constructor".into(),
            &[],
            None,
            Some(function_proto),
            Some("get".into()),
        );
        define_property_or_throw(
            &obj,
            "constructor",
            PotentialPropertyDescriptor {
                get: Some(constructor_getter.into()),
                set: None,
                enumerable: Some(true),
                configurable: Some(true),
                ..Default::default()
            },
        )
        .unwrap();
        obj
    }
    fn make_undefined_constructor_prop() -> Object {
        let proto = intrinsic(IntrinsicId::ArrayPrototype);
        let obj = super::super::array_create(0, Some(proto)).unwrap();
        define_property_or_throw(
            &obj,
            "constructor",
            PotentialPropertyDescriptor {
                value: Some(ECMAScriptValue::Undefined),
                writable: Some(true),
                enumerable: Some(true),
                configurable: Some(true),
                ..Default::default()
            },
        )
        .unwrap();
        obj
    }
    fn make_plain_array() -> Object {
        let proto = intrinsic(IntrinsicId::ArrayPrototype);
        super::super::array_create(5, Some(proto)).unwrap()
    }
    fn make_primitive_constructor_prop() -> Object {
        let proto = intrinsic(IntrinsicId::ArrayPrototype);
        let obj = super::super::array_create(0, Some(proto)).unwrap();
        define_property_or_throw(
            &obj,
            "constructor",
            PotentialPropertyDescriptor {
                value: Some(ECMAScriptValue::from(false)),
                writable: Some(true),
                enumerable: Some(true),
                configurable: Some(true),
                ..Default::default()
            },
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
    fn f(make_original: fn() -> Object, length: u64) -> Result<Vec<PropertyInfo>, String> {
        setup_test_agent();
        let original = make_original();
        array_species_create(&original, length)
            .map(|val| Object::try_from(val).unwrap().o.common_object_data().borrow().propdump())
            .map_err(unwind_any_error)
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
    setup_test_agent();
    let a = super::array_create(10, None).unwrap();
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
fn todo(f: fn(ECMAScriptValue, Option<&Object>, &[ECMAScriptValue]) -> Completion<ECMAScriptValue>) {
    setup_test_agent();
    f(ECMAScriptValue::Undefined, None, &[]).unwrap();
}
