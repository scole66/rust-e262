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

    fn make() -> Object {
        let o = ArrayObject::create(0, None).unwrap();
        let proto = o.o.get_prototype_of().unwrap().unwrap();
        set(&proto, "proto_sentinel".into(), true.into(), true).unwrap();
        o
    }

    default_uses_ordinary_get_prototype_of_test!();
    default_get_prototype_of_test!(ArrayPrototype);
    default_set_prototype_of_test!();
    default_is_extensible_test!();
    default_prevent_extensions_test!();
    default_get_own_property_test!();

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

    default_has_property_test!();
    default_get_test!(|| PropertyKey::from("proto_sentinel"), ECMAScriptValue::from(true));
    #[test]
    fn set_() {
        setup_test_agent();
        let a = ArrayObject::create(0, None).unwrap();
        let receiver: ECMAScriptValue = a.clone().into();
        let success = a.o.set("length".into(), 100.into(), &receiver).unwrap();
        assert!(success);
        assert_eq!(a.o.get(&"length".into(), &receiver).unwrap(), 100.into());
    }
    default_delete_test!();
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
    none_function!(to_symbol_obj);
    false_function!(is_plain_object);
    none_function!(to_string_obj);
    none_function!(to_generator_object);
    false_function!(is_generator_object);
    none_function!(to_arguments_object);
    false_function!(is_symbol_object);

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

#[test_case(super::array_from => panics; "array_from")]
#[test_case(super::array_is_array => panics; "array_is_array")]
#[test_case(super::array_of => panics; "array_of")]
#[test_case(super::array_species => panics; "array_species")]
#[test_case(super::array_prototype_at => panics; "array_prototype_at")]
#[test_case(super::array_prototype_concat => panics; "array_prototype_concat")]
#[test_case(super::array_prototype_copy_within => panics; "array_prototype_copy_within")]
#[test_case(super::array_prototype_entries => panics; "array_prototype_entries")]
#[test_case(super::array_prototype_every => panics; "array_prototype_every")]
#[test_case(super::array_prototype_fill => panics; "array_prototype_fill")]
#[test_case(super::array_prototype_filter => panics; "array_prototype_filter")]
#[test_case(super::array_prototype_find => panics; "array_prototype_find")]
#[test_case(super::array_prototype_find_index => panics; "array_prototype_find_index")]
#[test_case(super::array_prototype_find_last => panics; "array_prototype_find_last")]
#[test_case(super::array_prototype_find_last_index => panics; "array_prototype_find_last_index")]
#[test_case(super::array_prototype_flat => panics; "array_prototype_flat")]
#[test_case(super::array_prototype_flat_map => panics; "array_prototype_flat_map")]
#[test_case(super::array_prototype_for_each => panics; "array_prototype_for_each")]
#[test_case(super::array_prototype_includes => panics; "array_prototype_includes")]
#[test_case(super::array_prototype_index_of => panics; "array_prototype_index_of")]
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
#[test_case(super::array_prototype_to_reversed => panics; "array_prototype_to_reversed")]
#[test_case(super::array_prototype_to_sorted => panics; "array_prototype_to_sorted")]
#[test_case(super::array_prototype_to_spliced => panics; "array_prototype_to_spliced")]
#[test_case(super::array_prototype_unshift => panics; "array_prototype_unshift")]
#[test_case(super::array_prototype_with => panics; "array_prototype_with")]
fn todo(f: fn(ECMAScriptValue, Option<&Object>, &[ECMAScriptValue]) -> Completion<ECMAScriptValue>) {
    setup_test_agent();
    f(ECMAScriptValue::Undefined, None, &[]).unwrap();
}

#[test]
fn provision_array_intrinsic() {
    setup_test_agent();
    // Just setting up the test agent will complete coverage, so we're really just checking the result.

    let array = intrinsic(IntrinsicId::Array);
    let global = get_global_object().unwrap();
    let global_array = global.o.get_own_property(&"Array".into()).unwrap().unwrap();
    let arrayfcn = Object::try_from(func_validation(global_array, "Array", 1)).unwrap();
    assert_eq!(arrayfcn, array);

    let from = array.o.get_own_property(&"from".into()).unwrap().unwrap();
    func_validation(from, "from", 1);

    let is_array = array.o.get_own_property(&"isArray".into()).unwrap().unwrap();
    func_validation(is_array, "isArray", 1);

    let of = array.o.get_own_property(&"of".into()).unwrap().unwrap();
    func_validation(of, "of", 0);

    let species_sym = wks(WksId::Species);
    let species = array.o.get_own_property(&species_sym.into()).unwrap().unwrap();
    getter_validation(species, "get [Symbol.species]", 0);

    let prototype_pd = array.o.get_own_property(&"prototype".into()).unwrap().unwrap();
    let proto_intrinsic = intrinsic(IntrinsicId::ArrayPrototype);
    let prototype = Object::try_from(data_validation(prototype_pd, false, false, false)).unwrap();
    assert_eq!(proto_intrinsic, prototype);
    assert!(prototype.o.is_array_object());
    let proto_proto = prototype.o.get_prototype_of().unwrap().unwrap();
    assert_eq!(proto_proto, intrinsic(IntrinsicId::ObjectPrototype));

    let length_pd = prototype.o.get_own_property(&"length".into()).unwrap().unwrap();
    let length = data_validation(length_pd, true, false, false);
    assert_eq!(length, 0.0.into());

    func_validation(prototype.o.get_own_property(&"at".into()).unwrap().unwrap(), "at", 1);
    func_validation(prototype.o.get_own_property(&"concat".into()).unwrap().unwrap(), "concat", 1);
    func_validation(prototype.o.get_own_property(&"copyWithin".into()).unwrap().unwrap(), "copyWithin", 2);
    func_validation(prototype.o.get_own_property(&"entries".into()).unwrap().unwrap(), "entries", 0);
    func_validation(prototype.o.get_own_property(&"every".into()).unwrap().unwrap(), "every", 1);
    func_validation(prototype.o.get_own_property(&"fill".into()).unwrap().unwrap(), "fill", 1);
    func_validation(prototype.o.get_own_property(&"filter".into()).unwrap().unwrap(), "filter", 1);
    func_validation(prototype.o.get_own_property(&"find".into()).unwrap().unwrap(), "find", 1);
    func_validation(prototype.o.get_own_property(&"findIndex".into()).unwrap().unwrap(), "findIndex", 1);
    func_validation(prototype.o.get_own_property(&"findLast".into()).unwrap().unwrap(), "findLast", 1);
    func_validation(prototype.o.get_own_property(&"findLastIndex".into()).unwrap().unwrap(), "findLastIndex", 1);
    func_validation(prototype.o.get_own_property(&"flat".into()).unwrap().unwrap(), "flat", 0);
    func_validation(prototype.o.get_own_property(&"flatMap".into()).unwrap().unwrap(), "flatMap", 1);
    func_validation(prototype.o.get_own_property(&"forEach".into()).unwrap().unwrap(), "forEach", 1);
    func_validation(prototype.o.get_own_property(&"includes".into()).unwrap().unwrap(), "includes", 1);
    func_validation(prototype.o.get_own_property(&"indexOf".into()).unwrap().unwrap(), "indexOf", 1);
    func_validation(prototype.o.get_own_property(&"join".into()).unwrap().unwrap(), "join", 1);
    func_validation(prototype.o.get_own_property(&"keys".into()).unwrap().unwrap(), "keys", 0);
    func_validation(prototype.o.get_own_property(&"lastIndexOf".into()).unwrap().unwrap(), "lastIndexOf", 1);
    func_validation(prototype.o.get_own_property(&"map".into()).unwrap().unwrap(), "map", 1);
    func_validation(prototype.o.get_own_property(&"pop".into()).unwrap().unwrap(), "pop", 0);
    func_validation(prototype.o.get_own_property(&"push".into()).unwrap().unwrap(), "push", 1);
    func_validation(prototype.o.get_own_property(&"reduce".into()).unwrap().unwrap(), "reduce", 1);
    func_validation(prototype.o.get_own_property(&"reduceRight".into()).unwrap().unwrap(), "reduceRight", 1);
    func_validation(prototype.o.get_own_property(&"reverse".into()).unwrap().unwrap(), "reverse", 0);
    func_validation(prototype.o.get_own_property(&"shift".into()).unwrap().unwrap(), "shift", 0);
    func_validation(prototype.o.get_own_property(&"slice".into()).unwrap().unwrap(), "slice", 2);
    func_validation(prototype.o.get_own_property(&"some".into()).unwrap().unwrap(), "some", 1);
    func_validation(prototype.o.get_own_property(&"sort".into()).unwrap().unwrap(), "sort", 1);
    func_validation(prototype.o.get_own_property(&"splice".into()).unwrap().unwrap(), "splice", 2);
    func_validation(prototype.o.get_own_property(&"toLocaleString".into()).unwrap().unwrap(), "toLocaleString", 0);
    func_validation(prototype.o.get_own_property(&"toReversed".into()).unwrap().unwrap(), "toReversed", 0);
    func_validation(prototype.o.get_own_property(&"toSorted".into()).unwrap().unwrap(), "toSorted", 1);
    func_validation(prototype.o.get_own_property(&"toSpliced".into()).unwrap().unwrap(), "toSpliced", 2);
    func_validation(prototype.o.get_own_property(&"toString".into()).unwrap().unwrap(), "toString", 0);
    func_validation(prototype.o.get_own_property(&"unshift".into()).unwrap().unwrap(), "unshift", 1);
    let values = func_validation(prototype.o.get_own_property(&"values".into()).unwrap().unwrap(), "values", 0);
    func_validation(prototype.o.get_own_property(&"with".into()).unwrap().unwrap(), "with", 2);

    let constructor_pd = prototype.o.get_own_property(&"constructor".into()).unwrap().unwrap();
    let constructor = data_validation(constructor_pd, true, false, true);
    assert_eq!(constructor, array.into());

    let iter =
        func_validation(prototype.o.get_own_property(&wks(WksId::Iterator).into()).unwrap().unwrap(), "values", 0);
    assert_eq!(iter, values);
    assert_eq!(iter, ECMAScriptValue::from(intrinsic(IntrinsicId::ArrayPrototypeValues)));

    // todo: unscopables
}

#[test]
fn provision_array_iterator_intrinsic() {
    setup_test_agent();
    // Just setting up the test agent will complete coverage, so we're really just checking the result.
    let aiproto = intrinsic(IntrinsicId::ArrayIteratorPrototype);
    let tostringtag_symbol = wks(WksId::ToStringTag);
    let iterator_prototype = intrinsic(IntrinsicId::IteratorPrototype);

    assert_eq!(aiproto.o.get_prototype_of().unwrap().unwrap(), iterator_prototype);

    let tst = aiproto.o.get_own_property(&tostringtag_symbol.into()).unwrap().unwrap();
    let value = data_validation(tst, false, false, true);
    assert_eq!(value, "Array Iterator".into());

    let next = aiproto.o.get_own_property(&"next".into()).unwrap().unwrap();
    func_validation(next, "next", 0);
}

mod key_value_kind {
    use super::*;
    use test_case::test_case;

    #[test_case(KeyValueKind::Key => with |s| assert_ne!(s, ""))]
    #[test_case(KeyValueKind::Value => with |s| assert_ne!(s, ""))]
    #[test_case(KeyValueKind::KeyValue => with |s| assert_ne!(s, ""))]
    fn fmt(item: KeyValueKind) -> String {
        format!("{item:?}")
    }

    #[test]
    #[allow(clippy::clone_on_copy)]
    fn clone() {
        let item = KeyValueKind::Key;
        let other = item.clone();

        assert_eq!(item, other);
    }

    #[test_case(KeyValueKind::Key, KeyValueKind::Key => true)]
    #[test_case(KeyValueKind::Value, KeyValueKind::Key => false)]
    #[test_case(KeyValueKind::KeyValue, KeyValueKind::Key => false)]
    fn eq(a: KeyValueKind, b: KeyValueKind) -> bool {
        a.eq(&b)
    }
}

#[test_case(|| ECMAScriptValue::Undefined,
            || ECMAScriptValue::Undefined
            => serr("TypeError: Undefined and null cannot be converted to objects")
            ; "ToObject throws")]
#[test_case(|| {
                   let obj = ordinary_object_create(None, &[]);
                   let sym = wks(WksId::Unscopables);
                   create_data_property_or_throw(&obj, "length", sym).unwrap();
                   ECMAScriptValue::from(obj)
               },
            || ECMAScriptValue::Undefined
            => serr("TypeError: Symbol values cannot be converted to Number values")
            ; "length_of_array_like throws")]
#[test_case(|| ECMAScriptValue::from(super::super::array_create(0, None).unwrap()),
            || ECMAScriptValue::from(wks(WksId::Unscopables))
            => serr("TypeError: Symbols may not be converted to strings")
            ; "ToString fails")]
#[test_case(|| ECMAScriptValue::from(super::super::array_create(0, None).unwrap()),
            || ECMAScriptValue::Undefined
            => vok("")
            ; "empty array")]
#[test_case(|| ECMAScriptValue::from(
                create_array_from_list(
                    &[
                        ECMAScriptValue::from("first"),
                        ECMAScriptValue::Undefined,
                        ECMAScriptValue::from("second"),
                        ECMAScriptValue::Null,
                        ECMAScriptValue::from("third"),
                    ])),
            || ECMAScriptValue::from("-=-")
            => vok("first-=--=-second-=--=-third")
            ; "array with 5 elems")]
#[test_case(|| ECMAScriptValue::from(
                create_array_from_list(&[ECMAScriptValue::from(wks(WksId::Unscopables))])),
            || ECMAScriptValue::from("-=-")
            => serr("TypeError: Symbols may not be converted to strings")
            ; "symbol in elements")]
#[test_case(|| {
                    let obj = super::super::array_create(10, None).unwrap();
                    let thrower = intrinsic(IntrinsicId::ThrowTypeError);
                    let ppd = PotentialPropertyDescriptor::new()
                        .get(ECMAScriptValue::from(thrower.clone()))
                        .set(ECMAScriptValue::from(thrower))
                        .enumerable(false)
                        .configurable(false);
                    define_property_or_throw(&obj, "3", ppd).unwrap();
                    ECMAScriptValue::from(obj)
               },
            || ECMAScriptValue::Undefined
            => serr("TypeError: Generic TypeError")
            ; "bad element")]
fn array_prototype_join(
    make_this: impl FnOnce() -> ECMAScriptValue,
    make_sep: impl FnOnce() -> ECMAScriptValue,
) -> Result<ECMAScriptValue, String> {
    setup_test_agent();
    let this_value = make_this();
    let sep = make_sep();
    super::array_prototype_join(this_value, None, &[sep]).map_err(unwind_any_error)
}

#[test_case(|| ECMAScriptValue::Undefined
            => serr("TypeError: Undefined and null cannot be converted to objects")
            ; "ToObject throws")]
#[test_case(|| ECMAScriptValue::from(create_array_from_list(&[1.into(), 2.into(), 3.into()]))
            => vok("1,2,3")
            ; "std array")]
#[test_case(|| ECMAScriptValue::from(DeadObject::object())
            => serr("TypeError: get called on DeadObject")
            ; "get throws")]
#[test_case(|| ECMAScriptValue::from(ordinary_object_create(None, &[]))
            => vok("[object Object]")
            ; "lacking join")]
fn array_prototype_to_string(make_this: impl FnOnce() -> ECMAScriptValue) -> Result<ECMAScriptValue, String> {
    setup_test_agent();
    let this_value = make_this();
    super::array_prototype_to_string(this_value, None, &[]).map_err(unwind_any_error)
}

mod array_iterator {
    use super::super::array_create;
    use super::super::create_array_from_list;
    use super::*;
    use test_case::test_case;

    #[test_case(|| create_array_from_list(&[1.into(), 2.into()]), KeyValueKind::Key => Ok(vec![0.into(), 1.into()]); "keys")]
    #[test_case(|| create_array_from_list(&[1.into(), 2.into()]), KeyValueKind::Value => Ok(vec![1.into(), 2.into()]); "values")]
    #[test_case(|| create_array_from_list(&[1.into(), 2.into()]), KeyValueKind::KeyValue => Ok(vec![0.into(), 1.into(), 1.into(), 2.into()]); "key values")]
    #[test_case(DeadObject::object, KeyValueKind::Key => serr("TypeError: get called on DeadObject"); "LengthOfArrayLike throws")]
    #[test_case(|| {
                       let obj = array_create(10, None).unwrap();
                       let thrower = intrinsic(IntrinsicId::ThrowTypeError);
                       let ppd = PotentialPropertyDescriptor::new()
                           .get(ECMAScriptValue::from(thrower.clone()))
                           .set(ECMAScriptValue::from(thrower))
                           .enumerable(false)
                           .configurable(false);
                       define_property_or_throw(&obj, "1", ppd).unwrap();
                       obj
                   },
                KeyValueKind::Value
                => serr("TypeError: Generic TypeError")
                ; "element 1 can't be gotten")]
    #[test_case(|| create_array_from_list(&[1.into(), 2.into(), 99.into(), 100.into()]), KeyValueKind::Key => serr("TypeError: thrown from generator"); "keys/throw")]
    #[test_case(|| create_array_from_list(&[1.into(), 2.into(), 99.into(), 100.into()]), KeyValueKind::Value => serr("TypeError: thrown from generator"); "values/throw")]
    #[test_case(|| create_array_from_list(&[1.into(), 2.into(), 99.into(), 100.into()]), KeyValueKind::KeyValue => serr("TypeError: thrown from generator"); "key values/throw")]
    fn standard(make_array: impl FnOnce() -> Object, kind: KeyValueKind) -> Result<Vec<ECMAScriptValue>, String> {
        setup_test_agent();
        let array = make_array();

        let iter_obj = super::create_array_iterator(array, kind);
        let thrower =
            create_builtin_function(throwing_next, false, 0.0, "next".into(), BUILTIN_FUNCTION_SLOTS, None, None, None);
        set(&iter_obj, "next".into(), thrower.into(), true).map_err(unwind_any_error)?;
        let ir = get_iterator(&ECMAScriptValue::from(iter_obj), IteratorKind::Sync).map_err(unwind_any_error)?;
        let mut result = vec![];
        loop {
            let item = ir.step().map_err(unwind_any_error)?;
            match item {
                Some(iter_result) => {
                    if kind != KeyValueKind::KeyValue {
                        result.push(iterator_value(&iter_result).map_err(unwind_any_error)?);
                    } else {
                        let pair = iterator_value(&iter_result).map_err(unwind_any_error)?;
                        let left = getv(&pair, &"0".into()).map_err(unwind_any_error)?;
                        let right = getv(&pair, &"1".into()).map_err(unwind_any_error)?;
                        result.push(left);
                        result.push(right);
                    }
                }
                None => break,
            }
        }
        Ok(result)
    }

    fn throwing_next(
        this_value: ECMAScriptValue,
        _: Option<&Object>,
        _: &[ECMAScriptValue],
    ) -> Completion<ECMAScriptValue> {
        let obj = to_object(this_value.clone())?;
        let so_far = get(&obj, &"called_count".into())?;
        let so_far = if so_far.is_undefined() { 0.0 } else { to_number(so_far)? };

        let result = if so_far < 3.0 {
            generator_resume(this_value, ECMAScriptValue::Undefined, "%ArrayIteratorPrototype%")?
        } else {
            generator_resume_abrupt(this_value, create_type_error("thrown from generator"), "%ArrayIteratorPrototype%")?
        };
        set(&obj, "called_count".into(), ECMAScriptValue::from(so_far + 1.0), true)?;
        Ok(result)
    }
}

#[test_case(|| ECMAScriptValue::from(create_array_from_list(&[1.into(), 2.into()])) => Ok(vec![1.into(), 2.into()]); "normal")]
#[test_case(|| ECMAScriptValue::Undefined => serr("TypeError: Undefined and null cannot be converted to objects"); "not an object")]
fn array_prototype_values(make_this: impl FnOnce() -> ECMAScriptValue) -> Result<Vec<ECMAScriptValue>, String> {
    setup_test_agent();
    let this_value = make_this();
    let iter = super::array_prototype_values(this_value, None, &[]).map_err(unwind_any_error)?;
    let ir = get_iterator(&iter, IteratorKind::Sync).map_err(unwind_any_error)?;
    let mut result = vec![];
    loop {
        let item = ir.step().map_err(unwind_any_error)?;
        match item {
            Some(iter_result) => {
                result.push(iterator_value(&iter_result).map_err(unwind_any_error)?);
            }
            None => break,
        }
    }
    Ok(result)
}

mod array_constructor_function {
    use super::*;
    use test_case::test_case;

    #[test_case(|| Some(intrinsic(IntrinsicId::Array)), Vec::new => Ok(vec![]); "zero args")]
    #[test_case(|| Some(intrinsic(IntrinsicId::Array)), || vec![ECMAScriptValue::from(3)] => Ok(vec![ECMAScriptValue::Undefined, ECMAScriptValue::Undefined, ECMAScriptValue::Undefined]); "one numeric arg")]
    #[test_case(|| Some(intrinsic(IntrinsicId::Array)), || vec![ECMAScriptValue::from("three")] => Ok(vec![ECMAScriptValue::from("three")]); "one non-numeric arg")]
    #[test_case(|| Some(intrinsic(IntrinsicId::Array)), || vec![ECMAScriptValue::from(128.5)] => serr("RangeError: Bad length in array construction"); "one non-integer number arg")]
    #[test_case(|| Some(intrinsic(IntrinsicId::Array)), || vec![ECMAScriptValue::from("bob"), ECMAScriptValue::from(true), ECMAScriptValue::from(99)] => Ok(vec![ECMAScriptValue::from("bob"), ECMAScriptValue::from(true), ECMAScriptValue::from(99)]); "multiple args")]
    #[test_case(|| {
                let array_function = Some(intrinsic(IntrinsicId::Array));
                AGENT.with(|agent| {
                    let mut stack = agent.execution_context_stack.borrow_mut();
                    let mut ec = stack.last_mut().unwrap();
                    ec.function = array_function;
                });
                None
               },
            || vec![ECMAScriptValue::from("three")] => Ok(vec![ECMAScriptValue::from("three")]); "called as func, not constructor")]
    #[test_case(|| {
                let func_proto = intrinsic(IntrinsicId::FunctionPrototype);
                let obj = ordinary_object_create(Some(func_proto), &[]);
                let thrower = intrinsic(IntrinsicId::ThrowTypeError);
                let desc = PotentialPropertyDescriptor::new().get(thrower);
                define_property_or_throw(&obj, "prototype", desc).unwrap();
                Some(obj)
            },
            Vec::new => serr("TypeError: Generic TypeError"); "get-proto-from-constructor fails")]
    fn call(
        make_nt: impl FnOnce() -> Option<Object>,
        make_arguments: impl FnOnce() -> Vec<ECMAScriptValue>,
    ) -> Result<Vec<ECMAScriptValue>, String> {
        setup_test_agent();
        let nt = make_nt();
        let args = make_arguments();

        let array = super::array_constructor_function(ECMAScriptValue::Undefined, nt.as_ref(), &args)
            .map_err(unwind_any_error)?;
        let array = Object::try_from(array).unwrap();
        let mut result = vec![];
        let length = f64::try_from(get(&array, &"length".into()).unwrap()).unwrap();

        for x in 0..(length as usize) {
            let pk = format!("{x}");
            let item = get(&array, &pk.into()).unwrap();
            result.push(item);
        }

        Ok(result)
    }
}
