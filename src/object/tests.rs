use super::*;
use crate::testhelp::*;
use crate::tests::*;
use itertools::Itertools;
use lazy_static::lazy_static;
use regex::Regex;
use std::io::Write;
use test_case::test_case;

#[test]
fn data_property_debug() {
    let dp = DataProperty { value: ECMAScriptValue::Null, writable: true };
    assert_ne!(format!("{dp:?}"), "");
}
#[test]
fn data_property_eq() {
    let items: [DataProperty; 4] = [
        DataProperty { value: ECMAScriptValue::from("blue"), writable: true },
        DataProperty { value: ECMAScriptValue::from("blue"), writable: false },
        DataProperty { value: ECMAScriptValue::from(true), writable: true },
        DataProperty { value: ECMAScriptValue::from(33), writable: false },
    ];
    for (right_idx, right_value) in items.iter().enumerate() {
        for (left_idx, left_value) in items.iter().enumerate() {
            assert_eq!(*left_value == *right_value, left_idx == right_idx);
            assert_eq!(*left_value != *right_value, left_idx != right_idx);
        }
    }
}
#[test]
#[allow(clippy::redundant_clone)]
fn data_property_clone() {
    let p1 = DataProperty { value: ECMAScriptValue::from("blue"), writable: true };
    let p2 = p1.clone();
    assert_eq!(p1, p2);
}
#[test]
fn data_property_default() {
    let p: DataProperty = Default::default();
    assert_eq!(p, DataProperty { value: ECMAScriptValue::Undefined, writable: false });
}

#[test]
fn accessor_property_debug() {
    let p1 = AccessorProperty { get: ECMAScriptValue::Undefined, set: ECMAScriptValue::Undefined };
    assert_ne!(format!("{p1:?}"), "");
}
#[test]
fn accessor_property_eq() {
    let items: [AccessorProperty; 4] = [
        AccessorProperty { get: ECMAScriptValue::from(55), set: ECMAScriptValue::from("movies") },
        AccessorProperty { get: ECMAScriptValue::from(55), set: ECMAScriptValue::Undefined },
        AccessorProperty { get: ECMAScriptValue::Undefined, set: ECMAScriptValue::from("movies") },
        AccessorProperty { get: ECMAScriptValue::Undefined, set: ECMAScriptValue::Undefined },
    ];
    for (right_idx, right_value) in items.iter().enumerate() {
        for (left_idx, left_value) in items.iter().enumerate() {
            assert_eq!(*left_value == *right_value, left_idx == right_idx);
            assert_eq!(*left_value != *right_value, left_idx != right_idx);
        }
    }
}
#[test]
#[allow(clippy::redundant_clone)]
fn accessor_property_clone() {
    let p1 = AccessorProperty { get: ECMAScriptValue::from(10), set: ECMAScriptValue::from("a") };
    let p2 = p1.clone();
    assert_eq!(p1, p2);
}
#[test]
fn accessor_property_default() {
    let p: AccessorProperty = Default::default();
    assert_eq!(p, AccessorProperty { get: ECMAScriptValue::Undefined, set: ECMAScriptValue::Undefined });
}

#[test]
fn property_kind_debug() {
    let pk = PropertyKind::Data(DataProperty { value: ECMAScriptValue::Null, writable: false });
    assert_ne!(format!("{pk:?}"), "");
}
#[test]
fn property_kind_eq() {
    let items: [PropertyKind; 4] = [
        PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(true), writable: true }),
        PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(true), writable: false }),
        PropertyKind::Accessor(AccessorProperty {
            get: ECMAScriptValue::Undefined,
            set: ECMAScriptValue::from("movies"),
        }),
        PropertyKind::Accessor(AccessorProperty { get: ECMAScriptValue::Undefined, set: ECMAScriptValue::Undefined }),
    ];
    for (right_idx, right_value) in items.iter().enumerate() {
        for (left_idx, left_value) in items.iter().enumerate() {
            assert_eq!(*left_value == *right_value, left_idx == right_idx);
            assert_eq!(*left_value != *right_value, left_idx != right_idx);
        }
    }
}
#[test]
#[allow(clippy::redundant_clone)]
fn property_kind_clone() {
    let pk1 = PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(true), writable: true });
    let pk2 = pk1.clone();
    assert_eq!(pk1, pk2);
}
#[test]
fn property_kind_default() {
    let def: PropertyKind = Default::default();
    let expected = PropertyKind::Data(Default::default());
    assert_eq!(def, expected);
}

mod property_descriptor {
    use super::*;

    #[test]
    fn debug() {
        let p = PropertyDescriptor {
            property: PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(true), writable: false }),
            enumerable: true,
            configurable: false,
            spot: 10,
        };
        assert_ne!(format!("{p:?}"), "");
    }
    #[test]
    fn eq() {
        let items: [PropertyDescriptor; 6] = [
            PropertyDescriptor {
                property: PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(true), writable: false }),
                enumerable: true,
                configurable: false,
                spot: 10,
            },
            PropertyDescriptor {
                property: PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(false), writable: false }),
                enumerable: true,
                configurable: false,
                spot: 10,
            },
            PropertyDescriptor {
                property: PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(true), writable: false }),
                enumerable: false,
                configurable: false,
                spot: 10,
            },
            PropertyDescriptor {
                property: PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(true), writable: false }),
                enumerable: true,
                configurable: true,
                spot: 10,
            },
            PropertyDescriptor {
                property: PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(true), writable: false }),
                enumerable: true,
                configurable: false,
                spot: 1,
            },
            PropertyDescriptor {
                property: PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(false), writable: true }),
                enumerable: false,
                configurable: true,
                spot: 1,
            },
        ];
        for (right_idx, right_value) in items.iter().enumerate() {
            for (left_idx, left_value) in items.iter().enumerate() {
                assert_eq!(*left_value == *right_value, left_idx == right_idx);
                assert_eq!(*left_value != *right_value, left_idx != right_idx);
            }
        }
    }
    #[test]
    #[allow(clippy::redundant_clone)]
    fn clone() {
        let p1 = PropertyDescriptor {
            property: PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(true), writable: false }),
            enumerable: true,
            configurable: true,
            spot: 10,
        };
        let p2 = p1.clone();
        assert_eq!(p1, p2);
    }
    #[test]
    fn default() {
        let pd: PropertyDescriptor = Default::default();
        let expected =
            PropertyDescriptor { property: Default::default(), enumerable: false, configurable: false, spot: 0 };
        assert_eq!(pd, expected);
    }
    #[test]
    fn is_data_descriptor() {
        let d = PropertyDescriptor {
            property: PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(true), writable: false }),
            enumerable: true,
            configurable: true,
            spot: 10,
        };
        let a = PropertyDescriptor {
            property: PropertyKind::Accessor(AccessorProperty {
                get: ECMAScriptValue::Undefined,
                set: ECMAScriptValue::Undefined,
            }),
            enumerable: true,
            configurable: true,
            spot: 10,
        };

        assert!(d.is_data_descriptor());
        assert!(!a.is_data_descriptor());
    }
    #[test]
    fn is_accessor_descriptor() {
        let d = PropertyDescriptor {
            property: PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(true), writable: false }),
            enumerable: true,
            configurable: true,
            spot: 10,
        };
        let a = PropertyDescriptor {
            property: PropertyKind::Accessor(AccessorProperty {
                get: ECMAScriptValue::Undefined,
                set: ECMAScriptValue::Undefined,
            }),
            enumerable: true,
            configurable: true,
            spot: 10,
        };

        assert!(!d.is_accessor_descriptor());
        assert!(a.is_accessor_descriptor());
    }
    #[test]
    fn is_generic_descriptor() {
        let d = PropertyDescriptor {
            property: PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(true), writable: false }),
            enumerable: true,
            configurable: true,
            spot: 10,
        };
        let a = PropertyDescriptor {
            property: PropertyKind::Accessor(AccessorProperty {
                get: ECMAScriptValue::Undefined,
                set: ECMAScriptValue::Undefined,
            }),
            enumerable: true,
            configurable: true,
            spot: 10,
        };

        assert!(!d.is_generic_descriptor());
        assert!(!a.is_generic_descriptor());
    }
    #[test]
    fn is_writable() {
        let writable = PropertyDescriptor {
            property: PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(true), writable: true }),
            enumerable: true,
            configurable: true,
            spot: 10,
        };
        let constant = PropertyDescriptor {
            property: PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(true), writable: false }),
            enumerable: true,
            configurable: true,
            spot: 10,
        };
        let accessor = PropertyDescriptor {
            property: PropertyKind::Accessor(AccessorProperty {
                get: ECMAScriptValue::Undefined,
                set: ECMAScriptValue::Undefined,
            }),
            enumerable: true,
            configurable: true,
            spot: 10,
        };

        assert_eq!(writable.is_writable(), Some(true));
        assert_eq!(constant.is_writable(), Some(false));
        assert_eq!(accessor.is_writable(), None);
    }

    mod from {
        use super::*;
        use test_case::test_case;

        #[derive(PartialEq, Debug)]
        struct TestResult {
            value: Option<String>,
            writable: Option<bool>,
            get: Option<String>,
            set: Option<String>,
            enumerable: bool,
            configurable: bool,
        }

        #[test_case(PotentialPropertyDescriptor::new => serr("incomplete descriptor"); "empty ppd")]
        #[test_case(|| PotentialPropertyDescriptor::new().writable(true).value(1).enumerable(true).configurable(true) => Ok(TestResult{
            value: Some("1".to_string()), writable: Some(true), get: None, set: None, enumerable: true, configurable: true
        }); "data descriptor")]
        #[test_case(|| PotentialPropertyDescriptor::new().writable(true).enumerable(true).configurable(true) => serr("incomplete descriptor"); "value missing")]
        #[test_case(|| PotentialPropertyDescriptor::new().value(true).enumerable(true).configurable(true) => serr("incomplete descriptor"); "writable missing")]
        #[test_case(|| PotentialPropertyDescriptor::new().value(true).writable(true).configurable(true) => serr("incomplete descriptor"); "enumerable missing")]
        #[test_case(|| PotentialPropertyDescriptor::new().value(true).writable(true).enumerable(true) => serr("incomplete descriptor"); "configurable missing")]
        #[test_case(|| PotentialPropertyDescriptor::new().get(intrinsic(IntrinsicId::ThrowTypeError)).set(intrinsic(IntrinsicId::ThrowTypeError)).enumerable(true).configurable(true) =>
        Ok(TestResult {
            value: None, writable: None, get: Some(String::from("length:0,name:")), set: Some(String::from("length:0,name:")), enumerable: true, configurable: true
        }); "accessor descriptor")]
        #[test_case(|| PotentialPropertyDescriptor::new().set(intrinsic(IntrinsicId::ThrowTypeError)).enumerable(true).configurable(true) => serr("incomplete descriptor"); "get missing")]
        #[test_case(|| PotentialPropertyDescriptor::new().get(intrinsic(IntrinsicId::ThrowTypeError)).enumerable(true).configurable(true) => serr("incomplete descriptor"); "set missing")]
        fn potential_property_descriptor(
            maker: impl FnOnce() -> PotentialPropertyDescriptor,
        ) -> Result<TestResult, String> {
            setup_test_agent();
            PropertyDescriptor::try_from(maker()).map_err(|e| e.to_string()).map(|pd| {
                let (value, writable, get, set) = match pd.property {
                    PropertyKind::Data(DataProperty { value, writable }) => (Some(value), Some(writable), None, None),
                    PropertyKind::Accessor(AccessorProperty { get, set }) => (None, None, Some(get), Some(set)),
                };
                TestResult {
                    value: value.map(|x| x.test_result_string()),
                    writable,
                    get: get.map(|x| x.test_result_string()),
                    set: set.map(|x| x.test_result_string()),
                    configurable: pd.configurable,
                    enumerable: pd.enumerable,
                }
            })
        }
    }
}

#[test]
fn concise_property_descriptor_from() {
    let p = PropertyDescriptor {
        property: PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(true), writable: true }),
        enumerable: true,
        configurable: true,
        spot: 10,
    };
    let c = ConcisePropertyDescriptor::from(&p);
    assert_eq!(*c.0, p);
}
#[test]
fn concise_property_descriptor_debug() {
    let p1 = PropertyDescriptor {
        property: PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(true), writable: true }),
        enumerable: true,
        configurable: true,
        spot: 10,
    };
    let c1 = ConcisePropertyDescriptor::from(&p1);
    assert_eq!(format!("{c1:?}"), "{ true wec }");
    printer_validate(|w| write!(w, "{c1:?}"));
    let p2 = PropertyDescriptor {
        property: PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(true), writable: false }),
        enumerable: false,
        configurable: false,
        spot: 10,
    };
    let c2 = ConcisePropertyDescriptor::from(&p2);
    assert_eq!(format!("{c2:?}"), "{ true --- }");
    printer_validate(|w| write!(w, "{c2:?}"));
    let p3 = PropertyDescriptor {
        property: PropertyKind::Accessor(AccessorProperty {
            get: ECMAScriptValue::from(true),
            set: ECMAScriptValue::Undefined,
        }),
        enumerable: false,
        configurable: true,
        spot: 10,
    };
    let c3 = ConcisePropertyDescriptor::from(&p3);
    assert_eq!(format!("{c3:?}"), "{ [[Get]]: true [[Set]]: undefined -c }");
    printer_validate(|w| write!(w, "{c3:?}"));
}

mod concise_properties {
    use super::*;

    #[test]
    fn debug() {
        let pd = PropertyDescriptor {
            property: PropertyKind::Data(DataProperty { value: 10.into(), writable: true }),
            enumerable: true,
            configurable: true,
            spot: 10,
        };
        let key = PropertyKey::from("alice");
        let mut map: AHashMap<PropertyKey, PropertyDescriptor> = AHashMap::new();
        map.insert(key, pd);
        let item = ConciseProperties::from(&map);

        assert_ne!(format!("{item:?}"), "");
    }
}

mod potential_property_descriptor {
    use super::*;
    use test_case::test_case;

    #[test]
    fn debug() {
        let ppd: PotentialPropertyDescriptor = Default::default();
        assert_ne!(format!("{ppd:?}"), "");
    }
    #[test]
    fn default() {
        let ppd: PotentialPropertyDescriptor = Default::default();
        assert_eq!(
            ppd,
            PotentialPropertyDescriptor {
                value: None,
                writable: None,
                get: None,
                set: None,
                enumerable: None,
                configurable: None
            }
        );
    }
    #[test]
    fn partialeq() {
        let items: [PotentialPropertyDescriptor; 8] = [
            PotentialPropertyDescriptor {
                value: Some(ECMAScriptValue::from(true)),
                writable: Some(true),
                get: None,
                set: None,
                enumerable: Some(true),
                configurable: Some(true),
            },
            PotentialPropertyDescriptor {
                value: None,
                writable: Some(true),
                get: None,
                set: None,
                enumerable: Some(true),
                configurable: Some(true),
            },
            PotentialPropertyDescriptor {
                value: Some(ECMAScriptValue::from(true)),
                writable: None,
                get: None,
                set: None,
                enumerable: Some(true),
                configurable: Some(true),
            },
            PotentialPropertyDescriptor {
                value: Some(ECMAScriptValue::from(true)),
                writable: Some(true),
                get: Some(ECMAScriptValue::Undefined),
                set: None,
                enumerable: Some(true),
                configurable: Some(true),
            },
            PotentialPropertyDescriptor {
                value: Some(ECMAScriptValue::from(true)),
                writable: Some(true),
                get: None,
                set: Some(ECMAScriptValue::Undefined),
                enumerable: Some(true),
                configurable: Some(true),
            },
            PotentialPropertyDescriptor {
                value: Some(ECMAScriptValue::from(true)),
                writable: Some(true),
                get: None,
                set: None,
                enumerable: None,
                configurable: Some(true),
            },
            PotentialPropertyDescriptor {
                value: Some(ECMAScriptValue::from(true)),
                writable: Some(true),
                get: None,
                set: None,
                enumerable: Some(true),
                configurable: None,
            },
            PotentialPropertyDescriptor {
                value: None,
                writable: None,
                get: None,
                set: None,
                enumerable: None,
                configurable: None,
            },
        ];
        for (right_idx, right_value) in items.iter().enumerate() {
            for (left_idx, left_value) in items.iter().enumerate() {
                assert_eq!(*left_value == *right_value, left_idx == right_idx);
                assert_eq!(*left_value != *right_value, left_idx != right_idx);
            }
        }
    }
    #[test]
    #[allow(clippy::redundant_clone)]
    fn clone() {
        let ppd1 = PotentialPropertyDescriptor {
            value: Some(ECMAScriptValue::from(true)),
            writable: Some(true),
            get: None,
            set: None,
            enumerable: Some(true),
            configurable: Some(true),
        };
        let ppd2 = ppd1.clone();
        assert_eq!(ppd1, ppd2);
    }
    #[test]
    fn is_writable() {
        let ppd1 = PotentialPropertyDescriptor { writable: Some(true), ..Default::default() };
        let ppd2 = PotentialPropertyDescriptor { writable: Some(false), ..Default::default() };
        let ppd3 = PotentialPropertyDescriptor { writable: None, ..Default::default() };

        assert_eq!(ppd1.is_writable(), Some(true));
        assert_eq!(ppd2.is_writable(), Some(false));
        assert_eq!(ppd3.is_writable(), None);
    }
    #[test]
    fn is_generic_descriptor() {
        let items = [
            (PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(true)), ..Default::default() }, false),
            (PotentialPropertyDescriptor { writable: Some(true), ..Default::default() }, false),
            (PotentialPropertyDescriptor { get: Some(ECMAScriptValue::from(true)), ..Default::default() }, false),
            (PotentialPropertyDescriptor { set: Some(ECMAScriptValue::from(true)), ..Default::default() }, false),
            (PotentialPropertyDescriptor { enumerable: Some(true), ..Default::default() }, true),
            (PotentialPropertyDescriptor { configurable: Some(true), ..Default::default() }, true),
        ];
        for (desc, expected) in &items {
            assert_eq!(desc.is_generic_descriptor(), *expected);
        }
    }
    #[test]
    fn is_data_descriptor() {
        let items = [
            (PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(true)), ..Default::default() }, true),
            (PotentialPropertyDescriptor { writable: Some(true), ..Default::default() }, true),
            (PotentialPropertyDescriptor { get: Some(ECMAScriptValue::from(true)), ..Default::default() }, false),
            (PotentialPropertyDescriptor { set: Some(ECMAScriptValue::from(true)), ..Default::default() }, false),
            (PotentialPropertyDescriptor { enumerable: Some(true), ..Default::default() }, false),
            (PotentialPropertyDescriptor { configurable: Some(true), ..Default::default() }, false),
        ];
        for (desc, expected) in &items {
            assert_eq!(desc.is_data_descriptor(), *expected);
        }
    }
    #[test]
    fn is_accessor_descriptor() {
        let items = [
            (PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(true)), ..Default::default() }, false),
            (PotentialPropertyDescriptor { writable: Some(true), ..Default::default() }, false),
            (PotentialPropertyDescriptor { get: Some(ECMAScriptValue::from(true)), ..Default::default() }, true),
            (PotentialPropertyDescriptor { set: Some(ECMAScriptValue::from(true)), ..Default::default() }, true),
            (PotentialPropertyDescriptor { enumerable: Some(true), ..Default::default() }, false),
            (PotentialPropertyDescriptor { configurable: Some(true), ..Default::default() }, false),
        ];
        for (desc, expected) in &items {
            assert_eq!(desc.is_accessor_descriptor(), *expected);
        }
    }

    #[test_case(true => PotentialPropertyDescriptor { configurable: Some(true), ..Default::default() }; "is configurable")]
    #[test_case(false => PotentialPropertyDescriptor { configurable: Some(false), ..Default::default() }; "isn't configurable")]
    fn configurable(cfgable: bool) -> PotentialPropertyDescriptor {
        PotentialPropertyDescriptor::new().configurable(cfgable)
    }
    #[test_case(true => PotentialPropertyDescriptor { enumerable: Some(true), ..Default::default() }; "is enumerable")]
    #[test_case(false => PotentialPropertyDescriptor { enumerable: Some(false), ..Default::default() }; "isn't enumerable")]
    fn enumerable(enumable: bool) -> PotentialPropertyDescriptor {
        PotentialPropertyDescriptor::new().enumerable(enumable)
    }
    #[test_case(true => PotentialPropertyDescriptor { writable: Some(true), ..Default::default() }; "is writable")]
    #[test_case(false => PotentialPropertyDescriptor { writable: Some(false), ..Default::default() }; "isn't writable")]
    fn writable(writable: bool) -> PotentialPropertyDescriptor {
        PotentialPropertyDescriptor::new().writable(writable)
    }
    #[test_case(|| -54_i32 => "-54"; "value is i32")]
    #[test_case(|| "string".to_string() => "string"; "value is String")]
    #[test_case(|| JSString::from("jsstring") => "jsstring"; "value is JSString")]
    #[test_case(|| ECMAScriptValue::Null => "null"; "value is ECMAScriptValue")]
    #[test_case(|| true => "true"; "value is boolean")]
    #[test_case(|| { let o = ordinary_object_create(None, &[]); o.set("propkey", "propvalue", true).unwrap(); o } => "propkey:propvalue"; "value is object")]
    fn value<T>(maker: impl FnOnce() -> T) -> String
    where
        T: Into<ECMAScriptValue>,
    {
        setup_test_agent();
        let val = maker();
        PotentialPropertyDescriptor::new().value(val).value.unwrap().test_result_string()
    }
    #[test_case(|| { let o = ordinary_object_create(None, &[]); o.set("propkey", "propvalue", true).unwrap(); o } => "propkey:propvalue"; "value is object")]
    #[test_case(|| true => "true"; "value is bool")]
    fn ppd_set<T>(maker: impl FnOnce() -> T) -> String
    where
        T: Into<ECMAScriptValue>,
    {
        setup_test_agent();
        let val = maker();
        PotentialPropertyDescriptor::new().set(val).set.unwrap().test_result_string()
    }

    mod from {
        use super::*;
        use test_case::test_case;

        #[derive(Debug, PartialEq)]
        struct TestResult {
            value: Option<String>,
            writable: Option<bool>,
            get: Option<String>,
            set: Option<String>,
            enumerable: bool,
            configurable: bool,
        }

        #[test_case(|| PropertyDescriptor {
            property: PropertyKind::Data(DataProperty{ value: ECMAScriptValue::from("testcase"), writable: true }),
            enumerable: true,
            configurable: true,
            ..Default::default()
        } => TestResult {
            value: Some("testcase".to_string()),
            writable: Some(true),
            get: None,
            set: None,
            enumerable: true,
            configurable: true,
        }; "data property")]
        #[test_case(|| PropertyDescriptor {
            property: PropertyKind::Accessor(AccessorProperty {
                get: intrinsic(IntrinsicId::ThrowTypeError).into(),
                set: intrinsic(IntrinsicId::ThrowTypeError).into(),
            }),
            enumerable: true,
            configurable: true,
            ..Default::default()
        } => TestResult {
            value: None,
            writable: None,
            get: Some("length:0,name:".to_string()),
            set: Some("length:0,name:".to_string()),
            enumerable: true,
            configurable: true,
        }; "accessor property")]
        fn property_descriptor(maker: impl FnOnce() -> PropertyDescriptor) -> TestResult {
            setup_test_agent();
            let ppd = PotentialPropertyDescriptor::from(maker());
            TestResult {
                value: ppd.value.map(|x| x.test_result_string()),
                writable: ppd.writable,
                get: ppd.get.map(|x| x.test_result_string()),
                set: ppd.set.map(|x| x.test_result_string()),
                enumerable: ppd.enumerable.unwrap(),
                configurable: ppd.configurable.unwrap(),
            }
        }

        #[test_case(PotentialPropertyDescriptor::new => TestResult {
            value: Some("undefined".to_string()),
            writable: Some(false),
            get: None,
            set: None,
            enumerable: false,
            configurable: false,
        }; "empty")]
        #[test_case(|| PotentialPropertyDescriptor::new().get(ECMAScriptValue::Undefined) => TestResult {
            value: None,
            writable: None,
            get: Some("undefined".to_string()),
            set: Some("undefined".to_string()),
            enumerable: false,
            configurable: false,
        }; "only get")]
        fn complete(maker: impl FnOnce() -> PotentialPropertyDescriptor) -> TestResult {
            setup_test_agent();
            let pd = maker().complete();
            let (value, writable, get, set) = match pd.property {
                PropertyKind::Data(DataProperty { value, writable }) => (Some(value), Some(writable), None, None),
                PropertyKind::Accessor(AccessorProperty { get, set }) => (None, None, Some(get), Some(set)),
            };
            TestResult {
                value: value.map(|x| x.test_result_string()),
                writable,
                get: get.map(|x| x.test_result_string()),
                set: set.map(|x| x.test_result_string()),
                configurable: pd.configurable,
                enumerable: pd.enumerable,
            }
        }
    }
}

#[test]
fn is_accessor_descriptor_01() {
    let ppd_no = PotentialPropertyDescriptor { writable: Some(true), ..Default::default() };
    let ppd_yes = PotentialPropertyDescriptor { get: Some(ECMAScriptValue::Undefined), ..Default::default() };
    let desc_no = PropertyDescriptor { property: PropertyKind::Data(Default::default()), ..Default::default() };
    let desc_yes = PropertyDescriptor { property: PropertyKind::Accessor(Default::default()), ..Default::default() };

    assert!(is_accessor_descriptor(&ppd_yes));
    assert!(is_accessor_descriptor(&desc_yes));
    assert!(!is_accessor_descriptor(&ppd_no));
    assert!(!is_accessor_descriptor(&desc_no));
}

#[test]
fn is_data_descriptor_01() {
    let ppd_yes = PotentialPropertyDescriptor { writable: Some(true), ..Default::default() };
    let ppd_no = PotentialPropertyDescriptor { get: Some(ECMAScriptValue::Undefined), ..Default::default() };
    let desc_yes = PropertyDescriptor { property: PropertyKind::Data(Default::default()), ..Default::default() };
    let desc_no = PropertyDescriptor { property: PropertyKind::Accessor(Default::default()), ..Default::default() };

    assert!(is_data_descriptor(&ppd_yes));
    assert!(is_data_descriptor(&desc_yes));
    assert!(!is_data_descriptor(&ppd_no));
    assert!(!is_data_descriptor(&desc_no));
}

#[test]
fn is_generic_descriptor_01() {
    let ppd_data = PotentialPropertyDescriptor { writable: Some(true), ..Default::default() };
    let ppd_acc = PotentialPropertyDescriptor { get: Some(ECMAScriptValue::Undefined), ..Default::default() };
    let desc_data = PropertyDescriptor { property: PropertyKind::Data(Default::default()), ..Default::default() };
    let desc_acc = PropertyDescriptor { property: PropertyKind::Accessor(Default::default()), ..Default::default() };
    let desc_def: PropertyDescriptor = Default::default();
    let ppd_def: PotentialPropertyDescriptor = Default::default();

    assert!(!is_generic_descriptor(&ppd_data));
    assert!(!is_generic_descriptor(&desc_data));
    assert!(!is_generic_descriptor(&ppd_acc));
    assert!(!is_generic_descriptor(&desc_acc));
    assert!(!is_generic_descriptor(&desc_def));
    assert!(is_generic_descriptor(&ppd_def));
}

#[test]
fn ordinary_get_prototype_of_01() {
    setup_test_agent();
    let object_proto = intrinsic(IntrinsicId::ObjectPrototype);
    let obj = ordinary_object_create(Some(object_proto.clone()), &[]);

    let result = ordinary_get_prototype_of(&obj);
    assert_eq!(result, Some(object_proto));
}

mod ordinary_set_prototype_of {
    use super::*;
    use test_case::test_case;

    #[allow(clippy::unnecessary_wraps)]
    fn steps(_: &ECMAScriptValue, _: Option<&Object>, _: &[ECMAScriptValue]) -> Completion<ECMAScriptValue> {
        Ok(ECMAScriptValue::Undefined)
    }

    #[test_case(
        || {
            let object_proto = intrinsic(IntrinsicId::ObjectPrototype);
            object_proto.create_data_property("sentinel", "original").unwrap();
            let new_proto = ordinary_object_create(Some(object_proto.clone()), &[]);
            new_proto.create_data_property("sentinel", "replacement").unwrap();
            let obj = ordinary_object_create(Some(object_proto), &[]);
            (obj, Some(new_proto))
        }
        => (true, Some("replacement".to_string()));
        "straightforward ordinary"
    )]
    #[test_case(
        || (ordinary_object_create(None, &[]), None)
        => (true, None);
        "none replaced with none"
    )]
    #[test_case(
        || {
            let object_proto = intrinsic(IntrinsicId::ObjectPrototype);
            object_proto.create_data_property("sentinel", "original").unwrap();
            let obj = ordinary_object_create(Some(object_proto.clone()), &[]);
            (obj, Some(object_proto))
        }
        => (true, Some("original".to_string()));
        "intrinsic replaced with intrinsic"
    )]
    #[test_case(
        || {
            let proto = intrinsic(IntrinsicId::ObjectPrototype);
            proto.create_data_property("sentinel", "intrinsic").unwrap();
            (ordinary_object_create(None, &[]), Some(intrinsic(IntrinsicId::ObjectPrototype)))
        }
        => (true, ssome("intrinsic"));
        "missing proto replaced by intrinsic"
    )]
    #[test_case(
        || {
            let proto = intrinsic(IntrinsicId::ObjectPrototype);
            proto.create_data_property("sentinel", "intrinsic").unwrap();
            let obj = ordinary_object_create(Some(proto), &[]);
            obj.o.prevent_extensions().unwrap();
            (obj, None)
        }
        => (false, ssome("intrinsic"));
        "remove proto from frozen object"
    )]
    #[test_case(
        || {
            let proto = intrinsic(IntrinsicId::ObjectPrototype);
            proto.create_data_property("sentinel", "intrinsic").unwrap();
            let obj = ordinary_object_create(Some(proto), &[]);
            (obj.clone(), Some(obj))
        }
        => (false, ssome("intrinsic"));
        "making circular proto loop"
    )]
    #[test_case(
        || (
            FunctionObject::new(
                None,
                current_realm_record().unwrap().borrow().global_env.clone().unwrap(),
                None,
                ParamSource::FormalParameters(Maker::new("").formal_parameters()),
                BodySource::Function(Maker::new("{}").function_body()),
                ConstructorKind::Base,
                current_realm_record().unwrap(),
                None,
                ThisMode::Lexical,
                true,
                None,
                "",
                vec![],
                vec![],
                ClassName::Empty,
                false,
                Rc::new(Chunk::new("test"))
            ),
            None
        )
        => (true, None);
        "using FunctionObject"
    )]
    #[test_case(|| (OrdinaryObject::new(None, true), None) => (true, None); "using OrdinaryObject")]
    #[test_case(|| (TestObject::new(None, &[]), None) => (true, None); "using TestObject")]
    #[test_case(
        || (AdaptableObject::new(None, &AdaptableMethods { ..Default::default() }), None)
        => (true, None);
        "using AdaptableObject"
    )]
    #[test_case(|| (ArrayObject::new(None), None) => (true, None); "using ArrayObject")]
    #[test_case(|| (NumberObject::new(None), None) => (true, None); "using NumberObject")]
    #[test_case(
        || (ForInIteratorObject::new(None, intrinsic(IntrinsicId::ObjectPrototype)), None)
        => (true, None);
        "using ForInIteratorObject"
    )]
    #[test_case(
        || (BuiltInFunctionObject::new(None, true, current_realm_record().unwrap(), None, steps, false), None)
        => (true, None);
        "using BuiltInFunctionObject"
    )]
    #[test_case(|| (ArgumentsObject::new(None, None), None) => (true, None); "using ArgumentsObject")]
    #[test_case(|| (BooleanObject::new(None), None) => (true, None); "using BooleanObject")]
    #[test_case(|| (SymbolObject::new(None), None) => (true, None); "using SymbolObject")]
    #[test_case(
        || (GeneratorObject::new(None, GeneratorState::Undefined, ""), None)
        => (true, None);
        "using GeneratorObject"
    )]
    #[test_case(|| (StringObject::new("".into(), None), None) => (true, None); "using StringObject")]
    #[test_case(|| (ErrorObject::new(None), None) => (true, None); "using ErrorObject")]
    fn t<O>(make_items: impl FnOnce() -> (O, Option<Object>)) -> (bool, Option<String>)
    where
        for<'a> &'a O: Into<&'a dyn ObjectInterface>,
    {
        setup_test_agent();
        let (obj, new_proto) = make_items();
        let result = ordinary_set_prototype_of(&obj, new_proto);
        (result, ordinary_get_prototype_of(&obj).map(|o| o.get(&"sentinel".into()).unwrap().test_result_string()))
    }
}

#[test]
fn ordinary_is_extensible_01() {
    setup_test_agent();
    let object_proto = intrinsic(IntrinsicId::ObjectPrototype);
    let obj = ordinary_object_create(Some(object_proto), &[]);

    let result = ordinary_is_extensible(&obj);
    assert!(result);

    ordinary_prevent_extensions(&obj);
    let result = ordinary_is_extensible(&obj);
    assert!(!result);
}

mod ordinary_prevent_extensions {
    use super::*;
    use test_case::test_case;

    #[allow(clippy::unnecessary_wraps)]
    fn steps(_: &ECMAScriptValue, _: Option<&Object>, _: &[ECMAScriptValue]) -> Completion<ECMAScriptValue> {
        Ok(ECMAScriptValue::Undefined)
    }

    #[test_case(|| ordinary_object_create(None, &[]) => (true, false); "normal")]
    #[test_case(|| AdaptableObject::new(None, &AdaptableMethods::default()) => (true, false); "with AdaptableObject")]
    #[test_case(|| StringObject::new("".into(), None) => (true, false); "with StringObject")]
    #[test_case(
        || ForInIteratorObject::new(None, intrinsic(IntrinsicId::Object))
        => (true, false);
        "with ForInIteratorObject"
    )]
    #[test_case(|| BooleanObject::new(None) => (true, false); "with BooleanObject")]
    #[test_case(|| ArrayObject::new(None) => (true, false); "with ArrayObject")]
    #[test_case(|| ArgumentsObject::new(None, None) => (true, false); "with ArgumentsObject")]
    #[test_case(|| ImmutablePrototypeExoticObject::new(None) => (true, false); "with ImmutablePrototypeExoticObject")]
    #[test_case(|| TestObject::new(None, &[]) => (true, false); "with TestObject")]
    #[test_case(|| ErrorObject::new(None) => (true, false); "with ErrorObject")]
    #[test_case(|| NumberObject::new(None) => (true, false); "with NumberObject")]
    #[test_case(|| SymbolObject::new(None) => (true, false); "with SymbolObject")]
    #[test_case(|| OrdinaryObject::new(None, true) => (true, false); "with OrdinaryObject")]
    #[test_case(|| GeneratorObject::new(None, GeneratorState::Undefined, "") => (true, false); "with GeneratorObject")]
    #[test_case(|| FunctionObject::new(
        None,
        current_realm_record().unwrap().borrow().global_env.clone().unwrap(),
        None,
        ParamSource::FormalParameters(Maker::new("").formal_parameters()),
        BodySource::Function(Maker::new("{}").function_body()),
        ConstructorKind::Base,
        current_realm_record().unwrap(),
        None,
        ThisMode::Lexical,
        true,
        None,
        "",
        vec![],
        vec![],
        ClassName::Empty,
        false,
        Rc::new(Chunk::new("test"))
    ) => (true, false); "with FunctionObject")]
    #[test_case(
        || BuiltInFunctionObject::new(None, true, current_realm_record().unwrap(), None, steps, false)
        => (true, false);
        "with BuiltInFunctionObject"
    )]
    fn t<O>(make_obj: impl FnOnce() -> O) -> (bool, bool)
    where
        for<'a> &'a O: Into<&'a dyn ObjectInterface>,
    {
        setup_test_agent();

        let obj = make_obj();
        let result = ordinary_prevent_extensions(&obj);
        let after = (&obj).into().common_object_data().borrow().extensible;
        (result, after)
    }
}

#[test]
fn ordinary_get_own_property_01() {
    setup_test_agent();
    let object_proto = intrinsic(IntrinsicId::ObjectPrototype);
    let obj = ordinary_object_create(Some(object_proto), &[]);
    let key = PropertyKey::from("a");

    let result = ordinary_get_own_property(&obj, &key);
    assert!(result.is_none());
}
#[test]
fn ordinary_get_own_property_02() {
    setup_test_agent();
    let object_proto = intrinsic(IntrinsicId::ObjectPrototype);
    let obj = ordinary_object_create(Some(object_proto), &[]);
    let key = PropertyKey::from("a");
    let ppd = PotentialPropertyDescriptor {
        value: Some(ECMAScriptValue::from(10)),
        writable: Some(true),
        enumerable: Some(true),
        configurable: Some(true),
        ..Default::default()
    };
    define_property_or_throw(&obj, key.clone(), ppd).unwrap();

    let result = ordinary_get_own_property(&obj, &key).unwrap();
    assert_eq!(result.configurable, true);
    assert_eq!(result.enumerable, true);
    assert!(matches!(result.property, PropertyKind::Data(..)));
    if let PropertyKind::Data(data) = &result.property {
        assert_eq!(data.value, ECMAScriptValue::from(10));
        assert_eq!(data.writable, true);
    }
}

mod ordinary_define_own_property {
    use super::*;
    use test_case::test_case;

    #[allow(clippy::unnecessary_wraps)]
    fn steps(_: &ECMAScriptValue, _: Option<&Object>, _: &[ECMAScriptValue]) -> Completion<ECMAScriptValue> {
        Ok(ECMAScriptValue::Undefined)
    }

    #[test_case(
        || ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)), &[]),
        || "a",
        || PotentialPropertyDescriptor::new().value(10).writable(true).enumerable(true).configurable(true)
        => Ok((true, ssome("value:10,writable:true,enumerable:true,configurable:true")));
        "ordinary extensible"
    )]
    #[test_case(
        || {
            let obj = ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)), &[]);
            obj.o.prevent_extensions().unwrap();
            obj
        },
        || "a",
        || PotentialPropertyDescriptor::new().value(10).writable(true).enumerable(true).configurable(true)
        => Ok((false, None));
        "ordinary non-extensible"
    )]
    #[test_case(
        || {
            let obj = ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)), &[]);
            obj.create_data_property("a", 10).unwrap();
            obj
        },
        || "a",
        || PotentialPropertyDescriptor::new().value(0).writable(true).enumerable(true).configurable(true)
        => Ok((true, ssome("value:0,writable:true,enumerable:true,configurable:true")));
        "ordinary change"
    )]
    #[test_case(
        || TestObject::object(&[FunctionId::GetOwnProperty(None)]),
        || "a",
        || PotentialPropertyDescriptor::new().value(10).writable(true).enumerable(true).configurable(true)
        => serr("TypeError: [[GetOwnProperty]] called on TestObject");
        "[[GetOwnProperty]] fails"
    )]
    #[test_case(
        || TestObject::object(&[FunctionId::IsExtensible]),
        || "a",
        || PotentialPropertyDescriptor::new().value(10).writable(true).enumerable(true).configurable(true)
        => serr("TypeError: [[IsExtensible]] called on TestObject");
        "[[IsExtensible]] fails"
    )]
    #[test_case(
        || ForInIteratorObject::new(None, intrinsic(IntrinsicId::Object)),
        || PropertyKey::from("pk"),
        || PotentialPropertyDescriptor::new().value(99).writable(true).enumerable(true).configurable(true)
        => Ok((true, ssome("value:99,writable:true,enumerable:true,configurable:true")));
        "using ForInIteratorObject"
    )]
    #[test_case(
        || ArgumentsObject::new(None, None),
        || PropertyKey::from("pk"),
        || PotentialPropertyDescriptor::new().value(99).writable(true).enumerable(true).configurable(true)
        => Ok((true, ssome("value:99,writable:true,enumerable:true,configurable:true")));
        "using ArgumentsObject"
    )]
    #[test_case(
        || ArrayObject::new(None),
        || "pk",
        || PotentialPropertyDescriptor::new().value(99).writable(true).enumerable(true).configurable(true)
        => Ok((true, ssome("value:99,writable:true,enumerable:true,configurable:true")));
        "using ArrayObject:&str"
    )]
    #[test_case(
        || ArrayObject::new(None),
        || PropertyKey::from("pk"),
        || PotentialPropertyDescriptor::new().value(99).writable(true).enumerable(true).configurable(true)
        => Ok((true, ssome("value:99,writable:true,enumerable:true,configurable:true")));
        "using ArrayObject:PropertyKey"
    )]
    #[test_case(
        || BooleanObject::new(None),
        || PropertyKey::from("pk"),
        || PotentialPropertyDescriptor::new().value(99).writable(true).enumerable(true).configurable(true)
        => Ok((true, ssome("value:99,writable:true,enumerable:true,configurable:true")));
        "using BooleanObject"
    )]
    #[test_case(
        || GeneratorObject::new(None, GeneratorState::Undefined, ""),
        || PropertyKey::from("pk"),
        || PotentialPropertyDescriptor::new().value(99).writable(true).enumerable(true).configurable(true)
        => Ok((true, ssome("value:99,writable:true,enumerable:true,configurable:true")));
        "using GeneratorObject"
    )]
    #[test_case(
        || ErrorObject::new(None),
        || PropertyKey::from("pk"),
        || PotentialPropertyDescriptor::new().value(99).writable(true).enumerable(true).configurable(true)
        => Ok((true, ssome("value:99,writable:true,enumerable:true,configurable:true")));
        "using ErrorObject"
    )]
    #[test_case(
        || BuiltInFunctionObject::new(None, true, current_realm_record().unwrap(), None, steps, false),
        || PropertyKey::from("pk"),
        || PotentialPropertyDescriptor::new().value(99).writable(true).enumerable(true).configurable(true)
        => Ok((true, ssome("value:99,writable:true,enumerable:true,configurable:true")));
        "using BuiltInFunctionObject"
    )]
    #[test_case(
        || FunctionObject::new(
            None,
            current_realm_record().unwrap().borrow().global_env.clone().unwrap(),
            None,
            ParamSource::FormalParameters(Maker::new("").formal_parameters()),
            BodySource::Function(Maker::new("{}").function_body()),
            ConstructorKind::Base,
            current_realm_record().unwrap(),
            None,
            ThisMode::Lexical,
            true,
            None,
            "",
            vec![],
            vec![],
            ClassName::Empty,
            false,
            Rc::new(Chunk::new("test"))
        ),
        || PropertyKey::from("pk"),
        || PotentialPropertyDescriptor::new().value(99).writable(true).enumerable(true).configurable(true)
        => Ok((true, ssome("value:99,writable:true,enumerable:true,configurable:true")));
        "using FunctionObject"
    )]
    #[test_case(
        || NumberObject::new(None),
        || PropertyKey::from("pk"),
        || PotentialPropertyDescriptor::new().value(99).writable(true).enumerable(true).configurable(true)
        => Ok((true, ssome("value:99,writable:true,enumerable:true,configurable:true")));
        "using NumberObject"
    )]
    #[test_case(
        || ImmutablePrototypeExoticObject::new(None),
        || PropertyKey::from("pk"),
        || PotentialPropertyDescriptor::new().value(99).writable(true).enumerable(true).configurable(true)
        => Ok((true, ssome("value:99,writable:true,enumerable:true,configurable:true")));
        "using ImmutablePrototypeExoticObject"
    )]
    #[test_case(
        || OrdinaryObject::new(None, true),
        || PropertyKey::from("pk"),
        || PotentialPropertyDescriptor::new().value(99).writable(true).enumerable(true).configurable(true)
        => Ok((true, ssome("value:99,writable:true,enumerable:true,configurable:true")));
        "using OrdinaryObject"
    )]
    #[test_case(
        || StringObject::new("".into(), None),
        || PropertyKey::from("pk"),
        || PotentialPropertyDescriptor::new().value(99).writable(true).enumerable(true).configurable(true)
        => Ok((true, ssome("value:99,writable:true,enumerable:true,configurable:true")));
        "using StringObject"
    )]
    #[test_case(
        || SymbolObject::new(None),
        || PropertyKey::from("pk"),
        || PotentialPropertyDescriptor::new().value(99).writable(true).enumerable(true).configurable(true)
        => Ok((true, ssome("value:99,writable:true,enumerable:true,configurable:true")));
        "using SymbolObject"
    )]
    #[test_case(
        || AdaptableObject::new(None, &AdaptableMethods::default()),
        || PropertyKey::from("pk"),
        || PotentialPropertyDescriptor::new().value(99).writable(true).enumerable(true).configurable(true)
        => Ok((true, ssome("value:99,writable:true,enumerable:true,configurable:true")));
        "using AdaptableObject"
    )]
    #[test_case(
        || TestObject::new(None, &[]),
        || PropertyKey::from("pk"),
        || PotentialPropertyDescriptor::new().value(99).writable(true).enumerable(true).configurable(true)
        => Ok((true, ssome("value:99,writable:true,enumerable:true,configurable:true")));
        "using TestObject"
    )]
    fn t<O, K, D>(
        make_obj: impl FnOnce() -> O,
        make_key: impl FnOnce() -> K,
        make_ppd: impl FnOnce() -> D,
    ) -> Result<(bool, Option<String>), String>
    where
        for<'a> &'a O: Into<&'a dyn ObjectInterface>,
        K: Into<PropertyKey> + Clone,
        D: Into<PotentialPropertyDescriptor>,
    {
        setup_test_agent();
        let obj = make_obj();
        let key = make_key();
        let key_dup: PropertyKey = key.clone().into();
        let ppd = make_ppd();
        ordinary_define_own_property(&obj, key, ppd).map_err(unwind_any_error).map(|x| {
            let o: &dyn ObjectInterface = (&obj).into();
            (
                x,
                o.get_own_property(&key_dup).unwrap().map(|prop| {
                    ECMAScriptValue::from(from_property_descriptor(Some(prop)).unwrap()).test_result_string()
                }),
            )
        })
    }
}

#[test]
fn validate_and_apply_property_descriptor_01() {
    // current = Undefined & extensible = false => false
    let ppd = PotentialPropertyDescriptor { ..Default::default() };
    let result = validate_and_apply_property_descriptor::<&Object>(None, None, false, ppd, None);
    assert!(!result);
}
#[test]
fn validate_and_apply_property_descriptor_02() {
    // current = Undefined & extensible = true & O = Undefined => true
    let ppd = PotentialPropertyDescriptor { ..Default::default() };
    let result = validate_and_apply_property_descriptor::<&Object>(None, None, true, ppd, None);
    assert!(result);
}
#[test]
fn validate_and_apply_property_descriptor_03() {
    // current Undefined; empty descriptor
    setup_test_agent();
    let object_proto = intrinsic(IntrinsicId::ObjectPrototype);
    let obj = ordinary_object_create(Some(object_proto), &[]);
    let ppd = PotentialPropertyDescriptor { ..Default::default() };
    let key = PropertyKey::from("key");

    let result = validate_and_apply_property_descriptor(Some(&obj), Some(key.clone()), true, ppd, None);

    assert!(result);
    let pd = obj.o.get_own_property(&key).unwrap().unwrap();
    assert_eq!(pd.configurable, false);
    assert_eq!(pd.enumerable, false);
    assert_eq!(pd.property, PropertyKind::Data(DataProperty { value: ECMAScriptValue::Undefined, writable: false }));
}
#[test]
fn validate_and_apply_property_descriptor_04() {
    // current Undefined; overfull descriptor
    setup_test_agent();
    let object_proto = intrinsic(IntrinsicId::ObjectPrototype);
    let obj = ordinary_object_create(Some(object_proto), &[]);
    let ppd = PotentialPropertyDescriptor {
        value: Some(ECMAScriptValue::from(true)),
        writable: Some(true),
        enumerable: Some(true),
        configurable: Some(true),
        get: Some(ECMAScriptValue::from("get")),
        set: Some(ECMAScriptValue::from("set")),
    };
    let key = PropertyKey::from("key");

    let result = validate_and_apply_property_descriptor(Some(&obj), Some(key.clone()), true, ppd, None);

    assert!(result);
    let pd = obj.o.get_own_property(&key).unwrap().unwrap();
    assert_eq!(pd.configurable, true);
    assert_eq!(pd.enumerable, true);
    assert_eq!(pd.property, PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(true), writable: true }));
}
#[test]
fn validate_and_apply_property_descriptor_05() {
    // current Undefined; accessor descriptor
    setup_test_agent();
    let object_proto = intrinsic(IntrinsicId::ObjectPrototype);
    let obj = ordinary_object_create(Some(object_proto), &[]);
    let ppd = PotentialPropertyDescriptor {
        enumerable: Some(true),
        configurable: Some(true),
        get: Some(ECMAScriptValue::from(intrinsic(IntrinsicId::ThrowTypeError))),
        set: Some(ECMAScriptValue::Undefined),
        ..Default::default()
    };
    let key = PropertyKey::from("key");

    let result = validate_and_apply_property_descriptor(Some(&obj), Some(key.clone()), true, ppd, None);

    assert!(result);
    let pd = obj.o.get_own_property(&key).unwrap().unwrap();
    assert_eq!(pd.configurable, true);
    assert_eq!(pd.enumerable, true);
    assert_eq!(
        pd.property,
        PropertyKind::Accessor(AccessorProperty {
            get: ECMAScriptValue::from(intrinsic(IntrinsicId::ThrowTypeError)),
            set: ECMAScriptValue::Undefined
        })
    );
}
#[test]
fn validate_and_apply_property_descriptor_06() {
    // object Undefined; current reasonable; any valid input
    setup_test_agent();
    let object_proto = intrinsic(IntrinsicId::ObjectPrototype);
    let obj = ordinary_object_create(Some(object_proto), &[]);
    let existing = PotentialPropertyDescriptor {
        value: Some(ECMAScriptValue::from(99)),
        writable: Some(true),
        enumerable: Some(true),
        configurable: Some(true),
        ..Default::default()
    };
    let key = PropertyKey::from("key");
    define_property_or_throw(&obj, key.clone(), existing).unwrap();
    let current = obj.o.get_own_property(&key).unwrap().unwrap();

    let ppd = PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(10)), ..Default::default() };

    let result = validate_and_apply_property_descriptor::<&Object>(None, Some(key.clone()), true, ppd, Some(&current));

    assert!(result);
    let pd = obj.o.get_own_property(&key).unwrap().unwrap();
    assert_eq!(pd.configurable, true);
    assert_eq!(pd.enumerable, true);
    assert_eq!(pd.property, PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(99), writable: true }));
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// ONE BIG TEST to check all different PotentialPropertyDescriptors against all different existing PropertyDescriptors.
// (There are 23,328 different tests included in this.)
#[derive(PartialEq, Eq)]
enum Stage {
    Data,
    Accessor,
    Done,
}
impl Default for Stage {
    fn default() -> Self {
        Self::Data
    }
}
#[allow(clippy::struct_excessive_bools)]
#[derive(Default)]
struct VAPDIter {
    value: u8,
    enumerable: bool,
    writable: bool,
    configurable: bool,
    get_throws: bool,
    set_throws: bool,
    stage: Stage,
    tte: ECMAScriptValue,
}
impl VAPDIter {
    fn new() -> Self {
        VAPDIter { tte: ECMAScriptValue::from(intrinsic(IntrinsicId::ThrowTypeError)), ..Default::default() }
    }
    fn id_name(&self) -> String {
        format!(
            "{}{}{}{}",
            if self.configurable { 'C' } else { '-' },
            if self.enumerable { 'E' } else { '-' },
            if self.stage == Stage::Data {
                if self.writable {
                    'W'
                } else {
                    '-'
                }
            } else if self.get_throws {
                'G'
            } else {
                'u'
            },
            if self.stage == Stage::Data {
                (self.value + 0x30) as char
            } else if self.set_throws {
                'S'
            } else {
                'u'
            }
        )
    }
}
impl Iterator for VAPDIter {
    type Item = (String, PotentialPropertyDescriptor);
    fn next(&mut self) -> Option<Self::Item> {
        if self.stage == Stage::Done {
            None
        } else {
            let result = Some((
                self.id_name(),
                PotentialPropertyDescriptor {
                    value: if self.stage == Stage::Data {
                        Some(ECMAScriptValue::from(self.value as i32))
                    } else {
                        None
                    },
                    writable: if self.stage == Stage::Data { Some(self.writable) } else { None },
                    enumerable: Some(self.enumerable),
                    configurable: Some(self.configurable),
                    get: if self.stage == Stage::Accessor {
                        Some(if self.get_throws { self.tte.clone() } else { ECMAScriptValue::Undefined })
                    } else {
                        None
                    },
                    set: if self.stage == Stage::Accessor {
                        Some(if self.set_throws { self.tte.clone() } else { ECMAScriptValue::Undefined })
                    } else {
                        None
                    },
                },
            ));

            if self.configurable {
                self.configurable = false;
                if self.enumerable {
                    self.enumerable = false;
                    if self.stage == Stage::Data {
                        if self.writable {
                            self.writable = false;
                            if self.value == 0 {
                                self.value = 1;
                            } else {
                                self.stage = Stage::Accessor;
                            }
                        } else {
                            self.writable = true;
                        }
                    } else if !self.get_throws {
                        self.get_throws = true;
                    } else {
                        self.get_throws = false;
                        if self.set_throws {
                            self.stage = Stage::Done;
                        } else {
                            self.set_throws = true;
                        }
                    }
                } else {
                    self.enumerable = true;
                }
            } else {
                self.configurable = true;
            }

            result
        }
    }
}
#[derive(Default)]
struct VAPDCheck {
    value: Option<i32>,
    writable: Option<bool>,
    enumerable: Option<bool>,
    configurable: Option<bool>,
    get_throws: Option<bool>,
    set_throws: Option<bool>,
    tte: ECMAScriptValue,
    done: bool,
}
impl VAPDCheck {
    fn new() -> Self {
        Self { tte: ECMAScriptValue::from(intrinsic(IntrinsicId::ThrowTypeError)), ..Default::default() }
    }
}
impl Iterator for VAPDCheck {
    type Item = PotentialPropertyDescriptor;
    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            None
        } else {
            let result = Some(PotentialPropertyDescriptor {
                value: self.value.map(ECMAScriptValue::from),
                writable: self.writable,
                enumerable: self.enumerable,
                configurable: self.configurable,
                get: self.get_throws.map(|b| if b { self.tte.clone() } else { ECMAScriptValue::Undefined }),
                set: self.set_throws.map(|b| if b { self.tte.clone() } else { ECMAScriptValue::Undefined }),
            });

            if self.configurable.is_none() {
                self.configurable = Some(false);
            } else if self.configurable == Some(false) {
                self.configurable = Some(true);
            } else {
                self.configurable = None;
                if self.writable.is_none() {
                    self.writable = Some(false);
                } else if self.writable == Some(false) {
                    self.writable = Some(true);
                } else {
                    self.writable = None;
                    if self.enumerable.is_none() {
                        self.enumerable = Some(false);
                    } else if self.enumerable == Some(false) {
                        self.enumerable = Some(true);
                    } else {
                        self.enumerable = None;
                        if self.value.is_none() {
                            self.value = Some(0);
                        } else if self.value == Some(0) {
                            self.value = Some(1);
                        } else {
                            self.value = None;
                            if self.get_throws.is_none() {
                                self.get_throws = Some(false);
                            } else if self.get_throws == Some(false) {
                                self.get_throws = Some(true);
                            } else {
                                self.get_throws = None;
                                if self.set_throws.is_none() {
                                    self.set_throws = Some(false);
                                } else if self.set_throws == Some(false) {
                                    self.set_throws = Some(true);
                                } else {
                                    self.set_throws = None;
                                    self.done = true;
                                }
                            }
                        }
                    }
                }
            }

            result
        }
    }
}
fn figure_expectation(
    current: &PropertyDescriptor,
    incoming: &PotentialPropertyDescriptor,
) -> Option<PropertyDescriptor> {
    let config_changed = current.configurable != incoming.configurable.unwrap_or(current.configurable);
    let enumerable_changed = current.enumerable != incoming.enumerable.unwrap_or(current.enumerable);
    let current_configurable = current.configurable;
    let current_enumurable = current.enumerable;
    let kind_changed =
        current.is_data_descriptor() && incoming.is_accessor_descriptor() && !incoming.is_data_descriptor()
            || current.is_accessor_descriptor() && incoming.is_data_descriptor();

    match &current.property {
        PropertyKind::Data(dp) => {
            let writable_changed = dp.writable != incoming.writable.unwrap_or(dp.writable);
            let value_changed = &dp.value != incoming.value.as_ref().unwrap_or(&dp.value);
            let new_value = if let Some(v) = &incoming.value { v.clone() } else { dp.value.clone() };

            // If existing is "config", all changes are allowed.
            if current_configurable {
                // If existing is configurable, all changes are allowed.
                if kind_changed {
                    let new_get = if let Some(g) = &incoming.get { g.clone() } else { ECMAScriptValue::Undefined };
                    let new_set = if let Some(s) = &incoming.set { s.clone() } else { ECMAScriptValue::Undefined };
                    Some(PropertyDescriptor {
                        property: PropertyKind::Accessor(AccessorProperty { get: new_get, set: new_set }),
                        configurable: incoming.configurable.unwrap_or(current_configurable),
                        enumerable: incoming.enumerable.unwrap_or(current_enumurable),
                        spot: current.spot,
                    })
                } else {
                    Some(PropertyDescriptor {
                        property: PropertyKind::Data(DataProperty {
                            writable: incoming.writable.unwrap_or(dp.writable),
                            value: new_value,
                        }),
                        configurable: incoming.configurable.unwrap_or(current_configurable),
                        enumerable: incoming.enumerable.unwrap_or(current_enumurable),
                        spot: current.spot,
                    })
                }
            } else if dp.writable {
                // Otherwise, if existing is writable, changes to writable and value are allowed (but not configurable or enumerable)
                if !config_changed && !enumerable_changed && !kind_changed {
                    Some(PropertyDescriptor {
                        property: PropertyKind::Data(DataProperty {
                            writable: incoming.writable.unwrap_or(dp.writable),
                            value: new_value,
                        }),
                        configurable: current_configurable,
                        enumerable: current_enumurable,
                        spot: current.spot,
                    })
                } else {
                    None
                }
            } else {
                // Otherwise, the function succeeds only if no changes are made
                if !config_changed && !enumerable_changed && !writable_changed && !value_changed && !kind_changed {
                    Some(PropertyDescriptor {
                        property: PropertyKind::Data(DataProperty { writable: dp.writable, value: new_value }),
                        configurable: current_configurable,
                        enumerable: current_enumurable,
                        spot: current.spot,
                    })
                } else {
                    None
                }
            }
        }
        PropertyKind::Accessor(ap) => {
            let get_changed = &ap.get != incoming.get.as_ref().unwrap_or(&ap.get);
            let set_changed = &ap.set != incoming.set.as_ref().unwrap_or(&ap.set);

            // If existing is "config", all changes are allowed.
            if current_configurable {
                // If existing is configurable, all changes are allowed.
                if kind_changed {
                    let new_value = if let Some(v) = &incoming.value { v.clone() } else { ECMAScriptValue::Undefined };
                    Some(PropertyDescriptor {
                        property: PropertyKind::Data(DataProperty {
                            value: new_value,
                            writable: incoming.writable.unwrap_or(false),
                        }),
                        configurable: incoming.configurable.unwrap_or(current_configurable),
                        enumerable: incoming.enumerable.unwrap_or(current_enumurable),
                        spot: current.spot,
                    })
                } else {
                    let get_value = if let Some(g) = &incoming.get { g.clone() } else { ap.get.clone() };
                    let set_value = if let Some(s) = &incoming.set { s.clone() } else { ap.set.clone() };
                    Some(PropertyDescriptor {
                        property: PropertyKind::Accessor(AccessorProperty { get: get_value, set: set_value }),
                        configurable: incoming.configurable.unwrap_or(current_configurable),
                        enumerable: incoming.enumerable.unwrap_or(current_enumurable),
                        spot: current.spot,
                    })
                }
            } else {
                // Otherwise, the function succeeds only if no changes are made
                if !config_changed
                    && !enumerable_changed
                    && !get_changed
                    && !set_changed
                    && !incoming.is_data_descriptor()
                {
                    Some(PropertyDescriptor {
                        property: PropertyKind::Accessor(AccessorProperty { get: ap.get.clone(), set: ap.set.clone() }),
                        configurable: current_configurable,
                        enumerable: current_enumurable,
                        spot: current.spot,
                    })
                } else {
                    None
                }
            }
        }
    }
}
#[test]
fn validate_and_apply_property_descriptor_many() {
    setup_test_agent();
    let object_proto = intrinsic(IntrinsicId::ObjectPrototype);
    let obj = ordinary_object_create(Some(object_proto), &[]);
    for (name, ppd) in VAPDIter::new() {
        for (idx, _) in VAPDCheck::new().enumerate() {
            let key = PropertyKey::from(format!("{name}-{idx}"));
            define_property_or_throw(&obj, key, ppd.clone()).unwrap();
        }
    }

    for (name, _) in VAPDIter::new() {
        for (idx, check) in VAPDCheck::new().enumerate() {
            let key = PropertyKey::from(format!("{name}-{idx}"));
            let current = obj.o.get_own_property(&key).unwrap().unwrap();
            let expected_prop = figure_expectation(&current, &check);

            let result = validate_and_apply_property_descriptor(
                Some(&obj),
                Some(key.clone()),
                true,
                check.clone(),
                Some(&current),
            );
            assert_eq!(
                result,
                expected_prop.is_some(),
                "\nkey: {key:?};\ncurrent: {current:#?};\nPPD: {check:#?};\nexpected: {expected_prop:#?}"
            );
            let after = obj.o.get_own_property(&key).unwrap().unwrap();
            assert_eq!(
                &after,
                expected_prop.as_ref().unwrap_or(&current),
                "\nkey: {key:?};\ncurrent: {current:#?};\nPPD: {check:#?};\nexpected: {expected_prop:#?};"
            );
        }
    }
}

mod ordinary_has_property {
    use super::*;
    use test_case::test_case;

    #[allow(clippy::unnecessary_wraps)]
    fn steps(_: &ECMAScriptValue, _: Option<&Object>, _: &[ECMAScriptValue]) -> Completion<ECMAScriptValue> {
        Ok(ECMAScriptValue::Undefined)
    }

    fn setup<O>(obj: O) -> (O, PropertyKey)
    where
        O: ObjectInterface,
    {
        let key = PropertyKey::from("test_key");
        let ppd =
            PotentialPropertyDescriptor::new().value("test string").writable(true).enumerable(true).configurable(true);
        obj.define_own_property(key.clone(), ppd).unwrap();
        (obj, key)
    }

    #[test_case(
        || (TestObject::object(&[FunctionId::GetOwnProperty(Some("test_key".into()))]), PropertyKey::from("test_key"))
        => serr("TypeError: [[GetOwnProperty]] called on TestObject");
        "GetOwnProperty fails"
    )]
    #[test_case(
        || {
            let proto = intrinsic(IntrinsicId::ObjectPrototype);
            let obj = ordinary_object_create(Some(proto), &[]);
            (obj, PropertyKey::from("not_actually_a_key"))
        }
        => Ok(false);
        "Property not there, with prototype chain"
    )]
    #[test_case(
        || (TestObject::object(&[FunctionId::GetPrototypeOf]), PropertyKey::from("test_key"))
        => serr("TypeError: [[GetPrototypeOf]] called on TestObject");
        "GetPrototypeOf fails"
    )]
    #[test_case(
        || {
            let obj = ordinary_object_create(None, &[]);
            obj.set("test_key", "value", true).unwrap();
            (obj, PropertyKey::from("test_key"))
        }
        => Ok(true);
        "Property exists"
    )]
    #[test_case(|| setup(NumberObject::new(None)) => Ok(true); "using NumberObject")]
    #[test_case(
        || setup(ForInIteratorObject::new(None, intrinsic(IntrinsicId::Object)))
        => Ok(true);
        "using ForInIteratorObject"
    )]
    #[test_case(
        || setup(GeneratorObject::new(None, GeneratorState::Undefined, ""))
        => Ok(true);
        "using GeneratorObject"
    )]
    #[test_case(|| setup(OrdinaryObject::new(None, true)) => Ok(true); "using OrdinaryObject")]
    #[test_case(|| setup(ArrayObject::new(None)) => Ok(true); "using ArrayObject")]
    #[test_case(|| setup(BooleanObject::new(None)) => Ok(true); "using BooleanObject")]
    #[test_case(|| setup(SymbolObject::new(None)) => Ok(true); "using SymbolObject")]
    #[test_case(
        || setup(BuiltInFunctionObject::new(None, true, current_realm_record().unwrap(), None, steps, false))
        => Ok(true);
        "using BuiltInFunctionObject"
    )]
    #[test_case(|| setup(TestObject::new(None, &[])) => Ok(true); "using TestObject")]
    #[test_case(|| setup(ErrorObject::new(None)) => Ok(true); "using ErrorObject")]
    #[test_case(|| setup(ArgumentsObject::new(None, None)) => Ok(true); "using ArgumentsObject")]
    #[test_case(|| setup(AdaptableObject::new(None, &AdaptableMethods{..Default::default()})) => Ok(true); "using AdaptableObject")]
    #[test_case(|| setup(StringObject::new("".into(), None)) => Ok(true); "using StringObject")]
    #[test_case(
        || {
            setup(FunctionObject::new(
                None,
                current_realm_record().unwrap().borrow().global_env.clone().unwrap(),
                None,
                ParamSource::FormalParameters(Maker::new("").formal_parameters()),
                BodySource::Function(Maker::new("{}").function_body()),
                ConstructorKind::Base,
                current_realm_record().unwrap(),
                None,
                ThisMode::Lexical,
                true,
                None,
                "",
                vec![],
                vec![],
                ClassName::Empty,
                false,
                Rc::new(Chunk::new("test"))
            ))
        }
        => Ok(true);
        "using FunctionObject"
    )]
    fn t<O>(make_items: impl FnOnce() -> (O, PropertyKey)) -> Result<bool, String>
    where
        for<'a> &'a O: Into<&'a dyn ObjectInterface>,
    {
        setup_test_agent();
        let (obj, key) = make_items();
        ordinary_has_property(&obj, &key).map_err(unwind_any_error)
    }
}

fn test_getter(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // This is a getter; it is essentially:
    // function() { return this.result; }
    let obj = to_object(this_value.clone())?;
    let key = PropertyKey::from("result");
    obj.get(&key)
}

mod ordinary_get {
    use super::*;
    use test_case::test_case;

    fn withprop<O>(obj: O) -> (O, PropertyKey, ECMAScriptValue)
    where
        O: ObjectInterface,
    {
        let key = PropertyKey::from("test_key");
        let ppd =
            PotentialPropertyDescriptor::new().value("sentinel").writable(true).enumerable(true).configurable(true);
        obj.define_own_property(key.clone(), ppd).unwrap();
        (obj, key, ECMAScriptValue::Undefined)
    }
    fn withoutprop<O>(obj: O) -> (O, PropertyKey, ECMAScriptValue)
    where
        for<'a> &'a O: Into<&'a dyn ObjectInterface>,
    {
        let key = PropertyKey::from("test_key");
        (obj, key, ECMAScriptValue::Undefined)
    }
    #[allow(clippy::unnecessary_wraps)]
    fn steps(
        this_value: &ECMAScriptValue,
        _: Option<&Object>,
        arguments: &[ECMAScriptValue],
    ) -> Completion<ECMAScriptValue> {
        let rval = create_array_from_list(arguments);
        rval.create_data_property_or_throw("this_value", this_value.test_result_string()).unwrap();
        Ok(ECMAScriptValue::from(rval))
    }
    fn cbf(
        behavior: fn(&ECMAScriptValue, Option<&Object>, &[ECMAScriptValue]) -> Completion<ECMAScriptValue>,
    ) -> Object {
        create_builtin_function(
            behavior,
            false,
            0.0,
            "f".into(),
            BUILTIN_FUNCTION_SLOTS,
            current_realm_record(),
            Some(intrinsic(IntrinsicId::FunctionPrototype)),
            None,
        )
    }

    #[test_case(
        || withoutprop(TestObject::new(None, &[FunctionId::GetOwnProperty(None)]))
        => serr("TypeError: [[GetOwnProperty]] called on TestObject");
        "GetOwnProperty fails"
    )]
    #[test_case(
        || withoutprop(TestObject::new(None, &[FunctionId::GetPrototypeOf]))
        => serr("TypeError: [[GetPrototypeOf]] called on TestObject");
        "GetPrototypeOf fails"
    )]
    #[test_case(
        || {
            let proto = intrinsic(IntrinsicId::ObjectPrototype);
            withoutprop(ordinary_object_create(Some(proto), &[]))
        }
        => sok("undefined");
        "has proto, but not in proto chain"
    )]
    #[test_case(
        || {
            let proto = intrinsic(IntrinsicId::ObjectPrototype);
            proto.set("test_key", "thirteen", true).unwrap();
            withoutprop(ordinary_object_create(Some(proto), &[]))
        }
        => sok("thirteen");
        "in proto chain"
    )]
    #[test_case(
        || {
            let proto = intrinsic(IntrinsicId::ObjectPrototype);
            let obj = OrdinaryObject::new(Some(proto), true);
            withprop(obj)
        }
        => sok("sentinel");
        "in obj itself"
    )]
    #[test_case(
        || {
            let obj = ordinary_object_create(None, &[]);
            let receiver = obj.clone();
            let ppd = PotentialPropertyDescriptor::new().get(ECMAScriptValue::Undefined);
            define_property_or_throw(&obj, "test_key", ppd).unwrap();
            (obj, PropertyKey::from("test_key"), ECMAScriptValue::from(receiver))
        }
        => sok("undefined");
        "Undefined getter"
    )]
    #[test_case(
        || {
            let obj = ordinary_object_create(None, &[]);
            obj.create_data_property_or_throw("marker", "object-to-get-from").unwrap();
            let receiver = ordinary_object_create(None, &[]);
            receiver.create_data_property_or_throw("marker", "receiver-object").unwrap();
            let getter = cbf(steps);
            let ppd = PotentialPropertyDescriptor::new().get(getter);
            define_property_or_throw(&obj, "test_key", ppd).unwrap();
            (obj, PropertyKey::from("test_key"), ECMAScriptValue::from(receiver))
        }
        => sok("length:0,this_value:marker:receiver-object");
        "getter defined"
    )]
    #[test_case(|| withoutprop(StringObject::new("a".into(), None)) => sok("undefined"); "using StringObject")]
    #[test_case(|| withoutprop(NumberObject::new(None)) => sok("undefined"); "using NumberObject")]
    #[test_case(|| withoutprop(BooleanObject::new(None)) => sok("undefined"); "using BooleanObject")]
    #[test_case(
        || withoutprop(GeneratorObject::new(None, GeneratorState::Undefined, ""))
        => sok("undefined");
        "using GeneratorObject"
    )]
    #[test_case(|| withoutprop(TestObject::new(None, &[])) => sok("undefined"); "using TestObject")]
    #[test_case(|| withoutprop(ArrayObject::new(None)) => sok("undefined"); "using ArrayObject")]
    #[test_case(
        || withoutprop(ImmutablePrototypeExoticObject::new(None))
        => sok("undefined");
        "using ImmutablePrototypeExoticObject"
    )]
    #[test_case(|| withoutprop(SymbolObject::new(None)) => sok("undefined"); "using SymbolObject")]
    #[test_case(
        || withoutprop(ForInIteratorObject::new(None, intrinsic(IntrinsicId::StringPrototype)))
        => sok("undefined");
        "using ForInIteratorObject")]
    #[test_case(
        || {
            withoutprop(FunctionObject::new(
                None,
                current_realm_record().unwrap().borrow().global_env.clone().unwrap(),
                None,
                ParamSource::FormalParameters(Maker::new("").formal_parameters()),
                BodySource::Function(Maker::new("{}").function_body()),
                ConstructorKind::Base,
                current_realm_record().unwrap(),
                None,
                ThisMode::Lexical,
                true,
                None,
                "",
                vec![],
                vec![],
                ClassName::Empty,
                false,
                Rc::new(Chunk::new("test"))
            ))
        }
        => sok("undefined");
        "using FunctionObject")]
    #[test_case(|| withoutprop(ArgumentsObject::new(None, None)) => sok("undefined"); "using ArgumentsObject")]
    #[test_case(
        || withoutprop(AdaptableObject::new(None, &AdaptableMethods{..Default::default()}))
        => sok("undefined");
        "using AdaptableObject"
    )]
    fn t<O>(make_items: impl FnOnce() -> (O, PropertyKey, ECMAScriptValue)) -> Result<String, String>
    where
        for<'a> &'a O: Into<&'a dyn ObjectInterface>,
    {
        setup_test_agent();
        let (obj, key, receiver) = make_items();
        ordinary_get(&obj, &key, &receiver).map_err(unwind_any_error).map(|v| v.test_result_string())
    }
}

mod ordinary_set {
    use super::*;
    use test_case::test_case;

    #[allow(clippy::unnecessary_wraps)]
    fn steps(_: &ECMAScriptValue, _: Option<&Object>, _: &[ECMAScriptValue]) -> Completion<ECMAScriptValue> {
        Ok(ECMAScriptValue::Undefined)
    }

    fn setup<O>(obj: O) -> (O, PropertyKey, ECMAScriptValue, ECMAScriptValue) {
        let key = PropertyKey::from("test_key");
        (obj, key, ECMAScriptValue::Undefined, ordinary_object_create(None, &[]).into())
    }

    #[test_case(
        || setup(TestObject::object(&[FunctionId::GetOwnProperty(None)]))
        => serr("TypeError: [[GetOwnProperty]] called on TestObject");
        "[[GetOwnProperty]] throws"
    )]
    #[test_case(
        || {
            let obj = ordinary_object_create(None, &[]);
            (
                obj.clone(),
                PropertyKey::from("test_key"),
                ECMAScriptValue::from("sentinel"),
                ECMAScriptValue::from(obj)
            )
        }
        => Ok((true, "sentinel".to_string()));
        "simple set"
    )]
    #[test_case(
        || setup(BuiltInFunctionObject::new(None, true, current_realm_record().unwrap(), None, steps, false))
        => Ok((true, "undefined".to_string()));
        "BuiltInFunctionObject argument"
    )]
    #[test_case(
        || setup(OrdinaryObject::new(None, true))
        => Ok((true, "undefined".to_string()));
        "OrdinaryObject argument"
    )]
    #[test_case(
        || setup(ArrayObject::new(None))
        => Ok((true, "undefined".to_string()));
        "ArrayObject argument"
    )]
    #[test_case(
        || setup(ArgumentsObject::new(None, None))
        => Ok((true, "undefined".to_string()));
        "ArgumentsObject argument"
    )]
    #[test_case(
        || setup(Object::new(None, true))
        => Ok((true, "undefined".to_string()));
        "Object argument"
    )]
    #[test_case(
        || setup(ForInIteratorObject::new(None, intrinsic(IntrinsicId::StringPrototype)))
        => Ok((true, "undefined".to_string()));
        "ForInIteratorObject argument"
    )]
    #[test_case(
        || setup(FunctionObject::new(None,
            current_realm_record().unwrap().borrow().global_env.clone().unwrap(),
            None,
            ParamSource::FormalParameters(Maker::new("").formal_parameters()),
            BodySource::Function(Maker::new("{}").function_body()),
            ConstructorKind::Base,
            current_realm_record().unwrap(),
            None,
            ThisMode::Lexical,
            true,
            None,
            "",
            vec![],
            vec![],
            ClassName::Empty,
            false,
            Rc::new(Chunk::new("test"))))
        => Ok((true, "undefined".to_string()));
        "FunctionObject argument"
    )]
    #[test_case(
        || setup(NumberObject::new(None))
        => Ok((true, "undefined".to_string()));
        "NumberObject argument"
    )]
    #[test_case(
        || setup(SymbolObject::new(None))
        => Ok((true, "undefined".to_string()));
        "SymbolObject argument"
    )]
    #[test_case(
        || setup(TestObject::new(None, &[]))
        => Ok((true, "undefined".to_string()));
        "TestObject argument"
    )]
    #[test_case(
        || setup(ImmutablePrototypeExoticObject::new(None))
        => Ok((true, "undefined".to_string()));
        "ImmutablePrototypeExoticObject argument"
    )]
    #[test_case(
        || setup(StringObject::new("blue".into(), None))
        => Ok((true, "undefined".to_string()));
        "StringObject argument"
    )]
    #[test_case(
        || setup(AdaptableObject::new(None, &AdaptableMethods { ..Default::default() }))
        => Ok((true, "undefined".to_string()));
        "AdaptableObject argument"
    )]
    #[test_case(
        || setup(ErrorObject::new(None))
        => Ok((true, "undefined".to_string()));
        "ErrorObject argument"
    )]
    #[test_case(
        || setup(BooleanObject::new(None))
        => Ok((true, "undefined".to_string()));
        "BooleanObject argument"
    )]
    #[test_case(
        || setup(GeneratorObject::new(None, GeneratorState::Undefined, ""))
        => Ok((true, "undefined".to_string()));
        "GeneratorObject argument"
    )]

    fn t<O, P, V>(make_items: impl FnOnce() -> (O, P, V, ECMAScriptValue)) -> Result<(bool, String), String>
    where
        for<'a> &'a O: Into<&'a dyn ObjectInterface>,
        P: Into<PropertyKey>,
        V: Into<ECMAScriptValue>,
    {
        setup_test_agent();
        let (obj, p, v, receiver) = make_items();
        ordinary_set(&obj, p, v, &receiver)
            .map_err(unwind_any_error)
            .map(|b| (b, receiver.get(&"test_key".into()).unwrap().test_result_string()))
    }
}

#[test]
fn ordinary_set_with_own_descriptor_01() {
    // [[GetPrototypeOf]] throws
    setup_test_agent();
    let obj = TestObject::object(&[FunctionId::GetPrototypeOf]);
    let key = PropertyKey::from("a");

    let result = ordinary_set_with_own_descriptor(&obj, key, ECMAScriptValue::Undefined, &ECMAScriptValue::Null, None)
        .unwrap_err();
    assert_eq!(unwind_type_error(result), "[[GetPrototypeOf]] called on TestObject");
}
#[test]
fn ordinary_set_with_own_descriptor_02() {
    // If ownDesc is None, call [[Set]] on the parent. (We check by having the parent throw when we call its [[Set]].)
    setup_test_agent();
    let parent = TestObject::object(&[FunctionId::Set(None)]);
    let obj = ordinary_object_create(Some(parent), &[]);
    let key = PropertyKey::from("a");

    let result = ordinary_set_with_own_descriptor(&obj, key, ECMAScriptValue::Undefined, &ECMAScriptValue::Null, None)
        .unwrap_err();
    assert_eq!(unwind_type_error(result), "[[Set]] called on TestObject");
}
#[test]
fn ordinary_set_with_own_descriptor_03() {
    // ownDesc has writable:false; function should return false.
    setup_test_agent();
    let obj = ordinary_object_create(None, &[]);
    let own_desc = PropertyDescriptor {
        property: PropertyKind::Data(DataProperty { writable: false, value: ECMAScriptValue::Undefined }),
        enumerable: true,
        configurable: true,
        spot: 0,
    };
    let key = PropertyKey::from("a");
    let value = ECMAScriptValue::Undefined;
    let receiver = ECMAScriptValue::from(obj.clone());

    let result = ordinary_set_with_own_descriptor(&obj, key, value, &receiver, Some(own_desc)).unwrap();
    assert!(!result);
}
#[test]
fn ordinary_set_with_own_descriptor_04() {
    // Type(receiver) is not object -> return false
    setup_test_agent();
    let obj = ordinary_object_create(None, &[]);
    let key = PropertyKey::from("a");
    let value = ECMAScriptValue::Undefined;
    let receiver = ECMAScriptValue::from(999);

    let result = ordinary_set_with_own_descriptor(&obj, key, value, &receiver, None).unwrap();
    assert!(!result);
}
#[test]
fn ordinary_set_with_own_descriptor_05() {
    // receiver.[[GetOwnProperty]] throws
    setup_test_agent();
    let obj = TestObject::object(&[FunctionId::GetOwnProperty(None)]);
    let key = PropertyKey::from("a");
    let value = ECMAScriptValue::Undefined;
    let receiver = ECMAScriptValue::from(obj.clone());

    let result = ordinary_set_with_own_descriptor(&obj, key, value, &receiver, None).unwrap_err();
    assert_eq!(unwind_type_error(result), "[[GetOwnProperty]] called on TestObject");
}
#[test]
fn ordinary_set_with_own_descriptor_06() {
    // existing is an accessor
    setup_test_agent();
    let obj = ordinary_object_create(None, &[]);
    let key = PropertyKey::from("a");
    let value = ECMAScriptValue::Undefined;
    let receiver = ECMAScriptValue::from(obj.clone());
    obj.create_data_property("result", "sentinel value").unwrap();
    let getter =
        create_builtin_function(test_getter, false, 0_f64, key.clone(), &[], None, None, Some(JSString::from("get")));
    let accessor_prop = PotentialPropertyDescriptor {
        get: Some(ECMAScriptValue::from(getter)),
        enumerable: Some(true),
        configurable: Some(true),
        ..Default::default()
    };
    define_property_or_throw(&obj, key.clone(), accessor_prop).unwrap();
    let own_desc = PropertyDescriptor {
        property: PropertyKind::Data(DataProperty { writable: true, value: ECMAScriptValue::Undefined }),
        enumerable: true,
        configurable: true,
        spot: 0,
    };

    let result = ordinary_set_with_own_descriptor(&obj, key, value, &receiver, Some(own_desc)).unwrap();
    assert!(!result);
}
#[test]
fn ordinary_set_with_own_descriptor_07() {
    // existing is read-only
    setup_test_agent();
    let obj = ordinary_object_create(None, &[]);
    let key = PropertyKey::from("a");
    let value = ECMAScriptValue::Undefined;
    let receiver = ECMAScriptValue::from(obj.clone());
    let readonly = PotentialPropertyDescriptor {
        value: Some(ECMAScriptValue::from("read and weep")),
        writable: Some(false),
        enumerable: Some(true),
        configurable: Some(true),
        ..Default::default()
    };
    define_property_or_throw(&obj, key.clone(), readonly).unwrap();
    let own_desc = PropertyDescriptor {
        property: PropertyKind::Data(DataProperty { writable: true, value: ECMAScriptValue::Undefined }),
        enumerable: true,
        configurable: true,
        spot: 0,
    };

    let result = ordinary_set_with_own_descriptor(&obj, key, value, &receiver, Some(own_desc)).unwrap();
    assert!(!result);
}
#[test]
fn ordinary_set_with_own_descriptor_08() {
    // existing exists
    setup_test_agent();
    let obj = ordinary_object_create(None, &[]);
    let key = PropertyKey::from("a");
    let value = ECMAScriptValue::Undefined;
    let receiver = ECMAScriptValue::from(obj.clone());
    let previously = PotentialPropertyDescriptor {
        value: Some(ECMAScriptValue::from("initial")),
        writable: Some(true),
        enumerable: Some(true),
        configurable: Some(true),
        ..Default::default()
    };
    define_property_or_throw(&obj, key.clone(), previously).unwrap();
    let own_desc = PropertyDescriptor {
        property: PropertyKind::Data(DataProperty { writable: true, value: ECMAScriptValue::from(0) }),
        enumerable: true,
        configurable: true,
        spot: 0,
    };

    let result = ordinary_set_with_own_descriptor(&obj, key.clone(), value.clone(), &receiver, Some(own_desc)).unwrap();
    assert!(result);

    let item = obj.get(&key).unwrap();
    assert_eq!(item, value);
}
#[test]
fn ordinary_set_with_own_descriptor_09() {
    // existing does not exist
    setup_test_agent();
    let obj = ordinary_object_create(None, &[]);
    let key = PropertyKey::from("a");
    let value = ECMAScriptValue::from("test sentinel");
    let receiver = ECMAScriptValue::from(obj.clone());
    let own_desc = PropertyDescriptor {
        property: PropertyKind::Data(DataProperty { writable: true, value: ECMAScriptValue::from(0) }),
        enumerable: true,
        configurable: true,
        spot: 0,
    };

    let result = ordinary_set_with_own_descriptor(&obj, key.clone(), value.clone(), &receiver, Some(own_desc)).unwrap();
    assert!(result);

    let item = obj.get(&key).unwrap();
    assert_eq!(item, value);
}
fn test_setter(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // This is a setter; it is essentially:
    // function(val) { this.value = val; }
    let obj = to_object(this_value.clone())?;
    let key = PropertyKey::from("result");
    let mut args = arguments.iter();
    let val = args.next().cloned().unwrap_or(ECMAScriptValue::Undefined);
    obj.set(key, val, true)?;
    Ok(ECMAScriptValue::Undefined)
}
#[test]
fn ordinary_set_with_own_descriptor_10() {
    // own_desc is an accessor descriptor, with the above setter function
    setup_test_agent();
    let obj = ordinary_object_create(None, &[]);
    let key = PropertyKey::from("a");
    let value = ECMAScriptValue::from("test sentinel");
    let receiver = ECMAScriptValue::from(obj.clone());
    obj.create_data_property("result", "initial value").unwrap();
    let setter =
        create_builtin_function(test_setter, false, 1_f64, key.clone(), &[], None, None, Some(JSString::from("set")));
    let accessor_prop = PropertyDescriptor {
        property: PropertyKind::Accessor(AccessorProperty {
            get: ECMAScriptValue::Undefined,
            set: ECMAScriptValue::from(setter),
        }),
        enumerable: true,
        configurable: true,
        spot: 0,
    };

    let result = ordinary_set_with_own_descriptor(&obj, key, value.clone(), &receiver, Some(accessor_prop)).unwrap();
    assert!(result);

    let item = obj.get(&PropertyKey::from("result")).unwrap();
    assert_eq!(item, value);
}
#[test]
fn ordinary_set_with_own_descriptor_11() {
    // own_desc is an accessor descriptor, with a setter function that throws
    setup_test_agent();
    let obj = ordinary_object_create(None, &[]);
    let key = PropertyKey::from("a");
    let value = ECMAScriptValue::from("test sentinel");
    let receiver = ECMAScriptValue::from(obj.clone());
    let setter = intrinsic(IntrinsicId::ThrowTypeError);
    let accessor_prop = PropertyDescriptor {
        property: PropertyKind::Accessor(AccessorProperty {
            get: ECMAScriptValue::Undefined,
            set: ECMAScriptValue::from(setter),
        }),
        enumerable: true,
        configurable: true,
        spot: 0,
    };

    let result = ordinary_set_with_own_descriptor(&obj, key, value, &receiver, Some(accessor_prop)).unwrap_err();
    assert_eq!(unwind_type_error(result), "Generic TypeError");
}
#[test]
fn ordinary_set_with_own_descriptor_12() {
    // own_desc is an accessor descriptor, with an undefined setter function
    setup_test_agent();
    let obj = ordinary_object_create(None, &[]);
    let key = PropertyKey::from("a");
    let value = ECMAScriptValue::from("test sentinel");
    let receiver = ECMAScriptValue::from(obj.clone());
    let accessor_prop = PropertyDescriptor {
        property: PropertyKind::Accessor(AccessorProperty {
            get: ECMAScriptValue::Undefined,
            set: ECMAScriptValue::Undefined,
        }),
        enumerable: true,
        configurable: true,
        spot: 0,
    };

    let result = ordinary_set_with_own_descriptor(&obj, key, value, &receiver, Some(accessor_prop)).unwrap();
    assert!(!result);
}

mod ordinary_delete {
    use super::*;
    use test_case::test_case;

    #[allow(clippy::unnecessary_wraps)]
    fn steps(_: &ECMAScriptValue, _: Option<&Object>, _: &[ECMAScriptValue]) -> Completion<ECMAScriptValue> {
        Ok(ECMAScriptValue::Undefined)
    }

    #[test_case(
        || TestObject::object(&[FunctionId::GetOwnProperty(None)]), "a"
        => serr("TypeError: [[GetOwnProperty]] called on TestObject");
        "[[GetOwnProperty]] throws"
    )]
    #[test_case(
        || ordinary_object_create(None, &[]), "a"
        => Ok((true, "".to_string()));
        "property isn't actually there"
    )]
    #[test_case(
        || {
            let obj = ordinary_object_create(None, &[]);
            let ppd = PotentialPropertyDescriptor::new();
            define_property_or_throw(&obj, "key", ppd).unwrap();
            obj
        },
        "key"
        => Ok((false, "key:undefined (---)".to_string()));
        "property not configurable"
    )]
    #[test_case(
        || {
            let obj = ordinary_object_create(None, &[]);
            obj.create_data_property_or_throw("key", "normal").unwrap();
            obj.create_data_property_or_throw("alt", "sticks around").unwrap();
            obj
        },
        "key"
        => Ok((true, "alt:sticks around (wec)".to_string()));
        "normal property"
    )]
    #[test_case(|| OrdinaryObject::new(None, true), "key" => Ok((true, "".to_string())); "with OrdinaryObject")]
    #[test_case(
        || GeneratorObject::new(None, GeneratorState::Undefined, ""), "key"
        => Ok((true, "".to_string()));
        "with GeneratorObject"
    )]
    #[test_case(|| SymbolObject::new(None), "key" => Ok((true, "".to_string())); "with SymbolObject")]
    #[test_case(|| NumberObject::new(None), "key" => Ok((true, "".to_string())); "with NumberObject")]
    #[test_case(|| ErrorObject::new(None), "key" => Ok((true, "".to_string())); "with ErrorObject")]
    #[test_case(|| TestObject::new(None, &[]), "key" => Ok((true, "".to_string())); "with TestObject")]
    #[test_case(
        || ImmutablePrototypeExoticObject::new(None), "key"
        => Ok((true, "".to_string()));
        "with ImmutablePrototypeExoticObject"
    )]
    #[test_case(|| ArgumentsObject::new(None, None), "key" => Ok((true, "".to_string())); "with ArgumentsObject")]
    #[test_case(|| ArrayObject::new(None), "key" => Ok((true, "".to_string())); "with ArrayObject")]
    #[test_case(|| BooleanObject::new(None), "key" => Ok((true, "".to_string())); "with BooleanObject")]
    #[test_case(
        || ForInIteratorObject::new(None, intrinsic(IntrinsicId::Object)), "key"
        => Ok((true, "".to_string()));
        "with ForInIteratorObject"
    )]
    #[test_case(|| StringObject::new("".into(), None), "key" => Ok((true, "".to_string())); "with StringObject")]
    #[test_case(
        || AdaptableObject::new(None, &AdaptableMethods::default()), "key"
        => Ok((true, "".to_string()));
        "with AdaptableObject"
    )]
    #[test_case(
        || BuiltInFunctionObject::new(None, false, current_realm_record().unwrap(), None, steps, false), "key"
        => Ok((true, "".to_string()));
        "with BuiltInFunctionObject"
    )]
    #[test_case(
        || FunctionObject::new(
            None,
            current_realm_record().unwrap().borrow().global_env.clone().unwrap(),
            None,
            ParamSource::FormalParameters(Maker::new("").formal_parameters()),
            BodySource::Function(Maker::new("{}").function_body()),
            ConstructorKind::Base,
            current_realm_record().unwrap(),
            None,
            ThisMode::Lexical,
            true,
            None,
            "",
            vec![],
            vec![],
            ClassName::Empty,
            false,
            Rc::new(Chunk::new("test"))
        ),
        "key"
        => Ok((true, "".to_string()));
        "with FunctionObject"
    )]
    fn t<O>(make_obj: impl FnOnce() -> O, key: impl Into<PropertyKey>) -> Result<(bool, String), String>
    where
        for<'a> &'a O: Into<&'a dyn ObjectInterface>,
    {
        setup_test_agent();
        let obj = make_obj();
        let key = key.into();
        ordinary_delete(&obj, &key).map_err(unwind_any_error).map(|b| {
            (
                b,
                (&obj)
                    .into()
                    .common_object_data()
                    .borrow()
                    .propdump()
                    .into_iter()
                    .map(|info| info.to_string())
                    .join(","),
            )
        })
    }
}

mod ordinary_own_property_keys {
    use super::*;
    use test_case::test_case;

    #[allow(clippy::unnecessary_wraps)]
    fn steps(_: &ECMAScriptValue, _: Option<&Object>, _: &[ECMAScriptValue]) -> Completion<ECMAScriptValue> {
        Ok(ECMAScriptValue::Undefined)
    }
    #[test_case(
        || {
            let obj = ordinary_object_create(None, &[]);
            let sym1 = Symbol::new(Some(JSString::from("TestSymbol 1")));
            let sym2 = Symbol::new(Some(JSString::from("TestSymbol 2")));
            obj.create_data_property(sym1.clone(), ECMAScriptValue::Null).unwrap();
            obj.create_data_property("hillbilly", ECMAScriptValue::Null).unwrap();
            obj.create_data_property("automobile", ECMAScriptValue::Null).unwrap();
            obj.create_data_property("888", ECMAScriptValue::Null).unwrap();
            obj.create_data_property(sym2.clone(), ECMAScriptValue::Null).unwrap();
            obj.create_data_property("1002", ECMAScriptValue::Null).unwrap();
            obj.create_data_property("green", ECMAScriptValue::Null).unwrap();
            obj.create_data_property("-1", ECMAScriptValue::Null).unwrap();
            obj.create_data_property("0", ECMAScriptValue::Null).unwrap();
            obj
        }
        => svec(&[
                "0",
                "888",
                "1002",
                "hillbilly",
                "automobile",
                "green",
                "-1",
                "Symbol(TestSymbol 1)",
                "Symbol(TestSymbol 2)",
            ]);
        "many keys"
    )]
    #[test_case(|| ordinary_object_create(None, &[]) => svec(&[]); "empty object")]
    #[test_case(|| ArgumentsObject::new(None, None) => svec(&[]); "ArgumentsObject")]
    #[test_case(|| ErrorObject::new(None) => svec(&[]); "ErrorObject")]
    #[test_case(
        || BuiltInFunctionObject::new(None, false, current_realm_record().unwrap(), None, steps, false)
        => svec(&[]);
        "BuiltinFunctionObject")]
    #[test_case(|| OrdinaryObject::new(None, true) => svec(&[]); "OrdinaryObject")]
    #[test_case(|| AdaptableObject::new(None, &AdaptableMethods{..Default::default()}) => svec(&[]); "AdaptableObject")]
    #[test_case(
        || {
            FunctionObject::new(
                None,
                current_realm_record().unwrap().borrow().global_env.clone().unwrap(),
                None,
                ParamSource::FormalParameters(Maker::new("").formal_parameters()),
                BodySource::Function(Maker::new("{}").function_body()),
                ConstructorKind::Base,
                current_realm_record().unwrap(),
                None,
                ThisMode::Lexical,
                true,
                None,
                "",
                vec![],
                vec![],
                ClassName::Empty,
                false,
                Rc::new(Chunk::new("test"))
            )
        }
        => svec(&[]);
        "FunctionObject")]
    #[test_case(|| BooleanObject::new(None) => svec(&[]); "BooleanObject")]
    #[test_case(|| NumberObject::new(None) => svec(&[]); "NumberObject")]
    #[test_case(|| GeneratorObject::new(None, GeneratorState::Undefined, "") => svec(&[]); "GeneratorObject")]
    #[test_case(|| TestObject::new(None, &[]) => svec(&[]); "TestObject")]
    #[test_case(|| ArrayObject::new(None) => svec(&[]); "ArrayObject")]
    #[test_case(|| ImmutablePrototypeExoticObject::new(None) => svec(&[]); "ImmutablePrototypeExoticObject")]
    #[test_case(|| SymbolObject::new(None) => svec(&[]); "SymbolObject")]
    #[test_case(
        || ForInIteratorObject::new(None, intrinsic(IntrinsicId::StringPrototype))
        => svec(&[]);
        "ForInIteratorObject")]
    fn t<O>(make_obj: impl FnOnce() -> O) -> Vec<String>
    where
        for<'a> &'a O: Into<&'a dyn ObjectInterface>,
    {
        setup_test_agent();
        let obj = make_obj();
        ordinary_own_property_keys(&obj)
            .into_iter()
            .map(|k| ECMAScriptValue::from(k).test_result_string())
            .collect::<Vec<_>>()
    }
}

#[test]
fn array_index_key_01() {
    assert_eq!(array_index_key(&PropertyKey::from("981")), 981);
}
#[test]
#[should_panic(expected = "ParseIntError")]
fn array_index_key_02() {
    array_index_key(&PropertyKey::from("blip"));
}
#[test]
#[should_panic(expected = "unreachable code")]
fn array_index_key_03() {
    setup_test_agent();
    array_index_key(&PropertyKey::from(Symbol::new(None)));
}

#[test]
fn object_interface_to_boolean_obj() {
    setup_test_agent();
    let obj = ordinary_object_create(None, &[]);

    assert!(obj.o.to_boolean_obj().is_none());
}
#[test]
fn object_interface_to_function_obj() {
    setup_test_agent();
    let obj = ordinary_object_create(None, &[]);

    assert!(obj.o.to_function_obj().is_none());
}
#[test]
fn object_interface_to_callable_obj() {
    setup_test_agent();
    let obj = ordinary_object_create(None, &[]);

    assert!(obj.o.to_callable_obj().is_none());
}
#[test]
fn object_interface_to_builtin_function_obj() {
    setup_test_agent();
    let obj = ordinary_object_create(None, &[]);

    assert!(obj.o.to_builtin_function_obj().is_none());
}
#[test]
fn object_interface_is_arguments_object() {
    setup_test_agent();
    let obj = ordinary_object_create(None, &[]);

    assert!(!obj.o.is_arguments_object());
}
#[test]
fn object_interface_is_callable_obj() {
    setup_test_agent();
    let obj = ordinary_object_create(None, &[]);

    assert!(!obj.o.is_callable_obj());
}
#[test]
fn object_interface_is_error_object() {
    setup_test_agent();
    let obj = ordinary_object_create(None, &[]);

    assert!(!obj.o.is_error_object());
}
#[test]
fn object_interface_is_boolean_object() {
    setup_test_agent();
    let obj = ordinary_object_create(None, &[]);

    assert!(!obj.o.is_boolean_object());
}
#[test]
fn object_interface_is_number_object() {
    setup_test_agent();
    let obj = ordinary_object_create(None, &[]);

    assert!(!obj.o.is_number_object());
}
#[test]
fn object_interface_is_string_object() {
    setup_test_agent();
    let obj = ordinary_object_create(None, &[]);

    assert!(!obj.o.is_string_object());
}
#[test]
fn object_interface_is_date_object() {
    setup_test_agent();
    let obj = ordinary_object_create(None, &[]);

    assert!(!obj.o.is_date_object());
}
#[test]
fn object_interface_is_regexp_object() {
    setup_test_agent();
    let obj = ordinary_object_create(None, &[]);

    assert!(!obj.o.is_regexp_object());
}

#[test]
fn ordinary_object_create_01() {
    // When: An agent is given
    setup_test_agent();

    // Then requesting a new object with no prototype or extra slots
    let obj = ordinary_object_create(None, &[]);

    // Gives us the emptiest of all objects
    let data = obj.o.common_object_data().borrow();
    assert_eq!(data.prototype, None);
    assert_eq!(data.extensible, true);
    assert_eq!(data.properties.len(), 0);
}

#[test]
fn ordinary_object_create_02() {
    // When an agent and a prototype are provided
    setup_test_agent();
    let proto = ordinary_object_create(None, &[]);

    // Then requesting a new object with that prototype but no extra slots
    let obj = ordinary_object_create(Some(proto.clone()), &[]);

    // Gives us an empty object with its prototype slot filled.
    let data = obj.o.common_object_data().borrow();
    assert_eq!(data.prototype, Some(proto.clone()));
    assert_eq!(data.extensible, true);
    assert_eq!(data.properties.len(), 0);
    assert_ne!(obj, proto);
}

#[test]
fn ordinary_object_create_03a() {
    // When an agent and a prototype are provided
    setup_test_agent();
    let proto = ordinary_object_create(None, &[]);

    // Then requesting a new object with that prototype and needlessly requesting prototype or extensible slots
    let obj = ordinary_object_create(Some(proto.clone()), &[InternalSlotName::Prototype]);

    // Gives us an empty object with its prototype slot filled.
    let data = obj.o.common_object_data().borrow();
    assert_eq!(data.prototype.as_ref(), Some(&proto));
    assert_eq!(data.extensible, true);
    assert_eq!(data.properties.len(), 0);
    assert_ne!(&obj, &proto);
}
#[test]
fn ordinary_object_create_03b() {
    // When an agent and a prototype are provided
    setup_test_agent();
    let proto = ordinary_object_create(None, &[]);

    // Then requesting a new object with that prototype and needlessly requesting prototype or extensible slots
    let obj = ordinary_object_create(Some(proto.clone()), &[InternalSlotName::Extensible]);

    // Gives us an empty object with its prototype slot filled.
    let data = obj.o.common_object_data().borrow();
    assert_eq!(data.prototype.as_ref(), Some(&proto));
    assert_eq!(data.extensible, true);
    assert_eq!(data.properties.len(), 0);
    assert_ne!(&obj, &proto);
}
#[test]
fn ordinary_object_create_03c() {
    // When an agent and a prototype are provided
    setup_test_agent();
    let proto = ordinary_object_create(None, &[]);

    // Then requesting a new object with that prototype and needlessly requesting prototype or extensible slots
    let obj = ordinary_object_create(Some(proto.clone()), &[InternalSlotName::Prototype, InternalSlotName::Extensible]);

    // Gives us an empty object with its prototype slot filled.
    let data = obj.o.common_object_data().borrow();
    assert_eq!(data.prototype.as_ref(), Some(&proto));
    assert_eq!(data.extensible, true);
    assert_eq!(data.properties.len(), 0);
    assert_ne!(&obj, &proto);
}

#[test_case(&[InternalSlotName::Nonsense] => panics "Nonsense"; "all bad")]
#[test_case(&[InternalSlotName::Nonsense, InternalSlotName::Prototype, InternalSlotName::Extensible] => panics "Nonsense"; "one bad")]
#[test_case(ORDINARY_OBJECT_SLOTS => with |obj: Object| assert!(obj.o.is_plain_object()); "ordinary obj")]
#[test_case(BOOLEAN_OBJECT_SLOTS => with |obj: Object| assert!(obj.o.is_boolean_object()); "boolean obj")]
#[test_case(ERROR_OBJECT_SLOTS => with |obj: Object| assert!(obj.o.is_error_object()); "error obj")]
#[test_case(NUMBER_OBJECT_SLOTS => with |obj: Object| assert!(obj.o.is_number_object()); "number obj")]
#[test_case(ARRAY_OBJECT_SLOTS => with |obj: Object| assert!(obj.o.is_array_object()); "array obj")]
#[test_case(SYMBOL_OBJECT_SLOTS => with |obj: Object| assert!(obj.o.is_symbol_object()); "symbol obj")]
#[test_case(ARGUMENTS_OBJECT_SLOTS => panics "Additional info needed for arguments object; use direct constructor"; "args obj")]
#[test_case(FUNCTION_OBJECT_SLOTS => panics "More items are needed for initialization. Use FunctionObject::object directly instead"; "function obj")]
#[test_case(GENERATOR_OBJECT_SLOTS => panics "Additional info needed for generator object; use direct constructor"; "generator obj")]
fn make_basic_object(slots: &[InternalSlotName]) -> Object {
    setup_test_agent();
    super::make_basic_object(slots, None)
}

#[test]
fn get_prototype_of_01() {
    setup_test_agent();
    let obj = ordinary_object_create(None, &[]);
    let result = obj.o.get_prototype_of();
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), None);
}
#[test]
fn get_prototype_of_02() {
    setup_test_agent();
    let proto = ordinary_object_create(None, &[]);
    let obj = ordinary_object_create(Some(proto.clone()), &[]);
    let result = obj.o.get_prototype_of();
    assert!(result.is_ok());
    assert_eq!(result.unwrap().as_ref(), Some(&proto));
}

#[test]
fn set_prototype_of_01() {
    // Not changing an empty prototype
    setup_test_agent();
    let obj_a = ordinary_object_create(None, &[]);
    let result = obj_a.o.set_prototype_of(None);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), true);
    assert_eq!(obj_a.o.common_object_data().borrow().prototype, None);
}
#[test]
fn set_prototype_of_02() {
    // Not changing a Some() prototype
    setup_test_agent();
    let obj_a = ordinary_object_create(None, &[]);
    let obj_b = ordinary_object_create(Some(obj_a.clone()), &[]);
    let result = obj_b.o.set_prototype_of(Some(obj_a.clone()));
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), true);
    assert_eq!(obj_b.o.common_object_data().borrow().prototype.as_ref(), Some(&obj_a));
}
#[test]
fn set_prototype_of_03() {
    // Changing a Some() prototype to a different Some() prototype
    setup_test_agent();
    let proto = ordinary_object_create(None, &[]);
    let obj_b = ordinary_object_create(Some(proto), &[]);
    let new_proto = ordinary_object_create(None, &[]);
    let result = obj_b.o.set_prototype_of(Some(new_proto.clone()));
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), true);
    assert_eq!(obj_b.o.common_object_data().borrow().prototype.as_ref(), Some(&new_proto));
}
#[test]
fn set_prototype_of_04() {
    // Trying to make a prototype loop
    setup_test_agent();
    let proto = ordinary_object_create(None, &[]);
    let obj_b = ordinary_object_create(Some(proto.clone()), &[]);
    let result = proto.o.set_prototype_of(Some(obj_b));
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), false);
    assert_eq!(proto.o.common_object_data().borrow().prototype.as_ref(), None);
}
#[test]
fn set_prototype_of_05() {
    // Changing the prototype of an object that's not extensible
    setup_test_agent();
    let proto = ordinary_object_create(None, &[]);
    let obj_b = ordinary_object_create(Some(proto.clone()), &[]);
    obj_b.o.common_object_data().borrow_mut().extensible = false;
    let new_proto = ordinary_object_create(None, &[]);
    let result = obj_b.o.set_prototype_of(Some(new_proto));
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), false);
    assert_eq!(obj_b.o.common_object_data().borrow().prototype.as_ref(), Some(&proto));
}

#[test]
fn is_extensible_01() {
    setup_test_agent();
    let obj = ordinary_object_create(None, &[]);
    let result = obj.o.is_extensible();
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), true);
}
#[test]
fn is_extensible_02() {
    setup_test_agent();
    let obj = ordinary_object_create(None, &[]);
    obj.o.common_object_data().borrow_mut().extensible = false;
    let result = obj.o.is_extensible();
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), false);
}

#[test]
fn prevent_extensions_01() {
    setup_test_agent();
    let obj = ordinary_object_create(None, &[]);
    let result = obj.o.prevent_extensions();
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), true);
    assert_eq!(obj.o.common_object_data().borrow().extensible, false);
}

#[test]
fn get_own_property_01() {
    setup_test_agent();
    let obj = ordinary_object_create(None, &[]);
    let key = PropertyKey::String(JSString::from("blue"));
    let result = obj.o.get_own_property(&key);
    assert!(result.is_ok());
    assert!(result.unwrap().is_none());
}
#[test]
fn get_own_property_02() {
    setup_test_agent();
    let obj = ordinary_object_create(None, &[]);
    let key = PropertyKey::String(JSString::from("blue"));
    let value = ECMAScriptValue::Number(89.0);
    let desc = PotentialPropertyDescriptor {
        value: Some(value),
        writable: Some(false),
        enumerable: Some(true),
        configurable: None,
        get: None,
        set: None,
    };
    obj.o.define_own_property(key.clone(), desc).unwrap();

    let result = obj.o.get_own_property(&key);
    assert!(result.is_ok());
    let maybe_pd = result.unwrap();
    assert!(maybe_pd.is_some());
    let pd = maybe_pd.unwrap();
    assert_eq!(pd.configurable, false);
    assert_eq!(pd.enumerable, true);
    assert!(matches!(pd.property, PropertyKind::Data(_)));
    if let PropertyKind::Data(data) = pd.property {
        assert_eq!(data.value, ECMAScriptValue::Number(89.0));
        assert_eq!(data.writable, false);
    }
}

#[test]
fn set_and_get() {
    setup_test_agent();

    let obj = ordinary_object_create(None, &[]);
    let key = PropertyKey::String(JSString::from("blue"));
    let value = ECMAScriptValue::Number(56.7);

    obj.set(key.clone(), value, false).unwrap();
    let result = obj.get(&key).unwrap();

    assert_eq!(result, ECMAScriptValue::Number(56.7));
}

mod private_element_find {
    use super::*;
    use test_case::test_case;

    fn setup() -> (Object, Vec<PrivateName>) {
        setup_test_agent();
        let object_proto = intrinsic(IntrinsicId::ObjectPrototype);
        let obj = ordinary_object_create(Some(object_proto), &[]);

        let name1 = PrivateName::new("name1");
        let name2 = PrivateName::new("alice");
        let name3 = PrivateName::new("charley");
        let all_names = vec![name1, name2, name3];

        {
            let elements = &mut obj.o.common_object_data().borrow_mut().private_elements;
            elements.push(Rc::new(PrivateElement {
                key: all_names[0].clone(),
                kind: PrivateElementKind::Field { value: RefCell::new(ECMAScriptValue::from(1)) },
            }));
            elements.push(Rc::new(PrivateElement {
                key: all_names[1].clone(),
                kind: PrivateElementKind::Field { value: RefCell::new(ECMAScriptValue::from(2)) },
            }));
            elements.push(Rc::new(PrivateElement {
                key: all_names[2].clone(),
                kind: PrivateElementKind::Field { value: RefCell::new(ECMAScriptValue::from(3)) },
            }));
        }

        (obj, all_names)
    }

    #[test_case(0 => (true, true); "first")]
    #[test_case(1 => (true, true); "second")]
    #[test_case(2 => (true, true); "last")]
    fn normal(idx: usize) -> (bool, bool) {
        let (obj, names) = setup();
        let result = private_element_find(&obj, &names[idx]);
        let elem = result.unwrap();
        let keys_match = elem.key == names[idx];
        if let PrivateElementKind::Field { value } = &elem.kind {
            let values_match = *value.borrow() == ECMAScriptValue::from(idx + 1);
            (keys_match, values_match)
        } else {
            panic!("Bad element kind came back")
        }
    }

    #[test_case(&PrivateName::new("ice cream") => false; "not present")]
    fn missing(name: &PrivateName) -> bool {
        let (obj, _) = setup();
        let result = private_element_find(&obj, name);
        result.is_some()
    }
}

mod private_field_add {
    use super::*;
    use test_case::test_case;

    fn setup() -> (Object, Vec<PrivateName>) {
        let object_proto = intrinsic(IntrinsicId::ObjectPrototype);
        let obj = ordinary_object_create(Some(object_proto), &[]);

        let name1 = PrivateName::new("name1");
        let name2 = PrivateName::new("alice");
        let name3 = PrivateName::new("charley");
        let names = vec![name1, name2, name3];

        {
            let elements = &mut obj.o.common_object_data().borrow_mut().private_elements;
            elements.push(Rc::new(PrivateElement {
                key: names[0].clone(),
                kind: PrivateElementKind::Field { value: RefCell::new(ECMAScriptValue::from(1)) },
            }));
            elements.push(Rc::new(PrivateElement {
                key: names[1].clone(),
                kind: PrivateElementKind::Field { value: RefCell::new(ECMAScriptValue::from(2)) },
            }));
            elements.push(Rc::new(PrivateElement {
                key: names[2].clone(),
                kind: PrivateElementKind::Field { value: RefCell::new(ECMAScriptValue::from(3)) },
            }));
        }

        (obj, names)
    }

    #[test_case(&PrivateName::new("orange") => (true, Some(ECMAScriptValue::Null)); "orange")]
    fn normal(name: &PrivateName) -> (bool, Option<ECMAScriptValue>) {
        setup_test_agent();
        let (obj, _) = setup();

        let result = private_field_add(&obj, name.clone(), ECMAScriptValue::Null);

        (
            result.is_ok(),
            private_element_find(&obj, name).map(|pe| match &pe.kind {
                PrivateElementKind::Field { value } => value.borrow().clone(),
                _ => {
                    panic!("Bad element kind")
                }
            }),
        )
    }

    #[test_case(0; "first")]
    #[test_case(1; "second")]
    #[test_case(2; "last")]
    fn previously_added(idx: usize) {
        setup_test_agent();
        let (obj, names) = setup();

        let result = private_field_add(&obj, names[idx].clone(), ECMAScriptValue::Null).unwrap_err();
        assert_eq!(unwind_type_error(result), "PrivateName already defined");
    }
}

mod private_method_or_accessor_add {
    use super::*;
    //use test_case::test_case;

    fn setup() -> (Object, PrivateName) {
        let object_proto = intrinsic(IntrinsicId::ObjectPrototype);
        let obj = ordinary_object_create(Some(object_proto), &[]);

        let name = PrivateName::new("name1");

        {
            let elements = &mut obj.o.common_object_data().borrow_mut().private_elements;
            elements.push(Rc::new(PrivateElement {
                key: name.clone(),
                kind: PrivateElementKind::Field { value: RefCell::new(ECMAScriptValue::from(1)) },
            }));
        }
        (obj, name)
    }

    #[test]
    fn add() {
        setup_test_agent();
        let (obj, _) = setup();
        let key = PrivateName::new("orange");
        let method = Rc::new(PrivateElement {
            key: key.clone(),
            kind: PrivateElementKind::Method { value: ECMAScriptValue::from(100) },
        });

        private_method_or_accessor_add(&obj, method).unwrap();
        let x = private_element_find(&obj, &key).map(|pe| match &pe.kind {
            PrivateElementKind::Method { value } => value.clone(),
            _ => {
                panic!("Bad element kind")
            }
        });
        assert_eq!(x, Some(ECMAScriptValue::from(100)));
    }

    #[test]
    fn replace() {
        setup_test_agent();
        let (obj, key) = setup();
        let method =
            Rc::new(PrivateElement { key, kind: PrivateElementKind::Method { value: ECMAScriptValue::from(100) } });

        let err = private_method_or_accessor_add(&obj, method).unwrap_err();
        assert_eq!(unwind_type_error(err), "PrivateName already defined");
    }
}

mod private_get {
    use super::*;
    use test_case::test_case;

    fn setup() -> (Object, PrivateName, PrivateName, PrivateName, PrivateName) {
        let object_proto = intrinsic(IntrinsicId::ObjectPrototype);
        let obj = ordinary_object_create(Some(object_proto), &[]);

        let field_name = PrivateName::new("field");
        let method_name = PrivateName::new("method");
        let getter_name = PrivateName::new("getter");
        let nogetter_name = PrivateName::new("nogetter");

        private_field_add(&obj, field_name.clone(), ECMAScriptValue::from("FIELD")).unwrap();
        let method = PrivateElement {
            key: method_name.clone(),
            kind: PrivateElementKind::Method { value: ECMAScriptValue::from("METHOD") },
        };
        private_method_or_accessor_add(&obj, Rc::new(method)).unwrap();
        let getter_method = create_builtin_function(
            test_getter,
            false,
            0_f64,
            PropertyKey::from("getter"),
            &[],
            None,
            None,
            Some(JSString::from("get")),
        );
        let getter = PrivateElement {
            key: getter_name.clone(),
            kind: PrivateElementKind::Accessor { get: Some(getter_method), set: None },
        };
        private_method_or_accessor_add(&obj, Rc::new(getter)).unwrap();
        define_property_or_throw(
            &obj,
            PropertyKey::from("result"),
            PotentialPropertyDescriptor {
                value: Some(ECMAScriptValue::from("GETTER")),
                writable: Some(true),
                enumerable: Some(true),
                configurable: Some(true),
                ..Default::default()
            },
        )
        .unwrap();
        let nogetter =
            PrivateElement { key: nogetter_name.clone(), kind: PrivateElementKind::Accessor { get: None, set: None } };
        private_method_or_accessor_add(&obj, Rc::new(nogetter)).unwrap();

        (obj, field_name, method_name, getter_name, nogetter_name)
    }
    #[derive(Copy, Clone)]
    enum FieldName {
        Field,
        Method,
        Getter,
        NoGetter,
        Unavailable,
    }
    #[test_case(FieldName::Field => Ok(ECMAScriptValue::from("FIELD")); "field")]
    #[test_case(FieldName::Method => Ok(ECMAScriptValue::from("METHOD")); "method")]
    #[test_case(FieldName::Getter => Ok(ECMAScriptValue::from("GETTER")); "getter")]
    #[test_case(FieldName::NoGetter => Err(String::from("PrivateName has no getter")); "no getter")]
    #[test_case(FieldName::Unavailable => Err(String::from("PrivateName not defined")); "undefined")]
    fn f(field: FieldName) -> Result<ECMAScriptValue, String> {
        setup_test_agent();
        let (obj, field_name, method_name, getter_name, nogetter_name) = setup();

        let query = match field {
            FieldName::Field => field_name,
            FieldName::Method => method_name,
            FieldName::Getter => getter_name,
            FieldName::NoGetter => nogetter_name,
            FieldName::Unavailable => PrivateName::new("unavailable"),
        };
        private_get(&obj, &query).map_err(unwind_type_error)
    }
}

mod private_set {
    use super::*;
    use test_case::test_case;

    fn setup() -> (Object, PrivateName, PrivateName, PrivateName, PrivateName, PrivateName) {
        let object_proto = intrinsic(IntrinsicId::ObjectPrototype);
        let obj = ordinary_object_create(Some(object_proto), &[]);

        let field_name = PrivateName::new("field");
        let method_name = PrivateName::new("method");
        let setter_name = PrivateName::new("setter");
        let nosetter_name = PrivateName::new("nosetter");
        let broken_setter_name = PrivateName::new("brokenSetter");

        private_field_add(&obj, field_name.clone(), ECMAScriptValue::from("FIELD")).unwrap();
        let method = PrivateElement {
            key: method_name.clone(),
            kind: PrivateElementKind::Method { value: ECMAScriptValue::from("METHOD") },
        };
        private_method_or_accessor_add(&obj, Rc::new(method)).unwrap();
        let getter_method = create_builtin_function(
            test_getter,
            false,
            0_f64,
            PropertyKey::from("$state"),
            &[],
            None,
            None,
            Some(JSString::from("get")),
        );
        let setter_method = create_builtin_function(
            test_setter,
            false,
            1_f64,
            PropertyKey::from("$state"),
            &[],
            None,
            None,
            Some(JSString::from("set")),
        );

        let setter = PrivateElement {
            key: setter_name.clone(),
            kind: PrivateElementKind::Accessor { get: Some(getter_method), set: Some(setter_method) },
        };
        private_method_or_accessor_add(&obj, Rc::new(setter)).unwrap();
        define_property_or_throw(
            &obj,
            PropertyKey::from("result"),
            PotentialPropertyDescriptor {
                value: Some(ECMAScriptValue::from("SETTER")),
                writable: Some(true),
                enumerable: Some(true),
                configurable: Some(true),
                ..Default::default()
            },
        )
        .unwrap();
        let nosetter =
            PrivateElement { key: nosetter_name.clone(), kind: PrivateElementKind::Accessor { get: None, set: None } };
        private_method_or_accessor_add(&obj, Rc::new(nosetter)).unwrap();
        let broken_setter = PrivateElement {
            key: broken_setter_name.clone(),
            kind: PrivateElementKind::Accessor { get: None, set: Some(intrinsic(IntrinsicId::ThrowTypeError)) },
        };
        private_method_or_accessor_add(&obj, Rc::new(broken_setter)).unwrap();

        (obj, field_name, method_name, setter_name, nosetter_name, broken_setter_name)
    }
    #[derive(Clone, Copy)]
    enum FieldName {
        Field,
        Method,
        Setter,
        NoSetter,
        Unavailable,
        BrokenSetter,
    }

    #[test_case(FieldName::Field => Ok(ECMAScriptValue::from("NEW VALUE")); "field")]
    #[test_case(FieldName::Method => Err(String::from("PrivateName method may not be assigned")); "method")]
    #[test_case(FieldName::Setter => Ok(ECMAScriptValue::from("NEW VALUE")); "setter")]
    #[test_case(FieldName::NoSetter => Err(String::from("PrivateName has no setter")); "no-setter")]
    #[test_case(FieldName::Unavailable => Err(String::from("PrivateName not defined")); "undefined")]
    #[test_case(FieldName::BrokenSetter => serr("Generic TypeError"); "bad setter")]
    fn f(field: FieldName) -> Result<ECMAScriptValue, String> {
        setup_test_agent();
        let (obj, field_name, method_name, setter_name, nosetter_name, broken_setter_name) = setup();
        let new_value = ECMAScriptValue::from("NEW VALUE");
        let query = match field {
            FieldName::Field => field_name,
            FieldName::Method => method_name,
            FieldName::Setter => setter_name,
            FieldName::NoSetter => nosetter_name,
            FieldName::Unavailable => PrivateName::new("unavailable"),
            FieldName::BrokenSetter => broken_setter_name,
        };

        private_set(&obj, &query, new_value).map_err(unwind_type_error)?;

        Ok(private_get(&obj, &query).unwrap())
    }
}

mod from_property_descriptor {
    use super::*;
    use test_case::test_case;

    fn maybeprop(obj: &Object, key: impl Into<PropertyKey>) -> Option<ECMAScriptValue> {
        let key = key.into();
        if has_property(obj, &key).unwrap() {
            Some(obj.get(&key).unwrap())
        } else {
            None
        }
    }

    #[derive(Debug, PartialEq)]
    struct TestResult {
        value: Option<ECMAScriptValue>,
        writable: Option<ECMAScriptValue>,
        get: Option<ECMAScriptValue>,
        set: Option<ECMAScriptValue>,
        enumerable: Option<ECMAScriptValue>,
        configurable: Option<ECMAScriptValue>,
    }

    #[test_case(None::<PotentialPropertyDescriptor> => None; "undefined")]
    #[test_case(Some(PotentialPropertyDescriptor {
        value: None,
        writable: None,
        get: None,
        set: None,
        enumerable: None,
        configurable: None,
     }) => Some(TestResult {
        value: None,
        writable: None,
        get: None,
        set: None,
        enumerable: None,
        configurable: None,
     }) ; "empty")]
    #[test_case(Some(PropertyDescriptor {
        property: PropertyKind::Data( DataProperty { value: ECMAScriptValue::Null, writable: true } ),
        enumerable: true,
        configurable: true,
        spot: 0
    }) => Some(TestResult {
        value: Some(ECMAScriptValue::Null),
        writable: Some(ECMAScriptValue::from(true)),
        get: None,
        set: None,
        enumerable: Some(ECMAScriptValue::from(true)),
        configurable: Some(ECMAScriptValue::from(true)),
    }); "standard data")]
    #[test_case(Some(PropertyDescriptor {
        property: PropertyKind::Accessor( AccessorProperty { get: ECMAScriptValue::Undefined, set: ECMAScriptValue::from(10) } ),
        enumerable: false,
        configurable: true,
        spot: 0
    }) => Some(TestResult {
        value: None,
        writable: None,
        get: Some(ECMAScriptValue::Undefined),
        set: Some(ECMAScriptValue::from(10)),
        enumerable: Some(ECMAScriptValue::from(false)),
        configurable: Some(ECMAScriptValue::from(true)),
    }); "standard accessor")]
    fn happy<T>(desc: Option<T>) -> Option<TestResult>
    where
        T: Into<PotentialPropertyDescriptor>,
    {
        setup_test_agent();
        from_property_descriptor(desc).map(|o| TestResult {
            value: maybeprop(&o, "value"),
            writable: maybeprop(&o, "writable"),
            get: maybeprop(&o, "get"),
            set: maybeprop(&o, "set"),
            enumerable: maybeprop(&o, "enumerable"),
            configurable: maybeprop(&o, "configurable"),
        })
    }
}

mod to_property_descriptor {
    use super::*;
    use test_case::test_case;

    fn happy_data() -> ECMAScriptValue {
        let obj = ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)), &[]);
        obj.create_data_property_or_throw("value", "blue").unwrap();
        obj.create_data_property_or_throw("writable", true).unwrap();
        obj.create_data_property_or_throw("enumerable", true).unwrap();
        obj.create_data_property_or_throw("configurable", true).unwrap();
        ECMAScriptValue::from(obj)
    }
    fn fcn_data() -> ECMAScriptValue {
        let obj = ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)), &[]);
        obj.create_data_property_or_throw("get", ECMAScriptValue::Undefined).unwrap();
        obj.create_data_property_or_throw("set", ECMAScriptValue::Undefined).unwrap();
        obj.create_data_property_or_throw("enumerable", true).unwrap();
        obj.create_data_property_or_throw("configurable", true).unwrap();
        ECMAScriptValue::from(obj)
    }

    fn faux_errors(
        _this_value: &ECMAScriptValue,
        _new_target: Option<&Object>,
        _arguments: &[ECMAScriptValue],
    ) -> Completion<ECMAScriptValue> {
        Err(create_type_error("Test Sentinel"))
    }

    #[test_case(happy_data => PotentialPropertyDescriptor {
        value: Some(ECMAScriptValue::from("blue")),
        writable: Some(true),
        get: None,
        set: None,
        enumerable: Some(true),
        configurable: Some(true),
    }; "normal data")]
    #[test_case(fcn_data => PotentialPropertyDescriptor {
        value: None,
        writable: None,
        get: Some(ECMAScriptValue::Undefined),
        set: Some(ECMAScriptValue::Undefined),
        enumerable: Some(true),
        configurable: Some(true),
    }; "normal accessor")]
    fn happy(create_input: fn() -> ECMAScriptValue) -> PotentialPropertyDescriptor {
        setup_test_agent();
        let input = create_input();
        let result = to_property_descriptor(&input);
        result.unwrap()
    }

    fn create_hasprop_error(name: &str) -> ECMAScriptValue {
        ECMAScriptValue::from(TestObject::object(&[FunctionId::GetOwnProperty(Some(PropertyKey::from(name)))]))
    }
    fn create_getter_error(name: &str) -> ECMAScriptValue {
        let realm = current_realm_record().unwrap();
        let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let obj = ordinary_object_create(Some(object_prototype), &[]);
        let function_proto = intrinsic(IntrinsicId::FunctionPrototype);
        let key = PropertyKey::from(name);
        let getter = create_builtin_function(
            faux_errors,
            false,
            0_f64,
            key.clone(),
            BUILTIN_FUNCTION_SLOTS,
            Some(realm),
            Some(function_proto),
            Some(JSString::from("get")),
        );
        let desc = PotentialPropertyDescriptor {
            enumerable: Some(true),
            configurable: Some(true),
            get: Some(ECMAScriptValue::from(getter)),
            ..Default::default()
        };
        define_property_or_throw(&obj, key, desc).unwrap();
        ECMAScriptValue::from(obj)
    }
    fn create_nonfcn(name: &str) -> ECMAScriptValue {
        let obj = ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)), &[]);
        obj.create_data_property_or_throw(name, name).unwrap();
        ECMAScriptValue::from(obj)
    }

    #[test_case(|| ECMAScriptValue::Undefined => "Must be an object"; "non-object")]
    #[test_case(|| create_hasprop_error("enumerable") => "[[GetOwnProperty]] called on TestObject"; "enumerable has_property throws")]
    #[test_case(|| create_hasprop_error("configurable") => "[[GetOwnProperty]] called on TestObject"; "configurable has_property throws")]
    #[test_case(|| create_hasprop_error("value") => "[[GetOwnProperty]] called on TestObject"; "value has_property throws")]
    #[test_case(|| create_hasprop_error("writable") => "[[GetOwnProperty]] called on TestObject"; "writable has_property throws")]
    #[test_case(|| create_hasprop_error("get") => "[[GetOwnProperty]] called on TestObject"; "get has_property throws")]
    #[test_case(|| create_hasprop_error("set") => "[[GetOwnProperty]] called on TestObject"; "set has_property throws")]
    #[test_case(|| create_getter_error("enumerable") => "Test Sentinel"; "enumerable getter throws")]
    #[test_case(|| create_getter_error("configurable") => "Test Sentinel"; "configurable getter throws")]
    #[test_case(|| create_getter_error("value") => "Test Sentinel"; "value getter throws")]
    #[test_case(|| create_getter_error("writable") => "Test Sentinel"; "writable getter throws")]
    #[test_case(|| create_getter_error("get") => "Test Sentinel"; "get getter throws")]
    #[test_case(|| create_getter_error("set") => "Test Sentinel"; "set getter throws")]
    #[test_case(|| create_nonfcn("get") => "Getter must be callable (or undefined)"; "uncallable getter")]
    #[test_case(|| create_nonfcn("set") => "Setter must be callable (or undefined)"; "uncallable setter")]
    fn error(create_input: fn() -> ECMAScriptValue) -> String {
        setup_test_agent();
        let input = create_input();
        let result = to_property_descriptor(&input);
        unwind_type_error(result.unwrap_err())
    }
}

mod create_array_from_list {
    use super::*;
    use test_case::test_case;

    #[test_case(&[] => vec![
        PropertyInfo {
            name: PropertyKey::from("length"),
            enumerable: false,
            configurable: false,
            kind: PropertyInfoKind::Data {
                value: ECMAScriptValue::from(0.0),
                writable: true,
            }
        },
    ]; "empty list")]
    #[test_case(&[ECMAScriptValue::Undefined, ECMAScriptValue::from(true), ECMAScriptValue::from(-32111.342)] => vec![
        PropertyInfo {
            name: PropertyKey::from("length"),
            enumerable: false,
            configurable: false,
            kind: PropertyInfoKind::Data {
                value: ECMAScriptValue::from(3.0),
                writable: true,
            }
        },
        PropertyInfo {
            name: PropertyKey::from("0"),
            enumerable: true,
            configurable: true,
            kind: PropertyInfoKind::Data {
                value: ECMAScriptValue::Undefined,
                writable: true,
            }
        },
        PropertyInfo {
            name: PropertyKey::from("1"),
            enumerable: true,
            configurable: true,
            kind: PropertyInfoKind::Data {
                value: ECMAScriptValue::from(true),
                writable: true,
            }
        },
        PropertyInfo {
            name: PropertyKey::from("2"),
            enumerable: true,
            configurable: true,
            kind: PropertyInfoKind::Data {
                value: ECMAScriptValue::from(-32111.342),
                writable: true,
            }
        },
    ]; "some items")]
    fn cafl(items: &[ECMAScriptValue]) -> Vec<PropertyInfo> {
        setup_test_agent();
        create_array_from_list(items).o.common_object_data().borrow().propdump()
    }
}

mod enumerable_own_property_names {
    use super::*;
    use test_case::test_case;

    fn dead() -> Object {
        DeadObject::object()
    }
    fn normal() -> Object {
        let object_proto = intrinsic(IntrinsicId::ObjectPrototype);
        let obj = ordinary_object_create(Some(object_proto), &[]);
        obj.create_data_property_or_throw("one", 1.0).unwrap();
        obj.create_data_property_or_throw("three", 3.0).unwrap();
        let sym = Symbol::new(Some("two".into()));
        obj.create_data_property_or_throw(sym, 2.0).unwrap();
        define_property_or_throw(
            &obj,
            "hidden",
            PotentialPropertyDescriptor {
                value: Some("hidden".into()),
                writable: Some(true),
                enumerable: Some(false),
                configurable: Some(true),
                ..Default::default()
            },
        )
        .unwrap();
        obj
    }
    fn gop_override(this: &AdaptableObject, key: &PropertyKey) -> Completion<Option<PropertyDescriptor>> {
        if this.something.get() == 0 {
            this.something.set(1);
            Ok(ordinary_get_own_property(this, key))
        } else {
            Err(create_type_error("[[GetOwnProperty]] called more than once"))
        }
    }
    fn ownprop() -> Object {
        let obj = AdaptableObject::object(&AdaptableMethods {
            get_own_property_override: Some(gop_override),
            ..Default::default()
        });
        obj.create_data_property_or_throw("one", 1.0).unwrap();
        obj
    }
    fn getthrows() -> Object {
        let obj = TestObject::object(&[FunctionId::Get(None)]);
        obj.create_data_property_or_throw("one", 1.0).unwrap();
        obj
    }
    #[allow(clippy::unnecessary_wraps)]
    fn lying_ownprops(_: &AdaptableObject) -> Completion<Vec<PropertyKey>> {
        Ok(vec!["one".into(), "two".into(), "three".into()])
    }
    fn lyingkeys() -> Object {
        AdaptableObject::object(&AdaptableMethods {
            own_property_keys_override: Some(lying_ownprops),
            ..Default::default()
        })
    }

    #[test_case(dead, KeyValueKind::Key => Err("TypeError: own_property_keys called on DeadObject".to_string()); "own_property_keys throws")]
    #[test_case(normal, KeyValueKind::Key => Ok(vec!["one".into(), "three".into()]); "keys: normal object")]
    #[test_case(normal, KeyValueKind::Value => Ok(vec![1.0.into(), 3.0.into()]); "values: normal object")]
    #[test_case(ownprop, KeyValueKind::Value => Err("TypeError: [[GetOwnProperty]] called more than once".to_string()); "GetOwnProperty throws")]
    #[test_case(getthrows, KeyValueKind::Value => Err("TypeError: [[Get]] called on TestObject".to_string()); "get throws")]
    #[test_case(lyingkeys, KeyValueKind::Value => Ok(Vec::<ECMAScriptValue>::new()); "ownkeys lies")]
    fn f(make_obj: fn() -> Object, kind: KeyValueKind) -> Result<Vec<ECMAScriptValue>, String> {
        setup_test_agent();
        let obj = make_obj();
        enumerable_own_properties(&obj, kind).map_err(unwind_any_error)
    }

    #[test]
    fn keyvalue() {
        setup_test_agent();
        let obj = normal();
        let result = enumerable_own_properties(&obj, KeyValueKind::KeyValue).unwrap();
        assert_eq!(result.len(), 2);
        assert_eq!(result[0].get(&"0".into()).unwrap(), "one".into());
        assert_eq!(result[0].get(&"1".into()).unwrap(), 1.0.into());
        assert_eq!(result[0].get(&"length".into()).unwrap(), 2.0.into());
        assert_eq!(result[1].get(&"0".into()).unwrap(), "three".into());
        assert_eq!(result[1].get(&"1".into()).unwrap(), 3.0.into());
        assert_eq!(result[1].get(&"length".into()).unwrap(), 2.0.into());
    }
}

mod set_integrity_level {
    use super::*;
    use test_case::test_case;

    fn normal() -> Object {
        let proto = intrinsic(IntrinsicId::ObjectPrototype);
        let obj = ordinary_object_create(Some(proto), &[]);
        obj.create_data_property_or_throw("property", 67).unwrap();
        define_property_or_throw(
            &obj,
            "accessor",
            PotentialPropertyDescriptor {
                get: Some(ECMAScriptValue::Undefined),
                set: Some(ECMAScriptValue::Undefined),
                configurable: Some(true),
                enumerable: Some(true),
                ..Default::default()
            },
        )
        .unwrap();
        obj
    }
    fn dead() -> Object {
        DeadObject::object()
    }
    fn prevention_disabled() -> Object {
        AdaptableObject::object(&AdaptableMethods {
            prevent_extensions_override: Some(|_| Ok(false)),
            ..Default::default()
        })
    }
    fn opk_throws() -> Object {
        TestObject::object(&[FunctionId::OwnPropertyKeys])
    }
    fn dop_throws() -> Object {
        let obj = AdaptableObject::object(&AdaptableMethods {
            define_own_property_override: Some(|this, key, desc| {
                if this.something.get() == 0 {
                    this.something.set(1);
                    ordinary_define_own_property(this, key, desc)
                } else {
                    Err(create_type_error("Test Sentinel"))
                }
            }),
            ..Default::default()
        });
        obj.create_data_property_or_throw("property", 99.0).unwrap();
        obj
    }
    fn gop_override(this: &AdaptableObject, key: &PropertyKey) -> Completion<Option<PropertyDescriptor>> {
        if this.something.get() == 0 {
            this.something.set(1);
            Ok(ordinary_get_own_property(this, key))
        } else {
            Err(create_type_error("[[GetOwnProperty]] called more than once"))
        }
    }
    fn gop_throws() -> Object {
        let obj = AdaptableObject::object(&AdaptableMethods {
            get_own_property_override: Some(gop_override),
            ..Default::default()
        });
        obj.create_data_property_or_throw("one", 1.0).unwrap();
        obj
    }
    #[allow(clippy::unnecessary_wraps)]
    fn lying_ownprops(_: &AdaptableObject) -> Completion<Vec<PropertyKey>> {
        Ok(vec!["one".into(), "two".into(), "three".into()])
    }
    fn lyingkeys() -> Object {
        AdaptableObject::object(&AdaptableMethods {
            own_property_keys_override: Some(lying_ownprops),
            ..Default::default()
        })
    }

    #[test_case(normal, IntegrityLevel::Frozen => Ok((true, vec![
        PropertyInfo {
            name: "property".into(),
            kind: PropertyInfoKind::Data {
                value: 67.0.into(),
                writable: false,
            },
            enumerable: true,
            configurable: false,
        },
        PropertyInfo {
            name: "accessor".into(),
            kind: PropertyInfoKind::Accessor {
                getter: ECMAScriptValue::Undefined,
                setter: ECMAScriptValue::Undefined,
            },
            enumerable: true,
            configurable: false,
        }
    ])); "frozen ordinary")]
    #[test_case(normal, IntegrityLevel::Sealed => Ok((true, vec![
        PropertyInfo {
            name: "property".into(),
            kind: PropertyInfoKind::Data {
                value: 67.0.into(),
                writable: true,
            },
            enumerable: true,
            configurable: false,
        },
        PropertyInfo {
            name: "accessor".into(),
            kind: PropertyInfoKind::Accessor {
                getter: ECMAScriptValue::Undefined,
                setter: ECMAScriptValue::Undefined,
            },
            enumerable: true,
            configurable: false,
        }
    ])); "sealed ordinary")]
    #[test_case(dead, IntegrityLevel::Frozen => Err("TypeError: prevent_extensions called on DeadObject".to_string()); "prevent_extensions throws")]
    #[test_case(prevention_disabled, IntegrityLevel::Sealed => Ok((false, Vec::<PropertyInfo>::new())); "prevent_extensions returns false")]
    #[test_case(opk_throws, IntegrityLevel::Sealed => Err("TypeError: [[OwnPropertyKeys]] called on TestObject".to_string()); "OwnPropertyKeys throws")]
    #[test_case(dop_throws, IntegrityLevel::Sealed => Err("TypeError: Test Sentinel".to_string()); "Sealed: DefineOwn throws")]
    #[test_case(dop_throws, IntegrityLevel::Frozen => Err("TypeError: Test Sentinel".to_string()); "Frozen: DefineOwn throws")]
    #[test_case(gop_throws, IntegrityLevel::Frozen => Err("TypeError: [[GetOwnProperty]] called more than once".to_string()); "GetOwnProp throws")]
    #[test_case(lyingkeys, IntegrityLevel::Frozen => Ok((true, Vec::<PropertyInfo>::new())); "lying own property keys")]
    fn sil(make_obj: fn() -> Object, level: IntegrityLevel) -> Result<(bool, Vec<PropertyInfo>), String> {
        setup_test_agent();
        let obj = make_obj();
        set_integrity_level(&obj, level)
            .map(|success| (success, obj.o.common_object_data().borrow().propdump()))
            .map_err(unwind_any_error)
    }
}

mod ordinary_has_instance {
    use super::*;
    use test_case::test_case;

    type ValueMaker = fn() -> ECMAScriptValue;

    fn undef() -> ECMAScriptValue {
        ECMAScriptValue::Undefined
    }
    fn bool_class() -> ECMAScriptValue {
        let boolean = intrinsic(IntrinsicId::Boolean);
        ECMAScriptValue::from(boolean)
    }
    fn basic_constructor() -> Object {
        let realm = current_realm_record();
        let function_prototype = intrinsic(IntrinsicId::FunctionPrototype);
        create_builtin_function(
            throw_type_error,
            true,
            0_f64,
            PropertyKey::from("TestConstructor"),
            BUILTIN_FUNCTION_SLOTS,
            realm,
            Some(function_prototype),
            None,
        )
    }
    fn ungettable_prototype() -> ECMAScriptValue {
        let throw = ECMAScriptValue::from(intrinsic(IntrinsicId::ThrowTypeError));
        let constructor = basic_constructor();
        define_property_or_throw(
            &constructor,
            "prototype",
            PotentialPropertyDescriptor::new().set(throw.clone()).get(throw).enumerable(false).configurable(false),
        )
        .unwrap();

        ECMAScriptValue::from(constructor)
    }
    fn nonobject_prototype() -> ECMAScriptValue {
        let constructor = basic_constructor();
        define_property_or_throw(
            &constructor,
            "prototype",
            PotentialPropertyDescriptor::new()
                .writable(false)
                .value(ECMAScriptValue::Undefined)
                .enumerable(false)
                .configurable(false),
        )
        .unwrap();

        ECMAScriptValue::from(constructor)
    }
    fn dead_object() -> ECMAScriptValue {
        ECMAScriptValue::from(DeadObject::object())
    }
    fn empty_object() -> ECMAScriptValue {
        let obj_proto = intrinsic(IntrinsicId::ObjectPrototype);
        ECMAScriptValue::from(ordinary_object_create(Some(obj_proto), &[]))
    }
    fn bool_child() -> ECMAScriptValue {
        let bool_constructor = intrinsic(IntrinsicId::Boolean);
        bool_constructor
            .ordinary_create_from_constructor(IntrinsicId::BooleanPrototype, BOOLEAN_OBJECT_SLOTS)
            .unwrap()
            .into()
    }
    fn bool_grandchild() -> ECMAScriptValue {
        let bool_constructor = intrinsic(IntrinsicId::Boolean);
        let bool_child = bool_constructor
            .ordinary_create_from_constructor(IntrinsicId::BooleanPrototype, BOOLEAN_OBJECT_SLOTS)
            .unwrap();
        let grandkid = ordinary_object_create(Some(bool_child), &[]);
        grandkid.into()
    }

    #[test_case(undef, empty_object => Ok(false); "c not callable")]
    #[test_case(bool_class, undef => Ok(false); "o not an object")]
    #[test_case(ungettable_prototype, empty_object => serr("TypeError: Generic TypeError"); "broken prototype on class")]
    #[test_case(nonobject_prototype, empty_object => serr("TypeError: Bad prototype chain in 'instanceof'"); "nonobject prototype on class")]
    #[test_case(bool_class, dead_object => serr("TypeError: get_prototype_of called on DeadObject"); "broken prototype on object")]
    #[test_case(bool_class, empty_object => Ok(false); "legit false return")]
    #[test_case(bool_class, bool_child => Ok(true); "true instance via prototype chain (child)")]
    #[test_case(bool_class, bool_grandchild => Ok(true); "true instance via prototype chain (grandchild)")]
    fn ordinary_has_instance(make_c: ValueMaker, make_o: ValueMaker) -> Result<bool, String> {
        setup_test_agent();
        let c = make_c();
        let o = make_o();

        super::ordinary_has_instance(&c, &o).map_err(unwind_any_error)
    }
}

mod internal_slot_name {
    use super::*;
    use ahash::RandomState;
    use test_case::test_case;

    #[test_case(InternalSlotName::Prototype, InternalSlotName::Extensible => false; "not equal")]
    #[test_case(InternalSlotName::Prototype, InternalSlotName::Prototype => true; "equal")]
    fn eq(left: InternalSlotName, right: InternalSlotName) -> bool {
        left == right
    }

    #[test]
    fn debug() {
        assert_ne!(format!("{:?}", InternalSlotName::Prototype), "");
    }

    #[test]
    #[allow(clippy::clone_on_copy)]
    fn clone() {
        let a = InternalSlotName::Extensible;
        let b = a.clone();
        assert_eq!(a, b);
    }

    #[test]
    fn hash() {
        let factory = RandomState::new();

        let name_a = InternalSlotName::Prototype;
        let name_b = InternalSlotName::NumberData;
        let name_c = InternalSlotName::Prototype;

        let hash_a = calculate_hash(&factory, &name_a);
        let hash_b = calculate_hash(&factory, &name_b);
        let hash_c = calculate_hash(&factory, &name_c);

        assert_eq!(hash_a, hash_c);
        assert_ne!(hash_a, hash_b);
        assert_ne!(hash_b, hash_c);
    }
}

#[test_case(DeadObject::object => serr("TypeError: get called on DeadObject"); "get throws")]
#[test_case(|| {
        let o = ordinary_object_create(None, &[]);
        let sym = wks(WksId::Unscopables);
        o.create_data_property_or_throw("length", sym).unwrap();
        o
    } => serr("TypeError: Symbol values cannot be converted to Number values"); "tolength throws")]
#[test_case(|| array_create(0, None).unwrap() => Ok(0); "empty array")]
fn length_of_array_like(make_obj: impl FnOnce() -> Object) -> Result<i64, String> {
    setup_test_agent();
    let obj = make_obj();
    super::length_of_array_like(&obj).map_err(unwind_any_error)
}

mod object {
    use super::*;
    use test_case::test_case;

    #[test_case(|| ordinary_object_create(None, &[]) => false; "not")]
    fn is_typed_array(make_obj: impl FnOnce() -> Object) -> bool {
        setup_test_agent();
        let obj = make_obj();
        obj.is_typed_array()
    }

    mod concise {
        use super::*;
        use test_case::test_case;

        struct Wrapper(Object);
        impl fmt::Debug for Wrapper {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                self.0.concise(f)
            }
        }

        #[test_case(|| ordinary_object_create(None, &[]) => (true, 1); "simple obj")]
        fn t(make_obj: impl FnOnce() -> Object) -> (bool, usize) {
            let output = format!("{:?}", Wrapper(make_obj()));
            let lines = output.lines().collect::<Vec<_>>();
            (!output.is_empty(), lines.len())
        }
    }

    #[test]
    fn debug() {
        setup_test_agent();
        let obj = ordinary_object_create(None, &[]);
        let result = format!("{obj:?}");
        assert_ne!(result, "");
    }

    #[test]
    fn display() {
        lazy_static! {
            static ref MATCH: Regex = Regex::new("^<Object [0-9]+>$").expect("Valid regex");
        }
        setup_test_agent();
        let obj = ordinary_object_create(None, &[]);
        let result = format!("{obj}");
        assert!(MATCH.is_match(&result));
    }

    mod try_from {
        use super::*;
        use test_case::test_case;

        #[test_case(|| ECMAScriptValue::Undefined => serr("Only object values may be converted to true objects"); "not object")]
        #[test_case(
            || {
                let obj = ordinary_object_create(None, &[]);
                obj.set("key_1", "value_1", true).unwrap();
                obj.set("key_2", "value_2", true).unwrap();
                obj.into()
            }
            => Ok("key_1:value_1,key_2:value_2".to_string());
            "is object")]
        fn value(make_val: impl FnOnce() -> ECMAScriptValue) -> Result<String, String> {
            setup_test_agent();
            Object::try_from(make_val())
                .map_err(|e| e.to_string())
                .map(|o| ECMAScriptValue::from(o).test_result_string())
        }

        #[test_case(|| ECMAScriptValue::Undefined
            => serr("Only object values may be converted to true objects");
            "not object")]
        #[test_case(
            || {
                let obj = ordinary_object_create(None, &[]);
                obj.set("key_1", "value_1", true).unwrap();
                obj.set("key_2", "value_2", true).unwrap();
                obj.into()
            }
            => Ok("key_1:value_1,key_2:value_2".to_string());
            "is object")]
        fn value_ref(make_val: impl FnOnce() -> ECMAScriptValue) -> Result<String, String> {
            setup_test_agent();
            Object::try_from(&make_val())
                .map_err(ToString::to_string)
                .map(|o| ECMAScriptValue::from(o).test_result_string())
        }
    }

    #[test_case(
        || {
            let obj = ordinary_object_create(None, &[]);
            (obj.clone(), obj)
        }
        => true; "equal")]
    #[test_case(
        || {
            let obj_1 = ordinary_object_create(None, &[]);
            let obj_2 = ordinary_object_create(None, &[]);
            (obj_1, obj_2)
        }
        => false; "unequal")]
    fn eq(make_objects: impl FnOnce() -> (Object, Object)) -> bool {
        setup_test_agent();
        let (left, right) = make_objects();
        left == right
    }

    #[test]
    fn clone() {
        setup_test_agent();
        let obj = ordinary_object_create(None, &[]);
        let second = obj.clone();
        assert_eq!(obj, second);
    }

    #[test]
    fn new() {
        let o = Object::new(None, true);
        assert!(o.o.get_prototype_of().unwrap().is_none());
        assert!(o.o.is_extensible().unwrap());
        assert!(o.o.is_plain_object());
    }

    #[test_case(
        || ordinary_object_create(None, &[])
        => Ok(false);
        "plain object")]
    #[test_case(
        || create_array_from_list(&[ECMAScriptValue::from(67), ECMAScriptValue::from(91)])
        => Ok(true);
        "true array")]
    #[test_case(
        || ProxyObject::object(None)
        => serr("TypeError: Proxy has been revoked");
        "revoked proxy")]
    #[test_case(
        || ProxyObject::object(Some((create_array_from_list(&[]), ordinary_object_create(None, &[]))))
        => Ok(true);
        "proxy on array")]
    #[test_case(
        || ProxyObject::object(Some((ordinary_object_create(None, &[]), ordinary_object_create(None, &[]))))
        => Ok(false);
        "proxy on plain object")]
    fn is_array(make_obj: impl FnOnce() -> Object) -> Result<bool, String> {
        setup_test_agent();
        make_obj().is_array().map_err(unwind_any_error)
    }

    #[test_case(
        || {
            let obj = ordinary_object_create(None, &[]);
            obj.create_data_property_or_throw("test_key", "test_value").unwrap();
            obj
        },
        || PropertyKey::from("test_key")
        => sok("test_value");
        "successful get"
    )]
    fn get(make_obj: impl FnOnce() -> Object, make_key: impl FnOnce() -> PropertyKey) -> Result<String, String> {
        setup_test_agent();
        let key = make_key();
        make_obj().get(&key).map_err(unwind_any_error).map(|v| v.test_result_string())
    }

    mod create_data_property {
        use super::*;
        use test_case::test_case;

        fn make_regex_validator(expected_flag: bool, regex: &str) -> impl Fn(Result<(bool, String), String>) + '_ {
            move |actual: Result<(bool, String), String>| {
                let re = Regex::new(regex).unwrap();
                match &actual {
                    Err(_) => panic!("Saw an Err when Ok was expected"),
                    Ok((flag, msg)) => {
                        assert!(*flag == expected_flag, "Expected success flag to be {expected_flag:?}");
                        assert!(re.is_match(msg));
                    }
                }
            }
        }

        fn empty_obj() -> Object {
            ordinary_object_create(None, &[])
        }
        #[test_case(
            empty_obj,
            || PropertyKey::from("key"),
            || ECMAScriptValue::from("value")
            => Ok((true, "key:value".to_string()));
            "PropertyKey:ECMAScriptValue")]
        #[test_case(
            empty_obj,
            || 45_usize,
            || "value"
            => Ok((true, "45:value".to_string()));
            "usize:&str")]
        #[test_case(
            empty_obj,
            || 45_usize,
            || ECMAScriptValue::from("value")
            => Ok((true, "45:value".to_string()));
            "usize:ECMAScriptValue")]
        #[test_case(
            empty_obj,
            || "test_key",
            || 32_i32
            => Ok((true, "test_key:32".to_string()));
            "&str:i32")]
        #[test_case(empty_obj, || "test_key", || true => Ok((true, "test_key:true".to_string())); "&str:bool")]
        #[test_case(
            empty_obj,
            || JSString::from("test_key"),
            || ECMAScriptValue::from("value")
            => Ok((true, "test_key:value".to_string()));
            "JSString:ECMAScriptValue")]
        #[test_case(
            empty_obj, || wks(WksId::ToStringTag), || 0.125
            => Ok((true, "Symbol(Symbol.toStringTag):0.125".to_string()));
            "Symbol:f64")]
        #[test_case(
            empty_obj, || PropertyKey::from("test_key"), || "value"
            => Ok((true, "test_key:value".to_string()));
            "PropertyKey:&str")]
        #[test_case(
            empty_obj, || String::from("test_key"), || ECMAScriptValue::from("value")
            => Ok((true, "test_key:value".to_string()));
            "String:ECMAScriptValue")]
        #[test_case(
            empty_obj, || wks(WksId::ToStringTag), || "value"
            => Ok((true, "Symbol(Symbol.toStringTag):value".to_string()));
            "Symbol:&str")]
        #[test_case(
            empty_obj,
            || "test_key",
            || {
                let obj = ordinary_object_create(None, &[]);
                let ppd = PotentialPropertyDescriptor::new().value("value");
                define_property_or_throw(&obj, "test_key", ppd).unwrap();
                obj
            } => using make_regex_validator(true, "test_key:<Object [0-9]+>");
            "&str:Object")]
        #[test_case(
            empty_obj,
            || PropertyKey::from("test_key"),
            || {
                let obj = ordinary_object_create(None, &[]);
                let ppd = PotentialPropertyDescriptor::new().value("value");
                define_property_or_throw(&obj, "test_key", ppd).unwrap();
                obj
            } => using make_regex_validator(true, "test_key:<Object [0-9]+>");
            "PropertyKey:Object")]
        #[test_case(
            empty_obj, || wks(WksId::ToStringTag), || ECMAScriptValue::from("value")
            => Ok((true, "Symbol(Symbol.toStringTag):value".to_string()));
            "Symbol:ECMAScriptValue")]
        #[test_case(empty_obj, || "test_key", || "value" => Ok((true, "test_key:value".to_string())); "&str:&str")]
        #[test_case(
            empty_obj, || "test_key", || ECMAScriptValue::from("value")
            => Ok((true, "test_key:value".to_string()));
            "&str:ECMAScriptValue")]
        #[test_case(empty_obj, || "test_key", || 0.125 => Ok((true, "test_key:0.125".to_string())); "&str:f64")]
        #[test_case(
            empty_obj, || "test_key", || wks(WksId::ToStringTag)
            => Ok((true, "test_key:Symbol(Symbol.toStringTag)".to_string()));
            "&str:Symbol")]
        fn t<K, V>(
            make_obj: impl FnOnce() -> Object,
            make_key: impl FnOnce() -> K,
            make_value: impl FnOnce() -> V,
        ) -> Result<(bool, String), String>
        where
            K: Into<PropertyKey>,
            V: Into<ECMAScriptValue>,
        {
            setup_test_agent();
            let obj = make_obj();
            let key = make_key();
            let value = make_value();
            obj.create_data_property(key, value)
                .map_err(unwind_any_error)
                .map(|b| (b, ECMAScriptValue::from(obj).test_result_string()))
        }
    }

    mod create_data_property_or_throw {
        use super::*;
        use test_case::test_case;

        #[test_case(
            || (ordinary_object_create(None, &[]), PropertyKey::from("test_key"), ECMAScriptValue::from("value"))
            => sok("test_key:value");
            "no conversion; success")]
        #[test_case(
            || {
                let obj = ordinary_object_create(None, &[]);
                obj.o.prevent_extensions().unwrap();
                (obj, PropertyKey::from("test_key"), ECMAScriptValue::from("value"))
            }
            => serr("TypeError: Unable to create data property");
            "no conversions; error")]
        #[test_case(
            || (
                TestObject::object(&[FunctionId::DefineOwnProperty(None)]),
                PropertyKey::from("key"),
                ECMAScriptValue::from("val")
            )
            => serr("TypeError: [[DefineOwnProperty]] called on TestObject");
            "no conversions; define own throws")]
        #[test_case(
            || (ordinary_object_create(None, &[]), JSString::from("test_key"), ECMAScriptValue::from("value"))
            => sok("test_key:value");
            "JSString:ECMAScriptValue")]
        #[test_case(
            || (ordinary_object_create(None, &[]), "test_key", "value")
            => sok("test_key:value");
            "&str:&str")]
        #[test_case(
            || (ordinary_object_create(None, &[]), "test_key", 0.125)
            => sok("test_key:0.125");
            "&str:f64")]
        #[test_case(
            || (ordinary_object_create(None, &[]), "test_key", 1_i32)
            => sok("test_key:1");
            "&str:i32")]
        #[test_case(
            || (ordinary_object_create(None, &[]), "test_key", true)
            => sok("test_key:true");
            "&str:bool")]
        #[test_case(
            || (ordinary_object_create(None, &[]), "test_key", intrinsic(IntrinsicId::Object))
            => sok("test_key:function Object");
            "&str:Object")]
        #[test_case(
            || (ordinary_object_create(None, &[]), PropertyKey::from("test_key"), 0.125)
            => sok("test_key:0.125");
            "PropertyKey:f64")]
        #[test_case(
            || (ordinary_object_create(None, &[]), "test_key", wks(WksId::ToStringTag))
            => sok("test_key:Symbol(Symbol.toStringTag)");
            "&str:Symbol")]
        #[test_case(
            || (ordinary_object_create(None, &[]), "test_key", ECMAScriptValue::from("value"))
            => sok("test_key:value");
            "&str:ECMAScriptValue")]
        #[test_case(
            || (ordinary_object_create(None, &[]), PropertyKey::from("test_key"), intrinsic(IntrinsicId::Object))
            => sok("test_key:function Object");
            "PropertyKey:Object")]
        #[test_case(
            || (ordinary_object_create(None, &[]), PropertyKey::from("test_key"), "value")
            => sok("test_key:value");
            "PropertyKey:&str")]
        #[test_case(
            || (ordinary_object_create(None, &[]), 1010_usize, "value")
            => sok("1010:value");
            "usize:&str")]
        #[test_case(
            || (ordinary_object_create(None, &[]), 1010_usize, ECMAScriptValue::from("value"))
            => sok("1010:value");
            "usize:ECMAScriptValue")]
        #[test_case(
            || (ordinary_object_create(None, &[]), wks(WksId::ToStringTag), 0.125)
            => sok("Symbol(Symbol.toStringTag):0.125");
            "Symbol:f64")]
        #[test_case(
            || (ordinary_object_create(None, &[]), wks(WksId::ToStringTag), "value")
            => sok("Symbol(Symbol.toStringTag):value");
            "Symbol:&str")]
        #[test_case(
            || (ordinary_object_create(None, &[]), String::from("test_key"), ECMAScriptValue::from("value"))
            => sok("test_key:value");
            "String:ECMAScriptValue")]
        fn t<P, V>(make_items: impl FnOnce() -> (Object, P, V)) -> Result<String, String>
        where
            P: Into<PropertyKey>,
            V: Into<ECMAScriptValue>,
        {
            setup_test_agent();
            let (obj, key, value) = make_items();
            obj.create_data_property_or_throw(key, value)
                .map_err(unwind_any_error)
                .map(|()| ECMAScriptValue::from(obj).test_result_string())
        }
    }

    #[test_case(|| (ordinary_object_create(None, &[]), "value"), "key", true => Ok(true); "simple set")]
    #[test_case(
        || (TestObject::object(&[FunctionId::Set(Some("key".into()))]), "value"), "key", true
        => serr("TypeError: [[Set]] called on TestObject");
        "[[set]] fails"
    )]
    #[test_case(
        || ({
            let obj = ordinary_object_create(None, &[]);
            obj.o.prevent_extensions().unwrap();
            obj
        }, "value"),
        "key",
        true
        => serr("TypeError: Cannot add property, for one of many different possible reasons");
        "setting returned false"
    )]
    #[test_case(|| (intrinsic(IntrinsicId::Object), "val"), "str", true => Ok(true); "&str:&str")]
    #[test_case(|| (intrinsic(IntrinsicId::Object), String::from("string")), "str", true => Ok(true); "&str:String")]
    #[test_case(|| (intrinsic(IntrinsicId::Object), true), "str", true => Ok(true); "&str:bool")]
    #[test_case(|| (intrinsic(IntrinsicId::Object), 27_i32), "str", true => Ok(true); "&str:i32")]
    #[test_case(
        || (intrinsic(IntrinsicId::Object), intrinsic(IntrinsicId::Object)), "str", true
        => Ok(true);
        "&str:Object"
    )]
    #[test_case(
        || (intrinsic(IntrinsicId::Object), ECMAScriptValue::Null), "str", true
        => Ok(true);
        "&str:ECMAScriptValue"
    )]
    #[test_case(|| (intrinsic(IntrinsicId::Object), wks(WksId::ToStringTag)), "str", true => Ok(true); "&str:Symbol")]
    #[test_case(|| (intrinsic(IntrinsicId::Object), 27_u32), "str", true => Ok(true); "&str:u32")]
    #[test_case(
        || (intrinsic(IntrinsicId::Object), ECMAScriptValue::Null), JSString::from("jsstring"), true
        => Ok(true);
        "JSString:ECMAScriptValue"
    )]
    #[test_case(
        || (intrinsic(IntrinsicId::Object), ECMAScriptValue::Null), PropertyKey::from("key"), true
        => Ok(true);
        "PropertyKey:ECMASciptValue"
    )]
    #[test_case(|| (intrinsic(IntrinsicId::Object), 32_u32), 33_usize, true => Ok(true); "usize:u32")]
    fn set<X>(
        make_parts: impl FnOnce() -> (Object, X),
        key: impl Into<PropertyKey>,
        throw: bool,
    ) -> Result<bool, String>
    where
        X: Into<ECMAScriptValue>,
    {
        setup_test_agent();
        let (o, v) = make_parts();
        o.set(key, v, throw).map_err(unwind_any_error)
    }

    mod copy_data_properties {
        use super::*;
        use test_case::test_case;

        #[allow(clippy::unnecessary_wraps)]
        fn lying_ownprops(_: &AdaptableObject) -> Completion<Vec<PropertyKey>> {
            Ok(vec!["one".into(), "two".into(), "three".into()])
        }
        fn just_throw(_: &AdaptableObject) -> Completion<PropertyDescriptor> {
            Err(create_type_error("Test Case Thrower"))
        }
        fn second_kabloom_throws(ao: &AdaptableObject, key: &PropertyKey) -> Completion<Option<PropertyDescriptor>> {
            if *key == PropertyKey::from("kabloom") {
                let state = ao.something.get();
                if state == 0 {
                    ao.something.set(1);
                } else {
                    return Err(create_type_error("[[GetOwnProperty]] called on kabloom"));
                }
            }
            Ok(ordinary_get_own_property(ao, key))
        }
        fn throwing_get_own_property() -> Object {
            let throwing_key = PropertyKey::from("kabloom");
            let obj = AdaptableObject::object(&AdaptableMethods {
                get_own_property_override: Some(second_kabloom_throws),
                ..Default::default()
            });
            obj.create_data_property_or_throw(throwing_key, "boom").unwrap();
            obj
        }
        fn throwing_get() -> Object {
            let throwing_key = PropertyKey::from("kabloom");
            let obj = TestObject::object(&[FunctionId::Get(Some(throwing_key.clone()))]);
            obj.create_data_property_or_throw(throwing_key, "boom").unwrap();
            obj
        }

        #[test_case(|| ECMAScriptValue::Undefined, Vec::new => sok(""); "source undefined")]
        #[test_case(|| ECMAScriptValue::Null, Vec::new => sok(""); "source null")]
        #[test_case(|| "bob", Vec::new => sok("0:b,1:o,2:b"); "source string")]
        #[test_case(
            || ECMAScriptValue::from(TestObject::object(&[FunctionId::OwnPropertyKeys])), Vec::new
            => serr("TypeError: [[OwnPropertyKeys]] called on TestObject");
            "[[OwnPropertyKeys]] fails"
        )]
        #[test_case(|| "bob", || vec!["1".into(), "0".into()] => sok("2:b"); "check exclusion")]
        #[test_case(
            || AdaptableObject::object(
                &AdaptableMethods {
                    own_property_keys_override: Some(lying_ownprops),
                    ..Default::default()
                }
            ),
            Vec::new
            => sok("");
            "property names without descriptors"
        )]
        #[test_case(
            throwing_get_own_property, Vec::new
            => serr("TypeError: [[GetOwnProperty]] called on kabloom");
            "[[GetOwnProperty]] fails"
        )]
        #[test_case(throwing_get, Vec::new => serr("TypeError: [[Get]] called on TestObject"); "[[Get]] fails")]
        fn t<X>(
            make_source: impl FnOnce() -> X,
            make_excluded: impl FnOnce() -> Vec<PropertyKey>,
        ) -> Result<String, String>
        where
            X: Into<ECMAScriptValue>,
        {
            setup_test_agent();
            let target = ordinary_object_create(None, &[]);
            let source = make_source().into();
            let excluded = make_excluded();
            target
                .copy_data_properties(source, excluded.as_slice())
                .map_err(unwind_any_error)
                .map(|()| ECMAScriptValue::from(target).test_result_string())
        }
    }
}

mod integrity_level {
    use super::*;
    use test_case::test_case;

    #[test_case(IntegrityLevel::Sealed => with |s| assert_ne!(s, ""); "sealed")]
    #[test_case(IntegrityLevel::Frozen => with |s| assert_ne!(s, ""); "frozen")]
    fn debug_fmt(item: IntegrityLevel) -> String {
        format!("{item:?}")
    }

    #[test_case(IntegrityLevel::Sealed, IntegrityLevel::Sealed => true; "sealed/sealed")]
    #[test_case(IntegrityLevel::Frozen, IntegrityLevel::Sealed => false; "frozen/sealed")]
    #[test_case(IntegrityLevel::Sealed, IntegrityLevel::Frozen => false; "sealed/frozen")]
    #[test_case(IntegrityLevel::Frozen, IntegrityLevel::Frozen => true; "frozen/frozen")]
    fn eq(left: IntegrityLevel, right: IntegrityLevel) -> bool {
        left == right
    }
}

mod test_integrity_level {
    use super::*;
    use test_case::test_case;

    fn make_simple_obj(level: IntegrityLevel) -> Object {
        let obj = create_array_from_list(&[
            ECMAScriptValue::from(100),
            ECMAScriptValue::from(200),
            ECMAScriptValue::from(300),
        ]);
        super::set_integrity_level(&obj, level).unwrap();
        obj
    }
    fn throwing_own_property_keys() -> Object {
        let obj = TestObject::object(&[FunctionId::OwnPropertyKeys]);
        obj.o.prevent_extensions().unwrap();
        obj
    }
    fn second_kabloom_throws(ao: &AdaptableObject, key: &PropertyKey) -> Completion<Option<PropertyDescriptor>> {
        if *key == PropertyKey::from("kabloom") {
            let state = ao.something.get();
            if state == 0 {
                ao.something.set(1);
            } else {
                return Err(create_type_error("[[GetOwnProperty]] called on kabloom"));
            }
        }
        Ok(ordinary_get_own_property(ao, key))
    }
    fn throwing_get_own_property() -> Object {
        let throwing_key = PropertyKey::from("kabloom");
        let obj = AdaptableObject::object(&AdaptableMethods {
            get_own_property_override: Some(second_kabloom_throws),
            ..Default::default()
        });
        obj.create_data_property_or_throw(throwing_key, "boom").unwrap();
        obj.o.prevent_extensions().unwrap();
        obj
    }
    #[allow(clippy::unnecessary_wraps)]
    fn lying_ownprops(_: &AdaptableObject) -> Completion<Vec<PropertyKey>> {
        Ok(vec!["one".into(), "two".into(), "three".into()])
    }
    fn lyingkeys() -> Object {
        let obj = AdaptableObject::object(&AdaptableMethods {
            own_property_keys_override: Some(lying_ownprops),
            ..Default::default()
        });
        obj.o.prevent_extensions().unwrap();
        obj
    }

    #[test_case(|| ordinary_object_create(None, &[]), IntegrityLevel::Sealed => Ok(false); "very basic object")]
    #[test_case(DeadObject::object, IntegrityLevel::Sealed => serr("TypeError: is_extensible called on DeadObject"); "is_extensible throws")]
    #[test_case(|| make_simple_obj(IntegrityLevel::Sealed), IntegrityLevel::Sealed => Ok(true); "sealed object / checking sealed")]
    #[test_case(|| make_simple_obj(IntegrityLevel::Sealed), IntegrityLevel::Frozen => Ok(false); "sealed object / checking frozen")]
    #[test_case(|| make_simple_obj(IntegrityLevel::Frozen), IntegrityLevel::Sealed => Ok(true); "frozen object / checking sealed")]
    #[test_case(|| make_simple_obj(IntegrityLevel::Frozen), IntegrityLevel::Frozen => Ok(true); "frozen object / checking frozen")]
    #[test_case(throwing_own_property_keys, IntegrityLevel::Sealed => serr("TypeError: [[OwnPropertyKeys]] called on TestObject"); "own_property_keys throws")]
    #[test_case(throwing_get_own_property, IntegrityLevel::Sealed => serr("TypeError: [[GetOwnProperty]] called on kabloom"); "get_own_property throws")]
    #[test_case(lyingkeys, IntegrityLevel::Frozen => Ok(true); "own_property_keys lied about keys")]
    fn call(make_obj: impl FnOnce() -> Object, level: IntegrityLevel) -> Result<bool, String> {
        setup_test_agent();
        let obj = make_obj();

        super::test_integrity_level(&obj, level).map_err(unwind_any_error)
    }
}

mod concise_optional_object {
    use super::*;

    #[test]
    fn from_some() {
        setup_test_agent();
        let obj = ordinary_object_create(None, &[]);

        let obj_id = obj.o.id();

        let opt_obj = Some(obj);

        let res = ConciseOptionalObject::from(&opt_obj);

        let res_id = res.0.as_ref().unwrap().o.id();
        assert_eq!(obj_id, res_id);
    }
    #[test]
    fn from_none() {
        setup_test_agent();
        let res = ConciseOptionalObject::from(&None);
        assert!(res.0.is_none());
    }

    #[test]
    fn fmt_none() {
        setup_test_agent();
        let coo = ConciseOptionalObject::from(&None);
        let res = format!("{coo:?}");
        assert_eq!(res, "None");
    }

    #[test]
    fn fmt_some() {
        setup_test_agent();
        let obj = Some(ordinary_object_create(None, &[]));
        let coo = ConciseOptionalObject::from(&obj);
        let res = format!("{coo:#?}");
        assert_ne!(res, "");
        assert!(!res.contains('\n'));
    }
}

mod create_list_from_array_like {
    use super::*;
    use test_case::test_case;

    fn not_arraylike() -> ECMAScriptValue {
        let obj = ordinary_object_create(None, &[]);
        obj.set("length", wks(WksId::AsyncIterator), true).unwrap();
        obj.into()
    }
    fn three_numbers() -> ECMAScriptValue {
        let obj = create_array_from_list(&[8.into(), 3.into(), 100.into()]);
        obj.into()
    }
    fn evil_get() -> ECMAScriptValue {
        let obj = ordinary_object_create(None, &[]);
        obj.set("length", 10, true).unwrap();
        let thrower = intrinsic(IntrinsicId::ThrowTypeError);
        let desc = PotentialPropertyDescriptor::new().get(thrower);
        define_property_or_throw(&obj, "0", desc).unwrap();
        obj.into()
    }

    #[test_case(|| ECMAScriptValue::Undefined, None => serr("TypeError: CreateListFromArrayLike called on non-object"); "not an object")]
    #[test_case(not_arraylike, None => serr("TypeError: Symbol values cannot be converted to Number values"); "object, not arraylike")]
    #[test_case(three_numbers, Some(&[ValueKind::Number]) => Ok("8, 3, 100".into()); "three item list")]
    #[test_case(three_numbers, Some(&[ValueKind::String]) => serr("TypeError: Invalid kind for array"); "kind mismatch")]
    #[test_case(evil_get, None => serr("TypeError: Generic TypeError"); "get fails")]
    fn call(make_al: impl FnOnce() -> ECMAScriptValue, types: Option<&[ValueKind]>) -> Result<String, String> {
        setup_test_agent();
        let array_like = make_al();

        create_list_from_array_like(array_like, types).map_err(unwind_any_error).map(|seq| {
            seq.iter().map(|val| String::from(to_string(val.clone()).unwrap())).collect::<Vec<_>>().join(", ")
        })
    }
}

mod is_compatible_property_descriptor {
    use super::*;
    use test_case::test_case;

    #[test_case(|| PotentialPropertyDescriptor::new().value(ECMAScriptValue::Null).writable(false).configurable(false),
                || Some(PropertyDescriptor { property: PropertyKind::Data(DataProperty{value: ECMAScriptValue::Undefined, writable: false}), enumerable: true, configurable: false, ..Default::default() }),
                true => false; "incompatible")]
    #[test_case(|| PotentialPropertyDescriptor::new().value(ECMAScriptValue::Null).writable(true).configurable(true),
                || Some(PropertyDescriptor { property: PropertyKind::Data(DataProperty{value: ECMAScriptValue::Undefined, writable: true}), enumerable: true, configurable: true, ..Default::default() }),
                true => true; "compatible")]
    fn call(
        ppd_maker: impl FnOnce() -> PotentialPropertyDescriptor,
        current_maker: impl FnOnce() -> Option<PropertyDescriptor>,
        extensible: bool,
    ) -> bool {
        let ppd = ppd_maker();
        let current = current_maker();
        super::is_compatible_property_descriptor(extensible, ppd, current.as_ref())
    }
}

mod define_property_or_throw {
    use super::*;
    use test_case::test_case;

    #[test_case(
        || (
            ordinary_object_create(None, &[]),
            PropertyKey::from("test_key"),
            PotentialPropertyDescriptor::new().value(99)
        )
        => sok("test_key:99");
        "success")]
    #[test_case(
        || (
            TestObject::object(&[FunctionId::DefineOwnProperty(Some("test_key".into()))]),
            PropertyKey::from("test_key"),
            PotentialPropertyDescriptor::new().value(99)
        )
        => serr("TypeError: [[DefineOwnProperty]] called on TestObject");
        "define_own_property fails")]
    #[test_case(
        || ({
                let obj = ordinary_object_create(None, &[]);
                let ppd = PotentialPropertyDescriptor::new().value(100);
                super::super::define_property_or_throw(&obj, "test_key", ppd).unwrap();
                obj
            },
            PropertyKey::from("test_key"),
            PotentialPropertyDescriptor::new().value(99)
        )
        => serr("TypeError: Property cannot be assigned to");
        "un-alterable property")]
    fn idpot(
        make_items: impl FnOnce() -> (Object, PropertyKey, PotentialPropertyDescriptor),
    ) -> Result<String, String> {
        setup_test_agent();
        let (obj, key, desc) = make_items();
        internal_define_property_or_throw(&obj, key, desc)
            .map_err(unwind_any_error)
            .map(|()| ECMAScriptValue::from(obj).test_result_string())
    }

    #[test_case(wks(WksId::ToStringTag) => sok("Symbol(Symbol.toStringTag):0"); "symbol")]
    #[test_case("simple_string" => sok("simple_string:0"); "string reference")]
    #[test_case(JSString::from("jsstring") => sok("jsstring:0"); "jsstring")]
    #[test_case(PropertyKey::from("key") => sok("key:0"); "just a key")]
    fn define_property_or_throw(key: impl Into<PropertyKey>) -> Result<String, String> {
        setup_test_agent();
        let obj = ordinary_object_create(None, &[]);
        let ppd = PotentialPropertyDescriptor::new().value(0);
        super::super::define_property_or_throw(&obj, key, ppd)
            .map_err(unwind_any_error)
            .map(|()| ECMAScriptValue::from(obj).test_result_string())
    }
}

#[test_case(|| ECMAScriptValue::from(99) => None; "not a constructor")]
#[test_case(
    || {
        let cstr = intrinsic(IntrinsicId::Object);
        cstr.set("sentinel", "test response", true).unwrap();
        ECMAScriptValue::from(cstr)
    }
    => Some("test response".to_string());
    "constructor object")]
#[test_case(|| intrinsic(IntrinsicId::ObjectPrototype).into() => None; "object but not cstr")]
fn to_constructor(make_val: impl FnOnce() -> ECMAScriptValue) -> Option<String> {
    setup_test_agent();
    let val = make_val();
    super::to_constructor(&val).map(|cstr| {
        let key = PropertyKey::from("sentinel");
        cstr.get(&key, &val).unwrap().test_result_string()
    })
}

mod ordinary_object {
    use super::*;

    fn make() -> Object {
        let proto = intrinsic(IntrinsicId::ObjectPrototype);
        let o = OrdinaryObject::object(Some(proto), true);
        let proto = o.o.get_prototype_of().unwrap().unwrap();
        proto.set("proto_sentinel", true, true).unwrap();
        o
    }

    default_get_prototype_of_test!(ObjectPrototype);
    default_set_prototype_of_test!();
    default_is_extensible_test!();
    default_prevent_extensions_test!();
    default_delete_test!();
    default_id_test!();
    default_has_property_test!();
    default_uses_ordinary_get_prototype_of_test!();
    default_get_own_property_test!();
    default_define_own_property_test!();
    default_get_test!(|| "proto_sentinel".into(), ECMAScriptValue::from(true));
    default_set_test!();
    default_own_property_keys_test!();
    none_function!(to_for_in_iterator);
    false_function!(is_arguments_object);
    false_function!(is_callable_obj);
    false_function!(is_error_object);
    none_function!(to_number_obj);
    false_function!(is_array_object);
    false_function!(is_symbol_object);
    none_function!(to_boolean_obj);
    none_function!(to_symbol_obj);
    none_function!(to_arguments_object);
    none_function!(to_array_object);
    false_function!(is_generator_object);
    none_function!(to_proxy_object);
    false_function!(is_string_object);
    false_function!(is_boolean_object);
    none_function!(to_constructable);
    none_function!(to_builtin_function_obj);
    false_function!(is_date_object);
    none_function!(to_string_obj);
    none_function!(to_error_obj);
    none_function!(to_generator_object);
    false_function!(is_number_object);
    false_function!(is_regexp_object);
    false_function!(is_proxy_object);
    none_function!(to_callable_obj);
    none_function!(to_function_obj);

    #[test]
    fn is_plain_object() {
        setup_test_agent();
        let obj = make();
        assert!(obj.o.is_plain_object());
    }

    #[test]
    fn debug() {
        setup_test_agent();
        let obj = make();
        assert_ne!(format!("{obj:?}"), "");
    }
}

mod immutable_prototype_exotic_object {
    use super::*;
    use test_case::test_case;

    fn make() -> Object {
        let proto = intrinsic(IntrinsicId::ObjectPrototype);
        let o = ImmutablePrototypeExoticObject::object(Some(proto));
        let proto = o.o.get_prototype_of().unwrap().unwrap();
        proto.set("proto_sentinel", true, true).unwrap();
        o
    }

    default_get_prototype_of_test!(ObjectPrototype);
    default_is_extensible_test!();
    default_prevent_extensions_test!();
    default_delete_test!();
    default_id_test!();
    default_has_property_test!();
    default_get_own_property_test!();
    default_define_own_property_test!();
    default_get_test!(|| "proto_sentinel".into(), ECMAScriptValue::from(true));
    default_set_test!();
    default_own_property_keys_test!();
    none_function!(to_for_in_iterator);
    false_function!(is_arguments_object);
    false_function!(is_callable_obj);
    false_function!(is_error_object);
    none_function!(to_number_obj);
    false_function!(is_array_object);
    false_function!(is_symbol_object);
    none_function!(to_boolean_obj);
    none_function!(to_symbol_obj);
    none_function!(to_arguments_object);
    none_function!(to_array_object);
    false_function!(is_generator_object);
    none_function!(to_proxy_object);
    false_function!(is_string_object);
    false_function!(is_boolean_object);
    none_function!(to_constructable);
    none_function!(to_builtin_function_obj);
    false_function!(is_date_object);
    none_function!(to_string_obj);
    none_function!(to_error_obj);
    none_function!(to_generator_object);
    false_function!(is_number_object);
    false_function!(is_regexp_object);
    false_function!(is_proxy_object);
    none_function!(to_callable_obj);
    none_function!(to_function_obj);
    false_function!(is_plain_object);
    false_function!(uses_ordinary_get_prototype_of);

    #[test]
    fn debug() {
        setup_test_agent();
        let obj = make();
        assert_ne!(format!("{obj:?}"), "");
    }

    #[test_case(make, || Some(intrinsic(IntrinsicId::ObjectPrototype)) => Ok(true); "setting to same")]
    #[test_case(make, || None => Ok(false); "trying to change")]
    fn set_prototype_of(
        make_obj: impl FnOnce() -> Object,
        make_proto: impl FnOnce() -> Option<Object>,
    ) -> Result<bool, String> {
        setup_test_agent();
        let obj = make_obj();
        let proto = make_proto();
        obj.o.set_prototype_of(proto).map_err(unwind_any_error)
    }
}

#[test_case(
    || ImmutablePrototypeExoticObject::object(Some(intrinsic(IntrinsicId::ObjectPrototype))),
    || Some(intrinsic(IntrinsicId::ObjectPrototype))
    => Ok(true);
    "setting to same")]
#[test_case(
    || ImmutablePrototypeExoticObject::object(Some(intrinsic(IntrinsicId::ObjectPrototype))),
    || Some(intrinsic(IntrinsicId::StringPrototype))
    => Ok(false);
    "changing")]
#[test_case(
    || TestObject::object(&[FunctionId::GetPrototypeOf]),
    || None
    => serr("TypeError: [[GetPrototypeOf]] called on TestObject");
    "error thrown")]
#[test_case(
    || ImmutablePrototypeExoticObject::new(Some(intrinsic(IntrinsicId::ObjectPrototype))),
    || Some(intrinsic(IntrinsicId::ObjectPrototype))
    => Ok(true);
    "immutable obj itself")]
fn set_immutable_prototype<O>(
    make_obj: impl FnOnce() -> O,
    make_proto: impl FnOnce() -> Option<Object>,
) -> Result<bool, String>
where
    for<'a> &'a O: Into<&'a dyn ObjectInterface>,
{
    setup_test_agent();
    let obj = make_obj();
    let proto = make_proto();
    super::set_immutable_prototype(&obj, &proto).map_err(unwind_any_error)
}

#[test_case(
    || (ECMAScriptValue::Undefined, ECMAScriptValue::Undefined, vec![])
    => serr("TypeError: Value not callable");
    "not a function")]
#[test_case(
    || {
        let sp = intrinsic(IntrinsicId::StringPrototype);
        let index_of = sp.get(&"indexOf".into()).unwrap();
        let mystring = create_string_object("Sentence With Many Words".into());
        let needle = ECMAScriptValue::from("With");
        (index_of, mystring.into(), vec![needle])
    }
    => sok("9");
    "calling a builtin-function")]
fn call(
    make_items: impl FnOnce() -> (ECMAScriptValue, ECMAScriptValue, Vec<ECMAScriptValue>),
) -> Result<String, String> {
    setup_test_agent();
    let (func, this_value, arguments) = make_items();
    super::call(&func, &this_value, arguments.as_ref()).map_err(unwind_any_error).map(|val| val.test_result_string())
}

#[test_case(
    || {
        let sp = intrinsic(IntrinsicId::StringPrototype);
        let index_of = sp.get(&"indexOf".into()).unwrap();
        let mystring = create_string_object("Sentence With Many Words".into());
        let needle = ECMAScriptValue::from("With");
        (index_of, mystring.into(), vec![needle])
    }
    => (true, "Value(Number(9.0))".to_string());
    "calling a builtin-function")]
#[test_case(
    || (ECMAScriptValue::Undefined, ECMAScriptValue::Undefined, vec![])
    => (false, "TypeError: Value not callable".to_string());
    "not a function")]
fn initiate_call(
    make_items: impl FnOnce() -> (ECMAScriptValue, ECMAScriptValue, Vec<ECMAScriptValue>),
) -> (bool, String) {
    setup_test_agent();
    let (func, this_value, arguments) = make_items();
    let res = super::initiate_call(&func, &this_value, arguments.as_slice());
    let tos = ec_pop().unwrap().map_or_else(unwind_any_error, |nc| format!("{nc:?}"));
    (res, tos)
}

#[test_case(
    || {
        let sp = intrinsic(IntrinsicId::StringPrototype);
        let index_of = sp.get(&"indexOf".into()).unwrap();
        (index_of, 9.into())
    }
    => Ok("9".into());
    "calling a builtin-function")]
fn complete_call(make_items: impl FnOnce() -> (ECMAScriptValue, ECMAScriptValue)) -> Result<String, String> {
    setup_test_agent();
    let (func, value) = make_items();
    ec_push(Ok(NormalCompletion::from(value)));
    super::complete_call(&func).map_err(unwind_any_error).map(|v| v.test_result_string())
}

#[test_case(
    || {
        let string = intrinsic(IntrinsicId::String);
        let args = vec![ECMAScriptValue::from("yolo")];
        let new_target = Some(string.clone());
        (string, args, new_target)
    }
    => sok("length:4");
    "strightforward"
)]
fn construct(make_items: impl FnOnce() -> (Object, Vec<ECMAScriptValue>, Option<Object>)) -> Result<String, String> {
    setup_test_agent();
    let (func, args, new_target) = make_items();
    super::construct(&func, args.as_slice(), new_target.as_ref())
        .map_err(unwind_any_error)
        .map(|v| v.test_result_string())
}

mod data_descriptor {
    use super::*;
    use test_case::test_case;

    #[test_case(
        || PropertyDescriptor {
            property: PropertyKind::Data(DataProperty { value:"value".into(), writable:true }),
            enumerable: true,
            configurable: true,
            spot: 0
        }
        => Ok(("value".to_string(), true, true, true));
        "data property"
    )]
    #[test_case(
        || PropertyDescriptor {
            property: PropertyKind::Accessor(AccessorProperty {
                get: ECMAScriptValue::Undefined,
                set: ECMAScriptValue::Undefined
            }),
            enumerable: true,
            configurable: true,
            spot: 0
        }
        => serr("Accessor Property cannot be formed into a DataDescriptor");
        "accessor property"
    )]
    fn try_from(make_pd: impl FnOnce() -> PropertyDescriptor) -> Result<(String, bool, bool, bool), String> {
        setup_test_agent();
        let pd = make_pd();
        DataDescriptor::try_from(pd)
            .map_err(ToString::to_string)
            .map(|dd| (dd.value.test_result_string(), dd.writable, dd.enumerable, dd.configurable))
    }
}

#[test_case(|| ("word".into(), "length".into()) => sok("4"); "simple")]
#[test_case(
    || (ECMAScriptValue::Undefined, "length".into())
    => serr("TypeError: Undefined and null cannot be converted to objects");
    "not convertable"
)]
fn ecmascriptvalue_get(make_items: impl FnOnce() -> (ECMAScriptValue, PropertyKey)) -> Result<String, String> {
    setup_test_agent();
    let (v, p) = make_items();
    v.get(&p).map_err(unwind_any_error).map(|v| v.test_result_string())
}

#[test_case(|| ordinary_object_create(None, &[]), &ClassFieldDefinitionRecord{} => panics "not yet implemented"; "panics")]
fn define_field(make_obj: impl FnOnce() -> Object, fdr: &ClassFieldDefinitionRecord) -> Result<(), String> {
    setup_test_agent();
    let obj = make_obj();
    super::define_field(&obj, fdr).map_err(unwind_any_error)
}

mod property_info {
    use super::*;
    use test_case::test_case;

    #[test]
    fn debug_fmt() {
        let item = PropertyInfo {
            name: "name".into(),
            enumerable: true,
            configurable: true,
            kind: PropertyInfoKind::Data { value: ECMAScriptValue::Null, writable: true },
        };
        assert_ne!(format!("{item:?}"), "");
    }

    #[test_case(
        &PropertyInfo {
            name: "name".into(),
            enumerable: true,
            configurable: true,
            kind: PropertyInfoKind::Data { value: ECMAScriptValue::Null, writable: true },
        },
        &PropertyInfo {
            name: "name".into(),
            enumerable: true,
            configurable: true,
            kind: PropertyInfoKind::Data { value: ECMAScriptValue::Null, writable: true },
        }
        => true;
        "equal"
    )]
    #[test_case(
        &PropertyInfo {
            name: "name".into(),
            enumerable: true,
            configurable: true,
            kind: PropertyInfoKind::Data { value: ECMAScriptValue::Null, writable: true },
        },
        &PropertyInfo {
            name: "name".into(),
            enumerable: true,
            configurable: false,
            kind: PropertyInfoKind::Data { value: ECMAScriptValue::Null, writable: true },
        }
        => false;
        "not equal"
    )]
    fn eq(left: &PropertyInfo, right: &PropertyInfo) -> bool {
        left == right
    }
}

mod property_info_kind {
    use super::*;
    use test_case::test_case;

    #[test]
    fn debug_fmt() {
        let item = PropertyInfoKind::Data { value: ECMAScriptValue::Null, writable: true };
        assert_ne!(format!("{item:?}"), "");
    }

    #[test_case(
        &PropertyInfoKind::Data { value: ECMAScriptValue::Null, writable: true },
        &PropertyInfoKind::Data { value: ECMAScriptValue::Null, writable: true }
        => true;
        "equal"
    )]
    #[test_case(
        &PropertyInfoKind::Data { value: ECMAScriptValue::Null, writable: true },
        &PropertyInfoKind::Data { value: ECMAScriptValue::Null, writable: false }
        => false;
        "not equal"
    )]
    fn eq(left: &PropertyInfoKind, right: &PropertyInfoKind) -> bool {
        left == right
    }
}

mod dead_object {
    use super::*;
    //use test_case::test_case;

    fn make() -> Object {
        DeadObject::object()
    }

    none_function!(to_symbol_obj);
    false_function!(is_plain_object);
    none_function!(to_string_obj);
    none_function!(to_generator_object);
    false_function!(is_generator_object);
    none_function!(to_arguments_object);
    false_function!(is_symbol_object);
    false_function!(uses_ordinary_get_prototype_of);
    false_function!(is_date_object);
    none_function!(to_callable_obj);
    none_function!(to_array_object);
    none_function!(to_boolean_obj);
    none_function!(to_constructable);
    none_function!(to_proxy_object);
    none_function!(to_error_obj);
    none_function!(to_for_in_iterator);
    none_function!(to_number_obj);
    none_function!(to_function_obj);
    none_function!(to_builtin_function_obj);
    false_function!(is_error_object);
    false_function!(is_string_object);
    false_function!(is_callable_obj);
    false_function!(is_number_object);
    false_function!(is_arguments_object);
    false_function!(is_regexp_object);
    false_function!(is_proxy_object);
    false_function!(is_array_object);
    false_function!(is_boolean_object);

    #[test]
    fn is_extensible() {
        setup_test_agent();
        assert_eq!(
            unwind_any_error(make().o.is_extensible().unwrap_err()),
            "TypeError: is_extensible called on DeadObject"
        );
    }

    #[test]
    fn define_own_property() {
        setup_test_agent();
        assert_eq!(
            unwind_any_error(
                make().o.define_own_property("key".into(), PotentialPropertyDescriptor::new()).unwrap_err()
            ),
            "TypeError: define_own_property called on DeadObject"
        );
    }

    #[test]
    fn get_prototype_of() {
        setup_test_agent();
        assert_eq!(
            unwind_any_error(make().o.get_prototype_of().unwrap_err()),
            "TypeError: get_prototype_of called on DeadObject"
        );
    }

    #[test]
    fn set_prototype_of() {
        setup_test_agent();
        assert_eq!(
            unwind_any_error(make().o.set_prototype_of(None).unwrap_err()),
            "TypeError: set_prototype_of called on DeadObject"
        );
    }

    #[test]
    fn prevent_extensions() {
        setup_test_agent();
        assert_eq!(
            unwind_any_error(make().o.prevent_extensions().unwrap_err()),
            "TypeError: prevent_extensions called on DeadObject"
        );
    }

    #[test]
    fn get_own_property() {
        setup_test_agent();
        assert_eq!(
            unwind_any_error(make().o.get_own_property(&"key".into()).unwrap_err()),
            "TypeError: get_own_property called on DeadObject"
        );
    }

    #[test]
    fn has_property() {
        setup_test_agent();
        assert_eq!(
            unwind_any_error(make().o.has_property(&"a".into()).unwrap_err()),
            "TypeError: has_property called on DeadObject"
        );
    }

    #[test]
    fn delete() {
        setup_test_agent();
        assert_eq!(
            unwind_any_error(make().o.delete(&"a".into()).unwrap_err()),
            "TypeError: delete called on DeadObject"
        );
    }

    #[test]
    fn own_property_keys() {
        setup_test_agent();
        assert_eq!(
            unwind_any_error(make().o.own_property_keys().unwrap_err()),
            "TypeError: own_property_keys called on DeadObject"
        );
    }

    mod get {
        use super::*;
        #[test]
        fn get() {
            setup_test_agent();
            assert_eq!(
                unwind_any_error(make().o.get(&"d".into(), &ECMAScriptValue::Null).unwrap_err()),
                "TypeError: get called on DeadObject"
            );
        }
    }

    mod set {
        use super::*;
        #[test]
        fn set() {
            setup_test_agent();
            assert_eq!(
                unwind_any_error(
                    make().o.set("d".into(), ECMAScriptValue::Undefined, &ECMAScriptValue::Null).unwrap_err()
                ),
                "TypeError: set called on DeadObject"
            );
        }
    }

    #[test]
    fn debug() {
        setup_test_agent();
        assert_ne!(format!("{:?}", make()), "");
    }

    #[should_panic]
    #[test]
    fn common_object_data() {
        setup_test_agent();
        make().o.common_object_data();
    }

    default_id_test!();
}

mod get_prototype_from_constructor {
    use super::*;
    use test_case::test_case;

    fn cbf(
        behavior: fn(&ECMAScriptValue, Option<&Object>, &[ECMAScriptValue]) -> Completion<ECMAScriptValue>,
    ) -> Object {
        create_builtin_function(
            behavior,
            false,
            0.0,
            "f".into(),
            BUILTIN_FUNCTION_SLOTS,
            current_realm_record(),
            Some(intrinsic(IntrinsicId::FunctionPrototype)),
            None,
        )
    }

    fn fn_returning_target_then_revoking() -> Object {
        fn behavior(
            _this_value: &ECMAScriptValue,
            _new_target: Option<&Object>,
            arguments: &[ECMAScriptValue],
        ) -> Completion<ECMAScriptValue> {
            let mut args = FuncArgs::from(arguments);
            let target = Object::try_from(args.next_arg()).unwrap();
            let key = PropertyKey::try_from(args.next_arg()).unwrap();
            let receiver = args.next_arg();
            let value = target.o.get(&key, &receiver);

            let global = get_global_object().unwrap();
            let test_proxy = global.get(&"test_proxy".into()).unwrap();
            let proxy_object = to_object(test_proxy).unwrap();
            let proxy_interface = proxy_object.o.to_proxy_object().unwrap();
            proxy_interface.revoke();
            value
        }
        cbf(behavior)
    }

    fn make_handler(fcn: Object) -> Object {
        let handler = ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)), &[]);
        let ppd = PotentialPropertyDescriptor::new().value(fcn);
        define_property_or_throw(&handler, "get", ppd).unwrap();
        handler
    }

    #[test_case(
        || {
            let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
            object_prototype.create_data_property_or_throw("sentinel", "prototype").unwrap();
            ordinary_object_create(None, &[])
        },
        IntrinsicId::ObjectPrototype
        => sok("prototype");
        "constructor has no prototype property"
    )]
    #[test_case(
        || {
            let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
            object_prototype.create_data_property_or_throw("sentinel", "object prototype").unwrap();
            let error_prototype = intrinsic(IntrinsicId::ErrorPrototype);
            error_prototype.create_data_property_or_throw("sentinel", "error prototype").unwrap();
            intrinsic(IntrinsicId::Object)
        },
        IntrinsicId::ErrorPrototype
        => sok("object prototype");
        "Object constructor; but error prototype chosen as default"
    )]
    #[test_case(
        || TestObject::object(&[FunctionId::Get(Some("prototype".into()))]),
        IntrinsicId::ObjectPrototype
        => serr("TypeError: [[Get]] called on TestObject");
        "constructor object is broken"
    )]
    #[test_case(
        || {
            let po = ProxyObject::object(Some((
                intrinsic(IntrinsicId::ObjectPrototype),
                make_handler(fn_returning_target_then_revoking())
            )));
            let val = po.clone();
            let global = get_global_object().unwrap();
            global.set("test_proxy", val, true).unwrap();
            po
        },
        IntrinsicId::ErrorPrototype
        => serr("TypeError: Proxy has been revoked");
        "get_function_realm fails"
    )]
    fn t(make_obj: impl FnOnce() -> Object, iproto: IntrinsicId) -> Result<String, String> {
        setup_test_agent();
        let cstr = make_obj();
        cstr.get_prototype_from_constructor(iproto)
            .map_err(unwind_any_error)
            .map(|obj| obj.get(&"sentinel".into()).unwrap().test_result_string())
    }
}

mod get_method {
    use super::*;
    use test_case::test_case;

    #[test_case(
        || ECMAScriptValue::Undefined, "something"
        => serr("TypeError: Undefined and null cannot be converted to objects");
        "Not an object"
    )]
    #[test_case(
        || intrinsic(IntrinsicId::Object), "nothing"
        => sok("undefined");
        "property is undefined"
    )]
    #[test_case(
        || {
            let obj = ordinary_object_create(None, &[]);
            obj.create_data_property_or_throw("some_key", ECMAScriptValue::Null).unwrap();
            obj
        },
        "some_key"
        => sok("undefined");
        "property is null"
    )]
    #[test_case(
        || {
            let obj = ordinary_object_create(None, &[]);
            obj.create_data_property_or_throw("some_key", "some_string").unwrap();
            obj
        },
        "some_key"
        => serr("TypeError: item is not callable");
        "property is not callable"
    )]
    #[test_case(|| intrinsic(IntrinsicId::Object), "create" => sok("create"); "a real method")]
    fn t<X>(make_val: impl FnOnce() -> X, key: impl Into<PropertyKey>) -> Result<String, String>
    where
        X: Into<ECMAScriptValue>,
    {
        setup_test_agent();
        let val = make_val().into();
        let key = key.into();
        val.get_method(&key).map_err(unwind_any_error).map(|fval| {
            if fval.is_undefined() {
                "undefined".to_string()
            } else {
                let fobj = to_object(fval).unwrap();
                fobj.get(&"name".into()).unwrap().test_result_string()
            }
        })
    }
}

mod get_function_realm {
    use super::*;
    use test_case::test_case;

    #[test_case(|| ProxyObject::object(None) => serr("TypeError: Proxy has been revoked"); "revoked proxy")]
    #[test_case(
        || ProxyObject::object(Some((intrinsic(IntrinsicId::Object), ordinary_object_create(None, &[]))))
        => Ok(0);
        "through the proxy"
    )]
    #[test_case(|| ordinary_object_create(None, &[]) => Ok(0); "not actually a function")]
    #[test_case(
        || {
            initialize_host_defined_realm(1, false);
            let obj = FunctionObject::object(
                None,
                current_realm_record().unwrap().borrow().global_env.clone().unwrap(),
                None,
                ParamSource::FormalParameters(Maker::new("").formal_parameters()),
                BodySource::Function(Maker::new("{}").function_body()),
                ConstructorKind::Base,
                current_realm_record().unwrap(),
                None,
                ThisMode::Lexical,
                true,
                None,
                "",
                vec![],
                vec![],
                ClassName::Empty,
                false,
                Rc::new(Chunk::new("test"))
            );
            pop_execution_context();
            obj
        }
        => Ok(1);
        "normal function, alternate realm"
    )]
    #[test_case(|| intrinsic(IntrinsicId::Object) => Ok(0); "normal builtin, default realm")]
    fn t(make_fobj: impl FnOnce() -> Object) -> Result<RealmId, String> {
        setup_test_agent();
        let fobj = make_fobj();
        fobj.get_function_realm().map_err(unwind_any_error).map(|realm| realm.borrow().id())
    }
}

mod ordinary_create_from_constructor {
    use super::*;
    use test_case::test_case;

    #[test_case(
        || {
            let string_proto = intrinsic(IntrinsicId::StringPrototype);
            let object_proto = intrinsic(IntrinsicId::ObjectPrototype);
            string_proto.create_data_property_or_throw("[[TestName]]", "String").unwrap();
            object_proto.create_data_property_or_throw("[[TestName]]", "Object").unwrap();
            ordinary_object_create(Some(object_proto), &[])
        },
        IntrinsicId::StringPrototype
        => sok("String");
        "no proto property; get default"
    )]
    #[test_case(
        DeadObject::object, IntrinsicId::ObjectPrototype
        => serr("TypeError: get called on DeadObject");
        "constructor is broken"
    )]
    fn t(make_cstr: impl FnOnce() -> Object, default_ip: IntrinsicId) -> Result<String, String> {
        setup_test_agent();
        let cstr = make_cstr();
        cstr.ordinary_create_from_constructor(default_ip, &[]).map_err(unwind_any_error).map(|obj| {
            let proto = obj.o.get_prototype_of().unwrap().unwrap();
            proto.get(&"[[TestName]]".into()).unwrap().test_result_string()
        })
    }
}

#[test_case(|| 3, "toString", &[2] => sok("11"); "straightforward")]
#[test_case(DeadObject::object, "unused", &[2] => serr("TypeError: get called on DeadObject"); "broken func")]
fn invoke<X, Y>(make_val: impl FnOnce() -> X, key: impl Into<PropertyKey>, args: &[Y]) -> Result<String, String>
where
    X: Into<ECMAScriptValue>,
    Y: Into<ECMAScriptValue> + Clone,
{
    setup_test_agent();
    let val = make_val().into();
    let key = key.into();
    let args = args.iter().map(|v| v.clone().into()).collect::<Vec<ECMAScriptValue>>();
    val.invoke(&key, args.as_slice()).map_err(unwind_any_error).map(|val| val.test_result_string())
}

#[test_case(
    DeadObject::object, "key"
    => serr("TypeError: get_own_property called on DeadObject");
    "get_own_property fails"
)]
#[test_case(|| intrinsic(IntrinsicId::Object), "is" => Ok(true); "property exists")]
#[test_case(|| intrinsic(IntrinsicId::Object), "not-really-here" => Ok(false); "property does not exist")]
fn has_own_property(make_val: impl FnOnce() -> Object, key: impl Into<PropertyKey>) -> Result<bool, String> {
    setup_test_agent();
    let obj = make_val();
    let key = key.into();
    obj.has_own_property(&key).map_err(unwind_any_error)
}

#[test_case(|| ordinary_object_create(None, &[]), "bob" => sok(""); "prop not there")]
#[test_case(
    || {
        let obj = ordinary_object_create(None, &[]);
        obj.create_data_property_or_throw("named", 27).unwrap();
        obj.create_data_property_or_throw("other", 12).unwrap();
        obj
    },
    "named"
    => sok("other:12");
    "good deletion"
)]
#[test_case(
    || TestObject::object(&[FunctionId::Delete(Some("named".into()))]), "named"
    => serr("TypeError: [[Delete]] called on TestObject");
    "delete throws"
)]
#[test_case(
    || {
        let obj = ordinary_object_create(None, &[]);
        obj.create_data_property_or_throw("named", 11).unwrap();
        set_integrity_level(&obj, IntegrityLevel::Frozen).unwrap();
        obj
    },
    "named"
    => serr("TypeError: Property could not be deleted");
    "delete fail switched to error"
)]
fn delete_property_or_throw(make_obj: impl FnOnce() -> Object, key: impl Into<PropertyKey>) -> Result<String, String> {
    setup_test_agent();
    let obj = make_obj();
    let key = key.into();
    obj.delete_property_or_throw(&key)
        .map_err(unwind_any_error)
        .map(|()| ECMAScriptValue::from(obj).test_result_string())
}
