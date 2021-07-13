use super::*;
use crate::strings::JSString;
use crate::tests::{printer_validate, test_agent, unwind_type_error, FunctionId, TestObject};
use std::io::Write;

#[test]
fn data_property_debug() {
    let dp = DataProperty { value: ECMAScriptValue::Null, writable: true };
    assert_ne!(format!("{:?}", dp), "");
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
    assert_ne!(format!("{:?}", p1), "");
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
    assert_ne!(format!("{:?}", pk), "");
}
#[test]
fn property_kind_eq() {
    let items: [PropertyKind; 4] = [
        PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(true), writable: true }),
        PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(true), writable: false }),
        PropertyKind::Accessor(AccessorProperty { get: ECMAScriptValue::Undefined, set: ECMAScriptValue::from("movies") }),
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

#[test]
fn property_descriptor_debug() {
    let p = PropertyDescriptor { property: PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(true), writable: false }), enumerable: true, configurable: false, spot: 10 };
    assert_ne!(format!("{:?}", p), "");
}
#[test]
fn property_descriptor_eq() {
    let items: [PropertyDescriptor; 6] = [
        PropertyDescriptor { property: PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(true), writable: false }), enumerable: true, configurable: false, spot: 10 },
        PropertyDescriptor { property: PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(false), writable: false }), enumerable: true, configurable: false, spot: 10 },
        PropertyDescriptor { property: PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(true), writable: false }), enumerable: false, configurable: false, spot: 10 },
        PropertyDescriptor { property: PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(true), writable: false }), enumerable: true, configurable: true, spot: 10 },
        PropertyDescriptor { property: PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(true), writable: false }), enumerable: true, configurable: false, spot: 1 },
        PropertyDescriptor { property: PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(false), writable: true }), enumerable: false, configurable: true, spot: 1 },
    ];
    for (right_idx, right_value) in items.iter().enumerate() {
        for (left_idx, left_value) in items.iter().enumerate() {
            assert_eq!(*left_value == *right_value, left_idx == right_idx);
            assert_eq!(*left_value != *right_value, left_idx != right_idx);
        }
    }
}
#[test]
fn property_descriptor_clone() {
    let p1 = PropertyDescriptor { property: PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(true), writable: false }), enumerable: true, configurable: true, spot: 10 };
    let p2 = p1.clone();
    assert_eq!(p1, p2);
}
#[test]
fn property_descriptor_default() {
    let pd: PropertyDescriptor = Default::default();
    let expected = PropertyDescriptor { property: Default::default(), enumerable: false, configurable: false, spot: 0 };
    assert_eq!(pd, expected);
}
#[test]
fn property_descriptor_is_data_descriptor() {
    let d = PropertyDescriptor { property: PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(true), writable: false }), enumerable: true, configurable: true, spot: 10 };
    let a = PropertyDescriptor {
        property: PropertyKind::Accessor(AccessorProperty { get: ECMAScriptValue::Undefined, set: ECMAScriptValue::Undefined }),
        enumerable: true,
        configurable: true,
        spot: 10,
    };

    assert!(d.is_data_descriptor());
    assert!(!a.is_data_descriptor());
}
#[test]
fn property_descriptor_is_accessor_descriptor() {
    let d = PropertyDescriptor { property: PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(true), writable: false }), enumerable: true, configurable: true, spot: 10 };
    let a = PropertyDescriptor {
        property: PropertyKind::Accessor(AccessorProperty { get: ECMAScriptValue::Undefined, set: ECMAScriptValue::Undefined }),
        enumerable: true,
        configurable: true,
        spot: 10,
    };

    assert!(!d.is_accessor_descriptor());
    assert!(a.is_accessor_descriptor());
}
#[test]
fn property_descriptor_is_generic_descriptor() {
    let d = PropertyDescriptor { property: PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(true), writable: false }), enumerable: true, configurable: true, spot: 10 };
    let a = PropertyDescriptor {
        property: PropertyKind::Accessor(AccessorProperty { get: ECMAScriptValue::Undefined, set: ECMAScriptValue::Undefined }),
        enumerable: true,
        configurable: true,
        spot: 10,
    };

    assert!(!d.is_generic_descriptor());
    assert!(!a.is_generic_descriptor());
}
#[test]
fn property_descriptor_writable() {
    let writable = PropertyDescriptor { property: PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(true), writable: true }), enumerable: true, configurable: true, spot: 10 };
    let constant = PropertyDescriptor { property: PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(true), writable: false }), enumerable: true, configurable: true, spot: 10 };
    let accessor = PropertyDescriptor {
        property: PropertyKind::Accessor(AccessorProperty { get: ECMAScriptValue::Undefined, set: ECMAScriptValue::Undefined }),
        enumerable: true,
        configurable: true,
        spot: 10,
    };

    assert_eq!(writable.writable(), Some(true));
    assert_eq!(constant.writable(), Some(false));
    assert_eq!(accessor.writable(), None);
}

#[test]
fn concise_property_descriptor_from() {
    let p = PropertyDescriptor { property: PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(true), writable: true }), enumerable: true, configurable: true, spot: 10 };
    let c = ConcisePropertyDescriptor::from(&p);
    assert_eq!(*c.0, p);
}
#[test]
fn concise_property_descriptor_debug() {
    let p1 = PropertyDescriptor { property: PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(true), writable: true }), enumerable: true, configurable: true, spot: 10 };
    let c1 = ConcisePropertyDescriptor::from(&p1);
    assert_eq!(format!("{:?}", c1), "{ true wec }");
    printer_validate(|w| write!(w, "{:?}", c1));
    let p2 = PropertyDescriptor { property: PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(true), writable: false }), enumerable: false, configurable: false, spot: 10 };
    let c2 = ConcisePropertyDescriptor::from(&p2);
    assert_eq!(format!("{:?}", c2), "{ true --- }");
    printer_validate(|w| write!(w, "{:?}", c2));
    let p3 = PropertyDescriptor {
        property: PropertyKind::Accessor(AccessorProperty { get: ECMAScriptValue::from(true), set: ECMAScriptValue::Undefined }),
        enumerable: false,
        configurable: true,
        spot: 10,
    };
    let c3 = ConcisePropertyDescriptor::from(&p3);
    assert_eq!(format!("{:?}", c3), "{ [[Get]]: true [[Set]]: undefined -c }");
    printer_validate(|w| write!(w, "{:?}", c3));
}

#[test]
fn potential_property_descriptor_debug() {
    let ppd: PotentialPropertyDescriptor = Default::default();
    assert_ne!(format!("{:?}", ppd), "");
}
#[test]
fn potential_property_descriptor_default() {
    let ppd: PotentialPropertyDescriptor = Default::default();
    assert_eq!(ppd, PotentialPropertyDescriptor { value: None, writable: None, get: None, set: None, enumerable: None, configurable: None });
}
#[test]
fn potential_property_descriptor_partialeq() {
    let items: [PotentialPropertyDescriptor; 8] = [
        PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(true)), writable: Some(true), get: None, set: None, enumerable: Some(true), configurable: Some(true) },
        PotentialPropertyDescriptor { value: None, writable: Some(true), get: None, set: None, enumerable: Some(true), configurable: Some(true) },
        PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(true)), writable: None, get: None, set: None, enumerable: Some(true), configurable: Some(true) },
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
        PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(true)), writable: Some(true), get: None, set: None, enumerable: None, configurable: Some(true) },
        PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(true)), writable: Some(true), get: None, set: None, enumerable: Some(true), configurable: None },
        PotentialPropertyDescriptor { value: None, writable: None, get: None, set: None, enumerable: None, configurable: None },
    ];
    for (right_idx, right_value) in items.iter().enumerate() {
        for (left_idx, left_value) in items.iter().enumerate() {
            assert_eq!(*left_value == *right_value, left_idx == right_idx);
            assert_eq!(*left_value != *right_value, left_idx != right_idx);
        }
    }
}
#[test]
fn potential_property_descriptor_clone() {
    let ppd1 = PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(true)), writable: Some(true), get: None, set: None, enumerable: Some(true), configurable: Some(true) };
    let ppd2 = ppd1.clone();
    assert_eq!(ppd1, ppd2);
}
#[test]
fn potential_property_descriptor_writable() {
    let ppd1 = PotentialPropertyDescriptor { writable: Some(true), ..Default::default() };
    let ppd2 = PotentialPropertyDescriptor { writable: Some(false), ..Default::default() };
    let ppd3 = PotentialPropertyDescriptor { writable: None, ..Default::default() };

    assert_eq!(ppd1.writable(), Some(true));
    assert_eq!(ppd2.writable(), Some(false));
    assert_eq!(ppd3.writable(), None);
}
#[test]
fn potential_property_descriptor_is_generic_descriptor() {
    let items = [
        (PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(true)), ..Default::default() }, false),
        (PotentialPropertyDescriptor { writable: Some(true), ..Default::default() }, false),
        (PotentialPropertyDescriptor { get: Some(ECMAScriptValue::from(true)), ..Default::default() }, false),
        (PotentialPropertyDescriptor { set: Some(ECMAScriptValue::from(true)), ..Default::default() }, false),
        (PotentialPropertyDescriptor { enumerable: Some(true), ..Default::default() }, true),
        (PotentialPropertyDescriptor { configurable: Some(true), ..Default::default() }, true),
    ];
    for (desc, expected) in items.iter() {
        assert_eq!(desc.is_generic_descriptor(), *expected);
    }
}
#[test]
fn potential_property_descriptor_is_data_descriptor() {
    let items = [
        (PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(true)), ..Default::default() }, true),
        (PotentialPropertyDescriptor { writable: Some(true), ..Default::default() }, true),
        (PotentialPropertyDescriptor { get: Some(ECMAScriptValue::from(true)), ..Default::default() }, false),
        (PotentialPropertyDescriptor { set: Some(ECMAScriptValue::from(true)), ..Default::default() }, false),
        (PotentialPropertyDescriptor { enumerable: Some(true), ..Default::default() }, false),
        (PotentialPropertyDescriptor { configurable: Some(true), ..Default::default() }, false),
    ];
    for (desc, expected) in items.iter() {
        assert_eq!(desc.is_data_descriptor(), *expected);
    }
}
#[test]
fn potential_property_descriptor_is_accessor_descriptor() {
    let items = [
        (PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(true)), ..Default::default() }, false),
        (PotentialPropertyDescriptor { writable: Some(true), ..Default::default() }, false),
        (PotentialPropertyDescriptor { get: Some(ECMAScriptValue::from(true)), ..Default::default() }, true),
        (PotentialPropertyDescriptor { set: Some(ECMAScriptValue::from(true)), ..Default::default() }, true),
        (PotentialPropertyDescriptor { enumerable: Some(true), ..Default::default() }, false),
        (PotentialPropertyDescriptor { configurable: Some(true), ..Default::default() }, false),
    ];
    for (desc, expected) in items.iter() {
        assert_eq!(desc.is_accessor_descriptor(), *expected);
    }
}

#[test]
fn is_accessor_descriptor_01() {
    let ppd_no = PotentialPropertyDescriptor { writable: Some(true), ..Default::default() };
    let ppd_yes = PotentialPropertyDescriptor { get: Some(ECMAScriptValue::Undefined), ..Default::default() };
    let pd_no = PropertyDescriptor { property: PropertyKind::Data(Default::default()), ..Default::default() };
    let pd_yes = PropertyDescriptor { property: PropertyKind::Accessor(Default::default()), ..Default::default() };

    assert!(is_accessor_descriptor(&ppd_yes));
    assert!(is_accessor_descriptor(&pd_yes));
    assert!(!is_accessor_descriptor(&ppd_no));
    assert!(!is_accessor_descriptor(&pd_no));
}

#[test]
fn is_data_descriptor_01() {
    let ppd_yes = PotentialPropertyDescriptor { writable: Some(true), ..Default::default() };
    let ppd_no = PotentialPropertyDescriptor { get: Some(ECMAScriptValue::Undefined), ..Default::default() };
    let pd_yes = PropertyDescriptor { property: PropertyKind::Data(Default::default()), ..Default::default() };
    let pd_no = PropertyDescriptor { property: PropertyKind::Accessor(Default::default()), ..Default::default() };

    assert!(is_data_descriptor(&ppd_yes));
    assert!(is_data_descriptor(&pd_yes));
    assert!(!is_data_descriptor(&ppd_no));
    assert!(!is_data_descriptor(&pd_no));
}

#[test]
fn is_generic_descriptor_01() {
    let ppd_data = PotentialPropertyDescriptor { writable: Some(true), ..Default::default() };
    let ppd_acc = PotentialPropertyDescriptor { get: Some(ECMAScriptValue::Undefined), ..Default::default() };
    let pd_data = PropertyDescriptor { property: PropertyKind::Data(Default::default()), ..Default::default() };
    let pd_acc = PropertyDescriptor { property: PropertyKind::Accessor(Default::default()), ..Default::default() };
    let pd_def: PropertyDescriptor = Default::default();
    let ppd_def: PotentialPropertyDescriptor = Default::default();

    assert!(!is_generic_descriptor(&ppd_data));
    assert!(!is_generic_descriptor(&pd_data));
    assert!(!is_generic_descriptor(&ppd_acc));
    assert!(!is_generic_descriptor(&pd_acc));
    assert!(!is_generic_descriptor(&pd_def));
    assert!(is_generic_descriptor(&ppd_def));
}

#[test]
fn ordinary_get_prototype_of_01() {
    let mut agent = test_agent();
    let object_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
    let obj = ordinary_object_create(&mut agent, Some(&object_proto), &[]);

    let result = ordinary_get_prototype_of(&obj);
    assert_eq!(result, Some(object_proto));
}

#[test]
fn ordinary_set_prototype_of_01() {
    let mut agent = test_agent();
    let object_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
    let new_proto = ordinary_object_create(&mut agent, Some(&object_proto), &[]);
    let obj = ordinary_object_create(&mut agent, Some(&object_proto), &[]);

    let result = ordinary_set_prototype_of(&obj, Some(new_proto.clone()));
    assert!(result);
    assert_eq!(ordinary_get_prototype_of(&obj), Some(new_proto));
}
#[test]
fn ordinary_set_prototype_of_02() {
    let mut agent = test_agent();
    let obj = ordinary_object_create(&mut agent, None, &[]);

    let result = ordinary_set_prototype_of(&obj, None);
    assert!(result);
    assert_eq!(ordinary_get_prototype_of(&obj), None);
}
#[test]
fn ordinary_set_prototype_of_03() {
    let mut agent = test_agent();
    let object_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
    let obj = ordinary_object_create(&mut agent, Some(&object_proto), &[]);

    let result = ordinary_set_prototype_of(&obj, Some(object_proto.clone()));
    assert!(result);
    assert_eq!(ordinary_get_prototype_of(&obj), Some(object_proto));
}
#[test]
fn ordinary_set_prototype_of_04() {
    let mut agent = test_agent();
    let object_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
    let obj = ordinary_object_create(&mut agent, None, &[]);

    let result = ordinary_set_prototype_of(&obj, Some(object_proto.clone()));
    assert!(result);
    assert_eq!(ordinary_get_prototype_of(&obj), Some(object_proto));
}
#[test]
fn ordinary_set_prototype_of_05() {
    let mut agent = test_agent();
    let object_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
    let obj = ordinary_object_create(&mut agent, Some(&object_proto), &[]);
    obj.o.prevent_extensions(&mut agent).unwrap();

    let result = ordinary_set_prototype_of(&obj, None);
    assert!(!result);
    assert_eq!(ordinary_get_prototype_of(&obj), Some(object_proto));
}
#[test]
fn ordinary_set_prototype_of_06() {
    let mut agent = test_agent();
    let object_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
    let obj = ordinary_object_create(&mut agent, Some(&object_proto), &[]);

    let result = ordinary_set_prototype_of(&obj, Some(obj.clone()));
    assert!(!result);
    assert_eq!(ordinary_get_prototype_of(&obj), Some(object_proto));
}

#[test]
fn ordinary_is_extensible_01() {
    let mut agent = test_agent();
    let object_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
    let obj = ordinary_object_create(&mut agent, Some(&object_proto), &[]);

    let result = ordinary_is_extensible(&obj);
    assert!(result);

    ordinary_prevent_extensions(&obj);
    let result = ordinary_is_extensible(&obj);
    assert!(!result);
}

#[test]
fn ordinary_prevent_extensions_01() {
    let mut agent = test_agent();
    let object_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
    let obj = ordinary_object_create(&mut agent, Some(&object_proto), &[]);

    let result = ordinary_prevent_extensions(&obj);
    assert!(result);
    assert!(!ordinary_is_extensible(&obj));
}

#[test]
fn ordinary_get_own_property_01() {
    let mut agent = test_agent();
    let object_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
    let obj = ordinary_object_create(&mut agent, Some(&object_proto), &[]);
    let key = PropertyKey::from("a");

    let result = ordinary_get_own_property(&obj, &key);
    assert!(result.is_none());
}
#[test]
fn ordinary_get_own_property_02() {
    let mut agent = test_agent();
    let object_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
    let obj = ordinary_object_create(&mut agent, Some(&object_proto), &[]);
    let key = PropertyKey::from("a");
    let ppd = PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(10)), writable: Some(true), enumerable: Some(true), configurable: Some(true), ..Default::default() };
    define_property_or_throw(&mut agent, &obj, &key, &ppd).unwrap();

    let result = ordinary_get_own_property(&obj, &key).unwrap();
    assert_eq!(result.configurable, true);
    assert_eq!(result.enumerable, true);
    assert!(matches!(result.property, PropertyKind::Data(..)));
    if let PropertyKind::Data(data) = &result.property {
        assert_eq!(data.value, ECMAScriptValue::from(10));
        assert_eq!(data.writable, true);
    }
}

#[test]
fn ordinary_define_own_property_01() {
    // Add a new property, object is extensible (the default)
    let mut agent = test_agent();
    let object_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
    let obj = ordinary_object_create(&mut agent, Some(&object_proto), &[]);
    let key = PropertyKey::from("a");
    let ppd = PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(10)), writable: Some(true), enumerable: Some(true), configurable: Some(true), ..Default::default() };

    let result = ordinary_define_own_property(&mut agent, &obj, &key, &ppd).unwrap();

    assert!(result);
    let prop = obj.o.get_own_property(&mut agent, &key).unwrap().unwrap();
    assert_eq!(prop.configurable, true);
    assert_eq!(prop.enumerable, true);
    assert!(matches!(prop.property, PropertyKind::Data(..)));
    if let PropertyKind::Data(data) = &prop.property {
        assert_eq!(data.value, ECMAScriptValue::from(10));
        assert_eq!(data.writable, true);
    }
}
#[test]
fn ordinary_define_own_property_02() {
    // Add a new property, object is not extensible
    let mut agent = test_agent();
    let object_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
    let obj = ordinary_object_create(&mut agent, Some(&object_proto), &[]);
    let key = PropertyKey::from("a");
    let ppd = PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(10)), writable: Some(true), enumerable: Some(true), configurable: Some(true), ..Default::default() };
    obj.o.prevent_extensions(&mut agent).unwrap();

    let result = ordinary_define_own_property(&mut agent, &obj, &key, &ppd).unwrap();

    assert!(!result);
}
#[test]
fn ordinary_define_own_property_03() {
    // Change an existing property
    let mut agent = test_agent();
    let object_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
    let obj = ordinary_object_create(&mut agent, Some(&object_proto), &[]);
    let key = PropertyKey::from("a");
    let initial = PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(10)), writable: Some(true), enumerable: Some(true), configurable: Some(true), ..Default::default() };
    define_property_or_throw(&mut agent, &obj, &key, &initial).unwrap();
    let ppd = PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(0)), ..Default::default() };

    let result = ordinary_define_own_property(&mut agent, &obj, &key, &ppd).unwrap();

    assert!(result);
    let prop = obj.o.get_own_property(&mut agent, &key).unwrap().unwrap();
    assert_eq!(prop.configurable, true);
    assert_eq!(prop.enumerable, true);
    assert!(matches!(prop.property, PropertyKind::Data(..)));
    if let PropertyKind::Data(data) = &prop.property {
        assert_eq!(data.value, ECMAScriptValue::from(0));
        assert_eq!(data.writable, true);
    }
}
#[test]
fn ordinary_define_own_property_04() {
    // [[GetOwnProperty]] throws
    let mut agent = test_agent();
    let obj = TestObject::object(&mut agent, &[FunctionId::GetOwnProperty]);
    let key = PropertyKey::from("a");
    let ppd = PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(10)), writable: Some(true), enumerable: Some(true), configurable: Some(true), ..Default::default() };

    let result = ordinary_define_own_property(&mut agent, &obj, &key, &ppd).unwrap_err();

    let msg = unwind_type_error(&mut agent, result);
    assert_eq!(msg, "[[GetOwnProperty]] called on TestObject");
}
#[test]
fn ordinary_define_own_property_05() {
    // [[IsExtensible]] throws
    let mut agent = test_agent();
    let obj = TestObject::object(&mut agent, &[FunctionId::IsExtensible]);
    let key = PropertyKey::from("a");
    let ppd = PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(10)), writable: Some(true), enumerable: Some(true), configurable: Some(true), ..Default::default() };

    let result = ordinary_define_own_property(&mut agent, &obj, &key, &ppd).unwrap_err();

    let msg = unwind_type_error(&mut agent, result);
    assert_eq!(msg, "[[IsExtensible]] called on TestObject");
}

#[test]
fn validate_and_apply_property_descriptor_01() {
    // current = Undefined & extensible = false => false
    let ppd = PotentialPropertyDescriptor { ..Default::default() };
    let result = validate_and_apply_property_descriptor::<&Object>(None, None, false, &ppd, None);
    assert!(!result);
}
#[test]
fn validate_and_apply_property_descriptor_02() {
    // current = Undefined & extensible = true & O = Undefined => true
    let ppd = PotentialPropertyDescriptor { ..Default::default() };
    let result = validate_and_apply_property_descriptor::<&Object>(None, None, true, &ppd, None);
    assert!(result);
}
#[test]
fn validate_and_apply_property_descriptor_03() {
    // current Undefined; empty descriptor
    let mut agent = test_agent();
    let object_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
    let obj = ordinary_object_create(&mut agent, Some(&object_proto), &[]);
    let ppd = PotentialPropertyDescriptor { ..Default::default() };
    let key = PropertyKey::from("key");

    let result = validate_and_apply_property_descriptor(Some(&obj), Some(&key), true, &ppd, None);

    assert!(result);
    let pd = obj.o.get_own_property(&mut agent, &key).unwrap().unwrap();
    assert_eq!(pd.configurable, false);
    assert_eq!(pd.enumerable, false);
    assert_eq!(pd.property, PropertyKind::Data(DataProperty { value: ECMAScriptValue::Undefined, writable: false }));
}
#[test]
fn validate_and_apply_property_descriptor_04() {
    // current Undefined; overfull descriptor
    let mut agent = test_agent();
    let object_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
    let obj = ordinary_object_create(&mut agent, Some(&object_proto), &[]);
    let ppd = PotentialPropertyDescriptor {
        value: Some(ECMAScriptValue::from(true)),
        writable: Some(true),
        enumerable: Some(true),
        configurable: Some(true),
        get: Some(ECMAScriptValue::from("get")),
        set: Some(ECMAScriptValue::from("set")),
    };
    let key = PropertyKey::from("key");

    let result = validate_and_apply_property_descriptor(Some(&obj), Some(&key), true, &ppd, None);

    assert!(result);
    let pd = obj.o.get_own_property(&mut agent, &key).unwrap().unwrap();
    assert_eq!(pd.configurable, true);
    assert_eq!(pd.enumerable, true);
    assert_eq!(pd.property, PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(true), writable: true }));
}
#[test]
fn validate_and_apply_property_descriptor_05() {
    // current Undefined; accessor descriptor
    let mut agent = test_agent();
    let object_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
    let obj = ordinary_object_create(&mut agent, Some(&object_proto), &[]);
    let ppd = PotentialPropertyDescriptor {
        enumerable: Some(true),
        configurable: Some(true),
        get: Some(ECMAScriptValue::from(agent.intrinsic(IntrinsicId::ThrowTypeError))),
        set: Some(ECMAScriptValue::Undefined),
        ..Default::default()
    };
    let key = PropertyKey::from("key");

    let result = validate_and_apply_property_descriptor(Some(&obj), Some(&key), true, &ppd, None);

    assert!(result);
    let pd = obj.o.get_own_property(&mut agent, &key).unwrap().unwrap();
    assert_eq!(pd.configurable, true);
    assert_eq!(pd.enumerable, true);
    assert_eq!(pd.property, PropertyKind::Accessor(AccessorProperty { get: ECMAScriptValue::from(agent.intrinsic(IntrinsicId::ThrowTypeError)), set: ECMAScriptValue::Undefined }));
}
#[test]
fn validate_and_apply_property_descriptor_06() {
    // object Undefined; current reasonable; any valid input
    let mut agent = test_agent();
    let object_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
    let obj = ordinary_object_create(&mut agent, Some(&object_proto), &[]);
    let existing = PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(99)), writable: Some(true), enumerable: Some(true), configurable: Some(true), ..Default::default() };
    let key = PropertyKey::from("key");
    define_property_or_throw(&mut agent, &obj, &key, &existing).unwrap();
    let current = obj.o.get_own_property(&mut agent, &key).unwrap().unwrap();

    let ppd = PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(10)), ..Default::default() };

    let result = validate_and_apply_property_descriptor::<&Object>(None, Some(&key), true, &ppd, Some(&current));

    assert!(result);
    let pd = obj.o.get_own_property(&mut agent, &key).unwrap().unwrap();
    assert_eq!(pd.configurable, true);
    assert_eq!(pd.enumerable, true);
    assert_eq!(pd.property, PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(99), writable: true }));
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// ONE BIG TEST to check all different PotentialPropertyDescriptors against all different existing PropertyDescriptors.
// (There are 23,328 different tests included in this.)
#[derive(PartialEq)]
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
    fn new(agent: &Agent) -> Self {
        VAPDIter { tte: ECMAScriptValue::from(agent.intrinsic(IntrinsicId::ThrowTypeError)), ..Default::default() }
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
                    value: if self.stage == Stage::Data { Some(ECMAScriptValue::from(self.value as i32)) } else { None },
                    writable: if self.stage == Stage::Data { Some(self.writable) } else { None },
                    enumerable: Some(self.enumerable),
                    configurable: Some(self.configurable),
                    get: if self.stage == Stage::Accessor { Some(if self.get_throws { self.tte.clone() } else { ECMAScriptValue::Undefined }) } else { None },
                    set: if self.stage == Stage::Accessor { Some(if self.set_throws { self.tte.clone() } else { ECMAScriptValue::Undefined }) } else { None },
                },
            ));

            if !self.configurable {
                self.configurable = true;
            } else {
                self.configurable = false;
                if !self.enumerable {
                    self.enumerable = true;
                } else {
                    self.enumerable = false;
                    if self.stage == Stage::Data {
                        if !self.writable {
                            self.writable = true;
                        } else {
                            self.writable = false;
                            if self.value == 0 {
                                self.value = 1;
                            } else {
                                self.stage = Stage::Accessor;
                            }
                        }
                    } else if !self.get_throws {
                        self.get_throws = true;
                    } else {
                        self.get_throws = false;
                        if !self.set_throws {
                            self.set_throws = true;
                        } else {
                            self.stage = Stage::Done;
                        }
                    }
                }
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
    fn new(agent: &Agent) -> Self {
        Self { tte: ECMAScriptValue::from(agent.intrinsic(IntrinsicId::ThrowTypeError)), ..Default::default() }
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
fn figure_expectation(current: &PropertyDescriptor, incoming: &PotentialPropertyDescriptor) -> Option<PropertyDescriptor> {
    let config_changed = current.configurable != incoming.configurable.unwrap_or(current.configurable);
    let enumerable_changed = current.enumerable != incoming.enumerable.unwrap_or(current.enumerable);
    let current_configurable = current.configurable;
    let current_enumurable = current.enumerable;
    let kind_changed =
        current.is_data_descriptor() && incoming.is_accessor_descriptor() && !incoming.is_data_descriptor() || current.is_accessor_descriptor() && incoming.is_data_descriptor();

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
                        property: PropertyKind::Data(DataProperty { writable: incoming.writable.unwrap_or(dp.writable), value: new_value }),
                        configurable: incoming.configurable.unwrap_or(current_configurable),
                        enumerable: incoming.enumerable.unwrap_or(current_enumurable),
                        spot: current.spot,
                    })
                }
            } else if dp.writable {
                // Otherwise, if existing is writable, changes to writable and value are allowed (but not configurable or enumerable)
                if !config_changed && !enumerable_changed && !kind_changed {
                    Some(PropertyDescriptor {
                        property: PropertyKind::Data(DataProperty { writable: incoming.writable.unwrap_or(dp.writable), value: new_value }),
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
                        property: PropertyKind::Data(DataProperty { value: new_value, writable: incoming.writable.unwrap_or(false) }),
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
                if !config_changed && !enumerable_changed && !get_changed && !set_changed && !incoming.is_data_descriptor() {
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
    let mut agent = test_agent();
    let object_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
    let obj = ordinary_object_create(&mut agent, Some(&object_proto), &[]);
    for (name, ppd) in VAPDIter::new(&agent) {
        for (idx, _) in VAPDCheck::new(&agent).enumerate() {
            let key = PropertyKey::from(format!("{}-{}", name, idx));
            define_property_or_throw(&mut agent, &obj, &key, &ppd).unwrap();
        }
    }

    for (name, _) in VAPDIter::new(&agent) {
        for (idx, check) in VAPDCheck::new(&agent).enumerate() {
            let key = PropertyKey::from(format!("{}-{}", name, idx));
            let current = obj.o.get_own_property(&mut agent, &key).unwrap().unwrap();
            let expected_prop = figure_expectation(&current, &check);

            let result = validate_and_apply_property_descriptor(Some(&obj), Some(&key), true, &check, Some(&current));
            assert_eq!(result, expected_prop.is_some(), "\nkey: {:?};\ncurrent: {:#?};\nPPD: {:#?};\nexpected: {:#?}", key, current, check, expected_prop);
            let after = obj.o.get_own_property(&mut agent, &key).unwrap().unwrap();
            assert_eq!(&after, expected_prop.as_ref().unwrap_or(&current), "\nkey: {:?};\ncurrent: {:#?};\nPPD: {:#?};\nexpected: {:#?};", key, current, check, expected_prop);
        }
    }
}

#[test]
fn ordinary_object_create_01() {
    // When: An agent is given
    let mut agent = test_agent();

    // Then requesting a new object with no prototype or extra slots
    let obj = ordinary_object_create(&mut agent, None, &[]);

    // Gives us the emptiest of all objects
    let data = obj.o.common_object_data().borrow();
    assert_eq!(data.prototype, None);
    assert_eq!(data.extensible, true);
    assert_eq!(data.properties.len(), 0);
}

#[test]
fn ordinary_object_create_02() {
    // When an agent and a prototype are provided
    let mut agent = test_agent();
    let proto = ordinary_object_create(&mut agent, None, &[]);

    // Then requesting a new object with that prototype but no extra slots
    let obj = ordinary_object_create(&mut agent, Some(&proto), &[]);

    // Gives us an empty object with its prototype slot filled.
    let data = obj.o.common_object_data().borrow();
    assert_eq!(data.prototype.as_ref(), Some(&proto));
    assert_eq!(data.extensible, true);
    assert_eq!(data.properties.len(), 0);
    assert_ne!(&obj, &proto);
}

#[test]
fn ordinary_object_create_03a() {
    // When an agent and a prototype are provided
    let mut agent = test_agent();
    let proto = ordinary_object_create(&mut agent, None, &[]);

    // Then requesting a new object with that prototype and needlessly requesting prototype or extensible slots
    let obj = ordinary_object_create(&mut agent, Some(&proto), &[InternalSlotName::Prototype]);

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
    let mut agent = test_agent();
    let proto = ordinary_object_create(&mut agent, None, &[]);

    // Then requesting a new object with that prototype and needlessly requesting prototype or extensible slots
    let obj = ordinary_object_create(&mut agent, Some(&proto), &[InternalSlotName::Extensible]);

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
    let mut agent = test_agent();
    let proto = ordinary_object_create(&mut agent, None, &[]);

    // Then requesting a new object with that prototype and needlessly requesting prototype or extensible slots
    let obj = ordinary_object_create(&mut agent, Some(&proto), &[InternalSlotName::Prototype, InternalSlotName::Extensible]);

    // Gives us an empty object with its prototype slot filled.
    let data = obj.o.common_object_data().borrow();
    assert_eq!(data.prototype.as_ref(), Some(&proto));
    assert_eq!(data.extensible, true);
    assert_eq!(data.properties.len(), 0);
    assert_ne!(&obj, &proto);
}

#[test]
#[should_panic]
fn make_basic_object_01() {
    let mut agent = test_agent();
    let _obj = make_basic_object(&mut agent, &[InternalSlotName::Nonsense], None);
}
#[test]
#[should_panic]
fn make_basic_object_02() {
    let mut agent = test_agent();
    let _obj = make_basic_object(&mut agent, &[InternalSlotName::Nonsense, InternalSlotName::Prototype, InternalSlotName::Extensible], None);
}

#[test]
fn get_prototype_of_01() {
    let mut agent = test_agent();
    let obj = ordinary_object_create(&mut test_agent(), None, &[]);
    let result = obj.o.get_prototype_of(&mut agent);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), None);
}
#[test]
fn get_prototype_of_02() {
    let mut agent = test_agent();
    let proto = ordinary_object_create(&mut agent, None, &[]);
    let obj = ordinary_object_create(&mut agent, Some(&proto), &[]);
    let result = obj.o.get_prototype_of(&mut agent);
    assert!(result.is_ok());
    assert_eq!(result.unwrap().as_ref(), Some(&proto));
}

#[test]
fn set_prototype_of_01() {
    // Not changing an empty prototype
    let mut agent = test_agent();
    let obj_a = ordinary_object_create(&mut agent, None, &[]);
    let result = obj_a.o.set_prototype_of(&mut agent, None);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), true);
    assert_eq!(obj_a.o.common_object_data().borrow().prototype, None);
}
#[test]
fn set_prototype_of_02() {
    // Not changing a Some() prototype
    let mut agent = test_agent();
    let obj_a = ordinary_object_create(&mut agent, None, &[]);
    let obj_b = ordinary_object_create(&mut agent, Some(&obj_a), &[]);
    let result = obj_b.o.set_prototype_of(&mut agent, Some(obj_a.clone()));
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), true);
    assert_eq!(obj_b.o.common_object_data().borrow().prototype.as_ref(), Some(&obj_a));
}
#[test]
fn set_prototype_of_03() {
    // Changing a Some() prototype to a different Some() prototype
    let mut agent = test_agent();
    let proto = ordinary_object_create(&mut agent, None, &[]);
    let obj_b = ordinary_object_create(&mut agent, Some(&proto), &[]);
    let new_proto = ordinary_object_create(&mut agent, None, &[]);
    let result = obj_b.o.set_prototype_of(&mut agent, Some(new_proto.clone()));
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), true);
    assert_eq!(obj_b.o.common_object_data().borrow().prototype.as_ref(), Some(&new_proto));
}
#[test]
fn set_prototype_of_04() {
    // Trying to make a prototype loop
    let mut agent = test_agent();
    let proto = ordinary_object_create(&mut agent, None, &[]);
    let obj_b = ordinary_object_create(&mut agent, Some(&proto), &[]);
    let result = proto.o.set_prototype_of(&mut agent, Some(obj_b));
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), false);
    assert_eq!(proto.o.common_object_data().borrow().prototype.as_ref(), None);
}
#[test]
fn set_prototype_of_05() {
    // Changing the prototype of an object that's not extensible
    let mut agent = test_agent();
    let proto = ordinary_object_create(&mut agent, None, &[]);
    let obj_b = ordinary_object_create(&mut agent, Some(&proto), &[]);
    obj_b.o.common_object_data().borrow_mut().extensible = false;
    let new_proto = ordinary_object_create(&mut agent, None, &[]);
    let result = obj_b.o.set_prototype_of(&mut agent, Some(new_proto));
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), false);
    assert_eq!(obj_b.o.common_object_data().borrow().prototype.as_ref(), Some(&proto));
}

#[test]
fn is_extensible_01() {
    let mut agent = test_agent();
    let obj = ordinary_object_create(&mut agent, None, &[]);
    let result = obj.o.is_extensible(&mut agent);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), true);
}
#[test]
fn is_extensible_02() {
    let mut agent = test_agent();
    let obj = ordinary_object_create(&mut agent, None, &[]);
    obj.o.common_object_data().borrow_mut().extensible = false;
    let result = obj.o.is_extensible(&mut agent);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), false);
}

#[test]
fn prevent_extensions_01() {
    let mut agent = test_agent();
    let obj = ordinary_object_create(&mut agent, None, &[]);
    let result = obj.o.prevent_extensions(&mut agent);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), true);
    assert_eq!(obj.o.common_object_data().borrow().extensible, false);
}

#[test]
fn get_own_property_01() {
    let mut agent = test_agent();
    let obj = ordinary_object_create(&mut agent, None, &[]);
    let key = PropertyKey::String(JSString::from("blue"));
    let result = obj.o.get_own_property(&mut agent, &key);
    assert!(result.is_ok());
    assert!(result.unwrap().is_none());
}
#[test]
fn get_own_property_02() {
    let mut agent = test_agent();
    let obj = ordinary_object_create(&mut agent, None, &[]);
    let key = PropertyKey::String(JSString::from("blue"));
    let value = ECMAScriptValue::Number(89.0);
    let desc = PotentialPropertyDescriptor { value: Some(value), writable: Some(false), enumerable: Some(true), configurable: None, get: None, set: None };
    obj.o.define_own_property(&mut agent, &key, &desc).expect("oops");

    let result = obj.o.get_own_property(&mut agent, &key);
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
    let mut agent = test_agent();

    let obj = ordinary_object_create(&mut agent, None, &[]);
    let key = PropertyKey::String(JSString::from("blue"));
    let value = ECMAScriptValue::Number(56.7);

    set(&mut agent, &obj, &key, &value, false).unwrap();
    let result = get(&mut agent, &obj, &key).unwrap();

    assert_eq!(result, ECMAScriptValue::Number(56.7));
}
