use super::*;
use crate::errors::unwind_any_error;
use crate::function_object::create_builtin_function;
use crate::strings::JSString;
use crate::tests::{printer_validate, test_agent, unwind_type_error, AdaptableMethods, AdaptableObject, FunctionId, TestObject};
use crate::values::to_object;
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
fn property_descriptor_is_writable() {
    let writable = PropertyDescriptor { property: PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(true), writable: true }), enumerable: true, configurable: true, spot: 10 };
    let constant = PropertyDescriptor { property: PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(true), writable: false }), enumerable: true, configurable: true, spot: 10 };
    let accessor = PropertyDescriptor {
        property: PropertyKind::Accessor(AccessorProperty { get: ECMAScriptValue::Undefined, set: ECMAScriptValue::Undefined }),
        enumerable: true,
        configurable: true,
        spot: 10,
    };

    assert_eq!(writable.is_writable(), Some(true));
    assert_eq!(constant.is_writable(), Some(false));
    assert_eq!(accessor.is_writable(), None);
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
fn potential_property_descriptor_is_writable() {
    let ppd1 = PotentialPropertyDescriptor { writable: Some(true), ..Default::default() };
    let ppd2 = PotentialPropertyDescriptor { writable: Some(false), ..Default::default() };
    let ppd3 = PotentialPropertyDescriptor { writable: None, ..Default::default() };

    assert_eq!(ppd1.is_writable(), Some(true));
    assert_eq!(ppd2.is_writable(), Some(false));
    assert_eq!(ppd3.is_writable(), None);
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
    let obj = ordinary_object_create(&mut agent, Some(object_proto.clone()), &[]);

    let result = ordinary_get_prototype_of(&obj);
    assert_eq!(result, Some(object_proto));
}

#[test]
fn ordinary_set_prototype_of_01() {
    let mut agent = test_agent();
    let object_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
    let new_proto = ordinary_object_create(&mut agent, Some(object_proto.clone()), &[]);
    let obj = ordinary_object_create(&mut agent, Some(object_proto), &[]);

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
    let obj = ordinary_object_create(&mut agent, Some(object_proto.clone()), &[]);

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
    let obj = ordinary_object_create(&mut agent, Some(object_proto.clone()), &[]);
    obj.o.prevent_extensions(&mut agent).unwrap();

    let result = ordinary_set_prototype_of(&obj, None);
    assert!(!result);
    assert_eq!(ordinary_get_prototype_of(&obj), Some(object_proto));
}
#[test]
fn ordinary_set_prototype_of_06() {
    let mut agent = test_agent();
    let object_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
    let obj = ordinary_object_create(&mut agent, Some(object_proto.clone()), &[]);

    let result = ordinary_set_prototype_of(&obj, Some(obj.clone()));
    assert!(!result);
    assert_eq!(ordinary_get_prototype_of(&obj), Some(object_proto));
}

#[test]
fn ordinary_is_extensible_01() {
    let mut agent = test_agent();
    let object_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
    let obj = ordinary_object_create(&mut agent, Some(object_proto), &[]);

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
    let obj = ordinary_object_create(&mut agent, Some(object_proto), &[]);

    let result = ordinary_prevent_extensions(&obj);
    assert!(result);
    assert!(!ordinary_is_extensible(&obj));
}

#[test]
fn ordinary_get_own_property_01() {
    let mut agent = test_agent();
    let object_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
    let obj = ordinary_object_create(&mut agent, Some(object_proto), &[]);
    let key = PropertyKey::from("a");

    let result = ordinary_get_own_property(&obj, &key);
    assert!(result.is_none());
}
#[test]
fn ordinary_get_own_property_02() {
    let mut agent = test_agent();
    let object_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
    let obj = ordinary_object_create(&mut agent, Some(object_proto), &[]);
    let key = PropertyKey::from("a");
    let ppd = PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(10)), writable: Some(true), enumerable: Some(true), configurable: Some(true), ..Default::default() };
    define_property_or_throw(&mut agent, &obj, key.clone(), ppd).unwrap();

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
    let obj = ordinary_object_create(&mut agent, Some(object_proto), &[]);
    let key = PropertyKey::from("a");
    let ppd = PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(10)), writable: Some(true), enumerable: Some(true), configurable: Some(true), ..Default::default() };

    let result = ordinary_define_own_property(&mut agent, &obj, key.clone(), ppd).unwrap();

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
    let obj = ordinary_object_create(&mut agent, Some(object_proto), &[]);
    let key = PropertyKey::from("a");
    let ppd = PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(10)), writable: Some(true), enumerable: Some(true), configurable: Some(true), ..Default::default() };
    obj.o.prevent_extensions(&mut agent).unwrap();

    let result = ordinary_define_own_property(&mut agent, &obj, key, ppd).unwrap();

    assert!(!result);
}
#[test]
fn ordinary_define_own_property_03() {
    // Change an existing property
    let mut agent = test_agent();
    let object_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
    let obj = ordinary_object_create(&mut agent, Some(object_proto), &[]);
    let key = PropertyKey::from("a");
    let initial = PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(10)), writable: Some(true), enumerable: Some(true), configurable: Some(true), ..Default::default() };
    define_property_or_throw(&mut agent, &obj, key.clone(), initial).unwrap();
    let ppd = PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(0)), ..Default::default() };

    let result = ordinary_define_own_property(&mut agent, &obj, key.clone(), ppd).unwrap();

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
    let obj = TestObject::object(&mut agent, &[FunctionId::GetOwnProperty(None)]);
    let key = PropertyKey::from("a");
    let ppd = PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(10)), writable: Some(true), enumerable: Some(true), configurable: Some(true), ..Default::default() };

    let result = ordinary_define_own_property(&mut agent, &obj, key, ppd).unwrap_err();

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

    let result = ordinary_define_own_property(&mut agent, &obj, key, ppd).unwrap_err();

    let msg = unwind_type_error(&mut agent, result);
    assert_eq!(msg, "[[IsExtensible]] called on TestObject");
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
    let mut agent = test_agent();
    let object_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
    let obj = ordinary_object_create(&mut agent, Some(object_proto), &[]);
    let ppd = PotentialPropertyDescriptor { ..Default::default() };
    let key = PropertyKey::from("key");

    let result = validate_and_apply_property_descriptor(Some(&obj), Some(key.clone()), true, ppd, None);

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
    let obj = ordinary_object_create(&mut agent, Some(object_proto), &[]);
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
    let obj = ordinary_object_create(&mut agent, Some(object_proto), &[]);
    let ppd = PotentialPropertyDescriptor {
        enumerable: Some(true),
        configurable: Some(true),
        get: Some(ECMAScriptValue::from(agent.intrinsic(IntrinsicId::ThrowTypeError))),
        set: Some(ECMAScriptValue::Undefined),
        ..Default::default()
    };
    let key = PropertyKey::from("key");

    let result = validate_and_apply_property_descriptor(Some(&obj), Some(key.clone()), true, ppd, None);

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
    let obj = ordinary_object_create(&mut agent, Some(object_proto), &[]);
    let existing = PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(99)), writable: Some(true), enumerable: Some(true), configurable: Some(true), ..Default::default() };
    let key = PropertyKey::from("key");
    define_property_or_throw(&mut agent, &obj, key.clone(), existing).unwrap();
    let current = obj.o.get_own_property(&mut agent, &key).unwrap().unwrap();

    let ppd = PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(10)), ..Default::default() };

    let result = validate_and_apply_property_descriptor::<&Object>(None, Some(key.clone()), true, ppd, Some(&current));

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
    let obj = ordinary_object_create(&mut agent, Some(object_proto), &[]);
    for (name, ppd) in VAPDIter::new(&agent) {
        for (idx, _) in VAPDCheck::new(&agent).enumerate() {
            let key = PropertyKey::from(format!("{}-{}", name, idx));
            define_property_or_throw(&mut agent, &obj, key, ppd.clone()).unwrap();
        }
    }

    for (name, _) in VAPDIter::new(&agent) {
        for (idx, check) in VAPDCheck::new(&agent).enumerate() {
            let key = PropertyKey::from(format!("{}-{}", name, idx));
            let current = obj.o.get_own_property(&mut agent, &key).unwrap().unwrap();
            let expected_prop = figure_expectation(&current, &check);

            let result = validate_and_apply_property_descriptor(Some(&obj), Some(key.clone()), true, check.clone(), Some(&current));
            assert_eq!(result, expected_prop.is_some(), "\nkey: {:?};\ncurrent: {:#?};\nPPD: {:#?};\nexpected: {:#?}", key, current, check, expected_prop);
            let after = obj.o.get_own_property(&mut agent, &key).unwrap().unwrap();
            assert_eq!(&after, expected_prop.as_ref().unwrap_or(&current), "\nkey: {:?};\ncurrent: {:#?};\nPPD: {:#?};\nexpected: {:#?};", key, current, check, expected_prop);
        }
    }
}

#[test]
fn ordinary_has_property_01() {
    let mut agent = test_agent();
    let object_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
    let obj = ordinary_object_create(&mut agent, Some(object_proto), &[]);
    let initial = PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(true)), writable: Some(true), configurable: Some(true), enumerable: Some(true), ..Default::default() };
    let key = PropertyKey::from("a");
    define_property_or_throw(&mut agent, &obj, key.clone(), initial).unwrap();
    let key2 = PropertyKey::from("b");
    let key3 = PropertyKey::from("toString");

    // own property
    let result = ordinary_has_property(&mut agent, &obj, &key).unwrap();
    assert!(result);

    // property not there
    let result = ordinary_has_property(&mut agent, &obj, &key2).unwrap();
    assert!(!result);

    // parent property
    let result = ordinary_has_property(&mut agent, &obj, &key3).unwrap();
    assert!(result);
}
#[test]
fn ordinary_has_property_02() {
    // [[GetOwnProperty]] throws
    let mut agent = test_agent();
    let obj = TestObject::object(&mut agent, &[FunctionId::GetOwnProperty(None)]);
    let key = PropertyKey::from("a");

    let result = ordinary_has_property(&mut agent, &obj, &key).unwrap_err();
    assert_eq!(unwind_type_error(&mut agent, result), "[[GetOwnProperty]] called on TestObject");
}
#[test]
fn ordinary_has_property_03() {
    // [GetPrototypeOf]] throws
    let mut agent = test_agent();
    let obj = TestObject::object(&mut agent, &[FunctionId::GetPrototypeOf]);
    let key = PropertyKey::from("a");

    let result = ordinary_has_property(&mut agent, &obj, &key).unwrap_err();
    assert_eq!(unwind_type_error(&mut agent, result), "[[GetPrototypeOf]] called on TestObject");
}

#[test]
fn ordinary_get_01() {
    // [[GetOwnProperty]] throws
    let mut agent = test_agent();
    let obj = TestObject::object(&mut agent, &[FunctionId::GetOwnProperty(None)]);
    let key = PropertyKey::from("a");

    let result = ordinary_get(&mut agent, &obj, &key, &ECMAScriptValue::Undefined).unwrap_err();
    assert_eq!(unwind_type_error(&mut agent, result), "[[GetOwnProperty]] called on TestObject");
}
#[test]
fn ordinary_get_02() {
    // [[GetPrototypeOf]] throws
    let mut agent = test_agent();
    let obj = TestObject::object(&mut agent, &[FunctionId::GetPrototypeOf]);
    let key = PropertyKey::from("a");

    let result = ordinary_get(&mut agent, &obj, &key, &ECMAScriptValue::Undefined).unwrap_err();
    assert_eq!(unwind_type_error(&mut agent, result), "[[GetPrototypeOf]] called on TestObject");
}
#[test]
fn ordinary_get_03() {
    // Top of the prototype chain
    let mut agent = test_agent();
    let obj = ordinary_object_create(&mut agent, None, &[]);
    let key = PropertyKey::from("a");

    let result = ordinary_get(&mut agent, &obj, &key, &ECMAScriptValue::Undefined).unwrap();
    assert_eq!(result, ECMAScriptValue::Undefined);
}
#[test]
fn ordinary_get_04() {
    // Normal data property
    let mut agent = test_agent();
    let obj = ordinary_object_create(&mut agent, None, &[]);
    let key = PropertyKey::from("a");
    let initial = PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(0)), writable: Some(true), enumerable: Some(true), configurable: Some(true), ..Default::default() };
    define_property_or_throw(&mut agent, &obj, key.clone(), initial).unwrap();

    let result = ordinary_get(&mut agent, &obj, &key, &ECMAScriptValue::Undefined).unwrap();
    assert_eq!(result, ECMAScriptValue::from(0));
}
fn test_getter(agent: &mut Agent, this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion<ECMAScriptValue> {
    // This is a getter; it is essentially:
    // function() { return this.result; }
    let obj = to_object(agent, this_value)?;
    let key = PropertyKey::from("result");
    get(agent, &obj, &key)
}
#[test]
fn ordinary_get_05() {
    // Normal accessor property
    let mut agent = test_agent();
    let obj = ordinary_object_create(&mut agent, None, &[]);
    let key = PropertyKey::from("a");
    let getter = create_builtin_function(&mut agent, test_getter, false, 0_f64, key.clone(), &[], None, None, Some(JSString::from("get")));
    let initial = PotentialPropertyDescriptor { get: Some(ECMAScriptValue::from(getter)), enumerable: Some(true), configurable: Some(true), ..Default::default() };
    define_property_or_throw(&mut agent, &obj, key.clone(), initial).unwrap();
    define_property_or_throw(
        &mut agent,
        &obj,
        PropertyKey::from("result"),
        PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from("sentinel value")), writable: Some(true), enumerable: Some(true), configurable: Some(true), ..Default::default() },
    )
    .unwrap();

    let result = ordinary_get(&mut agent, &obj, &key, &ECMAScriptValue::from(obj.clone())).unwrap();
    assert_eq!(result, ECMAScriptValue::from("sentinel value"));
}
#[test]
fn ordinary_get_06() {
    // Accessor property on parent (this ensures we're passing "receiver" properly)
    let mut agent = test_agent();
    let parent = ordinary_object_create(&mut agent, None, &[]);
    let key = PropertyKey::from("a");
    let getter = create_builtin_function(&mut agent, test_getter, false, 0_f64, key.clone(), &[], None, None, Some(JSString::from("get")));
    let initial = PotentialPropertyDescriptor { get: Some(ECMAScriptValue::from(getter)), enumerable: Some(true), configurable: Some(true), ..Default::default() };
    // GETTER ON PARENT
    define_property_or_throw(&mut agent, &parent, key.clone(), initial).unwrap();
    let child = ordinary_object_create(&mut agent, Some(parent), &[]);
    // RESULT VALUE ON CHILD
    define_property_or_throw(
        &mut agent,
        &child,
        PropertyKey::from("result"),
        PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from("sentinel value")), writable: Some(true), enumerable: Some(true), configurable: Some(true), ..Default::default() },
    )
    .unwrap();

    // THEREFORE:
    //    child.a == "sentinel value"
    let result = ordinary_get(&mut agent, &child, &key, &ECMAScriptValue::from(child.clone())).unwrap();
    assert_eq!(result, ECMAScriptValue::from("sentinel value"));
}
#[test]
fn ordinary_get_07() {
    // Accessor properties undefined
    let mut agent = test_agent();
    let obj = ordinary_object_create(&mut agent, None, &[]);
    let key = PropertyKey::from("a");
    let initial = PotentialPropertyDescriptor { get: Some(ECMAScriptValue::Undefined), enumerable: Some(true), configurable: Some(true), ..Default::default() };
    define_property_or_throw(&mut agent, &obj, key.clone(), initial).unwrap();

    let result = ordinary_get(&mut agent, &obj, &key, &ECMAScriptValue::from(obj.clone())).unwrap();
    assert_eq!(result, ECMAScriptValue::Undefined);
}

#[test]
fn ordinary_set_01() {
    // [[GetOwnProperty]] throws
    let mut agent = test_agent();
    let obj = TestObject::object(&mut agent, &[FunctionId::GetOwnProperty(None)]);
    let key = PropertyKey::from("a");
    let value = ECMAScriptValue::Undefined;
    let receiver = ECMAScriptValue::from(obj.clone());

    let result = ordinary_set(&mut agent, &obj, key, value, &receiver).unwrap_err();
    assert_eq!(unwind_type_error(&mut agent, result), "[[GetOwnProperty]] called on TestObject");
}
#[test]
fn ordinary_set_02() {
    // success
    let mut agent = test_agent();
    let obj = ordinary_object_create(&mut agent, None, &[]);
    let key = PropertyKey::from("a");
    let value = ECMAScriptValue::from("test sentinel");
    let receiver = ECMAScriptValue::from(obj.clone());

    let result = ordinary_set(&mut agent, &obj, key.clone(), value.clone(), &receiver).unwrap();
    assert!(result);
    let item = get(&mut agent, &obj, &key).unwrap();
    assert_eq!(item, value);
}

#[test]
fn ordinary_set_with_own_descriptor_01() {
    // [[GetPrototypeOf]] throws
    let mut agent = test_agent();
    let obj = TestObject::object(&mut agent, &[FunctionId::GetPrototypeOf]);
    let key = PropertyKey::from("a");

    let result = ordinary_set_with_own_descriptor(&mut agent, &obj, key, ECMAScriptValue::Undefined, &ECMAScriptValue::Null, None).unwrap_err();
    assert_eq!(unwind_type_error(&mut agent, result), "[[GetPrototypeOf]] called on TestObject");
}
#[test]
fn ordinary_set_with_own_descriptor_02() {
    // If ownDesc is None, call [[Set]] on the parent. (We check by having the parent throw when we call its [[Set]].)
    let mut agent = test_agent();
    let parent = TestObject::object(&mut agent, &[FunctionId::Set(None)]);
    let obj = ordinary_object_create(&mut agent, Some(parent), &[]);
    let key = PropertyKey::from("a");

    let result = ordinary_set_with_own_descriptor(&mut agent, &obj, key, ECMAScriptValue::Undefined, &ECMAScriptValue::Null, None).unwrap_err();
    assert_eq!(unwind_type_error(&mut agent, result), "[[Set]] called on TestObject");
}
#[test]
fn ordinary_set_with_own_descriptor_03() {
    // ownDesc has writable:false; function should return false.
    let mut agent = test_agent();
    let obj = ordinary_object_create(&mut agent, None, &[]);
    let own_desc = PropertyDescriptor { property: PropertyKind::Data(DataProperty { writable: false, value: ECMAScriptValue::Undefined }), enumerable: true, configurable: true, spot: 0 };
    let key = PropertyKey::from("a");
    let value = ECMAScriptValue::Undefined;
    let receiver = ECMAScriptValue::from(obj.clone());

    let result = ordinary_set_with_own_descriptor(&mut agent, &obj, key, value, &receiver, Some(own_desc)).unwrap();
    assert!(!result);
}
#[test]
fn ordinary_set_with_own_descriptor_04() {
    // Type(receiver) is not object -> return false
    let mut agent = test_agent();
    let obj = ordinary_object_create(&mut agent, None, &[]);
    let key = PropertyKey::from("a");
    let value = ECMAScriptValue::Undefined;
    let receiver = ECMAScriptValue::from(999);

    let result = ordinary_set_with_own_descriptor(&mut agent, &obj, key, value, &receiver, None).unwrap();
    assert!(!result);
}
#[test]
fn ordinary_set_with_own_descriptor_05() {
    // receiver.[[GetOwnProperty]] throws
    let mut agent = test_agent();
    let obj = TestObject::object(&mut agent, &[FunctionId::GetOwnProperty(None)]);
    let key = PropertyKey::from("a");
    let value = ECMAScriptValue::Undefined;
    let receiver = ECMAScriptValue::from(obj.clone());

    let result = ordinary_set_with_own_descriptor(&mut agent, &obj, key, value, &receiver, None).unwrap_err();
    assert_eq!(unwind_type_error(&mut agent, result), "[[GetOwnProperty]] called on TestObject");
}
#[test]
fn ordinary_set_with_own_descriptor_06() {
    // existing is an accessor
    let mut agent = test_agent();
    let obj = ordinary_object_create(&mut agent, None, &[]);
    let key = PropertyKey::from("a");
    let value = ECMAScriptValue::Undefined;
    let receiver = ECMAScriptValue::from(obj.clone());
    create_data_property(&mut agent, &obj, PropertyKey::from("result"), ECMAScriptValue::from("sentinel value")).unwrap();
    let getter = create_builtin_function(&mut agent, test_getter, false, 0_f64, key.clone(), &[], None, None, Some(JSString::from("get")));
    let accessor_prop = PotentialPropertyDescriptor { get: Some(ECMAScriptValue::from(getter)), enumerable: Some(true), configurable: Some(true), ..Default::default() };
    define_property_or_throw(&mut agent, &obj, key.clone(), accessor_prop).unwrap();
    let own_desc = PropertyDescriptor { property: PropertyKind::Data(DataProperty { writable: true, value: ECMAScriptValue::Undefined }), enumerable: true, configurable: true, spot: 0 };

    let result = ordinary_set_with_own_descriptor(&mut agent, &obj, key, value, &receiver, Some(own_desc)).unwrap();
    assert!(!result);
}
#[test]
fn ordinary_set_with_own_descriptor_07() {
    // existing is read-only
    let mut agent = test_agent();
    let obj = ordinary_object_create(&mut agent, None, &[]);
    let key = PropertyKey::from("a");
    let value = ECMAScriptValue::Undefined;
    let receiver = ECMAScriptValue::from(obj.clone());
    let readonly =
        PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from("read and weep")), writable: Some(false), enumerable: Some(true), configurable: Some(true), ..Default::default() };
    define_property_or_throw(&mut agent, &obj, key.clone(), readonly).unwrap();
    let own_desc = PropertyDescriptor { property: PropertyKind::Data(DataProperty { writable: true, value: ECMAScriptValue::Undefined }), enumerable: true, configurable: true, spot: 0 };

    let result = ordinary_set_with_own_descriptor(&mut agent, &obj, key, value, &receiver, Some(own_desc)).unwrap();
    assert!(!result);
}
#[test]
fn ordinary_set_with_own_descriptor_08() {
    // existing exists
    let mut agent = test_agent();
    let obj = ordinary_object_create(&mut agent, None, &[]);
    let key = PropertyKey::from("a");
    let value = ECMAScriptValue::Undefined;
    let receiver = ECMAScriptValue::from(obj.clone());
    let previously =
        PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from("initial")), writable: Some(true), enumerable: Some(true), configurable: Some(true), ..Default::default() };
    define_property_or_throw(&mut agent, &obj, key.clone(), previously).unwrap();
    let own_desc = PropertyDescriptor { property: PropertyKind::Data(DataProperty { writable: true, value: ECMAScriptValue::from(0) }), enumerable: true, configurable: true, spot: 0 };

    let result = ordinary_set_with_own_descriptor(&mut agent, &obj, key.clone(), value.clone(), &receiver, Some(own_desc)).unwrap();
    assert!(result);

    let item = get(&mut agent, &obj, &key).unwrap();
    assert_eq!(item, value);
}
#[test]
fn ordinary_set_with_own_descriptor_09() {
    // existing does not exist
    let mut agent = test_agent();
    let obj = ordinary_object_create(&mut agent, None, &[]);
    let key = PropertyKey::from("a");
    let value = ECMAScriptValue::from("test sentinel");
    let receiver = ECMAScriptValue::from(obj.clone());
    let own_desc = PropertyDescriptor { property: PropertyKind::Data(DataProperty { writable: true, value: ECMAScriptValue::from(0) }), enumerable: true, configurable: true, spot: 0 };

    let result = ordinary_set_with_own_descriptor(&mut agent, &obj, key.clone(), value.clone(), &receiver, Some(own_desc)).unwrap();
    assert!(result);

    let item = get(&mut agent, &obj, &key).unwrap();
    assert_eq!(item, value);
}
fn test_setter(agent: &mut Agent, this_value: ECMAScriptValue, _new_target: Option<&Object>, arguments: &[ECMAScriptValue]) -> Completion<ECMAScriptValue> {
    // This is a setter; it is essentially:
    // function(val) { this.value = val; }
    let obj = to_object(agent, this_value)?;
    let key = PropertyKey::from("result");
    let mut args = arguments.iter();
    let val = args.next().cloned().unwrap_or(ECMAScriptValue::Undefined);
    set(agent, &obj, key, val, true)?;
    Ok(ECMAScriptValue::Undefined)
}
#[test]
fn ordinary_set_with_own_descriptor_10() {
    // own_desc is an accessor descriptor, with the above setter function
    let mut agent = test_agent();
    let obj = ordinary_object_create(&mut agent, None, &[]);
    let key = PropertyKey::from("a");
    let value = ECMAScriptValue::from("test sentinel");
    let receiver = ECMAScriptValue::from(obj.clone());
    create_data_property(&mut agent, &obj, PropertyKey::from("result"), ECMAScriptValue::from("initial value")).unwrap();
    let setter = create_builtin_function(&mut agent, test_setter, false, 1_f64, key.clone(), &[], None, None, Some(JSString::from("set")));
    let accessor_prop = PropertyDescriptor {
        property: PropertyKind::Accessor(AccessorProperty { get: ECMAScriptValue::Undefined, set: ECMAScriptValue::from(setter) }),
        enumerable: true,
        configurable: true,
        spot: 0,
    };

    let result = ordinary_set_with_own_descriptor(&mut agent, &obj, key, value.clone(), &receiver, Some(accessor_prop)).unwrap();
    assert!(result);

    let item = get(&mut agent, &obj, &PropertyKey::from("result")).unwrap();
    assert_eq!(item, value);
}
#[test]
fn ordinary_set_with_own_descriptor_11() {
    // own_desc is an accessor descriptor, with a setter function that throws
    let mut agent = test_agent();
    let obj = ordinary_object_create(&mut agent, None, &[]);
    let key = PropertyKey::from("a");
    let value = ECMAScriptValue::from("test sentinel");
    let receiver = ECMAScriptValue::from(obj.clone());
    let setter = agent.intrinsic(IntrinsicId::ThrowTypeError);
    let accessor_prop = PropertyDescriptor {
        property: PropertyKind::Accessor(AccessorProperty { get: ECMAScriptValue::Undefined, set: ECMAScriptValue::from(setter) }),
        enumerable: true,
        configurable: true,
        spot: 0,
    };

    let result = ordinary_set_with_own_descriptor(&mut agent, &obj, key, value, &receiver, Some(accessor_prop)).unwrap_err();
    assert_eq!(unwind_type_error(&mut agent, result), "Generic TypeError");
}
#[test]
fn ordinary_set_with_own_descriptor_12() {
    // own_desc is an accessor descriptor, with an undefined setter function
    let mut agent = test_agent();
    let obj = ordinary_object_create(&mut agent, None, &[]);
    let key = PropertyKey::from("a");
    let value = ECMAScriptValue::from("test sentinel");
    let receiver = ECMAScriptValue::from(obj.clone());
    let accessor_prop = PropertyDescriptor {
        property: PropertyKind::Accessor(AccessorProperty { get: ECMAScriptValue::Undefined, set: ECMAScriptValue::Undefined }),
        enumerable: true,
        configurable: true,
        spot: 0,
    };

    let result = ordinary_set_with_own_descriptor(&mut agent, &obj, key, value, &receiver, Some(accessor_prop)).unwrap();
    assert!(!result);
}

#[test]
fn ordinary_delete_01() {
    // [[GetOwnProperty]] throws
    let mut agent = test_agent();
    let obj = TestObject::object(&mut agent, &[FunctionId::GetOwnProperty(None)]);
    let key = PropertyKey::from("a");

    let result = ordinary_delete(&mut agent, &obj, &key).unwrap_err();
    assert_eq!(unwind_type_error(&mut agent, result), "[[GetOwnProperty]] called on TestObject");
}
#[test]
fn ordinary_delete_02() {
    // property isn't actually there
    let mut agent = test_agent();
    let obj = ordinary_object_create(&mut agent, None, &[]);
    let key = PropertyKey::from("a");

    let result = ordinary_delete(&mut agent, &obj, &key).unwrap();
    assert!(result);
}
#[test]
fn ordinary_delete_03() {
    // property isn't configurable
    let mut agent = test_agent();
    let obj = ordinary_object_create(&mut agent, None, &[]);
    let key = PropertyKey::from("a");
    let value = ECMAScriptValue::from(0);
    define_property_or_throw(
        &mut agent,
        &obj,
        key.clone(),
        PotentialPropertyDescriptor { value: Some(value.clone()), writable: Some(false), enumerable: Some(true), configurable: Some(false), ..Default::default() },
    )
    .unwrap();

    let result = ordinary_delete(&mut agent, &obj, &key).unwrap();
    assert!(!result);
    let item = get(&mut agent, &obj, &key).unwrap();
    assert_eq!(item, value);
}
#[test]
fn ordinary_delete_04() {
    // property is normal
    let mut agent = test_agent();
    let obj = ordinary_object_create(&mut agent, None, &[]);
    let key = PropertyKey::from("a");
    let value = ECMAScriptValue::from(0);
    create_data_property(&mut agent, &obj, key.clone(), value).unwrap();

    let result = ordinary_delete(&mut agent, &obj, &key).unwrap();
    assert!(result);
    let item = get(&mut agent, &obj, &key).unwrap();
    assert_eq!(item, ECMAScriptValue::Undefined);
}

#[test]
fn ordinary_own_property_keys_01() {
    let mut agent = test_agent();
    let obj = ordinary_object_create(&mut agent, None, &[]);

    let result = ordinary_own_property_keys(&mut agent, &obj);
    assert_eq!(result, &[]);
}
use crate::values::Symbol;
#[test]
fn ordinary_own_property_keys_02() {
    let mut agent = test_agent();
    let obj = ordinary_object_create(&mut agent, None, &[]);
    let sym1 = Symbol::new(&mut agent, Some(JSString::from("TestSymbol 1")));
    let sym2 = Symbol::new(&mut agent, Some(JSString::from("TestSymbol 2")));
    create_data_property(&mut agent, &obj, PropertyKey::from(sym1.clone()), ECMAScriptValue::Null).unwrap();
    create_data_property(&mut agent, &obj, PropertyKey::from("hillbilly"), ECMAScriptValue::Null).unwrap();
    create_data_property(&mut agent, &obj, PropertyKey::from("automobile"), ECMAScriptValue::Null).unwrap();
    create_data_property(&mut agent, &obj, PropertyKey::from("888"), ECMAScriptValue::Null).unwrap();
    create_data_property(&mut agent, &obj, PropertyKey::from(sym2.clone()), ECMAScriptValue::Null).unwrap();
    create_data_property(&mut agent, &obj, PropertyKey::from("1002"), ECMAScriptValue::Null).unwrap();
    create_data_property(&mut agent, &obj, PropertyKey::from("green"), ECMAScriptValue::Null).unwrap();
    create_data_property(&mut agent, &obj, PropertyKey::from("-1"), ECMAScriptValue::Null).unwrap();
    create_data_property(&mut agent, &obj, PropertyKey::from("0"), ECMAScriptValue::Null).unwrap();

    let result = ordinary_own_property_keys(&mut agent, &obj);
    assert_eq!(
        result,
        &[
            PropertyKey::from("0"),
            PropertyKey::from("888"),
            PropertyKey::from("1002"),
            PropertyKey::from("hillbilly"),
            PropertyKey::from("automobile"),
            PropertyKey::from("green"),
            PropertyKey::from("-1"),
            PropertyKey::from(sym1),
            PropertyKey::from(sym2),
        ]
    );
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
    let mut agent = test_agent();
    array_index_key(&PropertyKey::from(Symbol::new(&mut agent, None)));
}

#[test]
fn object_interface_to_boolean_obj() {
    let mut agent = test_agent();
    let obj = ordinary_object_create(&mut agent, None, &[]);

    assert!(obj.o.to_boolean_obj().is_none());
}
#[test]
fn object_interface_to_function_obj() {
    let mut agent = test_agent();
    let obj = ordinary_object_create(&mut agent, None, &[]);

    assert!(obj.o.to_function_obj().is_none());
}
#[test]
fn object_interface_to_callable_obj() {
    let mut agent = test_agent();
    let obj = ordinary_object_create(&mut agent, None, &[]);

    assert!(obj.o.to_callable_obj().is_none());
}
#[test]
fn object_interface_to_builtin_function_obj() {
    let mut agent = test_agent();
    let obj = ordinary_object_create(&mut agent, None, &[]);

    assert!(obj.o.to_builtin_function_obj().is_none());
}
#[test]
fn object_interface_is_arguments_object() {
    let mut agent = test_agent();
    let obj = ordinary_object_create(&mut agent, None, &[]);

    assert!(!obj.o.is_arguments_object());
}
#[test]
fn object_interface_is_callable_obj() {
    let mut agent = test_agent();
    let obj = ordinary_object_create(&mut agent, None, &[]);

    assert!(!obj.o.is_callable_obj());
}
#[test]
fn object_interface_is_error_object() {
    let mut agent = test_agent();
    let obj = ordinary_object_create(&mut agent, None, &[]);

    assert!(!obj.o.is_error_object());
}
#[test]
fn object_interface_is_boolean_object() {
    let mut agent = test_agent();
    let obj = ordinary_object_create(&mut agent, None, &[]);

    assert!(!obj.o.is_boolean_object());
}
#[test]
fn object_interface_is_number_object() {
    let mut agent = test_agent();
    let obj = ordinary_object_create(&mut agent, None, &[]);

    assert!(!obj.o.is_number_object());
}
#[test]
fn object_interface_is_string_object() {
    let mut agent = test_agent();
    let obj = ordinary_object_create(&mut agent, None, &[]);

    assert!(!obj.o.is_string_object());
}
#[test]
fn object_interface_is_date_object() {
    let mut agent = test_agent();
    let obj = ordinary_object_create(&mut agent, None, &[]);

    assert!(!obj.o.is_date_object());
}
#[test]
fn object_interface_is_regexp_object() {
    let mut agent = test_agent();
    let obj = ordinary_object_create(&mut agent, None, &[]);

    assert!(!obj.o.is_regexp_object());
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
    let obj = ordinary_object_create(&mut agent, Some(proto.clone()), &[]);

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
    let mut agent = test_agent();
    let proto = ordinary_object_create(&mut agent, None, &[]);

    // Then requesting a new object with that prototype and needlessly requesting prototype or extensible slots
    let obj = ordinary_object_create(&mut agent, Some(proto.clone()), &[InternalSlotName::Prototype]);

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
    let obj = ordinary_object_create(&mut agent, Some(proto.clone()), &[InternalSlotName::Extensible]);

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
    let obj = ordinary_object_create(&mut agent, Some(proto.clone()), &[InternalSlotName::Prototype, InternalSlotName::Extensible]);

    // Gives us an empty object with its prototype slot filled.
    let data = obj.o.common_object_data().borrow();
    assert_eq!(data.prototype.as_ref(), Some(&proto));
    assert_eq!(data.extensible, true);
    assert_eq!(data.properties.len(), 0);
    assert_ne!(&obj, &proto);
}

#[test]
#[should_panic(expected = "Nonsense")]
fn make_basic_object_01() {
    let mut agent = test_agent();
    let _obj = make_basic_object(&mut agent, &[InternalSlotName::Nonsense], None);
}
#[test]
#[should_panic(expected = "Nonsense")]
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
    let obj = ordinary_object_create(&mut agent, Some(proto.clone()), &[]);
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
    let obj_b = ordinary_object_create(&mut agent, Some(obj_a.clone()), &[]);
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
    let obj_b = ordinary_object_create(&mut agent, Some(proto), &[]);
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
    let obj_b = ordinary_object_create(&mut agent, Some(proto.clone()), &[]);
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
    let obj_b = ordinary_object_create(&mut agent, Some(proto.clone()), &[]);
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
    obj.o.define_own_property(&mut agent, key.clone(), desc).unwrap();

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

    set(&mut agent, &obj, key.clone(), value, false).unwrap();
    let result = get(&mut agent, &obj, &key).unwrap();

    assert_eq!(result, ECMAScriptValue::Number(56.7));
}

mod private_element_find {
    use super::*;
    use test_case::test_case;

    fn setup() -> (Object, Vec<PrivateName>) {
        let mut agent = test_agent();
        let object_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let obj = ordinary_object_create(&mut agent, Some(object_proto), &[]);

        let name1 = PrivateName::new("name1");
        let name2 = PrivateName::new("alice");
        let name3 = PrivateName::new("charley");
        let names = vec![name1, name2, name3];

        {
            let elements = &mut obj.o.common_object_data().borrow_mut().private_elements;
            elements.push(Rc::new(PrivateElement { key: names[0].clone(), kind: PrivateElementKind::Field { value: RefCell::new(ECMAScriptValue::from(1)) } }));
            elements.push(Rc::new(PrivateElement { key: names[1].clone(), kind: PrivateElementKind::Field { value: RefCell::new(ECMAScriptValue::from(2)) } }));
            elements.push(Rc::new(PrivateElement { key: names[2].clone(), kind: PrivateElementKind::Field { value: RefCell::new(ECMAScriptValue::from(3)) } }));
        }

        (obj, names)
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
            let values_match = *value.borrow() == ECMAScriptValue::from(idx as u32 + 1);
            (keys_match, values_match)
        } else {
            panic!("Bad element kind came back")
        }
    }

    #[test_case(PrivateName::new("ice cream") => false; "not present")]
    fn missing(name: PrivateName) -> bool {
        let (obj, _) = setup();
        let result = private_element_find(&obj, &name);
        result.is_some()
    }
}

mod private_field_add {
    use super::*;
    use test_case::test_case;

    fn setup(agent: &mut Agent) -> (Object, Vec<PrivateName>) {
        let object_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let obj = ordinary_object_create(agent, Some(object_proto), &[]);

        let name1 = PrivateName::new("name1");
        let name2 = PrivateName::new("alice");
        let name3 = PrivateName::new("charley");
        let names = vec![name1, name2, name3];

        {
            let elements = &mut obj.o.common_object_data().borrow_mut().private_elements;
            elements.push(Rc::new(PrivateElement { key: names[0].clone(), kind: PrivateElementKind::Field { value: RefCell::new(ECMAScriptValue::from(1)) } }));
            elements.push(Rc::new(PrivateElement { key: names[1].clone(), kind: PrivateElementKind::Field { value: RefCell::new(ECMAScriptValue::from(2)) } }));
            elements.push(Rc::new(PrivateElement { key: names[2].clone(), kind: PrivateElementKind::Field { value: RefCell::new(ECMAScriptValue::from(3)) } }));
        }

        (obj, names)
    }

    #[test_case(PrivateName::new("orange") => (true, Some(ECMAScriptValue::Null)); "orange")]
    fn normal(name: PrivateName) -> (bool, Option<ECMAScriptValue>) {
        let mut agent = test_agent();
        let (obj, _) = setup(&mut agent);

        let result = private_field_add(&mut agent, &obj, name.clone(), ECMAScriptValue::Null);

        (
            result.is_ok(),
            private_element_find(&obj, &name).map(|pe| match &pe.kind {
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
        let mut agent = test_agent();
        let (obj, names) = setup(&mut agent);

        let result = private_field_add(&mut agent, &obj, names[idx].clone(), ECMAScriptValue::Null).unwrap_err();
        assert_eq!(unwind_type_error(&mut agent, result), "PrivateName already defined");
    }
}

mod private_method_or_accessor_add {
    use super::*;
    //use test_case::test_case;

    fn setup(agent: &mut Agent) -> (Object, PrivateName) {
        let object_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let obj = ordinary_object_create(agent, Some(object_proto), &[]);

        let name = PrivateName::new("name1");

        {
            let elements = &mut obj.o.common_object_data().borrow_mut().private_elements;
            elements.push(Rc::new(PrivateElement { key: name.clone(), kind: PrivateElementKind::Field { value: RefCell::new(ECMAScriptValue::from(1)) } }));
        }
        (obj, name)
    }

    #[test]
    fn add() {
        let mut agent = test_agent();
        let (obj, _) = setup(&mut agent);
        let key = PrivateName::new("orange");
        let method = Rc::new(PrivateElement { key: key.clone(), kind: PrivateElementKind::Method { value: ECMAScriptValue::from(100) } });

        private_method_or_accessor_add(&mut agent, &obj, method).unwrap();
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
        let mut agent = test_agent();
        let (obj, key) = setup(&mut agent);
        let method = Rc::new(PrivateElement { key, kind: PrivateElementKind::Method { value: ECMAScriptValue::from(100) } });

        let err = private_method_or_accessor_add(&mut agent, &obj, method).unwrap_err();
        assert_eq!(unwind_type_error(&mut agent, err), "PrivateName already defined");
    }
}

mod private_get {
    use super::*;
    use test_case::test_case;

    fn setup(agent: &mut Agent) -> (Object, PrivateName, PrivateName, PrivateName, PrivateName) {
        let object_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let obj = ordinary_object_create(agent, Some(object_proto), &[]);

        let field_name = PrivateName::new("field");
        let method_name = PrivateName::new("method");
        let getter_name = PrivateName::new("getter");
        let nogetter_name = PrivateName::new("nogetter");

        private_field_add(agent, &obj, field_name.clone(), ECMAScriptValue::from("FIELD")).unwrap();
        let method = PrivateElement { key: method_name.clone(), kind: PrivateElementKind::Method { value: ECMAScriptValue::from("METHOD") } };
        private_method_or_accessor_add(agent, &obj, Rc::new(method)).unwrap();
        let getter_method = create_builtin_function(agent, test_getter, false, 0_f64, PropertyKey::from("getter"), &[], None, None, Some(JSString::from("get")));
        let getter = PrivateElement { key: getter_name.clone(), kind: PrivateElementKind::Accessor { get: Some(getter_method), set: None } };
        private_method_or_accessor_add(agent, &obj, Rc::new(getter)).unwrap();
        define_property_or_throw(
            agent,
            &obj,
            PropertyKey::from("result"),
            PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from("GETTER")), writable: Some(true), enumerable: Some(true), configurable: Some(true), ..Default::default() },
        )
        .unwrap();
        let nogetter = PrivateElement { key: nogetter_name.clone(), kind: PrivateElementKind::Accessor { get: None, set: None } };
        private_method_or_accessor_add(agent, &obj, Rc::new(nogetter)).unwrap();

        (obj, field_name, method_name, getter_name, nogetter_name)
    }

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
        let mut agent = test_agent();
        let (obj, field_name, method_name, getter_name, nogetter_name) = setup(&mut agent);

        let query = match field {
            FieldName::Field => field_name,
            FieldName::Method => method_name,
            FieldName::Getter => getter_name,
            FieldName::NoGetter => nogetter_name,
            FieldName::Unavailable => PrivateName::new("unavailable"),
        };
        private_get(&mut agent, &obj, &query).map_err(|e| unwind_type_error(&mut agent, e))
    }
}

mod private_set {
    use super::*;
    use test_case::test_case;

    fn setup(agent: &mut Agent) -> (Object, PrivateName, PrivateName, PrivateName, PrivateName) {
        let object_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let obj = ordinary_object_create(agent, Some(object_proto), &[]);

        let field_name = PrivateName::new("field");
        let method_name = PrivateName::new("method");
        let setter_name = PrivateName::new("setter");
        let nosetter_name = PrivateName::new("nosetter");

        private_field_add(agent, &obj, field_name.clone(), ECMAScriptValue::from("FIELD")).unwrap();
        let method = PrivateElement { key: method_name.clone(), kind: PrivateElementKind::Method { value: ECMAScriptValue::from("METHOD") } };
        private_method_or_accessor_add(agent, &obj, Rc::new(method)).unwrap();
        let getter_method = create_builtin_function(agent, test_getter, false, 0_f64, PropertyKey::from("$state"), &[], None, None, Some(JSString::from("get")));
        let setter_method = create_builtin_function(agent, test_setter, false, 1_f64, PropertyKey::from("$state"), &[], None, None, Some(JSString::from("set")));

        let setter = PrivateElement { key: setter_name.clone(), kind: PrivateElementKind::Accessor { get: Some(getter_method), set: Some(setter_method) } };
        private_method_or_accessor_add(agent, &obj, Rc::new(setter)).unwrap();
        define_property_or_throw(
            agent,
            &obj,
            PropertyKey::from("result"),
            PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from("SETTER")), writable: Some(true), enumerable: Some(true), configurable: Some(true), ..Default::default() },
        )
        .unwrap();
        let nosetter = PrivateElement { key: nosetter_name.clone(), kind: PrivateElementKind::Accessor { get: None, set: None } };
        private_method_or_accessor_add(agent, &obj, Rc::new(nosetter)).unwrap();

        (obj, field_name, method_name, setter_name, nosetter_name)
    }
    enum FieldName {
        Field,
        Method,
        Setter,
        NoSetter,
        Unavailable,
    }

    #[test_case(FieldName::Field => Ok(ECMAScriptValue::from("NEW VALUE")); "field")]
    #[test_case(FieldName::Method => Err(String::from("PrivateName method may not be assigned")); "method")]
    #[test_case(FieldName::Setter => Ok(ECMAScriptValue::from("NEW VALUE")); "setter")]
    #[test_case(FieldName::NoSetter => Err(String::from("PrivateName has no setter")); "no-setter")]
    #[test_case(FieldName::Unavailable => Err(String::from("PrivateName not defined")); "undefined")]
    fn f(field: FieldName) -> Result<ECMAScriptValue, String> {
        let mut agent = test_agent();
        let (obj, field_name, method_name, setter_name, nosetter_name) = setup(&mut agent);
        let new_value = ECMAScriptValue::from("NEW VALUE");
        let query = match field {
            FieldName::Field => field_name,
            FieldName::Method => method_name,
            FieldName::Setter => setter_name,
            FieldName::NoSetter => nosetter_name,
            FieldName::Unavailable => PrivateName::new("unavailable"),
        };

        private_set(&mut agent, &obj, &query, new_value).map_err(|e| unwind_type_error(&mut agent, e))?;

        Ok(private_get(&mut agent, &obj, &query).unwrap())
    }
}

mod create_data_property_or_throw {
    // create_data_property_or_throw::<&str, &str>
    // create_data_property_or_throw::<&str, bool>
    // create_data_property_or_throw::<&str, i32>
    // create_data_property_or_throw::<&str, res::values::ECMAScriptValue>
    use super::*;

    mod happy {
        use super::*;
        #[test]
        fn string() {
            let mut agent = test_agent();
            let obj = ordinary_object_create(&mut agent, None, &[]);

            create_data_property_or_throw(&mut agent, &obj, "key", "blue").unwrap();

            assert_eq!(get(&mut agent, &obj, &PropertyKey::from("key")).unwrap(), ECMAScriptValue::from("blue"));
        }
        #[test]
        fn boolean() {
            let mut agent = test_agent();
            let obj = ordinary_object_create(&mut agent, None, &[]);

            create_data_property_or_throw(&mut agent, &obj, "key", true).unwrap();

            assert_eq!(get(&mut agent, &obj, &PropertyKey::from("key")).unwrap(), ECMAScriptValue::from(true));
        }
        #[test]
        fn value() {
            let mut agent = test_agent();
            let obj = ordinary_object_create(&mut agent, None, &[]);

            create_data_property_or_throw(&mut agent, &obj, "key", ECMAScriptValue::Null).unwrap();

            assert_eq!(get(&mut agent, &obj, &PropertyKey::from("key")).unwrap(), ECMAScriptValue::Null);
        }
        #[test]
        fn integer() {
            let mut agent = test_agent();
            let obj = ordinary_object_create(&mut agent, None, &[]);

            create_data_property_or_throw(&mut agent, &obj, "key", 10).unwrap();

            assert_eq!(get(&mut agent, &obj, &PropertyKey::from("key")).unwrap(), ECMAScriptValue::from(10));
        }
    }

    mod normal_error {
        use super::*;
        #[test]
        fn string() {
            let mut agent = test_agent();
            let obj = ordinary_object_create(&mut agent, None, &[]);
            obj.o.prevent_extensions(&mut agent).unwrap();

            let err = create_data_property_or_throw(&mut agent, &obj, "key", "blue").unwrap_err();

            assert_eq!(unwind_type_error(&mut agent, err), "Unable to create data property");
        }
        #[test]
        fn boolean() {
            let mut agent = test_agent();
            let obj = ordinary_object_create(&mut agent, None, &[]);
            obj.o.prevent_extensions(&mut agent).unwrap();

            let err = create_data_property_or_throw(&mut agent, &obj, "key", true).unwrap_err();

            assert_eq!(unwind_type_error(&mut agent, err), "Unable to create data property");
        }
        #[test]
        fn value() {
            let mut agent = test_agent();
            let obj = ordinary_object_create(&mut agent, None, &[]);
            obj.o.prevent_extensions(&mut agent).unwrap();

            let err = create_data_property_or_throw(&mut agent, &obj, "key", ECMAScriptValue::Null).unwrap_err();

            assert_eq!(unwind_type_error(&mut agent, err), "Unable to create data property");
        }
        #[test]
        fn integer() {
            let mut agent = test_agent();
            let obj = ordinary_object_create(&mut agent, None, &[]);
            obj.o.prevent_extensions(&mut agent).unwrap();

            let err = create_data_property_or_throw(&mut agent, &obj, "key", 10).unwrap_err();

            assert_eq!(unwind_type_error(&mut agent, err), "Unable to create data property");
        }
    }

    mod thrown_error {
        use super::*;
        #[test]
        fn string() {
            let mut agent = test_agent();
            let obj = TestObject::object(&mut agent, &[FunctionId::DefineOwnProperty(None)]);

            let err = create_data_property_or_throw(&mut agent, &obj, "key", "blue").unwrap_err();

            assert_eq!(unwind_type_error(&mut agent, err), "[[DefineOwnProperty]] called on TestObject");
        }
        #[test]
        fn boolean() {
            let mut agent = test_agent();
            let obj = TestObject::object(&mut agent, &[FunctionId::DefineOwnProperty(None)]);

            let err = create_data_property_or_throw(&mut agent, &obj, "key", true).unwrap_err();

            assert_eq!(unwind_type_error(&mut agent, err), "[[DefineOwnProperty]] called on TestObject");
        }
        #[test]
        fn value() {
            let mut agent = test_agent();
            let obj = TestObject::object(&mut agent, &[FunctionId::DefineOwnProperty(None)]);

            let err = create_data_property_or_throw(&mut agent, &obj, "key", ECMAScriptValue::Null).unwrap_err();

            assert_eq!(unwind_type_error(&mut agent, err), "[[DefineOwnProperty]] called on TestObject");
        }
        #[test]
        fn integer() {
            let mut agent = test_agent();
            let obj = TestObject::object(&mut agent, &[FunctionId::DefineOwnProperty(None)]);

            let err = create_data_property_or_throw(&mut agent, &obj, "key", 10).unwrap_err();

            assert_eq!(unwind_type_error(&mut agent, err), "[[DefineOwnProperty]] called on TestObject");
        }
    }
}

mod from_property_descriptor {
    use super::*;
    use test_case::test_case;

    fn maybeprop(agent: &mut Agent, obj: &Object, key: impl Into<PropertyKey>) -> Option<ECMAScriptValue> {
        let key = key.into();
        if has_property(agent, obj, &key).unwrap() {
            Some(get(agent, obj, &key).unwrap())
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
        enumerable: ECMAScriptValue,
        configurable: ECMAScriptValue,
    }

    #[test_case(None => None; "empty")]
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
        enumerable: ECMAScriptValue::from(true),
        configurable:ECMAScriptValue::from(true)
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
        enumerable: ECMAScriptValue::from(false),
        configurable:ECMAScriptValue::from(true)
    }); "standard accessor")]
    fn happy(pd: Option<PropertyDescriptor>) -> Option<TestResult> {
        let mut agent = test_agent();
        from_property_descriptor(&mut agent, pd).map(|o| TestResult {
            value: maybeprop(&mut agent, &o, "value"),
            writable: maybeprop(&mut agent, &o, "writable"),
            get: maybeprop(&mut agent, &o, "get"),
            set: maybeprop(&mut agent, &o, "set"),
            enumerable: maybeprop(&mut agent, &o, "enumerable").unwrap(),
            configurable: maybeprop(&mut agent, &o, "configurable").unwrap(),
        })
    }
}

mod to_property_descriptor {
    use super::*;
    use test_case::test_case;

    fn happy_data(agent: &mut Agent) -> ECMAScriptValue {
        let obj = ordinary_object_create(agent, Some(agent.intrinsic(IntrinsicId::ObjectPrototype)), &[]);
        create_data_property_or_throw(agent, &obj, "value", "blue").unwrap();
        create_data_property_or_throw(agent, &obj, "writable", true).unwrap();
        create_data_property_or_throw(agent, &obj, "enumerable", true).unwrap();
        create_data_property_or_throw(agent, &obj, "configurable", true).unwrap();
        ECMAScriptValue::from(obj)
    }
    fn fcn_data(agent: &mut Agent) -> ECMAScriptValue {
        let obj = ordinary_object_create(agent, Some(agent.intrinsic(IntrinsicId::ObjectPrototype)), &[]);
        create_data_property_or_throw(agent, &obj, "get", ECMAScriptValue::Undefined).unwrap();
        create_data_property_or_throw(agent, &obj, "set", ECMAScriptValue::Undefined).unwrap();
        create_data_property_or_throw(agent, &obj, "enumerable", true).unwrap();
        create_data_property_or_throw(agent, &obj, "configurable", true).unwrap();
        ECMAScriptValue::from(obj)
    }

    fn faux_errors(agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion<ECMAScriptValue> {
        Err(create_type_error(agent, "Test Sentinel"))
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
    fn happy(create_input: fn(&mut Agent) -> ECMAScriptValue) -> PotentialPropertyDescriptor {
        let mut agent = test_agent();
        let input = create_input(&mut agent);
        let result = to_property_descriptor(&mut agent, &input);
        result.unwrap()
    }

    fn create_hasprop_error(agent: &mut Agent, name: &str) -> ECMAScriptValue {
        ECMAScriptValue::from(TestObject::object(agent, &[FunctionId::GetOwnProperty(Some(PropertyKey::from(name)))]))
    }
    fn create_getter_error(agent: &mut Agent, name: &str) -> ECMAScriptValue {
        let realm = agent.current_realm_record().unwrap();
        let object_prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let obj = ordinary_object_create(agent, Some(object_prototype), &[]);
        let function_proto = agent.intrinsic(IntrinsicId::FunctionPrototype);
        let key = PropertyKey::from(name);
        let getter = create_builtin_function(agent, faux_errors, false, 0_f64, key.clone(), BUILTIN_FUNCTION_SLOTS, Some(realm), Some(function_proto), Some(JSString::from("get")));
        let desc = PotentialPropertyDescriptor { enumerable: Some(true), configurable: Some(true), get: Some(ECMAScriptValue::from(getter)), ..Default::default() };
        define_property_or_throw(agent, &obj, key, desc).unwrap();
        ECMAScriptValue::from(obj)
    }
    fn create_nonfcn(agent: &mut Agent, name: &str) -> ECMAScriptValue {
        let obj = ordinary_object_create(agent, Some(agent.intrinsic(IntrinsicId::ObjectPrototype)), &[]);
        create_data_property_or_throw(agent, &obj, name, name).unwrap();
        ECMAScriptValue::from(obj)
    }

    #[test_case(|_| ECMAScriptValue::Undefined => "Must be an object"; "non-object")]
    #[test_case(|a| create_hasprop_error(a, "enumerable") => "[[GetOwnProperty]] called on TestObject"; "enumerable has_property throws")]
    #[test_case(|a| create_hasprop_error(a, "configurable") => "[[GetOwnProperty]] called on TestObject"; "configurable has_property throws")]
    #[test_case(|a| create_hasprop_error(a, "value") => "[[GetOwnProperty]] called on TestObject"; "value has_property throws")]
    #[test_case(|a| create_hasprop_error(a, "writable") => "[[GetOwnProperty]] called on TestObject"; "writable has_property throws")]
    #[test_case(|a| create_hasprop_error(a, "get") => "[[GetOwnProperty]] called on TestObject"; "get has_property throws")]
    #[test_case(|a| create_hasprop_error(a, "set") => "[[GetOwnProperty]] called on TestObject"; "set has_property throws")]
    #[test_case(|a| create_getter_error(a, "enumerable") => "Test Sentinel"; "enumerable getter throws")]
    #[test_case(|a| create_getter_error(a, "configurable") => "Test Sentinel"; "configurable getter throws")]
    #[test_case(|a| create_getter_error(a, "value") => "Test Sentinel"; "value getter throws")]
    #[test_case(|a| create_getter_error(a, "writable") => "Test Sentinel"; "writable getter throws")]
    #[test_case(|a| create_getter_error(a, "get") => "Test Sentinel"; "get getter throws")]
    #[test_case(|a| create_getter_error(a, "set") => "Test Sentinel"; "set getter throws")]
    #[test_case(|a| create_nonfcn(a, "get") => "Getter must be callable (or undefined)"; "uncallable getter")]
    #[test_case(|a| create_nonfcn(a, "set") => "Setter must be callable (or undefined)"; "uncallable setter")]
    fn error(create_input: fn(&mut Agent) -> ECMAScriptValue) -> String {
        let mut agent = test_agent();
        let input = create_input(&mut agent);
        let result = to_property_descriptor(&mut agent, &input);
        unwind_type_error(&mut agent, result.unwrap_err())
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
        let mut agent = test_agent();
        create_array_from_list(&mut agent, items).o.common_object_data().borrow().propdump()
    }
}

mod enumerable_own_property_names {
    use super::*;
    use test_case::test_case;

    fn dead(agent: &mut Agent) -> Object {
        DeadObject::object(agent)
    }
    fn normal(agent: &mut Agent) -> Object {
        let object_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let obj = ordinary_object_create(agent, Some(object_proto), &[]);
        create_data_property_or_throw(agent, &obj, "one", 1.0).unwrap();
        create_data_property_or_throw(agent, &obj, "three", 3.0).unwrap();
        let sym = Symbol::new(agent, Some("two".into()));
        create_data_property_or_throw(agent, &obj, sym, 2.0).unwrap();
        define_property_or_throw(
            agent,
            &obj,
            "hidden",
            PotentialPropertyDescriptor { value: Some("hidden".into()), writable: Some(true), enumerable: Some(false), configurable: Some(true), ..Default::default() },
        )
        .unwrap();
        obj
    }
    fn gop_override(agent: &mut Agent, this: &AdaptableObject, key: &PropertyKey) -> Completion<Option<PropertyDescriptor>> {
        if this.something.get() == 0 {
            this.something.set(1);
            Ok(ordinary_get_own_property(this, key))
        } else {
            Err(create_type_error(agent, "[[GetOwnProperty]] called more than once"))
        }
    }
    fn ownprop(agent: &mut Agent) -> Object {
        let obj = AdaptableObject::object(agent, AdaptableMethods { get_own_property_override: Some(gop_override), ..Default::default() });
        create_data_property_or_throw(agent, &obj, "one", 1.0).unwrap();
        obj
    }
    fn getthrows(agent: &mut Agent) -> Object {
        let obj = TestObject::object(agent, &[FunctionId::Get(None)]);
        create_data_property_or_throw(agent, &obj, "one", 1.0).unwrap();
        obj
    }
    fn lying_ownprops(_: &mut Agent, _: &AdaptableObject) -> Completion<Vec<PropertyKey>> {
        Ok(vec!["one".into(), "two".into(), "three".into()])
    }
    fn lyingkeys(agent: &mut Agent) -> Object {
        AdaptableObject::object(agent, AdaptableMethods { own_property_keys_override: Some(lying_ownprops), ..Default::default() })
    }

    #[test_case(dead, EnumerationStyle::Key => Err("TypeError: own_property_keys called on DeadObject".to_string()); "own_property_keys throws")]
    #[test_case(normal, EnumerationStyle::Key => Ok(vec!["one".into(), "three".into()]); "keys: normal object")]
    #[test_case(normal, EnumerationStyle::Value => Ok(vec![1.0.into(), 3.0.into()]); "values: normal object")]
    #[test_case(ownprop, EnumerationStyle::Value => Err("TypeError: [[GetOwnProperty]] called more than once".to_string()); "GetOwnProperty throws")]
    #[test_case(getthrows, EnumerationStyle::Value => Err("TypeError: [[Get]] called on TestObject".to_string()); "get throws")]
    #[test_case(lyingkeys, EnumerationStyle::Value => Ok(Vec::<ECMAScriptValue>::new()); "ownkeys lies")]
    fn f(make_obj: fn(&mut Agent) -> Object, kind: EnumerationStyle) -> Result<Vec<ECMAScriptValue>, String> {
        let mut agent = test_agent();
        let obj = make_obj(&mut agent);
        enumerable_own_property_names(&mut agent, &obj, kind).map_err(|err| unwind_any_error(&mut agent, err))
    }

    #[test]
    fn keyvalue() {
        let mut agent = test_agent();
        let obj = normal(&mut agent);
        let result = enumerable_own_property_names(&mut agent, &obj, EnumerationStyle::KeyPlusValue).unwrap();
        assert_eq!(result.len(), 2);
        assert_eq!(getv(&mut agent, &result[0], &"0".into()).unwrap(), "one".into());
        assert_eq!(getv(&mut agent, &result[0], &"1".into()).unwrap(), 1.0.into());
        assert_eq!(getv(&mut agent, &result[0], &"length".into()).unwrap(), 2.0.into());
        assert_eq!(getv(&mut agent, &result[1], &"0".into()).unwrap(), "three".into());
        assert_eq!(getv(&mut agent, &result[1], &"1".into()).unwrap(), 3.0.into());
        assert_eq!(getv(&mut agent, &result[1], &"length".into()).unwrap(), 2.0.into());
    }
}

mod set_integrity_level {
    use super::*;
    use test_case::test_case;

    fn normal(agent: &mut Agent) -> Object {
        let proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let obj = ordinary_object_create(agent, Some(proto), &[]);
        create_data_property_or_throw(agent, &obj, "property", 67).unwrap();
        define_property_or_throw(
            agent,
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
    fn dead(agent: &mut Agent) -> Object {
        DeadObject::object(agent)
    }
    fn prevention_disabled(agent: &mut Agent) -> Object {
        AdaptableObject::object(agent, AdaptableMethods { prevent_extensions_override: Some(|_, _| Ok(false)), ..Default::default() })
    }
    fn opk_throws(agent: &mut Agent) -> Object {
        TestObject::object(agent, &[FunctionId::OwnPropertyKeys])
    }
    fn dop_throws(agent: &mut Agent) -> Object {
        let obj = AdaptableObject::object(
            agent,
            AdaptableMethods {
                define_own_property_override: Some(|agent, this, key, desc| {
                    if this.something.get() == 0 {
                        this.something.set(1);
                        ordinary_define_own_property(agent, this, key, desc)
                    } else {
                        Err(create_type_error(agent, "Test Sentinel"))
                    }
                }),
                ..Default::default()
            },
        );
        create_data_property_or_throw(agent, &obj, "property", 99.0).unwrap();
        obj
    }
    fn gop_override(agent: &mut Agent, this: &AdaptableObject, key: &PropertyKey) -> Completion<Option<PropertyDescriptor>> {
        if this.something.get() == 0 {
            this.something.set(1);
            Ok(ordinary_get_own_property(this, key))
        } else {
            Err(create_type_error(agent, "[[GetOwnProperty]] called more than once"))
        }
    }
    fn gop_throws(agent: &mut Agent) -> Object {
        let obj = AdaptableObject::object(agent, AdaptableMethods { get_own_property_override: Some(gop_override), ..Default::default() });
        create_data_property_or_throw(agent, &obj, "one", 1.0).unwrap();
        obj
    }
    fn lying_ownprops(_: &mut Agent, _: &AdaptableObject) -> Completion<Vec<PropertyKey>> {
        Ok(vec!["one".into(), "two".into(), "three".into()])
    }
    fn lyingkeys(agent: &mut Agent) -> Object {
        AdaptableObject::object(agent, AdaptableMethods { own_property_keys_override: Some(lying_ownprops), ..Default::default() })
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
    fn sil(make_obj: fn(&mut Agent) -> Object, level: IntegrityLevel) -> Result<(bool, Vec<PropertyInfo>), String> {
        let mut agent = test_agent();
        let obj = make_obj(&mut agent);
        set_integrity_level(&mut agent, &obj, level).map(|success| (success, obj.o.common_object_data().borrow().propdump())).map_err(|err| unwind_any_error(&mut agent, err))
    }
}
