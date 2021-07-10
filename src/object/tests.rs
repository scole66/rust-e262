use super::*;
use crate::strings::JSString;
use crate::tests::test_agent;

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
    let p2 = PropertyDescriptor { property: PropertyKind::Data(DataProperty { value: ECMAScriptValue::from(true), writable: false }), enumerable: false, configurable: false, spot: 10 };
    let c2 = ConcisePropertyDescriptor::from(&p2);
    assert_eq!(format!("{:?}", c2), "{ true --- }");
    let p3 = PropertyDescriptor {
        property: PropertyKind::Accessor(AccessorProperty { get: ECMAScriptValue::from(true), set: ECMAScriptValue::Undefined }),
        enumerable: false,
        configurable: true,
        spot: 10,
    };
    let c3 = ConcisePropertyDescriptor::from(&p3);
    assert_eq!(format!("{:?}", c3), "{ [[Get]]: true [[Set]]: undefined -c }");
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
    let result = obj_b.o.set_prototype_of(&mut agent, Some(&obj_a));
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
    let result = obj_b.o.set_prototype_of(&mut agent, Some(&new_proto));
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
    let result = proto.o.set_prototype_of(&mut agent, Some(&obj_b));
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
    let result = obj_b.o.set_prototype_of(&mut agent, Some(&new_proto));
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
