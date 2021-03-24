use super::*;
use crate::strings::JSString;

#[test]
fn ordinary_object_create_01() {
    // When: An agent is given
    let mut agent = Agent::new();

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
    let mut agent = Agent::new();
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
    let mut agent = Agent::new();
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
    let mut agent = Agent::new();
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
    let mut agent = Agent::new();
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
    let mut agent = Agent::new();
    let obj = make_basic_object(&mut agent, &[InternalSlotName::Nonsense]);
}
#[test]
#[should_panic]
fn make_basic_object_02() {
    let mut agent = Agent::new();
    let obj = make_basic_object(&mut agent, &[InternalSlotName::Nonsense, InternalSlotName::Prototype, InternalSlotName::Extensible]);
}

#[test]
fn get_prototype_of_01() {
    let obj = ordinary_object_create(&mut Agent::new(), None, &[]);
    let result = obj.o.get_prototype_of();
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), None);
}
#[test]
fn get_prototype_of_02() {
    let mut agent = Agent::new();
    let proto = ordinary_object_create(&mut agent, None, &[]);
    let obj = ordinary_object_create(&mut agent, Some(&proto), &[]);
    let result = obj.o.get_prototype_of();
    assert!(result.is_ok());
    assert_eq!(result.unwrap().as_ref(), Some(&proto));
}

#[test]
fn set_prototype_of_01() {
    // Not changing an empty prototype
    let mut agent = Agent::new();
    let obj_a = ordinary_object_create(&mut agent, None, &[]);
    let result = obj_a.o.set_prototype_of(None);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), true);
    assert_eq!(obj_a.o.common_object_data().borrow().prototype, None);
}
#[test]
fn set_prototype_of_02() {
    // Not changing a Some() prototype
    let mut agent = Agent::new();
    let obj_a = ordinary_object_create(&mut agent, None, &[]);
    let obj_b = ordinary_object_create(&mut agent, Some(&obj_a), &[]);
    let result = obj_b.o.set_prototype_of(Some(&obj_a));
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), true);
    assert_eq!(obj_b.o.common_object_data().borrow().prototype.as_ref(), Some(&obj_a));
}
#[test]
fn set_prototype_of_03() {
    // Changing a Some() prototype to a different Some() prototype
    let mut agent = Agent::new();
    let proto = ordinary_object_create(&mut agent, None, &[]);
    let obj_b = ordinary_object_create(&mut agent, Some(&proto), &[]);
    let new_proto = ordinary_object_create(&mut agent, None, &[]);
    let result = obj_b.o.set_prototype_of(Some(&new_proto));
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), true);
    assert_eq!(obj_b.o.common_object_data().borrow().prototype.as_ref(), Some(&new_proto));
}
#[test]
fn set_prototype_of_04() {
    // Trying to make a prototype loop
    let mut agent = Agent::new();
    let proto = ordinary_object_create(&mut agent, None, &[]);
    let obj_b = ordinary_object_create(&mut agent, Some(&proto), &[]);
    let result = proto.o.set_prototype_of(Some(&obj_b));
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), false);
    assert_eq!(proto.o.common_object_data().borrow().prototype.as_ref(), None);
}
#[test]
fn set_prototype_of_05() {
    // Changing the prototype of an object that's not extensible
    let mut agent = Agent::new();
    let proto = ordinary_object_create(&mut agent, None, &[]);
    let obj_b = ordinary_object_create(&mut agent, Some(&proto), &[]);
    obj_b.o.common_object_data().borrow_mut().extensible = false;
    let new_proto = ordinary_object_create(&mut agent, None, &[]);
    let result = obj_b.o.set_prototype_of(Some(&new_proto));
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), false);
    assert_eq!(obj_b.o.common_object_data().borrow().prototype.as_ref(), Some(&proto));
}

#[test]
fn is_extensible_01() {
    let obj = ordinary_object_create(&mut Agent::new(), None, &[]);
    let result = obj.o.is_extensible();
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), true);
}
#[test]
fn is_extensible_02() {
    let obj = ordinary_object_create(&mut Agent::new(), None, &[]);
    obj.o.common_object_data().borrow_mut().extensible = false;
    let result = obj.o.is_extensible();
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), false);
}

#[test]
fn prevent_extensions_01() {
    let obj = ordinary_object_create(&mut Agent::new(), None, &[]);
    let result = obj.o.prevent_extensions();
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), true);
    assert_eq!(obj.o.common_object_data().borrow().extensible, false);
}

#[test]
fn get_own_property_01() {
    let obj = ordinary_object_create(&mut Agent::new(), None, &[]);
    let key = PropertyKey::String(JSString::from("blue"));
    let result = obj.o.get_own_property(&key);
    assert!(result.is_ok());
    assert!(result.unwrap().is_none());
}
#[test]
fn get_own_property_02() {
    let obj = ordinary_object_create(&mut Agent::new(), None, &[]);
    let key = PropertyKey::String(JSString::from("blue"));
    let value = ECMAScriptValue::Number(89.0);
    let desc = PotentialPropertyDescriptor { value: Some(value), writable: Some(false), enumerable: Some(true), configurable: None, get: None, set: None };
    obj.o.define_own_property(&key, &desc).expect("oops");

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
    let mut agent = Agent::new();

    let obj = ordinary_object_create(&mut agent, None, &[]);
    let key = PropertyKey::String(JSString::from("blue"));
    let value = ECMAScriptValue::Number(56.7);

    set(&mut agent, &obj, &key, &value, false).unwrap();
    let result = get(&mut agent, &obj, &key).unwrap();

    assert_eq!(result, ECMAScriptValue::Number(56.7));
}
