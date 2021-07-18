use super::*;
use crate::object::{call, construct, get, AccessorProperty, PropertyKind};
use crate::realm::IntrinsicId;
use crate::tests::{test_agent, unwind_type_error};
use crate::values::Symbol;
use num::BigInt;

#[test]
fn number_object_debug() {
    let mut agent = test_agent();
    let no = NumberObject { common: RefCell::new(CommonObjectData::new(&mut agent, None, false, &NUMBER_OBJECT_SLOTS)), number_data: RefCell::new(0.0) };

    assert_ne!(format!("{:?}", no), "");
}

#[test]
#[allow(clippy::float_cmp)]
fn number_object_object() {
    let mut agent = test_agent();
    let number_prototype = agent.intrinsic(IntrinsicId::NumberPrototype);
    let no = NumberObject::object(&mut agent, Some(number_prototype.clone()));

    assert_eq!(no.o.common_object_data().borrow().prototype, Some(number_prototype));
    assert_eq!(*no.o.to_number_obj().unwrap().number_data().borrow(), 0.0);
}

#[test]
#[allow(clippy::float_cmp)]
fn create_number_object_01() {
    let mut agent = test_agent();
    let no = create_number_object(&mut agent, 100.0);

    let number_prototype = agent.intrinsic(IntrinsicId::NumberPrototype);
    assert_eq!(no.o.get_prototype_of(&mut agent).unwrap(), Some(number_prototype));
    assert_eq!(*no.o.to_number_obj().unwrap().number_data().borrow(), 100.0);
}

#[test]
fn number_object_common_object_data() {
    let mut agent = test_agent();
    let no = create_number_object(&mut agent, 100.0);
    let number_prototype = agent.intrinsic(IntrinsicId::NumberPrototype);

    let cod = no.o.common_object_data();

    assert!(cod.borrow().properties.is_empty());
    assert_eq!(cod.borrow().prototype, Some(number_prototype));
    assert!(cod.borrow().extensible);
    assert_eq!(cod.borrow().next_spot, 0);
    assert!(cod.borrow().slots.contains(&InternalSlotName::NumberData));
}
#[test]
fn number_object_is_ordinary() {
    let mut agent = test_agent();
    let no = create_number_object(&mut agent, 100.0);

    let result = no.o.is_ordinary();

    assert!(result);
}
#[test]
fn number_object_id() {
    let mut agent = test_agent();
    let no = create_number_object(&mut agent, 100.0);

    // ... essentially, assert that it doesn't panic.
    no.o.id();
}
#[test]
fn number_object_to_number_object() {
    let mut agent = test_agent();
    let no = create_number_object(&mut agent, 100.0);

    let result = no.o.to_number_obj();
    assert!(result.is_some());
}
#[test]
fn number_object_is_number_object() {
    let mut agent = test_agent();
    let no = create_number_object(&mut agent, 100.0);

    let result = no.o.is_number_object();

    assert!(result);
}
#[test]
fn number_object_get_prototype_of() {
    let mut agent = test_agent();
    let no = create_number_object(&mut agent, 100.0);

    let result = no.o.get_prototype_of(&mut agent).unwrap();
    assert!(result.is_some());
}
#[test]
fn number_object_set_prototype_of() {
    let mut agent = test_agent();
    let no = create_number_object(&mut agent, 100.0);

    let result = no.o.set_prototype_of(&mut agent, None).unwrap();
    assert!(result);
}
#[test]
fn number_object_is_extensible() {
    let mut agent = test_agent();
    let no = create_number_object(&mut agent, 100.0);

    let result = no.o.is_extensible(&mut agent).unwrap();
    assert!(result);
}
#[test]
fn number_object_prevent_extensions() {
    let mut agent = test_agent();
    let no = create_number_object(&mut agent, 100.0);

    let result = no.o.prevent_extensions(&mut agent).unwrap();
    assert!(result);
}
#[test]
fn number_object_get_own_property() {
    let mut agent = test_agent();
    let no = create_number_object(&mut agent, 100.0);

    let result = no.o.get_own_property(&mut agent, &PropertyKey::from("a")).unwrap();
    assert!(result.is_none());
}
#[test]
fn number_object_define_own_property() {
    let mut agent = test_agent();
    let no = create_number_object(&mut agent, 100.0);

    let result = no.o.define_own_property(&mut agent, PropertyKey::from("a"), PotentialPropertyDescriptor { value: Some(ECMAScriptValue::Undefined), ..Default::default() }).unwrap();
    assert!(result);
}
#[test]
fn number_object_has_property() {
    let mut agent = test_agent();
    let no = create_number_object(&mut agent, 100.0);

    let result = no.o.has_property(&mut agent, &PropertyKey::from("a")).unwrap();
    assert!(!result);
}
#[test]
fn number_object_get() {
    let mut agent = test_agent();
    let no = create_number_object(&mut agent, 100.0);

    let result = no.o.get(&mut agent, &PropertyKey::from("a"), &ECMAScriptValue::from(no.clone())).unwrap();
    assert_eq!(result, ECMAScriptValue::Undefined);
}
#[test]
fn number_object_set() {
    let mut agent = test_agent();
    let no = create_number_object(&mut agent, 100.0);

    let result = no.o.set(&mut agent, PropertyKey::from("a"), ECMAScriptValue::from(88.0), &ECMAScriptValue::from(no.clone())).unwrap();
    assert!(result);
}
#[test]
fn number_object_delete() {
    let mut agent = test_agent();
    let no = create_number_object(&mut agent, 100.0);

    let result = no.o.delete(&mut agent, &PropertyKey::from("a")).unwrap();
    assert!(result);
}
#[test]
fn number_object_own_property_keys() {
    let mut agent = test_agent();
    let no = create_number_object(&mut agent, 100.0);

    let result = no.o.own_property_keys(&mut agent).unwrap();
    assert_eq!(result, &[]);
}
#[test]
fn number_object_other_automatic_functions() {
    let mut agent = test_agent();
    let no = create_number_object(&mut agent, 100.0);

    assert!(!no.o.is_error_object());
    assert!(no.o.to_function_obj().is_none());
    assert!(!no.o.is_boolean_object());
    assert!(!no.o.is_string_object());
    assert!(!no.o.is_regexp_object());
    assert!(no.o.to_builtin_function_obj().is_none());
    assert!(!no.o.is_callable_obj());
    assert!(no.o.to_boolean_obj().is_none());
    assert!(no.o.to_error_obj().is_none());
    assert!(no.o.to_callable_obj().is_none());
    assert!(no.o.to_constructable().is_none());
    assert!(!no.o.is_arguments_object());
    assert!(!no.o.is_date_object());
}

#[test]
fn number_constructor_data_props() {
    let mut agent = test_agent();
    let number_constructor = agent.intrinsic(IntrinsicId::Number);

    let val = get(&mut agent, &number_constructor, &PropertyKey::from("EPSILON")).unwrap();
    assert_eq!(val, ECMAScriptValue::from(f64::EPSILON));

    let val = get(&mut agent, &number_constructor, &PropertyKey::from("MAX_SAFE_INTEGER")).unwrap();
    assert_eq!(val, ECMAScriptValue::from(9007199254740991.0));

    let val = get(&mut agent, &number_constructor, &PropertyKey::from("MAX_VALUE")).unwrap();
    assert_eq!(val, ECMAScriptValue::from(f64::MAX));

    let val = get(&mut agent, &number_constructor, &PropertyKey::from("MIN_SAFE_INTEGER")).unwrap();
    assert_eq!(val, ECMAScriptValue::from(-9007199254740991.0));

    let val = get(&mut agent, &number_constructor, &PropertyKey::from("MIN_VALUE")).unwrap();
    assert_eq!(val, ECMAScriptValue::from(5e-324));

    let val = get(&mut agent, &number_constructor, &PropertyKey::from("NaN")).unwrap();
    assert!(matches!(val, ECMAScriptValue::Number(_)));
    if let ECMAScriptValue::Number(n) = val {
        assert!(n.is_nan());
    }

    let val = get(&mut agent, &number_constructor, &PropertyKey::from("NEGATIVE_INFINITY")).unwrap();
    assert_eq!(val, ECMAScriptValue::from(f64::NEG_INFINITY));

    let val = get(&mut agent, &number_constructor, &PropertyKey::from("POSITIVE_INFINITY")).unwrap();
    assert_eq!(val, ECMAScriptValue::from(f64::INFINITY));

    let val = get(&mut agent, &number_constructor, &PropertyKey::from("prototype")).unwrap();
    let number_prototype = agent.intrinsic(IntrinsicId::NumberPrototype);
    assert_eq!(val, ECMAScriptValue::from(number_prototype));
}

#[test]
fn number_constructor_called_as_function_01() {
    // No arguments passed:
    //   > Number()
    //   0
    let mut agent = test_agent();
    let number_constructor = ECMAScriptValue::from(agent.intrinsic(IntrinsicId::Number));

    let result = call(&mut agent, &number_constructor, &ECMAScriptValue::Undefined, &[]).unwrap();
    assert_eq!(result, ECMAScriptValue::from(0));
}
#[test]
fn number_constructor_called_as_function_02() {
    // Argument with a "Number" result from ToNumeric.
    //   > Number(true)
    //   1
    let mut agent = test_agent();
    let number_constructor = ECMAScriptValue::from(agent.intrinsic(IntrinsicId::Number));

    let result = call(&mut agent, &number_constructor, &ECMAScriptValue::Undefined, &[ECMAScriptValue::from(true)]).unwrap();
    assert_eq!(result, ECMAScriptValue::from(1));
}
#[test]
fn number_constructor_called_as_function_03() {
    // Argument with a "BigInt" result from ToNumeric.
    //   > Number(10n)
    //   10
    let mut agent = test_agent();
    let number_constructor = ECMAScriptValue::from(agent.intrinsic(IntrinsicId::Number));

    let result = call(&mut agent, &number_constructor, &ECMAScriptValue::Undefined, &[ECMAScriptValue::from(BigInt::from(10))]).unwrap();
    assert_eq!(result, ECMAScriptValue::from(10));
}
#[test]
fn number_constructor_called_as_function_04() {
    // Argument that cannot be converted to a number
    //   > Number(Symbol())
    //   Uncaught TypeError: Cannot convert a Symbol value to a number
    //       at Number (<anonymous>)
    let mut agent = test_agent();
    let number_constructor = ECMAScriptValue::from(agent.intrinsic(IntrinsicId::Number));

    let sym = Symbol::new(&mut agent, None);
    let result = call(&mut agent, &number_constructor, &ECMAScriptValue::Undefined, &[ECMAScriptValue::from(sym)]).unwrap_err();
    assert_eq!(unwind_type_error(&mut agent, result), "Symbol values cannot be converted to Number values");
}

#[test]
#[allow(clippy::float_cmp)]
fn number_constructor_as_constructor_01() {
    // No arguments:
    //   > new Number()
    //   [Number: 0]
    let mut agent = test_agent();
    let number_constructor = agent.intrinsic(IntrinsicId::Number);

    let result = construct(&mut agent, &number_constructor, &[], None).unwrap();

    assert!(result.is_object());
    if let ECMAScriptValue::Object(o) = result {
        assert!(o.o.is_number_object());
        let data = *o.o.to_number_obj().unwrap().number_data().borrow();
        assert_eq!(data, 0.0);
    }
}
#[test]
#[allow(clippy::float_cmp)]
fn number_constructor_as_constructor_02() {
    // Argument needing conversion:
    //   > new Number("0xbadfade")
    //   [Number: 195951326]
    let mut agent = test_agent();
    let number_constructor = agent.intrinsic(IntrinsicId::Number);
    let arg = ECMAScriptValue::from("0xbadfade");

    let result = construct(&mut agent, &number_constructor, &[arg], None).unwrap();

    assert!(result.is_object());
    if let ECMAScriptValue::Object(o) = result {
        assert!(o.o.is_number_object());
        let data = *o.o.to_number_obj().unwrap().number_data().borrow();
        assert_eq!(data, 195951326.0);
    }
}
#[test]
fn number_constructor_throws() {
    // ordinary_create_from_contructor throws.
    // This looks to be difficult to make happen, but I can imagine some class shenanigans that could do it.
    let mut agent = test_agent();
    let number_constructor = agent.intrinsic(IntrinsicId::Number);

    // This hack is to get around the "not configurable" characteristic of Number.prototype.
    // (It replaces Number.prototype (a data property) with an accessor property that throws when "prototype" is gotten.)
    let new_prop = PropertyKind::Accessor(AccessorProperty { get: ECMAScriptValue::from(agent.intrinsic(IntrinsicId::ThrowTypeError)), set: ECMAScriptValue::Undefined });
    {
        let mut cod = number_constructor.o.common_object_data().borrow_mut();
        let mut prop = cod.properties.get_mut(&PropertyKey::from("prototype")).unwrap();
        prop.property = new_prop;
    }

    let result = construct(&mut agent, &number_constructor, &[], None).unwrap_err();
    assert_eq!(unwind_type_error(&mut agent, result), "Generic TypeError");
}

#[test]
fn number_is_finite_no_args() {
    // no args
    //    > Number.isFinite()
    //    false
    let mut agent = test_agent();
    let number_constructor = agent.intrinsic(IntrinsicId::Number);
    let is_finite = get(&mut agent, &number_constructor, &PropertyKey::from("isFinite")).unwrap();
    let this_value = ECMAScriptValue::from(number_constructor.clone());

    let result = call(&mut agent, &is_finite, &this_value, &[]).unwrap();
    assert_eq!(result, ECMAScriptValue::from(false));
}
#[test]
fn number_is_finite_one_arg() {
    let mut agent = test_agent();
    let number_constructor = agent.intrinsic(IntrinsicId::Number);
    let is_finite = get(&mut agent, &number_constructor, &PropertyKey::from("isFinite")).unwrap();
    let this_value = ECMAScriptValue::from(number_constructor.clone());

    for (arg, expected) in [(f64::INFINITY, false), (f64::NAN, false), (f64::NEG_INFINITY, false), (0.0, true), (-0.0, true), (89.3, true), (-89.3, true)] {
        let result = call(&mut agent, &is_finite, &this_value, &[ECMAScriptValue::from(arg)]).unwrap();
        assert_eq!(result, ECMAScriptValue::from(expected));
    }

    let result = call(&mut agent, &is_finite, &this_value, &[ECMAScriptValue::from("blue")]).unwrap();
    assert_eq!(result, ECMAScriptValue::from(false));
}

#[test]
fn number_is_integer_no_args() {
    let mut agent = test_agent();
    let number_constructor = agent.intrinsic(IntrinsicId::Number);
    let is_integer = get(&mut agent, &number_constructor, &PropertyKey::from("isInteger")).unwrap();
    let this_value = ECMAScriptValue::from(number_constructor.clone());

    let result = call(&mut agent, &is_integer, &this_value, &[]).unwrap();
    assert_eq!(result, ECMAScriptValue::from(false));
}
#[test]
fn number_is_integer_one_arg() {
    let mut agent = test_agent();
    let number_constructor = agent.intrinsic(IntrinsicId::Number);
    let is_integer = get(&mut agent, &number_constructor, &PropertyKey::from("isInteger")).unwrap();
    let this_value = ECMAScriptValue::from(number_constructor.clone());

    for (arg, expected) in [(f64::INFINITY, false), (f64::NAN, false), (f64::NEG_INFINITY, false), (0.0, true), (-0.0, true), (89.3, false), (-89.3, false), (10.0, true), (3.33e200, true)] {
        let result = call(&mut agent, &is_integer, &this_value, &[ECMAScriptValue::from(arg)]).unwrap();
        assert_eq!(result, ECMAScriptValue::from(expected));
    }

    let result = call(&mut agent, &is_integer, &this_value, &[ECMAScriptValue::from("blue")]).unwrap();
    assert_eq!(result, ECMAScriptValue::from(false));
}

#[test]
fn number_is_nan_no_args() {
    let mut agent = test_agent();
    let number_constructor = agent.intrinsic(IntrinsicId::Number);
    let is_nan = get(&mut agent, &number_constructor, &PropertyKey::from("isNaN")).unwrap();
    let this_value = ECMAScriptValue::from(number_constructor.clone());

    let result = call(&mut agent, &is_nan, &this_value, &[]).unwrap();
    assert_eq!(result, ECMAScriptValue::from(false));
}
#[test]
fn number_is_nan_one_arg() {
    let mut agent = test_agent();
    let number_constructor = agent.intrinsic(IntrinsicId::Number);
    let is_nan = get(&mut agent, &number_constructor, &PropertyKey::from("isNaN")).unwrap();
    let this_value = ECMAScriptValue::from(number_constructor.clone());

    for (arg, expected) in [(f64::INFINITY, false), (f64::NAN, true), (f64::NEG_INFINITY, false), (0.0, false), (-0.0, false), (89.3, false)] {
        let result = call(&mut agent, &is_nan, &this_value, &[ECMAScriptValue::from(arg)]).unwrap();
        assert_eq!(result, ECMAScriptValue::from(expected));
    }

    let result = call(&mut agent, &is_nan, &this_value, &[ECMAScriptValue::from("blue")]).unwrap();
    assert_eq!(result, ECMAScriptValue::from(false));
}

#[test]
fn number_is_safe_integer_no_args() {
    let mut agent = test_agent();
    let number_constructor = agent.intrinsic(IntrinsicId::Number);
    let is_safe_integer = get(&mut agent, &number_constructor, &PropertyKey::from("isSafeInteger")).unwrap();
    let this_value = ECMAScriptValue::from(number_constructor.clone());

    let result = call(&mut agent, &is_safe_integer, &this_value, &[]).unwrap();
    assert_eq!(result, ECMAScriptValue::from(false));
}
#[test]
fn number_is_safe_integer_one_arg() {
    let mut agent = test_agent();
    let number_constructor = agent.intrinsic(IntrinsicId::Number);
    let is_safe_integer = get(&mut agent, &number_constructor, &PropertyKey::from("isSafeInteger")).unwrap();
    let this_value = ECMAScriptValue::from(number_constructor.clone());

    for (arg, expected) in [
        (f64::INFINITY, false),
        (f64::NAN, false),
        (f64::NEG_INFINITY, false),
        (0.0, true),
        (-0.0, true),
        (89.3, false),
        (3.33e200, false),
        (0x1fffffffffffff_u64 as f64, true),
        (0x20000000000000_u64 as f64, false),
        (-0x1fffffffffffff_i64 as f64, true),
        (-0x20000000000000_i64 as f64, false),
    ] {
        let result = call(&mut agent, &is_safe_integer, &this_value, &[ECMAScriptValue::from(arg)]).unwrap();
        assert_eq!(result, ECMAScriptValue::from(expected), "Tried {}, should have been {:?}", arg, expected);
    }

    let result = call(&mut agent, &is_safe_integer, &this_value, &[ECMAScriptValue::from("blue")]).unwrap();
    assert_eq!(result, ECMAScriptValue::from(false));
}
