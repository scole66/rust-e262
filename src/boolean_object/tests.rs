use super::*;
use crate::tests::*;

#[test]
fn create_boolean_object_01() {
    setup_test_agent();
    let result = create_boolean_object(true);

    let maybe_native = result.o.to_boolean_obj();
    assert!(maybe_native.is_some());

    let native = maybe_native.unwrap();
    assert_eq!(*native.boolean_data().borrow(), true);
}

#[test]
fn create_boolean_object_02() {
    setup_test_agent();
    let result = create_boolean_object(false);

    let maybe_native = result.o.to_boolean_obj();
    assert!(maybe_native.is_some());

    let native = maybe_native.unwrap();
    assert_eq!(*native.boolean_data().borrow(), false);
}

#[test]
fn this_boolean_value_01() {
    setup_test_agent();
    let result = this_boolean_value(&ECMAScriptValue::Boolean(true));
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), true);
}
#[test]
fn this_boolean_value_02() {
    setup_test_agent();
    let result = this_boolean_value(&ECMAScriptValue::Boolean(false));
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), false);
}
#[test]
fn this_boolean_value_03() {
    setup_test_agent();
    let result = this_boolean_value(&ECMAScriptValue::Null);
    assert!(result.is_err());
    let msg = unwind_type_error(result.unwrap_err());
    assert_eq!(msg, "Value is not boolean");
}
#[test]
fn this_boolean_value_04() {
    setup_test_agent();
    let bool_obj = create_boolean_object(true);
    let result = this_boolean_value(&ECMAScriptValue::Object(bool_obj));
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), true);
}
#[test]
fn this_boolean_value_05() {
    setup_test_agent();
    let bool_obj = create_boolean_object(false);
    let result = this_boolean_value(&ECMAScriptValue::Object(bool_obj));
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), false);
}

#[test]
fn this_boolean_value_06() {
    setup_test_agent();
    let proto = intrinsic(IntrinsicId::ObjectPrototype);
    let other_obj = ordinary_object_create(Some(proto), &[]);
    let result = this_boolean_value(&ECMAScriptValue::Object(other_obj));
    assert!(result.is_err());
    let msg = unwind_type_error(result.unwrap_err());
    assert_eq!(msg, "Object has no boolean value");
}

#[test]
fn get_prototype_of_01() {
    setup_test_agent();
    let bool_obj = create_boolean_object(true);
    let proto = bool_obj.o.get_prototype_of().unwrap().unwrap();
    assert_eq!(proto, intrinsic(IntrinsicId::BooleanPrototype));
}

#[test]
fn set_prototype_of_01() {
    setup_test_agent();
    let bool_obj = create_boolean_object(true);
    let res = bool_obj.o.set_prototype_of(None).unwrap();
    assert!(res);
    assert!(bool_obj.o.get_prototype_of().unwrap().is_none());
}

#[test]
fn is_extensible_01() {
    setup_test_agent();
    let bool_obj = create_boolean_object(true);
    let res = bool_obj.o.is_extensible().unwrap();
    assert!(res);
}

#[test]
fn prevent_extensions_01() {
    setup_test_agent();
    let bool_obj = create_boolean_object(true);
    let res = bool_obj.o.prevent_extensions().unwrap();
    assert!(res);
    assert!(!bool_obj.o.is_extensible().unwrap());
}

#[test]
fn define_and_get_own_property_01() {
    setup_test_agent();
    let bool_obj = create_boolean_object(true);
    let res = bool_obj
        .o
        .define_own_property(
            PropertyKey::from("rust"),
            PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from("is awesome")), ..Default::default() },
        )
        .unwrap();
    assert!(res);
    let val = bool_obj.o.get_own_property(&PropertyKey::from("rust")).unwrap().unwrap();
    assert_eq!(val.enumerable, false);
    assert_eq!(val.configurable, false);
    assert!(matches!(val.property, PropertyKind::Data(..)));
    if let PropertyKind::Data(d) = val.property {
        assert_eq!(d.value, ECMAScriptValue::from("is awesome"));
        assert_eq!(d.writable, false);
    }
}

#[test]
fn has_property_01() {
    setup_test_agent();
    let bool_obj = create_boolean_object(true);
    let res = bool_obj.o.has_property(&PropertyKey::from("rust")).unwrap();
    assert_eq!(res, false);
    let res2 = bool_obj.o.has_property(&PropertyKey::from("constructor")).unwrap();
    assert_eq!(res2, true);
}

#[test]
fn get_01() {
    setup_test_agent();
    let bool_obj = create_boolean_object(true);
    let res = bool_obj.o.get(&PropertyKey::from("rust"), &ECMAScriptValue::Undefined).unwrap();
    assert_eq!(res, ECMAScriptValue::Undefined);
    let res2 = bool_obj.o.get(&PropertyKey::from("constructor"), &ECMAScriptValue::Undefined).unwrap();
    assert!(matches!(res2, ECMAScriptValue::Object(..)));
    if let ECMAScriptValue::Object(obj) = res2 {
        assert_eq!(obj, intrinsic(IntrinsicId::Boolean));
    }
}

#[test]
fn set_01() {
    setup_test_agent();
    let bool_obj = create_boolean_object(true);
    let receiver = ECMAScriptValue::Object(bool_obj.clone());
    let res = bool_obj.o.set(PropertyKey::from("rust"), ECMAScriptValue::Null, &receiver).unwrap();
    assert_eq!(res, true);
}

#[test]
fn delete_01() {
    setup_test_agent();
    let bool_obj = create_boolean_object(true);
    let res = bool_obj.o.delete(&PropertyKey::from("rust")).unwrap();
    assert_eq!(res, true);
}

#[test]
fn own_keys_01() {
    setup_test_agent();
    let bool_obj = create_boolean_object(true);
    let res = bool_obj.o.own_property_keys().unwrap();
    assert!(res.is_empty())
}

#[test]
fn uses_ordinary_get_prototype_of_01() {
    setup_test_agent();
    let bool_obj = create_boolean_object(true);
    assert_eq!(bool_obj.o.uses_ordinary_get_prototype_of(), true);
}

#[test]
fn bool_object_checks() {
    setup_test_agent();
    let bool_obj = create_boolean_object(true);
    assert_eq!(bool_obj.o.is_boolean_object(), true);
    assert_eq!(bool_obj.o.is_callable_obj(), false);
    assert_eq!(bool_obj.o.is_string_object(), false);
    assert_eq!(bool_obj.o.is_regexp_object(), false);
    assert_eq!(bool_obj.o.is_arguments_object(), false);
    assert_eq!(bool_obj.o.is_error_object(), false);
    assert!(bool_obj.o.to_function_obj().is_none());
    assert!(bool_obj.o.to_builtin_function_obj().is_none());
    assert_eq!(bool_obj.o.is_number_object(), false);
    assert_eq!(bool_obj.o.is_date_object(), false);
    assert!(bool_obj.o.to_error_obj().is_none());
    assert!(bool_obj.o.to_callable_obj().is_none());
}
#[test]
fn bool_object_debug() {
    setup_test_agent();
    let bool_obj = create_boolean_object(true);
    assert_ne!(format!("{:?}", bool_obj), "");
}
