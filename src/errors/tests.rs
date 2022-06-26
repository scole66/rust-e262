use super::*;
use crate::tests::*;
use test_case::test_case;

#[test]
fn create_native_error_object_01() {
    let mut agent = test_agent();
    let constructor = agent.intrinsic(IntrinsicId::RangeError);
    let message = "Great Googly Moogly!";
    let proto_id = IntrinsicId::RangeErrorPrototype;

    let result = create_native_error_object(&mut agent, message, constructor, proto_id, None);

    assert!(result.o.is_error_object());
    let msg_val = get(&mut agent, &result, &PropertyKey::from("message")).unwrap();
    assert_eq!(msg_val, ECMAScriptValue::from(message));
    let kind = get(&mut agent, &result, &PropertyKey::from("name")).unwrap();
    assert_eq!(kind, ECMAScriptValue::from("RangeError"));
}

#[test]
fn create_type_error_object_01() {
    let mut agent = test_agent();

    let result = create_type_error_object(&mut agent, "Happy Days");

    assert!(result.o.is_error_object());
    assert_eq!(get(&mut agent, &result, &PropertyKey::from("name")).unwrap(), ECMAScriptValue::from("TypeError"));
    assert_eq!(get(&mut agent, &result, &PropertyKey::from("message")).unwrap(), ECMAScriptValue::from("Happy Days"));
}

#[test]
fn create_type_error_01() {
    let mut agent = test_agent();

    let result = create_type_error(&mut agent, "A");
    assert!(matches!(result, AbruptCompletion::Throw { .. }));
    if let AbruptCompletion::Throw { value: objval } = result {
        assert!(objval.is_object());
        if let ECMAScriptValue::Object(obj) = objval {
            assert!(obj.o.is_error_object());
            assert_eq!(get(&mut agent, &obj, &PropertyKey::from("name")).unwrap(), ECMAScriptValue::from("TypeError"));
            assert_eq!(get(&mut agent, &obj, &PropertyKey::from("message")).unwrap(), ECMAScriptValue::from("A"));
        }
    }
}

#[test]
fn create_eval_error_object_01() {
    let mut agent = test_agent();

    let result = create_eval_error_object(&mut agent, "Happy Days");

    assert!(result.o.is_error_object());
    assert_eq!(get(&mut agent, &result, &PropertyKey::from("name")).unwrap(), ECMAScriptValue::from("EvalError"));
    assert_eq!(get(&mut agent, &result, &PropertyKey::from("message")).unwrap(), ECMAScriptValue::from("Happy Days"));
}

#[test]
fn create_eval_error_01() {
    let mut agent = test_agent();

    let result = create_eval_error(&mut agent, "A");
    assert!(matches!(result, AbruptCompletion::Throw { .. }));
    if let AbruptCompletion::Throw { value: objval } = result {
        assert!(objval.is_object());
        if let ECMAScriptValue::Object(obj) = objval {
            assert!(obj.o.is_error_object());
            assert_eq!(get(&mut agent, &obj, &PropertyKey::from("name")).unwrap(), ECMAScriptValue::from("EvalError"));
            assert_eq!(get(&mut agent, &obj, &PropertyKey::from("message")).unwrap(), ECMAScriptValue::from("A"));
        }
    }
}

#[test]
fn create_reference_error_object_01() {
    let mut agent = test_agent();

    let result = create_reference_error_object(&mut agent, "Happy Days");

    assert!(result.o.is_error_object());
    assert_eq!(get(&mut agent, &result, &PropertyKey::from("name")).unwrap(), ECMAScriptValue::from("ReferenceError"));
    assert_eq!(get(&mut agent, &result, &PropertyKey::from("message")).unwrap(), ECMAScriptValue::from("Happy Days"));
}

#[test]
fn create_reference_error_01() {
    let mut agent = test_agent();

    let result = create_reference_error(&mut agent, "A");
    assert!(matches!(result, AbruptCompletion::Throw { .. }));
    if let AbruptCompletion::Throw { value: objval } = result {
        assert!(objval.is_object());
        if let ECMAScriptValue::Object(obj) = objval {
            assert!(obj.o.is_error_object());
            assert_eq!(
                get(&mut agent, &obj, &PropertyKey::from("name")).unwrap(),
                ECMAScriptValue::from("ReferenceError")
            );
            assert_eq!(get(&mut agent, &obj, &PropertyKey::from("message")).unwrap(), ECMAScriptValue::from("A"));
        }
    }
}

#[test]
fn create_range_error_object_01() {
    let mut agent = test_agent();

    let result = create_range_error_object(&mut agent, "Happy Days");

    assert!(result.o.is_error_object());
    assert_eq!(get(&mut agent, &result, &PropertyKey::from("name")).unwrap(), ECMAScriptValue::from("RangeError"));
    assert_eq!(get(&mut agent, &result, &PropertyKey::from("message")).unwrap(), ECMAScriptValue::from("Happy Days"));
}

#[test]
fn create_range_error_01() {
    let mut agent = test_agent();

    let result = create_range_error(&mut agent, "A");
    assert!(matches!(result, AbruptCompletion::Throw { .. }));
    if let AbruptCompletion::Throw { value: objval } = result {
        assert!(objval.is_object());
        if let ECMAScriptValue::Object(obj) = objval {
            assert!(obj.o.is_error_object());
            assert_eq!(get(&mut agent, &obj, &PropertyKey::from("name")).unwrap(), ECMAScriptValue::from("RangeError"));
            assert_eq!(get(&mut agent, &obj, &PropertyKey::from("message")).unwrap(), ECMAScriptValue::from("A"));
        }
    }
}

#[test]
fn create_syntax_error_object_01() {
    let mut agent = test_agent();

    let result = create_syntax_error_object(
        &mut agent,
        "Happy Days",
        Some(Location { starting_line: 10, starting_column: 5, span: Span { starting_index: 232, length: 12 } }),
    );

    assert!(result.o.is_error_object());
    assert_eq!(get(&mut agent, &result, &PropertyKey::from("name")).unwrap(), ECMAScriptValue::from("SyntaxError"));
    assert_eq!(get(&mut agent, &result, &PropertyKey::from("message")).unwrap(), ECMAScriptValue::from("Happy Days"));
    let loc_obj = Object::try_from(get(&mut agent, &result, &"location".into()).unwrap()).unwrap();
    assert_eq!(get(&mut agent, &loc_obj, &"line".into()).unwrap(), ECMAScriptValue::from(10));
    assert_eq!(get(&mut agent, &loc_obj, &"column".into()).unwrap(), ECMAScriptValue::from(5));
    assert_eq!(get(&mut agent, &loc_obj, &"byte_length".into()).unwrap(), ECMAScriptValue::from(12));
}

#[test]
fn create_syntax_error_01() {
    let mut agent = test_agent();

    let result = create_syntax_error(&mut agent, "A", None);
    assert!(matches!(result, AbruptCompletion::Throw { .. }));
    if let AbruptCompletion::Throw { value: objval } = result {
        assert!(objval.is_object());
        if let ECMAScriptValue::Object(obj) = objval {
            assert!(obj.o.is_error_object());
            assert_eq!(
                get(&mut agent, &obj, &PropertyKey::from("name")).unwrap(),
                ECMAScriptValue::from("SyntaxError")
            );
            assert_eq!(get(&mut agent, &obj, &PropertyKey::from("message")).unwrap(), ECMAScriptValue::from("A"));
        }
    }
}

#[test]
fn create_uri_error_object_01() {
    let mut agent = test_agent();

    let result = create_uri_error_object(&mut agent, "Happy Days");

    assert!(result.o.is_error_object());
    assert_eq!(get(&mut agent, &result, &PropertyKey::from("name")).unwrap(), ECMAScriptValue::from("URIError"));
    assert_eq!(get(&mut agent, &result, &PropertyKey::from("message")).unwrap(), ECMAScriptValue::from("Happy Days"));
}

#[test]
fn create_uri_error_01() {
    let mut agent = test_agent();

    let result = create_uri_error(&mut agent, "A");
    assert!(matches!(result, AbruptCompletion::Throw { .. }));
    if let AbruptCompletion::Throw { value: objval } = result {
        assert!(objval.is_object());
        if let ECMAScriptValue::Object(obj) = objval {
            assert!(obj.o.is_error_object());
            assert_eq!(get(&mut agent, &obj, &PropertyKey::from("name")).unwrap(), ECMAScriptValue::from("URIError"));
            assert_eq!(get(&mut agent, &obj, &PropertyKey::from("message")).unwrap(), ECMAScriptValue::from("A"));
        }
    }
}

#[test]
fn error_object_debug() {
    let mut agent = test_agent();
    let eo = ErrorObject { common: RefCell::new(CommonObjectData::new(&mut agent, None, true, &[])) };
    assert_ne!(format!("{:?}", eo), "");
}

#[test]
fn error_object_object() {
    let mut agent = test_agent();
    let eo = ErrorObject::object(&mut agent, None);

    assert!(eo.o.is_error_object());
    assert!(eo.o.get_prototype_of(&mut agent).unwrap().is_none());
}

fn create_error_object(agent: &mut Agent) -> Object {
    let error_proto = agent.intrinsic(IntrinsicId::ErrorPrototype);
    ErrorObject::object(agent, Some(error_proto))
}
#[test]
fn error_object_common_object_data() {
    let mut agent = test_agent();
    let no = create_error_object(&mut agent);
    let error_prototype = agent.intrinsic(IntrinsicId::ErrorPrototype);

    let cod = no.o.common_object_data();

    assert!(cod.borrow().properties.is_empty());
    assert_eq!(cod.borrow().prototype, Some(error_prototype));
    assert!(cod.borrow().extensible);
    assert_eq!(cod.borrow().next_spot, 0);
    assert!(cod.borrow().slots.contains(&InternalSlotName::ErrorData));
}
#[test]
fn error_object_is_ordinary() {
    let mut agent = test_agent();
    let no = create_error_object(&mut agent);

    let result = no.o.is_ordinary();

    assert!(result);
}
#[test]
fn error_object_id() {
    let mut agent = test_agent();
    let no = create_error_object(&mut agent);

    // ... essentially, assert that it doesn't panic.
    no.o.id();
}
#[test]
fn error_object_to_error_object() {
    let mut agent = test_agent();
    let no = create_error_object(&mut agent);

    let result = no.o.to_error_obj();
    assert!(result.is_some());
}
#[test]
fn error_object_is_error_object() {
    let mut agent = test_agent();
    let no = create_error_object(&mut agent);

    let result = no.o.is_error_object();

    assert!(result);
}
#[test]
fn error_object_get_prototype_of() {
    let mut agent = test_agent();
    let no = create_error_object(&mut agent);

    let result = no.o.get_prototype_of(&mut agent).unwrap();
    assert!(result.is_some());
}
#[test]
fn error_object_set_prototype_of() {
    let mut agent = test_agent();
    let no = create_error_object(&mut agent);

    let result = no.o.set_prototype_of(&mut agent, None).unwrap();
    assert!(result);
}
#[test]
fn error_object_is_extensible() {
    let mut agent = test_agent();
    let no = create_error_object(&mut agent);

    let result = no.o.is_extensible(&mut agent).unwrap();
    assert!(result);
}
#[test]
fn error_object_prevent_extensions() {
    let mut agent = test_agent();
    let no = create_error_object(&mut agent);

    let result = no.o.prevent_extensions(&mut agent).unwrap();
    assert!(result);
}
#[test]
fn error_object_get_own_property() {
    let mut agent = test_agent();
    let no = create_error_object(&mut agent);

    let result = no.o.get_own_property(&mut agent, &PropertyKey::from("a")).unwrap();
    assert!(result.is_none());
}
#[test]
fn error_object_define_own_property() {
    let mut agent = test_agent();
    let no = create_error_object(&mut agent);

    let result =
        no.o.define_own_property(
            &mut agent,
            PropertyKey::from("a"),
            PotentialPropertyDescriptor { value: Some(ECMAScriptValue::Undefined), ..Default::default() },
        )
        .unwrap();
    assert!(result);
}
#[test]
fn error_object_has_property() {
    let mut agent = test_agent();
    let no = create_error_object(&mut agent);

    let result = no.o.has_property(&mut agent, &PropertyKey::from("a")).unwrap();
    assert!(!result);
}
#[test]
fn error_object_get() {
    let mut agent = test_agent();
    let no = create_error_object(&mut agent);

    let result = no.o.get(&mut agent, &PropertyKey::from("a"), &ECMAScriptValue::from(no.clone())).unwrap();
    assert_eq!(result, ECMAScriptValue::Undefined);
}
#[test]
fn error_object_set() {
    let mut agent = test_agent();
    let no = create_error_object(&mut agent);

    let result =
        no.o.set(&mut agent, PropertyKey::from("a"), ECMAScriptValue::from(88.0), &ECMAScriptValue::from(no.clone()))
            .unwrap();
    assert!(result);
}
#[test]
fn error_object_delete() {
    let mut agent = test_agent();
    let no = create_error_object(&mut agent);

    let result = no.o.delete(&mut agent, &PropertyKey::from("a")).unwrap();
    assert!(result);
}
#[test]
fn error_object_own_property_keys() {
    let mut agent = test_agent();
    let no = create_error_object(&mut agent);

    let result = no.o.own_property_keys(&mut agent).unwrap();
    assert_eq!(result, &[]);
}
#[test]
fn error_object_other_automatic_functions() {
    let mut agent = test_agent();
    let no = create_error_object(&mut agent);

    assert!(!no.o.is_number_object());
    assert!(no.o.to_function_obj().is_none());
    assert!(!no.o.is_boolean_object());
    assert!(!no.o.is_string_object());
    assert!(!no.o.is_regexp_object());
    assert!(no.o.to_builtin_function_obj().is_none());
    assert!(!no.o.is_callable_obj());
    assert!(no.o.to_boolean_obj().is_none());
    //assert!(no.o.to_number_obj().is_none());
    assert!(no.o.to_callable_obj().is_none());
    assert!(no.o.to_constructable().is_none());
    assert!(!no.o.is_arguments_object());
    assert!(!no.o.is_date_object());
}

#[test]
fn error_constructor_data_props() {
    let mut agent = test_agent();
    let error_constructor = agent.intrinsic(IntrinsicId::Error);

    let val = get(&mut agent, &error_constructor, &PropertyKey::from("prototype")).unwrap();
    let error_prototype = agent.intrinsic(IntrinsicId::ErrorPrototype);
    assert_eq!(val, ECMAScriptValue::from(error_prototype));
}

#[test]
fn error_constructor_function_01() {
    // Called as function, with argument.
    let mut agent = test_agent();
    let error_constructor = ECMAScriptValue::from(agent.intrinsic(IntrinsicId::Error));

    let result =
        call(&mut agent, &error_constructor, &ECMAScriptValue::Undefined, &[ECMAScriptValue::from("A")]).unwrap();
    let obj = to_object(&mut agent, result).unwrap();

    assert!(obj.o.is_error_object());
    assert_eq!(get(&mut agent, &obj, &PropertyKey::from("message")).unwrap(), ECMAScriptValue::from("A"));
    assert_eq!(get(&mut agent, &obj, &PropertyKey::from("name")).unwrap(), ECMAScriptValue::from("Error"));
}

#[test]
fn error_constructor_function_02() {
    // Called as function, with no argument.
    let mut agent = test_agent();
    let error_constructor = ECMAScriptValue::from(agent.intrinsic(IntrinsicId::Error));

    let result = call(&mut agent, &error_constructor, &ECMAScriptValue::Undefined, &[]).unwrap();
    let obj = to_object(&mut agent, result).unwrap();

    assert!(obj.o.is_error_object());
    assert!(!has_own_property(&mut agent, &obj, &PropertyKey::from("message")).unwrap());
    assert_eq!(get(&mut agent, &obj, &PropertyKey::from("name")).unwrap(), ECMAScriptValue::from("Error"));
}

#[test]
fn error_constructor_function_03() {
    // Called as constructor
    let mut agent = test_agent();
    let error_constructor = agent.intrinsic(IntrinsicId::Error);

    let result = construct(&mut agent, &error_constructor, &[ECMAScriptValue::from("A")], None).unwrap();
    let obj = to_object(&mut agent, result).unwrap();

    assert!(obj.o.is_error_object());
    assert_eq!(get(&mut agent, &obj, &PropertyKey::from("message")).unwrap(), ECMAScriptValue::from("A"));
    assert_eq!(get(&mut agent, &obj, &PropertyKey::from("name")).unwrap(), ECMAScriptValue::from("Error"));
}

#[test]
fn error_constructor_throws() {
    // ordinary_create_from_contructor throws.
    // This looks to be difficult to make happen, but I can imagine some class shenanigans that could do it.
    let mut agent = test_agent();
    let error_constructor = agent.intrinsic(IntrinsicId::Error);

    // This hack is to get around the "not configurable" characteristic of Error.prototype.
    // (It replaces Error.prototype (a data property) with an accessor property that throws when "prototype" is gotten.)
    let new_prop = PropertyKind::Accessor(AccessorProperty {
        get: ECMAScriptValue::from(agent.intrinsic(IntrinsicId::ThrowTypeError)),
        set: ECMAScriptValue::Undefined,
    });
    {
        let mut cod = error_constructor.o.common_object_data().borrow_mut();
        let mut prop = cod.properties.get_mut(&PropertyKey::from("prototype")).unwrap();
        prop.property = new_prop;
    }

    let result = construct(&mut agent, &error_constructor, &[], None).unwrap_err();
    assert_eq!(unwind_type_error(&mut agent, result), "Generic TypeError");
}
#[test]
fn error_constructor_to_string_throws() {
    let mut agent = test_agent();
    let error_constructor = agent.intrinsic(IntrinsicId::Error);
    let sym = ECMAScriptValue::from(Symbol::new(&mut agent, None));

    let result = construct(&mut agent, &error_constructor, &[sym], None).unwrap_err();
    assert_eq!(unwind_type_error(&mut agent, result), "Symbols may not be converted to strings");
}

#[test]
fn error_prototype_data_props() {
    let mut agent = test_agent();
    let error_prototype = agent.intrinsic(IntrinsicId::ErrorPrototype);

    let val = get(&mut agent, &error_prototype, &PropertyKey::from("constructor")).unwrap();
    let error_constructor = agent.intrinsic(IntrinsicId::Error);
    assert_eq!(val, ECMAScriptValue::from(error_constructor));

    let val = get(&mut agent, &error_prototype, &PropertyKey::from("message")).unwrap();
    assert_eq!(val, ECMAScriptValue::from(""));

    let val = get(&mut agent, &error_prototype, &PropertyKey::from("name")).unwrap();
    assert_eq!(val, ECMAScriptValue::from("Error"));
}

use crate::object::{define_property_or_throw, invoke, set};
#[test]
fn error_prototype_tostring_01() {
    let mut agent = test_agent();
    let error_constructor = agent.intrinsic(IntrinsicId::Error);
    let errobj = construct(&mut agent, &error_constructor, &[ECMAScriptValue::from("ErrorMessage")], None).unwrap();

    let result = invoke(&mut agent, errobj, &PropertyKey::from("toString"), &[]).unwrap();
    assert_eq!(result, ECMAScriptValue::from("Error: ErrorMessage"));
}
#[test]
fn error_prototype_tostring_02() {
    let mut agent = test_agent();
    let error_constructor = agent.intrinsic(IntrinsicId::Error);
    let errobjval = construct(&mut agent, &error_constructor, &[ECMAScriptValue::from("ErrorMessage")], None).unwrap();
    let errobj = to_object(&mut agent, errobjval.clone()).unwrap();
    set(&mut agent, &errobj, PropertyKey::from("name"), ECMAScriptValue::from("Bob"), false).unwrap();
    set(&mut agent, &errobj, PropertyKey::from("message"), ECMAScriptValue::from("you have a phone call"), false)
        .unwrap();

    let result = invoke(&mut agent, errobjval, &PropertyKey::from("toString"), &[]).unwrap();
    assert_eq!(result, ECMAScriptValue::from("Bob: you have a phone call"));
}
#[test]
fn error_prototype_tostring_03() {
    let mut agent = test_agent();
    let error_constructor = agent.intrinsic(IntrinsicId::Error);
    let errobjval = construct(&mut agent, &error_constructor, &[ECMAScriptValue::from("ErrorMessage")], None).unwrap();
    let errobj = to_object(&mut agent, errobjval.clone()).unwrap();
    set(&mut agent, &errobj, PropertyKey::from("name"), ECMAScriptValue::Undefined, false).unwrap();
    set(&mut agent, &errobj, PropertyKey::from("message"), ECMAScriptValue::Undefined, false).unwrap();

    let result = invoke(&mut agent, errobjval, &PropertyKey::from("toString"), &[]).unwrap();
    assert_eq!(result, ECMAScriptValue::from("Error"));
}
#[test]
fn error_prototype_tostring_04() {
    let mut agent = test_agent();
    let error_constructor = agent.intrinsic(IntrinsicId::Error);
    let errobjval = construct(&mut agent, &error_constructor, &[ECMAScriptValue::from("ErrorMessage")], None).unwrap();
    let errobj = to_object(&mut agent, errobjval.clone()).unwrap();
    set(&mut agent, &errobj, PropertyKey::from("name"), ECMAScriptValue::from("Bob"), false).unwrap();
    set(&mut agent, &errobj, PropertyKey::from("message"), ECMAScriptValue::Undefined, false).unwrap();

    let result = invoke(&mut agent, errobjval, &PropertyKey::from("toString"), &[]).unwrap();
    assert_eq!(result, ECMAScriptValue::from("Bob"));
}
#[test]
fn error_prototype_tostring_05() {
    let mut agent = test_agent();
    let error_constructor = agent.intrinsic(IntrinsicId::Error);
    let errobjval = construct(&mut agent, &error_constructor, &[ECMAScriptValue::from("ErrorMessage")], None).unwrap();
    let errobj = to_object(&mut agent, errobjval.clone()).unwrap();
    set(&mut agent, &errobj, PropertyKey::from("name"), ECMAScriptValue::Undefined, false).unwrap();
    set(&mut agent, &errobj, PropertyKey::from("message"), ECMAScriptValue::from("Message"), false).unwrap();

    let result = invoke(&mut agent, errobjval, &PropertyKey::from("toString"), &[]).unwrap();
    assert_eq!(result, ECMAScriptValue::from("Error: Message"));
}
#[test]
fn error_prototype_tostring_06() {
    let mut agent = test_agent();
    let error_constructor = agent.intrinsic(IntrinsicId::Error);
    let errobjval = construct(&mut agent, &error_constructor, &[ECMAScriptValue::from("ErrorMessage")], None).unwrap();
    let errobj = to_object(&mut agent, errobjval.clone()).unwrap();
    set(&mut agent, &errobj, PropertyKey::from("name"), ECMAScriptValue::from(""), false).unwrap();
    set(&mut agent, &errobj, PropertyKey::from("message"), ECMAScriptValue::from("Message"), false).unwrap();

    let result = invoke(&mut agent, errobjval, &PropertyKey::from("toString"), &[]).unwrap();
    assert_eq!(result, ECMAScriptValue::from("Message"));
}
#[test]
fn error_prototype_tostring_07() {
    // getting property "name" throws
    let mut agent = test_agent();
    let error_constructor = agent.intrinsic(IntrinsicId::Error);
    let errobjval = construct(&mut agent, &error_constructor, &[ECMAScriptValue::from("ErrorMessage")], None).unwrap();
    let errobj = to_object(&mut agent, errobjval.clone()).unwrap();
    let desc = PotentialPropertyDescriptor {
        get: Some(ECMAScriptValue::from(agent.intrinsic(IntrinsicId::ThrowTypeError))),
        ..Default::default()
    };
    define_property_or_throw(&mut agent, &errobj, PropertyKey::from("name"), desc).unwrap();

    let result = invoke(&mut agent, errobjval, &PropertyKey::from("toString"), &[]).unwrap_err();
    assert_eq!(unwind_type_error(&mut agent, result), "Generic TypeError");
}
#[test]
fn error_prototype_tostring_08() {
    // getting property "message" throws
    let mut agent = test_agent();
    let error_constructor = agent.intrinsic(IntrinsicId::Error);
    let errobjval = construct(&mut agent, &error_constructor, &[ECMAScriptValue::from("ErrorMessage")], None).unwrap();
    let errobj = to_object(&mut agent, errobjval.clone()).unwrap();
    let desc = PotentialPropertyDescriptor {
        get: Some(ECMAScriptValue::from(agent.intrinsic(IntrinsicId::ThrowTypeError))),
        ..Default::default()
    };
    define_property_or_throw(&mut agent, &errobj, PropertyKey::from("message"), desc).unwrap();

    let result = invoke(&mut agent, errobjval, &PropertyKey::from("toString"), &[]).unwrap_err();
    assert_eq!(unwind_type_error(&mut agent, result), "Generic TypeError");
}
#[test]
fn error_prototype_tostring_09() {
    let mut agent = test_agent();
    let error_constructor = agent.intrinsic(IntrinsicId::Error);
    let errobjval = construct(&mut agent, &error_constructor, &[ECMAScriptValue::from("ErrorMessage")], None).unwrap();
    let errobj = to_object(&mut agent, errobjval.clone()).unwrap();
    let sym = ECMAScriptValue::from(Symbol::new(&mut agent, None));
    set(&mut agent, &errobj, PropertyKey::from("name"), sym, false).unwrap();

    let result = invoke(&mut agent, errobjval, &PropertyKey::from("toString"), &[]).unwrap_err();
    assert_eq!(unwind_type_error(&mut agent, result), "Symbols may not be converted to strings");
}
#[test]
fn error_prototype_tostring_10() {
    let mut agent = test_agent();
    let error_constructor = agent.intrinsic(IntrinsicId::Error);
    let errobjval = construct(&mut agent, &error_constructor, &[ECMAScriptValue::from("ErrorMessage")], None).unwrap();
    let errobj = to_object(&mut agent, errobjval.clone()).unwrap();
    let sym = ECMAScriptValue::from(Symbol::new(&mut agent, None));
    set(&mut agent, &errobj, PropertyKey::from("message"), sym, false).unwrap();

    let result = invoke(&mut agent, errobjval, &PropertyKey::from("toString"), &[]).unwrap_err();
    assert_eq!(unwind_type_error(&mut agent, result), "Symbols may not be converted to strings");
}
#[test]
fn error_prototype_to_string_11() {
    let mut agent = test_agent();
    let error_prototype = agent.intrinsic(IntrinsicId::ErrorPrototype);
    let func = get(&mut agent, &error_prototype, &PropertyKey::from("toString")).unwrap();

    let result = call(&mut agent, &func, &ECMAScriptValue::Undefined, &[]).unwrap_err();
    assert_eq!(unwind_type_error(&mut agent, result), "Error.prototype.toString called with non-object this value");
}

fn native_error_constructor_properties(intrinsic: IntrinsicId, expected_name: &str) {
    let mut agent = test_agent();
    let constructor = agent.intrinsic(intrinsic);

    let proto = constructor.o.get_prototype_of(&mut agent).unwrap().unwrap();
    assert_eq!(proto, agent.intrinsic(IntrinsicId::Error));
    let name = get(&mut agent, &constructor, &PropertyKey::from("name")).unwrap();
    assert_eq!(name, ECMAScriptValue::from(expected_name));
}

#[test]
fn type_error_constructor_properties() {
    native_error_constructor_properties(IntrinsicId::TypeError, "TypeError");
}
#[test]
fn eval_error_constructor_properties() {
    native_error_constructor_properties(IntrinsicId::EvalError, "EvalError");
}
#[test]
fn range_error_constructor_properties() {
    native_error_constructor_properties(IntrinsicId::RangeError, "RangeError");
}
#[test]
fn reference_error_constructor_properties() {
    native_error_constructor_properties(IntrinsicId::ReferenceError, "ReferenceError");
}
#[test]
fn syntax_error_constructor_properties() {
    native_error_constructor_properties(IntrinsicId::SyntaxError, "SyntaxError");
}
#[test]
fn uri_error_constructor_properties() {
    native_error_constructor_properties(IntrinsicId::URIError, "URIError");
}

fn native_error_prototype_properties(prototype: IntrinsicId, constructor: IntrinsicId, name: &str) {
    let mut agent = test_agent();
    let prototype = agent.intrinsic(prototype);
    let constructor = agent.intrinsic(constructor);
    let error_prototype = agent.intrinsic(IntrinsicId::ErrorPrototype);

    assert!(!prototype.o.is_error_object());
    let proto_proto = prototype.o.get_prototype_of(&mut agent).unwrap().unwrap();
    assert_eq!(proto_proto, error_prototype);

    let cons = get(&mut agent, &prototype, &PropertyKey::from("constructor")).unwrap();
    assert_eq!(cons, ECMAScriptValue::from(constructor));

    let msg = get(&mut agent, &prototype, &PropertyKey::from("message")).unwrap();
    assert_eq!(msg, ECMAScriptValue::from(""));

    let myname = get(&mut agent, &prototype, &PropertyKey::from("name")).unwrap();
    assert_eq!(myname, ECMAScriptValue::from(name));
}

#[test]
fn eval_error_prototype_properties() {
    native_error_prototype_properties(IntrinsicId::EvalErrorPrototype, IntrinsicId::EvalError, "EvalError");
}
#[test]
fn range_error_prototype_properties() {
    native_error_prototype_properties(IntrinsicId::RangeErrorPrototype, IntrinsicId::RangeError, "RangeError");
}
#[test]
fn reference_error_prototype_properties() {
    native_error_prototype_properties(
        IntrinsicId::ReferenceErrorPrototype,
        IntrinsicId::ReferenceError,
        "ReferenceError",
    );
}
#[test]
fn syntax_error_prototype_properties() {
    native_error_prototype_properties(IntrinsicId::SyntaxErrorPrototype, IntrinsicId::SyntaxError, "SyntaxError");
}
#[test]
fn type_error_prototype_properties() {
    native_error_prototype_properties(IntrinsicId::TypeErrorPrototype, IntrinsicId::TypeError, "TypeError");
}
#[test]
fn uri_error_prototype_properties() {
    native_error_prototype_properties(IntrinsicId::URIErrorPrototype, IntrinsicId::URIError, "URIError");
}

fn test_error_constructor(const_id: IntrinsicId, proto_id: IntrinsicId, name: &str) {
    let mut agent = test_agent();
    let constructor = agent.intrinsic(const_id);
    let proto = agent.intrinsic(proto_id);
    let objval = construct(&mut agent, &constructor, &[ECMAScriptValue::from("test message")], None).unwrap();
    let obj = to_object(&mut agent, objval).unwrap();

    assert!(obj.o.is_error_object());
    assert_eq!(get(&mut agent, &obj, &PropertyKey::from("name")).unwrap(), ECMAScriptValue::from(name));
    assert_eq!(get(&mut agent, &obj, &PropertyKey::from("message")).unwrap(), ECMAScriptValue::from("test message"));
    assert_eq!(obj.o.get_prototype_of(&mut agent).unwrap().unwrap(), proto);
}

#[test]
fn test_eval_error_constructor() {
    test_error_constructor(IntrinsicId::EvalError, IntrinsicId::EvalErrorPrototype, "EvalError");
}
#[test]
fn test_range_error_constructor() {
    test_error_constructor(IntrinsicId::RangeError, IntrinsicId::RangeErrorPrototype, "RangeError");
}
#[test]
fn test_reference_error_constructor() {
    test_error_constructor(IntrinsicId::ReferenceError, IntrinsicId::ReferenceErrorPrototype, "ReferenceError");
}
#[test]
fn test_sytax_error_constructor() {
    test_error_constructor(IntrinsicId::SyntaxError, IntrinsicId::SyntaxErrorPrototype, "SyntaxError");
}
#[test]
fn test_type_error_constructor() {
    test_error_constructor(IntrinsicId::TypeError, IntrinsicId::TypeErrorPrototype, "TypeError");
}
#[test]
fn test_uri_error_constructor() {
    test_error_constructor(IntrinsicId::URIError, IntrinsicId::URIErrorPrototype, "URIError");
}

#[test_case(|a: &mut Agent| create_type_error_object(a, "message 1") => "TypeError: message 1"; "type error")]
#[test_case(|a: &mut Agent| create_syntax_error_object(a, "message 2", None) => "SyntaxError: message 2"; "syntax error")]
fn unwind_any_error_value(maker: fn(&mut Agent) -> Object) -> String {
    let mut agent = test_agent();
    let errobj = maker(&mut agent);
    super::unwind_any_error_value(&mut agent, ECMAScriptValue::from(errobj))
}

#[test_case(|a: &mut Agent| create_type_error(a, "blue") => "TypeError: blue"; "type error")]
#[test_case(|a: &mut Agent| create_syntax_error(a, "ouch", None) => "SyntaxError: ouch"; "syntax error")]
#[test_case(|_: &mut Agent| AbruptCompletion::Break{value: NormalCompletion::Empty, target: None} => panics "Improper completion for error: "; "not error")]
fn unwind_any_error(maker: fn(&mut Agent) -> AbruptCompletion) -> String {
    let mut agent = test_agent();
    let completion = maker(&mut agent);
    super::unwind_any_error(&mut agent, completion)
}
