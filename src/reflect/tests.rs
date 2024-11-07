use super::*;
use crate::tests::*;
use test_case::test_case;

#[test]
fn provision_reflect_intrinsic() {
    setup_test_agent();
    // Just setting up the test agent will complete coverage, so we're really just checking the result.

    let reflect = intrinsic(IntrinsicId::Reflect);
    let global = get_global_object().unwrap();
    let global_reflect_pd = global.o.get_own_property(&"Reflect".into()).unwrap().unwrap();
    let global_reflect = Object::try_from(data_validation(global_reflect_pd, true, false, true)).unwrap();
    assert_eq!(reflect, global_reflect);

    let apply = reflect.o.get_own_property(&"apply".into()).unwrap().unwrap();
    func_validation(apply, "apply", 3);

    // Reflect.construct ( target, argumentsList [ , newTarget ] )
    let construct = reflect.o.get_own_property(&"construct".into()).unwrap().unwrap();
    func_validation(construct, "construct", 2);
    // Reflect.defineProperty ( target, propertyKey, attributes )
    let define_property = reflect.o.get_own_property(&"defineProperty".into()).unwrap().unwrap();
    func_validation(define_property, "defineProperty", 3);
    // Reflect.deleteProperty ( target, propertyKey )
    let delete_property = reflect.o.get_own_property(&"deleteProperty".into()).unwrap().unwrap();
    func_validation(delete_property, "deleteProperty", 2);
    // Reflect.get ( target, propertyKey [ , receiver ] )
    let get = reflect.o.get_own_property(&"get".into()).unwrap().unwrap();
    func_validation(get, "get", 2);
    // Reflect.getOwnPropertyDescriptor ( target, propertyKey )
    let get_own_property_descriptor = reflect.o.get_own_property(&"getOwnPropertyDescriptor".into()).unwrap().unwrap();
    func_validation(get_own_property_descriptor, "getOwnPropertyDescriptor", 2);
    // Reflect.getPrototypeOf ( target )
    let get_prototype_of = reflect.o.get_own_property(&"getPrototypeOf".into()).unwrap().unwrap();
    func_validation(get_prototype_of, "getPrototypeOf", 1);
    // Reflect.has ( target, propertyKey )
    let has = reflect.o.get_own_property(&"has".into()).unwrap().unwrap();
    func_validation(has, "has", 2);
    // Reflect.isExtensible ( target )
    let is_extensible = reflect.o.get_own_property(&"isExtensible".into()).unwrap().unwrap();
    func_validation(is_extensible, "isExtensible", 1);
    // Reflect.ownKeys ( target )
    let own_keys = reflect.o.get_own_property(&"ownKeys".into()).unwrap().unwrap();
    func_validation(own_keys, "ownKeys", 1);
    // Reflect.preventExtensions ( target )
    let prevent_extensions = reflect.o.get_own_property(&"preventExtensions".into()).unwrap().unwrap();
    func_validation(prevent_extensions, "preventExtensions", 1);
    // Reflect.set ( target, propertyKey, V [ , receiver ] )
    let set = reflect.o.get_own_property(&"set".into()).unwrap().unwrap();
    func_validation(set, "set", 3);
    // Reflect.setPrototypeOf ( target, proto )
    let set_prototype_of = reflect.o.get_own_property(&"setPrototypeOf".into()).unwrap().unwrap();
    func_validation(set_prototype_of, "setPrototypeOf", 2);

    let string_tag = reflect.o.get_own_property(&wks(WksId::ToStringTag).into()).unwrap().unwrap();
    let tag = JSString::try_from(data_validation(string_tag, false, false, true)).unwrap();
    assert_eq!(tag, JSString::from("Reflect"));
}

#[test_case(|| (ordinary_object_create(None).into(), ECMAScriptValue::Undefined, ECMAScriptValue::Undefined) => serr("TypeError: Reflect.apply requires a callable target"); "not callable")]
#[test_case(|| (intrinsic(IntrinsicId::IsNaN).into(), ECMAScriptValue::Undefined, create_array_from_list(&[100.into()]).into()) => sok("false"); "successful; without this")]
#[test_case(|| (intrinsic(IntrinsicId::IsNaN).into(), ECMAScriptValue::Undefined, ECMAScriptValue::Null) => serr("TypeError: CreateListFromArrayLike called on non-object"); "bad args")]
fn reflect_apply(
    make_args: impl FnOnce() -> (ECMAScriptValue, ECMAScriptValue, ECMAScriptValue),
) -> Result<String, String> {
    setup_test_agent();
    let (target, this_argument, argument_list) = make_args();
    super::reflect_apply(&ECMAScriptValue::Undefined, None, &[target, this_argument, argument_list])
        .map_err(unwind_any_error)
        .map(|v| v.test_result_string())
}

#[test_case(Vec::new => serr("TypeError: Reflect.construct: target must be a constructor"); "no args")]
#[test_case(
    || vec![ECMAScriptValue::Object(intrinsic(IntrinsicId::Array))]
    => serr("TypeError: CreateListFromArrayLike called on non-object");
    "constructor target only"
)]
#[test_case(
    || vec![
        ECMAScriptValue::Object(intrinsic(IntrinsicId::Array)),
        ECMAScriptValue::Object(create_array_from_list(&[])),
        ECMAScriptValue::Number(10.3)
    ]
    => serr("TypeError: Reflect.construct: newTarget, if supplied, must be a constructor");
    "newTarget not constructor"
)]
#[test_case(
    || vec![
        ECMAScriptValue::Object(intrinsic(IntrinsicId::Array)),
        ECMAScriptValue::Object(create_array_from_list(
            &[ECMAScriptValue::from("a"), ECMAScriptValue::from("b"), ECMAScriptValue::from("c")])
        ),
        ECMAScriptValue::Object(intrinsic(IntrinsicId::String))
    ]
    => sok("0:a,1:b,2:c,length:3");
    "Array as String"
)]
fn reflect_construct(make_args: impl FnOnce() -> Vec<ECMAScriptValue>) -> Result<String, String> {
    setup_test_agent();
    let args = make_args();
    super::reflect_construct(&ECMAScriptValue::Undefined, None, &args)
        .map_err(unwind_any_error)
        .map(|v| v.test_result_string())
}

#[test_case(Vec::new => serr("TypeError: Reflect.defineProperty: target must be an object"); "no args")]
#[test_case(
    || vec![ECMAScriptValue::Object(ordinary_object_create(None))]
    => serr("TypeError: Must be an object");
    "ToPropertyDescriptor fails"
)]
#[test_case(
    || vec![
        ECMAScriptValue::Object(ordinary_object_create(None)),
        ECMAScriptValue::Object(DeadObject::object())
    ]
    => serr("TypeError: get called on DeadObject");
    "ToPropertyKey fails"
)]
#[test_case(
    || vec![
        ECMAScriptValue::Object(DeadObject::object()),
        ECMAScriptValue::String(JSString::from("property")),
        ECMAScriptValue::Object({
            let o = ordinary_object_create(None);
            o.create_data_property_or_throw("writable", true).unwrap();
            o.create_data_property_or_throw("value", 12345).unwrap();
            o.create_data_property_or_throw("enumerable", true).unwrap();
            o.create_data_property_or_throw("configurable", true).unwrap();
            o
        })
    ]
    => serr("TypeError: define_own_property called on DeadObject");
    "[[DefineOwnProperty]] fails"
)]
#[test_case(
    || vec![
        ECMAScriptValue::Object(ordinary_object_create(None)),
        ECMAScriptValue::String(JSString::from("property")),
        ECMAScriptValue::Object({
            let o = ordinary_object_create(None);
            o.create_data_property_or_throw("writable", true).unwrap();
            o.create_data_property_or_throw("value", 12345).unwrap();
            o.create_data_property_or_throw("enumerable", true).unwrap();
            o.create_data_property_or_throw("configurable", true).unwrap();
            o
        })
    ]
    => sok("true; property:12345");
    "success"
)]
fn reflect_define_property(make_args: impl FnOnce() -> Vec<ECMAScriptValue>) -> Result<String, String> {
    setup_test_agent();
    let args = make_args();
    let target = if args.is_empty() {
        None
    } else if let ECMAScriptValue::Object(obj) = &args[0] {
        Some(obj)
    } else {
        None
    };
    super::reflect_define_property(&ECMAScriptValue::Undefined, None, &args).map_err(unwind_any_error).map(|v| {
        let result = v.test_result_string();
        if let Some(target) = target {
            let target = ECMAScriptValue::Object(target.clone()).test_result_string();
            format!("{result}; {target}")
        } else {
            result
        }
    })
}

#[test_case(Vec::new => serr("TypeError: Reflect.deleteProperty: target must be an object"); "no args")]
#[test_case(
    || vec![
        ECMAScriptValue::Object(ordinary_object_create(None)),
        ECMAScriptValue::Object(DeadObject::object())
    ]
    => serr("TypeError: get called on DeadObject");
    "ToPropertyKey fails"
)]
#[test_case(
    || vec![
        ECMAScriptValue::Object(DeadObject::object()),
        ECMAScriptValue::from("key")
    ]
    => serr("TypeError: delete called on DeadObject");
    "[[Delete]] fails"
)]
#[test_case(
    || vec![
        ECMAScriptValue::Object({
            let o = ordinary_object_create(None);
            define_property_or_throw(
                &o,
                "key",
                PotentialPropertyDescriptor::new()
                    .value(11)
                    .writable(false)
                    .enumerable(true)
                    .configurable(false)
            ).unwrap();
            o
        }),
        ECMAScriptValue::from("key")
    ]
    => sok("false; key:11");
    "delete unsuccessful"
)]
#[test_case(
    || vec![
        ECMAScriptValue::Object({
            let o = ordinary_object_create(None);
            o.create_data_property("first", 1).unwrap();
            o.create_data_property("second", 2).unwrap();
            o
        }),
        ECMAScriptValue::from("first")
    ]
    => sok("true; second:2");
    "delete successful"
)]
fn reflect_delete_property(make_args: impl FnOnce() -> Vec<ECMAScriptValue>) -> Result<String, String> {
    setup_test_agent();
    let args = make_args();
    let target = if args.is_empty() {
        None
    } else if let ECMAScriptValue::Object(obj) = &args[0] {
        Some(obj)
    } else {
        None
    };
    super::reflect_delete_property(&ECMAScriptValue::Undefined, None, &args).map_err(unwind_any_error).map(|v| {
        let result = v.test_result_string();
        if let Some(target) = target {
            let target = ECMAScriptValue::Object(target.clone()).test_result_string();
            format!("{result}; {target}")
        } else {
            result
        }
    })
}

#[test_case(Vec::new => serr("TypeError: Reflect.get: target must be an object"); "no args")]
#[test_case(
    || vec![
        ECMAScriptValue::Object(ordinary_object_create(None)),
        ECMAScriptValue::Object(DeadObject::object()),
    ]
    => serr("TypeError: get called on DeadObject");
    "toPropertyKey fails"
)]
#[test_case(
    || vec![
        ECMAScriptValue::Object(intrinsic(IntrinsicId::BigIntPrototype)),
        ECMAScriptValue::Symbol(wks(WksId::ToStringTag)),
    ]
    => sok("BigInt");
    "successful get, no receiver"
)]
#[test_case(
    || vec![
        ECMAScriptValue::Object(DeadObject::object()),
        ECMAScriptValue::Symbol(wks(WksId::ToStringTag)),
    ]
    => serr("TypeError: get called on DeadObject");
    "target.o.get fails"
)]
#[test_case(
    || {
        fn behavior(this: &ECMAScriptValue, _new_target: Option<&Object>, _args: &[ECMAScriptValue]) -> Completion<ECMAScriptValue> {
            let obj = to_object(this.clone())?;
            let title = obj.get(&PropertyKey::from("title"))?;
            let result = to_string(title)?.concat(JSString::from(" Bob"));
            Ok(ECMAScriptValue::String(result))
        }
        let func = create_builtin_function(Box::new(behavior), None, 0.0, PropertyKey::from("name"), &[], None, None, None);
        let o = ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)));
        let p = ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)));
        p.create_data_property_or_throw("title", "Mrs.").unwrap();
        o.create_data_property_or_throw("title", "Mr.").unwrap();
        define_property_or_throw(&o, "name", PotentialPropertyDescriptor::new().get(func).configurable(true).enumerable(true)).unwrap();
        vec![
            ECMAScriptValue::Object(o),
            ECMAScriptValue::from("name"),
            ECMAScriptValue::Object(p),
        ]
    }
    => sok("Mrs. Bob");
    "Uses receiver"
)]
fn reflect_get(make_args: impl FnOnce() -> Vec<ECMAScriptValue>) -> Result<String, String> {
    setup_test_agent();
    let args = make_args();
    super::reflect_get(&ECMAScriptValue::Undefined, None, &args)
        .map_err(unwind_any_error)
        .map(|v| v.test_result_string())
}

#[test_case(Vec::new => serr("TypeError: Reflect.getOwnPropertyDescriptor: target must be an object"); "empty args")]
#[test_case(
    || {
        let proto = intrinsic(IntrinsicId::ObjectPrototype);
        let obj = ordinary_object_create(Some(proto));
        vec![ECMAScriptValue::Object(obj), ECMAScriptValue::Object(DeadObject::object())]
    }
    => serr("TypeError: get called on DeadObject");
    "toPropertyKey throws"
)]
#[test_case(
    || vec![ECMAScriptValue::Object(DeadObject::object()), ECMAScriptValue::from("bob")]
    => serr("TypeError: get_own_property called on DeadObject");
    "[[getOwnProperty]] throws"
)]
#[test_case(
    || {
        let proto = intrinsic(IntrinsicId::ObjectPrototype);
        let obj = ordinary_object_create(Some(proto));
        obj.create_data_property_or_throw("test-key", "test-value").unwrap();
        vec![ECMAScriptValue::Object(obj), ECMAScriptValue::from("test-key")]
    }
    => sok("value:test-value,writable:true,enumerable:true,configurable:true");
    "successful retrieval"
)]
#[test_case(
    || {
        let proto = intrinsic(IntrinsicId::ObjectPrototype);
        proto.create_data_property_or_throw("test-key", "test-value").unwrap();
        let obj = ordinary_object_create(Some(proto));
        vec![ECMAScriptValue::Object(obj), ECMAScriptValue::from("test-key")]
    }
    => sok("undefined");
    "unsuccessful retrieval"
)]
fn reflect_get_own_property_descriptor(make_args: impl FnOnce() -> Vec<ECMAScriptValue>) -> Result<String, String> {
    setup_test_agent();
    let args = make_args();
    super::reflect_get_own_property_descriptor(&ECMAScriptValue::Undefined, None, &args)
        .map_err(unwind_any_error)
        .map(|v| v.test_result_string())
}

#[test_case(Vec::new => serr("TypeError: Reflect.getPrototypeOf: target must be an object"); "empty args")]
#[test_case(
    || vec![ECMAScriptValue::Object(DeadObject::object())]
    => serr("TypeError: get_prototype_of called on DeadObject");
    "[[getPrototypeOf]] throws"
)]
#[test_case(
    || vec![ECMAScriptValue::Object(ordinary_object_create(None))]
    => sok("null");
    "null prototype"
)]
#[test_case(
    || {
        let proto = ordinary_object_create(None);
        proto.create_data_property_or_throw("test-key", "test-value").unwrap();
        vec![ECMAScriptValue::Object(ordinary_object_create(Some(proto)))]
    }
    => sok("test-key:test-value");
    "some prototype"
)]
fn reflect_get_prototype_of(make_args: impl FnOnce() -> Vec<ECMAScriptValue>) -> Result<String, String> {
    setup_test_agent();
    let args = make_args();
    super::reflect_get_prototype_of(&ECMAScriptValue::Undefined, None, &args)
        .map_err(unwind_any_error)
        .map(|v| v.test_result_string())
}

#[test_case(Vec::new => serr("TypeError: Reflect.has: target must be an object"); "empty args")]
#[test_case(
    || vec![
        ECMAScriptValue::Object(intrinsic(IntrinsicId::Object)),
        ECMAScriptValue::Object(DeadObject::object())
    ]
    => serr("TypeError: get called on DeadObject");
    "toPropertyKey throws"
)]
#[test_case(
    || vec![
        ECMAScriptValue::Object(DeadObject::object()),
        ECMAScriptValue::from("test-key")
    ]
    => serr("TypeError: has_property called on DeadObject");
    "[[hasProperty]] throws"
)]
#[test_case(
    || vec![
        ECMAScriptValue::Object(intrinsic(IntrinsicId::Object)),
        ECMAScriptValue::from("prototype")
    ]
    => sok("true");
    "property found"
)]
#[test_case(
    || vec![
        ECMAScriptValue::Object(intrinsic(IntrinsicId::Object)),
        ECMAScriptValue::from("test-key")
    ]
    => sok("false");
    "property not found"
)]
fn reflect_has(make_args: impl FnOnce() -> Vec<ECMAScriptValue>) -> Result<String, String> {
    setup_test_agent();
    let args = make_args();
    super::reflect_has(&ECMAScriptValue::Undefined, None, &args)
        .map_err(unwind_any_error)
        .map(|v| v.test_result_string())
}

#[test_case(Vec::new => serr("TypeError: Reflect.isExtensible: target must be an object"); "empty args")]
#[test_case(
    || vec![ECMAScriptValue::Object(DeadObject::object())]
    => serr("TypeError: is_extensible called on DeadObject");
    "[[isExtensible]] throws"
)]
#[test_case(
    || vec![ECMAScriptValue::Object(intrinsic(IntrinsicId::Object))]
    => sok("true");
    "object is extensible"
)]
#[test_case(
    || vec![{
        let o = ordinary_object_create(None);
        o.o.prevent_extensions().unwrap();
        ECMAScriptValue::Object(o)
    }]
    => sok("false");
    "object is frozen"
)]
fn reflect_is_extensible(make_args: impl FnOnce() -> Vec<ECMAScriptValue>) -> Result<String, String> {
    setup_test_agent();
    let args = make_args();
    super::reflect_is_extensible(&ECMAScriptValue::Undefined, None, &args)
        .map_err(unwind_any_error)
        .map(|v| v.test_result_string())
}

#[test_case(Vec::new => serr("TypeError: Reflect.ownKeys: target must be an object"); "empty args")]
#[test_case(
    || vec![ECMAScriptValue::Object(DeadObject::object())]
    => serr("TypeError: own_property_keys called on DeadObject");
    "[[OwnPropertyKeys]] throws"
)]
#[test_case(
    || vec![{
        let o = ordinary_object_create(None);
        o.create_data_property_or_throw("test-key-1", "test-value-1").unwrap();
        o.create_data_property_or_throw("test-key-2", "test-value-2").unwrap();
        ECMAScriptValue::Object(o)
    }]
    => sok("0:test-key-1,1:test-key-2,length:2");
    "success"
)]
fn reflect_own_keys(make_args: impl FnOnce() -> Vec<ECMAScriptValue>) -> Result<String, String> {
    setup_test_agent();
    let args = make_args();
    super::reflect_own_keys(&ECMAScriptValue::Undefined, None, &args)
        .map_err(unwind_any_error)
        .map(|v| v.test_result_string())
}

#[test_case(Vec::new => serr("TypeError: Reflect.preventExtensions: target must be an object"); "empty args")]
#[test_case(
    || vec![ECMAScriptValue::Object(DeadObject::object())]
    => serr("TypeError: prevent_extensions called on DeadObject");
    "[[PreventExtensions]] throws"
)]
#[test_case(
    || vec![ECMAScriptValue::Object(intrinsic(IntrinsicId::Object))]
    => sok("true");
    "success"
)]
fn reflect_prevent_extensions(make_args: impl FnOnce() -> Vec<ECMAScriptValue>) -> Result<String, String> {
    setup_test_agent();
    let args = make_args();
    super::reflect_prevent_extensions(&ECMAScriptValue::Undefined, None, &args)
        .map_err(unwind_any_error)
        .map(|v| v.test_result_string())
}

#[test_case(Vec::new => serr("TypeError: Reflect.set: target must be an object"); "empty args")]
#[test_case(
    || vec![
        ECMAScriptValue::Object(intrinsic(IntrinsicId::Object)),
        ECMAScriptValue::Object(DeadObject::object())
    ]
    => serr("TypeError: get called on DeadObject");
    "ToPropertyKey throws"
)]
#[test_case(
    || vec![ECMAScriptValue::Object(DeadObject::object())]
    => serr("TypeError: set called on DeadObject");
    "[[Set]] throws"
)]
#[test_case(
    || vec![
        ECMAScriptValue::Object(ordinary_object_create(None)),
        ECMAScriptValue::from("test-key"),
        ECMAScriptValue::from("test-value")
    ]
    => sok("true; test-key:test-value");
    "works ok"
)]
#[test_case(
    || vec![
        {
            let o = ordinary_object_create(None);
            o.o.prevent_extensions().unwrap();
            ECMAScriptValue::Object(o)
        },
        ECMAScriptValue::from("test-key"),
        ECMAScriptValue::from("test-value")
    ]
    => sok("false; ");
    "object has extensions prevented"
)]
#[test_case(
    || {
        fn behavior(this: &ECMAScriptValue, _new_target: Option<&Object>, args: &[ECMAScriptValue]) -> Completion<ECMAScriptValue> {
            let mut args = FuncArgs::from(args);
            let value = args.next_arg();
            let obj = to_object(this.clone())?;
            let title = obj.get(&PropertyKey::from("title"))?;
            let result = to_string(title)?.concat(JSString::from(" ")).concat(to_string(value)?);
            obj.create_data_property_or_throw("test-report", result)?;
            Ok(ECMAScriptValue::Boolean(true))
        }
        let func = create_builtin_function(Box::new(behavior), None, 0.0, PropertyKey::from("name"), &[], None, None, None);
        let o = ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)));
        let p = ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)));
        p.create_data_property_or_throw("title", "Mrs.").unwrap();
        o.create_data_property_or_throw("title", "Mr.").unwrap();
        define_property_or_throw(&o, "name", PotentialPropertyDescriptor::new().set(func).configurable(true).enumerable(true)).unwrap();
        vec![
            ECMAScriptValue::Object(o),
            ECMAScriptValue::from("name"),
            ECMAScriptValue::from("name-value"),
            ECMAScriptValue::Object(p),
        ]
    }
    => sok("true; title:Mr.,name:undefined; title:Mrs.,test-report:Mrs. name-value");
    "Uses receiver"
)]
fn reflect_set(make_args: impl FnOnce() -> Vec<ECMAScriptValue>) -> Result<String, String> {
    setup_test_agent();
    let args = make_args();
    let target = if args.is_empty() {
        None
    } else if let ECMAScriptValue::Object(obj) = &args[0] {
        Some(obj)
    } else {
        None
    };
    let receiver = if args.len() < 4 {
        None
    } else if let ECMAScriptValue::Object(obj) = &args[3] {
        Some(obj)
    } else {
        None
    };
    super::reflect_set(&ECMAScriptValue::Undefined, None, &args).map_err(unwind_any_error).map(|v| {
        let result = v.test_result_string();
        match (target, receiver) {
            (Some(target), Some(receiver)) => {
                let target = ECMAScriptValue::Object(target.clone()).test_result_string();
                let receiver = ECMAScriptValue::Object(receiver.clone()).test_result_string();
                format!("{result}; {target}; {receiver}")
            }
            (Some(target), None) => {
                let target = ECMAScriptValue::Object(target.clone()).test_result_string();
                format!("{result}; {target}")
            }
            (None, Some(_)) => {
                panic!("receiver with no target!");
            }
            (None, None) => result,
        }
    })
}

#[test_case(Vec::new => serr("TypeError: Reflect.setPrototypeOf: target must be an object"); "empty args")]
#[test_case(
    || vec![
        ECMAScriptValue::Object(intrinsic(IntrinsicId::Object)),
        ECMAScriptValue::Undefined
    ]
    => serr("TypeError: Reflect.setPrototypeOf: proto must be an object or null");
    "bad proto"
)]
#[test_case(
    || vec![
        ECMAScriptValue::Object(DeadObject::object()),
        ECMAScriptValue::Null
    ]
    => serr("TypeError: set_prototype_of called on DeadObject");
    "[[SetPrototypeOf]] throws"
)]
#[test_case(
    || vec![
        ECMAScriptValue::Object(ordinary_object_create(None)),
        ECMAScriptValue::Object(intrinsic(IntrinsicId::StringPrototype))
    ]
    => sok("true; StringPrototype");
    "set to string prototype"
)]
#[test_case(
    || vec![
        ECMAScriptValue::Object(ordinary_object_create(Some(intrinsic(IntrinsicId::BooleanPrototype)))),
        ECMAScriptValue::Null
    ]
    => sok("true; null");
    "clear prototype"
)]
#[test_case(
    || vec![
        {
            let o = ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)));
            o.o.prevent_extensions().unwrap();
            ECMAScriptValue::Object(o)
        },
        ECMAScriptValue::Object(intrinsic(IntrinsicId::StringPrototype))
    ]
    => sok("false; ObjectPrototype");
    "unable to set proto"
)]
fn reflect_set_prototype_of(make_args: impl FnOnce() -> Vec<ECMAScriptValue>) -> Result<String, String> {
    setup_test_agent();
    let args = make_args();
    let target = if args.is_empty() {
        None
    } else if let ECMAScriptValue::Object(obj) = &args[0] {
        Some(obj)
    } else {
        None
    };
    super::reflect_set_prototype_of(&ECMAScriptValue::Undefined, None, &args).map_err(unwind_any_error).map(|v| {
        let result = v.test_result_string();
        if let Some(obj) = target {
            let target_proto = obj.o.get_prototype_of().unwrap();
            let proto_descriptor = if let Some(proto) = &target_proto {
                let id = proto.which_intrinsic();
                if let Some(which) = id {
                    format!("{which:?}")
                } else {
                    String::from("Not Intrinsic")
                }
            } else {
                String::from("null")
            };
            format!("{result}; {proto_descriptor}")
        } else {
            result
        }
    })
}
