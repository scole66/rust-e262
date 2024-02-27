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

#[test_case(|| (ordinary_object_create(None, &[]).into(), ECMAScriptValue::Undefined, ECMAScriptValue::Undefined) => serr("TypeError: Reflect.apply requires a callable target"); "not callable")]
#[test_case(|| (intrinsic(IntrinsicId::IsNaN).into(), ECMAScriptValue::Undefined, create_array_from_list(&[100.into()]).into()) => sok("false"); "successful; without this")]
#[test_case(|| (intrinsic(IntrinsicId::IsNaN).into(), ECMAScriptValue::Undefined, ECMAScriptValue::Null) => serr("TypeError: CreateListFromArrayLike called on non-object"); "bad args")]
fn reflect_apply(make_args: impl FnOnce() -> (ECMAScriptValue, ECMAScriptValue, ECMAScriptValue)) -> Result<String, String> {
    setup_test_agent();
    let (target, this_argument, argument_list) = make_args();
    super::reflect_apply(&ECMAScriptValue::Undefined, None, &[target, this_argument, argument_list])
        .map_err(unwind_any_error)
        .map(|v| v.test_result_string())
}
