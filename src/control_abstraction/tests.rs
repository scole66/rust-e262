use super::*;
use crate::tests::*;
use ahash::AHashMap;
use test_case::test_case;

#[test]
fn provision_iterator_prototype() {
    setup_test_agent();

    let iterator_prototype = intrinsic(IntrinsicId::IteratorPrototype);
    let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);

    let iter_proto_proto = iterator_prototype.o.common_object_data().borrow().prototype.as_ref().unwrap().clone();
    assert_eq!(iter_proto_proto, object_prototype);

    let iterator_sym = wks(WksId::Iterator);
    let iterator_desc = IdealizedPropertyDescriptor::from(
        iterator_prototype.o.get_own_property(&iterator_sym.into()).unwrap().unwrap(),
    );
    assert_eq!(iterator_desc.configurable, true);
    assert_eq!(iterator_desc.enumerable, false);
    assert_eq!(iterator_desc.writable, Some(true));
    assert!(iterator_desc.get.is_none());
    assert!(iterator_desc.set.is_none());
    if let Some(val) = iterator_desc.value {
        assert!(is_callable(&val));
    } else {
        panic!("Expected a value");
    }
}

mod provision_generator_function_intrinsics {
    use super::*;
    use test_case::test_case;

    #[test]
    fn generator_function() {
        setup_test_agent();
        let function = intrinsic(IntrinsicId::Function);
        let generator_function = intrinsic(IntrinsicId::GeneratorFunction);

        assert!(generator_function.o.to_builtin_function_obj().is_some());
        assert_eq!(generator_function.o.common_object_data().borrow().prototype.as_ref().unwrap().clone(), function);

        assert_eq!(get(&generator_function, &"name".into()).unwrap(), ECMAScriptValue::from("GeneratorFunction"));
        assert_eq!(get(&generator_function, &"length".into()).unwrap(), ECMAScriptValue::from(1));
        assert_eq!(
            get(&generator_function, &"prototype".into()).unwrap(),
            ECMAScriptValue::from(intrinsic(IntrinsicId::GeneratorFunctionPrototype))
        );
    }

    #[test]
    fn generator_function_prototype() {
        setup_test_agent();
        let generator_function_prototype = intrinsic(IntrinsicId::GeneratorFunctionPrototype);
        let function_prototype = intrinsic(IntrinsicId::FunctionPrototype);

        assert!(!generator_function_prototype.o.is_callable_obj());
        assert_eq!(
            generator_function_prototype.o.common_object_data().borrow().prototype.as_ref().unwrap().clone(),
            function_prototype
        );

        let to_string_tag = wks(WksId::ToStringTag);
        assert_eq!(
            get(&generator_function_prototype, &to_string_tag.into()).unwrap(),
            ECMAScriptValue::from("GeneratorFunction")
        );
        assert_eq!(
            get(&generator_function_prototype, &"constructor".into()).unwrap(),
            ECMAScriptValue::from(intrinsic(IntrinsicId::GeneratorFunction))
        );
        assert_eq!(
            get(&generator_function_prototype, &"prototype".into()).unwrap(),
            ECMAScriptValue::from(intrinsic(IntrinsicId::GeneratorFunctionPrototypePrototype))
        );
    }

    #[test]
    fn generator_prototype() {
        setup_test_agent();
        let generator_prototype = intrinsic(IntrinsicId::GeneratorFunctionPrototypePrototype);

        assert_eq!(
            generator_prototype.o.common_object_data().borrow().prototype.as_ref().unwrap().clone(),
            intrinsic(IntrinsicId::IteratorPrototype)
        );
        assert_eq!(
            get(&generator_prototype, &"constructor".into()).unwrap(),
            ECMAScriptValue::from(intrinsic(IntrinsicId::GeneratorFunctionPrototype))
        );
        let to_string_tag = wks(WksId::ToStringTag);
        assert_eq!(get(&generator_prototype, &to_string_tag.into()).unwrap(), ECMAScriptValue::from("Generator"));
    }

    #[test_case("next" => "next;1"; "next function")]
    #[test_case("return" => "return;1"; "return function")]
    #[test_case("throw" => "throw;1"; "throw function")]
    fn generator_prototype_func(key: &str) -> String {
        setup_test_agent();
        let key = PropertyKey::from(key);
        let proto = intrinsic(IntrinsicId::GeneratorFunctionPrototypePrototype);
        let val = super::get(&proto, &key).unwrap();
        assert!(is_callable(&val));
        let name = getv(&val, &"name".into()).unwrap();
        let name = to_string(name).unwrap();
        let length = getv(&val, &"length".into()).unwrap();
        let length = to_string(length).unwrap();
        format!("{};{}", String::from(name), length)
    }
}

#[test_case("a value".into(), true; "is done")]
#[test_case(ECMAScriptValue::Undefined, false; "not done")]
fn create_iter_result_object(value: ECMAScriptValue, done: bool) {
    setup_test_agent();
    let obj = super::create_iter_result_object(value.clone(), done);
    let value_res = get(&obj, &"value".into()).unwrap();
    let done_res = get(&obj, &"done".into()).unwrap();

    assert_eq!(value_res, value);
    assert_eq!(done_res, ECMAScriptValue::from(done));
}

#[test_case(|| ECMAScriptValue::Undefined, "" => serr("TypeError: Generator required"); "not an object")]
#[test_case(|| create_string_object("blue".into()).into(), "" => serr("TypeError: Generator required"); "not a generator")]
#[test_case(|| {
        let proto = intrinsic(IntrinsicId::GeneratorFunctionPrototypePrototype);
        GeneratorObject::object(Some(proto), GeneratorState::SuspendedStart, "TestingBrand").into()
    }, "TestingBrand" => Ok(GeneratorState::SuspendedStart); "valid")]
#[test_case(|| {
        let proto = intrinsic(IntrinsicId::GeneratorFunctionPrototypePrototype);
        GeneratorObject::object(Some(proto), GeneratorState::Executing, "TestingBrand").into()
    }, "TestingBrand" => serr("TypeError: Generator is already executing"); "already running")]
#[test_case(|| {
        let proto = intrinsic(IntrinsicId::GeneratorFunctionPrototypePrototype);
        GeneratorObject::object(Some(proto), GeneratorState::SuspendedStart, "TestingBrand").into()
    }, "OtherBrand" => serr("TypeError: Generator brand mismatch"); "brand mismatch")]
fn generator_validate(
    make_value: impl FnOnce() -> ECMAScriptValue,
    desired_brand: &str,
) -> Result<GeneratorState, String> {
    setup_test_agent();
    let value = make_value();
    super::generator_validate(value, desired_brand).map_err(unwind_any_error)
}

#[test_case(|| ECMAScriptValue::Undefined => Ok(ECMAScriptValue::Undefined); "pass-thru/undefined")]
#[test_case(|| ECMAScriptValue::from(67) => Ok(ECMAScriptValue::from(67)); "pass-thru/number")]
fn iterator_prototype_iterator(make_params: impl FnOnce() -> ECMAScriptValue) -> Result<ECMAScriptValue, String> {
    setup_test_agent();
    let this_value = make_params();
    super::iterator_prototype_iterator(this_value, None, &[]).map_err(unwind_any_error)
}

tbd_function!(generator_function);
tbd_function!(generator_prototype_return);
tbd_function!(generator_prototype_throw);

mod generator_prototype_next {
    use super::*;
    use test_case::test_case;

    fn list_iterator_sample() -> (ECMAScriptValue, Vec<ECMAScriptValue>) {
        let ir = super::super::create_list_iterator_record(vec![ECMAScriptValue::from(10), ECMAScriptValue::from(2)]);
        (ir.iterator.into(), vec![ECMAScriptValue::Undefined])
    }

    #[test_case(|| (ECMAScriptValue::Undefined, vec![ECMAScriptValue::Undefined]) => serr("TypeError: Generator required"); "not-a-generator")]
    #[test_case(list_iterator_sample => Ok(ECMAScriptValue::from(10)); "generator")]
    fn once(make_params: impl FnOnce() -> (ECMAScriptValue, Vec<ECMAScriptValue>)) -> Result<ECMAScriptValue, String> {
        setup_test_agent();
        let (this_value, arguments) = make_params();
        let result_obj =
            super::generator_prototype_next(this_value, None, arguments.as_slice()).map_err(unwind_any_error)?;
        getv(&result_obj, &"value".into()).map_err(unwind_any_error)
    }

    #[test]
    pub fn multi() {
        setup_test_agent();
        let (this_value, _) = list_iterator_sample();
        let mut results = vec![];
        loop {
            let it_obj = super::generator_prototype_next(this_value.clone(), None, &[]).unwrap();
            let done = getv(&it_obj, &"done".into()).unwrap();
            if bool::from(done) {
                break;
            }
            let val = getv(&it_obj, &"value".into()).unwrap();
            results.push(val.clone());
        }
        assert_eq!(results.len(), 2);
        assert_eq!(results[0], ECMAScriptValue::from(10));
        assert_eq!(results[1], ECMAScriptValue::from(2));
    }
}

mod list_iterator {
    use super::*;

    // It's impossible to call the list_iterator function directly, as it's a generator. So this leverages
    // other tests and functions to get coverage, as they all use the list_iterator as the "piece of thing in
    // the test".

    #[test]
    fn happypath() {
        generator_prototype_next::multi()
    }

    #[test]
    fn cancelled_yield() {
        setup_test_agent();
        let ir = super::super::create_list_iterator_record(vec![ECMAScriptValue::from(10), ECMAScriptValue::from(2)]);
        let this_value = ECMAScriptValue::from(ir.iterator);
        let ac = AbruptCompletion::Return { value: ECMAScriptValue::from("return token") };
        generator_resume(this_value.clone(), ECMAScriptValue::Undefined, "").unwrap();
        let res = generator_resume_abrupt(this_value, ac, "").unwrap();
        let val = getv(&res, &"value".into()).unwrap();
        assert_eq!(val, "return token".into());
    }
}

#[test]
fn create_list_iterator_record() {
    setup_test_agent();
    let ir = super::create_list_iterator_record(vec![
        ECMAScriptValue::from(0),
        ECMAScriptValue::from(10),
        ECMAScriptValue::from(20),
    ]);
    // done must be false...
    assert!(!ir.done);
    // next_method must be the appropriate intrinsic...
    assert_eq!(ir.next_method, intrinsic(IntrinsicId::GeneratorFunctionPrototypePrototypeNext));
    // and iterator must be a generator object that returns the above list.
    assert!(ir.iterator.o.to_generator_object().is_some());
    let mut results = vec![];
    loop {
        let item = generator_resume(ir.iterator.clone().into(), ECMAScriptValue::Undefined, "").unwrap();
        let done = getv(&item, &"done".into()).unwrap();
        if bool::from(done) {
            break;
        }
        let value = getv(&item, &"value".into()).unwrap();
        results.push(value);
    }
    assert_eq!(results.len(), 3);
    assert_eq!(results[0], ECMAScriptValue::from(0));
    assert_eq!(results[1], ECMAScriptValue::from(10));
    assert_eq!(results[2], ECMAScriptValue::from(20));
}

mod generator_state {
    use super::*;
    use test_case::test_case;

    #[test]
    fn debug() {
        assert_ne!(format!("{:?}", GeneratorState::Executing), "");
    }

    #[test_case(GeneratorState::SuspendedStart, GeneratorState::Executing => false; "not equal")]
    #[test_case(GeneratorState::SuspendedStart, GeneratorState::SuspendedStart => true; "equal")]
    fn eq(left: GeneratorState, right: GeneratorState) -> bool {
        left == right
    }

    #[test]
    #[allow(clippy::clone_on_copy)]
    fn clone() {
        let left = GeneratorState::Executing;
        let right = left.clone();
        assert_eq!(left, right);
    }
}

mod generator_data {
    use super::*;

    #[test]
    fn debug() {
        let item = GeneratorData {
            generator_state: GeneratorState::Completed,
            generator_context: None,
            generator_brand: "Testerino".to_owned(),
        };
        assert_ne!(format!("{:?}", item), "");
    }
}

mod generator_error {
    use super::*;
    use test_case::test_case;

    #[test]
    fn debug() {
        assert_ne!(format!("{:?}", GeneratorError::AlreadyActive), "");
    }

    #[test_case(GeneratorError::BrandMismatch => "Generator brand mismatch"; "BrandMismatch")]
    #[test_case(GeneratorError::AlreadyActive => "Generator is already executing"; "AlreadyActive")]
    #[test_case(GeneratorError::NotAGenerator => "Generator required"; "NotAGenerator")]
    fn display(item: GeneratorError) -> String {
        item.to_string()
    }

    #[test_case(GeneratorError::BrandMismatch, GeneratorError::BrandMismatch => true; "equal")]
    #[test_case(GeneratorError::NotAGenerator, GeneratorError::BrandMismatch => false; "not equal")]
    fn eq(left: GeneratorError, right: GeneratorError) -> bool {
        left == right
    }
}

mod generator_object {
    use super::*;
    use test_case::test_case;

    #[test]
    fn object() {
        setup_test_agent();
        let gp = intrinsic(IntrinsicId::GeneratorFunctionPrototypePrototype);
        let o = GeneratorObject::object(Some(gp.clone()), GeneratorState::Undefined, "TestingBrand");

        let go_data = o.o.to_generator_object().unwrap().generator_data.borrow();

        assert_eq!(go_data.generator_state, GeneratorState::Undefined);
        assert!(go_data.generator_context.is_none());
        assert_eq!(go_data.generator_brand, "TestingBrand");

        let cod = o.o.common_object_data().borrow();
        assert_eq!(cod.prototype.as_ref().unwrap().clone(), gp);
        assert!(cod.extensible);
        assert!(cod.properties.is_empty());
        assert_eq!(cod.slots, GENERATOR_OBJECT_SLOTS);
        assert!(cod.private_elements.is_empty());
    }

    fn make() -> Object {
        let gp = intrinsic(IntrinsicId::GeneratorFunctionPrototypePrototype);
        GeneratorObject::object(Some(gp), GeneratorState::Undefined, "TestingBrand")
    }

    false_function!(is_boolean_object);
    false_function!(is_string_object);
    false_function!(is_proxy_object);
    false_function!(is_symbol_object);
    false_function!(is_date_object);
    false_function!(is_plain_object);
    false_function!(is_arguments_object);
    false_function!(is_callable_obj);
    false_function!(is_error_object);
    false_function!(is_number_object);
    false_function!(is_regexp_object);
    false_function!(is_array_object);
    none_function!(to_number_obj);
    none_function!(to_error_obj);
    none_function!(to_symbol_obj);
    none_function!(to_function_obj);
    none_function!(to_callable_obj);
    none_function!(to_constructable);
    none_function!(to_builtin_function_obj);
    none_function!(to_arguments_object);
    none_function!(to_string_obj);
    none_function!(to_array_object);
    none_function!(to_boolean_obj);

    #[test]
    fn debug() {
        setup_test_agent();
        let obj = make();
        let go = obj.o.to_generator_object().unwrap();
        assert_ne!(format!("{:?}", go), "");
    }

    #[test]
    fn common_object_data() {
        setup_test_agent();
        let obj = make();
        super::set(&obj, "test".into(), "sentinel".into(), true).unwrap();

        let cod = obj.o.common_object_data().borrow();
        assert_eq!(cod.properties.len(), 1);
        assert!(cod.properties.contains_key(&"test".into()));
    }

    default_get_prototype_of_test!(GeneratorFunctionPrototypePrototype);
    default_set_prototype_of_test!();
    default_is_extensible_test!();
    default_prevent_extensions_test!();
    default_delete_test!();
    default_id_test!();
    default_has_property_test!();
    default_is_ordinary_test!();
    default_get_own_property_test!();
    default_define_own_property_test!();
    default_get_test!(|| wks(WksId::ToStringTag).into(), ECMAScriptValue::from("Generator"));
    default_set_test!();
    default_own_property_keys_test!();

    #[test]
    fn is_generator_object() {
        setup_test_agent();
        let obj = make();
        assert!(obj.o.is_generator_object());
    }

    #[test]
    fn to_generator_object() {
        setup_test_agent();
        let obj = make();
        let go = obj.o.to_generator_object();
        assert!(go.is_some());
    }

    #[test_case("TestingBrand", GeneratorState::SuspendedStart => Ok(GeneratorState::SuspendedStart); "simple")]
    #[test_case("OtherBrand", GeneratorState::SuspendedStart => Err(GeneratorError::BrandMismatch); "brand mismatch")]
    #[test_case("TestingBrand", GeneratorState::Executing => Err(GeneratorError::AlreadyActive); "already active")]
    fn validate(desired_brand: &str, generator_state: GeneratorState) -> Result<GeneratorState, GeneratorError> {
        setup_test_agent();
        let obj = make(); // brand is "TestingBrand"
        let gen_obj = obj.o.to_generator_object().unwrap();
        gen_obj.generator_data.borrow_mut().generator_state = generator_state;

        gen_obj.validate(desired_brand)
    }
}

mod create_iterator_from_closure {
    use super::*;

    async fn goofy_values(co: Co<ECMAScriptValue, Completion<ECMAScriptValue>>) -> Completion<ECMAScriptValue> {
        let val = ECMAScriptValue::from(create_iter_result_object(ECMAScriptValue::from("Once, []"), false));
        let res = generator_yield(&co, val).await?;
        let res_str = format!("Twice, [{}]", to_string(res).unwrap());
        let val = ECMAScriptValue::from(create_iter_result_object(ECMAScriptValue::from(res_str), false));
        let res = generator_yield(&co, val).await?;
        let res_str = format!("Three times a lady, [{}]", to_string(res).unwrap());
        let val = ECMAScriptValue::from(create_iter_result_object(ECMAScriptValue::from(res_str), false));
        let res = generator_yield(&co, val).await?;
        Ok(res)
    }

    #[test]
    fn happy() {
        setup_test_agent();
        let iter = ECMAScriptValue::from(create_iterator_from_closure(asyncfn_wrap(goofy_values), "", None));
        let mut results = vec![];
        let mut iter_index = 1;
        loop {
            let item = generator_resume(iter.clone(), ECMAScriptValue::from(iter_index), "").unwrap();
            iter_index += 1;
            let done = getv(&item, &"done".into()).unwrap();
            let value = getv(&item, &"value".into()).unwrap();
            results.push(value);
            if bool::from(done) {
                break;
            }
        }
        assert_eq!(results.len(), 4);
        assert_eq!(results[0], ECMAScriptValue::from("Once, []"));
        assert_eq!(results[1], ECMAScriptValue::from("Twice, [2]"));
        assert_eq!(results[2], ECMAScriptValue::from("Three times a lady, [3]"));
        assert_eq!(results[3], ECMAScriptValue::Undefined);
    }
}

mod generator_resume {
    use super::*;

    async fn gen_vals(co: Co<ECMAScriptValue, Completion<ECMAScriptValue>>) -> Completion<ECMAScriptValue> {
        let val = ECMAScriptValue::from(create_iter_result_object(ECMAScriptValue::from("Token 1"), false));
        let res = generator_yield(&co, val).await?;
        let res_str = format!("Token 2: [{}]", to_string(res).unwrap());
        let val = ECMAScriptValue::from(create_iter_result_object(ECMAScriptValue::from(res_str), false));
        let res = generator_yield(&co, val).await?;
        Ok(res)
    }

    #[test]
    fn invalid() {
        setup_test_agent();
        let iter = ECMAScriptValue::from(create_iterator_from_closure(asyncfn_wrap(gen_vals), "", None));
        let item = generator_resume(iter, ECMAScriptValue::from(632), "bogus").unwrap_err();
        assert_eq!(unwind_any_error(item), String::from("TypeError: Generator brand mismatch"));
    }

    #[test]
    fn suspended_start() {
        setup_test_agent();
        let iter = ECMAScriptValue::from(create_iterator_from_closure(asyncfn_wrap(gen_vals), "", None));
        let item = generator_resume(iter, ECMAScriptValue::from(987), "").unwrap();

        assert_eq!(getv(&item, &"value".into()).unwrap(), ECMAScriptValue::from("Token 1"));
        assert_eq!(getv(&item, &"done".into()).unwrap(), ECMAScriptValue::from(false));
    }

    #[test]
    fn suspended_yield() {
        setup_test_agent();
        let iter = ECMAScriptValue::from(create_iterator_from_closure(asyncfn_wrap(gen_vals), "", None));
        generator_resume(iter.clone(), ECMAScriptValue::from(813), "").unwrap();
        let item = generator_resume(iter, ECMAScriptValue::from(123), "").unwrap();

        assert_eq!(getv(&item, &"value".into()).unwrap(), ECMAScriptValue::from("Token 2: [123]"));
        assert_eq!(getv(&item, &"done".into()).unwrap(), ECMAScriptValue::from(false));
    }

    #[test]
    fn first_completion() {
        setup_test_agent();
        let iter = ECMAScriptValue::from(create_iterator_from_closure(asyncfn_wrap(gen_vals), "", None));
        generator_resume(iter.clone(), ECMAScriptValue::from(523), "").unwrap();
        generator_resume(iter.clone(), ECMAScriptValue::from(81), "").unwrap();
        let item = generator_resume(iter, ECMAScriptValue::from(6581), "").unwrap();

        assert_eq!(getv(&item, &"value".into()).unwrap(), ECMAScriptValue::Undefined);
        assert_eq!(getv(&item, &"done".into()).unwrap(), ECMAScriptValue::from(true));
    }

    #[test]
    fn second_completion() {
        setup_test_agent();
        let iter = ECMAScriptValue::from(create_iterator_from_closure(asyncfn_wrap(gen_vals), "", None));
        generator_resume(iter.clone(), ECMAScriptValue::from(523), "").unwrap();
        generator_resume(iter.clone(), ECMAScriptValue::from(81), "").unwrap();
        generator_resume(iter.clone(), ECMAScriptValue::from(6581), "").unwrap();
        let item = generator_resume(iter, ECMAScriptValue::from(3), "").unwrap();

        assert_eq!(getv(&item, &"value".into()).unwrap(), ECMAScriptValue::Undefined);
        assert_eq!(getv(&item, &"done".into()).unwrap(), ECMAScriptValue::from(true));
    }
}

mod generator_resume_abrupt {
    use super::*;

    async fn gen_vals(co: Co<ECMAScriptValue, Completion<ECMAScriptValue>>) -> Completion<ECMAScriptValue> {
        let val = ECMAScriptValue::from(create_iter_result_object(ECMAScriptValue::from("Token 1"), false));
        let res_full = generator_yield(&co, val).await;
        let res = match res_full {
            Ok(val) => val,
            Err(err) => match &err {
                AbruptCompletion::Throw { value } => {
                    if value == &ECMAScriptValue::from("special") {
                        value.clone()
                    } else {
                        return Err(err);
                    }
                }
                _ => return Err(err),
            },
        };
        let res_str = format!("Token 2: [{}]", to_string(res).unwrap());
        let val = ECMAScriptValue::from(create_iter_result_object(ECMAScriptValue::from(res_str), false));
        let res = generator_yield(&co, val).await?;
        Ok(res)
    }

    #[test]
    fn invalid() {
        setup_test_agent();
        let iter = ECMAScriptValue::from(create_iterator_from_closure(asyncfn_wrap(gen_vals), "", None));
        let ac = AbruptCompletion::Throw { value: ECMAScriptValue::from("Testing Sentinel") };
        let item = generator_resume_abrupt(iter, ac, "bogus").unwrap_err();
        assert_eq!(unwind_any_error(item), String::from("TypeError: Generator brand mismatch"));
    }

    #[test]
    fn suspended_start_throw() {
        setup_test_agent();
        let iter = ECMAScriptValue::from(create_iterator_from_closure(asyncfn_wrap(gen_vals), "", None));
        let ac = AbruptCompletion::Throw { value: ECMAScriptValue::from("Testing Sentinel") };
        let item = generator_resume_abrupt(iter.clone(), ac, "").unwrap_err();
        assert_eq!(unwind_any_error(item), String::from("Testing Sentinel"));
        let obj = Object::try_from(iter).unwrap();
        let gen = obj.o.to_generator_object().unwrap();
        assert_eq!(gen.generator_data.borrow().generator_state, GeneratorState::Completed);
    }

    #[test]
    fn suspended_start_return() {
        setup_test_agent();
        let iter = ECMAScriptValue::from(create_iterator_from_closure(asyncfn_wrap(gen_vals), "", None));
        let ac = AbruptCompletion::Return { value: ECMAScriptValue::from("Testing Sentinel") };
        let item = generator_resume_abrupt(iter.clone(), ac, "").unwrap();
        assert_eq!(getv(&item, &"value".into()).unwrap(), ECMAScriptValue::from("Testing Sentinel"));
        assert_eq!(getv(&item, &"done".into()).unwrap(), ECMAScriptValue::from(true));
        let obj = Object::try_from(iter).unwrap();
        let gen = obj.o.to_generator_object().unwrap();
        assert_eq!(gen.generator_data.borrow().generator_state, GeneratorState::Completed);
    }

    #[test]
    fn completed_throw() {
        setup_test_agent();
        let iter = ECMAScriptValue::from(create_iterator_from_closure(asyncfn_wrap(gen_vals), "", None));
        generator_resume(iter.clone(), ECMAScriptValue::from(523), "").unwrap();
        generator_resume(iter.clone(), ECMAScriptValue::from(81), "").unwrap();
        generator_resume(iter.clone(), ECMAScriptValue::from(711), "").unwrap();

        let ac = AbruptCompletion::Throw { value: ECMAScriptValue::from("Testing Sentinel") };
        let item = generator_resume_abrupt(iter, ac, "").unwrap_err();
        assert_eq!(unwind_any_error(item), String::from("Testing Sentinel"));
    }

    #[test]
    fn completed_return() {
        setup_test_agent();
        let iter = ECMAScriptValue::from(create_iterator_from_closure(asyncfn_wrap(gen_vals), "", None));
        generator_resume(iter.clone(), ECMAScriptValue::from(523), "").unwrap();
        generator_resume(iter.clone(), ECMAScriptValue::from(81), "").unwrap();
        generator_resume(iter.clone(), ECMAScriptValue::from(711), "").unwrap();

        let ac = AbruptCompletion::Return { value: ECMAScriptValue::from("Testing Sentinel") };
        let item = generator_resume_abrupt(iter, ac, "").unwrap();
        assert_eq!(getv(&item, &"value".into()).unwrap(), ECMAScriptValue::from("Testing Sentinel"));
        assert_eq!(getv(&item, &"done".into()).unwrap(), ECMAScriptValue::from(true));
    }

    #[test]
    fn suspended_yield_rethrows() {
        setup_test_agent();
        let iter = ECMAScriptValue::from(create_iterator_from_closure(asyncfn_wrap(gen_vals), "", None));
        generator_resume(iter.clone(), ECMAScriptValue::from(523), "").unwrap();
        let ac = AbruptCompletion::Throw { value: ECMAScriptValue::from("Testing Sentinel") };
        let item = generator_resume_abrupt(iter.clone(), ac, "").unwrap_err();
        assert_eq!(unwind_any_error(item), String::from("Testing Sentinel"));
        let obj = Object::try_from(iter).unwrap();
        let gen = obj.o.to_generator_object().unwrap();
        assert_eq!(gen.generator_data.borrow().generator_state, GeneratorState::Completed);
    }

    #[test]
    fn suspended_yield_handled() {
        setup_test_agent();
        let iter = ECMAScriptValue::from(create_iterator_from_closure(asyncfn_wrap(gen_vals), "", None));
        generator_resume(iter.clone(), ECMAScriptValue::from(523), "").unwrap();
        let ac = AbruptCompletion::Throw { value: ECMAScriptValue::from("special") };
        let item = generator_resume_abrupt(iter.clone(), ac, "").unwrap();
        assert_eq!(getv(&item, &"done".into()).unwrap(), ECMAScriptValue::from(false));
        assert_eq!(getv(&item, &"value".into()).unwrap(), ECMAScriptValue::from("Token 2: [special]"));
        let obj = Object::try_from(iter).unwrap();
        let gen = obj.o.to_generator_object().unwrap();
        assert_eq!(gen.generator_data.borrow().generator_state, GeneratorState::SuspendedYield);
    }
}

mod generator_yield {
    use super::*;

    // generator_yield can only be used inside generator functions, making the testing of it a bit indirect.
    async fn gen_vals(co: Co<ECMAScriptValue, Completion<ECMAScriptValue>>) -> Completion<ECMAScriptValue> {
        let mut val = ECMAScriptValue::from(create_iter_result_object(ECMAScriptValue::from("Primed!"), false));
        loop {
            let res_full = generator_yield(&co, val).await;
            val = ECMAScriptValue::from(create_iter_result_object(
                match res_full {
                    Ok(normal) => ECMAScriptValue::from(to_string(normal).unwrap()),
                    Err(ac) => match ac {
                        AbruptCompletion::Break { .. } | AbruptCompletion::Continue { .. } => unreachable!(),
                        AbruptCompletion::Return { value } => {
                            ECMAScriptValue::from(format!("Return({})", to_string(value).unwrap()))
                        }
                        AbruptCompletion::Throw { value } => {
                            ECMAScriptValue::from(format!("Throw({})", to_string(value).unwrap()))
                        }
                    },
                },
                false,
            ));
        }
    }

    #[test]
    fn normal() {
        setup_test_agent();
        let iter = ECMAScriptValue::from(create_iterator_from_closure(asyncfn_wrap(gen_vals), "", None));
        generator_resume(iter.clone(), ECMAScriptValue::from(523), "").unwrap();
        let item = generator_resume(iter, ECMAScriptValue::from(732), "").unwrap();
        assert_eq!(getv(&item, &"value".into()).unwrap(), ECMAScriptValue::from("732"));
        assert_eq!(getv(&item, &"done".into()).unwrap(), ECMAScriptValue::from(false));
    }

    #[test]
    fn abrupt_return() {
        setup_test_agent();
        let iter = ECMAScriptValue::from(create_iterator_from_closure(asyncfn_wrap(gen_vals), "", None));
        generator_resume(iter.clone(), ECMAScriptValue::from(523), "").unwrap();
        let item = generator_resume_abrupt(iter, AbruptCompletion::Throw { value: ECMAScriptValue::from("olive") }, "")
            .unwrap();
        assert_eq!(getv(&item, &"value".into()).unwrap(), ECMAScriptValue::from("Throw(olive)"));
        assert_eq!(getv(&item, &"done".into()).unwrap(), ECMAScriptValue::from(false));
    }
}

mod gen_caller {
    use super::*;

    async fn multi(co: Co<ECMAScriptValue, Completion<ECMAScriptValue>>) -> Completion<ECMAScriptValue> {
        let val = ECMAScriptValue::from(create_iter_result_object(ECMAScriptValue::from("Token 1"), false));
        generator_yield(&co, val).await?;
        Ok(ECMAScriptValue::Undefined)
    }

    async fn returns_normal(_: Co<ECMAScriptValue, Completion<ECMAScriptValue>>) -> Completion<ECMAScriptValue> {
        Ok(ECMAScriptValue::from(true))
    }

    async fn returns_return(_: Co<ECMAScriptValue, Completion<ECMAScriptValue>>) -> Completion<ECMAScriptValue> {
        Err(AbruptCompletion::Return { value: ECMAScriptValue::Null })
    }

    async fn returns_throw(_: Co<ECMAScriptValue, Completion<ECMAScriptValue>>) -> Completion<ECMAScriptValue> {
        Err(AbruptCompletion::Throw { value: ECMAScriptValue::from("throw") })
    }

    async fn returns_break(_: Co<ECMAScriptValue, Completion<ECMAScriptValue>>) -> Completion<ECMAScriptValue> {
        Err(AbruptCompletion::Break { value: NormalCompletion::Value(ECMAScriptValue::from("break")), target: None })
    }

    async fn returns_continue(_: Co<ECMAScriptValue, Completion<ECMAScriptValue>>) -> Completion<ECMAScriptValue> {
        Err(AbruptCompletion::Continue {
            value: NormalCompletion::Value(ECMAScriptValue::from("continue")),
            target: None,
        })
    }

    #[test]
    fn normal() {
        setup_test_agent();
        let iter = ECMAScriptValue::from(create_iterator_from_closure(asyncfn_wrap(returns_normal), "", None));
        let val = generator_resume(iter, ECMAScriptValue::Undefined, "").unwrap();
        assert_eq!(getv(&val, &"value".into()).unwrap(), ECMAScriptValue::Undefined);
        assert_eq!(getv(&val, &"done".into()).unwrap(), ECMAScriptValue::from(true));
    }

    #[test]
    fn multiple() {
        setup_test_agent();
        let iter = ECMAScriptValue::from(create_iterator_from_closure(asyncfn_wrap(multi), "", None));
        let val = generator_resume(iter.clone(), ECMAScriptValue::Undefined, "").unwrap();
        assert_eq!(getv(&val, &"value".into()).unwrap(), ECMAScriptValue::from("Token 1"));
        assert_eq!(getv(&val, &"done".into()).unwrap(), ECMAScriptValue::from(false));
        let val = generator_resume(iter, ECMAScriptValue::Undefined, "").unwrap();
        assert_eq!(getv(&val, &"value".into()).unwrap(), ECMAScriptValue::Undefined);
        assert_eq!(getv(&val, &"done".into()).unwrap(), ECMAScriptValue::from(true));
    }

    #[test]
    fn r_return() {
        setup_test_agent();
        let iter = ECMAScriptValue::from(create_iterator_from_closure(asyncfn_wrap(returns_return), "", None));
        let val = generator_resume(iter, ECMAScriptValue::Undefined, "").unwrap();
        assert_eq!(getv(&val, &"value".into()).unwrap(), ECMAScriptValue::Null);
        assert_eq!(getv(&val, &"done".into()).unwrap(), ECMAScriptValue::from(true));
    }
    #[test]
    #[should_panic(expected = "Invalid generator return value")]
    fn r_continue() {
        setup_test_agent();
        let iter = ECMAScriptValue::from(create_iterator_from_closure(asyncfn_wrap(returns_continue), "", None));
        generator_resume(iter, ECMAScriptValue::Undefined, "").unwrap();
    }
    #[test]
    #[should_panic(expected = "Invalid generator return value")]
    fn r_break() {
        setup_test_agent();
        let iter = ECMAScriptValue::from(create_iterator_from_closure(asyncfn_wrap(returns_break), "", None));
        generator_resume(iter, ECMAScriptValue::Undefined, "").unwrap();
    }
    #[test]
    fn r_throw() {
        setup_test_agent();
        let iter = ECMAScriptValue::from(create_iterator_from_closure(asyncfn_wrap(returns_throw), "", None));
        let ac = generator_resume(iter, ECMAScriptValue::Undefined, "").unwrap_err();
        assert_eq!(unwind_any_error(ac), String::from("throw"));
    }
}
