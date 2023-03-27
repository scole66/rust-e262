use super::*;
use crate::tests::*;
use ahash::AHashMap;
use test_case::test_case;

mod agent {
    use super::*;
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
            assert_eq!(
                generator_function.o.common_object_data().borrow().prototype.as_ref().unwrap().clone(),
                function
            );

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

fn list_iterator_sample() -> (ECMAScriptValue, Vec<ECMAScriptValue>) {
    let ir = super::create_list_iterator_record(vec![ECMAScriptValue::from(10), ECMAScriptValue::from(2)]);
    (ir.iterator.into(), vec![ECMAScriptValue::Undefined])
}

#[test_case(|| (ECMAScriptValue::Undefined, vec![ECMAScriptValue::Undefined]) => serr("TypeError: Generator required"); "not-a-generator")]
#[test_case(list_iterator_sample => Ok(ECMAScriptValue::from(10)); "generator")]
fn generator_prototype_next(
    make_params: impl FnOnce() -> (ECMAScriptValue, Vec<ECMAScriptValue>),
) -> Result<ECMAScriptValue, String> {
    setup_test_agent();
    let (this_value, arguments) = make_params();
    let result_obj =
        super::generator_prototype_next(this_value, None, arguments.as_slice()).map_err(unwind_any_error)?;
    getv(&result_obj, &"value".into()).map_err(unwind_any_error)
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
