use super::*;
use crate::tests::*;
use test_case::test_case;

mod agent {
    use super::*;
    use test_case::test_case;

    #[test]
    fn provision_iterator_prototype() {
        let mut agent = test_agent();

        let iterator_prototype = agent.intrinsic(IntrinsicId::IteratorPrototype);
        let object_prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);

        let iter_proto_proto = iterator_prototype.o.common_object_data().borrow().prototype.as_ref().unwrap().clone();
        assert_eq!(iter_proto_proto, object_prototype);

        let iterator_sym = agent.wks(WksId::Iterator);
        let iterator_desc = IdealizedPropertyDescriptor::from(
            iterator_prototype.o.get_own_property(&mut agent, &iterator_sym.into()).unwrap().unwrap(),
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
            let mut agent = test_agent();
            let function = agent.intrinsic(IntrinsicId::Function);
            let generator_function = agent.intrinsic(IntrinsicId::GeneratorFunction);

            assert!(generator_function.o.to_builtin_function_obj().is_some());
            assert_eq!(
                generator_function.o.common_object_data().borrow().prototype.as_ref().unwrap().clone(),
                function
            );

            assert_eq!(
                get(&mut agent, &generator_function, &"name".into()).unwrap(),
                ECMAScriptValue::from("GeneratorFunction")
            );
            assert_eq!(get(&mut agent, &generator_function, &"length".into()).unwrap(), ECMAScriptValue::from(1));
            assert_eq!(
                get(&mut agent, &generator_function, &"prototype".into()).unwrap(),
                ECMAScriptValue::from(agent.intrinsic(IntrinsicId::GeneratorFunctionPrototype))
            );
        }

        #[test]
        fn generator_function_prototype() {
            let mut agent = test_agent();
            let generator_function_prototype = agent.intrinsic(IntrinsicId::GeneratorFunctionPrototype);
            let function_prototype = agent.intrinsic(IntrinsicId::FunctionPrototype);

            assert!(!generator_function_prototype.o.is_callable_obj());
            assert_eq!(
                generator_function_prototype.o.common_object_data().borrow().prototype.as_ref().unwrap().clone(),
                function_prototype
            );

            let to_string_tag = agent.wks(WksId::ToStringTag);
            assert_eq!(
                get(&mut agent, &generator_function_prototype, &to_string_tag.into()).unwrap(),
                ECMAScriptValue::from("GeneratorFunction")
            );
            assert_eq!(
                get(&mut agent, &generator_function_prototype, &"constructor".into()).unwrap(),
                ECMAScriptValue::from(agent.intrinsic(IntrinsicId::GeneratorFunction))
            );
            assert_eq!(
                get(&mut agent, &generator_function_prototype, &"prototype".into()).unwrap(),
                ECMAScriptValue::from(agent.intrinsic(IntrinsicId::GeneratorFunctionPrototypePrototype))
            );
        }

        #[test]
        fn generator_prototype() {
            let mut agent = test_agent();
            let generator_prototype = agent.intrinsic(IntrinsicId::GeneratorFunctionPrototypePrototype);

            assert_eq!(
                generator_prototype.o.common_object_data().borrow().prototype.as_ref().unwrap().clone(),
                agent.intrinsic(IntrinsicId::IteratorPrototype)
            );
            assert_eq!(
                get(&mut agent, &generator_prototype, &"constructor".into()).unwrap(),
                ECMAScriptValue::from(agent.intrinsic(IntrinsicId::GeneratorFunctionPrototype))
            );
            let to_string_tag = agent.wks(WksId::ToStringTag);
            assert_eq!(
                get(&mut agent, &generator_prototype, &to_string_tag.into()).unwrap(),
                ECMAScriptValue::from("Generator")
            );
        }

        #[test_case("next" => "next;1"; "next function")]
        #[test_case("return" => "return;1"; "return function")]
        #[test_case("throw" => "throw;1"; "throw function")]
        fn generator_prototype_func(key: &str) -> String {
            let mut agent = test_agent();
            let key = PropertyKey::from(key);
            let proto = agent.intrinsic(IntrinsicId::GeneratorFunctionPrototypePrototype);
            let val = super::get(&mut agent, &proto, &key).unwrap();
            assert!(is_callable(&val));
            let name = getv(&mut agent, &val, &"name".into()).unwrap();
            let name = to_string(&mut agent, name).unwrap();
            let length = getv(&mut agent, &val, &"length".into()).unwrap();
            let length = to_string(&mut agent, length).unwrap();
            format!("{};{}", String::from(name), length)
        }
    }

    #[test_case("a value".into(), true; "is done")]
    #[test_case(ECMAScriptValue::Undefined, false; "not done")]
    fn create_iter_result_object(value: ECMAScriptValue, done: bool) {
        let mut agent = test_agent();
        let obj = agent.create_iter_result_object(value.clone(), done);
        let value_res = get(&mut agent, &obj, &"value".into()).unwrap();
        let done_res = get(&mut agent, &obj, &"done".into()).unwrap();

        assert_eq!(value_res, value);
        assert_eq!(done_res, ECMAScriptValue::from(done));
    }
}

#[test_case(|_| ECMAScriptValue::Undefined => Ok(ECMAScriptValue::Undefined); "pass-thru/undefined")]
#[test_case(|_| ECMAScriptValue::from(67) => Ok(ECMAScriptValue::from(67)); "pass-thru/number")]
fn iterator_prototype_iterator(
    make_params: impl FnOnce(&mut Agent) -> ECMAScriptValue,
) -> Result<ECMAScriptValue, String> {
    let mut agent = test_agent();
    let this_value = make_params(&mut agent);
    super::iterator_prototype_iterator(&mut agent, this_value, None, &[]).map_err(|e| unwind_any_error(&mut agent, e))
}

tbd_function!(generator_function);
tbd_function!(generator_prototype_next);
tbd_function!(generator_prototype_return);
tbd_function!(generator_prototype_throw);
