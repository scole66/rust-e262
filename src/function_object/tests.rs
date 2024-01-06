use super::*;
use crate::parser::testhelp::*;
use crate::tests::*;
use num::BigInt;
use test_case::test_case;

mod func_args {
    use super::*;
    #[test]
    fn empty() {
        let arguments: &[ECMAScriptValue] = &[];
        let mut args = FuncArgs::from(arguments);

        assert_eq!(args.count(), 0);
        assert_eq!(args.next_arg(), ECMAScriptValue::Undefined);
    }

    #[test]
    fn list_of_two() {
        let arguments: &[ECMAScriptValue] = &[ECMAScriptValue::from(10), ECMAScriptValue::from(20)];
        let mut args = FuncArgs::from(arguments);

        assert_eq!(args.count(), 2);
        assert_eq!(args.next_arg(), ECMAScriptValue::from(10));
        assert_eq!(args.next_arg(), ECMAScriptValue::from(20));
        assert_eq!(args.next_arg(), ECMAScriptValue::Undefined);
        assert_eq!(args.next_arg(), ECMAScriptValue::Undefined);
        assert_eq!(args.count(), 2);
    }

    #[test]
    fn remaining() {
        let arguments: &[ECMAScriptValue] = &[
            ECMAScriptValue::from("first"),
            ECMAScriptValue::from(1),
            ECMAScriptValue::from(2),
            ECMAScriptValue::from(3),
        ];
        let mut args = FuncArgs::from(arguments);

        let first = args.next_arg();
        assert_eq!(first, ECMAScriptValue::from("first"));

        let rest: Vec<&ECMAScriptValue> = args.remaining().collect();
        assert_eq!(rest, &[&ECMAScriptValue::from(1), &ECMAScriptValue::from(2), &ECMAScriptValue::from(3)]);
    }
}

mod function_declaration {
    use super::*;

    mod instantiate_function_object {
        use super::*;
        use test_case::test_case;

        #[test_case("a" => "a"; "named")]
        #[test_case("" => "default"; "unnamed")]
        fn typical(name: &str) -> String {
            let src = format!("function {name}(){{}}");
            let fd = Maker::new(&src).function_declaration();
            setup_test_agent();
            let realm_rc = current_realm_record().unwrap();
            let global_env = realm_rc.borrow().global_env.as_ref().unwrap().clone() as Rc<dyn EnvironmentRecord>;

            let fvalue = fd.instantiate_function_object(global_env.clone(), None, false, &src, fd.clone()).unwrap();
            let fobj = Object::try_from(fvalue).unwrap();

            let result = String::from(JSString::try_from(fobj.get(&"name".into()).unwrap()).unwrap());

            let function = fobj.o.to_function_obj().unwrap().function_data().borrow();
            assert_eq!(function.environment.name(), global_env.name());
            assert!(function.private_environment.is_none());
            let params: Rc<FormalParameters> = function.formal_parameters.clone().try_into().unwrap();
            assert!(Rc::ptr_eq(&fd.params, &params));
            let body: Rc<FunctionBody> = function.ecmascript_code.clone().try_into().unwrap();
            assert!(Rc::ptr_eq(&fd.body, &body));
            assert_eq!(function.constructor_kind, ConstructorKind::Base);
            assert!(Rc::ptr_eq(&function.realm, &realm_rc));
            assert!(function.script_or_module.is_none()); // No scripts in test agents
            assert_eq!(function.this_mode, ThisMode::Global);
            assert_eq!(function.strict, false);
            assert!(function.home_object.is_none());
            assert_eq!(function.source_text, src);
            assert!(function.fields.is_empty());
            assert!(function.private_methods.is_empty());
            assert!(matches!(function.class_field_initializer_name, ClassName::Empty));
            assert!(!function.is_class_constructor);
            assert!(function.is_constructor);

            result
        }

        #[test]
        fn compile_error() {
            let src = "function a(){ if (true) { @@@; } return 3; }";
            let fd = Maker::new(src).function_declaration();
            setup_test_agent();
            let realm_rc = current_realm_record().unwrap();
            let global_env = realm_rc.borrow().global_env.as_ref().unwrap().clone() as Rc<dyn EnvironmentRecord>;

            let fvalue = fd.instantiate_function_object(global_env.clone(), None, false, src, fd.clone()).unwrap_err();

            let msg = unwind_any_error(fvalue);
            assert_eq!(msg, "TypeError: out of range integral type conversion attempted");
        }
    }
}

mod generator_declaration {
    use super::*;
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn instantiate_function_object() {
        let src = "function *a(){}";
        let fd = Maker::new(src).generator_declaration();
        setup_test_agent();
        let global_env = {
            let realm_rc = current_realm_record().unwrap();
            let realm = realm_rc.borrow();
            realm.global_env.as_ref().unwrap().clone() as Rc<dyn EnvironmentRecord>
        };

        fd.instantiate_function_object(global_env, None, false, src, fd.clone()).unwrap();
    }
}

mod async_function_declaration {
    use super::*;
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn instantiate_function_object() {
        let src = "async function a(){}";
        let fd = Maker::new(src).async_function_declaration();
        setup_test_agent();
        let global_env = {
            let realm_rc = current_realm_record().unwrap();
            let realm = realm_rc.borrow();
            realm.global_env.as_ref().unwrap().clone() as Rc<dyn EnvironmentRecord>
        };

        fd.instantiate_function_object(global_env, None, false, src, fd.clone()).unwrap();
    }
}

mod async_generator_declaration {
    use super::*;
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn instantiate_function_object() {
        let src = "async function *a(){}";
        let fd = Maker::new(src).async_generator_declaration();
        setup_test_agent();
        let global_env = {
            let realm_rc = current_realm_record().unwrap();
            let realm = realm_rc.borrow();
            realm.global_env.as_ref().unwrap().clone() as Rc<dyn EnvironmentRecord>
        };

        fd.instantiate_function_object(global_env, None, false, src, fd.clone()).unwrap();
    }
}

mod this_lexicality {
    use super::*;
    use test_case::test_case;

    #[test_case(ThisLexicality::LexicalThis => with |s| assert_ne!(s, ""); "lexical")]
    #[test_case(ThisLexicality::NonLexicalThis => with |s| assert_ne!(s, ""); "non-lexical")]
    fn debug(item: ThisLexicality) -> String {
        format!("{item:?}")
    }

    #[test_case(ThisLexicality::LexicalThis, ThisLexicality::LexicalThis => true; "equal")]
    #[test_case(ThisLexicality::NonLexicalThis, ThisLexicality::LexicalThis => false; "unequal")]
    fn eq(left: ThisLexicality, right: ThisLexicality) -> bool {
        left == right
    }

    #[test]
    #[allow(clippy::clone_on_copy)]
    fn clone() {
        let l1 = ThisLexicality::LexicalThis;
        let l2 = l1.clone();
        assert_eq!(l1, l2);
    }
}

mod constructor_kind {
    use super::*;
    use test_case::test_case;

    #[test_case(ConstructorKind::Derived => with |s| assert_ne!(s, ""); "derived")]
    #[test_case(ConstructorKind::Base => with |s| assert_ne!(s, ""); "base")]
    fn debug(item: ConstructorKind) -> String {
        format!("{item:?}")
    }

    #[test_case(ConstructorKind::Derived, ConstructorKind::Derived => true; "equal")]
    #[test_case(ConstructorKind::Base, ConstructorKind::Derived => false; "unequal")]
    fn eq(left: ConstructorKind, right: ConstructorKind) -> bool {
        left == right
    }

    #[test]
    #[allow(clippy::clone_on_copy)]
    fn clone() {
        let l1 = ConstructorKind::Derived;
        let l2 = l1.clone();
        assert_eq!(l1, l2);
    }
}

mod function_prototype_call {
    use super::*;
    use test_case::test_case;

    #[test_case(|| ECMAScriptValue::Undefined, &[] => serr("TypeError: this isn't a function"); "bad this")]
    #[test_case(|| ECMAScriptValue::from(intrinsic(IntrinsicId::Number)), &[ECMAScriptValue::Undefined, ECMAScriptValue::from(10)] => vok(10); "built-in function call")]
    fn function_prototype_call(
        get_this: impl FnOnce() -> ECMAScriptValue,
        args: &[ECMAScriptValue],
    ) -> Result<ECMAScriptValue, String> {
        setup_test_agent();
        let this_value = get_this();
        super::function_prototype_call(this_value, None, args).map_err(unwind_any_error)
    }
}

#[test_case(super::function_prototype_apply => panics "not yet implemented"; "function_prototype_apply")]
#[test_case(super::function_prototype_bind => panics "not yet implemented"; "function_prototype_bind")]
#[test_case(super::function_constructor_function => panics "not yet implemented"; "function_constructor_function")]
fn todo(f: fn(ECMAScriptValue, Option<&Object>, &[ECMAScriptValue]) -> Completion<ECMAScriptValue>) {
    setup_test_agent();
    f(ECMAScriptValue::Undefined, None, &[]).unwrap();
}

#[test_case(|| intrinsic(IntrinsicId::IsNaN) => sok("function isNaN() { [native code] }"); "builtin fcn")]
#[test_case(|| ECMAScriptValue::Undefined => serr("TypeError: Function.prototype.toString requires that 'this' be a Function"); "non-function (undefined)")]
#[test_case(|| ECMAScriptValue::Null => serr("TypeError: Function.prototype.toString requires that 'this' be a Function"); "non-function (null)")]
#[test_case(|| true => serr("TypeError: Function.prototype.toString requires that 'this' be a Function"); "non-function (Boolean)")]
#[test_case(|| "string" => serr("TypeError: Function.prototype.toString requires that 'this' be a Function"); "non-function (String)")]
#[test_case(|| 0 => serr("TypeError: Function.prototype.toString requires that 'this' be a Function"); "non-function (Number)")]
#[test_case(|| BigInt::from(0) => serr("TypeError: Function.prototype.toString requires that 'this' be a Function"); "non-function (BigInt)")]
#[test_case(|| wks(WksId::Unscopables) => serr("TypeError: Function.prototype.toString requires that 'this' be a Function"); "non-function (Symbol)")]
#[test_case(|| ordinary_object_create(None, &[]) => serr("TypeError: Function.prototype.toString requires that 'this' be a Function"); "non-function (Object)")]
#[test_case(
    || {
        let env = current_realm_record().unwrap().borrow().global_env.clone().unwrap();
        let params = ParamSource::FormalParameters(Maker::new("(a, b, c)").formal_parameters());
        let body = BodySource::Function(Maker::new("{ return a + b + c; }").function_body());
        let realm = current_realm_record().unwrap();
        FunctionObject::object(
            None,
            env,
            None,
            params,
            body,
            ConstructorKind::Base,
            realm,
            None,
            ThisMode::Global,
            false,
            None,
            "function(a, b, c) { return a+b+c; }",
            vec![],
            vec![],
            ClassName::Empty,
            false,
            Rc::new(Chunk::new("tester")),
        )
    }
    => sok("function(a, b, c) { return a+b+c; }");
    "standard function object"
)]
#[test_case(
    || BuiltInFunctionObject::object(None, true, current_realm_record().unwrap(), None, do_nothing, false)
    => sok("function () { [native code] }");
    "built-in without InitialName"
)]
fn function_prototype_to_string<T>(make_this: impl FnOnce() -> T) -> Result<String, String>
where
    T: Into<ECMAScriptValue>,
{
    setup_test_agent();
    let this = make_this().into();
    super::function_prototype_to_string(this, None, &[]).map_err(unwind_any_error).map(|v| v.test_result_string())
}

#[test_case(|| intrinsic(IntrinsicId::Object).into(), || intrinsic(IntrinsicId::Function).into() => sok("true"); "success")]
fn function_prototype_has_instance(
    make_this: impl FnOnce() -> ECMAScriptValue,
    make_val: impl FnOnce() -> ECMAScriptValue,
) -> Result<String, String> {
    setup_test_agent();
    let this = make_this();
    let val = make_val();
    super::function_prototype_has_instance(this, None, &[val]).map_err(unwind_any_error).map(|v| v.test_result_string())
}

#[test]
fn provision_function_intrinsic() {
    setup_test_agent();

    let function_constructor = intrinsic(IntrinsicId::Function);
    let function_prototype = intrinsic(IntrinsicId::FunctionPrototype);
    let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);

    assert!(is_constructor(&function_constructor.clone().into()));
    assert!(is_callable(&function_prototype.clone().into()));

    assert_eq!(function_constructor.o.get_prototype_of().unwrap().unwrap(), function_prototype);
    assert_eq!(function_constructor.get(&"length".into()).unwrap(), ECMAScriptValue::from(1));
    assert!(matches!(
        function_constructor.o.get_own_property(&"prototype".into()),
        Ok(Some(PropertyDescriptor {
            property: PropertyKind::Data(DataProperty { value: ECMAScriptValue::Object(obj), writable: false }),
            enumerable: false,
            configurable: false,
            spot: _
        })) if obj == function_prototype
    ));
    assert_eq!(function_prototype.o.get_prototype_of().unwrap().unwrap(), object_prototype);
    assert!(function_prototype.o.get_own_property(&"prototype".into()).unwrap().is_none());
    assert!(matches!(
        function_prototype.o.get_own_property(&"length".into()),
        Ok(Some(PropertyDescriptor {
            property: PropertyKind::Data(DataProperty { value: ECMAScriptValue::Number(num), writable: false }),
            enumerable: false,
            configurable: true,
            spot: _
        })) if num == 0.0
    ));
    assert!(matches!(
        function_prototype.o.get_own_property(&"name".into()),
        Ok(Some(PropertyDescriptor {
            property: PropertyKind::Data(DataProperty { value, writable: false }),
            enumerable: false,
            configurable: true,
            spot: _
        })) if value == ECMAScriptValue::from("")
    ));
    assert!(matches!(
        function_prototype.o.get_own_property(&"apply".into()),
        Ok(Some(PropertyDescriptor {
            property: PropertyKind::Data(DataProperty { value, writable: true }),
            enumerable: false,
            configurable: true,
            spot: _
        })) if is_callable(&value)
    ));
    assert!(matches!(
        function_prototype.o.get_own_property(&"bind".into()),
        Ok(Some(PropertyDescriptor {
            property: PropertyKind::Data(DataProperty { value, writable: true }),
            enumerable: false,
            configurable: true,
            spot: _
        })) if is_callable(&value)
    ));
    assert!(matches!(
        function_prototype.o.get_own_property(&"call".into()),
        Ok(Some(PropertyDescriptor {
            property: PropertyKind::Data(DataProperty { value, writable: true }),
            enumerable: false,
            configurable: true,
            spot: _
        })) if is_callable(&value)
    ));
    assert!(matches!(
        function_prototype.o.get_own_property(&"toString".into()),
        Ok(Some(PropertyDescriptor {
            property: PropertyKind::Data(DataProperty { value, writable: true }),
            enumerable: false,
            configurable: true,
            spot: _
        })) if is_callable(&value)
    ));
    assert!(matches!(
        function_prototype.o.get_own_property(&wks(WksId::HasInstance).into()),
        Ok(Some(PropertyDescriptor {
            property: PropertyKind::Data(DataProperty { value, writable: false }),
            enumerable: false,
            configurable: false,
            spot: _
        })) if is_callable(&value)
    ));
    assert!(matches!(
        function_prototype.o.get_own_property(&"constructor".into()),
        Ok(Some(PropertyDescriptor {
            property: PropertyKind::Data(DataProperty { value: ECMAScriptValue::Object(obj), writable: true }),
            enumerable: false,
            configurable: true,
            spot: _
        })) if obj == function_constructor
    ));
}

mod concise_body_source {
    use super::*;

    #[test]
    fn fmt() {
        setup_test_agent();
        let bs = BodySource::from(Maker::new(";").function_body());
        let fnbody = ConciseBodySource(&bs);
        let res = format!("{fnbody:#?}");
        assert_ne!(res, "");
        assert!(!res.contains('\n'));
    }
}

mod concise_param_source {
    use super::*;

    #[test]
    fn fmt() {
        setup_test_agent();
        let fp = ParamSource::from(Maker::new("a,b,c").formal_parameters());
        let cps = ConciseParamSource(&fp);
        let res = format!("{cps:#?}");
        assert_ne!(res, "");
        assert!(!res.contains('\n'));
    }
}

mod function_object_data {
    use super::*;

    #[test]
    fn fmt() {
        setup_test_agent();
        let src = "function test_sample(){{}}";
        let fd = Maker::new(src).function_declaration();
        setup_test_agent();
        let realm_rc = current_realm_record().unwrap();
        let global_env = realm_rc.borrow().global_env.as_ref().unwrap().clone() as Rc<dyn EnvironmentRecord>;

        let fvalue = fd.instantiate_function_object(global_env.clone(), None, false, src, fd.clone()).unwrap();
        let fobj = Object::try_from(fvalue).unwrap();

        let function = fobj.o.to_function_obj().unwrap().function_data().borrow();

        let repr = format!("{function:?}");

        assert_ne!(repr, "");
        assert!(repr.len() < 700);

        let repr2 = format!("{function:#?}");
        assert!(repr2.lines().count() < 100);
    }
}

mod make_method {
    use super::*;

    #[test]
    fn call() {
        setup_test_agent();
        let src = "function a(){}";
        let fd = Maker::new(src).function_declaration();
        let realm_rc = current_realm_record().unwrap();
        let global_env = realm_rc.borrow().global_env.as_ref().unwrap().clone() as Rc<dyn EnvironmentRecord>;

        let fvalue = fd.instantiate_function_object(global_env.clone(), None, false, src, fd.clone()).unwrap();
        let fobj = Object::try_from(fvalue).unwrap();
        let f_funobj = fobj.o.to_function_obj().unwrap();

        let home = ordinary_object_create(None, &[]);

        assert!(f_funobj.function_data().borrow().home_object.is_none());

        make_method(f_funobj, home.clone());

        assert_eq!(f_funobj.function_data().borrow().home_object, Some(home));
    }
}
