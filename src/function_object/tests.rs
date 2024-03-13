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
        super::function_prototype_call(&this_value, None, args).map_err(unwind_any_error)
    }
}

#[test_case(super::function_prototype_apply => panics "not yet implemented"; "function_prototype_apply")]
#[test_case(super::function_prototype_bind => panics "not yet implemented"; "function_prototype_bind")]
#[test_case(super::function_constructor_function => panics "not yet implemented"; "function_constructor_function")]
fn todo(f: fn(&ECMAScriptValue, Option<&Object>, &[ECMAScriptValue]) -> Completion<ECMAScriptValue>) {
    setup_test_agent();
    f(&ECMAScriptValue::Undefined, None, &[]).unwrap();
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
    super::function_prototype_to_string(&this, None, &[]).map_err(unwind_any_error).map(|v| v.test_result_string())
}

#[test_case(|| intrinsic(IntrinsicId::Object).into(), || intrinsic(IntrinsicId::Function).into() => sok("true"); "success")]
fn function_prototype_has_instance(
    make_this: impl FnOnce() -> ECMAScriptValue,
    make_val: impl FnOnce() -> ECMAScriptValue,
) -> Result<String, String> {
    setup_test_agent();
    let this = make_this();
    let val = make_val();
    super::function_prototype_has_instance(&this, None, &[val])
        .map_err(unwind_any_error)
        .map(|v| v.test_result_string())
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

mod class_name {
    use super::*;
    use test_case::test_case;

    #[test]
    fn fmt_debug() {
        let cn = ClassName::Empty;
        assert_ne!(format!("{cn:?}"), "");
    }

    #[test_case(&ClassName::Empty => ClassName::Empty; "empty")]
    fn clone(a: &ClassName) -> ClassName {
        a.clone()
    }

    #[test_case("bob" => ClassName::String(JSString::from("bob")); "&str")]
    fn from(val: impl Into<JSString>) -> ClassName {
        ClassName::from(val)
    }

    #[test_case(|| NormalCompletion::Empty => sok("Empty"); "empty")]
    #[test_case(|| NormalCompletion::from("bob") => sok("String(bob)"); "string")]
    #[test_case(
        || NormalCompletion::from(wks(WksId::ToStringTag))
        => sok("Symbol(Symbol(Symbol.toStringTag))");
        "symbol"
    )]
    #[test_case(|| NormalCompletion::from(10.3) => serr("Not a class name"); "non-name value")]
    #[test_case(
        || NormalCompletion::from(PrivateName::new("blue heron"))
        => sok("PrivateName(matches)");
        "private name"
    )]
    #[test_case(
        || NormalCompletion::from(Reference::new(Base::Unresolvable, "blue", false, None))
        => serr("Not a class name");
        "non-value completion"
    )]
    fn try_from(make_nc: impl FnOnce() -> NormalCompletion) -> Result<String, String> {
        setup_test_agent();
        let nc = make_nc();
        let pn = if let NormalCompletion::PrivateName(pn) = &nc { Some(pn) } else { None };
        let cn = ClassName::try_from(nc.clone()).map_err(|e| e.to_string())?;

        match cn {
            ClassName::String(s) => Ok(format!("String({s})")),
            ClassName::Symbol(sym) => Ok(format!("Symbol({sym})")),
            ClassName::Private(cnpn) => {
                Ok(format!("PrivateName({})", if &cnpn == pn.unwrap() { "matches" } else { "doesn't match" }))
            }
            ClassName::Empty => Ok(String::from("Empty")),
        }
    }
}

mod function_name {
    use super::*;
    use test_case::test_case;

    #[test]
    fn fmt_debug() {
        let fname = FunctionName::String(JSString::from("bob"));
        assert_ne!(format!("{fname:?}"), "");
    }

    #[test]
    fn clone() {
        let fname = FunctionName::String(JSString::from("bob"));
        let copy = fname.clone();
        if let FunctionName::String(s) = copy {
            assert_eq!(s, JSString::from("bob"));
        } else {
            panic!("function name not cloned correctly");
        }
    }

    #[test_case(|| JSString::from("bob") => "String(bob)"; "string")]
    #[test_case(|| PropertyKey::from("stringy") => "String(stringy)"; "string as property key")]
    #[test_case(|| PropertyKey::from(wks(WksId::ToStringTag)) => "Symbol(Symbol(Symbol.toStringTag))"; "symbol as property key")]
    #[test_case(|| PrivateName::new("alice") => "Private(PN[alice])"; "private name")]
    fn from<T>(make_item: impl FnOnce() -> T) -> String
    where
        FunctionName: From<T>,
    {
        setup_test_agent();
        let item = make_item();
        let fname = FunctionName::from(item);
        match fname {
            FunctionName::String(s) => format!("String({s})"),
            FunctionName::Symbol(s) => format!("Symbol({s})"),
            FunctionName::PrivateName(pn) => format!("Private({pn})"),
        }
    }

    #[test_case(|| FunctionName::from(JSString::from("alice")) => "alice"; "string")]
    #[test_case(
        || FunctionName::from(PropertyKey::from(Symbol::new(Some("my description".into()))))
        => "[Symbol(my description)]";
        "symbol with description"
    )]
    #[test_case(
        || FunctionName::from(PropertyKey::from(Symbol::new(None)))
        => "[Symbol()]";
        "symbol without description"
    )]
    #[test_case(|| FunctionName::from(PrivateName::new("other_description")) => "#other_description"; "private name")]
    fn fmt_display(make_item: impl FnOnce() -> FunctionName) -> String {
        setup_test_agent();
        let item = make_item();
        format!("{item}")
    }

    mod try_from {
        use super::*;
        use test_case::test_case;

        #[test_case(|| NormalCompletion::Empty => serr("Completion type not valid for FunctionName"); "invalid")]
        #[test_case(|| NormalCompletion::from("alice") => sok("alice"); "string value")]
        #[test_case(|| NormalCompletion::from(Symbol::new(Some("charlie".into()))) => sok("[Symbol(charlie)]"); "symbol value")]
        #[test_case(|| NormalCompletion::from(PrivateName::new("david")) => sok("#david"); "private name")]
        fn normal_completion(make: impl FnOnce() -> NormalCompletion) -> Result<String, String> {
            setup_test_agent();
            let item = make();
            FunctionName::try_from(item).map_err(|e| e.to_string()).map(|fname| format!("{fname}"))
        }
    }
}

mod param_source {
    use super::*;
    use test_case::test_case;

    #[test]
    fn clone() {
        let params_a = Maker::new("a, b, c").formal_parameters();
        let a = ParamSource::FormalParameters(params_a);
        let b = a.clone();
        assert!(matches!(b, ParamSource::FormalParameters(_)));
        assert_eq!(format!("{a}"), format!("{b}"));
        assert_eq!(a, b);
    }

    #[test]
    fn debug_fmt() {
        let params_a = Maker::new("a, b, c").formal_parameters();
        let a = ParamSource::FormalParameters(params_a);
        assert_ne!(format!("{a:?}"), "");
    }
    #[derive(Copy, Clone)]
    enum Kind {
        Formal,
        Arrow,
        AsyncArrow,
        ArrowFormals,
        Unique,
    }
    fn same(kind: Kind) -> (ParamSource, ParamSource) {
        match kind {
            Kind::Formal => {
                let params = Maker::new("a").formal_parameters();
                (ParamSource::FormalParameters(params.clone()), ParamSource::FormalParameters(params))
            }
            Kind::Arrow => {
                let params = Maker::new("a").arrow_parameters();
                (ParamSource::ArrowParameters(params.clone()), ParamSource::ArrowParameters(params))
            }
            Kind::AsyncArrow => {
                let params = Maker::new("a").async_arrow_binding_identifier();
                (ParamSource::AsyncArrowBinding(params.clone()), ParamSource::AsyncArrowBinding(params))
            }
            Kind::ArrowFormals => {
                let params = Maker::new("(a)").arrow_formal_parameters();
                (ParamSource::ArrowFormals(params.clone()), ParamSource::ArrowFormals(params))
            }
            Kind::Unique => {
                let params = Maker::new("a").unique_formal_parameters();
                (ParamSource::UniqueFormalParameters(params.clone()), ParamSource::UniqueFormalParameters(params))
            }
        }
    }
    fn different(kind: Kind) -> (ParamSource, ParamSource) {
        match kind {
            Kind::Formal => {
                let left = Maker::new("a").formal_parameters();
                let right = Maker::new("b").formal_parameters();
                (ParamSource::FormalParameters(left), ParamSource::FormalParameters(right))
            }
            Kind::Arrow => {
                let left = Maker::new("a").arrow_parameters();
                let right = Maker::new("b").arrow_parameters();
                (ParamSource::ArrowParameters(left), ParamSource::ArrowParameters(right))
            }
            Kind::AsyncArrow => {
                let left = Maker::new("a").async_arrow_binding_identifier();
                let right = Maker::new("b").async_arrow_binding_identifier();
                (ParamSource::AsyncArrowBinding(left), ParamSource::AsyncArrowBinding(right))
            }
            Kind::ArrowFormals => {
                let left = Maker::new("(a)").arrow_formal_parameters();
                let right = Maker::new("(b)").arrow_formal_parameters();
                (ParamSource::ArrowFormals(left), ParamSource::ArrowFormals(right))
            }
            Kind::Unique => {
                let left = Maker::new("a").unique_formal_parameters();
                let right = Maker::new("b").unique_formal_parameters();
                (ParamSource::UniqueFormalParameters(left), ParamSource::UniqueFormalParameters(right))
            }
        }
    }
    fn diff2(left_kind: Kind, right_kind: Kind) -> (ParamSource, ParamSource) {
        let left = match left_kind {
            Kind::Formal => ParamSource::FormalParameters(Maker::new("a").formal_parameters()),
            Kind::Arrow => ParamSource::ArrowParameters(Maker::new("a").arrow_parameters()),
            Kind::AsyncArrow => ParamSource::AsyncArrowBinding(Maker::new("a").async_arrow_binding_identifier()),
            Kind::ArrowFormals => ParamSource::ArrowFormals(Maker::new("(a)").arrow_formal_parameters()),
            Kind::Unique => ParamSource::UniqueFormalParameters(Maker::new("a").unique_formal_parameters()),
        };
        let right = match right_kind {
            Kind::Formal => ParamSource::FormalParameters(Maker::new("b").formal_parameters()),
            Kind::Arrow => ParamSource::ArrowParameters(Maker::new("b").arrow_parameters()),
            Kind::AsyncArrow => ParamSource::AsyncArrowBinding(Maker::new("b").async_arrow_binding_identifier()),
            Kind::ArrowFormals => ParamSource::ArrowFormals(Maker::new("(b)").arrow_formal_parameters()),
            Kind::Unique => ParamSource::UniqueFormalParameters(Maker::new("b").unique_formal_parameters()),
        };
        (left, right)
    }
    #[test_case(|| same(Kind::Formal) => true; "formals: cloned parse objects")]
    #[test_case(|| same(Kind::Arrow) => true; "Arrow: cloned parse objects")]
    #[test_case(|| same(Kind::AsyncArrow) => true; "async arrow: cloned parse objects")]
    #[test_case(|| same(Kind::ArrowFormals) => true; "arrow formals: cloned parse objects")]
    #[test_case(|| same(Kind::Unique) => true; "unique formals: cloned parse objects")]
    #[test_case(|| different(Kind::Formal) => false; "formals: type-same parse objects")]
    #[test_case(|| different(Kind::Arrow) => false; "Arrow: type-same parse objects")]
    #[test_case(|| different(Kind::AsyncArrow) => false; "async arrow: type-same parse objects")]
    #[test_case(|| different(Kind::ArrowFormals) => false; "arrow formals: type-same parse objects")]
    #[test_case(|| different(Kind::Unique) => false; "unique formals: type-same parse objects")]
    #[test_case(|| diff2(Kind::Formal, Kind::Arrow) => false; "formal/arrow; diff-type")]
    #[test_case(|| diff2(Kind::Formal, Kind::AsyncArrow) => false; "formal/AsyncArrow; diff-type")]
    #[test_case(|| diff2(Kind::Formal, Kind::ArrowFormals) => false; "formal/ArrowFormals; diff-type")]
    #[test_case(|| diff2(Kind::Formal, Kind::Unique) => false; "formal/Unique; diff-type")]
    #[test_case(|| diff2(Kind::Arrow, Kind::Formal) => false; "arrow/formal; diff-type")]
    #[test_case(|| diff2(Kind::Arrow, Kind::AsyncArrow) => false; "Arrow/AsyncArrow; diff-type")]
    #[test_case(|| diff2(Kind::Arrow, Kind::ArrowFormals) => false; "Arrow/ArrowFormals; diff-type")]
    #[test_case(|| diff2(Kind::Arrow, Kind::Unique) => false; "Arrow/Unique; diff-type")]
    #[test_case(|| diff2(Kind::AsyncArrow, Kind::Formal) => false; "AsyncArrow/formal; diff-type")]
    #[test_case(|| diff2(Kind::AsyncArrow, Kind::Arrow) => false; "AsyncArrow/Arrow; diff-type")]
    #[test_case(|| diff2(Kind::AsyncArrow, Kind::ArrowFormals) => false; "AsyncArrow/ArrowFormals; diff-type")]
    #[test_case(|| diff2(Kind::AsyncArrow, Kind::Unique) => false; "AsyncArrow/Unique; diff-type")]
    #[test_case(|| diff2(Kind::ArrowFormals, Kind::Formal) => false; "ArrowFormals/formal; diff-type")]
    #[test_case(|| diff2(Kind::ArrowFormals, Kind::Arrow) => false; "ArrowFormals/Arrow; diff-type")]
    #[test_case(|| diff2(Kind::ArrowFormals, Kind::AsyncArrow) => false; "ArrowFormals/AsyncArrow; diff-type")]
    #[test_case(|| diff2(Kind::ArrowFormals, Kind::Unique) => false; "ArrowFormals/Unique; diff-type")]
    #[test_case(|| diff2(Kind::Unique, Kind::Formal) => false; "Unique/formal; diff-type")]
    #[test_case(|| diff2(Kind::Unique, Kind::Arrow) => false; "Unique/Arrow; diff-type")]
    #[test_case(|| diff2(Kind::Unique, Kind::AsyncArrow) => false; "Unique/AsyncArrow; diff-type")]
    #[test_case(|| diff2(Kind::Unique, Kind::ArrowFormals) => false; "Unique/ArrowFormals; diff-type")]
    fn eq(make_pair: impl FnOnce() -> (ParamSource, ParamSource)) -> bool {
        let (left, right) = make_pair();
        left == right
    }

    #[test_case(
        || {
            let node = Maker::new("a").formal_parameters();
            (node.clone(), ParamSource::FormalParameters(node))
        }
        => true;
        "formal parameters"
    )]
    #[test_case(
        || {
            let node = Maker::new("a").unique_formal_parameters();
            (node.clone(), ParamSource::UniqueFormalParameters(node))
        }
        => true;
        "unique formal parameters"
    )]
    #[test_case(
        || {
            let node = Maker::new("a").async_arrow_binding_identifier();
            (node.clone(), ParamSource::AsyncArrowBinding(node))
        }
        => true;
        "async_arrow_binding_identifier"
    )]
    #[test_case(
        || {
            let node = Maker::new("(a)").arrow_formal_parameters();
            (node.clone(), ParamSource::ArrowFormals(node))
        }
        => true;
        "arrow_formal_parameters"
    )]
    #[test_case(
        || {
            let node = Maker::new("a").arrow_parameters();
            (node.clone(), ParamSource::ArrowParameters(node))
        }
        => true;
        "arrow_parameters"
    )]
    fn from<T>(make_item: impl FnOnce() -> (T, ParamSource)) -> bool
    where
        ParamSource: From<T>,
    {
        let (item, expected) = make_item();
        let result = ParamSource::from(item);

        result == expected
    }

    #[test_case(&ParamSource::from(Maker::new("a,b,c").formal_parameters()) => "a , b , c"; "formal_parameters")]
    #[test_case(
        &ParamSource::from(Maker::new("a,b,c").unique_formal_parameters()) => "a , b , c"; "unique_formal_parameters"
    )]
    #[test_case(
        &ParamSource::from(Maker::new("a").async_arrow_binding_identifier()) => "a"; "async_arrow_binding_identifier"
    )]
    #[test_case(
        &ParamSource::from(Maker::new("(a,b,c)").arrow_formal_parameters()) => "( a , b , c )"; "arrow_formal_parameters"
    )]
    #[test_case(&ParamSource::from(Maker::new("a").arrow_parameters()) => "a"; "arrow_parameters")]
    fn display_fmt(item: &ParamSource) -> String {
        format!("{item}")
    }

    #[test_case(&ParamSource::from(Maker::new("a, b, c").formal_parameters()) => false; "formal_parameters / false")]
    #[test_case(&ParamSource::from(Maker::new("a=1").formal_parameters()) => true; "formal_parameters / true")]
    #[test_case(&ParamSource::from(Maker::new("a").unique_formal_parameters()) => false; "unique_formal_parameters / false")]
    #[test_case(&ParamSource::from(Maker::new("a=1").unique_formal_parameters()) => true; "unique_formal_parameters / true")]
    #[test_case(&ParamSource::from(Maker::new("a").async_arrow_binding_identifier()) => false; "async_arrow_binding_identifier / false")]
    #[test_case(&ParamSource::from(Maker::new("(a)").arrow_formal_parameters()) => false; "arrow_formal_parameters / false")]
    #[test_case(&ParamSource::from(Maker::new("(a=1)").arrow_formal_parameters()) => true; "arrow_formal_parameters / true")]
    #[test_case(&ParamSource::from(Maker::new("a").arrow_parameters()) => false; "arrow_parameters / false")]
    #[test_case(&ParamSource::from(Maker::new("(a=1)").arrow_parameters()) => true; "arrow_parameters / true")]
    fn contains_expression(item: &ParamSource) -> bool {
        item.contains_expression()
    }

    #[test_case(&ParamSource::from(Maker::new("a, b, c").formal_parameters()) => 3.0; "formal_parameters")]
    #[test_case(&ParamSource::from(Maker::new("a").unique_formal_parameters()) => 1.0; "unique_formal_parameters")]
    #[test_case(&ParamSource::from(Maker::new("a").async_arrow_binding_identifier()) => 1.0; "async_arrow_binding_identifier")]
    #[test_case(&ParamSource::from(Maker::new("(a, b)").arrow_formal_parameters()) => 2.0; "arrow_formal_parameters")]
    #[test_case(&ParamSource::from(Maker::new("(a=1,q)").arrow_parameters()) => 0.0; "arrow_parameters")]
    fn expected_argument_count(item: &ParamSource) -> f64 {
        item.expected_argument_count()
    }

    #[test_case(&ParamSource::from(Maker::new("a, b, c").formal_parameters()) => svec(&["a", "b", "c"]); "formal_parameters")]
    #[test_case(&ParamSource::from(Maker::new("a").unique_formal_parameters()) => svec(&["a"]); "unique_formal_parameters")]
    #[test_case(&ParamSource::from(Maker::new("a").async_arrow_binding_identifier()) => svec(&["a"]); "async_arrow_binding_identifier")]
    #[test_case(&ParamSource::from(Maker::new("(a,b)").arrow_formal_parameters()) => svec(&["a", "b"]); "arrow_formal_parameters")]
    #[test_case(&ParamSource::from(Maker::new("(c,b,a)").arrow_parameters()) => svec(&["c", "b", "a"]); "arrow_parameters")]
    fn bound_names(item: &ParamSource) -> Vec<String> {
        item.bound_names().into_iter().map(String::from).collect()
    }

    #[test_case(&ParamSource::from(Maker::new("a, b, c").formal_parameters()) => true; "formal_parameters")]
    #[test_case(&ParamSource::from(Maker::new("a").unique_formal_parameters()) => true; "unique_formal_parameters")]
    #[test_case(&ParamSource::from(Maker::new("a").async_arrow_binding_identifier()) => true; "async_arrow_binding_identifier")]
    #[test_case(&ParamSource::from(Maker::new("(a,b)").arrow_formal_parameters()) => true; "arrow_formal_parameters")]
    #[test_case(&ParamSource::from(Maker::new("(c,b,a)").arrow_parameters()) => true; "arrow_parameters")]
    fn is_simple_parameter_list(item: &ParamSource) -> bool {
        item.is_simple_parameter_list()
    }
}

mod function_object {
    use super::*;

    fn make() -> Object {
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

    false_function!(is_proxy_object);
    none_function!(to_proxy_object);
}

mod builtin_function_object {
    use super::*;

    fn make() -> Object {
        intrinsic(IntrinsicId::ThrowTypeError)
    }

    false_function!(is_proxy_object);
    none_function!(to_proxy_object);
}
