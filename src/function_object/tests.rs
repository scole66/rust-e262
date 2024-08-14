use super::*;
use crate::parser::testhelp::*;
use crate::tests::*;
use ahash::AHashMap;
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
    fn instantiate_function_object() {
        let src = "function *a(){}";
        let fd = Maker::new(src).generator_declaration();
        setup_test_agent();
        let global_env = {
            let realm_rc = current_realm_record().unwrap();
            let realm = realm_rc.borrow();
            realm.global_env.as_ref().unwrap().clone() as Rc<dyn EnvironmentRecord>
        };

        fd.instantiate_function_object(global_env, None, false, src).unwrap();
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

#[test_case(|| ECMAScriptValue::Undefined, &[] => serr("TypeError: Function.prototype.call requires that 'this' be a function"); "bad this")]
#[test_case(|| ECMAScriptValue::from(intrinsic(IntrinsicId::Number)), &[ECMAScriptValue::Undefined, ECMAScriptValue::from(10)] => vok(10); "built-in function call")]
fn function_prototype_call(
    get_this: impl FnOnce() -> ECMAScriptValue,
    args: &[ECMAScriptValue],
) -> Result<ECMAScriptValue, String> {
    setup_test_agent();
    let this_value = get_this();
    super::function_prototype_call(&this_value, None, args).map_err(unwind_any_error)
}

#[test_case(super::function_prototype_bind => panics "not yet implemented"; "function_prototype_bind")]
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
    #[test_case(|| "bob" => "String(bob)"; "string slice")]
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

fn make_working_function_object() -> Object {
    // Make a real function object, equivalent to:
    // function f(radix) {
    //     return this.toString(radix)
    // }
    let params_src = "(radix)";
    let body_src = "{ return this.toString(radix); }";
    let function_name = "test_function";
    let source_text = format!("function {function_name}{params_src}{body_src}");
    let env = current_realm_record().unwrap().borrow().global_env.clone().unwrap();
    let function_declaration = Maker::new(&source_text).function_declaration();
    let params = ParamSource::from(Rc::clone(&function_declaration.params));
    let body = BodySource::from(Rc::clone(&function_declaration.body));
    let realm = current_realm_record().unwrap();
    let proto = intrinsic(IntrinsicId::FunctionPrototype);
    let chunk_name = nameify(&source_text, 50);
    let mut compiled = Chunk::new(chunk_name);
    let function_data = StashedFunctionData {
        source_text: source_text.clone(),
        params: params.clone(),
        body: body.clone(),
        strict: false,
        to_compile: FunctionSource::from(function_declaration.clone()),
        this_mode: ThisLexicality::NonLexicalThis,
    };
    function_declaration.body.compile_body(&mut compiled, &source_text, &function_data).unwrap();
    for line in compiled.disassemble() {
        println!("{line}");
    }

    let fo = FunctionObject::object(
        Some(proto),
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
        &source_text,
        vec![],
        vec![],
        ClassName::Empty,
        false,
        Rc::new(compiled),
    );

    fo.create_data_property_or_throw("marker", "test_func").unwrap();
    fo
}

fn make_strict_function_object() -> Object {
    // Make a real strict function object, equivalent to:
    // function f(radix) {
    //     return this.toString(radix)
    // }
    let params_src = "(radix)";
    let body_src = "{ return this.toString(radix); }";
    let function_name = "test_function";
    let source_text = format!("function {function_name}{params_src}{body_src}");
    let env = current_realm_record().unwrap().borrow().global_env.clone().unwrap();
    let function_declaration = Maker::new(&source_text).function_declaration();
    let params = ParamSource::from(Rc::clone(&function_declaration.params));
    let body = BodySource::from(Rc::clone(&function_declaration.body));
    let realm = current_realm_record().unwrap();
    let proto = intrinsic(IntrinsicId::FunctionPrototype);
    let chunk_name = nameify(&source_text, 50);
    let mut compiled = Chunk::new(chunk_name);
    let function_data = StashedFunctionData {
        source_text: source_text.clone(),
        params: params.clone(),
        body: body.clone(),
        strict: true,
        to_compile: FunctionSource::from(function_declaration.clone()),
        this_mode: ThisLexicality::NonLexicalThis,
    };
    function_declaration.body.compile_body(&mut compiled, &source_text, &function_data).unwrap();
    for line in compiled.disassemble() {
        println!("{line}");
    }

    let fo = FunctionObject::object(
        Some(proto),
        env,
        None,
        params,
        body,
        ConstructorKind::Base,
        realm,
        None,
        ThisMode::Strict,
        true,
        None,
        &source_text,
        vec![],
        vec![],
        ClassName::Empty,
        false,
        Rc::new(compiled),
    );

    fo.create_data_property_or_throw("marker", "test_func").unwrap();
    fo
}

fn make_arrow_function_object() -> Object {
    // Make a real function object, equivalent to:
    // x => x * 2
    let source_text = "x => x * 2".to_string();
    let env = current_realm_record().unwrap().borrow().global_env.clone().unwrap();
    let function_declaration = Maker::new(&source_text).arrow_function();
    let params = ParamSource::from(Rc::clone(&function_declaration.parameters));
    let body = BodySource::from(Rc::clone(&function_declaration.body));
    let realm = current_realm_record().unwrap();
    let proto = intrinsic(IntrinsicId::FunctionPrototype);
    let chunk_name = nameify(&source_text, 50);
    let mut compiled = Chunk::new(chunk_name);
    let function_data = StashedFunctionData {
        source_text: source_text.clone(),
        params: params.clone(),
        body: body.clone(),
        strict: true,
        to_compile: FunctionSource::from(function_declaration.clone()),
        this_mode: ThisLexicality::LexicalThis,
    };
    function_declaration.body.compile_body(&mut compiled, &source_text, &function_data).unwrap();
    for line in compiled.disassemble() {
        println!("{line}");
    }

    let fo = FunctionObject::object(
        Some(proto),
        env,
        None,
        params,
        body,
        ConstructorKind::Base,
        realm,
        None,
        ThisMode::Lexical,
        true,
        None,
        &source_text,
        vec![],
        vec![],
        ClassName::Empty,
        false,
        Rc::new(compiled),
    );

    fo.create_data_property_or_throw("marker", "test_func").unwrap();
    fo
}

mod function_object {
    use super::*;
    use test_case::test_case;

    fn make() -> Object {
        let env = current_realm_record().unwrap().borrow().global_env.clone().unwrap();
        let params = ParamSource::FormalParameters(Maker::new("(a, b, c)").formal_parameters());
        let body = BodySource::Function(Maker::new("{ return a + b + c; }").function_body());
        let realm = current_realm_record().unwrap();
        let proto = intrinsic(IntrinsicId::FunctionPrototype);
        proto.set("proto_sentinel", true, true).unwrap();
        FunctionObject::object(
            Some(proto),
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

    false_function!(is_arguments_object);
    false_function!(is_array_object);
    false_function!(is_bigint_object);
    false_function!(is_date_object);
    false_function!(is_generator_object);
    false_function!(is_plain_object);
    false_function!(is_proxy_object);
    false_function!(is_regexp_object);
    false_function!(is_string_object);
    false_function!(is_symbol_object);
    none_function!(to_arguments_object);
    none_function!(to_array_object);
    none_function!(to_bigint_object);
    none_function!(to_boolean_obj);
    none_function!(to_error_obj);
    none_function!(to_for_in_iterator);
    none_function!(to_generator_object);
    none_function!(to_number_obj);
    none_function!(to_proxy_object);
    none_function!(to_string_obj);
    none_function!(to_symbol_obj);

    default_get_prototype_of_test!(FunctionPrototype);
    default_set_prototype_of_test!();
    default_is_extensible_test!();
    default_prevent_extensions_test!();
    default_delete_test!();
    default_id_test!();
    default_has_property_test!();
    default_uses_ordinary_get_prototype_of_test!();
    default_get_own_property_test!();
    default_get_test!(|| PropertyKey::from("proto_sentinel"), ECMAScriptValue::from(true));
    default_set_test!();
    default_own_property_keys_test!();

    #[test]
    fn to_callable_obj() {
        setup_test_agent();
        let obj = make();
        let id_1 = obj.o.id();
        let callable = obj.o.to_callable_obj().unwrap();
        let id_2 = callable.id();
        assert_eq!(id_1, id_2);
    }

    #[test]
    fn is_callable() {
        setup_test_agent();
        let obj = make();
        assert!(obj.o.is_callable_obj());
    }

    #[test]
    fn to_callable() {
        setup_test_agent();
        let obj = make();
        let orig_id = obj.o.id();
        let callable = obj.o.to_callable_obj().unwrap();
        let new_id = callable.id();
        assert_eq!(orig_id, new_id);
    }

    #[test_case(
        || {
            let o = make(); // Our "test" function object does not have [[Construct]]
            o.create_data_property_or_throw("test_key", "test_value").unwrap();
            o
        }
        => None;
        "not a constructor"
    )]
    #[test_case(
        || {
            let o = make();
            let dup = o.clone();
            o.create_data_property_or_throw("test_key", "test_value").unwrap();
            let mut fd = dup.o.to_function_obj().unwrap().function_data().borrow_mut();
            fd.is_constructor = true;
            o
        }
        => ssome("test_value");
        "is constructor"
    )]
    fn to_constructable(make_obj: impl FnOnce() -> Object) -> Option<String> {
        setup_test_agent();
        let obj = make_obj();
        let dup = obj.clone();
        let result = dup.o.to_constructable();
        result.map(|co| {
            let val = co.get(&PropertyKey::from("test_key"), &ECMAScriptValue::from(obj)).unwrap();
            val.test_result_string()
        })
    }

    #[test_case(
        make_working_function_object,
        || {
            let o = Object::from(30.0);
            o.create_data_property_or_throw("marker", "test_this").unwrap();
            ECMAScriptValue::Object(o)
        },
        || vec![ECMAScriptValue::from(8), ECMAScriptValue::from("additional")]
        => Ok((1, svec(&["additional", "8"]), "test_func".to_string(), "test_this".to_string()));
        "not a class constructor; uses args and this"
    )]
    #[test_case(
        || {
            let o = make_working_function_object();
            let func_obj = o.clone();
            let co = func_obj.o.to_function_obj().unwrap();
            let mut data = co.function_data().borrow_mut();
            data.is_class_constructor = true;
            o
        },
        || ECMAScriptValue::Undefined,
        || vec![ECMAScriptValue::from(10)]
        => serr("TypeError: constructors must use 'new'.");
        "a class constructor"
    )]
    fn call(
        make_callable: impl FnOnce() -> Object,
        make_this: impl FnOnce() -> ECMAScriptValue,
        make_args: impl FnOnce() -> Vec<ECMAScriptValue>,
    ) -> Result<(usize, Vec<String>, String, String), String> {
        setup_runnable_state();

        let starting_ec_stack_depth = execution_context_stack_len();

        let callable_obj = make_callable();
        let duplicate = callable_obj.clone();
        let this = make_this();
        let args = make_args();
        let callable = duplicate.o.to_callable_obj().ok_or_else(|| "Callable wasn't actually callable".to_string())?;
        callable.call(&callable_obj, &this, &args);
        let stack_depth_delta = execution_context_stack_len() - starting_ec_stack_depth;
        let arg_count = usize::try_from(
            ECMAScriptValue::try_from(ec_peek(0).ok_or_else(|| "Stack was empty".to_string())??)
                .map_err(|e| e.to_string())?
                .to_length()?,
        )
        .map_err(|e| e.to_string())?;
        let args_on_stack = itertools::repeat_n((), arg_count)
            .enumerate()
            .map(|idx| {
                Ok(ECMAScriptValue::try_from(
                    ec_peek(idx.0 + 1).ok_or_else(|| "Stack didn't have enough items".to_string())??,
                )
                .map_err(|e| e.to_string())?
                .test_result_string())
            })
            .collect::<Result<Vec<_>, String>>()?;
        let func_on_stack =
            ECMAScriptValue::try_from(ec_peek(arg_count + 1).ok_or_else(|| "Stack not deep enough".to_string())??)
                .map_err(|e| e.to_string())?;
        let func_on_stack_obj = Object::try_from(func_on_stack).map_err(|e| e.to_string())?;
        let func_marker = func_on_stack_obj.get(&PropertyKey::from("marker"))?.test_result_string();
        let local_env = current_lexical_environment().ok_or_else(|| "Environment missing".to_string())?;
        let this_binding = Object::try_from(local_env.get_this_binding()?).map_err(|e| e.to_string())?;
        let this_marker = this_binding.get(&PropertyKey::from("marker"))?.test_result_string();

        Ok((stack_depth_delta, args_on_stack, func_marker, this_marker))
    }

    type ConstructorTestSuccess = (usize, Vec<String>, String, Option<String>, String);
    #[test_case(
        make_working_function_object,
        Vec::new,
        DeadObject::object
        => serr("TypeError: get called on DeadObject");
        "bad constructor"
    )]
    #[test_case(
        make_working_function_object,
        || vec![ECMAScriptValue::from(11), ECMAScriptValue::from("additional")],
        || {
            let proto = intrinsic(IntrinsicId::NumberPrototype);
            proto.create_data_property_or_throw("marker", "test-number-proto").unwrap();
            intrinsic(IntrinsicId::Number)
        }
        => Ok(
            (
                1,
                svec(&["additional", "11"]),
                "test_func".to_string(),
                ssome("test-number-proto"),
                "function test_function(radix){ return this.toSt...".to_string()
            )
        );
        "standard constructor"
    )]
    #[test_case(
        || {
            let cstr = make_working_function_object();
            let dup = cstr.clone();
            let co = dup.o.to_function_obj().unwrap();
            let mut data = co.function_data().borrow_mut();
            data.is_class_constructor = true;
            data.constructor_kind = ConstructorKind::Derived;
            cstr
        },
        || vec![ECMAScriptValue::from(11), ECMAScriptValue::from("additional")],
        || {
            let proto = intrinsic(IntrinsicId::NumberPrototype);
            proto.create_data_property_or_throw("marker", "test-number-proto").unwrap();
            intrinsic(IntrinsicId::Number)
        }
        => Ok(
            (
                1,
                svec(&["additional", "11"]),
                "test_func".to_string(),
                None,
                "function test_function(radix){ return this.toSt...".to_string()
            )
        );
        "derived constructor"
    )]
    fn construct(
        make_obj: impl FnOnce() -> Object,
        make_args: impl FnOnce() -> Vec<ECMAScriptValue>,
        make_nt: impl FnOnce() -> Object,
    ) -> Result<ConstructorTestSuccess, String> {
        setup_runnable_state();

        let starting_ec_stack_depth = execution_context_stack_len();
        let callable_obj = make_obj();
        let args = make_args();
        let new_target = make_nt();
        let duplicate = callable_obj.clone();
        let callable = duplicate.o.to_callable_obj().ok_or_else(|| "Callable wasn't actually callable".to_string())?;
        callable.construct(&callable_obj, &args, &new_target);
        let stack_depth_delta = execution_context_stack_len() - starting_ec_stack_depth;
        let arg_count = usize::try_from(
            ECMAScriptValue::try_from(ec_peek(0).ok_or_else(|| "Stack was empty".to_string())??)
                .map_err(|e| e.to_string())?
                .to_length()?,
        )
        .map_err(|e| e.to_string())?;
        let args_on_stack = itertools::repeat_n((), arg_count)
            .enumerate()
            .map(|idx| {
                Ok(ECMAScriptValue::try_from(
                    ec_peek(idx.0 + 1).ok_or_else(|| "Stack didn't have enough items".to_string())??,
                )
                .map_err(|e| e.to_string())?
                .test_result_string())
            })
            .collect::<Result<Vec<_>, String>>()?;
        let func_on_stack =
            ECMAScriptValue::try_from(ec_peek(arg_count + 1).ok_or_else(|| "Stack not deep enough".to_string())??)
                .map_err(|e| e.to_string())?;
        let func_on_stack_obj = Object::try_from(func_on_stack).map_err(|e| e.to_string())?;
        let func_marker = func_on_stack_obj.get(&PropertyKey::from("marker"))?.test_result_string();
        let this_on_stack =
            ECMAScriptValue::try_from(ec_peek(arg_count + 2).ok_or_else(|| "Stack not deep enough".to_string())??)
                .map_err(|e| e.to_string())?;
        let this_marker = match this_on_stack {
            ECMAScriptValue::Object(obj) => Some(obj.get(&PropertyKey::from("marker"))?.test_result_string()),
            ECMAScriptValue::Null => None,
            _ => {
                return Err("Improper `this` value".to_string());
            }
        };
        let constructor_env: Rc<dyn EnvironmentRecord> = ec_peek(arg_count + 3)
            .ok_or_else(|| "Stack not deep enough".to_string())??
            .try_into()
            .map_err(|e: anyhow::Error| e.to_string())?;

        Ok((stack_depth_delta, args_on_stack, func_marker, this_marker, constructor_env.name()))
    }
}

mod built_in_function_object {
    use super::*;
    use test_case::test_case;

    #[allow(clippy::unnecessary_wraps)]
    fn behavior(_: &ECMAScriptValue, _: Option<&Object>, _: &[ECMAScriptValue]) -> Completion<ECMAScriptValue> {
        Ok(ECMAScriptValue::Undefined)
    }
    fn make() -> Object {
        let o = create_builtin_function(behavior, false, 0.0, PropertyKey::from("f"), &[], None, None, None);
        let proto = o.o.get_prototype_of().unwrap().unwrap();
        proto.set("proto_sentinel", true, true).unwrap();
        o
    }

    default_get_prototype_of_test!(FunctionPrototype);
    default_set_prototype_of_test!();
    default_is_extensible_test!();
    default_prevent_extensions_test!();
    default_delete_test!();
    default_id_test!();
    default_has_property_test!();
    default_uses_ordinary_get_prototype_of_test!();
    default_get_own_property_test!();
    default_get_test!(|| PropertyKey::from("proto_sentinel"), ECMAScriptValue::from(true));

    false_function!(is_arguments_object);
    false_function!(is_array_object);
    false_function!(is_bigint_object);
    false_function!(is_date_object);
    false_function!(is_generator_object);
    false_function!(is_plain_object);
    false_function!(is_proxy_object);
    false_function!(is_regexp_object);
    false_function!(is_string_object);
    false_function!(is_symbol_object);
    none_function!(to_arguments_object);
    none_function!(to_array_object);
    none_function!(to_bigint_object);
    none_function!(to_boolean_obj);
    none_function!(to_error_obj);
    none_function!(to_for_in_iterator);
    none_function!(to_function_obj);
    none_function!(to_generator_object);
    none_function!(to_number_obj);
    none_function!(to_proxy_object);
    none_function!(to_string_obj);
    none_function!(to_symbol_obj);

    #[test_case::test_case(
        776, "numberly"
        => (
            true,
            [
                (
                    PropertyKey::from("numberly"),
                    IdealizedPropertyDescriptor {
                        configurable: true,
                        enumerable: true,
                        writable: Some(true),
                        value: Some(ECMAScriptValue::from(776)),
                        get: None,
                        set: None
                    }
                ),
                (
                    PropertyKey::from("name"),
                    IdealizedPropertyDescriptor {
                        configurable: true,
                        enumerable: false,
                        writable: Some(false),
                        value: Some(ECMAScriptValue::from("f")),
                        get: None,
                        set: None
                    }
                ),
                (
                    PropertyKey::from("length"),
                    IdealizedPropertyDescriptor {
                        configurable: true,
                        enumerable: false,
                        writable: Some(false),
                        value: Some(ECMAScriptValue::Number(0.0)),
                        get: None,
                        set: None
                    }
                )
            ].into_iter().collect::<AHashMap<PropertyKey, IdealizedPropertyDescriptor>>()
        ); "ordinary set"
    )]
    fn set(
        new_val: impl Into<ECMAScriptValue>,
        key: impl Into<PropertyKey>,
    ) -> (bool, AHashMap<PropertyKey, IdealizedPropertyDescriptor>) {
        setup_test_agent();
        let obj = make();
        let receiver = ECMAScriptValue::Object(obj.clone());
        let success = obj.o.set(key.into(), new_val.into(), &receiver).unwrap();
        let properties = obj
            .o
            .common_object_data()
            .borrow()
            .properties
            .iter()
            .map(|(a, b)| (a.clone(), IdealizedPropertyDescriptor::from(b.clone())))
            .collect::<AHashMap<_, _>>();
        (success, properties)
    }

    #[test_case::test_case(
        PotentialPropertyDescriptor::new()
            .value(ECMAScriptValue::from(67))
            .writable(true)
            .configurable(true)
            .enumerable(true),
        "sixty-seven"
        => (
            true,
            [
                (
                    PropertyKey::from("sixty-seven"),
                    IdealizedPropertyDescriptor {
                        configurable: true,
                        enumerable: true,
                        writable: Some(true),
                        value: Some(ECMAScriptValue::from(67)),
                        get: None,
                        set: None
                    }
                ),
                (
                    PropertyKey::from("name"),
                    IdealizedPropertyDescriptor {
                        configurable: true,
                        enumerable: false,
                        writable: Some(false),
                        value: Some(ECMAScriptValue::from("f")),
                        get: None,
                        set: None
                    }
                ),
                (
                    PropertyKey::from("length"),
                    IdealizedPropertyDescriptor {
                        configurable: true,
                        enumerable: false,
                        writable: Some(false),
                        value: Some(ECMAScriptValue::Number(0.0)),
                        get: None,
                        set: None
                    }
                )
                ].into_iter().collect::<AHashMap<PropertyKey, IdealizedPropertyDescriptor>>()
            ); "ordinary property"
    )]
    fn define_own_property(
        new_value: PotentialPropertyDescriptor,
        key: &str,
    ) -> (bool, AHashMap<PropertyKey, IdealizedPropertyDescriptor>) {
        setup_test_agent();
        let obj = make();

        let success = obj.o.define_own_property(key.into(), new_value).unwrap();
        let properties = obj
            .o
            .common_object_data()
            .borrow()
            .properties
            .iter()
            .map(|(a, b)| (a.clone(), IdealizedPropertyDescriptor::from(b.clone())))
            .collect::<AHashMap<_, _>>();
        (success, properties)
    }

    #[test]
    fn own_property_keys() {
        setup_test_agent();
        let obj = make();

        let to_prim = wks(WksId::ToPrimitive);
        let species = wks(WksId::Species);

        obj.o
            .define_own_property(
                "60".into(),
                PotentialPropertyDescriptor::new().value("q").writable(true).enumerable(true).configurable(true),
            )
            .unwrap();
        obj.o
            .define_own_property(
                "6".into(),
                PotentialPropertyDescriptor::new().value("s").writable(true).enumerable(true).configurable(true),
            )
            .unwrap();
        obj.o
            .define_own_property(
                "zebra".into(),
                PotentialPropertyDescriptor::new().value(0).writable(true).enumerable(true).configurable(true),
            )
            .unwrap();
        obj.o
            .define_own_property(
                "alpha".into(),
                PotentialPropertyDescriptor::new().value(1).writable(true).enumerable(true).configurable(true),
            )
            .unwrap();
        obj.o
            .define_own_property(
                to_prim.clone().into(),
                PotentialPropertyDescriptor::new().value(2).writable(true).enumerable(true).configurable(true),
            )
            .unwrap();
        obj.o
            .define_own_property(
                species.clone().into(),
                PotentialPropertyDescriptor::new().value(3).writable(true).enumerable(true).configurable(true),
            )
            .unwrap();

        let keys = obj.o.own_property_keys().unwrap();

        assert_eq!(
            keys,
            vec![
                "6".into(),
                "60".into(),
                "length".into(),
                "name".into(),
                "zebra".into(),
                "alpha".into(),
                to_prim.into(),
                species.into()
            ]
        );
    }

    #[test]
    fn is_callable_obj() {
        setup_test_agent();
        let obj = make();
        assert!(obj.o.is_callable_obj());
    }

    #[test]
    fn to_callable_obj() {
        setup_test_agent();
        let obj = make();
        let id_1 = obj.o.id();
        let callable = obj.o.to_callable_obj().unwrap();
        let id_2 = callable.id();
        assert_eq!(id_1, id_2);
    }

    #[test_case(|| intrinsic(IntrinsicId::ThrowTypeError) => None; "not a constructor")]
    #[test_case(
        || {
            let o = intrinsic(IntrinsicId::Array);
            o.create_data_property_or_throw("test_key", "test_value").unwrap();
            o
        }
        => ssome("test_value");
        "is a constructor"
    )]
    fn to_constructable(make_obj: impl FnOnce() -> Object) -> Option<String> {
        setup_test_agent();
        let obj = make_obj();
        let dup = obj.clone();
        let result = dup.o.to_constructable();
        result.map(|co| {
            let val = co.get(&PropertyKey::from("test_key"), &ECMAScriptValue::from(obj)).unwrap();
            val.test_result_string()
        })
    }

    #[test_case(make => panics "end_evaluation called for builtin callable"; "always panics")]
    fn end_evaluation(make_obj: impl FnOnce() -> Object) {
        setup_test_agent();
        let obj = make_obj();
        let callable = obj.o.to_callable_obj().unwrap();
        callable.end_evaluation(Ok(NormalCompletion::from(true)));
    }

    #[test_case(|| () => panics "Call must return a Completion"; "empty stack")]
    #[test_case(|| ec_push(Ok(NormalCompletion::Empty)) => panics "Call must return a language value"; "non-value on stack")]
    #[test_case(|| ec_push(Err(create_type_error("test case"))) => serr("TypeError: test case"); "error on stack")]
    #[test_case(|| ec_push(Ok(NormalCompletion::from("test message"))) => sok("test message"); "value on stack")]
    fn complete_call(fill_stack: impl FnOnce()) -> Result<String, String> {
        setup_test_agent();
        fill_stack();
        let obj = make(); // The particular object doesn't matter; it's unused
        let callable = obj.o.to_callable_obj().unwrap();
        callable.complete_call().map_err(unwind_any_error).map(|v| v.test_result_string())
    }

    #[test_case(
        || (
            intrinsic(IntrinsicId::Object),
            ordinary_object_create(None, &[]),
            ECMAScriptValue::Undefined,
            vec![]
        )
        => panics "self and self_object must refer to the same object";
        "self != self_object"
    )]
    #[test_case(
        || (
            intrinsic(IntrinsicId::ThrowTypeError),
            intrinsic(IntrinsicId::ThrowTypeError),
            ECMAScriptValue::Undefined,
            vec![]
        )
        => serr("TypeError: Generic TypeError");
        "good call, but throws"
    )]
    #[test_case(
        || {
            let number_proto = intrinsic(IntrinsicId::NumberPrototype);
            let number_tostring = to_object(number_proto.get(&"toString".into()).unwrap()).unwrap();
            (
                number_tostring.clone(),
                number_tostring,
                ECMAScriptValue::Object(to_object(ECMAScriptValue::from(100.0)).unwrap()),
                vec![ECMAScriptValue::from(16)]
            )
        }
        => sok("64");
        "good call, using this and arguments"
    )]
    fn call(
        make_args: impl FnOnce() -> (Object, Object, ECMAScriptValue, Vec<ECMAScriptValue>),
    ) -> Result<String, String> {
        setup_test_agent();
        let (callee, callee_obj, this, args) = make_args();
        let callable = callee.o.to_callable_obj().unwrap();
        callable.call(&callee_obj, &this, &args);
        ec_peek(0)
            .unwrap()
            .map(|nc| ECMAScriptValue::try_from(nc).unwrap().test_result_string())
            .map_err(unwind_any_error)
    }

    #[test_case(
        || (
            intrinsic(IntrinsicId::Object),
            ordinary_object_create(None, &[]),
            vec![],
            ordinary_object_create(None, &[])
        )
        => panics "self and self_object must refer to the same object";
        "self != self_object"
    )]
    #[test_case(
        || (
            intrinsic(IntrinsicId::Object),
            intrinsic(IntrinsicId::Object),
            vec![ECMAScriptValue::Number(300.0)],
            intrinsic(IntrinsicId::Object),
        )
        => sok("300");
        "new Object(300)"
    )]
    fn construct(make_args: impl FnOnce() -> (Object, Object, Vec<ECMAScriptValue>, Object)) -> Result<String, String> {
        setup_test_agent();
        let (callee, callee_obj, args, new_target) = make_args();
        let constructable = callee.o.to_constructable().unwrap();
        constructable.construct(&callee_obj, &args, &new_target);
        ec_peek(0)
            .unwrap()
            .map(|nc| to_string(ECMAScriptValue::try_from(nc).unwrap()).unwrap().to_string())
            .map_err(unwind_any_error)
    }
}

mod rc_try_from {
    use super::*;
    use test_case::test_case;

    #[test_case(
        || FunctionSource::from(Maker::new("function () { return 3; }").function_expression())
        => sok("function ( ) { return 3 ; }");
        "function expression"
    )]
    #[test_case(
        || FunctionSource::from(Maker::new("function a() {}").function_declaration())
        => serr("FunctionExpression expected");
        "not function expression"
    )]
    fn function_expression_from_function_source(
        make_source: impl FnOnce() -> FunctionSource,
    ) -> Result<String, String> {
        setup_test_agent();
        <Rc<FunctionExpression> as TryFrom<FunctionSource>>::try_from(make_source())
            .as_ref()
            .map(ToString::to_string)
            .map_err(ToString::to_string)
    }

    #[test_case(
        || FunctionSource::from(Maker::new("function a() { return 3; }").function_declaration())
        => sok("function a ( ) { return 3 ; }");
        "function declaration"
    )]
    #[test_case(
        || FunctionSource::from(Maker::new("function () {}").function_expression())
        => serr("FunctionDeclaration expected");
        "not function declaration"
    )]
    fn function_declaration_from_function_source(
        make_source: impl FnOnce() -> FunctionSource,
    ) -> Result<String, String> {
        setup_test_agent();
        <Rc<FunctionDeclaration> as TryFrom<FunctionSource>>::try_from(make_source())
            .as_ref()
            .map(ToString::to_string)
            .map_err(ToString::to_string)
    }

    #[test_case(
        || FunctionSource::from(Maker::new("static {}").class_static_block())
        => sok("static { }");
        "class static block"
    )]
    #[test_case(
        || FunctionSource::from(Maker::new("function () {}").function_expression())
        => serr("ClassStaticBody expected");
        "not class static block"
    )]
    fn class_static_block_from_function_source(make_source: impl FnOnce() -> FunctionSource) -> Result<String, String> {
        setup_test_agent();
        <Rc<ClassStaticBlock> as TryFrom<FunctionSource>>::try_from(make_source())
            .as_ref()
            .map(ToString::to_string)
            .map_err(ToString::to_string)
    }

    #[test_case(|| FunctionSource::from(Maker::new("x => 0").arrow_function()) => sok("x => 0"); "arrow function")]
    #[test_case(
        || FunctionSource::from(Maker::new("function () {}").function_expression())
        => serr("ArrowFunction expected");
        "not arrow function"
    )]
    fn arrow_function_from_function_source(make_source: impl FnOnce() -> FunctionSource) -> Result<String, String> {
        setup_test_agent();
        <Rc<ArrowFunction> as TryFrom<FunctionSource>>::try_from(make_source())
            .as_ref()
            .map(ToString::to_string)
            .map_err(ToString::to_string)
    }

    #[test_case(|| FunctionSource::from(Maker::new("x").field_definition()) => sok("x"); "field definition")]
    #[test_case(
        || FunctionSource::from(Maker::new("function () {}").function_expression())
        => serr("FieldDefinition expected");
        "not field definition"
    )]
    fn field_definition_from_function_source(make_source: impl FnOnce() -> FunctionSource) -> Result<String, String> {
        setup_test_agent();
        <Rc<FieldDefinition> as TryFrom<FunctionSource>>::try_from(make_source())
            .as_ref()
            .map(ToString::to_string)
            .map_err(ToString::to_string)
    }

    #[test_case(
        || FunctionSource::from(Maker::new("x(){}").method_definition()) => sok("x ( ) { }"); "method definition"
    )]
    #[test_case(
        || FunctionSource::from(Maker::new("function () {}").function_expression())
        => serr("MethodDefinition expected");
        "not method definition"
    )]
    fn method_definition_from_function_source(make_source: impl FnOnce() -> FunctionSource) -> Result<String, String> {
        setup_test_agent();
        <Rc<MethodDefinition> as TryFrom<FunctionSource>>::try_from(make_source())
            .as_ref()
            .map(ToString::to_string)
            .map_err(ToString::to_string)
    }

    #[test_case(|| ParamSource::from(Maker::new("a, b").formal_parameters()) => sok("a , b"); "formal parameters")]
    #[test_case(
        || ParamSource::from(Maker::new("x").arrow_parameters())
         => serr("Not FormalParameters");
        "not formal parameters"
    )]
    fn formal_parameters_from_param_source(make_source: impl FnOnce() -> ParamSource) -> Result<String, String> {
        setup_test_agent();
        <Rc<FormalParameters> as TryFrom<ParamSource>>::try_from(make_source())
            .as_ref()
            .map(ToString::to_string)
            .map_err(ToString::to_string)
    }

    #[test_case(|| BodySource::from(Maker::new("x();").function_body()) => sok("x ( ) ;"); "function body")]
    #[test_case(|| BodySource::from(Maker::new("=x").initializer()) => serr("Not a FunctionBody"); "not function body")]
    fn function_body_from_body_source(make_source: impl FnOnce() -> BodySource) -> Result<String, String> {
        setup_test_agent();
        <Rc<FunctionBody> as TryFrom<BodySource>>::try_from(make_source())
            .as_ref()
            .map(ToString::to_string)
            .map_err(ToString::to_string)
    }
}

#[test_case("alpha    beta      gamma     ", 64 => "alpha beta gamma"; "ordinary")]
#[test_case("alpha    beta      gamma     ", 10 => "alpha b..."; "trimmed")]
#[test_case("alpha be", 10 => "alpha be"; "just untrimmed")]
fn nameify(src: &str, limit: usize) -> String {
    super::nameify(src, limit)
}

mod normal_completion {
    use super::*;
    use test_case::test_case;

    #[test_case(
        || ClassName::String(JSString::from("hello"))
        => NormalCompletion::Value(ECMAScriptValue::String(JSString::from("hello")));
        "JSString"
    )]
    #[test_case(
        || ClassName::Symbol(wks(WksId::Unscopables))
        => NormalCompletion::Value(ECMAScriptValue::Symbol(wks(WksId::Unscopables)));
        "Symbol"
    )]
    #[test_case(|| ClassName::Empty => NormalCompletion::Empty; "empty")]
    fn from(make_cn: impl FnOnce() -> ClassName) -> NormalCompletion {
        setup_test_agent();
        NormalCompletion::from(make_cn())
    }
    #[test]
    fn from_private_name() {
        setup_test_agent();
        let pn = PrivateName::new("Test-Name");
        let nc = NormalCompletion::from(ClassName::Private(pn.clone()));
        assert_eq!(nc, NormalCompletion::PrivateName(pn));
    }
}

mod property_key {
    use super::*;
    use test_case::test_case;

    #[test_case(
        || FunctionName::String(JSString::from("bob"))
        => Ok(PropertyKey::from("bob"));
        "string"
    )]
    #[test_case(
        || FunctionName::Symbol(wks(WksId::Unscopables))
        => Ok(PropertyKey::from(wks(WksId::Unscopables)));
        "symbol"
    )]
    #[test_case(
        || FunctionName::PrivateName(PrivateName::new("test-name"))
        => serr("PrivateName can't be converted to PropertyKey");
        "private name"
    )]
    fn try_from(make_item: impl FnOnce() -> FunctionName) -> Result<PropertyKey, String> {
        setup_test_agent();
        let function_name = make_item();
        PropertyKey::try_from(function_name).map_err(|e| e.to_string())
    }
}

mod initialize_instance_elements {
    use super::*;
    use test_case::test_case;

    #[test_case(
        || ordinary_object_create(None, &[]),
        make_working_function_object
        => Ok(Vec::new());
        "No private fields/methods"
    )]
    #[test_case(
        || ordinary_object_create(None, &[]),
        || {
            let cstr = make_working_function_object();
            let dup = cstr.clone();
            let mut data = dup.o.to_function_obj().unwrap().function_data().borrow_mut();
            let method = PrivateElement {
                key: PrivateName::new("test-key"),
                kind: PrivateElementKind::Method{ value: ECMAScriptValue::from("test-value") }, // not really a method
            };
            data.private_methods.push(Rc::new(method));
            cstr
        }
        => Ok(svec(&["PrivateElement{PN[test-key]: Method(test-value)}"]));
        "One private method"
    )]
    #[test_case(
        || ordinary_object_create(None, &[]),
        || {
            let cstr = make_working_function_object();
            let dup = cstr.clone();
            let mut data = dup.o.to_function_obj().unwrap().function_data().borrow_mut();
            let method = PrivateElement {
                key: PrivateName::new("test-key"),
                kind: PrivateElementKind::Method{ value: ECMAScriptValue::from("test-value") }, // not really a method
            };
            data.private_methods.push(Rc::new(method.clone()));
            data.private_methods.push(Rc::new(method));
            cstr
        }
        => serr("TypeError: PrivateName already defined");
        "Duplicate private method"
    )]
    #[test_case(
        || ordinary_object_create(None, &[]),
        || {
            let cstr = make_working_function_object();
            let dup = cstr.clone();
            let mut data = dup.o.to_function_obj().unwrap().function_data().borrow_mut();
            let field = ClassFieldDefinitionRecord {};
            data.fields.push(field);
            cstr
        }
        => panics "not yet implemented";
        "One private field"
    )]
    #[test_case(
        // This test only works on the "todo" form of define_field. Rewrite this when the todo goes away.
        DeadObject::object,
        || {
            let cstr = make_working_function_object();
            let dup = cstr.clone();
            let mut data = dup.o.to_function_obj().unwrap().function_data().borrow_mut();
            let field = ClassFieldDefinitionRecord {};
            data.fields.push(field);
            cstr
        }
        => serr("TypeError: get called on DeadObject");
        "Field setting fails"
    )]
    fn f(make_this: impl FnOnce() -> Object, make_cstr: impl FnOnce() -> Object) -> Result<Vec<String>, String> {
        setup_test_agent();
        let this = make_this();
        let cstr = make_cstr();
        initialize_instance_elements(&this, &cstr).map_err(unwind_any_error).map(|()| {
            let data = this.o.common_object_data().borrow();
            data.private_elements.iter().map(|item| format!("{item}")).collect::<Vec<_>>()
        })
    }
}

mod body_source {
    use super::*;
    use test_case::test_case;

    #[test_case(|| Maker::new("1;").function_body() => "Function(1 ;)"; "Function")]
    #[test_case(|| Maker::new("2;").generator_body() => "Generator(2 ;)";  "Generator")]
    #[test_case(|| Maker::new("3;").async_function_body() => "AsyncFunction(3 ;)";  "AsyncFunction")]
    #[test_case(|| Maker::new("4;").async_generator_body() => "AsyncGenerator(4 ;)";  "AsyncGenerator")]
    #[test_case(|| Maker::new("5").concise_body() => "ConciseBody(5)";  "ConciseBody")]
    #[test_case(|| Maker::new("6").async_concise_body() => "AsyncConciseBody(6)";  "AsyncConciseBody")]
    #[test_case(|| Maker::new("=7").initializer() => "Initializer(= 7)";  "Initializer")]
    #[test_case(|| Maker::new("{}").class_static_block_body() => "ClassStaticBlockBody({ })";  "ClassStaticBlockBody")]
    fn from<T>(make_value: impl FnOnce() -> T) -> String
    where
        BodySource: From<T>,
    {
        setup_test_agent();
        let value = make_value();
        match BodySource::from(value) {
            BodySource::Function(node) => format!("Function({node})"),
            BodySource::Generator(node) => format!("Generator({node})"),
            BodySource::AsyncFunction(node) => format!("AsyncFunction({node})"),
            BodySource::AsyncGenerator(node) => format!("AsyncGenerator({node})"),
            BodySource::ConciseBody(node) => format!("ConciseBody({node})"),
            BodySource::AsyncConciseBody(node) => format!("AsyncConciseBody({node})"),
            BodySource::Initializer(node) => format!("Initializer({node})"),
            BodySource::ClassStaticBlockBody(node) => format!("ClassStaticBlockBody({node})"),
        }
    }

    #[test_case(|| BodySource::Function(Maker::new("1;").function_body()) => false; "Function not strict")]
    #[test_case(|| BodySource::Function(Maker::new("'use strict'; 1;").function_body()) => true; "Function is strict")]
    #[test_case(|| BodySource::Generator(Maker::new("2;").generator_body()) => false; "Generator not strict")]
    #[test_case(|| BodySource::Generator(Maker::new("'use strict';").generator_body()) => true; "Generator is strict")]
    #[test_case(|| BodySource::AsyncFunction(Maker::new("3;").async_function_body()) => false; "AsyncFunction not strict")]
    #[test_case(|| BodySource::AsyncFunction(Maker::new("'use strict'; 3;").async_function_body()) => true; "AsyncFunction is strict")]
    #[test_case(|| BodySource::AsyncGenerator(Maker::new("4;").async_generator_body()) => false; "AsyncGenerator not strict")]
    #[test_case(|| BodySource::AsyncGenerator(Maker::new("'use strict'; 4;").async_generator_body()) => true; "AsyncGenerator is strict")]
    #[test_case(|| BodySource::ConciseBody(Maker::new("5").concise_body()) => false; "ConciseBody not strict")]
    #[test_case(|| BodySource::ConciseBody(Maker::new("{ 'use strict'; 5; }").concise_body()) => true; "ConciseBody is strict")]
    #[test_case(|| BodySource::AsyncConciseBody(Maker::new("6").async_concise_body()) => false; "AsyncConciseBody not strict")]
    #[test_case(|| BodySource::AsyncConciseBody(Maker::new("{ 'use strict'; 6; }").async_concise_body()) => true; "AsyncConciseBody is strict")]
    #[test_case(|| BodySource::Initializer(Maker::new("=7").initializer()) => false; "Initializer not strict")]
    #[test_case(|| BodySource::ClassStaticBlockBody(Maker::new("{'use strict';}").class_static_block_body()) => false; "ClassStaticBlockBody not strict")]
    fn contains_use_strict(make_value: impl FnOnce() -> BodySource) -> bool {
        setup_test_agent();
        make_value().contains_use_strict()
    }

    #[test_case(|| BodySource::Function(Maker::new("var x=10; const a=12; let c=99;").function_body()) => svec(&["const a = 12 ;", "let c = 99 ;"]); "Function")]
    #[test_case(|| BodySource::Generator(Maker::new("var x=10; const a=12; let c=99;").generator_body()) => svec(&["const a = 12 ;", "let c = 99 ;"]); "Generator")]
    #[test_case(|| BodySource::AsyncFunction(Maker::new("var x=10; const a=12; let c=99;").async_function_body()) => svec(&["const a = 12 ;", "let c = 99 ;"]); "AsyncFunction")]
    #[test_case(|| BodySource::AsyncGenerator(Maker::new("var x=10; const a=12; let c=99;").async_generator_body()) => svec(&["const a = 12 ;", "let c = 99 ;"]); "AsyncGenerator")]
    #[test_case(|| BodySource::ConciseBody(Maker::new("{ var x=10; const a=12; let c=99; }").concise_body()) => svec(&["const a = 12 ;", "let c = 99 ;"]); "Concise")]
    #[test_case(|| BodySource::AsyncConciseBody(Maker::new("{ var x=10; const a=12; let c=99; }").async_concise_body()) => svec(&["const a = 12 ;", "let c = 99 ;"]); "AsyncConcise")]
    #[test_case(|| BodySource::Initializer(Maker::new("=0").initializer()) => svec(&[]); "Initializer")]
    #[test_case(|| BodySource::ClassStaticBlockBody(Maker::new("{ var x; }").class_static_block_body()) => svec(&[]); "ClassStaticBlockBody")]
    fn lexically_scoped_declarations(make_value: impl FnOnce() -> BodySource) -> Vec<String> {
        setup_test_agent();
        make_value().lexically_scoped_declarations().into_iter().map(|dp| format!("{dp}")).collect::<Vec<_>>()
    }

    #[test_case(|| BodySource::Function(Maker::new("var x=10; const a=12; let c=99;").function_body()) => svec(&["a", "c"]); "Function")]
    #[test_case(|| BodySource::Generator(Maker::new("var x=10; const a=12; let c=99;").generator_body()) => svec(&["a", "c"]); "Generator")]
    #[test_case(|| BodySource::AsyncFunction(Maker::new("var x=10; const a=12; let c=99;").async_function_body()) => svec(&["a", "c"]); "AsyncFunction")]
    #[test_case(|| BodySource::AsyncGenerator(Maker::new("var x=10; const a=12; let c=99;").async_generator_body()) => svec(&["a", "c"]); "AsyncGenerator")]
    #[test_case(|| BodySource::ConciseBody(Maker::new("{ var x=10; const a=12; let c=99; }").concise_body()) => svec(&["a", "c"]); "Concise")]
    #[test_case(|| BodySource::AsyncConciseBody(Maker::new("{ var x=10; const a=12; let c=99; }").async_concise_body()) => svec(&["a", "c"]); "AsyncConcise")]
    #[test_case(|| BodySource::Initializer(Maker::new("=0").initializer()) => svec(&[]); "Initializer")]
    #[test_case(|| BodySource::ClassStaticBlockBody(Maker::new("{ var x; }").class_static_block_body()) => svec(&[]); "ClassStaticBlockBody")]
    fn lexically_declared_names(make_value: impl FnOnce() -> BodySource) -> Vec<String> {
        setup_test_agent();
        make_value().lexically_declared_names().into_iter().map(String::from).collect::<Vec<_>>()
    }

    #[test_case(|| BodySource::Function(Maker::new("var x=10; const a=12; let c=99;").function_body()) => svec(&["x = 10"]); "Function")]
    #[test_case(|| BodySource::Generator(Maker::new("var x=10; const a=12; let c=99;").generator_body()) => svec(&["x = 10"]); "Generator")]
    #[test_case(|| BodySource::AsyncFunction(Maker::new("var x=10; const a=12; let c=99;").async_function_body()) => svec(&["x = 10"]); "AsyncFunction")]
    #[test_case(|| BodySource::AsyncGenerator(Maker::new("var x=10; const a=12; let c=99;").async_generator_body()) => svec(&["x = 10"]); "AsyncGenerator")]
    #[test_case(|| BodySource::ConciseBody(Maker::new("{ var x=10; const a=12; let c=99; }").concise_body()) => svec(&["x = 10"]); "Concise")]
    #[test_case(|| BodySource::AsyncConciseBody(Maker::new("{ var x=10; const a=12; let c=99; }").async_concise_body()) => svec(&["x = 10"]); "AsyncConcise")]
    #[test_case(|| BodySource::Initializer(Maker::new("=0").initializer()) => svec(&[]); "Initializer")]
    #[test_case(|| BodySource::ClassStaticBlockBody(Maker::new("{ var x; }").class_static_block_body()) => svec(&[]); "ClassStaticBlockBody")]
    fn var_scoped_declarations(make_value: impl FnOnce() -> BodySource) -> Vec<String> {
        setup_test_agent();
        make_value().var_scoped_declarations().into_iter().map(|x| format!("{x}")).collect::<Vec<_>>()
    }

    #[test_case(|| BodySource::Function(Maker::new("var x=10; const a=12; let c=99;").function_body()) => svec(&["x"]); "Function")]
    #[test_case(|| BodySource::Generator(Maker::new("var x=10; const a=12; let c=99;").generator_body()) => svec(&["x"]); "Generator")]
    #[test_case(|| BodySource::AsyncFunction(Maker::new("var x=10; const a=12; let c=99;").async_function_body()) => svec(&["x"]); "AsyncFunction")]
    #[test_case(|| BodySource::AsyncGenerator(Maker::new("var x=10; const a=12; let c=99;").async_generator_body()) => svec(&["x"]); "AsyncGenerator")]
    #[test_case(|| BodySource::ConciseBody(Maker::new("{ var x=10; const a=12; let c=99; }").concise_body()) => svec(&["x"]); "Concise")]
    #[test_case(|| BodySource::AsyncConciseBody(Maker::new("{ var x=10; const a=12; let c=99; }").async_concise_body()) => svec(&["x"]); "AsyncConcise")]
    #[test_case(|| BodySource::Initializer(Maker::new("=0").initializer()) => svec(&[]); "Initializer")]
    #[test_case(|| BodySource::ClassStaticBlockBody(Maker::new("{ var x; }").class_static_block_body()) => svec(&[]); "ClassStaticBlockBody")]
    fn var_declared_names(make_value: impl FnOnce() -> BodySource) -> Vec<String> {
        setup_test_agent();
        make_value().var_declared_names().into_iter().map(String::from).collect::<Vec<_>>()
    }

    #[test_case(|| BodySource::Function(Maker::new("var x=10; const a=12; let c=99;").function_body()) => "var x = 10 ; const a = 12 ; let c = 99 ;"; "Function")]
    #[test_case(|| BodySource::Generator(Maker::new("var x=10; const a=12; let c=99;").generator_body()) => "var x = 10 ; const a = 12 ; let c = 99 ;"; "Generator")]
    #[test_case(|| BodySource::AsyncFunction(Maker::new("var x=10; const a=12; let c=99;").async_function_body()) => "var x = 10 ; const a = 12 ; let c = 99 ;"; "AsyncFunction")]
    #[test_case(|| BodySource::AsyncGenerator(Maker::new("var x=10; const a=12; let c=99;").async_generator_body()) => "var x = 10 ; const a = 12 ; let c = 99 ;"; "AsyncGenerator")]
    #[test_case(|| BodySource::ConciseBody(Maker::new("{ var x=10; const a=12; let c=99; }").concise_body()) => "{ var x = 10 ; const a = 12 ; let c = 99 ; }"; "Concise")]
    #[test_case(|| BodySource::AsyncConciseBody(Maker::new("{ var x=10; const a=12; let c=99; }").async_concise_body()) => "{ var x = 10 ; const a = 12 ; let c = 99 ; }"; "AsyncConcise")]
    #[test_case(|| BodySource::Initializer(Maker::new("=0").initializer()) => "= 0"; "Initializer")]
    #[test_case(|| BodySource::ClassStaticBlockBody(Maker::new("{ var x; }").class_static_block_body()) => "{ var x ; }"; "ClassStaticBlockBody")]
    fn display_fmt(make_value: impl FnOnce() -> BodySource) -> String {
        setup_test_agent();
        let val = make_value();
        format!("{val}")
    }

    #[test_case(|| { let bs = BodySource::Function(Maker::new("0;").function_body()); (bs.clone(), bs) } => true; "Same Function")]
    #[test_case(|| { let bs = BodySource::Generator(Maker::new("0;").generator_body()); (bs.clone(), bs) } => true; "Same Generator")]
    #[test_case(|| { let bs = BodySource::AsyncFunction(Maker::new("0;").async_function_body()); (bs.clone(), bs) } => true; "Same AsyncFunction")]
    #[test_case(|| { let bs = BodySource::AsyncGenerator(Maker::new("0;").async_generator_body()); (bs.clone(), bs) } => true; "Same AsyncGenerator")]
    #[test_case(|| { let bs = BodySource::ConciseBody(Maker::new("{}").concise_body()); (bs.clone(), bs) } => true; "Same Concise")]
    #[test_case(|| { let bs = BodySource::AsyncConciseBody(Maker::new("{}").async_concise_body()); (bs.clone(), bs) } => true; "Same AsyncConcise")]
    #[test_case(|| { let bs = BodySource::Initializer(Maker::new("=0").initializer()); (bs.clone(), bs) } => true; "Same Initializer")]
    #[test_case(|| { let bs = BodySource::ClassStaticBlockBody(Maker::new("{}").class_static_block_body()); (bs.clone(), bs) } => true; "Same ClassStaticBlockBody")]
    #[test_case(|| ( BodySource::Function(Maker::new("0;").function_body()), BodySource::Function(Maker::new("0;").function_body()) ) => false; "Diff Function")]
    #[test_case(|| ( BodySource::Generator(Maker::new("0;").generator_body()), BodySource::Generator(Maker::new("0;").generator_body()) ) => false; "Diff Generator")]
    #[test_case(|| ( BodySource::AsyncFunction(Maker::new("0;").async_function_body()), BodySource::AsyncFunction(Maker::new("0;").async_function_body()) ) => false; "Diff AsyncFunction")]
    #[test_case(|| ( BodySource::AsyncGenerator(Maker::new("0;").async_generator_body()), BodySource::AsyncGenerator(Maker::new("0;").async_generator_body()) ) => false; "Diff AsyncGenerator")]
    #[test_case(|| ( BodySource::ConciseBody(Maker::new("{}").concise_body()), BodySource::ConciseBody(Maker::new("{}").concise_body()) ) => false; "Diff Concise")]
    #[test_case(|| ( BodySource::AsyncConciseBody(Maker::new("{}").async_concise_body()), BodySource::AsyncConciseBody(Maker::new("{}").async_concise_body()) ) => false; "Diff AsyncConcise")]
    #[test_case(|| ( BodySource::Initializer(Maker::new("=0").initializer()), BodySource::Initializer(Maker::new("=0").initializer()) ) => false; "Diff Initializer")]
    #[test_case(|| ( BodySource::ClassStaticBlockBody(Maker::new("{}").class_static_block_body()), BodySource::ClassStaticBlockBody(Maker::new("{}").class_static_block_body()) ) => false; "Diff ClassStaticBlockBody")]
    #[test_case(|| ( BodySource::Function(Maker::new("0;").function_body()), BodySource::Generator(Maker::new("0;").generator_body()) ) => false; "Function vs Generator")]
    fn eq(make_pair: impl FnOnce() -> (BodySource, BodySource)) -> bool {
        setup_test_agent();
        let (left, right) = make_pair();
        left == right
    }
}

mod function_source {
    use super::*;
    use test_case::test_case;

    #[test_case(&FunctionSource::FunctionDeclaration(Maker::new("function x(){}").function_declaration()) => "function x ( ) { }"; "FunctionDeclaration")]
    #[test_case(&FunctionSource::FunctionExpression(Maker::new("function (){}").function_expression()) => "function ( ) { }"; "FunctionExpression")]
    #[test_case(&FunctionSource::GeneratorExpression(Maker::new("function *(){}").generator_expression()) => "function * ( ) { }"; "GeneratorExpression")]
    #[test_case(&FunctionSource::AsyncGeneratorExpression(Maker::new("async function *(){}").async_generator_expression()) => "async function * ( ) { }"; "AsyncGeneratorExpression")]
    #[test_case(&FunctionSource::AsyncFunctionExpression(Maker::new("async function (){}").async_function_expression()) => "async function ( ) { }"; "AsyncFunctionExpression")]
    #[test_case(&FunctionSource::ArrowFunction(Maker::new("() => 10").arrow_function()) => "( ) => 10"; "ArrowFunction")]
    #[test_case(&FunctionSource::AsyncArrowFunction(Maker::new("async () => 10").async_arrow_function()) => "async ( ) => 10"; "AsyncArrowFunction")]
    #[test_case(&FunctionSource::MethodDefinition(Maker::new("a(){}").method_definition()) => "a ( ) { }"; "MethodDefinition")]
    #[test_case(&FunctionSource::HoistableDeclaration(Maker::new("function foo(){}").hoistable_declaration()) => "function foo ( ) { }"; "HoistableDeclaration")]
    #[test_case(&FunctionSource::FieldDefinition(Maker::new("a").field_definition()) => "a"; "FieldDefinition")]
    #[test_case(&FunctionSource::ClassStaticBlock(Maker::new("static{}").class_static_block()) => "static { }"; "ClassStaticBlock")]
    fn display_fmt(item: &FunctionSource) -> String {
        format!("{item}")
    }

    #[test_case(|| Maker::new("function (a) { return a; }").function_expression() => "function ( a ) { return a ; }"; "FunctionExpression")]
    #[test_case(|| Maker::new("x => x").arrow_function() => "x => x"; "ArrowFunction")]
    #[test_case(|| Maker::new("function x(){}").function_declaration() => "function x ( ) { }"; "FunctionDeclaration")]
    #[test_case(|| Maker::new("a").field_definition() => "a"; "FieldDefinition")]
    #[test_case(|| Maker::new("static {}").class_static_block() => "static { }"; "ClassStaticBlock")]
    #[test_case(|| Maker::new("a(){}").method_definition() => "a ( ) { }"; "MethodDefinition")]
    fn from<T>(make_value: impl FnOnce() -> T) -> String
    where
        FunctionSource: From<T>,
    {
        setup_test_agent();
        let value = make_value();
        let result = FunctionSource::from(value);
        format!("{result}")
    }

    #[test_case(
        || {
            let fs = FunctionSource::FunctionExpression(Maker::new("function a(){}").function_expression());
            (fs.clone(), fs)
        }
        => true;
        "FunctionExpression ; equal"
    )]
    #[test_case(
        || {
            let fs = FunctionSource::GeneratorExpression(Maker::new("function *a(){}").generator_expression());
            (fs.clone(), fs)
        }
        => true;
        "GeneratorExpression ; equal"
    )]
    #[test_case(
        || {
            let fs = FunctionSource::AsyncGeneratorExpression(Maker::new("async function *a(){}").async_generator_expression());
            (fs.clone(), fs)
        }
        => true;
        "AsyncGeneratorExpression ; equal"
    )]
    #[test_case(
        || {
            let fs = FunctionSource::AsyncFunctionExpression(Maker::new("async function a(){}").async_function_expression());
            (fs.clone(), fs)
        }
        => true;
        "AsyncFunctionExpression ; equal"
    )]
    #[test_case(
        || {
            let fs = FunctionSource::ArrowFunction(Maker::new("x => x").arrow_function());
            (fs.clone(), fs)
        }
        => true;
        "ArrowFunction ; equal"
    )]
    #[test_case(
        || {
            let fs = FunctionSource::AsyncArrowFunction(Maker::new("async x => x").async_arrow_function());
            (fs.clone(), fs)
        }
        => true;
        "AsyncArrowFunction ; equal"
    )]
    #[test_case(
        || {
            let fs = FunctionSource::MethodDefinition(Maker::new("a(){}").method_definition());
            (fs.clone(), fs)
        }
        => true;
        "MethodDefinition ; equal"
    )]
    #[test_case(
        || {
            let fs = FunctionSource::HoistableDeclaration(Maker::new("function a(){}").hoistable_declaration());
            (fs.clone(), fs)
        }
        => true;
        "HoistableDeclaration ; equal"
    )]
    #[test_case(
        || {
            let fs = FunctionSource::FieldDefinition(Maker::new("a").field_definition());
            (fs.clone(), fs)
        }
        => true;
        "FieldDefinition ; equal"
    )]
    #[test_case(
        || {
            let fs = FunctionSource::ClassStaticBlock(Maker::new("static {}").class_static_block());
            (fs.clone(), fs)
        }
        => true;
        "ClassStaticBlock ; equal"
    )]
    #[test_case(
        || {
            let fs = FunctionSource::FunctionDeclaration(Maker::new("function a(){}").function_declaration());
            (fs.clone(), fs)
        }
        => true;
        "FunctionDeclaration ; equal"
    )]
    #[test_case(
        || (
            FunctionSource::FieldDefinition(Maker::new("a").field_definition()),
            FunctionSource::ClassStaticBlock(Maker::new("static{}").class_static_block())
        )
        => false;
        "different"
    )]
    fn eq(make_pair: impl FnOnce() -> (FunctionSource, FunctionSource)) -> bool {
        let (left, right) = make_pair();
        left == right
    }
}

#[test_case(make_working_function_object, || None => (); "a function, but no new-target")]
#[test_case(make_working_function_object, || Some(intrinsic(IntrinsicId::Array)) => (); "a function, with new-target")]
fn prepare_for_ordinary_call(make_func: impl FnOnce() -> Object, make_nt: impl FnOnce() -> Option<Object>) {
    setup_test_agent();
    let func = make_func();
    let nt = make_nt();
    let original_depth = execution_context_stack_len();

    super::prepare_for_ordinary_call(&func, nt.clone());

    assert_eq!(execution_context_stack_len(), original_depth + 1);
    let function_data = func.o.to_function_obj().unwrap().function_data().borrow();

    AGENT.with(|agent| {
        let ec_stack = agent.execution_context_stack.borrow();
        let len = ec_stack.len();
        assert!(len > 0);
        let current = &ec_stack[len - 1];

        assert!(Rc::ptr_eq(&current.realm, &function_data.realm));
        assert_eq!(&current.script_or_module, &function_data.script_or_module);
        assert!(Rc::ptr_eq(
            current.lexical_environment.as_ref().unwrap(),
            current.variable_environment.as_ref().unwrap()
        ));
        let env = current.lexical_environment.as_ref().unwrap();
        let env_func = env.get_function_object().unwrap();
        assert_eq!(&env_func, &func);
        let env = env.as_function_environment_record().unwrap();
        assert_eq!(env.get_new_target(), nt.as_ref());
        if let (Some(left), Some(right)) =
            (current.private_environment.as_ref(), function_data.private_environment.as_ref())
        {
            assert!(Rc::ptr_eq(left, right));
        } else {
            assert!(current.private_environment.is_none());
            assert!(function_data.private_environment.is_none());
        }
    });
}

#[test_case(|| (make_working_function_object(), false), || ECMAScriptValue::Undefined => Some(sok("debug_token: present")); "undefined this; this-mode global")]
#[test_case(|| (make_working_function_object(), false), || ECMAScriptValue::Null => Some(sok("debug_token: present")); "null this; this-mode global")]
#[test_case(|| (make_working_function_object(), false), || ECMAScriptValue::Number(10.0) => Some(sok("Object: 10")); "number this; this-mode global")]
#[test_case(|| (make_arrow_function_object(), true), || ECMAScriptValue::Boolean(true) => None; "boolean this; this-mode lexical")]
#[test_case(|| (make_strict_function_object(), false), || ECMAScriptValue::from("bob") => Some(sok("bob")); "string this; this-mode strict")]
fn ordinary_call_bind_this(
    make_func: impl FnOnce() -> (Object, bool),
    make_this: impl FnOnce() -> ECMAScriptValue,
) -> Option<Result<String, String>> {
    setup_runnable_state();
    let (func, is_lexical) = make_func();
    let this = make_this();
    super::prepare_for_ordinary_call(&func, None);

    let local_env = current_lexical_environment().unwrap();
    let prior_this = if is_lexical { None } else { Some(local_env.get_this_binding()) };

    if let Some(value) = prior_this {
        assert!(value.is_err());
    }

    super::ordinary_call_bind_this(&func, this.clone());

    let after_this = if is_lexical { None } else { Some(local_env.get_this_binding()) };

    after_this.map(|rs| {
        rs.map_err(unwind_any_error).map(|val| {
            if val.is_object() {
                let dt = val.get(&PropertyKey::from("debug_token")).unwrap();
                if dt.is_undefined() {
                    format!("Object: {}", to_string(val).unwrap())
                } else {
                    format!("debug_token: {}", dt.test_result_string())
                }
            } else {
                val.test_result_string()
            }
        })
    })
}
