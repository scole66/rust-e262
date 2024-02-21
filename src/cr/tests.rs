use super::*;
use crate::tests::*;
use num::BigInt;
use regex::Regex;
use std::rc::Rc;
use test_case::test_case;

mod normal_completion {
    use super::*;
    use test_case::test_case;

    #[test_case(&NormalCompletion::Empty, &NormalCompletion::Value(ECMAScriptValue::Undefined) => false; "not equal")]
    #[test_case(&NormalCompletion::Value(ECMAScriptValue::from(78.0)), &NormalCompletion::Value(ECMAScriptValue::from(78)) => true; "equal")]
    #[test_case(&NormalCompletion::from(67), &NormalCompletion::Empty => false; "value vs empty")]
    #[test_case(&NormalCompletion::Reference(Box::new(Reference::new(Base::Unresolvable, "alice", false, None))),
                &NormalCompletion::Reference(Box::new(Reference::new(Base::Unresolvable, "alice", false, None)))
                => true; "ref-equal")]
    #[test_case(&NormalCompletion::Reference(Box::new(Reference::new(Base::Unresolvable, "alice", false, None))),
                &NormalCompletion::from(10)
                => false; "ref-notequal")]
    #[test_case(&NormalCompletion::PrivateName(PrivateName::new("charlie")), &NormalCompletion::from(10) => false; "one pn")]
    #[test_case(&NormalCompletion::PrivateName(PrivateName::new("alice")),
                &NormalCompletion::PrivateName(PrivateName::new("bob"))
                => false; "both pn")]
    fn eq(left: &NormalCompletion, right: &NormalCompletion) -> bool {
        left == right
    }

    fn global_env() -> NormalCompletion {
        let global_env = current_realm_record().unwrap().borrow().global_env.clone().unwrap();
        NormalCompletion::Environment(global_env)
    }
    fn iterator_record() -> NormalCompletion {
        let ir = create_list_iterator_record(vec![]);
        NormalCompletion::from(ir)
    }
    fn private_element() -> NormalCompletion {
        PrivateElement {
            key: PrivateName::new("alice"),
            kind: PrivateElementKind::Method { value: ECMAScriptValue::Undefined },
        }
        .into()
    }

    #[test_case(global_env, global_env => true; "matching env")]
    #[test_case(global_env, iterator_record => false; "env no match")]
    #[test_case(iterator_record, iterator_record => false; "both ir, but not the same one")]
    #[test_case(iterator_record, || NormalCompletion::Empty => false; "ir no match")]
    #[test_case(private_element, private_element => false; "both PE, but not the same one")]
    #[test_case(private_element, || NormalCompletion::Empty => false; "PE no match")]
    fn eq2(make_left: fn() -> NormalCompletion, make_right: fn() -> NormalCompletion) -> bool {
        setup_test_agent();
        let left = make_left();
        let right = make_right();
        left == right
    }

    #[test_case(&NormalCompletion::Empty, &NormalCompletion::Value(ECMAScriptValue::Undefined) => true; "not equal")]
    #[test_case(&NormalCompletion::Value(ECMAScriptValue::from(78.0)), &NormalCompletion::Value(ECMAScriptValue::from(78)) => false; "equal")]
    fn ne(left: &NormalCompletion, right: &NormalCompletion) -> bool {
        left != right
    }

    #[test_case(&NormalCompletion::Empty => NormalCompletion::Empty; "empty")]
    #[test_case(&NormalCompletion::Value(ECMAScriptValue::from("alice")) => NormalCompletion::Value(ECMAScriptValue::from("alice")); "value")]
    #[test_case(&NormalCompletion::Reference(Box::new(Reference::new(Base::Unresolvable, "alice", false, None))) => NormalCompletion::Reference(Box::new(Reference::new(Base::Unresolvable, "alice", false, None))); "reference")]
    fn clone(orig: &NormalCompletion) -> NormalCompletion {
        #[allow(clippy::redundant_clone)]
        orig.clone()
    }

    #[test_case(&NormalCompletion::Empty => with |s| assert_ne!(s, ""); "empty")]
    fn debug(nc: &NormalCompletion) -> String {
        format!("{nc:?}")
    }

    mod from {
        use super::*;
        use test_case::test_case;

        #[test_case(ECMAScriptValue::from(true) => NormalCompletion::Value(ECMAScriptValue::from(true)); "value")]
        #[test_case(Reference::new(Base::Unresolvable, "fantastico", false, None) => NormalCompletion::Reference(Box::new(Reference::new(Base::Unresolvable, "fantastico", false, None))); "reference")]
        #[test_case(true => NormalCompletion::Value(ECMAScriptValue::from(true)); "from bool")]
        #[test_case(JSString::from("a") => NormalCompletion::Value(ECMAScriptValue::from("a")); "jsstring")]
        #[test_case(&JSString::from("a") => NormalCompletion::Value(ECMAScriptValue::from("a")); "jsstring ref")]
        #[test_case(100.0 => NormalCompletion::Value(ECMAScriptValue::from(100)); "float")]
        #[test_case(Rc::new(BigInt::from(12)) => NormalCompletion::Value(ECMAScriptValue::from(BigInt::from(12))); "bigint")]
        #[test_case(BigInt::from(27) => NormalCompletion::Value(ECMAScriptValue::from(BigInt::from(27))); "bigint, no rc")]
        #[test_case(Numeric::Number(7.0) => NormalCompletion::Value(ECMAScriptValue::from(7)); "numeric")]
        #[test_case(23_i64 => NormalCompletion::Value(ECMAScriptValue::from(23)); "from i64")]
        #[test_case(23_u32 => NormalCompletion::Value(ECMAScriptValue::from(23)); "from u32")]
        #[test_case(23_i32 => NormalCompletion::Value(ECMAScriptValue::from(23)); "from i32")]
        #[test_case(PropertyKey::from("bob") => NormalCompletion::Value(ECMAScriptValue::from("bob")); "from property key")]
        #[test_case(() => NormalCompletion::Empty; "from unit")]
        #[test_case("bob" => NormalCompletion::Value(ECMAScriptValue::from("bob")); "from &str")]
        fn simple(value: impl Into<NormalCompletion>) -> NormalCompletion {
            value.into()
        }

        #[test]
        fn object() {
            setup_test_agent();
            let obj = ordinary_object_create(None, &[]);
            let nc = NormalCompletion::from(obj.clone());

            if let NormalCompletion::Value(ECMAScriptValue::Object(result)) = nc {
                assert_eq!(result, obj);
            } else {
                panic!("Improper NC construction")
            }
        }

        #[test]
        fn iterator_record() {
            setup_test_agent();
            let ir = create_list_iterator_record(vec![]);
            let next = ir.next_method.clone();
            let obj = ir.iterator.clone();
            let done = ir.done.get();

            let nc = NormalCompletion::from(ir);
            if let NormalCompletion::IteratorRecord(recovered) = nc {
                assert_eq!(recovered.next_method, next);
                assert_eq!(recovered.iterator, obj);
                assert_eq!(recovered.done.get(), done);
            } else {
                panic!("NormalCompletion::Iterator(_) expected");
            }
        }

        #[test]
        fn rc_iterator_record() {
            setup_test_agent();
            let ir = Rc::new(create_list_iterator_record(vec![]));

            let nc = NormalCompletion::from(Rc::clone(&ir));
            if let NormalCompletion::IteratorRecord(recovered) = nc {
                assert!(Rc::ptr_eq(&recovered, &ir));
            }
        }

        #[test]
        fn private_name() {
            setup_test_agent();
            let pn = PrivateName::new("test-sentinel");

            let nc = NormalCompletion::from(pn.clone());
            let recovered = PrivateName::try_from(nc).unwrap();
            assert_eq!(recovered, pn);
        }

        #[test]
        fn symbol() {
            setup_test_agent();
            let sym = Symbol::new(Some("alice".into()));
            let nc = NormalCompletion::from(sym.clone());
            if let NormalCompletion::Value(ECMAScriptValue::Symbol(recovered)) = nc {
                assert_eq!(recovered, sym);
            } else {
                panic!("test failed; non symbol came back")
            }
        }

        #[test]
        fn private_element() {
            setup_test_agent();
            let pe = PrivateElement {
                key: PrivateName::new("private-element"),
                kind: PrivateElementKind::Field { value: RefCell::new(ECMAScriptValue::from(99)) },
            };

            let nc = NormalCompletion::from(pe.clone());

            if let NormalCompletion::PrivateElement(boxed) = nc {
                assert_eq!(&*boxed, &pe);
            } else {
                panic!("test failed; non-pe came back");
            }
        }

        mod option_pe {
            use super::*;

            #[test]
            fn none() {
                let pe: Option<PrivateElement> = None;
                let nc = NormalCompletion::from(pe);
                assert_eq!(nc, NormalCompletion::Empty);
            }

            #[test]
            fn some() {
                setup_test_agent();
                let pe = PrivateElement {
                    key: PrivateName::new("private-element"),
                    kind: PrivateElementKind::Field { value: RefCell::new(ECMAScriptValue::from(99)) },
                };

                let nc = NormalCompletion::from(Some(pe.clone()));

                if let NormalCompletion::PrivateElement(boxed) = nc {
                    assert_eq!(&*boxed, &pe);
                } else {
                    panic!("test failed; non-pe came back");
                }
            }
        }
    }

    mod try_from {
        use super::*;
        use test_case::test_case;

        #[test_case(NormalCompletion::Empty => Err("Not a language value!".to_string()); "empty")]
        #[test_case(NormalCompletion::Reference(Box::new(Reference::new(Base::Unresolvable, "fantastico", false, None))) => Err("Not a language value!".to_string()); "reference")]
        #[test_case(NormalCompletion::from("blue") => Ok(ECMAScriptValue::from("blue")); "value")]
        fn value(n: NormalCompletion) -> Result<ECMAScriptValue, String> {
            n.try_into().map_err(|a: anyhow::Error| a.to_string())
        }

        #[test_case(NormalCompletion::Empty => Ok(None); "empty")]
        #[test_case(NormalCompletion::Reference(Box::new(Reference::new(Base::Unresolvable, "fantastico", false, None))) => Err("Not a language value!".to_string()); "reference")]
        #[test_case(NormalCompletion::from("blue") => Ok(Some(ECMAScriptValue::from("blue"))); "value")]
        fn opt_value(n: NormalCompletion) -> Result<Option<ECMAScriptValue>, String> {
            n.try_into().map_err(|a: anyhow::Error| a.to_string())
        }

        #[test_case(NormalCompletion::Empty => Err("Not a language value!".to_string()); "empty")]
        #[test_case(NormalCompletion::Reference(Box::new(Reference::new(Base::Unresolvable, "a", false, None))) => Err("Not a language value!".to_string()); "reference")]
        #[test_case(NormalCompletion::from(103) => Err("Bad type for property key".to_string()); "value, but not key")]
        #[test_case(NormalCompletion::from("key") => Ok(PropertyKey::from("key")); "key")]
        fn property_key(n: NormalCompletion) -> Result<PropertyKey, String> {
            n.try_into().map_err(|e: anyhow::Error| e.to_string())
        }

        #[test_case(NormalCompletion::from(5) => Ok(Numeric::Number(5.0)); "from float")]
        #[test_case(NormalCompletion::from(Rc::new(BigInt::from(999))) => Ok(Numeric::BigInt(Rc::new(BigInt::from(999)))); "bigint")]
        #[test_case(NormalCompletion::Empty => Err("Not a language value!".to_string()); "not a value")]
        #[test_case(NormalCompletion::from(true) => Err("Value not numeric".to_string()); "not numeric")]
        fn numeric(n: NormalCompletion) -> Result<Numeric, String> {
            n.try_into().map_err(|e: anyhow::Error| e.to_string())
        }

        mod object {
            use super::*;
            use test_case::test_case;

            #[test_case(NormalCompletion::Empty => Err("Not a language value!".to_string()); "empty")]
            #[test_case(NormalCompletion::Reference(Box::new(Reference::new(Base::Unresolvable, "a", false, None))) => Err("Not a language value!".to_string()); "reference")]
            #[test_case(NormalCompletion::from(103) => Err("Only object values may be converted to true objects".to_string()); "value, but not object")]
            fn not(n: NormalCompletion) -> Result<Object, String> {
                n.try_into().map_err(|e: anyhow::Error| e.to_string())
            }

            #[test]
            fn actual() {
                setup_test_agent();
                let obj = ordinary_object_create(None, &[]);
                let val = ECMAScriptValue::from(obj.clone());
                let nc = NormalCompletion::from(val);
                let extracted: Object = nc.try_into().unwrap();
                assert_eq!(obj, extracted);
            }
        }
    }

    #[test_case(() => NormalCompletion::Empty; "unit")]
    fn into(src: impl Into<NormalCompletion>) -> NormalCompletion {
        src.into()
    }

    #[test_case(&NormalCompletion::Empty => "[empty]"; "empty")]
    #[test_case(&NormalCompletion::from(103) => "103"; "value")]
    #[test_case(&NormalCompletion::Reference(Box::new(Reference::new(Base::Unresolvable, "a", false, None))) => "Ref(unresolvable->a)"; "non-strict reference")]
    #[test_case(&NormalCompletion::Reference(Box::new(Reference::new(Base::Unresolvable, "b", true, None))) => "SRef(unresolvable->b)"; "strict reference")]
    #[test_case(&NormalCompletion::PrivateName(PrivateName::new("alpha")) => "PN[alpha]"; "private name")]
    fn display(n: &NormalCompletion) -> String {
        format!("{n}")
    }

    fn make_regex_validator(regex: &str) -> impl Fn(String) + '_ {
        move |actual: String| {
            let re = Regex::new(regex).unwrap();
            assert!(re.is_match(&actual));
        }
    }

    #[test_case(|| NormalCompletion::from(create_list_iterator_record(vec![]))
            => using make_regex_validator(r"^IR\(iter: <Object [0-9]+>; next: <Object [0-9]+>; unfinished\)$")
            ; "iterator record")]
    #[test_case(|| {
            let global_env = current_realm_record().unwrap().borrow().global_env.clone().unwrap();
            NormalCompletion::Environment(global_env)
        }
        => "GlobalEnvironmentRecord(realm-global)"
        ; "environment record")]
    #[test_case(
        || {
            let pe = PrivateElement {
                key: PrivateName::new("alice"),
                kind: PrivateElementKind::Method{ value: ECMAScriptValue::Undefined },
            };
            NormalCompletion::PrivateElement(Box::new(pe))
        }
        => "PrivateElement{PN[alice]: Method(undefined)}";
        "private element"
    )]
    fn display_constructed(make_nc: fn() -> NormalCompletion) -> String {
        setup_test_agent();
        let nc = make_nc();
        format!("{nc}")
    }
}

mod abrupt_completion {
    use super::*;
    use test_case::test_case;

    #[test]
    fn clone() {
        let value = NormalCompletion::from(ECMAScriptValue::Number(10.0));
        let target = Some(JSString::from("outer"));
        let first = AbruptCompletion::Break { value, target };
        let second = first.clone();
        assert!(matches!(second, AbruptCompletion::Break { .. }));
        if let AbruptCompletion::Break { value: second_value, target: second_target } = second {
            if let AbruptCompletion::Break { value: orig_value, target: orig_target } = first {
                assert_eq!(second_value, orig_value);
                assert_eq!(second_target, orig_target);
            }
        }
    }

    #[test]
    fn debug() {
        // Just for coverage. Essentially, assert that we don't panic.
        let value = NormalCompletion::from(ECMAScriptValue::Number(10.0));
        let target = Some(JSString::from("outer"));
        let first = AbruptCompletion::Break { value, target };
        assert_ne!(format!("{first:?}"), "");
    }

    #[test]
    fn eq() {
        let c1 = AbruptCompletion::Throw { value: ECMAScriptValue::from("error message") };
        let c2 = AbruptCompletion::Return { value: ECMAScriptValue::from(27) };
        let c3 = AbruptCompletion::Throw { value: ECMAScriptValue::from("error message") };

        assert_eq!(c1 == c2, false);
        assert_eq!(c1 == c3, true);
    }
    #[test]
    fn ne() {
        let c1 = AbruptCompletion::Throw { value: ECMAScriptValue::from("error message") };
        let c2 = AbruptCompletion::Return { value: ECMAScriptValue::from(27) };
        let c3 = AbruptCompletion::Throw { value: ECMAScriptValue::from("error message") };

        assert_eq!(c1 != c2, true);
        assert_eq!(c1 != c3, false);
    }

    #[test_case(&AbruptCompletion::Return{value: 67.into()} => "Return{67}"; "abrupt return")]
    #[test_case(&AbruptCompletion::Throw{value: 99.into()} => "Throw{99}"; "abrupt throw")]
    #[test_case(&AbruptCompletion::Break{value: ().into(), target: None} => "Break{}"; "break with no data")]
    #[test_case(&AbruptCompletion::Break{value: ().into(), target: Some("tgt".into())} => "Break{T:tgt}"; "break with target")]
    #[test_case(&AbruptCompletion::Break{value: 102.into(), target: None} => "Break{V:102}"; "break with value")]
    #[test_case(&AbruptCompletion::Break{value: 10.into(), target: Some("xx".into())} => "Break{V:10,T:xx}"; "break with value and target")]
    #[test_case(&AbruptCompletion::Continue{value: ().into(), target: None} => "Continue{}"; "Continue with no data")]
    #[test_case(&AbruptCompletion::Continue{value: ().into(), target: Some("tgt".into())} => "Continue{T:tgt}"; "Continue with target")]
    #[test_case(&AbruptCompletion::Continue{value: 102.into(), target: None} => "Continue{V:102}"; "Continue with value")]
    #[test_case(&AbruptCompletion::Continue{value: 10.into(), target: Some("xx".into())} => "Continue{V:10,T:xx}"; "Continue with value and target")]
    fn display(ac: &AbruptCompletion) -> String {
        format!("{ac}")
    }
}

#[test_case(Ok(NormalCompletion::Empty), NormalCompletion::Empty => Ok(NormalCompletion::Empty); "empties all over")]
#[test_case(Ok(NormalCompletion::Empty), ECMAScriptValue::from(3).into() => Ok(NormalCompletion::Value(3.into())); "old: 3, new: empty")]
#[test_case(Ok(NormalCompletion::Value("bob".into())), ECMAScriptValue::from(3).into() => Ok(NormalCompletion::Value("bob".into())); "old: 3, new: bob")]
#[test_case(Err(AbruptCompletion::Return{value: 1.into()}), ECMAScriptValue::from(3).into() => Err(AbruptCompletion::Return{value: 1.into()}); "Err return")]
#[test_case(Err(AbruptCompletion::Throw{value: 1.into()}), ECMAScriptValue::from(3).into() => Err(AbruptCompletion::Throw{value: 1.into()}); "Err throw")]
#[test_case(Err(AbruptCompletion::Break{value: NormalCompletion::from(ECMAScriptValue::from(1)), target: Some("lbl".into())}), ECMAScriptValue::from(3).into() => Err(AbruptCompletion::Break{value: NormalCompletion::from(ECMAScriptValue::from(1)), target: Some("lbl".into())}); "Err break with value")]
#[test_case(Err(AbruptCompletion::Break{value: NormalCompletion::Empty, target: Some("xyz".into())}), ECMAScriptValue::from(3).into() => Err(AbruptCompletion::Break{value: NormalCompletion::from(ECMAScriptValue::from(3)), target: Some("xyz".into())}); "Err break no value")]
#[test_case(Err(AbruptCompletion::Continue{value: NormalCompletion::from(ECMAScriptValue::from(1)), target: Some("lbl".into())}), ECMAScriptValue::from(3).into() => Err(AbruptCompletion::Continue{value: NormalCompletion::from(ECMAScriptValue::from(1)), target: Some("lbl".into())}); "Err Continue with value")]
#[test_case(Err(AbruptCompletion::Continue{value: NormalCompletion::Empty, target: Some("xyz".into())}), ECMAScriptValue::from(3).into() => Err(AbruptCompletion::Continue{value: NormalCompletion::from(ECMAScriptValue::from(3)), target: Some("xyz".into())}); "Err Continue no value")]
fn update_empty(new: FullCompletion, old: NormalCompletion) -> FullCompletion {
    super::update_empty(new, old)
}

mod throw_value {
    use super::*;
    use test_case::test_case;

    #[test_case(AbruptCompletion::Break{value: NormalCompletion::from(10), target: None} => serr("Break found when Throw expected"); "break completion")]
    #[test_case(AbruptCompletion::Continue{ value: NormalCompletion::from(10), target: None} => serr("Continue found when Throw expected"); "continue completion")]
    #[test_case(AbruptCompletion::Return{ value: 20.into() } => serr("Return found when Throw expected"); "return completion")]
    #[test_case(AbruptCompletion::Throw{ value: 99.into() } => Ok(ThrowValue(99.into())); "throw completion")]
    fn try_from_abrupt_completion(ac: AbruptCompletion) -> Result<ThrowValue, String> {
        ThrowValue::try_from(ac).map_err(|err| err.to_string())
    }

    #[test]
    fn debug() {
        assert_ne!(format!("{:?}", ThrowValue(99.into())), "");
    }

    #[test_case(&ThrowValue("hi".into()), &ThrowValue("hi".into()) => false; "equal")]
    #[test_case(&ThrowValue("hpi".into()), &ThrowValue("hi".into()) => true; "not equal")]
    fn ne(a: &ThrowValue, b: &ThrowValue) -> bool {
        a != b
    }
}

mod ecmascript_value {
    use super::*;
    use test_case::test_case;

    #[test_case(ThrowValue("sentinel".into()) => ECMAScriptValue::from("sentinel"); "typical")]
    fn from_throw_value(tv: ThrowValue) -> ECMAScriptValue {
        ECMAScriptValue::from(tv)
    }
}
