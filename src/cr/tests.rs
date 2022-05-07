use super::*;
use crate::reference::Base;
use test_case::test_case;

mod normal_completion {
    use super::*;
    use test_case::test_case;

    #[test_case(NormalCompletion::Empty, NormalCompletion::Value(ECMAScriptValue::Undefined) => false; "not equal")]
    #[test_case(NormalCompletion::Value(ECMAScriptValue::from(78.0)), NormalCompletion::Value(ECMAScriptValue::from(78)) => true; "equal")]
    fn eq(left: NormalCompletion, right: NormalCompletion) -> bool {
        left == right
    }

    #[test_case(NormalCompletion::Empty, NormalCompletion::Value(ECMAScriptValue::Undefined) => true; "not equal")]
    #[test_case(NormalCompletion::Value(ECMAScriptValue::from(78.0)), NormalCompletion::Value(ECMAScriptValue::from(78)) => false; "equal")]
    fn ne(left: NormalCompletion, right: NormalCompletion) -> bool {
        left != right
    }

    #[test_case(NormalCompletion::Empty => NormalCompletion::Empty; "empty")]
    #[test_case(NormalCompletion::Value(ECMAScriptValue::from("alice")) => NormalCompletion::Value(ECMAScriptValue::from("alice")); "value")]
    #[test_case(NormalCompletion::Reference(Box::new(Reference::new(Base::Unresolvable, "alice", false, None))) => NormalCompletion::Reference(Box::new(Reference::new(Base::Unresolvable, "alice", false, None))); "reference")]
    fn clone(orig: NormalCompletion) -> NormalCompletion {
        #[allow(clippy::redundant_clone)]
        orig.clone()
    }

    #[test_case(NormalCompletion::Empty => with |s| assert_ne!(s, ""); "empty")]
    fn debug(nc: NormalCompletion) -> String {
        format!("{:?}", nc)
    }

    #[test_case(ECMAScriptValue::from(true) => NormalCompletion::Value(ECMAScriptValue::from(true)); "value")]
    #[test_case(Reference::new(Base::Unresolvable, "fantastico", false, None) => NormalCompletion::Reference(Box::new(Reference::new(Base::Unresolvable, "fantastico", false, None))); "reference")]
    #[test_case(true => NormalCompletion::Value(ECMAScriptValue::from(true)); "from bool")]
    fn from(value: impl Into<NormalCompletion>) -> NormalCompletion {
        value.into()
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

        mod object {
            use super::*;
            use crate::object::ordinary_object_create;
            use crate::tests::test_agent;
            use test_case::test_case;

            #[test_case(NormalCompletion::Empty => Err("Not a language value!".to_string()); "empty")]
            #[test_case(NormalCompletion::Reference(Box::new(Reference::new(Base::Unresolvable, "a", false, None))) => Err("Not a language value!".to_string()); "reference")]
            #[test_case(NormalCompletion::from(103) => Err("Only object values may be converted to true objects".to_string()); "value, but not object")]
            fn not(n: NormalCompletion) -> Result<Object, String> {
                n.try_into().map_err(|e: anyhow::Error| e.to_string())
            }

            #[test]
            fn actual() {
                let mut agent = test_agent();
                let obj = ordinary_object_create(&mut agent, None, &[]);
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

    #[test_case(NormalCompletion::Empty => "[empty]"; "empty")]
    #[test_case(NormalCompletion::from(103) => "103"; "value")]
    #[test_case(NormalCompletion::Reference(Box::new(Reference::new(Base::Unresolvable, "a", false, None))) => "Ref(unresolvable->a)"; "non-strict reference")]
    #[test_case(NormalCompletion::Reference(Box::new(Reference::new(Base::Unresolvable, "b", true, None))) => "SRef(unresolvable->b)"; "strict reference")]
    fn display(n: NormalCompletion) -> String {
        format!("{n}")
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
        assert_ne!(format!("{:?}", first), "");
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

    #[test_case(AbruptCompletion::Return{value: 67.into()} => "Return{67}"; "abrupt return")]
    #[test_case(AbruptCompletion::Throw{value: 99.into()} => "Throw{99}"; "abrupt throw")]
    #[test_case(AbruptCompletion::Break{value: ().into(), target: None} => "Break{}"; "break with no data")]
    #[test_case(AbruptCompletion::Break{value: ().into(), target: Some("tgt".into())} => "Break{T:tgt}"; "break with target")]
    #[test_case(AbruptCompletion::Break{value: 102.into(), target: None} => "Break{V:102}"; "break with value")]
    #[test_case(AbruptCompletion::Break{value: 10.into(), target: Some("xx".into())} => "Break{V:10,T:xx}"; "break with value and target")]
    #[test_case(AbruptCompletion::Continue{value: ().into(), target: None} => "Continue{}"; "Continue with no data")]
    #[test_case(AbruptCompletion::Continue{value: ().into(), target: Some("tgt".into())} => "Continue{T:tgt}"; "Continue with target")]
    #[test_case(AbruptCompletion::Continue{value: 102.into(), target: None} => "Continue{V:102}"; "Continue with value")]
    #[test_case(AbruptCompletion::Continue{value: 10.into(), target: Some("xx".into())} => "Continue{V:10,T:xx}"; "Continue with value and target")]
    fn display(ac: AbruptCompletion) -> String {
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
