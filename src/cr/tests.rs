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
    }
}

mod abrupt_completion {
    use super::*;

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
