use super::*;
use crate::reference::Base;

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
        orig.clone()
    }

    #[test_case(NormalCompletion::Empty => with |s| assert_ne!(s, ""); "empty")]
    fn debug(nc: NormalCompletion) -> String {
        format!("{:?}", nc)
    }

    #[test_case(ECMAScriptValue::from(true) => NormalCompletion::Value(ECMAScriptValue::from(true)); "value")]
    #[test_case(Reference::new(Base::Unresolvable, "fantastico", false, None) => NormalCompletion::Reference(Box::new(Reference::new(Base::Unresolvable, "fantastico", false, None))); "reference")]
    fn from(value: impl Into<NormalCompletion>) -> NormalCompletion {
        value.into()
    }
}

mod abrupt_completion {
    use super::*;

    #[test]
    fn clone() {
        let value = Some(ECMAScriptValue::Number(10.0));
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
        let value = Some(ECMAScriptValue::Number(10.0));
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
