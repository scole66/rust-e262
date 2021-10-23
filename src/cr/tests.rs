use super::*;

mod completion_info {
    use super::*;
    #[test]
    fn clone() {
        let first = CompletionInfo { value: Some(ECMAScriptValue::Boolean(true)), target: Some(JSString::from("label")) };
        let second = first.clone();
        assert_eq!(first.value, second.value);
        assert_eq!(first.target, second.target);
    }

    #[test]
    fn debug() {
        // Just for coverage. Essentially, assert that we don't panic.
        let first = CompletionInfo { value: Some(ECMAScriptValue::Boolean(true)), target: Some(JSString::from("label")) };
        assert_ne!(format!("{:?}", first), "");
    }

    #[test]
    fn eq() {
        let ci1 = CompletionInfo { value: Some(ECMAScriptValue::from("red")), target: Some(JSString::from("label")) };
        let ci2 = CompletionInfo { value: None, target: None };
        let ci3 = CompletionInfo { value: Some(ECMAScriptValue::from("red")), target: Some(JSString::from("label")) };
        assert_eq!(ci1 == ci2, false);
        assert_eq!(ci1 == ci3, true);
    }
    #[test]
    fn ne() {
        let ci1 = CompletionInfo { value: Some(ECMAScriptValue::from("red")), target: Some(JSString::from("label")) };
        let ci2 = CompletionInfo { value: None, target: None };
        let ci3 = CompletionInfo { value: Some(ECMAScriptValue::from("red")), target: Some(JSString::from("label")) };
        assert_eq!(ci1 != ci2, true);
        assert_eq!(ci1 != ci3, false);
    }
}

mod abrupt_completion {
    use super::*;

    #[test]
    fn clone() {
        let value = Some(ECMAScriptValue::Number(10.0));
        let target = Some(JSString::from("outer"));
        let first = AbruptCompletion::Break(CompletionInfo { value, target });
        let second = first.clone();
        assert!(matches!(second, AbruptCompletion::Break(_)));
        if let AbruptCompletion::Break(ci) = second {
            if let AbruptCompletion::Break(orig) = first {
                assert_eq!(ci.value, orig.value);
                assert_eq!(ci.target, orig.target);
            }
        }
    }

    #[test]
    fn debug() {
        // Just for coverage. Essentially, assert that we don't panic.
        let value = Some(ECMAScriptValue::Number(10.0));
        let target = Some(JSString::from("outer"));
        let first = AbruptCompletion::Break(CompletionInfo { value, target });
        assert_ne!(format!("{:?}", first), "");
    }

    #[test]
    fn eq() {
        let c1 = AbruptCompletion::Throw(CompletionInfo { value: Some(ECMAScriptValue::from("error message")), target: None });
        let c2 = AbruptCompletion::Return(CompletionInfo { value: Some(ECMAScriptValue::from(27)), target: None });
        let c3 = AbruptCompletion::Throw(CompletionInfo { value: Some(ECMAScriptValue::from("error message")), target: None });

        assert_eq!(c1 == c2, false);
        assert_eq!(c1 == c3, true);
    }
    #[test]
    fn ne() {
        let c1 = AbruptCompletion::Throw(CompletionInfo { value: Some(ECMAScriptValue::from("error message")), target: None });
        let c2 = AbruptCompletion::Return(CompletionInfo { value: Some(ECMAScriptValue::from(27)), target: None });
        let c3 = AbruptCompletion::Throw(CompletionInfo { value: Some(ECMAScriptValue::from("error message")), target: None });

        assert_eq!(c1 != c2, true);
        assert_eq!(c1 != c3, false);
    }
}
