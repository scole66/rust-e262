use super::*;
use crate::tests::test_agent;
use crate::values::{PrivateName, PropertyKey};

mod base {
    use super::*;

    #[test]
    fn debug() {
        let b = Base::Value(ECMAScriptValue::from(33));
        assert_ne!(format!("{:?}", b), "");
    }
}

mod referenced_name {
    use super::*;

    #[test]
    fn debug() {
        let rn = ReferencedName::String(JSString::from("apple"));
        assert_ne!(format!("{:?}", rn), "");
    }

    mod partial_eq {
        use super::*;

        fn setup() -> (ReferencedName, ReferencedName, ReferencedName) {
            (ReferencedName::String(JSString::from("apple")), ReferencedName::PrivateName(PrivateName::new("apple")), ReferencedName::String(JSString::from("apple")))
        }

        #[test]
        fn ne() {
            let (rn1, rn2, rn3) = setup();
            assert_eq!(rn1 != rn2, true);
            assert_eq!(rn1 != rn3, false);
            assert_eq!(rn2 != rn3, true);
        }
        #[test]
        fn eq() {
            let (rn1, rn2, rn3) = setup();
            assert_eq!(rn1 == rn2, false);
            assert_eq!(rn1 == rn3, true);
            assert_eq!(rn2 == rn3, false);
        }
    }

    mod from {
        use super::*;

        #[test]
        fn str_slice() {
            let rn = ReferencedName::from("orange");
            assert_eq!(rn, ReferencedName::String(JSString::from("orange")));
        }
        #[test]
        fn string() {
            let rn = ReferencedName::from(String::from("orange"));
            assert_eq!(rn, ReferencedName::String(JSString::from("orange")));
        }
        #[test]
        fn jsstring() {
            let rn = ReferencedName::from(JSString::from("orange"));
            assert_eq!(rn, ReferencedName::String(JSString::from("orange")));
        }
        #[test]
        fn symbol() {
            let mut agent = test_agent();
            let sym = Symbol::new(&mut agent, None);
            let rn = ReferencedName::from(sym.clone());
            assert_eq!(rn, ReferencedName::Symbol(sym));
        }
        #[test]
        fn privatename() {
            let pn = PrivateName::new("a");
            let rn = ReferencedName::from(pn.clone());
            assert_eq!(rn, ReferencedName::PrivateName(pn));
        }
    }

    mod try_from {
        use super::*;

        mod jsstring {
            use super::*;
            #[test]
            fn string() {
                let rn = ReferencedName::from("a thing");
                let s: JSString = rn.try_into().unwrap();
                assert_eq!(s, JSString::from("a thing"));
            }
            #[test]
            fn symbol() {
                let mut agent = test_agent();
                let sym = Symbol::new(&mut agent, None);
                let rn = ReferencedName::from(sym.clone());
                let err = JSString::try_from(rn).unwrap_err();
                assert_eq!(err, "invalid string");
            }
            #[test]
            fn privatename() {
                let rn = ReferencedName::PrivateName(PrivateName::new("blue"));
                let err = JSString::try_from(rn).unwrap_err();
                assert_eq!(err, "invalid string");
            }
        }

        mod propertykey {
            use super::*;

            #[test]
            fn string() {
                let rn = ReferencedName::from("str");
                let pk: PropertyKey = rn.try_into().unwrap();
                assert_eq!(pk, PropertyKey::String(JSString::from("str")));
            }
            #[test]
            fn symbol() {
                let mut agent = test_agent();
                let sym = Symbol::new(&mut agent, None);
                let rn = ReferencedName::from(sym.clone());
                let pk: PropertyKey = rn.try_into().unwrap();
                assert_eq!(pk, PropertyKey::Symbol(sym));
            }
            #[test]
            fn privatename() {
                let rn = ReferencedName::PrivateName(PrivateName::new("blue"));
                let err = PropertyKey::try_from(rn).unwrap_err();
                assert_eq!(err, "invalid property key");
            }
        }
    }
}

mod reference {
    use super::*;

    #[test]
    fn debug() {
        let r = Reference { base: Base::Unresolvable, referenced_name: ReferencedName::String(JSString::from("name")), strict: false, this_value: None };
        assert_ne!(format!("{:?}", r), "");
    }

    #[test]
    fn new() {
        let r = Reference::new(Base::Unresolvable, JSString::from("anobject"), false, Some(ECMAScriptValue::from(999)));
        assert!(matches!(r.base, Base::Unresolvable));
        assert_eq!(r.referenced_name, ReferencedName::from("anobject"));
        assert_eq!(r.strict, false);
        assert_eq!(r.this_value, Some(ECMAScriptValue::from(999)));
    }

    mod is_property_reference {
        use super::*;

        #[test]
        fn propref() {
            let r = Reference::new(Base::Value(ECMAScriptValue::from(10)), "blue", false, None);
            assert!(r.is_property_reference());
        }

        #[test]
        fn unresolved() {
            let r = Reference::new(Base::Unresolvable, "blue", false, None);
            assert!(!r.is_property_reference());
        }
    }

    mod is_unresolvable_reference {
        use super::*;

        #[test]
        fn unresolved() {
            assert!(Reference::new(Base::Unresolvable, "blue", false, None).is_unresolvable_reference());
        }
        #[test]
        fn propref() {
            assert!(!Reference::new(Base::Value(ECMAScriptValue::from(10)), "blue", false, None).is_unresolvable_reference());
        }
    }

    mod is_super_reference {
        use super::*;

        #[test]
        fn has_this() {
            assert!(Reference::new(Base::Value(ECMAScriptValue::from(10)), "a", false, Some(ECMAScriptValue::from(1))).is_super_reference());
        }
        #[test]
        fn no_this() {
            assert!(!Reference::new(Base::Value(ECMAScriptValue::from(10)), "a", false, None).is_super_reference());
        }
    }

    mod is_private_reference {
        use super::*;

        #[test]
        fn public() {
            assert!(!Reference::new(Base::Value(ECMAScriptValue::from(10)), "a", false, None).is_private_reference());
        }
        #[test]
        fn private() {
            assert!(Reference::new(Base::Value(ECMAScriptValue::from(10)), PrivateName::new("a"), false, None).is_private_reference());
        }
    }
}
