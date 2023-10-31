use super::*;
use crate::tests::*;

mod proxy_object {
    use super::*;
    use test_case::test_case;

    #[test]
    fn debug_fmt() {
        let po = ProxyObject {
            common: RefCell::new(CommonObjectData::new(None, false, PROXY_OBJECT_SLOTS)),
            proxy_handler: None,
            proxy_target: None,
        };
        assert_ne!(format!("{po:?}"), "");
    }

    #[test_case(|| ProxyObject {
        common: RefCell::new(CommonObjectData::new(None, false, PROXY_OBJECT_SLOTS)),
        proxy_handler: None,
        proxy_target: None,
    } => serr("TypeError: Proxy has been revoked"); "revoked proxy")]
    #[test_case(|| {
        let target = ordinary_object_create(None, &[]);
        let handler = ordinary_object_create(None, &[]);
        ProxyObject {
            common: RefCell::new(CommonObjectData::new(None, false, PROXY_OBJECT_SLOTS)),
            proxy_handler: Some(handler),
            proxy_target: Some(target),
        }
    } => Ok(()); "valid")]
    fn validate_non_revoked(make_po: impl FnOnce() -> ProxyObject) -> Result<(), String> {
        setup_test_agent();
        let po = make_po();
        po.validate_non_revoked().map_err(unwind_any_error)
    }

    fn cbf(
        behavior: fn(ECMAScriptValue, Option<&Object>, &[ECMAScriptValue]) -> Completion<ECMAScriptValue>,
    ) -> Object {
        create_builtin_function(
            behavior,
            false,
            0.0,
            "f".into(),
            BUILTIN_FUNCTION_SLOTS,
            current_realm_record(),
            Some(intrinsic(IntrinsicId::FunctionPrototype)),
            None,
        )
    }

    // Behaviors
    fn fn_returning_null() -> Object {
        fn behavior(
            _this_value: ECMAScriptValue,
            _new_target: Option<&Object>,
            _args: &[ECMAScriptValue],
        ) -> Completion<ECMAScriptValue> {
            Ok(ECMAScriptValue::Null)
        }
        cbf(behavior)
    }
    fn fn_returning_string() -> Object {
        fn behavior(
            _this_value: ECMAScriptValue,
            _new_target: Option<&Object>,
            _args: &[ECMAScriptValue],
        ) -> Completion<ECMAScriptValue> {
            Ok(ECMAScriptValue::from("string result"))
        }
        cbf(behavior)
    }
    fn fn_returning_alternate_object() -> Object {
        fn behavior(
            _this_value: ECMAScriptValue,
            _new_target: Option<&Object>,
            _args: &[ECMAScriptValue],
        ) -> Completion<ECMAScriptValue> {
            let alternate = ordinary_object_create(None, &[]);
            define_property_or_throw(&alternate, "test_prop", PotentialPropertyDescriptor::new().value("alternate"))
                .unwrap();
            Ok(alternate.into())
        }
        cbf(behavior)
    }
    fn fn_returning_target_proto() -> Object {
        fn behavior(
            _this_value: ECMAScriptValue,
            _new_target: Option<&Object>,
            arguments: &[ECMAScriptValue],
        ) -> Completion<ECMAScriptValue> {
            let mut args = FuncArgs::from(arguments);
            let target = Object::try_from(args.next_arg()).unwrap();
            Ok(match target.o.get_prototype_of().unwrap() {
                Some(p) => ECMAScriptValue::from(p),
                None => ECMAScriptValue::Null,
            })
        }
        cbf(behavior)
    }

    // Proxy Object creators
    fn revoked() -> Object {
        ProxyObject::object(None, None)
    }
    fn no_overrides() -> Object {
        let prototype = ordinary_object_create(None, &[]);
        define_property_or_throw(&prototype, "test_prop", PotentialPropertyDescriptor::new().value("core")).unwrap();
        let target = ordinary_object_create(Some(prototype), &[]);
        let ppd = PotentialPropertyDescriptor {
            value: Some("target".into()),
            writable: Some(true),
            configurable: Some(true),
            ..Default::default()
        };
        define_property_or_throw(&target, "test_key", ppd).unwrap();
        let handler = ordinary_object_create(None, &[]);
        ProxyObject::object(Some(target), Some(handler))
    }
    fn dead_handler() -> Object {
        let target = ordinary_object_create(None, &[]);
        let handler = DeadObject::object();
        ProxyObject::object(Some(target), Some(handler))
    }

    mod get_prototype_of {
        use super::*;
        use test_case::test_case;

        fn handler_call_throws() -> Object {
            let target = ordinary_object_create(None, &[]);
            let handler = ordinary_object_create(None, &[]);
            let thrower = intrinsic(IntrinsicId::ThrowTypeError);
            let ppd = PotentialPropertyDescriptor::new().value(thrower);
            define_property_or_throw(&handler, "getPrototypeOf", ppd).unwrap();
            ProxyObject::object(Some(target), Some(handler))
        }
        fn get_prototype_of_returns_null() -> Object {
            let target = ordinary_object_create(None, &[]);
            let handler = ordinary_object_create(None, &[]);
            let get_prototype_of = fn_returning_null();
            let ppd = PotentialPropertyDescriptor::new().value(get_prototype_of);
            define_property_or_throw(&handler, "getPrototypeOf", ppd).unwrap();
            ProxyObject::object(Some(target), Some(handler))
        }
        fn get_prototype_of_returns_string() -> Object {
            let target = ordinary_object_create(None, &[]);
            let handler = ordinary_object_create(None, &[]);
            let get_prototype_of = fn_returning_string();
            let ppd = PotentialPropertyDescriptor::new().value(get_prototype_of);
            define_property_or_throw(&handler, "getPrototypeOf", ppd).unwrap();
            ProxyObject::object(Some(target), Some(handler))
        }
        fn target_dead() -> Object {
            let target = DeadObject::object();
            let handler = ordinary_object_create(None, &[]);
            let get_prototype_of = fn_returning_null();
            let ppd = PotentialPropertyDescriptor::new().value(get_prototype_of);
            define_property_or_throw(&handler, "getPrototypeOf", ppd).unwrap();
            ProxyObject::object(Some(target), Some(handler))
        }
        fn handler_returns_alt() -> Object {
            let prototype = ordinary_object_create(None, &[]);
            define_property_or_throw(&prototype, "test_prop", PotentialPropertyDescriptor::new().value("core"))
                .unwrap();
            let target = ordinary_object_create(Some(prototype), &[]);
            let handler = ordinary_object_create(None, &[]);
            let get_prototype_of = fn_returning_alternate_object();
            let ppd = PotentialPropertyDescriptor::new().value(get_prototype_of);
            define_property_or_throw(&handler, "getPrototypeOf", ppd).unwrap();
            ProxyObject::object(Some(target), Some(handler))
        }
        fn handler_returns_alt_but_not_extensible() -> Object {
            let prototype = ordinary_object_create(None, &[]);
            define_property_or_throw(&prototype, "test_prop", PotentialPropertyDescriptor::new().value("core"))
                .unwrap();
            let target = ordinary_object_create(Some(prototype), &[]);
            target.o.prevent_extensions().unwrap();
            let handler = ordinary_object_create(None, &[]);
            let get_prototype_of = fn_returning_alternate_object();
            let ppd = PotentialPropertyDescriptor::new().value(get_prototype_of);
            define_property_or_throw(&handler, "getPrototypeOf", ppd).unwrap();
            ProxyObject::object(Some(target), Some(handler))
        }
        fn handler_returns_same_and_not_extensible() -> Object {
            let prototype = ordinary_object_create(None, &[]);
            define_property_or_throw(&prototype, "test_prop", PotentialPropertyDescriptor::new().value("core"))
                .unwrap();
            let target = ordinary_object_create(Some(prototype.clone()), &[]);
            target.o.prevent_extensions().unwrap();
            let handler = ordinary_object_create(None, &[]);
            let get_prototype_of = fn_returning_target_proto();
            let ppd = PotentialPropertyDescriptor::new().value(get_prototype_of);
            define_property_or_throw(&handler, "getPrototypeOf", ppd).unwrap();
            ProxyObject::object(Some(target), Some(handler))
        }
        fn target_get_proto_of_fails() -> Object {
            let handler = ordinary_object_create(None, &[]);
            let get_prototype_of = fn_returning_alternate_object();
            let ppd = PotentialPropertyDescriptor::new().value(get_prototype_of);
            define_property_or_throw(&handler, "getPrototypeOf", ppd).unwrap();
            let target = TestObject::object(&[FunctionId::GetPrototypeOf]);
            target.o.prevent_extensions().unwrap();
            ProxyObject::object(Some(target), Some(handler))
        }

        #[test_case(revoked => serr("TypeError: Proxy has been revoked"); "revoked proxy")]
        #[test_case(no_overrides => ssok("core"); "no-call")]
        #[test_case(dead_handler => serr("TypeError: get called on DeadObject"); "get_method fails")]
        #[test_case(handler_call_throws => serr("TypeError: Generic TypeError"); "call fails")]
        #[test_case(get_prototype_of_returns_null => Ok(None); "returns null")]
        #[test_case(get_prototype_of_returns_string => serr("TypeError: proxy error: getPrototypeOf must return an object or null"); "returns string")]
        #[test_case(target_dead => serr("TypeError: is_extensible called on DeadObject"); "is_extensible fails")]
        #[test_case(handler_returns_alt => ssok("alternate"); "simple override")]
        #[test_case(handler_returns_alt_but_not_extensible => serr("TypeError: proxy error: non-extensible targets cannot change prototypes"); "change without extensibility")]
        #[test_case(handler_returns_same_and_not_extensible => ssok("core"); "extensible, but no change")]
        #[test_case(target_get_proto_of_fails => serr("TypeError: [[GetPrototypeOf]] called on TestObject"); "target get proto fails")]
        fn t(make_po: impl FnOnce() -> Object) -> Result<Option<String>, String> {
            setup_test_agent();
            let po = make_po();
            po.o.get_prototype_of()
                .map(|x| x.map(|obj| get(&obj, &"test_prop".into()).unwrap().test_result_string()))
                .map_err(unwind_any_error)
        }
    }

    mod set_prototype_of {
        use super::*;
        use test_case::test_case;

        // SetPrototypeOf argument makers
        fn proto_with_name() -> Option<Object> {
            let proto = ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)), &[]);
            let ppd = PotentialPropertyDescriptor::new().value("proto-in-argument");
            define_property_or_throw(&proto, "test_prop", ppd).unwrap();
            Some(proto)
        }
        fn proto_is_intrinsic() -> Option<Object> {
            Some(intrinsic(IntrinsicId::ObjectPrototype))
        }

        // Handler behaviors
        fn check_arg(this_value: ECMAScriptValue, arguments: &[ECMAScriptValue]) {
            let mut args = FuncArgs::from(arguments);
            assert!(args.next_arg().is_object());
            let proto = args.next_arg();
            let msg = match proto {
                ECMAScriptValue::Null => String::from("saw null"),
                ECMAScriptValue::Object(p) => {
                    let obj_id = get(&p, &"test_prop".into()).unwrap().test_result_string();
                    format!("saw {obj_id}")
                }
                _ => {
                    let id = proto.test_result_string();
                    format!("saw invalid {id}")
                }
            };
            let handler = Object::try_from(this_value).unwrap();
            set(&handler, "callback_message".into(), msg.into(), true).unwrap();
        }

        fn fn_checks_arg_returns_true() -> Object {
            fn behavior(
                this_value: ECMAScriptValue,
                _new_target: Option<&Object>,
                arguments: &[ECMAScriptValue],
            ) -> Completion<ECMAScriptValue> {
                check_arg(this_value, arguments);
                Ok(true.into())
            }
            cbf(behavior)
        }
        fn fn_checks_arg_returns_false() -> Object {
            fn behavior(
                this_value: ECMAScriptValue,
                _new_target: Option<&Object>,
                arguments: &[ECMAScriptValue],
            ) -> Completion<ECMAScriptValue> {
                check_arg(this_value, arguments);
                Ok(false.into())
            }
            cbf(behavior)
        }

        // Proxy Object makers
        fn make_handler(fcn: Object) -> Object {
            let handler = ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)), &[]);
            let ppd = PotentialPropertyDescriptor::new().value(fcn);
            define_property_or_throw(&handler, "setPrototypeOf", ppd).unwrap();
            handler
        }

        fn handler_set_proto_throws() -> Object {
            let target = ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)), &[]);
            let handler = make_handler(intrinsic(IntrinsicId::ThrowTypeError));
            ProxyObject::object(Some(target), Some(handler))
        }
        fn handler_checks_proto_arg() -> Object {
            let target = ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)), &[]);
            let handler = make_handler(fn_checks_arg_returns_true());
            ProxyObject::object(Some(target), Some(handler))
        }
        fn handler_checks_proto_returns_false() -> Object {
            let target = ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)), &[]);
            let handler = make_handler(fn_checks_arg_returns_false());
            ProxyObject::object(Some(target), Some(handler))
        }
        fn target_is_extensible_fails() -> Object {
            let target = TestObject::object(&[FunctionId::IsExtensible]);
            let handler = make_handler(fn_checks_arg_returns_true());
            ProxyObject::object(Some(target), Some(handler))
        }
        fn target_get_prototype_of_fails() -> Object {
            let target = TestObject::object(&[FunctionId::GetPrototypeOf]);
            target.o.prevent_extensions().unwrap();
            let handler = make_handler(fn_checks_arg_returns_true());
            ProxyObject::object(Some(target), Some(handler))
        }
        fn target_not_extensible() -> Object {
            let target = ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)), &[]);
            target.o.prevent_extensions().unwrap();
            let handler = make_handler(fn_checks_arg_returns_true());
            ProxyObject::object(Some(target), Some(handler))
        }

        #[test_case(revoked, || None => serr("TypeError: Proxy has been revoked"); "is revoked")]
        #[test_case(dead_handler, || None => serr("TypeError: get called on DeadObject"); "get_method fails")]
        #[test_case(no_overrides, || None => Ok((true, "undefined".to_string())); "no call")]
        #[test_case(handler_set_proto_throws, || None => serr("TypeError: Generic TypeError"); "call throws")]
        #[test_case(handler_checks_proto_arg, || None => Ok((true, "saw null".to_string())); "proto arg is null")]
        #[test_case(handler_checks_proto_arg, proto_with_name => Ok((true, "saw proto-in-argument".to_string())); "proto arg is object")]
        #[test_case(handler_checks_proto_returns_false, proto_with_name => Ok((false, "saw proto-in-argument".to_string())); "callback returns false")]
        #[test_case(target_is_extensible_fails, proto_with_name => serr("TypeError: [[IsExtensible]] called on TestObject"); "is_extensible fails")]
        #[test_case(target_get_prototype_of_fails, proto_with_name => serr("TypeError: [[GetPrototypeOf]] called on TestObject"); "get_prototype_of fails")]
        #[test_case(target_not_extensible, proto_with_name => serr("TypeError: proxy error: can't change prototype for non-extensible object"); "changing proto on non-extensible")]
        #[test_case(target_not_extensible, proto_is_intrinsic => Ok((true, "saw undefined".to_string())); "non-extensible, changing to same")]
        fn t(
            make_po: impl FnOnce() -> Object,
            make_proto: impl FnOnce() -> Option<Object>,
        ) -> Result<(bool, String), String> {
            setup_test_agent();
            let po = make_po();
            let proto = make_proto();
            po.o.set_prototype_of(proto).map_err(unwind_any_error).map(|b| {
                (b, {
                    let handler = po.o.to_proxy_object().unwrap().proxy_handler.clone().unwrap();
                    get(&handler, &"callback_message".into()).unwrap().test_result_string()
                })
            })
        }
    }

    mod is_extensible {
        use super::*;
        use test_case::test_case;

        fn check_arg(this_value: &ECMAScriptValue, arguments: &[ECMAScriptValue]) {
            assert_eq!(arguments.len(), 1);
            let mut args = FuncArgs::from(arguments);
            let target = Object::try_from(args.next_arg()).unwrap();
            assert_eq!(get(&target, &"test_marker".into()).unwrap(), ECMAScriptValue::from("target object"));
            let handler = Object::try_from(this_value).unwrap();
            assert_eq!(get(&handler, &"callback_message".into()).unwrap(), ECMAScriptValue::from("not called"));
        }

        // Handler behaviors
        fn fn_returns_false() -> Object {
            fn behavior(
                this_value: ECMAScriptValue,
                _new_target: Option<&Object>,
                arguments: &[ECMAScriptValue],
            ) -> Completion<ECMAScriptValue> {
                check_arg(&this_value, arguments);
                let handler = Object::try_from(this_value).unwrap();
                set(&handler, "callback_message".into(), "fn_returns_false called".into(), true).unwrap();
                Ok(false.into())
            }
            cbf(behavior)
        }

        fn fn_returns_false_no_checks() -> Object {
            fn behavior(
                this_value: ECMAScriptValue,
                _: Option<&Object>,
                _: &[ECMAScriptValue],
            ) -> Completion<ECMAScriptValue> {
                let handler = Object::try_from(this_value).unwrap();
                set(&handler, "callback_message".into(), "fn_returns_false_no_checks called".into(), true).unwrap();
                Ok(false.into())
            }
            cbf(behavior)
        }

        // Proxy Object makers
        fn make_handler(fcn: Object) -> Object {
            let handler = ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)), &[]);
            let ppd = PotentialPropertyDescriptor::new().value(fcn);
            define_property_or_throw(&handler, "isExtensible", ppd).unwrap();
            let ppd = PotentialPropertyDescriptor::new().value("not called").writable(true);
            define_property_or_throw(&handler, "callback_message", ppd).unwrap();
            handler
        }
        fn make_target() -> Object {
            let target = ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)), &[]);
            let ppd = PotentialPropertyDescriptor::new().value("target object");
            define_property_or_throw(&target, "test_marker", ppd).unwrap();
            target
        }

        fn handler_is_extensible_throws() -> Object {
            let target = make_target();
            let handler = make_handler(intrinsic(IntrinsicId::ThrowTypeError));
            ProxyObject::object(Some(target), Some(handler))
        }
        fn handler_returns_false_target_extensible() -> Object {
            let target = make_target();
            let handler = make_handler(fn_returns_false());
            ProxyObject::object(Some(target), Some(handler))
        }
        fn handler_returns_false_target_not_extensible() -> Object {
            let target = make_target();
            target.o.prevent_extensions().unwrap();
            let handler = make_handler(fn_returns_false());
            ProxyObject::object(Some(target), Some(handler))
        }
        fn target_is_extensible_fails() -> Object {
            let target = TestObject::object(&[FunctionId::IsExtensible]);
            let handler = make_handler(fn_returns_false_no_checks());
            ProxyObject::object(Some(target), Some(handler))
        }

        #[test_case(revoked => serr("TypeError: Proxy has been revoked"); "is revoked")]
        #[test_case(dead_handler => serr("TypeError: get called on DeadObject"); "get_method fails")]
        #[test_case(no_overrides => Ok((true, "undefined".into())); "no call")]
        #[test_case(handler_is_extensible_throws => serr("TypeError: Generic TypeError"); "call fails")]
        #[test_case(handler_returns_false_target_extensible => serr("TypeError: proxy error: extensible property cannot be changed"); "false but extensible")]
        #[test_case(handler_returns_false_target_not_extensible => Ok((false, "fn_returns_false called".to_string())); "false and not extensible")]
        #[test_case(target_is_extensible_fails => serr("TypeError: [[IsExtensible]] called on TestObject"); "target is extensible errs")]
        fn t(make_po: impl FnOnce() -> Object) -> Result<(bool, String), String> {
            setup_test_agent();
            let po = make_po();
            po.o.is_extensible().map_err(unwind_any_error).map(|b| {
                (b, {
                    let handler = po.o.to_proxy_object().unwrap().proxy_handler.clone().unwrap();
                    get(&handler, &"callback_message".into()).unwrap().test_result_string()
                })
            })
        }
    }

    mod prevent_extensions {
        use super::*;
        use test_case::test_case;

        fn check_arg(this_value: &ECMAScriptValue, arguments: &[ECMAScriptValue]) {
            assert_eq!(arguments.len(), 1);
            let mut args = FuncArgs::from(arguments);
            let target = Object::try_from(args.next_arg()).unwrap();
            assert_eq!(get(&target, &"test_marker".into()).unwrap(), ECMAScriptValue::from("target object"));
            let handler = Object::try_from(this_value).unwrap();
            assert_eq!(get(&handler, &"callback_message".into()).unwrap(), ECMAScriptValue::from("not called"));
        }

        // Handler behaviors
        fn fn_returns_false() -> Object {
            fn behavior(
                this_value: ECMAScriptValue,
                _new_target: Option<&Object>,
                arguments: &[ECMAScriptValue],
            ) -> Completion<ECMAScriptValue> {
                check_arg(&this_value, arguments);
                let handler = Object::try_from(this_value).unwrap();
                set(&handler, "callback_message".into(), "fn_returns_false called".into(), true).unwrap();
                Ok(false.into())
            }
            cbf(behavior)
        }

        fn fn_returns_true() -> Object {
            fn behavior(
                this_value: ECMAScriptValue,
                _new_target: Option<&Object>,
                arguments: &[ECMAScriptValue],
            ) -> Completion<ECMAScriptValue> {
                check_arg(&this_value, arguments);
                let handler = Object::try_from(this_value).unwrap();
                set(&handler, "callback_message".into(), "fn_returns_true called".into(), true).unwrap();
                Ok(true.into())
            }
            cbf(behavior)
        }
        fn fn_returns_true_no_checks() -> Object {
            fn behavior(
                this_value: ECMAScriptValue,
                _new_target: Option<&Object>,
                _arguments: &[ECMAScriptValue],
            ) -> Completion<ECMAScriptValue> {
                let handler = Object::try_from(this_value).unwrap();
                set(&handler, "callback_message".into(), "fn_returns_true_no_checks called".into(), true).unwrap();
                Ok(true.into())
            }
            cbf(behavior)
        }

        // Proxy Object makers
        fn make_handler(fcn: Object) -> Object {
            let handler = ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)), &[]);
            let ppd = PotentialPropertyDescriptor::new().value(fcn);
            define_property_or_throw(&handler, "preventExtensions", ppd).unwrap();
            let ppd = PotentialPropertyDescriptor::new().value("not called").writable(true);
            define_property_or_throw(&handler, "callback_message", ppd).unwrap();
            handler
        }
        fn make_target() -> Object {
            let target = ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)), &[]);
            let ppd = PotentialPropertyDescriptor::new().value("target object");
            define_property_or_throw(&target, "test_marker", ppd).unwrap();
            target
        }

        fn handler_prevent_extensions_throws() -> Object {
            let target = make_target();
            let handler = make_handler(intrinsic(IntrinsicId::ThrowTypeError));
            ProxyObject::object(Some(target), Some(handler))
        }
        fn handler_returns_false() -> Object {
            let target = make_target();
            let handler = make_handler(fn_returns_false());
            ProxyObject::object(Some(target), Some(handler))
        }
        fn handler_returns_true_extensible() -> Object {
            let target = make_target();
            let handler = make_handler(fn_returns_true());
            ProxyObject::object(Some(target), Some(handler))
        }
        fn handler_returns_true_non_extensible() -> Object {
            let target = make_target();
            target.o.prevent_extensions().unwrap();
            let handler = make_handler(fn_returns_true());
            ProxyObject::object(Some(target), Some(handler))
        }
        fn target_is_extensible_fails() -> Object {
            let target = TestObject::object(&[FunctionId::IsExtensible]);
            let handler = make_handler(fn_returns_true_no_checks());
            ProxyObject::object(Some(target), Some(handler))
        }

        #[test_case(revoked => serr("TypeError: Proxy has been revoked"); "is revoked")]
        #[test_case(dead_handler => serr("TypeError: get called on DeadObject"); "get_method fails")]
        #[test_case(no_overrides => Ok((true, "undefined".into())); "no call")]
        #[test_case(handler_prevent_extensions_throws => serr("TypeError: Generic TypeError"); "call fails")]
        #[test_case(handler_returns_false => Ok((false, "fn_returns_false called".to_string())); "returns false")]
        #[test_case(handler_returns_true_extensible => serr("TypeError: proxy error: extensible property cannot be changed"); "true/extensible")]
        #[test_case(handler_returns_true_non_extensible => Ok((true, "fn_returns_true called".to_string())); "true/nonextensible")]
        #[test_case(target_is_extensible_fails => serr("TypeError: [[IsExtensible]] called on TestObject"); "target is extensible errs")]
        fn t(make_po: impl FnOnce() -> Object) -> Result<(bool, String), String> {
            setup_test_agent();
            let po = make_po();
            po.o.prevent_extensions().map_err(unwind_any_error).map(|b| {
                (b, {
                    let handler = po.o.to_proxy_object().unwrap().proxy_handler.clone().unwrap();
                    get(&handler, &"callback_message".into()).unwrap().test_result_string()
                })
            })
        }
    }
    mod get_own_property {
        use super::*;
        use test_case::test_case;

        fn check_arg(this_value: &ECMAScriptValue, arguments: &[ECMAScriptValue]) {
            assert_eq!(arguments.len(), 2);
            let mut args = FuncArgs::from(arguments);
            let target = Object::try_from(args.next_arg()).unwrap();
            assert_eq!(get(&target, &"test_marker".into()).unwrap(), ECMAScriptValue::from("target object"));
            let key = String::from(JSString::try_from(args.next_arg()).unwrap());
            assert_eq!(key, "test_key");
            let handler = Object::try_from(this_value).unwrap();
            assert_eq!(get(&handler, &"callback_message".into()).unwrap(), ECMAScriptValue::from("not called"));
        }

        // Handler behaviors
        fn fn_new_value() -> Object {
            fn behavior(
                this_value: ECMAScriptValue,
                _new_target: Option<&Object>,
                arguments: &[ECMAScriptValue],
            ) -> Completion<ECMAScriptValue> {
                check_arg(&this_value, arguments);
                let ppd = PotentialPropertyDescriptor::new().value("new-value").writable(true).configurable(true);
                let rval = match from_property_descriptor(Some(ppd)) {
                    None => ECMAScriptValue::Null,
                    Some(obj) => ECMAScriptValue::from(obj),
                };
                let handler = Object::try_from(this_value).unwrap();
                set(&handler, "callback_message".into(), "fn_new_value called".into(), true).unwrap();
                Ok(rval)
            }
            cbf(behavior)
        }
        fn fn_new_value_no_checks() -> Object {
            fn behavior(
                this_value: ECMAScriptValue,
                _new_target: Option<&Object>,
                _arguments: &[ECMAScriptValue],
            ) -> Completion<ECMAScriptValue> {
                let ppd = PotentialPropertyDescriptor::new().value("new-value").writable(true).configurable(true);
                let rval = match from_property_descriptor(Some(ppd)) {
                    None => ECMAScriptValue::Null,
                    Some(obj) => ECMAScriptValue::from(obj),
                };
                let handler = Object::try_from(this_value).unwrap();
                set(&handler, "callback_message".into(), "fn_new_value_no_checks called".into(), true).unwrap();
                Ok(rval)
            }
            cbf(behavior)
        }
        fn fn_returns_invalid_pd() -> Object {
            fn behavior(
                this_value: ECMAScriptValue,
                _new_target: Option<&Object>,
                arguments: &[ECMAScriptValue],
            ) -> Completion<ECMAScriptValue> {
                check_arg(&this_value, arguments);
                let ppd =
                    PotentialPropertyDescriptor::new().value("new-value").writable(true).configurable(true).set(false);
                let rval = match from_property_descriptor(Some(ppd)) {
                    None => ECMAScriptValue::Null,
                    Some(obj) => ECMAScriptValue::from(obj),
                };
                let handler = Object::try_from(this_value).unwrap();
                set(&handler, "callback_message".into(), "fn_returns_invalid_pd called".into(), true).unwrap();
                Ok(rval)
            }
            cbf(behavior)
        }
        fn fn_returns_string() -> Object {
            fn behavior(
                this_value: ECMAScriptValue,
                _new_target: Option<&Object>,
                arguments: &[ECMAScriptValue],
            ) -> Completion<ECMAScriptValue> {
                check_arg(&this_value, arguments);
                Ok(ECMAScriptValue::from("a string"))
            }
            cbf(behavior)
        }
        fn fn_returns_undef_no_checks() -> Object {
            cbf(|_, _, _| Ok(ECMAScriptValue::Undefined))
        }
        fn fn_returns_undef() -> Object {
            fn behavior(
                this_value: ECMAScriptValue,
                _new_target: Option<&Object>,
                arguments: &[ECMAScriptValue],
            ) -> Completion<ECMAScriptValue> {
                check_arg(&this_value, arguments);
                let handler = Object::try_from(this_value).unwrap();
                set(&handler, "callback_message".into(), "fn_returns_undef called".into(), true).unwrap();
                Ok(ECMAScriptValue::Undefined)
            }
            cbf(behavior)
        }
        fn fn_returns_nonconfig() -> Object {
            fn behavior(
                this_value: ECMAScriptValue,
                _new_target: Option<&Object>,
                arguments: &[ECMAScriptValue],
            ) -> Completion<ECMAScriptValue> {
                check_arg(&this_value, arguments);
                let handler = Object::try_from(this_value).unwrap();
                set(&handler, "callback_message".into(), "fn_returns_nonconfig called".into(), true).unwrap();
                let ppd = PotentialPropertyDescriptor::new().value("unconfig").writable(true).configurable(false);
                let rval = match from_property_descriptor(Some(ppd)) {
                    None => ECMAScriptValue::Null,
                    Some(obj) => ECMAScriptValue::from(obj),
                };
                Ok(rval)
            }
            cbf(behavior)
        }
        fn fn_returns_nonconfig_nonwrite() -> Object {
            fn behavior(
                this_value: ECMAScriptValue,
                _new_target: Option<&Object>,
                arguments: &[ECMAScriptValue],
            ) -> Completion<ECMAScriptValue> {
                check_arg(&this_value, arguments);
                let handler = Object::try_from(this_value).unwrap();
                set(&handler, "callback_message".into(), "fn_returns_nonconfig_nonwrite called".into(), true).unwrap();
                let ppd = PotentialPropertyDescriptor::new().value("unconfig").writable(false).configurable(false);
                let rval = match from_property_descriptor(Some(ppd)) {
                    None => ECMAScriptValue::Null,
                    Some(obj) => ECMAScriptValue::from(obj),
                };
                Ok(rval)
            }
            cbf(behavior)
        }

        // Proxy Object Makers
        fn make_handler(fcn: Object) -> Object {
            let handler = ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)), &[]);
            let ppd = PotentialPropertyDescriptor::new().value(fcn);
            define_property_or_throw(&handler, "getOwnPropertyDescriptor", ppd).unwrap();
            let ppd = PotentialPropertyDescriptor::new().value("not called").writable(true).configurable(true);
            define_property_or_throw(&handler, "callback_message", ppd).unwrap();
            handler
        }
        fn make_target() -> Object {
            let target = ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)), &[]);
            let ppd = PotentialPropertyDescriptor::new().value("target object");
            define_property_or_throw(&target, "test_marker", ppd).unwrap();
            target
        }

        fn handler_prop_replacement() -> Object {
            let target = make_target();
            let handler = make_handler(fn_new_value());
            ProxyObject::object(Some(target), Some(handler))
        }
        fn handler_get_own_property_throws() -> Object {
            let target = make_target();
            let handler = make_handler(intrinsic(IntrinsicId::ThrowTypeError));
            ProxyObject::object(Some(target), Some(handler))
        }
        fn handler_returns_string() -> Object {
            let target = make_target();
            let handler = make_handler(fn_returns_string());
            ProxyObject::object(Some(target), Some(handler))
        }
        fn target_get_own_property_fails() -> Object {
            let target = TestObject::object(&[FunctionId::GetOwnProperty(None)]);
            let handler = make_handler(fn_returns_undef_no_checks());
            ProxyObject::object(Some(target), Some(handler))
        }
        fn handler_null_target_null() -> Object {
            let target = make_target();
            let handler = make_handler(fn_returns_undef());
            ProxyObject::object(Some(target), Some(handler))
        }
        fn handler_null_target_nonconfig() -> Object {
            let target = make_target();
            let ppd = PotentialPropertyDescriptor::new().value(10);
            define_property_or_throw(&target, "test_key", ppd).unwrap();
            let handler = make_handler(fn_returns_undef());
            ProxyObject::object(Some(target), Some(handler))
        }
        fn handler_null_target_nonextensible() -> Object {
            let target = make_target();
            let ppd = PotentialPropertyDescriptor::new().value(10).configurable(true);
            define_property_or_throw(&target, "test_key", ppd).unwrap();
            target.o.prevent_extensions().unwrap();
            let handler = make_handler(fn_returns_undef());
            ProxyObject::object(Some(target), Some(handler))
        }
        fn handler_null_target_config_and_extend() -> Object {
            let target = make_target();
            let ppd = PotentialPropertyDescriptor::new().value(10).configurable(true);
            define_property_or_throw(&target, "test_key", ppd).unwrap();
            let handler = make_handler(fn_returns_undef());
            ProxyObject::object(Some(target), Some(handler))
        }
        fn handler_null_target_is_extensible_fails() -> Object {
            fn is_extensible_override(this: &AdaptableObject) -> Completion<bool> {
                let call_count = this.something.get();
                if call_count == 0 {
                    let rval = ordinary_is_extensible(this);
                    this.something.set(1);
                    Ok(rval)
                } else {
                    this.something.set(call_count + 1);
                    Err(create_type_error("test case throws error"))
                }
            }
            let target = AdaptableObject::object(AdaptableMethods {
                is_extensible_override: Some(is_extensible_override),
                ..Default::default()
            });
            let ppd = PotentialPropertyDescriptor::new().value(10).configurable(true);
            define_property_or_throw(&target, "test_key", ppd).unwrap();
            let handler = make_handler(fn_returns_undef_no_checks());
            ProxyObject::object(Some(target), Some(handler))
        }
        fn handler_new_target_is_extensible_fails() -> Object {
            let target = TestObject::object(&[FunctionId::IsExtensible]);
            let handler = make_handler(fn_new_value_no_checks());
            ProxyObject::object(Some(target), Some(handler))
        }
        fn handler_bad_pd() -> Object {
            let target = make_target();
            let handler = make_handler(fn_returns_invalid_pd());
            ProxyObject::object(Some(target), Some(handler))
        }
        fn handler_something_target_not_extensible() -> Object {
            let target = make_target();
            target.o.prevent_extensions().unwrap();
            let handler = make_handler(fn_new_value());
            ProxyObject::object(Some(target), Some(handler))
        }
        fn handler_nonconfig_target_notpresent() -> Object {
            let target = make_target();
            let handler = make_handler(fn_returns_nonconfig());
            ProxyObject::object(Some(target), Some(handler))
        }
        fn handler_nonconfig_target_config() -> Object {
            let target = make_target();
            let ppd = PotentialPropertyDescriptor::new().value(99).writable(true).configurable(true);
            define_property_or_throw(&target, "test_key", ppd).unwrap();
            let handler = make_handler(fn_returns_nonconfig());
            ProxyObject::object(Some(target), Some(handler))
        }
        fn handler_nonconfig_nonwrite_target_nonconfig_write() -> Object {
            let target = make_target();
            let ppd = PotentialPropertyDescriptor::new().value(99).writable(true).configurable(false);
            define_property_or_throw(&target, "test_key", ppd).unwrap();
            let handler = make_handler(fn_returns_nonconfig_nonwrite());
            ProxyObject::object(Some(target), Some(handler))
        }
        fn handler_nonconfig_nonwrite_target_nonconfig_nonwrite() -> Object {
            let target = make_target();
            let ppd = PotentialPropertyDescriptor::new().value("unconfig").writable(false).configurable(false);
            define_property_or_throw(&target, "test_key", ppd).unwrap();
            let handler = make_handler(fn_returns_nonconfig_nonwrite());
            ProxyObject::object(Some(target), Some(handler))
        }

        #[test_case(revoked => serr("TypeError: Proxy has been revoked"); "is revoked")]
        #[test_case(dead_handler => serr("TypeError: get called on DeadObject"); "get_method fails")]
        #[test_case(no_overrides => Ok(("value:target,writable:true,enumerable:false,configurable:true".into(), "undefined".into())); "no call")]
        #[test_case(handler_prop_replacement => Ok(("value:new-value,writable:true,enumerable:false,configurable:true".into(), "fn_new_value called".into())); "simple prop replacement")]
        #[test_case(handler_get_own_property_throws => serr("TypeError: Generic TypeError"); "call fails")]
        #[test_case(handler_returns_string => serr("TypeError: proxy error: The result of [[GetOwnProperty]] must be either an Object or undefined."); "handler returns nonobj")]
        #[test_case(target_get_own_property_fails => serr("TypeError: [[GetOwnProperty]] called on TestObject"); "target getownprop fails")]
        #[test_case(handler_null_target_null => Ok(("undefined".into(), "fn_returns_undef called".into())); "the none/none case")]
        #[test_case(handler_null_target_nonconfig => serr("TypeError: proxy error: A property cannot be reported as non-existent, if it exists as a non-configurable own property of the target object."); "null handler, nonconfig prop")]
        #[test_case(handler_null_target_nonextensible => serr("TypeError: proxy error: A property cannot be reported as existent, if it does not exist as an own property of the target object and the target object is not extensible."); "null handler, nonextensible")]
        #[test_case(handler_null_target_config_and_extend => Ok(("undefined".into(), "fn_returns_undef called".into())); "handler null; acceptably")]
        #[test_case(handler_null_target_is_extensible_fails => serr("TypeError: test case throws error"); "handler is null; target is_extensible fails")]
        #[test_case(handler_new_target_is_extensible_fails => serr("TypeError: [[IsExtensible]] called on TestObject"); "handler something; target errs")]
        #[test_case(handler_bad_pd => serr("TypeError: Setter must be callable (or undefined)"); "handler makes bad descriptor")]
        #[test_case(handler_something_target_not_extensible => serr("TypeError: proxy error: A property cannot be reported as existent, if it does not exist as an own property of the target object and the target object is not extensible."); "adding prop; target not extensible")]
        #[test_case(handler_nonconfig_target_notpresent => serr("TypeError: proxy error: A property cannot be reported as non-configurable, unless it exists as a non-configurable own property of the target object."); "new pd not-config; target missing")]
        #[test_case(handler_nonconfig_target_config => serr("TypeError: proxy error: A property cannot be reported as non-configurable, unless it exists as a non-configurable own property of the target object."); "new pd not-config; but target was")]
        #[test_case(handler_nonconfig_nonwrite_target_nonconfig_write => serr("TypeError: proxy error: A property cannot be reported as both non-configurable and non-writable, unless it exists as a non-configurable, non-writable own property of the target object."); "new pd read-only, non-config; target non-config; writable")]
        #[test_case(handler_nonconfig_nonwrite_target_nonconfig_nonwrite => Ok(("value:unconfig,writable:false,enumerable:false,configurable:false".to_string(), "fn_returns_nonconfig_nonwrite called".into())); "same value of nonwrite, nonconfig")]
        fn t(make_po: impl FnOnce() -> Object) -> Result<(String, String), String> {
            setup_test_agent();
            let po = make_po();
            let key = "test_key".into();
            po.o.get_own_property(&key).map_err(unwind_any_error).map(|pd| {
                (
                    from_property_descriptor(pd)
                        .map(|pd| ECMAScriptValue::from(pd).test_result_string())
                        .unwrap_or_else(|| "undefined".to_string()),
                    {
                        let handler = po.o.to_proxy_object().unwrap().proxy_handler.clone().unwrap();
                        get(&handler, &"callback_message".into()).unwrap().test_result_string()
                    },
                )
            })
        }
    }
}
