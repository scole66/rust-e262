use super::*;
use crate::tests::*;
use ahash::AHashMap;
use test_case::test_case;

mod string_object {
    use super::*;
    use test_case::test_case;

    #[test]
    fn debug() {
        setup_test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject {
            common: RefCell::new(CommonObjectData::new(&agent, Some(prototype), true, STRING_OBJECT_SLOTS)),
            string_data: RefCell::new(JSString::from("baloney")),
        };
        assert_ne!(format!("{:?}", so), "");
    }

    #[test]
    fn object() {
        setup_test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&agent, "orange".into(), Some(prototype));

        let length = super::get(&agent, &so, &"length".into()).unwrap();
        assert_eq!(length, ECMAScriptValue::from(6));

        let sobj = so.o.to_string_obj().unwrap();
        assert_eq!(*sobj.string_data.borrow(), JSString::from("orange"));
    }

    #[test]
    fn is_boolean_object() {
        setup_test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&agent, "orange".into(), Some(prototype));

        assert!(!so.o.is_boolean_object());
    }

    #[test]
    fn is_date_object() {
        setup_test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&agent, "orange".into(), Some(prototype));

        assert!(!so.o.is_date_object());
    }

    #[test]
    fn is_array_object() {
        setup_test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&agent, "orange".into(), Some(prototype));

        assert!(!so.o.is_array_object());
    }

    #[test]
    fn is_proxy_object() {
        setup_test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&agent, "orange".into(), Some(prototype));

        assert!(!so.o.is_proxy_object());
    }

    #[test]
    fn is_symbol_object() {
        setup_test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&agent, "orange".into(), Some(prototype));

        assert!(!so.o.is_symbol_object());
    }

    #[test]
    fn is_number_object() {
        setup_test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&agent, "orange".into(), Some(prototype));

        assert!(!so.o.is_number_object());
    }

    #[test]
    fn is_arguments_object() {
        setup_test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&agent, "orange".into(), Some(prototype));

        assert!(!so.o.is_arguments_object());
    }

    #[test]
    fn is_plain_object() {
        setup_test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&agent, "orange".into(), Some(prototype));

        assert!(!so.o.is_plain_object());
    }

    #[test]
    fn is_regexp_object() {
        setup_test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&agent, "orange".into(), Some(prototype));

        assert!(!so.o.is_regexp_object());
    }

    #[test]
    fn is_error_object() {
        setup_test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&agent, "orange".into(), Some(prototype));

        assert!(!so.o.is_error_object());
    }

    #[test]
    fn is_callable_obj() {
        setup_test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&agent, "orange".into(), Some(prototype));

        assert!(!so.o.is_callable_obj());
    }

    #[test]
    fn is_string_object() {
        setup_test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&agent, "orange".into(), Some(prototype));

        assert!(so.o.is_string_object());
    }

    #[test]
    fn is_ordinary() {
        setup_test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&agent, "orange".into(), Some(prototype));

        assert!(so.o.is_ordinary());
    }

    #[test]
    fn to_arguments_object() {
        setup_test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&agent, "orange".into(), Some(prototype));

        assert!(so.o.to_arguments_object().is_none());
    }

    #[test]
    fn to_boolean_obj() {
        setup_test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&agent, "orange".into(), Some(prototype));

        assert!(so.o.to_boolean_obj().is_none());
    }

    #[test]
    fn to_array_object() {
        setup_test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&agent, "orange".into(), Some(prototype));

        assert!(so.o.to_array_object().is_none());
    }

    #[test]
    fn to_callable_obj() {
        setup_test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&agent, "orange".into(), Some(prototype));

        assert!(so.o.to_callable_obj().is_none());
    }

    #[test]
    fn to_function_obj() {
        setup_test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&agent, "orange".into(), Some(prototype));

        assert!(so.o.to_function_obj().is_none());
    }

    #[test]
    fn to_error_obj() {
        setup_test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&agent, "orange".into(), Some(prototype));

        assert!(so.o.to_error_obj().is_none());
    }

    #[test]
    fn to_builtin_function_obj() {
        setup_test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&agent, "orange".into(), Some(prototype));

        assert!(so.o.to_builtin_function_obj().is_none());
    }

    #[test]
    fn to_symbol_obj() {
        setup_test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&agent, "orange".into(), Some(prototype));

        assert!(so.o.to_symbol_obj().is_none());
    }

    #[test]
    fn to_number_obj() {
        setup_test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&agent, "orange".into(), Some(prototype));

        assert!(so.o.to_number_obj().is_none());
    }

    #[test]
    fn to_constructable() {
        setup_test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&agent, "orange".into(), Some(prototype));

        assert!(so.o.to_constructable().is_none());
    }

    #[test]
    fn get_prototype_of() {
        setup_test_agent();
        let str_obj = agent.create_string_object("orange".into());
        let proto = str_obj.o.get_prototype_of(&agent).unwrap().unwrap();
        assert_eq!(proto, agent.intrinsic(IntrinsicId::StringPrototype));
    }

    #[test]
    fn set_prototype_of() {
        setup_test_agent();
        let str_obj = agent.create_string_object("orange".into());
        let res = str_obj.o.set_prototype_of(&agent, None).unwrap();
        assert!(res);
        assert!(str_obj.o.get_prototype_of(&agent).unwrap().is_none());
    }

    #[test]
    fn is_extensible() {
        setup_test_agent();
        let str_obj = agent.create_string_object("orange".into());
        let res = str_obj.o.is_extensible(&agent).unwrap();
        assert!(res);
    }

    #[test]
    fn prevent_extensions() {
        setup_test_agent();
        let str_obj = agent.create_string_object("orange".into());
        let res = str_obj.o.prevent_extensions(&agent).unwrap();
        assert!(res);
        assert!(!str_obj.o.is_extensible(&agent).unwrap());
    }

    #[test_case(
        "orange", 776, "numberly"
        => (
            true,
            [
                (
                    PropertyKey::from("length"),
                    IdealizedPropertyDescriptor {
                        configurable: false,
                        enumerable: false,
                        writable: Some(false),
                        value: Some(ECMAScriptValue::from(6)),
                        get: None,
                        set: None
                    }
                ),
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
                )
             ].into_iter().collect::<AHashMap<PropertyKey, IdealizedPropertyDescriptor>>()
        ); "ordinary set"
    )]
    #[test_case(
        "orange", 776, "2"
        => (
            false,
            [
                (
                    PropertyKey::from("length"),
                    IdealizedPropertyDescriptor {
                        configurable: false,
                        enumerable: false,
                        writable: Some(false),
                        value: Some(ECMAScriptValue::from(6)),
                        get: None,
                        set: None
                    }
                )
             ].into_iter().collect::<AHashMap<PropertyKey, IdealizedPropertyDescriptor>>()
        ); "trying to overwrite an index"
    )]
    fn set(
        value: &str,
        new_val: impl Into<ECMAScriptValue>,
        key: impl Into<PropertyKey>,
    ) -> (bool, AHashMap<PropertyKey, IdealizedPropertyDescriptor>) {
        setup_test_agent();
        let str_obj = agent.create_string_object(value.into());
        let receiver = ECMAScriptValue::Object(str_obj.clone());
        let success = str_obj.o.set(&agent, key.into(), new_val.into(), &receiver).unwrap();
        let properties = str_obj
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
    fn delete() {
        setup_test_agent();
        let str_obj = agent.create_string_object("orange".into());
        let res = str_obj.o.delete(&agent, &PropertyKey::from("rust")).unwrap();
        assert_eq!(res, true);
    }

    #[test]
    fn id() {
        setup_test_agent();
        let str_obj = agent.create_string_object("orange".into());
        let str_obj2 = agent.create_string_object("orange".into());
        assert_ne!(str_obj.o.id(), str_obj2.o.id());
    }

    #[test]
    fn has_property() {
        setup_test_agent();
        let str_obj = agent.create_string_object("orange".into());
        let res = str_obj.o.has_property(&agent, &PropertyKey::from("rust")).unwrap();
        assert_eq!(res, false);
        let res2 = str_obj.o.has_property(&agent, &PropertyKey::from("length")).unwrap();
        assert_eq!(res2, true);
    }

    #[test]
    fn common_object_data() {
        setup_test_agent();
        let str_obj = agent.create_string_object("orange".into());
        let cod = str_obj.o.common_object_data().borrow();

        assert_eq!(cod.properties.len(), 1);
        assert!(cod.properties.contains_key(&"length".into()));
        let string_proto = agent.intrinsic(IntrinsicId::StringPrototype);
        assert_eq!(cod.prototype.as_ref().unwrap(), &string_proto);
        assert!(cod.extensible);
        assert_eq!(cod.slots, STRING_OBJECT_SLOTS);
        assert!(cod.private_elements.is_empty());
    }

    #[test_case("orange", |agent| agent.wks(WksId::ToPrimitive).into() => None; "not a string")]
    #[test_case("orange", |_| "blue".into() => None; "not an index")]
    #[test_case("orange", |_| "0.5".into() => None; "not an integer")]
    #[test_case("orange", |_| "-0".into() => None; "neg zero")]
    #[test_case("orange", |_| "-10".into() => None; "negative number")]
    #[test_case("orange", |_| "6".into() => None; "len or greater")]
    #[test_case("orange", |_| "0".into() => Some(IdealizedPropertyDescriptor{configurable: false, enumerable:  true, writable: Some(false), value: Some("o".into()), get: None, set: None }); "valid; index 0")]
    #[test_case("orange", |_| "1".into() => Some(IdealizedPropertyDescriptor{configurable: false, enumerable:  true, writable: Some(false), value: Some("r".into()), get: None, set: None }); "valid; index 1")]
    #[test_case("orange", |_| "2".into() => Some(IdealizedPropertyDescriptor{configurable: false, enumerable:  true, writable: Some(false), value: Some("a".into()), get: None, set: None }); "valid; index 2")]
    #[test_case("orange", |_| "3".into() => Some(IdealizedPropertyDescriptor{configurable: false, enumerable:  true, writable: Some(false), value: Some("n".into()), get: None, set: None }); "valid; index 3")]
    #[test_case("orange", |_| "4".into() => Some(IdealizedPropertyDescriptor{configurable: false, enumerable:  true, writable: Some(false), value: Some("g".into()), get: None, set: None }); "valid; index 4")]
    #[test_case("orange", |_| "5".into() => Some(IdealizedPropertyDescriptor{configurable: false, enumerable:  true, writable: Some(false), value: Some("e".into()), get: None, set: None }); "valid; index 5")]
    fn string_get_own_property(
        value: &str,
        make_key: impl FnOnce(&Agent) -> PropertyKey,
    ) -> Option<IdealizedPropertyDescriptor> {
        setup_test_agent();
        let probe = make_key(&agent);
        let str_obj = agent.create_string_object(value.into());
        str_obj.o.to_string_obj().unwrap().string_get_own_property(&probe).map(IdealizedPropertyDescriptor::from)
    }

    #[test_case("orange", "length" => Some(IdealizedPropertyDescriptor{configurable: false, enumerable: false, writable: Some(false), value: Some(ECMAScriptValue::from(6)), get: None, set: None}); "ordinary get")]
    #[test_case("orange", "3" => Some(IdealizedPropertyDescriptor{configurable: false, enumerable: true, writable: Some(false), value: Some(ECMAScriptValue::from("n")), get: None, set: None}); "stringish get")]
    #[test_case("orange", "color" => None; "key not present")]
    fn get_own_property(value: &str, key: &str) -> Option<IdealizedPropertyDescriptor> {
        setup_test_agent();
        let str_obj = agent.create_string_object(value.into());
        str_obj.o.get_own_property(&agent, &key.into()).unwrap().map(IdealizedPropertyDescriptor::from)
    }

    #[test_case(
        "orange",
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
                    PropertyKey::from("length"),
                    IdealizedPropertyDescriptor {
                        configurable: false,
                        enumerable: false,
                        writable: Some(false),
                        value: Some(ECMAScriptValue::from(6)),
                        get: None,
                        set: None
                    }
                ),
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
                )
             ].into_iter().collect::<AHashMap<PropertyKey, IdealizedPropertyDescriptor>>()
           ); "ordinary property"
    )]
    #[test_case(
        "orange",
        PotentialPropertyDescriptor::new()
            .value(ECMAScriptValue::from("o"))
            .writable(false)
            .configurable(false)
            .enumerable(true),
        "0"
        => (
            true,
            [
                (
                    PropertyKey::from("length"),
                    IdealizedPropertyDescriptor {
                        configurable: false,
                        enumerable: false,
                        writable: Some(false),
                        value: Some(ECMAScriptValue::from(6)),
                        get: None,
                        set: None
                    }
                )
             ].into_iter().collect::<AHashMap<PropertyKey, IdealizedPropertyDescriptor>>()
           ); "index property; successful"
    )]
    #[test_case(
        "orange",
        PotentialPropertyDescriptor::new()
            .value(ECMAScriptValue::from("O"))
            .writable(false)
            .configurable(false)
            .enumerable(true),
        "0"
        => (
            false,
            [
                (
                    PropertyKey::from("length"),
                    IdealizedPropertyDescriptor {
                        configurable: false,
                        enumerable: false,
                        writable: Some(false),
                        value: Some(ECMAScriptValue::from(6)),
                        get: None,
                        set: None
                    }
                )
             ].into_iter().collect::<AHashMap<PropertyKey, IdealizedPropertyDescriptor>>()
           ); "index property; unsuccessful"
    )]
    fn define_own_property(
        value: &str,
        new_value: PotentialPropertyDescriptor,
        key: &str,
    ) -> (bool, AHashMap<PropertyKey, IdealizedPropertyDescriptor>) {
        setup_test_agent();
        let str_obj = agent.create_string_object(value.into());

        let success = str_obj.o.define_own_property(&agent, key.into(), new_value).unwrap();
        let properties = str_obj
            .o
            .common_object_data()
            .borrow()
            .properties
            .iter()
            .map(|(a, b)| (a.clone(), IdealizedPropertyDescriptor::from(b.clone())))
            .collect::<AHashMap<_, _>>();
        (success, properties)
    }

    #[test_case("orange", "length" => ECMAScriptValue::from(6); "exists")]
    #[test_case("orange", "friendliness" => ECMAScriptValue::Undefined; "doesn't exist")]
    fn get(value: &str, key: &str) -> ECMAScriptValue {
        setup_test_agent();
        let str_obj = agent.create_string_object(value.into());

        let receiver = ECMAScriptValue::from(str_obj.clone());
        str_obj.o.get(&agent, &key.into(), &receiver).unwrap()
    }

    #[test]
    fn own_property_keys() {
        setup_test_agent();
        let str_obj = agent.create_string_object("orange".into());

        let to_prim = agent.wks(WksId::ToPrimitive);
        let species = agent.wks(WksId::Species);

        str_obj
            .o
            .define_own_property(
                &agent,
                "60".into(),
                PotentialPropertyDescriptor::new().value("q").writable(true).enumerable(true).configurable(true),
            )
            .unwrap();
        str_obj
            .o
            .define_own_property(
                &agent,
                "6".into(),
                PotentialPropertyDescriptor::new().value("s").writable(true).enumerable(true).configurable(true),
            )
            .unwrap();
        str_obj
            .o
            .define_own_property(
                &agent,
                "zebra".into(),
                PotentialPropertyDescriptor::new().value(0).writable(true).enumerable(true).configurable(true),
            )
            .unwrap();
        str_obj
            .o
            .define_own_property(
                &agent,
                "alpha".into(),
                PotentialPropertyDescriptor::new().value(1).writable(true).enumerable(true).configurable(true),
            )
            .unwrap();
        str_obj
            .o
            .define_own_property(
                &agent,
                to_prim.clone().into(),
                PotentialPropertyDescriptor::new().value(2).writable(true).enumerable(true).configurable(true),
            )
            .unwrap();
        str_obj
            .o
            .define_own_property(
                &agent,
                species.clone().into(),
                PotentialPropertyDescriptor::new().value(3).writable(true).enumerable(true).configurable(true),
            )
            .unwrap();

        let keys = str_obj.o.own_property_keys(&agent).unwrap();

        assert_eq!(
            keys,
            vec![
                "0".into(),
                "1".into(),
                "2".into(),
                "3".into(),
                "4".into(),
                "5".into(),
                "6".into(),
                "60".into(),
                "length".into(),
                "zebra".into(),
                "alpha".into(),
                to_prim.into(),
                species.into()
            ]
        );
    }
}

mod agent {
    use super::*;
    use test_case::test_case;

    #[test]
    fn string_create() {
        setup_test_agent();
        let object_prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let s = agent.string_create("value".into(), Some(object_prototype.clone()));

        let cod = s.o.common_object_data().borrow();
        assert_eq!(cod.prototype.as_ref().unwrap(), &object_prototype);

        let sobj = s.o.to_string_obj().unwrap();
        assert_eq!(&*sobj.string_data.borrow(), &JSString::from("value"));
    }

    #[test]
    fn create_string_object() {
        setup_test_agent();
        let string_prototype = agent.intrinsic(IntrinsicId::StringPrototype);
        let s = agent.create_string_object("value".into());

        let cod = s.o.common_object_data().borrow();
        assert_eq!(cod.prototype.as_ref().unwrap(), &string_prototype);

        let sobj = s.o.to_string_obj().unwrap();
        assert_eq!(&*sobj.string_data.borrow(), &JSString::from("value"));
    }

    mod provision_string_intrinsic {
        use super::*;
        use test_case::test_case;

        enum ToKey {
            String(String),
            Symbol(WksId),
        }
        impl From<&str> for ToKey {
            fn from(s: &str) -> Self {
                Self::String(s.to_string())
            }
        }
        impl From<WksId> for ToKey {
            fn from(id: WksId) -> Self {
                Self::Symbol(id)
            }
        }

        // Making the test agent is enough to cause intrinsics to get provisioned, so all we really do here is
        // assert that all the things we expect to show up actually do show up.

        #[test]
        fn string_prototype_intrinsic() {
            setup_test_agent();

            // The String prototype object: is %String.prototype%.
            let string_proto = agent.intrinsic(IntrinsicId::StringPrototype);

            // The String prototype object: has a [[StringData]] internal slot whose value is the empty
            // String.
            let sobj = string_proto.o.to_string_obj().unwrap();
            assert_eq!(&*sobj.string_data.borrow(), &JSString::from(""));

            // The String prototype object: has a "length" property whose initial value is +0𝔽 and whose
            // attributes are { [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: false }.
            assert_eq!(
                IdealizedPropertyDescriptor::from(
                    string_proto.o.get_own_property(&agent, &"length".into()).unwrap().unwrap()
                ),
                IdealizedPropertyDescriptor {
                    configurable: false,
                    enumerable: false,
                    writable: Some(false),
                    value: Some(ECMAScriptValue::from(0)),
                    get: None,
                    set: None
                }
            );

            // The String prototype object: has a [[Prototype]] internal slot whose value is
            // %Object.prototype%.
            let object_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
            let sproto_proto = string_proto.o.common_object_data().borrow().prototype.as_ref().unwrap().clone();
            assert_eq!(sproto_proto, object_proto);

            // The initial value of String.prototype.constructor is %String%.
            let string_constructor = agent.intrinsic(IntrinsicId::String);
            assert_eq!(
                IdealizedPropertyDescriptor::from(
                    string_proto.o.get_own_property(&agent, &"constructor".into()).unwrap().unwrap()
                ),
                IdealizedPropertyDescriptor {
                    configurable: true,
                    enumerable: false,
                    writable: Some(true),
                    value: Some(ECMAScriptValue::from(string_constructor)),
                    get: None,
                    set: None,
                }
            );
        }

        #[test_case("at" => "at;1"; "at function")]
        #[test_case("charAt" => "charAt;1"; "charAt function")]
        #[test_case("charCodeAt" => "charCodeAt;1"; "charCodeAt function")]
        #[test_case("codePointAt" => "codePointAt;1"; "codePointAt function")]
        #[test_case("concat" => "concat;1"; "concat function")]
        #[test_case("endsWith" => "endsWith;1"; "endsWith function")]
        #[test_case("includes" => "includes;1"; "includes function")]
        #[test_case("indexOf" => "indexOf;1"; "indexOf function")]
        #[test_case("lastIndexOf" => "lastIndexOf;1"; "lastIndexOf function")]
        #[test_case("localeCompare" => "localeCompare;1"; "localeCompare function")]
        #[test_case("match" => "match;1"; "match function")]
        #[test_case("matchAll" => "matchAll;1"; "matchAll function")]
        #[test_case("normalize" => "normalize;0"; "normalize function")]
        #[test_case("padEnd" => "padEnd;1"; "padEnd function")]
        #[test_case("padStart" => "padStart;1"; "padStart function")]
        #[test_case("repeat" => "repeat;1"; "repeat function")]
        #[test_case("replace" => "replace;2"; "replace function")]
        #[test_case("replaceAll" => "replaceAll;2"; "replaceAll function")]
        #[test_case("search" => "search;1"; "search function")]
        #[test_case("slice" => "slice;1"; "slice function")]
        #[test_case("split" => "split;2"; "split function")]
        #[test_case("startsWith" => "startsWith;1"; "startsWith function")]
        #[test_case("substring" => "substring;2"; "substring function")]
        #[test_case("toLocaleLowerCase" => "toLocaleLowerCase;0"; "toLocaleLowerCase function")]
        #[test_case("toLocaleUpperCase" => "toLocaleUpperCase;0"; "toLocaleUpperCase function")]
        #[test_case("toLowerCase" => "toLowerCase;0"; "toLowerCase function")]
        #[test_case("toString" => "toString;0"; "toString function")]
        #[test_case("toUpperCase" => "toUpperCase;0"; "toUpperCase function")]
        #[test_case("trim" => "trim;0"; "trim function")]
        #[test_case("trimEnd" => "trimEnd;0"; "trimEnd function")]
        #[test_case("trimStart" => "trimStart;0"; "trimStart function")]
        #[test_case("valueOf" => "valueOf;0"; "valueOf function")]
        #[test_case(WksId::Iterator => "[Symbol.iterator];0"; "@@iterator function")]
        fn prototype_func(key: impl Into<ToKey>) -> String {
            setup_test_agent();
            let key = match key.into() {
                ToKey::String(s) => PropertyKey::from(s),
                ToKey::Symbol(id) => PropertyKey::from(agent.wks(id)),
            };
            let proto = agent.intrinsic(IntrinsicId::StringPrototype);
            let val = super::get(&agent, &proto, &key).unwrap();
            assert!(is_callable(&val));
            let name = getv(&agent, &val, &"name".into()).unwrap();
            let name = to_string(&agent, name).unwrap();
            let length = getv(&agent, &val, &"length".into()).unwrap();
            let length = to_string(&agent, length).unwrap();
            format!("{};{}", String::from(name), length)
        }

        #[test]
        fn string_intrinsic() {
            setup_test_agent();
            // The String constructor: is %String%.
            let string_object = agent.intrinsic(IntrinsicId::String);

            // The String constructor: is the initial value of the "String" property of the global object.
            let global = agent.current_realm_record().unwrap().borrow().global_object.as_ref().unwrap().clone();
            let sfg_val = get(&agent, &global, &"String".into()).unwrap();
            let string_from_global = to_object(&agent, sfg_val).unwrap();
            assert_eq!(string_object, string_from_global);

            // The String constructor: has a [[Prototype]] internal slot whose value is %Function.prototype%.
            let function_proto = agent.intrinsic(IntrinsicId::FunctionPrototype);
            let from_constructor = string_object.o.common_object_data().borrow().prototype.as_ref().unwrap().clone();
            assert_eq!(from_constructor, function_proto);

            // The initial value of String.prototype is the String prototype object. This property has the
            // attributes { [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: false }.
            let string_prototype = agent.intrinsic(IntrinsicId::StringPrototype);
            assert_eq!(
                IdealizedPropertyDescriptor::from(
                    string_object.o.get_own_property(&agent, &"prototype".into()).unwrap().unwrap(),
                ),
                IdealizedPropertyDescriptor {
                    configurable: false,
                    enumerable: false,
                    writable: Some(false),
                    get: None,
                    set: None,
                    value: Some(ECMAScriptValue::from(string_prototype))
                }
            );
        }

        #[test_case("fromCharCode" => "fromCharCode;1"; "String.fromCharCode")]
        #[test_case("fromCodePoint" => "fromCodePoint;1"; "String.fromCodePoint")]
        #[test_case("raw" => "raw;1"; "String.raw")]
        fn constructor_func(key: impl Into<ToKey>) -> String {
            setup_test_agent();
            let key = match key.into() {
                ToKey::String(s) => PropertyKey::from(s),
                ToKey::Symbol(id) => PropertyKey::from(agent.wks(id)),
            };
            let cstr = agent.intrinsic(IntrinsicId::String);
            let val = super::get(&agent, &cstr, &key).unwrap();
            assert!(is_callable(&val));
            let name = getv(&agent, &val, &"name".into()).unwrap();
            let name = to_string(&agent, name).unwrap();
            let length = getv(&agent, &val, &"length".into()).unwrap();
            let length = to_string(&agent, length).unwrap();
            format!("{};{}", String::from(name), length)
        }
    }

    #[test_case(|_| ECMAScriptValue::from("blue") => sok("blue"); "string value")]
    #[test_case(|a| ECMAScriptValue::from(a.create_string_object(JSString::from("red"))) => sok("red"); "string object value")]
    #[test_case(|_| ECMAScriptValue::Undefined => serr("TypeError: unit testing requires that 'this' be a String"); "bad value")]
    #[test_case(|a| ECMAScriptValue::from(ordinary_object_create(a, None, &[])) => serr("TypeError: unit testing requires that 'this' be a String"); "bad object value")]
    fn this_string_value(make_val: impl FnOnce(&Agent) -> ECMAScriptValue) -> Result<String, String> {
        setup_test_agent();
        let val = make_val(&agent);
        agent.this_string_value(val, "unit testing").map(String::from).map_err(|e| unwind_any_error(&agent, e))
    }
}

#[test_case(|_| (None, vec![]) => Ok((false, "".to_string())); "AsFunc / no args")]
#[test_case(|_| (None, vec![ECMAScriptValue::from(true)]) => Ok((false, "true".to_string())); "AsFunc / stringable")]
#[test_case(|a| (None, vec![ECMAScriptValue::from(a.wks(WksId::ToPrimitive))]) => Ok((false, "Symbol(Symbol.toPrimitive)".to_string())); "AsFunc / Symbol")]
#[test_case(|a| (None, vec![ECMAScriptValue::from(DeadObject::object(a))]) => serr("TypeError: get called on DeadObject"); "to_string failure")]
#[test_case(|a| (Some(DeadObject::object(a)), vec![ECMAScriptValue::Null]) => serr("TypeError: get called on DeadObject"); "get_proto_from_cstr failure")]
#[test_case(|a| (Some(a.intrinsic(IntrinsicId::String)), vec![ECMAScriptValue::Undefined]) => Ok((true, "undefined".to_string())); "AsCstr / stringable")]
fn string_constructor_function(
    make_params: impl FnOnce(&Agent) -> (Option<Object>, Vec<ECMAScriptValue>),
) -> Result<(bool, String), String> {
    setup_test_agent();
    let (new_target, arguments) = make_params(&agent);
    super::string_constructor_function(&agent, ECMAScriptValue::Undefined, new_target.as_ref(), &arguments)
        .map(|val| match val {
            ECMAScriptValue::String(s) => (false, String::from(s)),
            ECMAScriptValue::Object(o) => {
                (true, String::from(o.o.to_string_obj().unwrap().string_data.borrow().clone()))
            }
            _ => panic!("Bad value from string_constructor_function: {:?}", val),
        })
        .map_err(|err| unwind_any_error(&agent, err))
}

#[test_case(|_| vec![ECMAScriptValue::from(112), ECMAScriptValue::from(97), ECMAScriptValue::from(115), ECMAScriptValue::from(115)] => sok("pass"); "normal")]
#[test_case(|a| vec![ECMAScriptValue::from(a.wks(WksId::ToPrimitive))] => serr("TypeError: Symbol values cannot be converted to Number values"); "bad args")]
#[test_case(|_| vec![] => sok(""); "emtpy args")]
fn string_from_char_code(make_params: impl FnOnce(&Agent) -> Vec<ECMAScriptValue>) -> Result<String, String> {
    setup_test_agent();
    let args = make_params(&agent);
    super::string_from_char_code(&agent, ECMAScriptValue::Undefined, None, &args)
        .map(|val| match val {
            ECMAScriptValue::String(s) => String::from(s),
            _ => panic!("Expected String value from String.fromCharCode: {:?}", val),
        })
        .map_err(|e| unwind_any_error(&agent, e))
}

#[test_case(|_| (ECMAScriptValue::Undefined, vec![]) => serr("TypeError: Undefined and null are not allowed in this context"); "'this' bad")]
#[test_case(|a| (ECMAScriptValue::from(a.create_string_object("hello".into())), vec![ECMAScriptValue::from("ell")]) => Ok(1.0); "search from zero")]
#[test_case(|a| (ECMAScriptValue::from(a.create_string_object("hello".into())), vec![ECMAScriptValue::from("ell"), ECMAScriptValue::from(2)]) => Ok(-1.0); "search from two")]
#[test_case(|a| (ECMAScriptValue::from(DeadObject::object(a)), vec![]) => serr("TypeError: get called on DeadObject"); "unstringable this")]
#[test_case(|a| (ECMAScriptValue::from(""), vec![ECMAScriptValue::from(DeadObject::object(a))]) => serr("TypeError: get called on DeadObject"); "unstringable search")]
#[test_case(|a| (ECMAScriptValue::from(""), vec![ECMAScriptValue::from(""), ECMAScriptValue::from(DeadObject::object(a))]) => serr("TypeError: get called on DeadObject"); "unnumberable position")]
fn string_prototype_index_of(
    make_params: impl FnOnce(&Agent) -> (ECMAScriptValue, Vec<ECMAScriptValue>),
) -> Result<f64, String> {
    setup_test_agent();
    let (this_value, arguments) = make_params(&agent);
    super::string_prototype_index_of(&agent, this_value, None, &arguments)
        .map(|val| match val {
            ECMAScriptValue::Number(n) => n,
            _ => panic!("Expected number value from String.prototype.indexOf: {:?}", val),
        })
        .map_err(|e| unwind_any_error(&agent, e))
}

#[test_case(|a| ECMAScriptValue::from(a.create_string_object("a string".into())) => sok("a string"); "from string object")]
#[test_case(|a| ECMAScriptValue::from(DeadObject::object(a)) => serr("TypeError: String.prototype.toString requires that 'this' be a String"); "bad this value")]
fn string_prototype_to_string(make_params: impl FnOnce(&Agent) -> ECMAScriptValue) -> Result<String, String> {
    setup_test_agent();
    let this_value = make_params(&agent);
    super::string_prototype_to_string(&agent, this_value, None, &[])
        .map(|val| match val {
            ECMAScriptValue::String(s) => String::from(s),
            _ => panic!("Expected string value from String.prototype.toString: {:?}", val),
        })
        .map_err(|e| unwind_any_error(&agent, e))
}

#[test_case(|a| ECMAScriptValue::from(a.create_string_object("a string".into())) => sok("a string"); "from string object")]
#[test_case(|a| ECMAScriptValue::from(DeadObject::object(a)) => serr("TypeError: String.prototype.valueOf requires that 'this' be a String"); "bad this value")]
fn string_prototype_value_of(make_params: impl FnOnce(&Agent) -> ECMAScriptValue) -> Result<String, String> {
    setup_test_agent();
    let this_value = make_params(&agent);
    super::string_prototype_value_of(&agent, this_value, None, &[])
        .map(|val| match val {
            ECMAScriptValue::String(s) => String::from(s),
            _ => panic!("Expected string value from String.prototype.valueOf: {:?}", val),
        })
        .map_err(|e| unwind_any_error(&agent, e))
}

tbd_function!(string_from_code_point);
tbd_function!(string_raw);
tbd_function!(string_prototype_at);
tbd_function!(string_prototype_char_at);
tbd_function!(string_prototype_char_code_at);
tbd_function!(string_prototype_code_point_at);
tbd_function!(string_prototype_concat);
tbd_function!(string_prototype_ends_with);
tbd_function!(string_prototype_includes);
tbd_function!(string_prototype_last_index_of);
tbd_function!(string_prototype_locale_compare);
tbd_function!(string_prototype_match);
tbd_function!(string_prototype_match_all);
tbd_function!(string_prototype_normalize);
tbd_function!(string_prototype_pad_end);
tbd_function!(string_prototype_pad_start);
tbd_function!(string_prototype_repeat);
tbd_function!(string_prototype_replace);
tbd_function!(string_prototype_replace_all);
tbd_function!(string_prototype_search);
tbd_function!(string_prototype_slice);
tbd_function!(string_prototype_split);
tbd_function!(string_prototype_starts_with);
tbd_function!(string_prototype_substring);
tbd_function!(string_prototype_to_locale_lower_case);
tbd_function!(string_prototype_to_locale_upper_case);
tbd_function!(string_prototype_to_lower_case);
tbd_function!(string_prototype_to_upper_case);
tbd_function!(string_prototype_trim);
tbd_function!(string_prototype_trim_end);
tbd_function!(string_prototype_trim_start);
tbd_function!(string_prototype_iterator);
