use super::*;
use crate::tests::*;
use ahash::AHashMap;

mod string_object {
    use super::*;
    use test_case::test_case;

    #[test]
    fn debug() {
        let mut agent = test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject {
            common: RefCell::new(CommonObjectData::new(&mut agent, Some(prototype), true, STRING_OBJECT_SLOTS)),
            string_data: RefCell::new(JSString::from("baloney")),
        };
        assert_ne!(format!("{:?}", so), "");
    }

    #[test]
    fn object() {
        let mut agent = test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&mut agent, "orange".into(), Some(prototype));

        let length = super::get(&mut agent, &so, &"length".into()).unwrap();
        assert_eq!(length, ECMAScriptValue::from(6));

        let sobj = so.o.to_string_obj().unwrap();
        assert_eq!(*sobj.string_data.borrow(), JSString::from("orange"));
    }
    #[test]

    fn is_boolean_object() {
        let mut agent = test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&mut agent, "orange".into(), Some(prototype));

        assert!(!so.o.is_boolean_object());
    }

    #[test]
    fn is_date_object() {
        let mut agent = test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&mut agent, "orange".into(), Some(prototype));

        assert!(!so.o.is_date_object());
    }

    #[test]
    fn is_array_object() {
        let mut agent = test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&mut agent, "orange".into(), Some(prototype));

        assert!(!so.o.is_array_object());
    }

    #[test]
    fn is_proxy_object() {
        let mut agent = test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&mut agent, "orange".into(), Some(prototype));

        assert!(!so.o.is_proxy_object());
    }

    #[test]
    fn is_symbol_object() {
        let mut agent = test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&mut agent, "orange".into(), Some(prototype));

        assert!(!so.o.is_symbol_object());
    }

    #[test]
    fn is_number_object() {
        let mut agent = test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&mut agent, "orange".into(), Some(prototype));

        assert!(!so.o.is_number_object());
    }

    #[test]
    fn is_arguments_object() {
        let mut agent = test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&mut agent, "orange".into(), Some(prototype));

        assert!(!so.o.is_arguments_object());
    }

    #[test]
    fn is_plain_object() {
        let mut agent = test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&mut agent, "orange".into(), Some(prototype));

        assert!(!so.o.is_plain_object());
    }

    #[test]
    fn is_regexp_object() {
        let mut agent = test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&mut agent, "orange".into(), Some(prototype));

        assert!(!so.o.is_regexp_object());
    }

    #[test]
    fn is_error_object() {
        let mut agent = test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&mut agent, "orange".into(), Some(prototype));

        assert!(!so.o.is_error_object());
    }

    #[test]
    fn is_callable_obj() {
        let mut agent = test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&mut agent, "orange".into(), Some(prototype));

        assert!(!so.o.is_callable_obj());
    }

    #[test]
    fn is_string_object() {
        let mut agent = test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&mut agent, "orange".into(), Some(prototype));

        assert!(so.o.is_string_object());
    }

    #[test]
    fn is_ordinary() {
        let mut agent = test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&mut agent, "orange".into(), Some(prototype));

        assert!(so.o.is_ordinary());
    }

    #[test]
    fn to_arguments_object() {
        let mut agent = test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&mut agent, "orange".into(), Some(prototype));

        assert!(so.o.to_arguments_object().is_none());
    }

    #[test]
    fn to_boolean_obj() {
        let mut agent = test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&mut agent, "orange".into(), Some(prototype));

        assert!(so.o.to_boolean_obj().is_none());
    }

    #[test]
    fn to_array_object() {
        let mut agent = test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&mut agent, "orange".into(), Some(prototype));

        assert!(so.o.to_array_object().is_none());
    }

    #[test]
    fn to_callable_obj() {
        let mut agent = test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&mut agent, "orange".into(), Some(prototype));

        assert!(so.o.to_callable_obj().is_none());
    }

    #[test]
    fn to_function_obj() {
        let mut agent = test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&mut agent, "orange".into(), Some(prototype));

        assert!(so.o.to_function_obj().is_none());
    }

    #[test]
    fn to_error_obj() {
        let mut agent = test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&mut agent, "orange".into(), Some(prototype));

        assert!(so.o.to_error_obj().is_none());
    }

    #[test]
    fn to_builtin_function_obj() {
        let mut agent = test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&mut agent, "orange".into(), Some(prototype));

        assert!(so.o.to_builtin_function_obj().is_none());
    }

    #[test]
    fn to_symbol_obj() {
        let mut agent = test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&mut agent, "orange".into(), Some(prototype));

        assert!(so.o.to_symbol_obj().is_none());
    }

    #[test]
    fn to_number_obj() {
        let mut agent = test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&mut agent, "orange".into(), Some(prototype));

        assert!(so.o.to_number_obj().is_none());
    }

    #[test]
    fn to_constructable() {
        let mut agent = test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object(&mut agent, "orange".into(), Some(prototype));

        assert!(so.o.to_constructable().is_none());
    }

    #[test]
    fn get_prototype_of() {
        let mut agent = test_agent();
        let str_obj = agent.create_string_object("orange".into());
        let proto = str_obj.o.get_prototype_of(&mut agent).unwrap().unwrap();
        assert_eq!(proto, agent.intrinsic(IntrinsicId::StringPrototype));
    }

    #[test]
    fn set_prototype_of() {
        let mut agent = test_agent();
        let str_obj = agent.create_string_object("orange".into());
        let res = str_obj.o.set_prototype_of(&mut agent, None).unwrap();
        assert!(res);
        assert!(str_obj.o.get_prototype_of(&mut agent).unwrap().is_none());
    }

    #[test]
    fn is_extensible() {
        let mut agent = test_agent();
        let str_obj = agent.create_string_object("orange".into());
        let res = str_obj.o.is_extensible(&mut agent).unwrap();
        assert!(res);
    }

    #[test]
    fn prevent_extensions() {
        let mut agent = test_agent();
        let str_obj = agent.create_string_object("orange".into());
        let res = str_obj.o.prevent_extensions(&mut agent).unwrap();
        assert!(res);
        assert!(!str_obj.o.is_extensible(&mut agent).unwrap());
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
        let mut agent = test_agent();
        let str_obj = agent.create_string_object(value.into());
        let receiver = ECMAScriptValue::Object(str_obj.clone());
        let success = str_obj.o.set(&mut agent, key.into(), new_val.into(), &receiver).unwrap();
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
        let mut agent = test_agent();
        let str_obj = agent.create_string_object("orange".into());
        let res = str_obj.o.delete(&mut agent, &PropertyKey::from("rust")).unwrap();
        assert_eq!(res, true);
    }

    #[test]
    fn id() {
        let mut agent = test_agent();
        let str_obj = agent.create_string_object("orange".into());
        let str_obj2 = agent.create_string_object("orange".into());
        assert_ne!(str_obj.o.id(), str_obj2.o.id());
    }

    #[test]
    fn has_property() {
        let mut agent = test_agent();
        let str_obj = agent.create_string_object("orange".into());
        let res = str_obj.o.has_property(&mut agent, &PropertyKey::from("rust")).unwrap();
        assert_eq!(res, false);
        let res2 = str_obj.o.has_property(&mut agent, &PropertyKey::from("length")).unwrap();
        assert_eq!(res2, true);
    }

    #[test]
    fn common_object_data() {
        let mut agent = test_agent();
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

    #[derive(Debug, PartialEq)]
    struct IdealizedPropertyDescriptor {
        configurable: bool,
        enumerable: bool,
        writable: Option<bool>,
        value: Option<ECMAScriptValue>,
        get: Option<ECMAScriptValue>,
        set: Option<ECMAScriptValue>,
    }
    impl From<PropertyDescriptor> for IdealizedPropertyDescriptor {
        fn from(pd: PropertyDescriptor) -> Self {
            let (writable, value, get, set) = match pd.property {
                PropertyKind::Data(dp) => (Some(dp.writable), Some(dp.value), None, None),
                PropertyKind::Accessor(ap) => (None, None, Some(ap.get), Some(ap.set)),
            };
            IdealizedPropertyDescriptor {
                configurable: pd.configurable,
                enumerable: pd.enumerable,
                writable,
                value,
                get,
                set,
            }
        }
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
        make_key: impl FnOnce(&mut Agent) -> PropertyKey,
    ) -> Option<IdealizedPropertyDescriptor> {
        let mut agent = test_agent();
        let probe = make_key(&mut agent);
        let str_obj = agent.create_string_object(value.into());
        str_obj.o.to_string_obj().unwrap().string_get_own_property(&probe).map(IdealizedPropertyDescriptor::from)
    }

    #[test_case("orange", "length" => Some(IdealizedPropertyDescriptor{configurable: false, enumerable: false, writable: Some(false), value: Some(ECMAScriptValue::from(6)), get: None, set: None}); "ordinary get")]
    #[test_case("orange", "3" => Some(IdealizedPropertyDescriptor{configurable: false, enumerable: true, writable: Some(false), value: Some(ECMAScriptValue::from("n")), get: None, set: None}); "stringish get")]
    #[test_case("orange", "color" => None; "key not present")]
    fn get_own_property(value: &str, key: &str) -> Option<IdealizedPropertyDescriptor> {
        let mut agent = test_agent();
        let str_obj = agent.create_string_object(value.into());
        str_obj.o.get_own_property(&mut agent, &key.into()).unwrap().map(IdealizedPropertyDescriptor::from)
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
        let mut agent = test_agent();
        let str_obj = agent.create_string_object(value.into());

        let success = str_obj.o.define_own_property(&mut agent, key.into(), new_value).unwrap();
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
        let mut agent = test_agent();
        let str_obj = agent.create_string_object(value.into());

        let receiver = ECMAScriptValue::from(str_obj.clone());
        str_obj.o.get(&mut agent, &key.into(), &receiver).unwrap()
    }

    #[test]
    fn own_property_keys() {
        let mut agent = test_agent();
        let str_obj = agent.create_string_object("orange".into());

        let to_prim = agent.wks(WksId::ToPrimitive);
        let species = agent.wks(WksId::Species);

        str_obj
            .o
            .define_own_property(
                &mut agent,
                "60".into(),
                PotentialPropertyDescriptor::new().value("q").writable(true).enumerable(true).configurable(true),
            )
            .unwrap();
        str_obj
            .o
            .define_own_property(
                &mut agent,
                "6".into(),
                PotentialPropertyDescriptor::new().value("s").writable(true).enumerable(true).configurable(true),
            )
            .unwrap();
        str_obj
            .o
            .define_own_property(
                &mut agent,
                "zebra".into(),
                PotentialPropertyDescriptor::new().value(0).writable(true).enumerable(true).configurable(true),
            )
            .unwrap();
        str_obj
            .o
            .define_own_property(
                &mut agent,
                "alpha".into(),
                PotentialPropertyDescriptor::new().value(1).writable(true).enumerable(true).configurable(true),
            )
            .unwrap();
        str_obj
            .o
            .define_own_property(
                &mut agent,
                to_prim.clone().into(),
                PotentialPropertyDescriptor::new().value(2).writable(true).enumerable(true).configurable(true),
            )
            .unwrap();
        str_obj
            .o
            .define_own_property(
                &mut agent,
                species.clone().into(),
                PotentialPropertyDescriptor::new().value(3).writable(true).enumerable(true).configurable(true),
            )
            .unwrap();

        let keys = str_obj.o.own_property_keys(&mut agent).unwrap();

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

    #[test]
    fn string_create() {
        let mut agent = test_agent();
        let object_prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let s = agent.string_create("value".into(), Some(object_prototype.clone()));

        let cod = s.o.common_object_data().borrow();
        assert_eq!(cod.prototype.as_ref().unwrap(), &object_prototype);

        let sobj = s.o.to_string_obj().unwrap();
        assert_eq!(&*sobj.string_data.borrow(), &JSString::from("value"));
    }

    #[test]
    fn create_string_object() {
        let mut agent = test_agent();
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
        fn prototype_func(key: impl Into<ToKey>) -> String {
            let mut agent = test_agent();
            let key = match key.into() {
                ToKey::String(s) => PropertyKey::from(s),
                ToKey::Symbol(id) => PropertyKey::from(agent.wks(id)),
            };
            let proto = agent.intrinsic(IntrinsicId::StringPrototype);
            let val = super::get(&mut agent, &proto, &key).unwrap();
            assert!(is_callable(&val));
            let name = getv(&mut agent, &val, &"name".into()).unwrap();
            let name = to_string(&mut agent, name).unwrap();
            let length = getv(&mut agent, &val, &"length".into()).unwrap();
            let length = to_string(&mut agent, length).unwrap();
            format!("{};{}", String::from(name), length)
        }
    }
}
