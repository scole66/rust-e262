use super::*;
use crate::tests::*;
use ahash::AHashMap;
use test_case::test_case;

mod string_object {
    use super::*;
    use test_case::test_case;

    fn make() -> Object {
        let proto = intrinsic(IntrinsicId::StringPrototype);
        let o = StringObject::object("sentinel".into(), Some(proto));
        let proto = o.o.get_prototype_of().unwrap().unwrap();
        proto.set("proto_sentinel", true, true).unwrap();
        o
    }

    #[test]
    fn debug() {
        setup_test_agent();
        let prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject {
            common: RefCell::new(CommonObjectData::new(Some(prototype), true, STRING_OBJECT_SLOTS)),
            string_data: JSString::from("baloney"),
        };
        assert_ne!(format!("{so:?}"), "");
    }

    #[test]
    fn object() {
        setup_test_agent();
        let prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object("orange".into(), Some(prototype));

        let length = so.get(&"length".into()).unwrap();
        assert_eq!(length, ECMAScriptValue::from(6));

        let sobj = so.o.to_string_obj().unwrap();
        assert_eq!(sobj.string_data, JSString::from("orange"));
    }

    false_function!(is_array_object);
    false_function!(is_bigint_object);
    false_function!(is_callable_obj);
    false_function!(is_date_object);
    false_function!(is_generator_object);
    false_function!(is_plain_object);
    false_function!(is_proxy_object);
    false_function!(is_regexp_object);
    false_function!(is_symbol_object);
    none_function!(to_bigint_object);

    #[test]
    fn is_string_object() {
        setup_test_agent();
        let prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object("orange".into(), Some(prototype));

        assert!(so.o.is_string_object());
    }

    #[test]
    fn uses_ordinary_get_prototype_of() {
        setup_test_agent();
        let prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object("orange".into(), Some(prototype));

        assert!(so.o.uses_ordinary_get_prototype_of());
    }

    none_function!(to_arguments_object);
    none_function!(to_boolean_obj);
    none_function!(to_array_object);
    none_function!(to_callable_obj);
    none_function!(to_function_obj);
    none_function!(to_builtin_function_obj);
    none_function!(to_symbol_obj);
    none_function!(to_number_obj);
    none_function!(to_generator_object);
    none_function!(to_for_in_iterator);
    none_function!(to_proxy_object);

    #[test]
    fn to_constructable() {
        setup_test_agent();
        let prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object("orange".into(), Some(prototype));

        assert!(so.o.to_constructable().is_none());
    }

    #[test]
    fn get_prototype_of() {
        setup_test_agent();
        let str_obj = Object::from("orange");
        let proto = str_obj.o.get_prototype_of().unwrap().unwrap();
        assert_eq!(proto, intrinsic(IntrinsicId::StringPrototype));
    }

    #[test]
    fn set_prototype_of() {
        setup_test_agent();
        let str_obj = Object::from("orange");
        let res = str_obj.o.set_prototype_of(None).unwrap();
        assert!(res);
        assert!(str_obj.o.get_prototype_of().unwrap().is_none());
    }

    #[test]
    fn is_extensible() {
        setup_test_agent();
        let str_obj = Object::from("orange");
        let res = str_obj.o.is_extensible().unwrap();
        assert!(res);
    }

    #[test]
    fn prevent_extensions() {
        setup_test_agent();
        let str_obj = Object::from("orange");
        let res = str_obj.o.prevent_extensions().unwrap();
        assert!(res);
        assert!(!str_obj.o.is_extensible().unwrap());
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
        let str_obj = Object::from(value);
        let receiver = ECMAScriptValue::Object(str_obj.clone());
        let success = str_obj.o.set(key.into(), new_val.into(), &receiver).unwrap();
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
        let str_obj = Object::from("orange");
        let res = str_obj.o.delete(&PropertyKey::from("rust")).unwrap();
        assert_eq!(res, true);
    }

    #[test]
    fn id() {
        setup_test_agent();
        let str_obj = Object::from("orange");
        let str_obj2 = Object::from("orange");
        assert_ne!(str_obj.o.id(), str_obj2.o.id());
    }

    #[test]
    fn has_property() {
        setup_test_agent();
        let str_obj = Object::from("orange");
        let res = str_obj.o.has_property(&PropertyKey::from("rust")).unwrap();
        assert_eq!(res, false);
        let res2 = str_obj.o.has_property(&PropertyKey::from("length")).unwrap();
        assert_eq!(res2, true);
    }

    #[test]
    fn common_object_data() {
        setup_test_agent();
        let str_obj = Object::from("orange");
        let cod = str_obj.o.common_object_data().borrow();

        assert_eq!(cod.properties.len(), 1);
        assert!(cod.properties.contains_key(&"length".into()));
        let string_proto = intrinsic(IntrinsicId::StringPrototype);
        assert_eq!(cod.prototype.as_ref().unwrap(), &string_proto);
        assert!(cod.extensible);
        assert_eq!(cod.slots, STRING_OBJECT_SLOTS);
        assert!(cod.private_elements.is_empty());
    }

    #[test_case("orange", || wks(WksId::ToPrimitive).into() => None; "not a string")]
    #[test_case("orange", || "blue".into() => None; "not an index")]
    #[test_case("orange", || "0.5".into() => None; "not an integer")]
    #[test_case("orange", || "-0".into() => None; "neg zero")]
    #[test_case("orange", || "-10".into() => None; "negative number")]
    #[test_case("orange", || "6".into() => None; "len or greater")]
    #[test_case("orange", || "0".into() => Some(IdealizedPropertyDescriptor{configurable: false, enumerable:  true, writable: Some(false), value: Some("o".into()), get: None, set: None }); "valid; index 0")]
    #[test_case("orange", || "1".into() => Some(IdealizedPropertyDescriptor{configurable: false, enumerable:  true, writable: Some(false), value: Some("r".into()), get: None, set: None }); "valid; index 1")]
    #[test_case("orange", || "2".into() => Some(IdealizedPropertyDescriptor{configurable: false, enumerable:  true, writable: Some(false), value: Some("a".into()), get: None, set: None }); "valid; index 2")]
    #[test_case("orange", || "3".into() => Some(IdealizedPropertyDescriptor{configurable: false, enumerable:  true, writable: Some(false), value: Some("n".into()), get: None, set: None }); "valid; index 3")]
    #[test_case("orange", || "4".into() => Some(IdealizedPropertyDescriptor{configurable: false, enumerable:  true, writable: Some(false), value: Some("g".into()), get: None, set: None }); "valid; index 4")]
    #[test_case("orange", || "5".into() => Some(IdealizedPropertyDescriptor{configurable: false, enumerable:  true, writable: Some(false), value: Some("e".into()), get: None, set: None }); "valid; index 5")]
    fn string_get_own_property(
        value: &str,
        make_key: impl FnOnce() -> PropertyKey,
    ) -> Option<IdealizedPropertyDescriptor> {
        setup_test_agent();
        let probe = make_key();
        let str_obj = Object::from(value);
        str_obj.o.to_string_obj().unwrap().string_get_own_property(&probe).map(IdealizedPropertyDescriptor::from)
    }

    #[test_case("orange", "length" => Some(IdealizedPropertyDescriptor{configurable: false, enumerable: false, writable: Some(false), value: Some(ECMAScriptValue::from(6)), get: None, set: None}); "ordinary get")]
    #[test_case("orange", "3" => Some(IdealizedPropertyDescriptor{configurable: false, enumerable: true, writable: Some(false), value: Some(ECMAScriptValue::from("n")), get: None, set: None}); "stringish get")]
    #[test_case("orange", "color" => None; "key not present")]
    fn get_own_property(value: &str, key: &str) -> Option<IdealizedPropertyDescriptor> {
        setup_test_agent();
        let str_obj = Object::from(value);
        str_obj.o.get_own_property(&key.into()).unwrap().map(IdealizedPropertyDescriptor::from)
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
        let str_obj = Object::from(value);

        let success = str_obj.o.define_own_property(key.into(), new_value).unwrap();
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
        let str_obj = Object::from(value);

        let receiver = ECMAScriptValue::from(str_obj.clone());
        str_obj.o.get(&key.into(), &receiver).unwrap()
    }

    #[test]
    fn own_property_keys() {
        setup_test_agent();
        let str_obj = Object::from("orange");

        let to_prim = wks(WksId::ToPrimitive);
        let species = wks(WksId::Species);

        str_obj
            .o
            .define_own_property(
                "60".into(),
                PotentialPropertyDescriptor::new().value("q").writable(true).enumerable(true).configurable(true),
            )
            .unwrap();
        str_obj
            .o
            .define_own_property(
                "6".into(),
                PotentialPropertyDescriptor::new().value("s").writable(true).enumerable(true).configurable(true),
            )
            .unwrap();
        str_obj
            .o
            .define_own_property(
                "zebra".into(),
                PotentialPropertyDescriptor::new().value(0).writable(true).enumerable(true).configurable(true),
            )
            .unwrap();
        str_obj
            .o
            .define_own_property(
                "alpha".into(),
                PotentialPropertyDescriptor::new().value(1).writable(true).enumerable(true).configurable(true),
            )
            .unwrap();
        str_obj
            .o
            .define_own_property(
                to_prim.clone().into(),
                PotentialPropertyDescriptor::new().value(2).writable(true).enumerable(true).configurable(true),
            )
            .unwrap();
        str_obj
            .o
            .define_own_property(
                species.clone().into(),
                PotentialPropertyDescriptor::new().value(3).writable(true).enumerable(true).configurable(true),
            )
            .unwrap();

        let keys = str_obj.o.own_property_keys().unwrap();

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

#[test]
fn string_create() {
    setup_test_agent();
    let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
    let s = super::string_create("value".into(), Some(object_prototype.clone()));

    let cod = s.o.common_object_data().borrow();
    assert_eq!(cod.prototype.as_ref().unwrap(), &object_prototype);

    let sobj = s.o.to_string_obj().unwrap();
    assert_eq!(&sobj.string_data, &JSString::from("value"));
}

#[test]
fn create_string_object_tst() {
    setup_test_agent();
    let string_prototype = intrinsic(IntrinsicId::StringPrototype);
    let s = Object::from("value");

    let cod = s.o.common_object_data().borrow();
    assert_eq!(cod.prototype.as_ref().unwrap(), &string_prototype);

    let sobj = s.o.to_string_obj().unwrap();
    assert_eq!(&sobj.string_data, &JSString::from("value"));
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
        let string_proto = intrinsic(IntrinsicId::StringPrototype);

        // The String prototype object: has a [[StringData]] internal slot whose value is the empty
        // String.
        let sobj = string_proto.o.to_string_obj().unwrap();
        assert_eq!(&sobj.string_data, &JSString::from(""));

        // The String prototype object: has a "length" property whose initial value is +0𝔽 and whose
        // attributes are { [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: false }.
        assert_eq!(
            IdealizedPropertyDescriptor::from(string_proto.o.get_own_property(&"length".into()).unwrap().unwrap()),
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
        let object_proto = intrinsic(IntrinsicId::ObjectPrototype);
        let sproto_proto = string_proto.o.common_object_data().borrow().prototype.as_ref().unwrap().clone();
        assert_eq!(sproto_proto, object_proto);

        // The initial value of String.prototype.constructor is %String%.
        let string_constructor = intrinsic(IntrinsicId::String);
        assert_eq!(
            IdealizedPropertyDescriptor::from(string_proto.o.get_own_property(&"constructor".into()).unwrap().unwrap()),
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
            ToKey::Symbol(id) => PropertyKey::from(wks(id)),
        };
        let proto = intrinsic(IntrinsicId::StringPrototype);
        let val = proto.get(&key).unwrap();
        assert!(is_callable(&val));
        let name = val.get(&"name".into()).unwrap();
        let name = to_string(name).unwrap();
        let length = val.get(&"length".into()).unwrap();
        let length = to_string(length).unwrap();
        format!("{};{}", String::from(name), length)
    }

    #[test]
    fn string_intrinsic() {
        setup_test_agent();
        // The String constructor: is %String%.
        let string_object = intrinsic(IntrinsicId::String);

        // The String constructor: is the initial value of the "String" property of the global object.
        let global = current_realm_record().unwrap().borrow().global_object.as_ref().unwrap().clone();
        let sfg_val = global.get(&"String".into()).unwrap();
        let string_from_global = to_object(sfg_val).unwrap();
        assert_eq!(string_object, string_from_global);

        // The String constructor: has a [[Prototype]] internal slot whose value is %Function.prototype%.
        let function_proto = intrinsic(IntrinsicId::FunctionPrototype);
        let from_constructor = string_object.o.common_object_data().borrow().prototype.as_ref().unwrap().clone();
        assert_eq!(from_constructor, function_proto);

        // The initial value of String.prototype is the String prototype object. This property has the
        // attributes { [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: false }.
        let string_prototype = intrinsic(IntrinsicId::StringPrototype);
        assert_eq!(
            IdealizedPropertyDescriptor::from(string_object.o.get_own_property(&"prototype".into()).unwrap().unwrap(),),
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
            ToKey::Symbol(id) => PropertyKey::from(wks(id)),
        };
        let cstr = intrinsic(IntrinsicId::String);
        let val = cstr.get(&key).unwrap();
        assert!(is_callable(&val));
        let name = val.get(&"name".into()).unwrap();
        let name = to_string(name).unwrap();
        let length = val.get(&"length".into()).unwrap();
        let length = to_string(length).unwrap();
        format!("{};{}", String::from(name), length)
    }
}

#[test_case(|| ECMAScriptValue::from("blue") => sok("blue"); "string value")]
#[test_case(|| ECMAScriptValue::from(Object::from(JSString::from("red"))) => sok("red"); "string object value")]
#[test_case(|| ECMAScriptValue::Undefined => serr("TypeError: unit testing requires that 'this' be a String"); "bad value")]
#[test_case(|| ECMAScriptValue::from(ordinary_object_create(None)) => serr("TypeError: unit testing requires that 'this' be a String"); "bad object value")]
fn this_string_value(make_val: impl FnOnce() -> ECMAScriptValue) -> Result<String, String> {
    setup_test_agent();
    let val = make_val();
    super::this_string_value(val, "unit testing").map(String::from).map_err(unwind_any_error)
}

#[test_case(|| (None, vec![]) => Ok((false, String::new())); "AsFunc / no args")]
#[test_case(|| (None, vec![ECMAScriptValue::from(true)]) => Ok((false, "true".to_string())); "AsFunc / stringable")]
#[test_case(|| (None, vec![ECMAScriptValue::from(wks(WksId::ToPrimitive))]) => Ok((false, "Symbol(Symbol.toPrimitive)".to_string())); "AsFunc / Symbol")]
#[test_case(|| (None, vec![ECMAScriptValue::from(DeadObject::object())]) => serr("TypeError: get called on DeadObject"); "to_string failure")]
#[test_case(|| (Some(DeadObject::object()), vec![ECMAScriptValue::Null]) => serr("TypeError: get called on DeadObject"); "get_proto_from_cstr failure")]
#[test_case(|| (Some(intrinsic(IntrinsicId::String)), vec![ECMAScriptValue::Undefined]) => Ok((true, "undefined".to_string())); "AsCstr / stringable")]
fn string_constructor_function(
    make_params: impl FnOnce() -> (Option<Object>, Vec<ECMAScriptValue>),
) -> Result<(bool, String), String> {
    setup_test_agent();
    let (new_target, arguments) = make_params();
    super::string_constructor_function(&ECMAScriptValue::Undefined, new_target.as_ref(), &arguments)
        .map(|val| match val {
            ECMAScriptValue::String(s) => (false, String::from(s)),
            ECMAScriptValue::Object(o) => (true, String::from(o.o.to_string_obj().unwrap().string_data.clone())),
            _ => panic!("Bad value from string_constructor_function: {val:?}"),
        })
        .map_err(unwind_any_error)
}

#[test_case(|| vec![ECMAScriptValue::from(112), ECMAScriptValue::from(97), ECMAScriptValue::from(115), ECMAScriptValue::from(115)] => sok("pass"); "normal")]
#[test_case(|| vec![ECMAScriptValue::from(wks(WksId::ToPrimitive))] => serr("TypeError: Symbol values cannot be converted to Number values"); "bad args")]
#[test_case(Vec::new => sok(""); "emtpy args")]
fn string_from_char_code(make_params: impl FnOnce() -> Vec<ECMAScriptValue>) -> Result<String, String> {
    setup_test_agent();
    let args = make_params();
    super::string_from_char_code(&ECMAScriptValue::Undefined, None, &args)
        .map(|val| match val {
            ECMAScriptValue::String(s) => String::from(s),
            _ => panic!("Expected String value from String.fromCharCode: {val:?}"),
        })
        .map_err(unwind_any_error)
}

#[test_case(|| (ECMAScriptValue::Undefined, vec![]) => serr("TypeError: Undefined and null are not allowed in this context"); "'this' bad")]
#[test_case(|| (ECMAScriptValue::from(Object::from("hello")), vec![ECMAScriptValue::from("ell")]) => Ok(1.0); "search from zero")]
#[test_case(|| (ECMAScriptValue::from(Object::from("hello")), vec![ECMAScriptValue::from("ell"), ECMAScriptValue::from(2)]) => Ok(-1.0); "search from two")]
#[test_case(|| (ECMAScriptValue::from(DeadObject::object()), vec![]) => serr("TypeError: get called on DeadObject"); "unstringable this")]
#[test_case(|| (ECMAScriptValue::from(""), vec![ECMAScriptValue::from(DeadObject::object())]) => serr("TypeError: get called on DeadObject"); "unstringable search")]
#[test_case(|| (ECMAScriptValue::from(""), vec![ECMAScriptValue::from(""), ECMAScriptValue::from(DeadObject::object())]) => serr("TypeError: get called on DeadObject"); "unnumberable position")]
fn string_prototype_index_of(
    make_params: impl FnOnce() -> (ECMAScriptValue, Vec<ECMAScriptValue>),
) -> Result<f64, String> {
    setup_test_agent();
    let (this_value, arguments) = make_params();
    super::string_prototype_index_of(&this_value, None, &arguments)
        .map(|val| match val {
            ECMAScriptValue::Number(n) => n,
            _ => panic!("Expected number value from String.prototype.indexOf: {val:?}"),
        })
        .map_err(unwind_any_error)
}

#[test_case(|| ECMAScriptValue::from(Object::from("a string")) => sok("a string"); "from string object")]
#[test_case(|| ECMAScriptValue::from(DeadObject::object()) => serr("TypeError: String.prototype.toString requires that 'this' be a String"); "bad this value")]
fn string_prototype_to_string(make_params: impl FnOnce() -> ECMAScriptValue) -> Result<String, String> {
    setup_test_agent();
    let this_value = make_params();
    super::string_prototype_to_string(&this_value, None, &[])
        .map(|val| match val {
            ECMAScriptValue::String(s) => String::from(s),
            _ => panic!("Expected string value from String.prototype.toString: {val:?}"),
        })
        .map_err(unwind_any_error)
}

#[test_case(|| ECMAScriptValue::from(Object::from("a string")) => sok("a string"); "from string object")]
#[test_case(|| ECMAScriptValue::from(DeadObject::object()) => serr("TypeError: String.prototype.valueOf requires that 'this' be a String"); "bad this value")]
fn string_prototype_value_of(make_params: impl FnOnce() -> ECMAScriptValue) -> Result<String, String> {
    setup_test_agent();
    let this_value = make_params();
    super::string_prototype_value_of(&this_value, None, &[])
        .map(|val| match val {
            ECMAScriptValue::String(s) => String::from(s),
            _ => panic!("Expected string value from String.prototype.valueOf: {val:?}"),
        })
        .map_err(unwind_any_error)
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
tbd_function!(string_prototype_starts_with);
tbd_function!(string_prototype_substring);
tbd_function!(string_prototype_to_locale_lower_case);
tbd_function!(string_prototype_to_locale_upper_case);
tbd_function!(string_prototype_to_upper_case);
tbd_function!(string_prototype_trim);
tbd_function!(string_prototype_trim_end);
tbd_function!(string_prototype_trim_start);
tbd_function!(string_prototype_iterator);
