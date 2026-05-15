#![expect(clippy::bool_assert_comparison)]
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

    false_function!(is_callable_obj);
    false_function!(is_date_object);
    false_function!(is_generator_object);
    false_function!(is_plain_object);
    false_function!(is_proxy_object);
    false_function!(is_regexp_object);
    false_function!(is_symbol_object);
    none_function!(to_bigint_object);
    none_function!(to_map_obj);
    none_function!(to_date_obj);
    none_function!(to_regexp_object);
    none_function!(to_bound_function_object);
    none_function!(to_builtin_function_with_revocable_proxy_slot);

    #[test]
    fn uses_ordinary_get_prototype_of() {
        setup_test_agent();
        let prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let so = StringObject::object("orange".into(), Some(prototype));

        assert!(so.o.uses_ordinary_get_prototype_of());
    }

    none_function!(to_arguments_object);
    none_function!(to_boolean_obj);
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

    #[test]
    fn kind() {
        setup_test_agent();
        let str_obj = Object::from("value");

        assert_eq!(str_obj.o.kind(), ObjectTag::String);
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
    #[test_case("slice" => "slice;2"; "slice function")]
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

fn ordinary_object_with_function_property(
    key: impl Into<PropertyKey>,
    func: impl Fn(&ECMAScriptValue, Option<&Object>, &[ECMAScriptValue]) -> Completion<ECMAScriptValue> + 'static,
) -> Object {
    let obj = ordinary_object_create(None);
    let objfunc = create_builtin_function(Box::new(func), None, 3.0, "my-replacer".into(), &[], None, None, None);
    define_property_or_throw(&obj, key, PotentialPropertyDescriptor::new().value(objfunc)).unwrap();
    obj
}
fn ordinary_object_with_getter(
    key: impl Into<PropertyKey>,
    func: impl Fn(&ECMAScriptValue, Option<&Object>, &[ECMAScriptValue]) -> Completion<ECMAScriptValue> + 'static,
) -> Object {
    let obj = ordinary_object_create(None);
    let getterfunc = create_builtin_function(Box::new(func), None, 0.0, "my-getter".into(), &[], None, None, None);
    define_property_or_throw(&obj, key, PotentialPropertyDescriptor::new().get(getterfunc)).unwrap();
    obj
}
fn ordinary_object_with_to_string(
    func: impl Fn(&ECMAScriptValue, Option<&Object>, &[ECMAScriptValue]) -> Completion<ECMAScriptValue> + 'static,
) -> Object {
    let to_string = create_builtin_function(Box::new(func), None, 0.0, "my-tostring".into(), &[], None, None, None);
    let obj = ordinary_object_create(None);
    define_property_or_throw(&obj, "toString", PotentialPropertyDescriptor::new().value(to_string)).unwrap();
    obj
}
fn ordinary_object_with_value_of(
    func: impl Fn(&ECMAScriptValue, Option<&Object>, &[ECMAScriptValue]) -> Completion<ECMAScriptValue> + 'static,
) -> Object {
    let to_string = create_builtin_function(Box::new(func), None, 0.0, "my-tostring".into(), &[], None, None, None);
    let obj = ordinary_object_create(None);
    define_property_or_throw(&obj, "valueOf", PotentialPropertyDescriptor::new().value(to_string)).unwrap();
    obj
}
fn builtin_function(
    func: impl Fn(&ECMAScriptValue, Option<&Object>, &[ECMAScriptValue]) -> Completion<ECMAScriptValue> + 'static,
) -> Object {
    create_builtin_function(Box::new(func), None, 0.0, "my-func".into(), &[], None, None, None)
}
fn ordinary_object_with_data_properties(entries: &[(&str, ECMAScriptValue)]) -> Object {
    let obj = ordinary_object_create(None);
    for (key, value) in entries {
        obj.create_data_property_or_throw(*key, value.clone()).unwrap();
    }
    obj
}

#[test_case(
    || (ECMAScriptValue::from("abc"), vec![ECMAScriptValue::from("b"), ECMAScriptValue::from("X")])
    => sok("aXc");
    "replaces first plain string match"
)]
#[test_case(
    || (ECMAScriptValue::from("ababa"), vec![ECMAScriptValue::from("a"), ECMAScriptValue::from("X")])
    => sok("Xbaba");
    "replaces only the first plain string match"
)]
#[test_case(
    || (ECMAScriptValue::from("abc"), vec![ECMAScriptValue::from("x"), ECMAScriptValue::from("X")])
    => sok("abc");
    "returns original string when search string is absent"
)]
#[test_case(
    || (ECMAScriptValue::from("abc"), vec![ECMAScriptValue::from(""), ECMAScriptValue::from("X")])
    => sok("Xabc");
    "empty search string matches at start"
)]
#[test_case(
    || (ECMAScriptValue::from("abc"), vec![ECMAScriptValue::from("b"), ECMAScriptValue::from("$$")])
    => sok("a$c");
    "replacement template dollar dollar"
)]
#[test_case(
    || (ECMAScriptValue::from("abc"), vec![ECMAScriptValue::from("b"), ECMAScriptValue::from("$&")])
    => sok("abc");
    "replacement template whole match"
)]
#[test_case(
    || (ECMAScriptValue::from("abc"), vec![ECMAScriptValue::from("b"), ECMAScriptValue::from("$`")])
    => sok("aac");
    "replacement template prefix"
)]
#[test_case(
    || (ECMAScriptValue::from("abc"), vec![ECMAScriptValue::from("b"), ECMAScriptValue::from("$'")])
    => sok("acc");
    "replacement template suffix"
)]
#[test_case(
    || (ECMAScriptValue::from("abc"), vec![ECMAScriptValue::from("b"), ECMAScriptValue::from("$0")])
    => sok("a$0c");
    "replacement template dollar zero is literal"
)]
#[test_case(
    || (ECMAScriptValue::from("abc"), vec![ECMAScriptValue::from("b"), ECMAScriptValue::from("$1")])
    => sok("a$1c");
    "replacement template capture is literal with no captures"
)]
#[test_case(
    || (ECMAScriptValue::from("abc"), vec![ECMAScriptValue::from("b"), ECMAScriptValue::from("$99")])
    => sok("a$99c");
    "replacement template high capture is literal with no captures"
)]
#[test_case(
    || (ECMAScriptValue::from("abc"), vec![ECMAScriptValue::from("b"), ECMAScriptValue::from("$<name>")])
    => sok("a$<name>c");
    "named capture template is literal without named captures"
)]
#[test_case(
    || (ECMAScriptValue::from("abc"), vec![ECMAScriptValue::from("b"), ECMAScriptValue::from("$<")])
    => sok("a$<c");
    "unterminated named capture prefix is literal"
)]
#[test_case(
    || (ECMAScriptValue::from("abc"), vec![ECMAScriptValue::from("b"), ECMAScriptValue::from("$x")])
    => sok("a$xc");
    "unknown dollar template is literal"
)]
#[test_case(
    || (ECMAScriptValue::from(123), vec![ECMAScriptValue::from("2"), ECMAScriptValue::from("X")])
    => sok("1X3");
    "generic receiver is stringified"
)]
#[test_case(
    || (ECMAScriptValue::Null, vec![ECMAScriptValue::from("x"), ECMAScriptValue::from("y")])
    => serr("TypeError: Undefined and null are not allowed in this context");
    "null receiver throws"
)]
#[test_case(
    || (ECMAScriptValue::Undefined, vec![ECMAScriptValue::from("x"), ECMAScriptValue::from("y")])
    => serr("TypeError: Undefined and null are not allowed in this context");
    "undefined receiver throws"
)]
#[test_case(
    || {
        let search_value = ordinary_object_with_function_property(
            wks(WksId::Replace),
            |_, _, arguments| {
                // Called as: replacer.call(searchValue, thisValue, replaceValue)
                let receiver = arguments[0].clone();
                let replace_value = arguments[1].clone();

                let receiver = to_string(receiver)?;
                let replace_value = to_string(replace_value)?;

                Ok(ECMAScriptValue::String(
                    JSString::from("custom:")
                        .concat(receiver)
                        .concat(":")
                        .concat(replace_value),
                ))
            },
        );

        (
            ECMAScriptValue::from("abc"),
            vec![ECMAScriptValue::from(search_value), ECMAScriptValue::from("R")],
        )
    }
    => sok("custom:abc:R");
    "custom @@replace method is called"
)]
#[test_case(
    || {
        let search_value = ordinary_object_with_getter(
            wks(WksId::Replace),
            |_,_,_| Err(create_type_error("poisoned @@replace getter")),
        );

        (
            ECMAScriptValue::from("abc"),
            vec![ECMAScriptValue::from(search_value), ECMAScriptValue::from("R")],
        )
    }
    => serr("TypeError: poisoned @@replace getter");
    "@@replace getter abrupt completion is propagated"
)]
#[test_case(
    || {
        let this_value = ordinary_object_with_to_string(|_,_,_| {
            Err(create_type_error("poisoned receiver toString"))
        });

        (
            ECMAScriptValue::from(this_value),
            vec![ECMAScriptValue::from("a"), ECMAScriptValue::from("R")],
        )
    }
    => serr("TypeError: poisoned receiver toString");
    "receiver ToString abrupt completion is propagated"
)]
#[test_case(
    || {
        let search_value = ordinary_object_with_to_string(|_,_,_| {
            Err(create_type_error("poisoned searchValue toString"))
        });

        (
            ECMAScriptValue::from("abc"),
            vec![ECMAScriptValue::from(search_value), ECMAScriptValue::from("R")],
        )
    }
    => serr("TypeError: poisoned searchValue toString");
    "searchValue ToString abrupt completion is propagated"
)]
#[test_case(
    || {
        let replace_value = ordinary_object_with_to_string(|_,_,_| {
            Err(create_type_error("poisoned replaceValue toString"))
        });

        (
            ECMAScriptValue::from("abc"),
            vec![ECMAScriptValue::from("b"), ECMAScriptValue::from(replace_value)],
        )
    }
    => serr("TypeError: poisoned replaceValue toString");
    "non-callable replaceValue ToString abrupt completion is propagated"
)]
#[test_case(
    || {
        let replace_value = builtin_function(|_this_value, _new_target, arguments| {
            let matched = to_string(arguments[0].clone())?;
            let position = to_string(arguments[1].clone())?;
            let full_string = to_string(arguments[2].clone())?;

            Ok(ECMAScriptValue::String(
                JSString::from("[")
                    .concat(matched)
                    .concat("@")
                    .concat(position)
                    .concat(" in ")
                    .concat(full_string)
                    .concat("]"),
            ))
        });

        (
            ECMAScriptValue::from("abc"),
            vec![ECMAScriptValue::from("b"), ECMAScriptValue::from(replace_value)],
        )
    }
    => sok("a[b@1 in abc]c");
    "callable replaceValue is called with match index and full string"
)]
#[test_case(
    || {
        let replace_value = builtin_function(|_this_value, _new_target, _arguments| {
            Ok(ECMAScriptValue::from(123))
        });

        (
            ECMAScriptValue::from("abc"),
            vec![ECMAScriptValue::from("b"), ECMAScriptValue::from(replace_value)],
        )
    }
    => sok("a123c");
    "callable replaceValue result is stringified"
)]
#[test_case(
    || {
        let replacement_result = ordinary_object_with_to_string(|_,_,_| {
            Err(create_type_error("poisoned replacement result toString"))
        });

        let replace_value = builtin_function(move |_this_value, _new_target, _arguments| {
            Ok(ECMAScriptValue::from(replacement_result.clone()))
        });

        (
            ECMAScriptValue::from("abc"),
            vec![ECMAScriptValue::from("b"), ECMAScriptValue::from(replace_value)],
        )
    }
    => serr("TypeError: poisoned replacement result toString");
    "callable replaceValue result ToString abrupt completion is propagated"
)]
#[test_case(
    || (ECMAScriptValue::from("abc undefined abc"), vec![ECMAScriptValue::Undefined, ECMAScriptValue::from("X")])
    => sok("abc X abc");
    "undefined searchValue skips custom replace lookup and stringifies"
)]
#[test_case(
    || (ECMAScriptValue::from("abc null abc"), vec![ECMAScriptValue::Null, ECMAScriptValue::from("X")])
    => sok("abc X abc");
    "null searchValue skips custom replace lookup and stringifies"
)]
fn string_prototype_replace(
    make_params: impl FnOnce() -> (ECMAScriptValue, Vec<ECMAScriptValue>),
) -> Result<String, String> {
    setup_test_agent();

    let (this_value, arguments) = make_params();

    super::string_prototype_replace(&this_value, None, &arguments)
        .map(|val| match val {
            ECMAScriptValue::String(s) => String::from(s),
            _ => panic!("Expected string value from String.prototype.replace: {val:?}"),
        })
        .map_err(unwind_any_error)
}

tbd_function!(string_prototype_substring);
tbd_function!(string_prototype_to_locale_lower_case);
tbd_function!(string_prototype_to_locale_upper_case);
tbd_function!(string_prototype_to_upper_case);
tbd_function!(string_prototype_trim);
tbd_function!(string_prototype_trim_end);
tbd_function!(string_prototype_trim_start);

#[test_case(&[], &[], 0 => Vec::<u16>::new(); "empty filler and zero length")]
#[test_case(&[], &[1], 0 => Vec::<u16>::new(); "non-empty filler and zero length")]
#[test_case(&[], &[1], 1 => vec![1]; "single exact one")]
#[test_case(&[], &[1], 4 => vec![1, 1, 1, 1]; "single repeated")]
#[test_case(&[], &[1, 2], 1 => vec![1]; "truncate before one full repeat")]
#[test_case(&[], &[1, 2], 2 => vec![1, 2]; "one exact repeat")]
#[test_case(&[], &[1, 2], 3 => vec![1, 2, 1]; "one repeat plus remainder")]
#[test_case(&[], &[1, 2], 5 => vec![1, 2, 1, 2, 1]; "multiple repeats plus remainder")]
#[test_case(&[], &[1, 2, 3], 8 => vec![1, 2, 3, 1, 2, 3, 1, 2]; "longer filler truncated")]
#[test_case(&[9, 9], &[1, 2], 3 => vec![9, 9, 1, 2, 1]; "appends to existing output")]
fn append_repeated_prefix(initial: &[u16], filler: &[u16], len: usize) -> Vec<u16> {
    let mut out = initial.to_vec();

    super::append_repeated_prefix(&mut out, filler, len);

    out
}

mod string_pad {
    use super::*;
    use test_case::test_case;

    #[test_case("", 0, "", PadPlacement::Start => ""; "empty string already at max length")]
    #[test_case("", 3, "", PadPlacement::Start => ""; "empty fill string returns original")]
    #[test_case("abc", 2, "0", PadPlacement::Start => "abc"; "start padding no-op when max length is smaller")]
    #[test_case("abc", 3, "0", PadPlacement::Start => "abc"; "start padding no-op when max length is equal")]
    #[test_case("abc", 5, "0", PadPlacement::Start => "00abc"; "start padding with single code unit fill")]
    #[test_case("abc", 8, "01", PadPlacement::Start => "01010abc"; "start padding repeats and truncates fill")]
    #[test_case("abc", 4, "xyz", PadPlacement::Start => "xabc"; "start padding truncates fill before first full repeat")]
    #[test_case("abc", 7, "xyz", PadPlacement::Start => "xyzxabc"; "start padding truncates after repeated fill")]
    #[test_case("abc", 2, "0", PadPlacement::End => "abc"; "end padding no-op when max length is smaller")]
    #[test_case("abc", 3, "0", PadPlacement::End => "abc"; "end padding no-op when max length is equal")]
    #[test_case("abc", 5, "0", PadPlacement::End => "abc00"; "end padding with single code unit fill")]
    #[test_case("abc", 8, "01", PadPlacement::End => "abc01010"; "end padding repeats and truncates fill")]
    #[test_case("abc", 4, "xyz", PadPlacement::End => "abcx"; "end padding truncates fill before first full repeat")]
    #[test_case("abc", 7, "xyz", PadPlacement::End => "abcxyzx"; "end padding truncates after repeated fill")]
    fn string_pad(source: &str, max_length: usize, fill: &str, placement: PadPlacement) -> String {
        let source = JSString::from(source);
        let fill = JSString::from(fill);

        source.string_pad(max_length, &fill, placement).to_string()
    }

    #[test_case(&[0xD83D, 0xDE00], 3, &[0x2E], PadPlacement::Start => vec![0x2E, 0xD83D, 0xDE00]; "start padding counts surrogate pair as two code units")]
    #[test_case(&[0xD83D, 0xDE00], 3, &[0x2E], PadPlacement::End => vec![0xD83D, 0xDE00, 0x2E]; "end padding counts surrogate pair as two code units")]
    #[test_case(&[0x61], 4, &[0xD83D, 0xDE00], PadPlacement::End => vec![0x61, 0xD83D, 0xDE00, 0xD83D]; "fill truncation can split surrogate pair")]
    fn string_pad_code_units(source: &[u16], max_length: usize, fill: &[u16], placement: PadPlacement) -> Vec<u16> {
        let source = JSString::from(source);
        let fill = JSString::from(fill);

        source.string_pad(max_length, &fill, placement).as_slice().to_vec()
    }
}

struct SubstitutionParams {
    matched: JSString,
    strx: JSString,
    position: usize,
    captures: Vec<Option<JSString>>,
    named_captures: Option<Object>,
    replacement_template: JSString,
}

#[test_case(
    || SubstitutionParams {
        matched: JSString::from("b"),
        strx: JSString::from("abc"),
        position: 1,
        captures: vec![],
        named_captures: None,
        replacement_template: JSString::from("X"),
    }
    => sok("X");
    "literal text"
)]
#[test_case(
    || SubstitutionParams {
        matched: JSString::from("b"),
        strx: JSString::from("abc"),
        position: 1,
        captures: vec![],
        named_captures: None,
        replacement_template: JSString::from("a$$z"),
    }
    => sok("a$z");
    "dollar dollar emits literal dollar"
)]
#[test_case(
    || SubstitutionParams {
        matched: JSString::from("b"),
        strx: JSString::from("abc"),
        position: 1,
        captures: vec![],
        named_captures: None,
        replacement_template: JSString::from("$`"),
    }
    => sok("a");
    "dollar backtick emits prefix"
)]
#[test_case(
    || SubstitutionParams {
        matched: JSString::from("b"),
        strx: JSString::from("abc"),
        position: 1,
        captures: vec![],
        named_captures: None,
        replacement_template: JSString::from("$&"),
    }
    => sok("b");
    "dollar ampersand emits whole match"
)]
#[test_case(
    || SubstitutionParams {
        matched: JSString::from("b"),
        strx: JSString::from("abc"),
        position: 1,
        captures: vec![],
        named_captures: None,
        replacement_template: JSString::from("$'"),
    }
    => sok("c");
    "dollar apostrophe emits suffix"
)]
#[test_case(
    || SubstitutionParams {
        matched: JSString::from("too long"),
        strx: JSString::from("abc"),
        position: 1,
        captures: vec![],
        named_captures: None,
        replacement_template: JSString::from("$'"),
    }
    => sok("");
    "dollar apostrophe clamps tail position"
)]
#[test_case(
    || SubstitutionParams {
        matched: JSString::from("b"),
        strx: JSString::from("abc"),
        position: 1,
        captures: vec![Some(JSString::from("first"))],
        named_captures: None,
        replacement_template: JSString::from("$1"),
    }
    => sok("first");
    "single digit capture"
)]
#[test_case(
    || SubstitutionParams {
        matched: JSString::from("b"),
        strx: JSString::from("abc"),
        position: 1,
        captures: vec![None],
        named_captures: None,
        replacement_template: JSString::from("$1"),
    }
    => sok("");
    "undefined numbered capture becomes empty string"
)]
#[test_case(
    || SubstitutionParams {
        matched: JSString::from("b"),
        strx: JSString::from("abc"),
        position: 1,
        captures: vec![],
        named_captures: None,
        replacement_template: JSString::from("$1"),
    }
    => sok("$1");
    "missing single digit capture stays literal"
)]
#[test_case(
    || SubstitutionParams {
        matched: JSString::from("b"),
        strx: JSString::from("abc"),
        position: 1,
        captures: vec![
            Some(JSString::from("1")),
            Some(JSString::from("2")),
            Some(JSString::from("3")),
            Some(JSString::from("4")),
            Some(JSString::from("5")),
            Some(JSString::from("6")),
            Some(JSString::from("7")),
            Some(JSString::from("8")),
            Some(JSString::from("9")),
            Some(JSString::from("10")),
            Some(JSString::from("11")),
            Some(JSString::from("twelve")),
        ],
        named_captures: None,
        replacement_template: JSString::from("$12"),
    }
    => sok("twelve");
    "two digit capture when present"
)]
#[test_case(
    || SubstitutionParams {
        matched: JSString::from("b"),
        strx: JSString::from("abc"),
        position: 1,
        captures: vec![Some(JSString::from("one"))],
        named_captures: None,
        replacement_template: JSString::from("$12"),
    }
    => sok("one2");
    "two digit capture falls back to one digit plus literal digit"
)]
#[test_case(
    || SubstitutionParams {
        matched: JSString::from("b"),
        strx: JSString::from("abc"),
        position: 1,
        captures: vec![],
        named_captures: None,
        replacement_template: JSString::from("$0"),
    }
    => sok("$0");
    "dollar zero is literal"
)]
#[test_case(
    || SubstitutionParams {
        matched: JSString::from("b"),
        strx: JSString::from("abc"),
        position: 1,
        captures: vec![],
        named_captures: None,
        replacement_template: JSString::from("$99"),
    }
    => sok("$99");
    "missing high two digit capture stays literal"
)]
#[test_case(
    || SubstitutionParams {
        matched: JSString::from("b"),
        strx: JSString::from("abc"),
        position: 1,
        captures: vec![],
        named_captures: None,
        replacement_template: JSString::from("$<name>"),
    }
    => sok("$<name>");
    "named capture syntax is literal without named captures"
)]
#[test_case(
    || SubstitutionParams {
        matched: JSString::from("b"),
        strx: JSString::from("abc"),
        position: 1,
        captures: vec![],
        named_captures: None,
        replacement_template: JSString::from("$<"),
    }
    => sok("$<");
    "unterminated named capture opener is literal"
)]
#[test_case(
    || SubstitutionParams {
        matched: JSString::from("b"),
        strx: JSString::from("abc"),
        position: 1,
        captures: vec![],
        named_captures: None,
        replacement_template: JSString::from("$x"),
    }
    => sok("$x");
    "unknown dollar sequence copies dollar literally then following character"
)]
#[test_case(
    || SubstitutionParams {
        matched: JSString::from("b"),
        strx: JSString::from("abc"),
        position: 1,
        captures: vec![Some(JSString::from("first"))],
        named_captures: None,
        replacement_template: JSString::from("[$`][$&][$'][$1][$$][$x]"),
    }
    => sok("[a][b][c][first][$][$x]");
    "mixed replacement template"
)]
#[test_case(
    || SubstitutionParams {
        matched: JSString::from("b"),
        strx: JSString::from("abc"),
        position: 1,
        captures: vec![],
        named_captures: Some(ordinary_object_with_data_properties(&[
            ("name", ECMAScriptValue::from("named")),
        ])),
        replacement_template: JSString::from("$<name>"),
    }
    => sok("named");
    "named capture value is substituted"
)]
#[test_case(
    || SubstitutionParams {
        matched: JSString::from("b"),
        strx: JSString::from("abc"),
        position: 1,
        captures: vec![],
        named_captures: Some(ordinary_object_with_data_properties(&[
            ("name", ECMAScriptValue::Undefined),
        ])),
        replacement_template: JSString::from("$<name>"),
    }
    => sok("");
    "undefined named capture becomes empty string"
)]
#[test_case(
    || SubstitutionParams {
        matched: JSString::from("b"),
        strx: JSString::from("abc"),
        position: 1,
        captures: vec![],
        named_captures: Some(ordinary_object_with_data_properties(&[])),
        replacement_template: JSString::from("$<missing>"),
    }
    => sok("");
    "missing named capture property becomes empty string"
)]
#[test_case(
    || {
        let capture_value = ordinary_object_with_to_string(|_, _, _| {
            Err(create_type_error("poisoned named capture toString"))
        });

        SubstitutionParams {
            matched: JSString::from("b"),
            strx: JSString::from("abc"),
            position: 1,
            captures: vec![],
            named_captures: Some(ordinary_object_with_data_properties(&[
                ("name", ECMAScriptValue::from(capture_value)),
            ])),
            replacement_template: JSString::from("$<name>"),
        }
    }
    => serr("TypeError: poisoned named capture toString");
    "named capture ToString abrupt completion is propagated"
)]
#[test_case(
    || {
        let named_captures = ordinary_object_with_getter("name", |_, _, _| {
            Err(create_type_error("poisoned named capture getter"))
        });

        SubstitutionParams {
            matched: JSString::from("b"),
            strx: JSString::from("abc"),
            position: 1,
            captures: vec![],
            named_captures: Some(named_captures),
            replacement_template: JSString::from("$<name>"),
        }
    }
    => serr("TypeError: poisoned named capture getter");
    "named capture get abrupt completion is propagated"
)]
fn get_substitution(make_params: impl FnOnce() -> SubstitutionParams) -> Result<String, String> {
    setup_test_agent();

    let params = make_params();

    super::get_substitution(
        &params.matched,
        &params.strx,
        params.position,
        &params.captures,
        params.named_captures.as_ref(),
        &params.replacement_template,
    )
    .map(String::from)
    .map_err(unwind_any_error)
}

#[test_case(
    || (ECMAScriptValue::from("abc"), vec![ECMAScriptValue::from("a")])
    => Ok(true);
    "matches at start with omitted position"
)]
#[test_case(
    || (ECMAScriptValue::from("abc"), vec![ECMAScriptValue::from("b")])
    => Ok(false);
    "does not match at start with omitted position"
)]
#[test_case(
    || (ECMAScriptValue::from("abc"), vec![ECMAScriptValue::from("b"), ECMAScriptValue::from(1)])
    => Ok(true);
    "matches at explicit position"
)]
#[test_case(
    || (ECMAScriptValue::from("abc"), vec![ECMAScriptValue::from("a"), ECMAScriptValue::from(1)])
    => Ok(false);
    "does not match at explicit position"
)]
#[test_case(
    || (ECMAScriptValue::from("abc"), vec![ECMAScriptValue::from("a"), ECMAScriptValue::from(-1)])
    => Ok(true);
    "negative position clamps to zero"
)]
#[test_case(
    || (ECMAScriptValue::from("abc"), vec![ECMAScriptValue::from(""), ECMAScriptValue::from(99)])
    => Ok(true);
    "empty search string always matches"
)]
#[test_case(
    || (ECMAScriptValue::from("abc"), vec![ECMAScriptValue::from("c"), ECMAScriptValue::from(99)])
    => Ok(false);
    "position past end clamps to length"
)]
#[test_case(
    || (ECMAScriptValue::from("abc"), vec![ECMAScriptValue::from("bc"), ECMAScriptValue::from(2)])
    => Ok(false);
    "search string extending past end does not match"
)]
#[test_case(
    || (ECMAScriptValue::from("abc"), vec![ECMAScriptValue::from("a"), ECMAScriptValue::from(f64::NAN)])
    => Ok(true);
    "nan position normalizes to zero"
)]
#[test_case(
    || (ECMAScriptValue::from("abc"), vec![ECMAScriptValue::from(""), ECMAScriptValue::from(f64::INFINITY)])
    => Ok(true);
    "positive infinity position clamps to length"
)]
#[test_case(
    || (ECMAScriptValue::from("abc"), vec![ECMAScriptValue::from("a"), ECMAScriptValue::from(f64::NEG_INFINITY)])
    => Ok(true);
    "negative infinity position clamps to zero"
)]
#[test_case(
    || (ECMAScriptValue::from(12345), vec![ECMAScriptValue::from("23"), ECMAScriptValue::from(1)])
    => Ok(true);
    "generic receiver is stringified"
)]
#[test_case(
    || (ECMAScriptValue::Null, vec![ECMAScriptValue::from("a")])
    => serr("TypeError: Undefined and null are not allowed in this context");
    "null receiver throws"
)]
#[test_case(
    || (ECMAScriptValue::Undefined, vec![ECMAScriptValue::from("a")])
    => serr("TypeError: Undefined and null are not allowed in this context");
    "undefined receiver throws"
)]
#[test_case(
    || {
        (
            ECMAScriptValue::from("abc"),
            vec![ECMAScriptValue::from(
                reg_exp_create(ECMAScriptValue::from("a"), None)
                    .expect("RegExpCreate with simple pattern should succeed"),
            )],
        )
    }
    => serr("TypeError: First argument to String.prototype.startsWith must not be a regular expression");
    "regexp search string throws"
)]
#[test_case(
    || {
        let this_value = ordinary_object_with_to_string(|_, _, _| {
            Err(create_type_error("poisoned receiver toString"))
        });

        (
            ECMAScriptValue::from(this_value),
            vec![ECMAScriptValue::from("a")],
        )
    }
    => serr("TypeError: poisoned receiver toString");
    "receiver ToString abrupt completion is propagated"
)]
#[test_case(
    || {
        let search_string = ordinary_object_with_getter(wks(WksId::Match), |_, _, _| {
            Err(create_type_error("poisoned @@match getter"))
        });

        (
            ECMAScriptValue::from("abc"),
            vec![ECMAScriptValue::from(search_string)],
        )
    }
    => serr("TypeError: poisoned @@match getter");
    "IsRegExp abrupt completion is propagated"
)]
#[test_case(
    || {
        let search_string = ordinary_object_with_to_string(|_, _, _| {
            Err(create_type_error("poisoned searchString toString"))
        });

        (
            ECMAScriptValue::from("abc"),
            vec![ECMAScriptValue::from(search_string)],
        )
    }
    => serr("TypeError: poisoned searchString toString");
    "searchString ToString abrupt completion is propagated"
)]
#[test_case(
    || {
        let position = ordinary_object_with_value_of(|_, _, _| {
            Err(create_type_error("poisoned position valueOf"))
        });

        (
            ECMAScriptValue::from("abc"),
            vec![ECMAScriptValue::from("a"), ECMAScriptValue::from(position)],
        )
    }
    => serr("TypeError: poisoned position valueOf");
    "position ToIntegerOrInfinity abrupt completion is propagated"
)]
fn string_prototype_starts_with(
    make_params: impl FnOnce() -> (ECMAScriptValue, Vec<ECMAScriptValue>),
) -> Result<bool, String> {
    setup_test_agent();

    let (this_value, arguments) = make_params();

    super::string_prototype_starts_with(&this_value, None, &arguments)
        .map(|val| match val {
            ECMAScriptValue::Boolean(b) => b,
            _ => panic!("Expected boolean value from String.prototype.startsWith: {val:?}"),
        })
        .map_err(unwind_any_error)
}

#[test_case(
    || {
        (
            ECMAScriptValue::from("abc"),
            vec![ECMAScriptValue::from(
                reg_exp_create(ECMAScriptValue::from("b"), None)
                    .expect("RegExpCreate with simple pattern should succeed"),
            )],
        )
    }
    => Ok(1.0);
    "regexp match returns index"
)]
#[test_case(
    || {
        (
            ECMAScriptValue::from("abc"),
            vec![ECMAScriptValue::from(
                reg_exp_create(ECMAScriptValue::from("x"), None)
                    .expect("RegExpCreate with simple pattern should succeed"),
            )],
        )
    }
    => Ok(-1.0);
    "regexp no match returns minus one"
)]
#[test_case(
    || (ECMAScriptValue::from("abc"), vec![ECMAScriptValue::from("b")])
    => Ok(1.0);
    "string pattern is compiled as regexp"
)]
#[test_case(
    || (ECMAScriptValue::from("abc"), vec![ECMAScriptValue::from("x")])
    => Ok(-1.0);
    "string pattern no match"
)]
#[test_case(
    || (ECMAScriptValue::from("undefined"), vec![])
    => Ok(0.0);
    "missing regexp argument searches for undefined"
)]
#[test_case(
    || (ECMAScriptValue::from("undefined"), vec![ECMAScriptValue::Undefined])
    => Ok(0.0);
    "undefined regexp argument searches for undefined"
)]
#[test_case(
    || (ECMAScriptValue::from(12345), vec![ECMAScriptValue::from("34")])
    => Ok(2.0);
    "generic receiver is stringified"
)]
#[test_case(
    || (ECMAScriptValue::Null, vec![ECMAScriptValue::from("a")])
    => serr("TypeError: Undefined and null are not allowed in this context");
    "null receiver throws"
)]
#[test_case(
    || (ECMAScriptValue::Undefined, vec![ECMAScriptValue::from("a")])
    => serr("TypeError: Undefined and null are not allowed in this context");
    "undefined receiver throws"
)]
#[test_case(
    || {
        let search_value = ordinary_object_with_function_property(
            wks(WksId::Search),
            |_this_value, _new_target, arguments| {
                let receiver = to_string(arguments[0].clone())?;
                Ok(if receiver == "abc" {
                    ECMAScriptValue::from(42)
                } else {
                    ECMAScriptValue::from(-1)
                })
            },
        );

        (
            ECMAScriptValue::from("abc"),
            vec![ECMAScriptValue::from(search_value)],
        )
    }
    => Ok(42.0);
    "custom @@search method is called"
)]
#[test_case(
    || {
        let search_value = ordinary_object_with_getter(wks(WksId::Search), |_, _, _| {
            Err(create_type_error("poisoned @@search getter"))
        });

        (
            ECMAScriptValue::from("abc"),
            vec![ECMAScriptValue::from(search_value)],
        )
    }
    => serr("TypeError: poisoned @@search getter");
    "@@search getter abrupt completion is propagated"
)]
#[test_case(
    || {
        let this_value = ordinary_object_with_to_string(|_, _, _| {
            Err(create_type_error("poisoned receiver toString"))
        });

        (
            ECMAScriptValue::from(this_value),
            vec![ECMAScriptValue::from("a")],
        )
    }
    => serr("TypeError: poisoned receiver toString");
    "receiver ToString abrupt completion is propagated"
)]
#[test_case(
    || {
        let pattern = ordinary_object_with_to_string(|_, _, _| {
            Err(create_type_error("poisoned pattern toString"))
        });

        (
            ECMAScriptValue::from("abc"),
            vec![ECMAScriptValue::from(pattern)],
        )
    }
    => serr("TypeError: poisoned pattern toString");
    "RegExpCreate pattern ToString abrupt completion is propagated"
)]
#[test_case(
    || {
        let this_value = ordinary_object_with_to_string(|_, _, _| {
            Err(create_type_error("receiver should not be stringified"))
        });

        let search_value = ordinary_object_with_function_property(
            wks(WksId::Search),
            |_, _, _| Ok(ECMAScriptValue::from(7)),
        );

        (
            ECMAScriptValue::from(this_value),
            vec![ECMAScriptValue::from(search_value)],
        )
    }
    => Ok(7.0);
    "custom @@search receives original receiver before ToString"
)]
fn string_prototype_search(
    make_params: impl FnOnce() -> (ECMAScriptValue, Vec<ECMAScriptValue>),
) -> Result<f64, String> {
    setup_test_agent();

    let (this_value, arguments) = make_params();

    super::string_prototype_search(&this_value, None, &arguments)
        .map(|val| match val {
            ECMAScriptValue::Number(n) => n,
            _ => panic!("Expected number value from String.prototype.search: {val:?}"),
        })
        .map_err(unwind_any_error)
}
