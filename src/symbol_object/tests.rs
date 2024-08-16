use super::*;
use crate::tests::*;
use test_case::test_case;

mod symbol_object {
    use super::*;

    fn make() -> Object {
        Object::from(wks(WksId::ToPrimitive))
    }

    #[test]
    fn debug() {
        setup_test_agent();
        let prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let so = SymbolObject {
            common: RefCell::new(CommonObjectData::new(Some(prototype), true, SYMBOL_OBJECT_SLOTS)),
            symbol_data: wks(WksId::ToStringTag),
        };
        assert_ne!(format!("{so:?}"), "");
    }

    #[test]
    fn object() {
        setup_test_agent();
        let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let prototype = ordinary_object_create(Some(object_prototype));
        define_property_or_throw(&prototype, "marker", PotentialPropertyDescriptor::new().value("sentinel")).unwrap();

        let obj = SymbolObject::object(Some(prototype), wks(WksId::ToStringTag));

        assert!(obj.o.is_symbol_object());
        let recovered_proto = obj.o.get_prototype_of().unwrap().unwrap();
        let prop = recovered_proto.get(&PropertyKey::from("marker")).unwrap();
        assert_eq!(prop, ECMAScriptValue::from("sentinel"));
    }

    #[test]
    fn symbol_data() {
        setup_test_agent();
        let prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let so = SymbolObject {
            common: RefCell::new(CommonObjectData::new(Some(prototype), true, SYMBOL_OBJECT_SLOTS)),
            symbol_data: wks(WksId::ToPrimitive),
        };

        let sd = so.symbol_data();
        let recovered = sd.clone();
        assert_eq!(recovered, wks(WksId::ToPrimitive));
    }

    #[test]
    fn is_callable_obj() {
        setup_test_agent();
        let sym = wks(WksId::ToPrimitive);
        let obj = Object::from(sym);

        assert!(!obj.o.is_callable_obj());
    }

    #[test]
    fn is_array_object() {
        setup_test_agent();
        let sym = wks(WksId::ToPrimitive);
        let obj = Object::from(sym);

        assert!(!obj.o.is_array_object());
    }

    #[test]
    fn is_regexp_object() {
        setup_test_agent();
        let sym = wks(WksId::ToPrimitive);
        let obj = Object::from(sym);

        assert!(!obj.o.is_regexp_object());
    }

    #[test]
    fn is_proxy_object() {
        setup_test_agent();
        let sym = wks(WksId::ToPrimitive);
        let obj = Object::from(sym);

        assert!(!obj.o.is_proxy_object());
    }

    #[test]
    fn to_proxy_object() {
        setup_test_agent();
        let obj = Object::from(wks(WksId::ToPrimitive));
        assert!(obj.o.to_proxy_object().is_none());
    }

    #[test]
    fn is_string_object() {
        setup_test_agent();
        let sym = wks(WksId::ToPrimitive);
        let obj = Object::from(sym);

        assert!(!obj.o.is_string_object());
    }

    #[test]
    fn is_date_object() {
        setup_test_agent();
        let sym = wks(WksId::ToPrimitive);
        let obj = Object::from(sym);

        assert!(!obj.o.is_date_object());
    }

    #[test]
    fn to_callable_obj() {
        setup_test_agent();
        let sym = wks(WksId::ToPrimitive);
        let obj = Object::from(sym);

        assert!(obj.o.to_callable_obj().is_none());
    }

    #[test]
    fn to_constructable() {
        setup_test_agent();
        let sym = wks(WksId::ToPrimitive);
        let obj = Object::from(sym);

        assert!(obj.o.to_constructable().is_none());
    }

    #[test]
    fn to_array_object() {
        setup_test_agent();
        let sym = wks(WksId::ToPrimitive);
        let obj = Object::from(sym);

        assert!(obj.o.to_array_object().is_none());
    }

    #[test]
    fn to_boolean_obj() {
        setup_test_agent();
        let sym = wks(WksId::ToPrimitive);
        let obj = Object::from(sym);

        assert!(obj.o.to_boolean_obj().is_none());
    }

    #[test]
    fn to_number_obj() {
        setup_test_agent();
        let sym = wks(WksId::ToPrimitive);
        let obj = Object::from(sym);

        assert!(obj.o.to_number_obj().is_none());
    }

    #[test]
    fn to_function_obj() {
        setup_test_agent();
        let sym = wks(WksId::ToPrimitive);
        let obj = Object::from(sym);

        assert!(obj.o.to_function_obj().is_none());
    }

    #[test]
    fn to_builtin_function_obj() {
        setup_test_agent();
        let sym = wks(WksId::ToPrimitive);
        let obj = Object::from(sym);

        assert!(obj.o.to_builtin_function_obj().is_none());
    }

    #[test]
    fn uses_ordinary_get_prototype_of() {
        setup_test_agent();
        let sym = wks(WksId::ToPrimitive);
        let obj = Object::from(sym);

        assert!(obj.o.uses_ordinary_get_prototype_of());
    }

    #[test]
    fn is_plain_object() {
        setup_test_agent();
        let sym = wks(WksId::ToPrimitive);
        let obj = Object::from(sym);

        assert!(!obj.o.is_plain_object());
    }

    #[test]
    fn to_arguments_object() {
        setup_test_agent();
        let sym = wks(WksId::ToPrimitive);
        let obj = Object::from(sym);

        assert!(obj.o.to_arguments_object().is_none());
    }

    false_function!(is_bigint_object);
    false_function!(is_generator_object);
    none_function!(to_bigint_object);
    none_function!(to_for_in_iterator);
    none_function!(to_generator_object);
    none_function!(to_string_obj);

    #[test]
    fn get_prototype_of() {
        setup_test_agent();
        let sym = wks(WksId::ToPrimitive);
        let sym_obj = Object::from(sym);
        let proto = sym_obj.o.get_prototype_of().unwrap().unwrap();
        assert_eq!(proto, intrinsic(IntrinsicId::SymbolPrototype));
    }

    #[test]
    fn set_prototype_of() {
        setup_test_agent();
        let sym = wks(WksId::ToPrimitive);
        let sym_obj = Object::from(sym);
        let res = sym_obj.o.set_prototype_of(None).unwrap();
        assert!(res);
        assert!(sym_obj.o.get_prototype_of().unwrap().is_none());
    }

    #[test]
    fn is_extensible() {
        setup_test_agent();
        let sym = wks(WksId::ToPrimitive);
        let sym_obj = Object::from(sym);
        let res = sym_obj.o.is_extensible().unwrap();
        assert!(res);
    }

    #[test]
    fn prevent_extensions() {
        setup_test_agent();
        let sym = wks(WksId::ToPrimitive);
        let sym_obj = Object::from(sym);
        let res = sym_obj.o.prevent_extensions().unwrap();
        assert!(res);
        assert!(!sym_obj.o.is_extensible().unwrap());
    }

    #[test]
    fn define_and_get_own_property() {
        setup_test_agent();
        let sym = wks(WksId::ToPrimitive);
        let sym_obj = Object::from(sym);
        let res = sym_obj
            .o
            .define_own_property(PropertyKey::from("rust"), PotentialPropertyDescriptor::new().value("is awesome"))
            .unwrap();
        assert!(res);
        let val = sym_obj.o.get_own_property(&PropertyKey::from("rust")).unwrap().unwrap();
        assert_eq!(val.enumerable, false);
        assert_eq!(val.configurable, false);
        assert!(matches!(val.property, PropertyKind::Data(..)));
        if let PropertyKind::Data(d) = val.property {
            assert_eq!(d.value, ECMAScriptValue::from("is awesome"));
            assert_eq!(d.writable, false);
        }
    }

    #[test]
    fn has_property() {
        setup_test_agent();
        let sym = wks(WksId::ToPrimitive);
        let sym_obj = Object::from(sym);
        let res = sym_obj.o.has_property(&PropertyKey::from("rust")).unwrap();
        assert_eq!(res, false);
        let tst = wks(WksId::ToStringTag);
        let res2 = sym_obj.o.has_property(&PropertyKey::from(tst)).unwrap();
        assert_eq!(res2, true);
    }

    #[test]
    fn get() {
        setup_test_agent();
        let sym = wks(WksId::ToPrimitive);
        let sym_obj = Object::from(sym);
        let res = sym_obj.o.get(&PropertyKey::from("rust"), &ECMAScriptValue::Undefined).unwrap();
        assert_eq!(res, ECMAScriptValue::Undefined);
        let tst = wks(WksId::ToStringTag);
        let res2 = sym_obj.o.get(&PropertyKey::from(tst), &ECMAScriptValue::Undefined).unwrap();
        assert_eq!(res2, ECMAScriptValue::from("Symbol"));
    }

    #[test]
    fn set() {
        setup_test_agent();
        let sym = wks(WksId::ToPrimitive);
        let sym_obj = Object::from(sym);
        let receiver = ECMAScriptValue::Object(sym_obj.clone());
        let res = sym_obj.o.set(PropertyKey::from("rust"), ECMAScriptValue::Null, &receiver).unwrap();
        assert_eq!(res, true);
    }

    #[test]
    fn delete() {
        setup_test_agent();
        let sym = wks(WksId::ToPrimitive);
        let sym_obj = Object::from(sym);
        let res = sym_obj.o.delete(&PropertyKey::from("rust")).unwrap();
        assert_eq!(res, true);
    }

    #[test]
    fn own_keys() {
        setup_test_agent();
        let sym = wks(WksId::ToPrimitive);
        let sym_obj = Object::from(sym);
        let res = sym_obj.o.own_property_keys().unwrap();
        assert!(res.is_empty());
    }

    #[test]
    fn id() {
        setup_test_agent();
        let sym = wks(WksId::ToPrimitive);
        let sym_obj = Object::from(sym.clone());
        let sym_obj2 = Object::from(sym);
        assert_ne!(sym_obj.o.id(), sym_obj2.o.id());
    }
}

fn symbol_match(expected: &str) -> impl FnOnce(Result<ECMAScriptValue, String>) + '_ {
    move |incoming: Result<ECMAScriptValue, String>| {
        let v = incoming.unwrap();
        if let ECMAScriptValue::Symbol(s) = v {
            assert_eq!(s.descriptive_string(), expected);
        } else {
            panic!()
        }
    }
}

#[test_case(|| Some(ordinary_object_create(None)), Vec::new => serr("TypeError: Symbol is not a constructor"); "called as constructor")]
#[test_case(|| None, Vec::new => using symbol_match("Symbol()"); "empty description")]
#[test_case(|| None, || vec![ECMAScriptValue::from("giants")] => using symbol_match("Symbol(giants)"); "with description")]
#[test_case(|| None, || vec![ECMAScriptValue::from(wks(WksId::ToPrimitive))] => serr("TypeError: Symbols may not be converted to strings"); "with bad description")]
fn symbol_constructor_function(
    tgt_maker: fn() -> Option<Object>,
    arg_maker: fn() -> Vec<ECMAScriptValue>,
) -> Result<ECMAScriptValue, String> {
    setup_test_agent();
    let nt = tgt_maker();
    let args = arg_maker();
    super::symbol_constructor_function(&ECMAScriptValue::Undefined, nt.as_ref(), &args).map_err(unwind_any_error)
}

mod symbol_for {
    use super::*;

    #[test]
    fn new() {
        setup_test_agent();
        let gsr = global_symbol_registry();
        let count_prior = gsr.borrow().len();
        let result = symbol_for(&ECMAScriptValue::Undefined, None, &["key".into()]);
        if let Ok(ECMAScriptValue::Symbol(sym)) = result {
            assert_eq!(sym.descriptive_string(), "Symbol(key)");
            let count_after = gsr.borrow().len();
            assert_eq!(count_after, count_prior + 1);
        } else {
            unreachable!()
        }
    }

    #[test]
    fn duplicate() {
        setup_test_agent();
        let gsr = global_symbol_registry();
        let count_prior = gsr.borrow().len();
        let first = symbol_for(&ECMAScriptValue::Undefined, None, &["key".into()]);
        let second = symbol_for(&ECMAScriptValue::Undefined, None, &["key".into()]);
        if let (Ok(ECMAScriptValue::Symbol(first)), Ok(ECMAScriptValue::Symbol(second))) = (first, second) {
            assert_eq!(first, second);
            assert_eq!(first.descriptive_string(), "Symbol(key)");
            let count_after = gsr.borrow().len();
            assert_eq!(count_after, count_prior + 1);
        } else {
            unreachable!()
        }
    }

    #[test]
    fn bad_key() {
        setup_test_agent();
        let to_primitive = wks(WksId::ToPrimitive);
        let result = symbol_for(&ECMAScriptValue::Undefined, None, &[to_primitive.into()]).unwrap_err();
        assert_eq!(unwind_any_error(result), "TypeError: Symbols may not be converted to strings");
    }
}

mod symbol_key_for {
    use super::*;

    #[test]
    fn not_symbol() {
        setup_test_agent();
        let this_value = ECMAScriptValue::Undefined;
        let new_target = None;
        let arguments = &[];

        let result = symbol_key_for(&this_value, new_target, arguments);

        assert_eq!(unwind_any_error(result.unwrap_err()), "TypeError: value is not a symbol");
    }

    #[test]
    fn not_in_registry() {
        setup_test_agent();
        let this_value = ECMAScriptValue::Undefined;
        let new_target = None;
        let sym = wks(WksId::ToPrimitive);
        let arguments = &[ECMAScriptValue::from(sym)];

        let result = symbol_key_for(&this_value, new_target, arguments);

        assert_eq!(result.unwrap(), ECMAScriptValue::Undefined);
    }

    #[test]
    fn in_registry() {
        setup_test_agent();
        let this_value = ECMAScriptValue::Undefined;
        let new_target = None;
        let registry_sym = symbol_for(&ECMAScriptValue::Undefined, new_target, &["test_sentinel".into()]).unwrap();
        let arguments = &[registry_sym];

        let results = symbol_key_for(&this_value, new_target, arguments);

        assert_eq!(results.unwrap().to_string(), "test_sentinel");
    }
}

mod this_symbol_value {
    use super::*;

    #[test]
    fn not_symbol() {
        setup_test_agent();
        let this_value = ECMAScriptValue::Undefined;

        let result = this_symbol_value(&this_value);

        assert_eq!(unwind_any_error(result.unwrap_err()), "TypeError: Not a symbol");
    }

    #[test]
    fn symbol() {
        setup_test_agent();
        let sym = Symbol::new(Some("test_sentinel".into()));
        let this_value = ECMAScriptValue::from(sym.clone());

        let result = this_symbol_value(&this_value).unwrap();

        assert_eq!(result, sym);
        assert_eq!(result.description(), sym.description());
    }

    #[test]
    fn symbol_in_object() {
        setup_test_agent();
        let sym = Symbol::new(Some("test_sentinel".into()));
        let o = Object::from(sym.clone());
        let this_value = ECMAScriptValue::from(o);

        let result = this_symbol_value(&this_value).unwrap();

        assert_eq!(result, sym);
        assert_eq!(String::from(result.description().unwrap()), "test_sentinel");
    }
}

mod symbol_registry {
    use super::*;

    #[test]
    fn debug() {
        let sr = SymbolRegistry::new();
        assert_ne!(format!("{sr:?}"), "");
    }

    #[test]
    fn default() {
        let sr = SymbolRegistry::default();
        assert_eq!(sr.len(), 0);
    }

    #[test]
    fn new() {
        let sr = SymbolRegistry::new();
        assert_eq!(sr.len(), 0);
    }

    #[test]
    fn len() {
        let mut sr = SymbolRegistry::new();
        assert_eq!(sr.len(), 0);
        setup_test_agent();
        let s1 = Symbol::new(Some("fisrt".into()));
        let s2 = Symbol::new(Some("second".into()));
        let s3: Symbol = Symbol::new(Some("third".into()));
        sr.add("1".into(), s1);
        sr.add("2".into(), s2);
        sr.add("3".into(), s3);
        assert_eq!(sr.len(), 3);
    }

    #[test]
    fn is_empty() {
        let mut sr = SymbolRegistry::new();
        assert!(sr.is_empty());
        setup_test_agent();
        let s1 = Symbol::new(Some("fisrt".into()));
        sr.add("1".into(), s1);
        assert!(!sr.is_empty());
    }

    mod add {
        use super::*;

        #[test]
        fn safe() {
            let mut sr = SymbolRegistry::new();
            setup_test_agent();
            let s1 = Symbol::new(Some("fisrt".into()));
            let s2 = Symbol::new(Some("second".into()));
            let s3: Symbol = Symbol::new(Some("third".into()));
            sr.add("1".into(), s1);
            sr.add("2".into(), s2);
            sr.add("3".into(), s3);
            assert_eq!(sr.len(), 3);
        }

        #[test]
        #[should_panic(expected = "second")]
        fn duplicates() {
            let mut sr = SymbolRegistry::new();
            setup_test_agent();
            let s1 = Symbol::new(Some("fisrt".into()));
            let s2 = Symbol::new(Some("second".into()));
            sr.add("1".into(), s1);
            sr.add("1".into(), s2);
        }
    }

    #[test]
    fn key_by_symbol() {
        let mut sr = SymbolRegistry::new();
        setup_test_agent();
        let s1 = Symbol::new(Some("fisrt".into()));
        let s2 = Symbol::new(Some("second".into()));
        let s3 = Symbol::new(Some("third".into()));
        let s4 = Symbol::new(Some("fourth".into()));
        sr.add("1".into(), s1.clone());
        sr.add("2".into(), s2.clone());
        sr.add("3".into(), s3.clone());

        assert_eq!(sr.key_by_symbol(&s1), Some(JSString::from("1")));
        assert_eq!(sr.key_by_symbol(&s2), Some(JSString::from("2")));
        assert_eq!(sr.key_by_symbol(&s3), Some(JSString::from("3")));
        assert_eq!(sr.key_by_symbol(&s4), None);
    }

    #[test]
    fn symbol_by_key() {
        let mut sr = SymbolRegistry::new();
        setup_test_agent();
        let s1 = Symbol::new(Some("fisrt".into()));
        let s2 = Symbol::new(Some("second".into()));
        let s3 = Symbol::new(Some("third".into()));
        sr.add("1".into(), s1.clone());
        sr.add("2".into(), s2.clone());
        sr.add("3".into(), s3.clone());

        assert_eq!(sr.symbol_by_key(&"1".into()), Some(s1));
        assert_eq!(sr.symbol_by_key(&"2".into()), Some(s2));
        assert_eq!(sr.symbol_by_key(&"3".into()), Some(s3));
        assert_eq!(sr.symbol_by_key(&"4".into()), None);
    }
}

mod create_symbol_object {
    use super::*;

    #[test]
    fn normal() {
        setup_test_agent();
        let s1 = Symbol::new(Some("train".into()));
        let sobj = Object::from(s1.clone());
        assert_eq!(s1, this_symbol_value(&ECMAScriptValue::from(sobj)).unwrap());
    }
}

mod symbol_to_string {
    use super::*;
    use test_case::test_case;

    #[test_case(|| ECMAScriptValue::Undefined => serr("TypeError: Not a symbol"); "Not a symbol")]
    #[test_case(|| ECMAScriptValue::from(Symbol::new(Some("test sentinel".into()))) => sok("Symbol(test sentinel)"); "true symbol")]
    fn normal(maker: fn() -> ECMAScriptValue) -> Result<String, String> {
        setup_test_agent();
        let this_value = maker();
        symbol_to_string(&this_value, None, &[]).map(|val| format!("{val}")).map_err(unwind_any_error)
    }
}

mod symbol_value_of {
    use super::*;

    #[test]
    fn symbol() {
        setup_test_agent();
        let s = Symbol::new(Some("test sentinel".into()));
        let this_value = ECMAScriptValue::from(s.clone());
        let result = symbol_value_of(&this_value, None, &[]).unwrap();
        assert_eq!(result, ECMAScriptValue::from(s));
    }

    #[test]
    fn error() {
        setup_test_agent();
        let this_value = ECMAScriptValue::Undefined;
        let result = symbol_value_of(&this_value, None, &[]).unwrap_err();
        assert_eq!(unwind_any_error(result), "TypeError: Not a symbol");
    }
}

mod symbol_description {
    use super::*;
    use test_case::test_case;

    #[test_case(None => ECMAScriptValue::Undefined; "no description")]
    #[test_case(Some("alice") => ECMAScriptValue::from("alice"); "with description")]
    fn normal(src: Option<&str>) -> ECMAScriptValue {
        setup_test_agent();
        let sym = Symbol::new(src.map(JSString::from));
        let this_value = ECMAScriptValue::from(sym);
        symbol_description(&this_value, None, &[]).unwrap()
    }

    #[test]
    fn bad_this() {
        setup_test_agent();
        let this_value = ECMAScriptValue::Undefined;
        let result = symbol_description(&this_value, None, &[]).unwrap_err();
        assert_eq!(unwind_any_error(result), "TypeError: Not a symbol");
    }
}
