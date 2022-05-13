use super::*;
use crate::tests::*;
use test_case::test_case;

mod symbol_object {
    use super::*;

    #[test]
    fn debug() {
        let mut agent = test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = SymbolObject { common: RefCell::new(CommonObjectData::new(&mut agent, Some(prototype), true, SYMBOL_OBJECT_SLOTS)), symbol_data: RefCell::new(None) };
        assert_ne!(format!("{:?}", so), "");
    }

    #[test]
    fn object() {
        let mut agent = test_agent();
        let object_prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let prototype = ordinary_object_create(&mut agent, Some(object_prototype), &[]);
        define_property_or_throw(&mut agent, &prototype, "marker", PotentialPropertyDescriptor::new().value("sentinel")).unwrap();

        let obj = SymbolObject::object(&mut agent, Some(prototype));

        assert!(obj.o.is_symbol_object());
        let recovered_proto = obj.o.get_prototype_of(&mut agent).unwrap().unwrap();
        let prop = super::get(&mut agent, &recovered_proto, &PropertyKey::from("marker")).unwrap();
        assert_eq!(prop, ECMAScriptValue::from("sentinel"));
    }

    #[test]
    fn symbol_data() {
        let mut agent = test_agent();
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        let so = SymbolObject {
            common: RefCell::new(CommonObjectData::new(&mut agent, Some(prototype), true, SYMBOL_OBJECT_SLOTS)),
            symbol_data: RefCell::new(Some(agent.wks(WksId::ToPrimitive))),
        };

        let sd = so.symbol_data();
        let recovered = sd.borrow().clone();
        assert_eq!(recovered, Some(agent.wks(WksId::ToPrimitive)));
    }

    #[test]
    fn is_callable_obj() {
        let mut agent = test_agent();
        let sym = agent.wks(WksId::ToPrimitive);
        let obj = create_symbol_object(&mut agent, sym);

        assert!(!obj.o.is_callable_obj());
    }

    #[test]
    fn is_number_object() {
        let mut agent = test_agent();
        let sym = agent.wks(WksId::ToPrimitive);
        let obj = create_symbol_object(&mut agent, sym);

        assert!(!obj.o.is_number_object());
    }

    #[test]
    fn is_arguments_object() {
        let mut agent = test_agent();
        let sym = agent.wks(WksId::ToPrimitive);
        let obj = create_symbol_object(&mut agent, sym);

        assert!(!obj.o.is_arguments_object());
    }

    #[test]
    fn is_boolean_object() {
        let mut agent = test_agent();
        let sym = agent.wks(WksId::ToPrimitive);
        let obj = create_symbol_object(&mut agent, sym);

        assert!(!obj.o.is_boolean_object());
    }

    #[test]
    fn is_array_object() {
        let mut agent = test_agent();
        let sym = agent.wks(WksId::ToPrimitive);
        let obj = create_symbol_object(&mut agent, sym);

        assert!(!obj.o.is_array_object());
    }

    #[test]
    fn is_error_object() {
        let mut agent = test_agent();
        let sym = agent.wks(WksId::ToPrimitive);
        let obj = create_symbol_object(&mut agent, sym);

        assert!(!obj.o.is_error_object());
    }

    #[test]
    fn is_regexp_object() {
        let mut agent = test_agent();
        let sym = agent.wks(WksId::ToPrimitive);
        let obj = create_symbol_object(&mut agent, sym);

        assert!(!obj.o.is_regexp_object());
    }

    #[test]
    fn is_proxy_object() {
        let mut agent = test_agent();
        let sym = agent.wks(WksId::ToPrimitive);
        let obj = create_symbol_object(&mut agent, sym);

        assert!(!obj.o.is_proxy_object());
    }

    #[test]
    fn is_string_object() {
        let mut agent = test_agent();
        let sym = agent.wks(WksId::ToPrimitive);
        let obj = create_symbol_object(&mut agent, sym);

        assert!(!obj.o.is_string_object());
    }

    #[test]
    fn is_date_object() {
        let mut agent = test_agent();
        let sym = agent.wks(WksId::ToPrimitive);
        let obj = create_symbol_object(&mut agent, sym);

        assert!(!obj.o.is_date_object());
    }

    #[test]
    fn to_callable_obj() {
        let mut agent = test_agent();
        let sym = agent.wks(WksId::ToPrimitive);
        let obj = create_symbol_object(&mut agent, sym);

        assert!(obj.o.to_callable_obj().is_none());
    }

    #[test]
    fn to_error_obj() {
        let mut agent = test_agent();
        let sym = agent.wks(WksId::ToPrimitive);
        let obj = create_symbol_object(&mut agent, sym);

        assert!(obj.o.to_error_obj().is_none());
    }

    #[test]
    fn to_constructable() {
        let mut agent = test_agent();
        let sym = agent.wks(WksId::ToPrimitive);
        let obj = create_symbol_object(&mut agent, sym);

        assert!(obj.o.to_constructable().is_none());
    }

    #[test]
    fn to_array_object() {
        let mut agent = test_agent();
        let sym = agent.wks(WksId::ToPrimitive);
        let obj = create_symbol_object(&mut agent, sym);

        assert!(obj.o.to_array_object().is_none());
    }

    #[test]
    fn to_boolean_obj() {
        let mut agent = test_agent();
        let sym = agent.wks(WksId::ToPrimitive);
        let obj = create_symbol_object(&mut agent, sym);

        assert!(obj.o.to_boolean_obj().is_none());
    }

    #[test]
    fn to_number_obj() {
        let mut agent = test_agent();
        let sym = agent.wks(WksId::ToPrimitive);
        let obj = create_symbol_object(&mut agent, sym);

        assert!(obj.o.to_number_obj().is_none());
    }

    #[test]
    fn to_function_obj() {
        let mut agent = test_agent();
        let sym = agent.wks(WksId::ToPrimitive);
        let obj = create_symbol_object(&mut agent, sym);

        assert!(obj.o.to_function_obj().is_none());
    }

    #[test]
    fn to_builtin_function_obj() {
        let mut agent = test_agent();
        let sym = agent.wks(WksId::ToPrimitive);
        let obj = create_symbol_object(&mut agent, sym);

        assert!(obj.o.to_builtin_function_obj().is_none());
    }

    #[test]
    fn is_ordinary() {
        let mut agent = test_agent();
        let sym = agent.wks(WksId::ToPrimitive);
        let obj = create_symbol_object(&mut agent, sym);

        assert!(obj.o.is_ordinary());
    }

    #[test]
    fn get_prototype_of() {
        let mut agent = test_agent();
        let sym = agent.wks(WksId::ToPrimitive);
        let sym_obj = create_symbol_object(&mut agent, sym);
        let proto = sym_obj.o.get_prototype_of(&mut agent).unwrap().unwrap();
        assert_eq!(proto, agent.intrinsic(IntrinsicId::SymbolPrototype));
    }

    #[test]
    fn set_prototype_of() {
        let mut agent = test_agent();
        let sym = agent.wks(WksId::ToPrimitive);
        let sym_obj = create_symbol_object(&mut agent, sym);
        let res = sym_obj.o.set_prototype_of(&mut agent, None).unwrap();
        assert!(res);
        assert!(sym_obj.o.get_prototype_of(&mut agent).unwrap().is_none());
    }

    #[test]
    fn is_extensible() {
        let mut agent = test_agent();
        let sym = agent.wks(WksId::ToPrimitive);
        let sym_obj = create_symbol_object(&mut agent, sym);
        let res = sym_obj.o.is_extensible(&mut agent).unwrap();
        assert!(res);
    }

    #[test]
    fn prevent_extensions() {
        let mut agent = test_agent();
        let sym = agent.wks(WksId::ToPrimitive);
        let sym_obj = create_symbol_object(&mut agent, sym);
        let res = sym_obj.o.prevent_extensions(&mut agent).unwrap();
        assert!(res);
        assert!(!sym_obj.o.is_extensible(&mut agent).unwrap());
    }

    #[test]
    fn define_and_get_own_property() {
        let mut agent = test_agent();
        let sym = agent.wks(WksId::ToPrimitive);
        let sym_obj = create_symbol_object(&mut agent, sym);
        let res = sym_obj.o.define_own_property(&mut agent, PropertyKey::from("rust"), PotentialPropertyDescriptor::new().value("is awesome")).unwrap();
        assert!(res);
        let val = sym_obj.o.get_own_property(&mut agent, &PropertyKey::from("rust")).unwrap().unwrap();
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
        let mut agent = test_agent();
        let sym = agent.wks(WksId::ToPrimitive);
        let sym_obj = create_symbol_object(&mut agent, sym);
        let res = sym_obj.o.has_property(&mut agent, &PropertyKey::from("rust")).unwrap();
        assert_eq!(res, false);
        let tst = agent.wks(WksId::ToStringTag);
        let res2 = sym_obj.o.has_property(&mut agent, &PropertyKey::from(tst)).unwrap();
        assert_eq!(res2, true);
    }

    #[test]
    fn get() {
        let mut agent = test_agent();
        let sym = agent.wks(WksId::ToPrimitive);
        let sym_obj = create_symbol_object(&mut agent, sym);
        let res = sym_obj.o.get(&mut agent, &PropertyKey::from("rust"), &ECMAScriptValue::Undefined).unwrap();
        assert_eq!(res, ECMAScriptValue::Undefined);
        let tst = agent.wks(WksId::ToStringTag);
        let res2 = sym_obj.o.get(&mut agent, &PropertyKey::from(tst), &ECMAScriptValue::Undefined).unwrap();
        assert_eq!(res2, ECMAScriptValue::from("Symbol"));
    }

    #[test]
    fn set() {
        let mut agent = test_agent();
        let sym = agent.wks(WksId::ToPrimitive);
        let sym_obj = create_symbol_object(&mut agent, sym);
        let receiver = ECMAScriptValue::Object(sym_obj.clone());
        let res = sym_obj.o.set(&mut agent, PropertyKey::from("rust"), ECMAScriptValue::Null, &receiver).unwrap();
        assert_eq!(res, true);
    }

    #[test]
    fn delete() {
        let mut agent = test_agent();
        let sym = agent.wks(WksId::ToPrimitive);
        let sym_obj = create_symbol_object(&mut agent, sym);
        let res = sym_obj.o.delete(&mut agent, &PropertyKey::from("rust")).unwrap();
        assert_eq!(res, true);
    }

    #[test]
    fn own_keys() {
        let mut agent = test_agent();
        let sym = agent.wks(WksId::ToPrimitive);
        let sym_obj = create_symbol_object(&mut agent, sym);
        let res = sym_obj.o.own_property_keys(&mut agent).unwrap();
        assert!(res.is_empty())
    }

    #[test]
    fn id() {
        let mut agent = test_agent();
        let sym = agent.wks(WksId::ToPrimitive);
        let sym_obj = create_symbol_object(&mut agent, sym.clone());
        let sym_obj2 = create_symbol_object(&mut agent, sym);
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

#[test_case(|a| Some(ordinary_object_create(a, None, &[])), |_| vec![] => serr("TypeError: Symbol is not a constructor"); "called as constructor")]
#[test_case(|_| None, |_| vec![] => using symbol_match("Symbol()"); "empty description")]
#[test_case(|_| None, |_| vec![ECMAScriptValue::from("giants")] => using symbol_match("Symbol(giants)"); "with description")]
#[test_case(|_| None, |a| vec![ECMAScriptValue::from(a.wks(WksId::ToPrimitive))] => serr("TypeError: Symbols may not be converted to strings"); "with bad description")]
fn symbol_constructor_function(tgt_maker: fn(&mut Agent) -> Option<Object>, arg_maker: fn(&mut Agent) -> Vec<ECMAScriptValue>) -> Result<ECMAScriptValue, String> {
    let mut agent = test_agent();
    let nt = tgt_maker(&mut agent);
    let args = arg_maker(&mut agent);
    super::symbol_constructor_function(&mut agent, ECMAScriptValue::Undefined, nt.as_ref(), &args).map_err(|e| unwind_any_error(&mut agent, e))
}

mod symbol_for {
    use super::*;

    #[test]
    fn new() {
        let mut agent = test_agent();
        let gsr = agent.global_symbol_registry();
        let count_prior = gsr.borrow().len();
        let result = symbol_for(&mut agent, ECMAScriptValue::Undefined, None, &["key".into()]);
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
        let mut agent = test_agent();
        let gsr = agent.global_symbol_registry();
        let count_prior = gsr.borrow().len();
        let first = symbol_for(&mut agent, ECMAScriptValue::Undefined, None, &["key".into()]);
        let second = symbol_for(&mut agent, ECMAScriptValue::Undefined, None, &["key".into()]);
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
        let mut agent = test_agent();
        let to_primitive = agent.wks(WksId::ToPrimitive);
        let result = symbol_for(&mut agent, ECMAScriptValue::Undefined, None, &[to_primitive.into()]).unwrap_err();
        assert_eq!(unwind_any_error(&mut agent, result), "TypeError: Symbols may not be converted to strings");
    }
}

mod symbol_key_for {
    use super::*;

    #[test]
    fn not_symbol() {
        let mut agent = test_agent();
        let this_value = ECMAScriptValue::Undefined;
        let new_target = None;
        let arguments = &[];

        let result = symbol_key_for(&mut agent, this_value, new_target, arguments);

        assert_eq!(unwind_any_error(&mut agent, result.unwrap_err()), "TypeError: value is not a symbol");
    }

    #[test]
    fn not_in_registry() {
        let mut agent = test_agent();
        let this_value = ECMAScriptValue::Undefined;
        let new_target = None;
        let sym = agent.wks(WksId::ToPrimitive);
        let arguments = &[ECMAScriptValue::from(sym)];

        let result = symbol_key_for(&mut agent, this_value, new_target, arguments);

        assert_eq!(result.unwrap(), ECMAScriptValue::Undefined);
    }

    #[test]
    fn in_registry() {
        let mut agent = test_agent();
        let this_value = ECMAScriptValue::Undefined;
        let new_target = None;
        let registry_sym = symbol_for(&mut agent, ECMAScriptValue::Undefined, new_target, &["test_sentinel".into()]).unwrap();
        let arguments = &[registry_sym];

        let results = symbol_key_for(&mut agent, this_value, new_target, arguments);

        assert_eq!(results.unwrap().to_string(), "test_sentinel");
    }
}

mod this_symbol_value {
    use super::*;

    #[test]
    fn not_symbol() {
        let mut agent = test_agent();
        let this_value = ECMAScriptValue::Undefined;

        let result = this_symbol_value(&mut agent, this_value);

        assert_eq!(unwind_any_error(&mut agent, result.unwrap_err()), "TypeError: Not a symbol");
    }

    #[test]
    fn symbol() {
        let mut agent = test_agent();
        let sym = Symbol::new(&mut agent, Some("test_sentinel".into()));
        let this_value = ECMAScriptValue::from(sym.clone());

        let result = this_symbol_value(&mut agent, this_value).unwrap();

        assert_eq!(result, sym);
        assert_eq!(result.description(), sym.description());
    }

    #[test]
    fn symbol_in_object() {
        let mut agent = test_agent();
        let sym = Symbol::new(&mut agent, Some("test_sentinel".into()));
        let o = create_symbol_object(&mut agent, sym.clone());
        let this_value = ECMAScriptValue::from(o);

        let result = this_symbol_value(&mut agent, this_value).unwrap();

        assert_eq!(result, sym);
        assert_eq!(String::from(result.description().unwrap()), "test_sentinel");
    }
}

mod symbol_registry {
    use super::*;

    #[test]
    fn debug() {
        let sr = SymbolRegistry::new();
        assert_ne!(format!("{:?}", sr), "");
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
        let mut agent = test_agent();
        let s1 = Symbol::new(&mut agent, Some("fisrt".into()));
        let s2 = Symbol::new(&mut agent, Some("second".into()));
        let s3: Symbol = Symbol::new(&mut agent, Some("third".into()));
        sr.add("1".into(), s1);
        sr.add("2".into(), s2);
        sr.add("3".into(), s3);
        assert_eq!(sr.len(), 3);
    }

    mod add {
        use super::*;

        #[test]
        fn safe() {
            let mut sr = SymbolRegistry::new();
            let mut agent = test_agent();
            let s1 = Symbol::new(&mut agent, Some("fisrt".into()));
            let s2 = Symbol::new(&mut agent, Some("second".into()));
            let s3: Symbol = Symbol::new(&mut agent, Some("third".into()));
            sr.add("1".into(), s1);
            sr.add("2".into(), s2);
            sr.add("3".into(), s3);
            assert_eq!(sr.len(), 3);
        }

        #[test]
        #[should_panic(expected = "second")]
        fn duplicates() {
            let mut sr = SymbolRegistry::new();
            let mut agent = test_agent();
            let s1 = Symbol::new(&mut agent, Some("fisrt".into()));
            let s2 = Symbol::new(&mut agent, Some("second".into()));
            sr.add("1".into(), s1);
            sr.add("1".into(), s2);
        }
    }

    #[test]
    fn key_by_symbol() {
        let mut sr = SymbolRegistry::new();
        let mut agent = test_agent();
        let s1 = Symbol::new(&mut agent, Some("fisrt".into()));
        let s2 = Symbol::new(&mut agent, Some("second".into()));
        let s3 = Symbol::new(&mut agent, Some("third".into()));
        let s4 = Symbol::new(&mut agent, Some("fourth".into()));
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
        let mut agent = test_agent();
        let s1 = Symbol::new(&mut agent, Some("fisrt".into()));
        let s2 = Symbol::new(&mut agent, Some("second".into()));
        let s3 = Symbol::new(&mut agent, Some("third".into()));
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
        let mut agent = test_agent();
        let s1 = Symbol::new(&mut agent, Some("train".into()));
        let sobj = create_symbol_object(&mut agent, s1.clone());
        assert_eq!(s1, this_symbol_value(&mut agent, sobj.into()).unwrap());
    }
}

mod symbol_to_string {
    use super::*;
    use test_case::test_case;

    #[test_case(|_| ECMAScriptValue::Undefined => serr("TypeError: Not a symbol"); "Not a symbol")]
    #[test_case(|a| ECMAScriptValue::from(Symbol::new(a, Some("test sentinel".into()))) => sok("Symbol(test sentinel)"); "true symbol")]
    fn normal(maker: fn(&mut Agent) -> ECMAScriptValue) -> Result<String, String> {
        let mut agent = test_agent();
        let this_value = maker(&mut agent);
        symbol_to_string(&mut agent, this_value, None, &[]).map(|val| format!("{val}")).map_err(|ac| unwind_any_error(&mut agent, ac))
    }
}

mod symbol_value_of {
    use super::*;

    #[test]
    fn symbol() {
        let mut agent = test_agent();
        let s = Symbol::new(&mut agent, Some("test sentinel".into()));
        let this_value = ECMAScriptValue::from(s.clone());
        let result = symbol_value_of(&mut agent, this_value, None, &[]).unwrap();
        assert_eq!(result, ECMAScriptValue::from(s));
    }

    #[test]
    fn error() {
        let mut agent = test_agent();
        let this_value = ECMAScriptValue::Undefined;
        let result = symbol_value_of(&mut agent, this_value, None, &[]).unwrap_err();
        assert_eq!(unwind_any_error(&mut agent, result), "TypeError: Not a symbol");
    }
}

mod symbol_description {
    use super::*;
    use test_case::test_case;

    #[test_case(None => ECMAScriptValue::Undefined; "no description")]
    #[test_case(Some("alice") => ECMAScriptValue::from("alice"); "with description")]
    fn normal(src: Option<&str>) -> ECMAScriptValue {
        let mut agent = test_agent();
        let sym = Symbol::new(&mut agent, src.map(JSString::from));
        let this_value = ECMAScriptValue::from(sym);
        symbol_description(&mut agent, this_value, None, &[]).unwrap()
    }

    #[test]
    fn bad_this() {
        let mut agent = test_agent();
        let this_value = ECMAScriptValue::Undefined;
        let result = symbol_description(&mut agent, this_value, None, &[]).unwrap_err();
        assert_eq!(unwind_any_error(&mut agent, result), "TypeError: Not a symbol");
    }
}
