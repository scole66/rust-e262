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
        let prop = get(&mut agent, &recovered_proto, &PropertyKey::from("marker")).unwrap();
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

#[test]
fn symbol_for() {
    let mut agent = test_agent();
    super::symbol_for(&mut agent, ECMAScriptValue::Undefined, None, &[]).unwrap();
}

#[test]
fn symbol_key_for() {
    let mut agent = test_agent();
    super::symbol_key_for(&mut agent, ECMAScriptValue::Undefined, None, &[]).unwrap();
}
