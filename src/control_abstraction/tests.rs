use super::*;
use crate::tests::*;
use test_case::test_case;

mod agent {
    use super::*;

    #[test]
    fn provision_iterator_prototype() {
        let mut agent = test_agent();

        let iterator_prototype = agent.intrinsic(IntrinsicId::IteratorPrototype);
        let object_prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);

        let iter_proto_proto = iterator_prototype.o.common_object_data().borrow().prototype.as_ref().unwrap().clone();
        assert_eq!(iter_proto_proto, object_prototype);

        let iterator_sym = agent.wks(WksId::Iterator);
        let iterator_desc = IdealizedPropertyDescriptor::from(
            iterator_prototype.o.get_own_property(&mut agent, &iterator_sym.into()).unwrap().unwrap(),
        );
        assert_eq!(iterator_desc.configurable, true);
        assert_eq!(iterator_desc.enumerable, false);
        assert_eq!(iterator_desc.writable, Some(true));
        assert!(iterator_desc.get.is_none());
        assert!(iterator_desc.set.is_none());
        if let Some(val) = iterator_desc.value {
            assert!(is_callable(&val));
        } else {
            panic!("Expected a value");
        }
    }
}

#[test_case(|_| ECMAScriptValue::Undefined => Ok(ECMAScriptValue::Undefined); "pass-thru/undefined")]
#[test_case(|_| ECMAScriptValue::from(67) => Ok(ECMAScriptValue::from(67)); "pass-thru/number")]
fn iterator_prototype_iterator(
    make_params: impl FnOnce(&mut Agent) -> ECMAScriptValue,
) -> Result<ECMAScriptValue, String> {
    let mut agent = test_agent();
    let this_value = make_params(&mut agent);
    super::iterator_prototype_iterator(&mut agent, this_value, None, &[]).map_err(|e| unwind_any_error(&mut agent, e))
}
