use super::*;
use crate::parser::testhelp::*;
use crate::tests::*;

mod parameter_map {
    use super::*;
    use test_case::test_case;

    #[test]
    fn debug() {
        let agent = test_agent();
        let env = agent.current_realm_record().unwrap().borrow().global_env.clone().unwrap();
        let pmap = ParameterMap { env, properties: vec![] };

        assert_ne!(format!("{:?}", pmap), "");
    }

    #[test]
    fn new() {
        let agent = test_agent();
        let env = agent.current_realm_record().unwrap().borrow().global_env.clone().unwrap();

        let map = ParameterMap::new(env.clone());

        assert!(map.properties.is_empty());
        assert_eq!(map.env.name(), env.name())
    }

    #[test_case(|_| PropertyKey::from("blue") => None; "Not a numeric key")]
    #[test_case(|_| PropertyKey::from("0") => Some(0); "Zero")]
    #[test_case(|_| PropertyKey::from("   10") => None; "invalid whitespace")]
    #[test_case(|_| PropertyKey::from("-9932") => None; "negative numbers")]
    #[test_case(|_| PropertyKey::from("83828") => Some(83828); "valid nonzero")]
    #[test_case(|a| PropertyKey::from(a.wks(WksId::Iterator)) => None; "symbol key")]
    fn idx_from_key(make_key: impl FnOnce(&mut Agent) -> PropertyKey) -> Option<usize> {
        let mut agent = test_agent();
        let actual_key = make_key(&mut agent);
        ParameterMap::idx_from_key(&actual_key)
    }

    #[test_case("0" => Some(0); "zero")]
    #[test_case("1" => None; "missing one")]
    #[test_case("2" => Some(2); "two")]
    #[test_case("3" => Some(3); "three")]
    #[test_case("4" => None; "too large")]
    fn to_index(key: &str) -> Option<usize> {
        let agent = test_agent();
        let env = agent.current_realm_record().unwrap().borrow().global_env.clone().unwrap();

        let pmap = ParameterMap {
            env,
            properties: vec![Some("first".into()), None, Some("third".into()), Some("fourth".into())],
        };
        pmap.to_index(&PropertyKey::from(key))
    }

    #[test_case("bob", 10 => ssome("bob"); "expand")]
    #[test_case("alice", 0 => ssome("alice"); "precede")]
    fn add_mapped_name(name: &str, loc: usize) -> Option<String> {
        let agent = test_agent();
        let env = agent.current_realm_record().unwrap().borrow().global_env.clone().unwrap();
        let mut pmap = ParameterMap::new(env);

        pmap.add_mapped_name("sentinel".into(), 5);

        pmap.add_mapped_name(JSString::from(name), loc);

        pmap.properties[loc].as_ref().map(String::from)
    }

    #[test_case(&[Some("first"), Some("second"), Some("third")], 1 => vec![ssome("first"), None, ssome("third")]; "delete existing")]
    #[test_case(&[Some("first"), Some("second"), Some("third")], 4 => panics "index out of bounds: the len is 3 but the index is 4"; "delete past end")]
    #[test_case(&[Some("first"), None, Some("third")], 1 => vec![ssome("first"), None, ssome("third")]; "delete already deleted")]
    fn delete(before: &[Option<&str>], loc: usize) -> Vec<Option<String>> {
        let agent = test_agent();
        let env = agent.current_realm_record().unwrap().borrow().global_env.clone().unwrap();
        let mut pmap = ParameterMap { env, properties: before.iter().map(|os| os.map(JSString::from)).collect() };

        pmap.delete(loc);

        pmap.properties.into_iter().map(|os| os.map(String::from)).collect()
    }

    #[test_case(&[Some("first"), Some("second"), Some("third")], 0 => ECMAScriptValue::from("first+0"); "typical")]
    #[test_case(&[Some("first"), None, Some("third")], 1 => panics "Get only used on existing values"; "get a deleted item")]
    fn get(before: &[Option<&str>], loc: usize) -> ECMAScriptValue {
        let mut agent = test_agent();
        let env = agent.current_realm_record().unwrap().borrow().global_env.clone().unwrap();
        let pmap =
            ParameterMap { env: env.clone(), properties: before.iter().map(|os| os.map(JSString::from)).collect() };
        for (idx, name) in
            before.iter().enumerate().filter_map(|(idx, os)| os.as_ref().map(|&s| (idx, JSString::from(s))))
        {
            let value = ECMAScriptValue::from(format!("{}+{}", name, idx));
            env.create_mutable_binding(&mut agent, name.clone(), false).unwrap();
            env.initialize_binding(&mut agent, &name, value).unwrap();
        }

        pmap.get(&mut agent, loc).unwrap()
    }

    #[test_case(&[Some("first"), Some("second"), Some("third")], 0, ECMAScriptValue::from("sentinel") => ECMAScriptValue::from("sentinel"); "typical")]
    #[test_case(&[Some("first"), None, Some("third")], 1, ECMAScriptValue::from("sentinel") => panics "Set only used on existing values"; "set a deleted item")]
    fn set(before: &[Option<&str>], loc: usize, val: ECMAScriptValue) -> ECMAScriptValue {
        let mut agent = test_agent();
        let env = agent.current_realm_record().unwrap().borrow().global_env.clone().unwrap();
        let pmap =
            ParameterMap { env: env.clone(), properties: before.iter().map(|os| os.map(JSString::from)).collect() };
        for (idx, name) in
            before.iter().enumerate().filter_map(|(idx, os)| os.as_ref().map(|&s| (idx, JSString::from(s))))
        {
            let value = ECMAScriptValue::from(format!("{}+{}", name, idx));
            env.create_mutable_binding(&mut agent, name.clone(), false).unwrap();
            env.initialize_binding(&mut agent, &name, value).unwrap();
        }

        pmap.set(&mut agent, loc, val).unwrap();

        env.get_binding_value(&mut agent, pmap.properties[loc].as_ref().unwrap(), true).unwrap()
    }
}

mod arguments_object {
    use super::*;
    use test_case::test_case;

    #[test]
    fn object() {
        let mut agent = test_agent();
        let env = agent.current_realm_record().unwrap().borrow().global_env.clone().unwrap();
        let pmap = ParameterMap {
            env: env.clone(),
            properties: vec![Some("from".into()), Some("the".into()), Some("test".into())],
        };

        let result = ArgumentsObject::object(&mut agent, Some(pmap));

        let d = result.o.common_object_data().borrow();

        assert_eq!(d.prototype, Some(agent.intrinsic(IntrinsicId::ObjectPrototype)));
        assert!(d.extensible);
        assert_eq!(d.slots, ARGUMENTS_OBJECT_SLOTS);
        assert!(d.private_elements.is_empty());
        assert!(d.properties.is_empty());

        let ao = result.o.to_arguments_object().unwrap();
        let pmap = ao.parameter_map.as_ref().unwrap().borrow();

        assert_eq!(pmap.env.name(), env.name());
        assert_eq!(pmap.properties, vec![Some("from".into()), Some("the".into()), Some("test".into())]);
    }

    fn test_ao(agent: &mut Agent) -> Object {
        let env = agent.current_realm_record().unwrap().borrow().global_env.clone().unwrap();
        let pmap = ParameterMap {
            env: env.clone(),
            properties: vec![Some("from".into()), Some("the".into()), Some("test".into())],
        };

        env.create_mutable_binding(agent, "from".into(), false).unwrap();
        env.create_mutable_binding(agent, "the".into(), false).unwrap();
        env.create_mutable_binding(agent, "test".into(), false).unwrap();
        env.initialize_binding(agent, &"from".into(), "value of 'from'".into()).unwrap();
        env.initialize_binding(agent, &"the".into(), "value of 'the'".into()).unwrap();
        env.initialize_binding(agent, &"test".into(), "value of 'test'".into()).unwrap();

        ArgumentsObject::object(agent, Some(pmap))
    }

    #[test_case(|ao| ao.o.is_proxy_object() => false; "is_proxy_object")]
    #[test_case(|ao| ao.o.is_number_object() => false; "is_number_object")]
    #[test_case(|ao| ao.o.is_date_object() => false; "is_date_object")]
    #[test_case(|ao| ao.o.is_boolean_object() => false; "is_boolean_object")]
    #[test_case(|ao| ao.o.is_regexp_object() => false; "is_regexp_object")]
    #[test_case(|ao| ao.o.is_callable_obj() => false; "is_callable_obj")]
    #[test_case(|ao| ao.o.is_plain_object() => false; "is_plain_object")]
    #[test_case(|ao| ao.o.is_symbol_object() => false; "is_symbol_object")]
    #[test_case(|ao| ao.o.is_string_object() => false; "is_string_object")]
    #[test_case(|ao| ao.o.is_array_object() => false; "is_array_object")]
    #[test_case(|ao| ao.o.is_error_object() => false; "is_error_object")]
    #[test_case(|ao| ao.o.is_ordinary() => true; "is_ordinary")]
    #[test_case(|ao| ao.o.is_arguments_object() => true; "is_arguments_object")]
    fn bool_stub(op: impl FnOnce(&Object) -> bool) -> bool {
        let mut agent = test_agent();
        let ao = test_ao(&mut agent);
        op(&ao)
    }

    #[test]
    fn to_array_object() {
        let mut agent = test_agent();
        let ao = test_ao(&mut agent);
        assert!(ao.o.to_array_object().is_none());
    }

    #[test]
    fn to_boolean_obj() {
        let mut agent = test_agent();
        let ao = test_ao(&mut agent);
        assert!(ao.o.to_boolean_obj().is_none());
    }

    #[test]
    fn to_error_obj() {
        let mut agent = test_agent();
        let ao = test_ao(&mut agent);
        assert!(ao.o.to_error_obj().is_none());
    }
    #[test]
    fn to_symbol_obj() {
        let mut agent = test_agent();
        let ao = test_ao(&mut agent);
        assert!(ao.o.to_symbol_obj().is_none());
    }

    #[test]
    fn to_number_obj() {
        let mut agent = test_agent();
        let ao = test_ao(&mut agent);
        assert!(ao.o.to_number_obj().is_none());
    }

    #[test]
    fn to_callable_obj() {
        let mut agent = test_agent();
        let ao = test_ao(&mut agent);
        assert!(ao.o.to_callable_obj().is_none());
    }

    #[test]
    fn to_constructable() {
        let mut agent = test_agent();
        let ao = test_ao(&mut agent);
        assert!(ao.o.to_constructable().is_none());
    }

    #[test]
    fn to_function_obj() {
        let mut agent = test_agent();
        let ao = test_ao(&mut agent);
        assert!(ao.o.to_function_obj().is_none());
    }

    #[test]
    fn to_builtin_function_obj() {
        let mut agent = test_agent();
        let ao = test_ao(&mut agent);
        assert!(ao.o.to_builtin_function_obj().is_none());
    }

    #[test]
    fn debug() {
        let mut agent = test_agent();
        let obj = test_ao(&mut agent);
        let ao = obj.o.to_arguments_object().unwrap();
        assert_ne!(format!("{:?}", ao), "");
    }

    #[test_case(test_ao, "0" => Ok(ECMAScriptValue::from("value of 'from'")); "index was there")]
    #[test_case(test_ao, "not" => Ok(ECMAScriptValue::Undefined); "prop wasn't there")]
    #[test_case(|a| { let ao = test_ao(a); ao.o.delete(a, &"1".into()).unwrap(); ao }, "1" => Ok(ECMAScriptValue::Undefined); "tried to get a deleted one")]
    #[test_case(|a| ArgumentsObject::object(a, None), "10" => Ok(ECMAScriptValue::Undefined); "unmapped")]
    fn get(make_object: impl FnOnce(&mut Agent) -> Object, propname: &str) -> Result<ECMAScriptValue, String> {
        let mut agent = test_agent();
        let obj = make_object(&mut agent);
        let receiver = ECMAScriptValue::from(obj.clone());

        obj.o.get(&mut agent, &propname.into(), &receiver).map_err(|err| unwind_any_error(&mut agent, err))
    }

    #[test_case(test_ao, "0", ECMAScriptValue::from(99), &["0", "1", "2", "from", "the", "test"] => Ok((true, vec![
        (ECMAScriptValue::from(99), ECMAScriptValue::Undefined),
        (ECMAScriptValue::from("value of 'the'"), ECMAScriptValue::Undefined),
        (ECMAScriptValue::from("value of 'test'"), ECMAScriptValue::Undefined),
        (ECMAScriptValue::Undefined, ECMAScriptValue::from(99)),
        (ECMAScriptValue::Undefined, ECMAScriptValue::from("value of 'the'")),
        (ECMAScriptValue::Undefined, ECMAScriptValue::from("value of 'test'")),
    ])); "legit")]
    #[test_case(test_ao, "10", ECMAScriptValue::from(99), &["0", "1", "2", "from", "the", "test", "10"] => Ok((true, vec![
        (ECMAScriptValue::from("value of 'from'"), ECMAScriptValue::Undefined),
        (ECMAScriptValue::from("value of 'the'"), ECMAScriptValue::Undefined),
        (ECMAScriptValue::from("value of 'test'"), ECMAScriptValue::Undefined),
        (ECMAScriptValue::Undefined, ECMAScriptValue::from("value of 'from'")),
        (ECMAScriptValue::Undefined, ECMAScriptValue::from("value of 'the'")),
        (ECMAScriptValue::Undefined, ECMAScriptValue::from("value of 'test'")),
        (ECMAScriptValue::from(99), ECMAScriptValue::Undefined),
    ])); "mapped, but not magic")]
    #[test_case(|a| ArgumentsObject::object(a, None), "0", "sentinel".into(), &["0"] => Ok((true, vec![
        (ECMAScriptValue::from("sentinel"), ECMAScriptValue::Undefined),
    ])); "unmapped")]
    fn set(
        make_object: impl FnOnce(&mut Agent) -> Object,
        propname: &str,
        val: ECMAScriptValue,
        to_check: &[&str],
    ) -> Result<(bool, Vec<(ECMAScriptValue, ECMAScriptValue)>), String> {
        let mut agent = test_agent();
        let env = agent.current_realm_record().unwrap().borrow().global_env.clone().unwrap();
        let obj = make_object(&mut agent);
        let receiver = ECMAScriptValue::from(obj.clone());

        let result =
            obj.o.set(&mut agent, propname.into(), val, &receiver).map_err(|err| unwind_any_error(&mut agent, err))?;

        let values = to_check
            .iter()
            .map(|&probe| {
                (
                    obj.o.get(&mut agent, &probe.into(), &receiver).unwrap(),
                    env.get_binding_value(&mut agent, &probe.into(), false).unwrap(),
                )
            })
            .collect::<Vec<_>>();

        Ok((result, values))
    }
}
