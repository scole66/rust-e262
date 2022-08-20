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
    fn idx_from_key(make_key: impl FnOnce(&Agent) -> PropertyKey) -> Option<usize> {
        let agent = test_agent();
        let actual_key = make_key(&agent);
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
        let agent = test_agent();
        let env = agent.current_realm_record().unwrap().borrow().global_env.clone().unwrap();
        let pmap =
            ParameterMap { env: env.clone(), properties: before.iter().map(|os| os.map(JSString::from)).collect() };
        for (idx, name) in
            before.iter().enumerate().filter_map(|(idx, os)| os.as_ref().map(|&s| (idx, JSString::from(s))))
        {
            let value = ECMAScriptValue::from(format!("{}+{}", name, idx));
            env.create_mutable_binding(&agent, name.clone(), false).unwrap();
            env.initialize_binding(&agent, &name, value).unwrap();
        }

        pmap.get(&agent, loc).unwrap()
    }

    #[test_case(&[Some("first"), Some("second"), Some("third")], 0, ECMAScriptValue::from("sentinel") => ECMAScriptValue::from("sentinel"); "typical")]
    #[test_case(&[Some("first"), None, Some("third")], 1, ECMAScriptValue::from("sentinel") => panics "Set only used on existing values"; "set a deleted item")]
    fn set(before: &[Option<&str>], loc: usize, val: ECMAScriptValue) -> ECMAScriptValue {
        let agent = test_agent();
        let env = agent.current_realm_record().unwrap().borrow().global_env.clone().unwrap();
        let pmap =
            ParameterMap { env: env.clone(), properties: before.iter().map(|os| os.map(JSString::from)).collect() };
        for (idx, name) in
            before.iter().enumerate().filter_map(|(idx, os)| os.as_ref().map(|&s| (idx, JSString::from(s))))
        {
            let value = ECMAScriptValue::from(format!("{}+{}", name, idx));
            env.create_mutable_binding(&agent, name.clone(), false).unwrap();
            env.initialize_binding(&agent, &name, value).unwrap();
        }

        pmap.set(&agent, loc, val).unwrap();

        env.get_binding_value(&agent, pmap.properties[loc].as_ref().unwrap(), true).unwrap()
    }
}

mod arguments_object {
    use super::*;
    use ahash::AHashMap;
    use test_case::test_case;

    #[test]
    fn object() {
        let agent = test_agent();
        let env = agent.current_realm_record().unwrap().borrow().global_env.clone().unwrap();
        let pmap = ParameterMap {
            env: env.clone(),
            properties: vec![Some("from".into()), Some("the".into()), Some("test".into())],
        };

        let result = ArgumentsObject::object(&agent, Some(pmap));

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

    fn test_ao(agent: &Agent) -> Object {
        let env = agent.current_realm_record().unwrap().borrow().global_env.clone().unwrap();
        let lexenv = Rc::new(DeclarativeEnvironmentRecord::new(Some(env), "test_ao"));
        agent.set_lexical_environment(Some(lexenv.clone() as Rc<dyn EnvironmentRecord>));
        let pmap = ParameterMap {
            env: lexenv.clone(),
            properties: vec![Some("from".into()), Some("the".into()), Some("test".into())],
        };

        lexenv.create_mutable_binding(agent, "from".into(), false).unwrap();
        lexenv.create_mutable_binding(agent, "the".into(), false).unwrap();
        lexenv.create_mutable_binding(agent, "test".into(), false).unwrap();
        lexenv.initialize_binding(agent, &"from".into(), "value of 'from'".into()).unwrap();
        lexenv.initialize_binding(agent, &"the".into(), "value of 'the'".into()).unwrap();
        lexenv.initialize_binding(agent, &"test".into(), "value of 'test'".into()).unwrap();

        let obj = ArgumentsObject::object(agent, Some(pmap));

        create_data_property_or_throw(agent, &obj, 0, "value of 'from'").unwrap();
        create_data_property_or_throw(agent, &obj, 1, "value of 'the'").unwrap();
        create_data_property_or_throw(agent, &obj, 2, "value of 'test'").unwrap();
        create_data_property_or_throw(agent, &obj, 100, "not in index").unwrap();

        obj
    }

    fn test_unmapped(agent: &Agent) -> Object {
        let obj = ArgumentsObject::object(agent, None);

        super::set(agent, &obj, "0".into(), "value of 'from'".into(), false).unwrap();
        super::set(agent, &obj, "1".into(), "value of 'the'".into(), false).unwrap();
        super::set(agent, &obj, "2".into(), "value of 'test'".into(), false).unwrap();

        obj
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
        let agent = test_agent();
        let ao = test_ao(&agent);
        op(&ao)
    }

    #[test]
    fn to_array_object() {
        let agent = test_agent();
        let ao = test_ao(&agent);
        assert!(ao.o.to_array_object().is_none());
    }

    #[test]
    fn to_boolean_obj() {
        let agent = test_agent();
        let ao = test_ao(&agent);
        assert!(ao.o.to_boolean_obj().is_none());
    }

    #[test]
    fn to_error_obj() {
        let agent = test_agent();
        let ao = test_ao(&agent);
        assert!(ao.o.to_error_obj().is_none());
    }
    #[test]
    fn to_symbol_obj() {
        let agent = test_agent();
        let ao = test_ao(&agent);
        assert!(ao.o.to_symbol_obj().is_none());
    }

    #[test]
    fn to_number_obj() {
        let agent = test_agent();
        let ao = test_ao(&agent);
        assert!(ao.o.to_number_obj().is_none());
    }

    #[test]
    fn to_callable_obj() {
        let agent = test_agent();
        let ao = test_ao(&agent);
        assert!(ao.o.to_callable_obj().is_none());
    }

    #[test]
    fn to_constructable() {
        let agent = test_agent();
        let ao = test_ao(&agent);
        assert!(ao.o.to_constructable().is_none());
    }

    #[test]
    fn to_function_obj() {
        let agent = test_agent();
        let ao = test_ao(&agent);
        assert!(ao.o.to_function_obj().is_none());
    }

    #[test]
    fn to_builtin_function_obj() {
        let agent = test_agent();
        let ao = test_ao(&agent);
        assert!(ao.o.to_builtin_function_obj().is_none());
    }

    #[test]
    fn debug() {
        let agent = test_agent();
        let obj = test_ao(&agent);
        let ao = obj.o.to_arguments_object().unwrap();
        assert_ne!(format!("{:?}", ao), "");
    }

    #[test_case(test_ao, "0" => Ok(ECMAScriptValue::from("value of 'from'")); "index was there")]
    #[test_case(test_ao, "not" => Ok(ECMAScriptValue::Undefined); "prop wasn't there")]
    #[test_case(|a| { let ao = test_ao(a); ao.o.delete(a, &"1".into()).unwrap(); ao }, "1" => Ok(ECMAScriptValue::Undefined); "tried to get a deleted one")]
    #[test_case(|a| ArgumentsObject::object(a, None), "10" => Ok(ECMAScriptValue::Undefined); "unmapped")]
    fn get(make_object: impl FnOnce(&Agent) -> Object, propname: &str) -> Result<ECMAScriptValue, String> {
        let agent = test_agent();
        let obj = make_object(&agent);
        let receiver = ECMAScriptValue::from(obj.clone());

        obj.o.get(&agent, &propname.into(), &receiver).map_err(|err| unwind_any_error(&agent, err))
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
        make_object: impl FnOnce(&Agent) -> Object,
        propname: &str,
        val: ECMAScriptValue,
        to_check: &[&str],
    ) -> Result<(bool, Vec<(ECMAScriptValue, ECMAScriptValue)>), String> {
        let agent = test_agent();
        let obj = make_object(&agent);
        let env = agent
            .current_lexical_environment()
            .unwrap_or_else(|| agent.current_realm_record().unwrap().borrow().global_env.clone().unwrap());
        let receiver = ECMAScriptValue::from(obj.clone());

        let result = obj.o.set(&agent, propname.into(), val, &receiver).map_err(|err| unwind_any_error(&agent, err))?;

        let values = to_check
            .iter()
            .map(|&probe| {
                (
                    obj.o.get(&agent, &probe.into(), &receiver).unwrap(),
                    if env.has_binding(&agent, &probe.into()).unwrap() {
                        env.get_binding_value(&agent, &probe.into(), false).unwrap()
                    } else {
                        ECMAScriptValue::Undefined
                    },
                )
            })
            .collect::<Vec<_>>();

        Ok((result, values))
    }

    type TestResult =
        Result<(bool, AHashMap<String, (ECMAScriptValue, ECMAScriptValue)>, Option<Vec<Option<String>>>), String>;

    fn test_hm(
        input: &[(&str, ECMAScriptValue, ECMAScriptValue)],
    ) -> AHashMap<String, (ECMAScriptValue, ECMAScriptValue)> {
        input
            .iter()
            .map(|(name, obj_value, env_value)| (name.to_string(), (obj_value.clone(), env_value.clone())))
            .collect()
    }
    fn test_v(input: &[Option<&str>]) -> Vec<Option<String>> {
        input.iter().map(|maybe_name| maybe_name.as_ref().map(|&s| s.to_string())).collect()
    }

    #[test_case(test_ao, "1", &["0", "1", "2", "from", "the", "test"] => Ok((true, test_hm(&[
        ("0", ECMAScriptValue::from("value of 'from'"), ECMAScriptValue::Undefined),
        ("1", ECMAScriptValue::Undefined, ECMAScriptValue::Undefined),
        ("2", ECMAScriptValue::from("value of 'test'"), ECMAScriptValue::Undefined),
        ("from", ECMAScriptValue::Undefined, ECMAScriptValue::from("value of 'from'")),
        ("the", ECMAScriptValue::Undefined, ECMAScriptValue::from("value of 'the'")),
        ("test", ECMAScriptValue::Undefined, ECMAScriptValue::from("value of 'test'")),
    ]), Some(test_v(&[Some("from"), None, Some("test")])))); "typical")]
    #[test_case(test_ao, "10", &["0", "1", "2", "from", "the", "test"] => Ok((true, test_hm(&[
        ("0", ECMAScriptValue::from("value of 'from'"), ECMAScriptValue::Undefined),
        ("1", ECMAScriptValue::from("value of 'the'"), ECMAScriptValue::Undefined),
        ("2", ECMAScriptValue::from("value of 'test'"), ECMAScriptValue::Undefined),
        ("from", ECMAScriptValue::Undefined, ECMAScriptValue::from("value of 'from'")),
        ("the", ECMAScriptValue::Undefined, ECMAScriptValue::from("value of 'the'")),
        ("test", ECMAScriptValue::Undefined, ECMAScriptValue::from("value of 'test'")),
    ]), Some(test_v(&[Some("from"), Some("the"), Some("test")])))); "delete a non-existing prop")]
    #[test_case(test_unmapped, "1", &["0", "1", "2"] => Ok((true, test_hm(&[
        ("0", ECMAScriptValue::from("value of 'from'"), ECMAScriptValue::Undefined),
        ("1", ECMAScriptValue::Undefined, ECMAScriptValue::Undefined),
        ("2", ECMAScriptValue::from("value of 'test'"), ECMAScriptValue::Undefined),
    ]), None)); "unmapped")]
    #[test_case(|a| {
        let obj = ArgumentsObject::object(a, None);
        define_property_or_throw(a, &obj, "key", PotentialPropertyDescriptor::new().value(39).configurable(false)).unwrap();
        obj
    }, "key", &["key"] => Ok((false, test_hm(&[("key", ECMAScriptValue::from(39), ECMAScriptValue::Undefined)]), None)); "undeletable")]
    fn delete(make_object: impl FnOnce(&Agent) -> Object, name: &str, to_check: &[&str]) -> TestResult {
        let agent = test_agent();
        let obj = make_object(&agent);
        let env = agent
            .current_lexical_environment()
            .unwrap_or_else(|| agent.current_realm_record().unwrap().borrow().global_env.clone().unwrap());
        let receiver = ECMAScriptValue::from(obj.clone());

        let result = obj.o.delete(&agent, &name.into()).map_err(|err| unwind_any_error(&agent, err))?;

        let values = to_check
            .iter()
            .map(|&probe| {
                (
                    probe.to_string(),
                    (
                        obj.o.get(&agent, &probe.into(), &receiver).unwrap(),
                        if env.has_binding(&agent, &probe.into()).unwrap() {
                            env.get_binding_value(&agent, &probe.into(), false).unwrap()
                        } else {
                            ECMAScriptValue::Undefined
                        },
                    ),
                )
            })
            .collect::<AHashMap<_, _>>();

        let ao = obj.o.to_arguments_object().unwrap();
        let map_result = ao.parameter_map.as_ref().map(|pmap| {
            pmap.borrow().properties.iter().cloned().map(|maybe_name| maybe_name.map(String::from)).collect::<Vec<_>>()
        });

        Ok((result, values, map_result))
    }

    fn prop_checker(expected: PotentialPropertyDescriptor) -> impl Fn(Option<PropertyDescriptor>) {
        move |actual: Option<PropertyDescriptor>| match &actual {
            None => panic!("{:?} should not have been None", actual),
            Some(pd) => {
                if let Some(enumerable) = expected.enumerable {
                    assert_eq!(pd.enumerable, enumerable);
                }
                if let Some(configurable) = expected.configurable {
                    assert_eq!(pd.configurable, configurable);
                }
                if let Some(expected_writable) = expected.writable {
                    assert!(
                        matches!(pd.property, PropertyKind::Data(DataProperty { value: _, writable }) if writable == expected_writable)
                    );
                }
                if let Some(expected_value) = &expected.value {
                    assert!(
                        matches!(&pd.property, PropertyKind::Data(DataProperty { value, writable: _ }) if value == expected_value)
                    );
                }
                if let Some(expected_get) = &expected.get {
                    assert!(
                        matches!(&pd.property, PropertyKind::Accessor(AccessorProperty{ get, set: _ }) if get == expected_get)
                    );
                }
                if let Some(expected_set) = &expected.set {
                    assert!(
                        matches!(&pd.property, PropertyKind::Accessor(AccessorProperty{ get: _, set }) if set == expected_set)
                    );
                }
            }
        }
    }

    #[test_case(test_ao, "0" => using prop_checker(PotentialPropertyDescriptor::new().value("value of 'from'").writable(true).enumerable(true).configurable(true)); "mapped value")]
    #[test_case(test_ao, "100" => using prop_checker(PotentialPropertyDescriptor::new().value("not in index").writable(true).enumerable(true).configurable(true)); "unmapped value")]
    #[test_case(test_unmapped, "2" => using prop_checker(PotentialPropertyDescriptor::new().value("value of 'test'").writable(true).enumerable(true).configurable(true)); "unmapped obj")]
    #[test_case(test_unmapped, "200" => None; "property not present")]
    fn get_own_property(make_object: impl FnOnce(&Agent) -> Object, name: &str) -> Option<PropertyDescriptor> {
        let agent = test_agent();
        let obj = make_object(&agent);

        obj.o.get_own_property(&agent, &name.into()).unwrap()
    }

    type DefineOwnPropertyTestResult =
        Result<(bool, AHashMap<String, ECMAScriptValue>, AHashMap<String, ECMAScriptValue>), String>;
    fn hm(data: &[(&str, ECMAScriptValue)]) -> AHashMap<String, ECMAScriptValue> {
        data.iter().map(|(s, v)| (s.to_string(), v.clone())).collect()
    }
    #[test_case(test_ao, "10", PotentialPropertyDescriptor::new().value(101) => Ok((true, hm(&[
        ("0", "value of 'from'".into()),
        ("1", "value of 'the'".into()),
        ("2", "value of 'test'".into()),
        ("100", "not in index".into()),
        ("10", 101.into()),
    ]), hm(&[
        ("from", "value of 'from'".into()),
        ("the", "value of 'the'".into()),
        ("test", "value of 'test'".into()),
    ]))); "adding beyond")]
    #[test_case(test_ao, "0", PotentialPropertyDescriptor::new().value(101) => Ok((true, hm(&[
        ("0", 101.into()),
        ("1", "value of 'the'".into()),
        ("2", "value of 'test'".into()),
        ("100", "not in index".into()),
    ]), hm(&[
        ("from", 101.into()),
        ("the", "value of 'the'".into()),
        ("test", "value of 'test'".into()),
    ]))); "replacing existing")]
    #[test_case(test_ao, "0", PotentialPropertyDescriptor::new().writable(false) => Ok((true, hm(&[
        ("0", "value of 'from'".into()),
        ("1", "value of 'the'".into()),
        ("2", "value of 'test'".into()),
        ("100", "not in index".into()),
    ]), hm(&[
        ("from", "value of 'from'".into()),
        ("the", "value of 'the'".into()),
        ("test", "value of 'test'".into()),
    ]))); "removing writability")]
    #[test_case(test_ao, "0", PotentialPropertyDescriptor::new().enumerable(true) => Ok((true, hm(&[
        ("0", "value of 'from'".into()),
        ("1", "value of 'the'".into()),
        ("2", "value of 'test'".into()),
        ("100", "not in index".into()),
    ]), hm(&[
        ("from", "value of 'from'".into()),
        ("the", "value of 'the'".into()),
        ("test", "value of 'test'".into()),
    ]))); "adding enumerable")]
    #[test_case(|a| {
        let obj = test_ao(a);
        obj.o.define_own_property(a, "own".into(), PotentialPropertyDescriptor::new().value(0).writable(false).enumerable(true).configurable(false)).unwrap();
        obj
    }, "own", PotentialPropertyDescriptor::new().value(99).configurable(true) => Ok((false, hm(&[
        ("0", "value of 'from'".into()),
        ("1", "value of 'the'".into()),
        ("2", "value of 'test'".into()),
        ("100", "not in index".into()),
        ("own", 0.into()),
    ]), hm(&[
        ("from", "value of 'from'".into()),
        ("the", "value of 'the'".into()),
        ("test", "value of 'test'".into()),
    ]))); "failed define")]
    #[test_case(|a| {
        let obj = test_unmapped(a);
        let env = a.current_realm_record().unwrap().borrow().global_env.clone().unwrap();
        let lexenv = Rc::new(DeclarativeEnvironmentRecord::new(Some(env), "test_unmapped"));
        a.set_lexical_environment(Some(lexenv as Rc<dyn EnvironmentRecord>));

        obj
    }, "0", PotentialPropertyDescriptor::new().value(22) => Ok((true, hm(&[
        ("0", 22.into()),
        ("1", "value of 'the'".into()),
        ("2", "value of 'test'".into()),
    ]), hm(&[]))); "unmapped")]
    fn define_own_property(
        make_object: impl FnOnce(&Agent) -> Object,
        name: &str,
        desc: PotentialPropertyDescriptor,
    ) -> DefineOwnPropertyTestResult {
        let agent = test_agent();
        let obj = make_object(&agent);
        let env = agent.current_lexical_environment().unwrap();

        let result = obj.o.define_own_property(&agent, name.into(), desc).map_err(|e| unwind_any_error(&agent, e))?;

        let object_keys = obj.o.own_property_keys(&agent).unwrap();
        let items = object_keys
            .iter()
            .map(|key| (key.to_string(), super::get(&agent, &obj, key).unwrap()))
            .collect::<AHashMap<_, _>>();

        let env_items = env
            .binding_names()
            .iter()
            .map(|key| (key.to_string(), env.get_binding_value(&agent, key, false).unwrap()))
            .collect::<AHashMap<_, _>>();

        Ok((result, items, env_items))
    }

    #[test_case(test_ao, "0" => true; "exists")]
    #[test_case(test_ao, "from" => false; "not in ao")]
    fn has_property(make_object: impl FnOnce(&Agent) -> Object, name: &str) -> bool {
        let agent = test_agent();
        let obj = make_object(&agent);

        obj.o.has_property(&agent, &name.into()).unwrap()
    }

    #[test_case(test_ao => true; "typical")]
    fn set_prototype_of(make_object: impl FnOnce(&Agent) -> Object) -> bool {
        let agent = test_agent();
        let obj = make_object(&agent);

        obj.o.set_prototype_of(&agent, None).unwrap()
    }

    #[test_case(test_ao => true; "typical")]
    fn prevent_extensions(make_object: impl FnOnce(&Agent) -> Object) -> bool {
        let agent = test_agent();
        let obj = make_object(&agent);

        obj.o.prevent_extensions(&agent).unwrap()
    }
}
