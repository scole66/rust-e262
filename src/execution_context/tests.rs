use super::*;
use crate::parser::testhelp::*;
use crate::tests::*;
use test_case::test_case;

impl ScriptRecord {
    pub fn new_empty(realm: Rc<RefCell<Realm>>) -> Self {
        let script = Maker::new("").script();
        ScriptRecord { realm, ecmascript_code: script, compiled: Rc::new(Chunk::new("empty")), text: String::new() }
    }
}

mod script_record {
    use super::*;

    #[test]
    fn debug() {
        let agent = test_agent();
        let sr = ScriptRecord::new_empty(agent.current_realm_record().unwrap());
        assert_ne!(format!("{:?}", sr), "");
    }

    #[test]
    fn clone() {
        let agent = test_agent();
        let sr = ScriptRecord::new_empty(agent.current_realm_record().unwrap());
        let sr2 = sr.clone();
        assert!(Rc::ptr_eq(&sr.realm, &sr2.realm));
        assert!(Rc::ptr_eq(&sr.ecmascript_code, &sr2.ecmascript_code));
        assert!(Rc::ptr_eq(&sr.compiled, &sr2.compiled));
        assert_eq!(&sr.text, &sr2.text);
    }
}

mod module_record {
    use super::*;

    #[test]
    fn debug() {
        let mr = ModuleRecord {};
        assert_ne!(format!("{:?}", mr), "");
    }
}

mod script_or_module {
    use super::*;
    use test_case::test_case;

    #[test]
    fn debug() {
        let som = ScriptOrModule::Module(Rc::new(ModuleRecord {}));
        assert_ne!(format!("{:?}", som), "");
    }

    #[test]
    fn clone() {
        let agent = test_agent();
        let s1 = ScriptOrModule::Script(Rc::new(ScriptRecord::new_empty(agent.current_realm_record().unwrap())));
        let s2 = s1.clone();

        if let (ScriptOrModule::Script(original), ScriptOrModule::Script(duplicate)) = (s1, s2) {
            assert!(Rc::ptr_eq(&original, &duplicate));
        } else {
            panic!("Clone failed; differing enum types");
        }
    }

    #[test_case(|agent| {
        let mut sr = ScriptRecord::new_empty(agent.current_realm_record().unwrap());
        sr.text = String::from("I am a walrus");
        ScriptOrModule::Script(Rc::new(sr))
    } => "I am a walrus"; "Script")]
    #[test_case(|_| ScriptOrModule::Module(Rc::new(ModuleRecord{})) => panics "not yet implemented"; "module")]
    fn source_text(maker: impl FnOnce(&Agent) -> ScriptOrModule) -> String {
        let agent = test_agent();
        let som = maker(&agent);
        let text = som.source_text();
        text.clone()
    }
}

mod execution_context {
    use super::*;
    use test_case::test_case;

    #[test]
    fn debug() {
        let agent = test_agent();
        let ec = ExecutionContext::new(None, agent.current_realm_record().unwrap(), None);

        assert_ne!(format!("{:?}", ec), "");
    }

    #[test_case(|_| None; "SOM is None")]
    #[test_case(|_| Some(ScriptOrModule::Module(Rc::new(ModuleRecord{}))) => panics "not yet implemented"; "SOM is module")]
    #[test_case(|a| Some(ScriptOrModule::Script(Rc::new(ScriptRecord::new_empty(a.current_realm_record().unwrap())))); "SOM is script")]
    fn new(maker: fn(&Agent) -> Option<ScriptOrModule>) {
        let agent = test_agent();
        let som = maker(&agent);
        let original_was_none = som.is_none();
        let func = Some(ordinary_object_create(&agent, None, &[]));
        let ec = ExecutionContext::new(func.clone(), agent.current_realm_record().unwrap(), som);
        assert_eq!(&func, &ec.function);
        assert!(Rc::ptr_eq(&ec.realm, &agent.current_realm_record().unwrap()));
        assert!(ec.lexical_environment.is_none());
        assert!(ec.variable_environment.is_none());
        assert!(ec.private_environment.is_none());
        assert!(ec.stack.is_empty());
        assert_eq!(ec.pc, 0);
        assert_eq!(ec.script_or_module.is_none(), original_was_none);
        assert_eq!(&ec.function, &func);
    }
}

#[test_case(|| Agent::new(Rc::new(RefCell::new(SymbolRegistry::new()))) => None; "empty agent")]
#[test_case(test_agent => Some("present".to_string()); "has global")]
fn get_global_object(maker: fn() -> Agent) -> Option<String> {
    let agent = maker();
    let maybe_obj = super::get_global_object(&agent);

    maybe_obj.map(|obj| {
        let val = get(&agent, &obj, &"debug_token".into()).unwrap_or_else(|_| "missing".into());
        to_string(&agent, val).unwrap().to_string()
    })
}

mod agent {
    use super::*;
    use test_case::test_case;

    mod get_this_environment {
        use super::*;

        #[test]
        fn global() {
            // Where the "this object" is the global object.

            let agent = test_agent();
            // Need to establish a lexical environment first.
            let realm = agent.current_realm_record().unwrap();
            let global_env = realm.borrow().global_env.clone();
            let mut script_context = ExecutionContext::new(None, Rc::clone(&realm), None);
            script_context.lexical_environment = global_env.clone().map(|g| g as Rc<dyn EnvironmentRecord>);
            script_context.variable_environment = global_env.map(|g| g as Rc<dyn EnvironmentRecord>);
            agent.push_execution_context(script_context);

            let env = agent.get_this_environment();
            let val = env.get_binding_value(&agent, &"debug_token".into(), false).unwrap();
            let repr = to_string(&agent, val).unwrap();

            assert_eq!(repr.to_string(), "present");
        }

        #[test]
        fn child() {
            // Where the we start from a child node without a this binding and work our way up.
            let agent = test_agent();
            // Need to establish a lexical environment first.
            let realm = agent.current_realm_record().unwrap();
            let global_env = realm.borrow().global_env.clone().unwrap() as Rc<dyn EnvironmentRecord>;
            let mut script_context = ExecutionContext::new(None, Rc::clone(&realm), None);
            script_context.lexical_environment = Some(Rc::new(DeclarativeEnvironmentRecord::new(
                Some(global_env),
                "child",
            )) as Rc<dyn EnvironmentRecord>);
            agent.push_execution_context(script_context);

            let env = agent.get_this_environment();
            let val = env.get_binding_value(&agent, &"debug_token".into(), false).unwrap();
            let repr = to_string(&agent, val).unwrap();

            assert_eq!(repr.to_string(), "present");
        }
    }

    #[test]
    fn resolve_this_binding() {
        let agent = test_agent();
        // Need to establish a lexical environment first.
        let realm = agent.current_realm_record().unwrap();
        let global_env = realm.borrow().global_env.clone().unwrap() as Rc<dyn EnvironmentRecord>;
        let mut script_context = ExecutionContext::new(None, Rc::clone(&realm), None);
        script_context.lexical_environment =
            Some(Rc::new(DeclarativeEnvironmentRecord::new(Some(global_env), "rtb")) as Rc<dyn EnvironmentRecord>);
        agent.push_execution_context(script_context);

        let this_binding_value = agent.resolve_this_binding().unwrap();
        let this_binding = to_object(&agent, this_binding_value).unwrap();
        let probe = get(&agent, &this_binding, &"debug_token".into()).unwrap();
        let repr = to_string(&agent, probe).unwrap();
        assert_eq!(repr.to_string(), "present");
    }

    #[test_case("debug_token", |_| None, false => Ok(("Environment(GlobalEnvironmentRecord(realm-global))".to_string(), ReferencedName::from("debug_token"), false, None)) ; "global ref")]
    #[test_case("notPresent", |_| None, true => Ok(("Unresolvable".to_string(), ReferencedName::from("notPresent"), true, None)); "unresolvable")]
    #[test_case("marker", |a| {
        let outer = a.current_lexical_environment().unwrap();
        let env = DeclarativeEnvironmentRecord::new(Some(outer), "test-decl-env");
        env.create_mutable_binding(a, "marker".into(), false).unwrap();
        env.initialize_binding(a, &"marker".into(), ECMAScriptValue::from(100)).unwrap();
        Some(Rc::new(env) as Rc<dyn EnvironmentRecord>)
    }, false => Ok(("Environment(DeclarativeEnvironmentRecord(test-decl-env))".to_string(), ReferencedName::from("marker"), false, None)); "Other env")]
    fn resolve_binding(
        name: &str,
        env_maker: fn(&Agent) -> Option<Rc<dyn EnvironmentRecord>>,
        strict: bool,
    ) -> Result<(String, ReferencedName, bool, Option<ECMAScriptValue>), String> {
        let agent = test_agent();
        // Need to establish a lexical environment first.
        let realm = agent.current_realm_record().unwrap();
        let global_env = realm.borrow().global_env.clone().unwrap() as Rc<dyn EnvironmentRecord>;
        let mut script_context = ExecutionContext::new(None, Rc::clone(&realm), None);
        script_context.lexical_environment =
            Some(Rc::new(DeclarativeEnvironmentRecord::new(Some(global_env), "script")) as Rc<dyn EnvironmentRecord>);
        agent.push_execution_context(script_context);

        let env = env_maker(&agent);
        let result = agent.resolve_binding(&name.into(), env, strict);

        result.map_err(|err| unwind_any_error(&agent, err)).and_then(|nc| match nc {
            NormalCompletion::Empty | NormalCompletion::Value(_) | NormalCompletion::Environment(_) => {
                Err("improper completion".to_string())
            }
            NormalCompletion::Reference(r) => Ok((format!("{:?}", r.base), r.referenced_name, r.strict, r.this_value)),
        })
    }
}
