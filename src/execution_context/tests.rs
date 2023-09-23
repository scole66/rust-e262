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
        setup_test_agent();
        let sr = ScriptRecord::new_empty(current_realm_record().unwrap());
        assert_ne!(format!("{:?}", sr), "");
    }

    #[test]
    #[allow(clippy::redundant_clone)]
    fn clone() {
        setup_test_agent();
        let sr = ScriptRecord::new_empty(current_realm_record().unwrap());
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
        setup_test_agent();
        let s1 = ScriptOrModule::Script(Rc::new(ScriptRecord::new_empty(current_realm_record().unwrap())));
        let s2 = s1.clone();

        if let (ScriptOrModule::Script(original), ScriptOrModule::Script(duplicate)) = (s1, s2) {
            assert!(Rc::ptr_eq(&original, &duplicate));
        } else {
            panic!("Clone failed; differing enum types");
        }
    }

    #[test_case(|| {
        let mut sr = ScriptRecord::new_empty(current_realm_record().unwrap());
        sr.text = String::from("I am a walrus");
        ScriptOrModule::Script(Rc::new(sr))
    } => "I am a walrus"; "Script")]
    #[test_case(|| ScriptOrModule::Module(Rc::new(ModuleRecord{})) => panics "not yet implemented"; "module")]
    fn source_text(maker: impl FnOnce() -> ScriptOrModule) -> String {
        setup_test_agent();
        let som = maker();
        let text = som.source_text();
        text.clone()
    }
}

mod execution_context {
    use super::*;
    use test_case::test_case;

    #[test]
    fn debug() {
        setup_test_agent();
        let ec = ExecutionContext::new(None, current_realm_record().unwrap(), None);

        assert_ne!(format!("{:?}", ec), "");
    }

    #[test_case(|| None; "SOM is None")]
    #[test_case(|| Some(ScriptOrModule::Module(Rc::new(ModuleRecord{}))) => panics "not yet implemented"; "SOM is module")]
    #[test_case(|| Some(ScriptOrModule::Script(Rc::new(ScriptRecord::new_empty(current_realm_record().unwrap())))); "SOM is script")]
    fn new(maker: fn() -> Option<ScriptOrModule>) {
        setup_test_agent();
        let som = maker();
        let original_was_none = som.is_none();
        let func = Some(ordinary_object_create(None, &[]));
        let ec = ExecutionContext::new(func.clone(), current_realm_record().unwrap(), som);
        assert_eq!(&func, &ec.function);
        assert!(Rc::ptr_eq(&ec.realm, &current_realm_record().unwrap()));
        assert!(ec.lexical_environment.is_none());
        assert!(ec.variable_environment.is_none());
        assert!(ec.private_environment.is_none());
        assert!(ec.stack.is_empty());
        assert_eq!(ec.pc, 0);
        assert_eq!(ec.script_or_module.is_none(), original_was_none);
        assert_eq!(&ec.function, &func);
        assert!(ec.generator.is_none());
        assert!(ec.gen_closure.is_none());
    }
}

#[test_case(true => None; "empty agent")]
#[test_case(false => Some("present".to_string()); "has global")]
fn get_global_object(reset: bool) -> Option<String> {
    setup_test_agent();
    if reset {
        AGENT.with(|agent| agent.reset())
    }
    let maybe_obj = super::get_global_object();

    maybe_obj.map(|obj| {
        let val = get(&obj, &"debug_token".into()).unwrap_or_else(|_| "missing".into());
        to_string(val).unwrap().to_string()
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

            setup_test_agent();
            // Need to establish a lexical environment first.
            let realm = current_realm_record().unwrap();
            let global_env = realm.borrow().global_env.clone();
            let mut script_context = ExecutionContext::new(None, Rc::clone(&realm), None);
            script_context.lexical_environment = global_env.clone().map(|g| g as Rc<dyn EnvironmentRecord>);
            script_context.variable_environment = global_env.map(|g| g as Rc<dyn EnvironmentRecord>);
            push_execution_context(script_context);

            let env = get_this_environment();
            let val = env.get_binding_value(&"debug_token".into(), false).unwrap();
            let repr = to_string(val).unwrap();

            assert_eq!(repr.to_string(), "present");
        }

        #[test]
        fn child() {
            // Where the we start from a child node without a this binding and work our way up.
            setup_test_agent();
            // Need to establish a lexical environment first.
            let realm = current_realm_record().unwrap();
            let global_env = realm.borrow().global_env.clone().unwrap() as Rc<dyn EnvironmentRecord>;
            let mut script_context = ExecutionContext::new(None, Rc::clone(&realm), None);
            script_context.lexical_environment = Some(Rc::new(DeclarativeEnvironmentRecord::new(
                Some(global_env),
                "child",
            )) as Rc<dyn EnvironmentRecord>);
            push_execution_context(script_context);

            let env = get_this_environment();
            let val = env.get_binding_value(&"debug_token".into(), false).unwrap();
            let repr = to_string(val).unwrap();

            assert_eq!(repr.to_string(), "present");
        }
    }

    #[test]
    fn resolve_this_binding() {
        setup_test_agent();
        // Need to establish a lexical environment first.
        let realm = current_realm_record().unwrap();
        let global_env = realm.borrow().global_env.clone().unwrap() as Rc<dyn EnvironmentRecord>;
        let mut script_context = ExecutionContext::new(None, Rc::clone(&realm), None);
        script_context.lexical_environment =
            Some(Rc::new(DeclarativeEnvironmentRecord::new(Some(global_env), "rtb")) as Rc<dyn EnvironmentRecord>);
        push_execution_context(script_context);

        let this_binding_value = super::resolve_this_binding().unwrap();
        let this_binding = to_object(this_binding_value).unwrap();
        let probe = get(&this_binding, &"debug_token".into()).unwrap();
        let repr = to_string(probe).unwrap();
        assert_eq!(repr.to_string(), "present");
    }

    #[test_case("debug_token", || None, false => Ok(("Environment(GlobalEnvironmentRecord(realm-global))".to_string(), ReferencedName::from("debug_token"), false, None)) ; "global ref")]
    #[test_case("notPresent", || None, true => Ok(("Unresolvable".to_string(), ReferencedName::from("notPresent"), true, None)); "unresolvable")]
    #[test_case("marker", || {
        let outer = current_lexical_environment().unwrap();
        let env = DeclarativeEnvironmentRecord::new(Some(outer), "test-decl-env");
        env.create_mutable_binding("marker".into(), false).unwrap();
        env.initialize_binding(&"marker".into(), ECMAScriptValue::from(100)).unwrap();
        Some(Rc::new(env) as Rc<dyn EnvironmentRecord>)
    }, false => Ok(("Environment(DeclarativeEnvironmentRecord(test-decl-env))".to_string(), ReferencedName::from("marker"), false, None)); "Other env")]
    fn resolve_binding(
        name: &str,
        env_maker: fn() -> Option<Rc<dyn EnvironmentRecord>>,
        strict: bool,
    ) -> Result<(String, ReferencedName, bool, Option<ECMAScriptValue>), String> {
        setup_test_agent();
        // Need to establish a lexical environment first.
        let realm = current_realm_record().unwrap();
        let global_env = realm.borrow().global_env.clone().unwrap() as Rc<dyn EnvironmentRecord>;
        let mut script_context = ExecutionContext::new(None, Rc::clone(&realm), None);
        script_context.lexical_environment =
            Some(Rc::new(DeclarativeEnvironmentRecord::new(Some(global_env), "script")) as Rc<dyn EnvironmentRecord>);
        push_execution_context(script_context);

        let env = env_maker();
        let result = super::resolve_binding(&name.into(), env, strict);

        result.map_err(unwind_any_error).and_then(|nc| match nc {
            NormalCompletion::IteratorRecord(_)
            | NormalCompletion::Empty
            | NormalCompletion::Value(_)
            | NormalCompletion::Environment(_)
            | NormalCompletion::PrivateName(_) => Err("improper completion".to_string()),
            NormalCompletion::Reference(r) => Ok((format!("{:?}", r.base), r.referenced_name, r.strict, r.this_value)),
        })
    }
}
