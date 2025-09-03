use super::*;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

#[derive(Clone)]
pub(crate) struct ScriptRecord {
    pub(crate) realm: Rc<RefCell<Realm>>,
    pub(crate) ecmascript_code: Rc<Script>,
    pub(crate) text: String,
    pub(crate) compiled: Rc<Chunk>,
}
struct ConciseScript<'a>(&'a Script);
impl fmt::Debug for ConciseScript<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

impl fmt::Debug for ScriptRecord {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ScriptRecord")
            .field("realm", &self.realm)
            .field("ecmascript_code", &ConciseScript(&self.ecmascript_code))
            .field("text", &self.text)
            .field("compiled", &ConciseChunk(&self.compiled))
            .finish()
    }
}

#[derive(Debug)]
pub(crate) struct ModuleRecord {}

#[cfg(not(test))]
#[derive(Debug, Clone)]
pub(crate) enum ScriptOrModule {
    Script(Rc<ScriptRecord>),
    #[expect(dead_code)]
    Module(Rc<ModuleRecord>),
}
#[cfg(test)]
#[derive(Debug, Clone)]
pub(crate) enum ScriptOrModule {
    Script(Rc<ScriptRecord>),
    Module(Rc<ModuleRecord>),
}

impl PartialEq for ScriptOrModule {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Script(l0), Self::Script(r0)) => Rc::ptr_eq(l0, r0),
            (Self::Module(l0), Self::Module(r0)) => Rc::ptr_eq(l0, r0),
            _ => false,
        }
    }
}

impl ScriptOrModule {
    #[cfg(test)]
    pub(crate) fn source_text(&self) -> &String {
        match self {
            ScriptOrModule::Script(som) => &som.text,
            ScriptOrModule::Module(_) => todo!(),
        }
    }
    pub(crate) fn source_tree(&self) -> SourceTree {
        match self {
            ScriptOrModule::Script(som) => {
                SourceTree { text: som.text.clone(), ast: ParsedText::Script(som.ecmascript_code.clone()) }
            }
            ScriptOrModule::Module(_) => todo!(),
        }
    }
}

pub(crate) struct ExecutionContext {
    pub(crate) realm: Rc<RefCell<Realm>>,
    pub(crate) function: Option<Object>,
    pub(crate) script_or_module: Option<ScriptOrModule>,

    // for code contexts
    pub(crate) lexical_environment: Option<Rc<dyn EnvironmentRecord>>,
    pub(crate) variable_environment: Option<Rc<dyn EnvironmentRecord>>,
    pub(crate) private_environment: Option<Rc<RefCell<PrivateEnvironmentRecord>>>,

    // code evaluation state
    pub(crate) stack: Vec<FullCompletion>,
    pub(crate) chunk: Option<Rc<Chunk>>, // This might change. It might be easier to have an empty chunk than a None.
    pub(crate) pc: usize,

    pub(crate) generator: Option<Object>,
    pub(crate) gen_closure: Option<ECMAClosure>,
}
impl std::fmt::Debug for ExecutionContext {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("ExecutionContext")
            .field("realm", &self.realm)
            .field("function", &self.function)
            .field("script_or_module", &self.script_or_module)
            .field("lexical_environment", &self.lexical_environment)
            .field("variable_environment", &self.variable_environment)
            .field("private_environment", &self.private_environment)
            .field("stack", &self.stack)
            .field("chunk", &self.chunk)
            .field("pc", &self.pc)
            .field("generator", &self.generator)
            .field("gen_closure", &self.gen_closure.as_ref().and(Some("something")))
            .finish()
    }
}

impl ExecutionContext {
    #[expect(unused_variables)]
    pub(crate) fn new(
        function: Option<Object>,
        realm: Rc<RefCell<Realm>>,
        script_or_module: Option<ScriptOrModule>,
    ) -> Self {
        let chunk = match &script_or_module {
            None => None,
            Some(ScriptOrModule::Script(sr)) => Some(Rc::clone(&sr.compiled)),
            Some(ScriptOrModule::Module(mr)) => todo!(),
        };
        ExecutionContext {
            realm,
            function,
            script_or_module,
            stack: vec![],
            chunk,
            pc: 0,
            lexical_environment: None,
            variable_environment: None,
            private_environment: None,
            generator: None,
            gen_closure: None,
        }
    }
}

// GetGlobalObject ( )
//
// The abstract operation GetGlobalObject takes no arguments. It returns the global object used by the currently
// running execution context. It performs the following steps when called:
//
//  1. Let currentRealm be the current Realm Record.
//  2. Return currentRealm.[[GlobalObject]].
pub(crate) fn get_global_object() -> Option<Object> {
    current_realm_record()?.borrow().global_object.clone()
}

/// Finds the Environment Record that currently supplies the binding of the keyword this.
///
/// See [GetThisEnvironment](https://tc39.es/ecma262/#sec-getthisenvironment) in ECMA-262.
pub(crate) fn get_this_environment() -> Rc<dyn EnvironmentRecord> {
    // The abstract operation GetThisEnvironment takes no arguments and returns an Environment Record. It finds the
    // Environment Record that currently supplies the binding of the keyword this. It performs the following steps when
    // called:
    //
    //  1. Let env be the running execution context's LexicalEnvironment.
    //  2. Repeat,
    //      a. Let exists be env.HasThisBinding().
    //      b. If exists is true, return env.
    //      c. Let outer be env.[[OuterEnv]].
    //      d. Assert: outer is not null.
    //      e. Set env to outer.
    // NOTE |   The loop in step 2 will always terminate because the list of environments always ends with the global
    //      |   environment which has a this binding.
    let mut env = current_lexical_environment().unwrap();
    loop {
        let exists = env.has_this_binding();
        if exists {
            return env;
        }
        let outer = env.get_outer_env().unwrap().clone();
        env = outer;
    }
}

/// Determine the binding of the "this" keyword (and return it)
///
/// See [ResolveThisBinding](https://tc39.es/ecma262/#sec-resolvethisbinding) in ECMA-262.
pub(crate) fn resolve_this_binding() -> Completion<ECMAScriptValue> {
    // ResolveThisBinding ( )
    //
    // The abstract operation ResolveThisBinding takes no arguments and returns either a normal completion containing an
    // ECMAScript language value or an abrupt completion. It determines the binding of the keyword this using the
    // LexicalEnvironment of the running execution context. It performs the following steps when called:
    //
    // 1. Let envRec be GetThisEnvironment().
    // 2. Return ? envRec.GetThisBinding().
    let env_rec = get_this_environment();
    env_rec.get_this_binding()
}

/// Constructs a Reference for the given name (and, potentially, environment)
///
/// See [ResolveBinding](https://tc39.es/ecma262/#sec-resolvebinding) in ECMA-262.
pub(crate) fn resolve_binding(name: &JSString, env: Option<Rc<dyn EnvironmentRecord>>, strict: bool) -> FullCompletion {
    // ResolveBinding ( name [ , env ] )
    // The abstract operation ResolveBinding takes argument name (a String) and optional argument env (an
    // Environment Record or undefined) and returns either a normal completion containing a Reference Record or an
    // abrupt completion. It is used to determine the binding of name. env can be used to explicitly provide the
    // Environment Record that is to be searched for the binding. It performs the following steps when called:
    //
    //  1. If env is not present or if env is undefined, then
    //      a. Set env to the running execution context's LexicalEnvironment.
    //  2. Assert: env is an Environment Record.
    //  3. If the source text matched by the syntactic production that is being evaluated is contained in strict
    //     mode code, let strict be true; else let strict be false.
    //  4. Return ? GetIdentifierReference(env, name, strict).
    // NOTE |   The result of ResolveBinding is always a Reference Record whose [[ReferencedName]] field is name.
    let env = match env {
        Some(e) => Some(e),
        None => current_lexical_environment(),
    };
    get_identifier_reference(env, name.clone(), strict).map(NormalCompletion::from)
}

#[cfg(test)]
mod tests;
