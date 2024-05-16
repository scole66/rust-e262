use super::*;
use anyhow::{anyhow, bail};
use itertools::Itertools;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ConstructorKind {
    Base,
    Derived,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ThisMode {
    Lexical,
    Strict,
    Global,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ThisLexicality {
    LexicalThis,
    NonLexicalThis,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ClassName {
    String(JSString),
    Symbol(Symbol),
    Private(PrivateName),
    Empty,
}

impl<T> From<T> for ClassName
where
    T: Into<JSString>,
{
    fn from(value: T) -> Self {
        Self::String(value.into())
    }
}

impl TryFrom<NormalCompletion> for ClassName {
    type Error = anyhow::Error;

    fn try_from(value: NormalCompletion) -> Result<Self, Self::Error> {
        match value {
            NormalCompletion::Empty => Ok(Self::Empty),
            NormalCompletion::PrivateName(pn) => Ok(Self::Private(pn)),
            NormalCompletion::Value(v) => match v {
                ECMAScriptValue::String(s) => Ok(Self::String(s)),
                ECMAScriptValue::Symbol(sym) => Ok(Self::Symbol(sym)),
                ECMAScriptValue::Undefined
                | ECMAScriptValue::Null
                | ECMAScriptValue::Boolean(_)
                | ECMAScriptValue::Number(_)
                | ECMAScriptValue::BigInt(_)
                | ECMAScriptValue::Object(_) => Err(anyhow::anyhow!("Not a class name")),
            },
            NormalCompletion::Reference(_)
            | NormalCompletion::Environment(_)
            | NormalCompletion::IteratorRecord(_)
            | NormalCompletion::PrivateElement(_) => Err(anyhow::anyhow!("Not a class name")),
        }
    }
}

impl From<ClassName> for NormalCompletion {
    fn from(value: ClassName) -> Self {
        match value {
            ClassName::String(s) => s.into(),
            ClassName::Symbol(sym) => sym.into(),
            ClassName::Private(pn) => pn.into(),
            ClassName::Empty => NormalCompletion::Empty,
        }
    }
}

#[derive(Debug)]
pub struct ClassFieldDefinitionRecord {}

#[derive(Debug, Clone)]
pub enum BodySource {
    Function(Rc<FunctionBody>),
    Generator(Rc<GeneratorBody>),
    AsyncFunction(Rc<AsyncFunctionBody>),
    AsyncGenerator(Rc<AsyncGeneratorBody>),
    ConciseBody(Rc<ConciseBody>),
    AsyncConciseBody(Rc<AsyncConciseBody>),
    Initializer(Rc<Initializer>),
    ClassStaticBlockBody(Rc<ClassStaticBlockBody>),
}
pub struct ConciseBodySource<'a>(&'a BodySource);
impl<'a> fmt::Debug for ConciseBodySource<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

impl From<Rc<FunctionBody>> for BodySource {
    fn from(src: Rc<FunctionBody>) -> Self {
        Self::Function(src)
    }
}
impl From<Rc<AsyncFunctionBody>> for BodySource {
    fn from(src: Rc<AsyncFunctionBody>) -> Self {
        Self::AsyncFunction(src)
    }
}
impl From<Rc<ConciseBody>> for BodySource {
    fn from(src: Rc<ConciseBody>) -> Self {
        Self::ConciseBody(src)
    }
}
impl From<Rc<AsyncConciseBody>> for BodySource {
    fn from(src: Rc<AsyncConciseBody>) -> Self {
        Self::AsyncConciseBody(src)
    }
}
impl From<Rc<GeneratorBody>> for BodySource {
    fn from(src: Rc<GeneratorBody>) -> Self {
        Self::Generator(src)
    }
}
impl From<Rc<AsyncGeneratorBody>> for BodySource {
    fn from(src: Rc<AsyncGeneratorBody>) -> Self {
        Self::AsyncGenerator(src)
    }
}
impl From<Rc<Initializer>> for BodySource {
    fn from(value: Rc<Initializer>) -> Self {
        Self::Initializer(value)
    }
}
impl From<Rc<ClassStaticBlockBody>> for BodySource {
    fn from(value: Rc<ClassStaticBlockBody>) -> Self {
        Self::ClassStaticBlockBody(value)
    }
}

impl PartialEq for BodySource {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Function(l0), Self::Function(r0)) => Rc::ptr_eq(l0, r0),
            (Self::Generator(l0), Self::Generator(r0)) => Rc::ptr_eq(l0, r0),
            (Self::AsyncFunction(l0), Self::AsyncFunction(r0)) => Rc::ptr_eq(l0, r0),
            (Self::AsyncGenerator(l0), Self::AsyncGenerator(r0)) => Rc::ptr_eq(l0, r0),
            (Self::ConciseBody(l0), Self::ConciseBody(r0)) => Rc::ptr_eq(l0, r0),
            (Self::AsyncConciseBody(l0), Self::AsyncConciseBody(r0)) => Rc::ptr_eq(l0, r0),
            (Self::Initializer(l0), Self::Initializer(r0)) => Rc::ptr_eq(l0, r0),
            (Self::ClassStaticBlockBody(l0), Self::ClassStaticBlockBody(r0)) => Rc::ptr_eq(l0, r0),
            (
                Self::Function(_)
                | Self::Generator(_)
                | Self::AsyncFunction(_)
                | Self::AsyncGenerator(_)
                | Self::ConciseBody(_)
                | Self::AsyncConciseBody(_)
                | Self::Initializer(_)
                | Self::ClassStaticBlockBody(_),
                _,
            ) => false,
        }
    }
}

impl TryFrom<BodySource> for Rc<FunctionBody> {
    type Error = anyhow::Error;
    fn try_from(value: BodySource) -> Result<Self, Self::Error> {
        match value {
            BodySource::Function(val) => Ok(val),
            _ => bail!("Not a FunctionBody"),
        }
    }
}

impl fmt::Display for BodySource {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BodySource::Function(node) => node.fmt(f),
            BodySource::Generator(node) => node.fmt(f),
            BodySource::AsyncFunction(node) => node.fmt(f),
            BodySource::AsyncGenerator(node) => node.fmt(f),
            BodySource::ConciseBody(node) => node.fmt(f),
            BodySource::AsyncConciseBody(node) => node.fmt(f),
            BodySource::Initializer(node) => node.fmt(f),
            BodySource::ClassStaticBlockBody(node) => node.fmt(f),
        }
    }
}

impl BodySource {
    /// Return a list of identifiers defined by the `var` statement for this node.
    ///
    /// Note that function bodies are treated like top-level code in that top-level function identifiers are part
    /// of the var-declared list.
    ///
    /// See [VarDeclaredNames](https://tc39.es/ecma262/#sec-static-semantics-vardeclarednames) from ECMA-262.
    pub fn var_declared_names(&self) -> Vec<JSString> {
        match self {
            BodySource::Function(f) => f.var_declared_names(),
            BodySource::Generator(g) => g.var_declared_names(),
            BodySource::AsyncFunction(af) => af.var_declared_names(),
            BodySource::AsyncGenerator(ag) => ag.var_declared_names(),
            BodySource::ConciseBody(cb) => cb.var_declared_names(),
            BodySource::AsyncConciseBody(acb) => acb.var_declared_names(),
            BodySource::Initializer(_) | BodySource::ClassStaticBlockBody(_) => Vec::new(),
        }
    }

    /// Return a list of parse nodes for the var-style declarations contained within the children of this node.
    ///
    /// See [VarScopedDeclarations](https://tc39.es/ecma262/#sec-static-semantics-varscopeddeclarations) in ECMA-262.
    pub fn var_scoped_declarations(&self) -> Vec<VarScopeDecl> {
        match self {
            BodySource::Function(f) => f.var_scoped_declarations(),
            BodySource::Generator(g) => g.var_scoped_declarations(),
            BodySource::AsyncFunction(af) => af.var_scoped_declarations(),
            BodySource::AsyncGenerator(ag) => ag.var_scoped_declarations(),
            BodySource::ConciseBody(cb) => cb.var_scoped_declarations(),
            BodySource::AsyncConciseBody(acb) => acb.var_scoped_declarations(),
            BodySource::Initializer(_) | BodySource::ClassStaticBlockBody(_) => Vec::new(),
        }
    }

    pub fn lexically_declared_names(&self) -> Vec<JSString> {
        match self {
            BodySource::Function(f) => f.lexically_declared_names(),
            BodySource::Generator(g) => g.lexically_declared_names(),
            BodySource::AsyncFunction(af) => af.lexically_declared_names(),
            BodySource::AsyncGenerator(ag) => ag.lexically_declared_names(),
            BodySource::ConciseBody(cb) => cb.lexically_declared_names(),
            BodySource::AsyncConciseBody(acb) => acb.lexically_declared_names(),
            BodySource::Initializer(_) | BodySource::ClassStaticBlockBody(_) => Vec::new(),
        }
    }

    pub fn lexically_scoped_declarations(&self) -> Vec<DeclPart> {
        match self {
            BodySource::Function(f) => f.lexically_scoped_declarations(),
            BodySource::Generator(g) => g.lexically_scoped_declarations(),
            BodySource::AsyncFunction(af) => af.lexically_scoped_declarations(),
            BodySource::AsyncGenerator(ag) => ag.lexically_scoped_declarations(),
            BodySource::ConciseBody(cb) => cb.lexically_scoped_declarations(),
            BodySource::AsyncConciseBody(acb) => acb.lexically_scoped_declarations(),
            BodySource::Initializer(_) | BodySource::ClassStaticBlockBody(_) => Vec::new(),
        }
    }

    pub fn contains_use_strict(&self) -> bool {
        match self {
            BodySource::Function(node) => node.function_body_contains_use_strict(),
            BodySource::Generator(node) => node.function_body_contains_use_strict(),
            BodySource::AsyncFunction(node) => node.function_body_contains_use_strict(),
            BodySource::AsyncGenerator(node) => node.function_body_contains_use_strict(),
            BodySource::ConciseBody(node) => node.concise_body_contains_use_strict(),
            BodySource::AsyncConciseBody(acb) => acb.contains_use_strict(),
            BodySource::Initializer(_) | BodySource::ClassStaticBlockBody(_) => false,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParamSource {
    FormalParameters(Rc<FormalParameters>),
    ArrowParameters(Rc<ArrowParameters>),
    AsyncArrowBinding(Rc<AsyncArrowBindingIdentifier>),
    ArrowFormals(Rc<ArrowFormalParameters>),
    UniqueFormalParameters(Rc<UniqueFormalParameters>),
    PropertySetParameterList(Rc<PropertySetParameterList>),
}

pub struct ConciseParamSource<'a>(&'a ParamSource);
impl<'a> fmt::Debug for ConciseParamSource<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

impl fmt::Display for ParamSource {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParamSource::FormalParameters(node) => node.fmt(f),
            ParamSource::ArrowParameters(node) => node.fmt(f),
            ParamSource::AsyncArrowBinding(node) => node.fmt(f),
            ParamSource::ArrowFormals(node) => node.fmt(f),
            ParamSource::UniqueFormalParameters(node) => node.fmt(f),
            ParamSource::PropertySetParameterList(node) => node.fmt(f),
        }
    }
}

impl PartialEq for ParamSource {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::FormalParameters(l0), Self::FormalParameters(r0)) => Rc::ptr_eq(l0, r0),
            (Self::ArrowParameters(l0), Self::ArrowParameters(r0)) => Rc::ptr_eq(l0, r0),
            (Self::AsyncArrowBinding(l0), Self::AsyncArrowBinding(r0)) => Rc::ptr_eq(l0, r0),
            (Self::ArrowFormals(l0), Self::ArrowFormals(r0)) => Rc::ptr_eq(l0, r0),
            (Self::UniqueFormalParameters(l0), Self::UniqueFormalParameters(r0)) => Rc::ptr_eq(l0, r0),
            (Self::PropertySetParameterList(l0), Self::PropertySetParameterList(r0)) => Rc::ptr_eq(l0, r0),
            (
                Self::UniqueFormalParameters(_)
                | Self::FormalParameters(_)
                | Self::ArrowParameters(_)
                | Self::AsyncArrowBinding(_)
                | Self::ArrowFormals(_)
                | Self::PropertySetParameterList(_),
                _,
            ) => false,
        }
    }
}

impl From<Rc<FormalParameters>> for ParamSource {
    fn from(src: Rc<FormalParameters>) -> Self {
        Self::FormalParameters(src)
    }
}
impl From<Rc<ArrowParameters>> for ParamSource {
    fn from(src: Rc<ArrowParameters>) -> Self {
        Self::ArrowParameters(src)
    }
}
impl From<Rc<AsyncArrowBindingIdentifier>> for ParamSource {
    fn from(src: Rc<AsyncArrowBindingIdentifier>) -> Self {
        Self::AsyncArrowBinding(src)
    }
}
impl From<Rc<ArrowFormalParameters>> for ParamSource {
    fn from(src: Rc<ArrowFormalParameters>) -> Self {
        Self::ArrowFormals(src)
    }
}
impl From<Rc<UniqueFormalParameters>> for ParamSource {
    fn from(value: Rc<UniqueFormalParameters>) -> Self {
        Self::UniqueFormalParameters(value)
    }
}
impl From<Rc<PropertySetParameterList>> for ParamSource {
    fn from(value: Rc<PropertySetParameterList>) -> Self {
        Self::PropertySetParameterList(value)
    }
}
impl TryFrom<ParamSource> for Rc<FormalParameters> {
    type Error = anyhow::Error;
    fn try_from(value: ParamSource) -> Result<Self, Self::Error> {
        match value {
            ParamSource::FormalParameters(val) => Ok(val),
            _ => bail!("Not FormalParameters"),
        }
    }
}
impl ParamSource {
    pub fn expected_argument_count(&self) -> f64 {
        match self {
            ParamSource::FormalParameters(formals) => formals.expected_argument_count(),
            ParamSource::ArrowParameters(arrow) => arrow.expected_argument_count(),
            ParamSource::AsyncArrowBinding(node) => node.expected_argument_count(),
            ParamSource::ArrowFormals(node) => node.expected_argument_count(),
            ParamSource::UniqueFormalParameters(node) => node.expected_argument_count(),
            ParamSource::PropertySetParameterList(node) => node.expected_argument_count(),
        }
    }

    pub fn bound_names(&self) -> Vec<JSString> {
        match self {
            ParamSource::FormalParameters(formals) => formals.bound_names(),
            ParamSource::ArrowParameters(arrow) => arrow.bound_names(),
            ParamSource::AsyncArrowBinding(node) => node.bound_names(),
            ParamSource::ArrowFormals(node) => node.bound_names(),
            ParamSource::UniqueFormalParameters(node) => node.bound_names(),
            ParamSource::PropertySetParameterList(node) => node.bound_names(),
        }
    }

    pub fn is_simple_parameter_list(&self) -> bool {
        match self {
            ParamSource::FormalParameters(formals) => formals.is_simple_parameter_list(),
            ParamSource::ArrowParameters(arrow) => arrow.is_simple_parameter_list(),
            ParamSource::AsyncArrowBinding(node) => node.is_simple_parameter_list(),
            ParamSource::ArrowFormals(node) => node.is_simple_parameter_list(),
            ParamSource::UniqueFormalParameters(node) => node.is_simple_parameter_list(),
            ParamSource::PropertySetParameterList(node) => node.is_simple_parameter_list(),
        }
    }

    pub fn contains_expression(&self) -> bool {
        match self {
            ParamSource::FormalParameters(formals) => formals.contains_expression(),
            ParamSource::ArrowParameters(arrow) => arrow.contains_expression(),
            ParamSource::AsyncArrowBinding(node) => node.contains_expression(),
            ParamSource::ArrowFormals(node) => node.contains_expression(),
            ParamSource::UniqueFormalParameters(node) => node.contains_expression(),
            ParamSource::PropertySetParameterList(node) => node.contains_expression(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum FunctionSource {
    FunctionExpression(Rc<FunctionExpression>),
    GeneratorExpression(Rc<GeneratorExpression>),
    AsyncGeneratorExpression(Rc<AsyncGeneratorExpression>),
    AsyncFunctionExpression(Rc<AsyncFunctionExpression>),
    ArrowFunction(Rc<ArrowFunction>),
    AsyncArrowFunction(Rc<AsyncArrowFunction>),
    MethodDefinition(Rc<MethodDefinition>),
    HoistableDeclaration(Rc<HoistableDeclaration>),
    FieldDefinition(Rc<FieldDefinition>),
    ClassStaticBlock(Rc<ClassStaticBlock>),
    FunctionDeclaration(Rc<FunctionDeclaration>),
}

impl fmt::Display for FunctionSource {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FunctionSource::FunctionExpression(node) => node.fmt(f),
            FunctionSource::GeneratorExpression(node) => node.fmt(f),
            FunctionSource::AsyncGeneratorExpression(node) => node.fmt(f),
            FunctionSource::AsyncFunctionExpression(node) => node.fmt(f),
            FunctionSource::ArrowFunction(node) => node.fmt(f),
            FunctionSource::AsyncArrowFunction(node) => node.fmt(f),
            FunctionSource::MethodDefinition(node) => node.fmt(f),
            FunctionSource::HoistableDeclaration(node) => node.fmt(f),
            FunctionSource::FieldDefinition(node) => node.fmt(f),
            FunctionSource::ClassStaticBlock(node) => node.fmt(f),
            FunctionSource::FunctionDeclaration(node) => node.fmt(f),
        }
    }
}

impl PartialEq for FunctionSource {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::FunctionExpression(l0), Self::FunctionExpression(r0)) => Rc::ptr_eq(l0, r0),
            (Self::GeneratorExpression(l0), Self::GeneratorExpression(r0)) => Rc::ptr_eq(l0, r0),
            (Self::AsyncGeneratorExpression(l0), Self::AsyncGeneratorExpression(r0)) => Rc::ptr_eq(l0, r0),
            (Self::AsyncFunctionExpression(l0), Self::AsyncFunctionExpression(r0)) => Rc::ptr_eq(l0, r0),
            (Self::ArrowFunction(l0), Self::ArrowFunction(r0)) => Rc::ptr_eq(l0, r0),
            (Self::AsyncArrowFunction(l0), Self::AsyncArrowFunction(r0)) => Rc::ptr_eq(l0, r0),
            (Self::MethodDefinition(l0), Self::MethodDefinition(r0)) => Rc::ptr_eq(l0, r0),
            (Self::HoistableDeclaration(l0), Self::HoistableDeclaration(r0)) => Rc::ptr_eq(l0, r0),
            (Self::FieldDefinition(l0), Self::FieldDefinition(r0)) => Rc::ptr_eq(l0, r0),
            (Self::ClassStaticBlock(l0), Self::ClassStaticBlock(r0)) => Rc::ptr_eq(l0, r0),
            (Self::FunctionDeclaration(l0), Self::FunctionDeclaration(r0)) => Rc::ptr_eq(l0, r0),
            _ => false,
        }
    }
}
impl From<Rc<FunctionExpression>> for FunctionSource {
    fn from(fe: Rc<FunctionExpression>) -> Self {
        Self::FunctionExpression(fe)
    }
}
impl From<Rc<ArrowFunction>> for FunctionSource {
    fn from(af: Rc<ArrowFunction>) -> Self {
        Self::ArrowFunction(af)
    }
}
impl From<Rc<FunctionDeclaration>> for FunctionSource {
    fn from(fd: Rc<FunctionDeclaration>) -> Self {
        Self::FunctionDeclaration(fd)
    }
}
impl From<Rc<FieldDefinition>> for FunctionSource {
    fn from(value: Rc<FieldDefinition>) -> Self {
        Self::FieldDefinition(value)
    }
}
impl From<Rc<ClassStaticBlock>> for FunctionSource {
    fn from(value: Rc<ClassStaticBlock>) -> Self {
        Self::ClassStaticBlock(value)
    }
}
impl From<Rc<MethodDefinition>> for FunctionSource {
    fn from(value: Rc<MethodDefinition>) -> Self {
        Self::MethodDefinition(value)
    }
}
impl TryFrom<FunctionSource> for Rc<FunctionExpression> {
    type Error = anyhow::Error;

    fn try_from(value: FunctionSource) -> Result<Self, Self::Error> {
        match value {
            FunctionSource::FunctionExpression(fe) => Ok(fe),
            _ => bail!("FunctionExpression expected"),
        }
    }
}
impl TryFrom<FunctionSource> for Rc<ArrowFunction> {
    type Error = anyhow::Error;

    fn try_from(value: FunctionSource) -> Result<Self, Self::Error> {
        match value {
            FunctionSource::ArrowFunction(af) => Ok(af),
            _ => bail!("ArrowFunction expected"),
        }
    }
}
impl TryFrom<FunctionSource> for Rc<FunctionDeclaration> {
    type Error = anyhow::Error;

    fn try_from(value: FunctionSource) -> Result<Self, Self::Error> {
        match value {
            FunctionSource::FunctionDeclaration(fd) => Ok(fd),
            _ => bail!("FunctionDeclaration expected"),
        }
    }
}
impl TryFrom<FunctionSource> for Rc<FieldDefinition> {
    type Error = anyhow::Error;

    fn try_from(value: FunctionSource) -> Result<Self, Self::Error> {
        match value {
            FunctionSource::FieldDefinition(fd) => Ok(fd),
            _ => bail!("FieldDefinition expected"),
        }
    }
}
impl TryFrom<FunctionSource> for Rc<ClassStaticBlock> {
    type Error = anyhow::Error;

    fn try_from(value: FunctionSource) -> Result<Self, Self::Error> {
        match value {
            FunctionSource::ClassStaticBlock(csb) => Ok(csb),
            _ => bail!("ClassStaticBody expected"),
        }
    }
}
impl TryFrom<FunctionSource> for Rc<MethodDefinition> {
    type Error = anyhow::Error;

    fn try_from(value: FunctionSource) -> Result<Self, Self::Error> {
        match value {
            FunctionSource::MethodDefinition(md) => Ok(md),
            _ => bail!("MethodDefinition expected"),
        }
    }
}

pub struct FunctionObjectData {
    pub environment: Rc<dyn EnvironmentRecord>,
    private_environment: Option<Rc<RefCell<PrivateEnvironmentRecord>>>,
    formal_parameters: ParamSource,
    ecmascript_code: BodySource,
    compiled: Rc<Chunk>,
    pub constructor_kind: ConstructorKind,
    pub realm: Rc<RefCell<Realm>>,
    script_or_module: Option<ScriptOrModule>,
    pub this_mode: ThisMode,
    strict: bool,
    pub home_object: Option<Object>,
    pub source_text: String,
    fields: Vec<ClassFieldDefinitionRecord>,
    private_methods: Vec<Rc<PrivateElement>>,
    pub class_field_initializer_name: ClassName,
    is_class_constructor: bool,
    is_constructor: bool,
}
impl fmt::Debug for FunctionObjectData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("FunctionObjectData")
            .field("environment", &ConciselyPrintedEnvironmentRecord(Rc::clone(&self.environment)))
            .field("private_environment", &self.private_environment)
            .field("formal_parameters", &ConciseParamSource(&self.formal_parameters))
            .field("ecmascript_code", &ConciseBodySource(&self.ecmascript_code))
            .field("compiled", &ConciseChunk(&self.compiled))
            .field("constructor_kind", &self.constructor_kind)
            .field("realm", &self.realm)
            .field("script_or_module", &self.script_or_module)
            .field("this_mode", &self.this_mode)
            .field("strict", &self.strict)
            .field("home_object", &ConciseOptionalObject::from(&self.home_object))
            .field("source_text", &self.source_text)
            .field("fields", &self.fields)
            .field("private_methods", &self.private_methods)
            .field("class_field_initializer_name", &self.class_field_initializer_name)
            .field("is_class_constructor", &self.is_class_constructor)
            .field("is_constructor", &self.is_constructor)
            .finish()
    }
}

#[derive(Debug)]
pub struct FunctionObject {
    common: RefCell<CommonObjectData>,
    function_data: RefCell<FunctionObjectData>,
}

impl<'a> From<&'a FunctionObject> for &'a dyn ObjectInterface {
    fn from(obj: &'a FunctionObject) -> Self {
        obj
    }
}

pub trait CallableObject: ObjectInterface {
    fn call(&self, self_object: &Object, this_argument: &ECMAScriptValue, arguments_list: &[ECMAScriptValue]);
    fn construct(&self, self_object: &Object, arguments_list: &[ECMAScriptValue], new_target: &Object);
    fn end_evaluation(&self, result: FullCompletion);
    fn complete_call(&self) -> Completion<ECMAScriptValue>;
}

impl ObjectInterface for FunctionObject {
    fn common_object_data(&self) -> &RefCell<CommonObjectData> {
        &self.common
    }
    fn uses_ordinary_get_prototype_of(&self) -> bool {
        true
    }
    fn id(&self) -> usize {
        self.common.borrow().objid
    }
    fn to_function_obj(&self) -> Option<&dyn FunctionInterface> {
        Some(self)
    }
    fn to_callable_obj(&self) -> Option<&dyn CallableObject> {
        // Whereas this is _anything_ that implements [[Call]]
        Some(self)
    }
    fn is_callable_obj(&self) -> bool {
        true
    }
    fn to_constructable(&self) -> Option<&dyn CallableObject> {
        let is_c = self.function_data().borrow().is_constructor;
        if is_c {
            Some(self)
        } else {
            None
        }
    }

    fn get_prototype_of(&self) -> Completion<Option<Object>> {
        Ok(ordinary_get_prototype_of(self))
    }
    fn set_prototype_of(&self, obj: Option<Object>) -> Completion<bool> {
        Ok(ordinary_set_prototype_of(self, obj))
    }
    fn is_extensible(&self) -> Completion<bool> {
        Ok(ordinary_is_extensible(self))
    }
    fn prevent_extensions(&self) -> Completion<bool> {
        Ok(ordinary_prevent_extensions(self))
    }
    fn get_own_property(&self, key: &PropertyKey) -> Completion<Option<PropertyDescriptor>> {
        Ok(ordinary_get_own_property(self, key))
    }
    fn define_own_property(&self, key: PropertyKey, desc: PotentialPropertyDescriptor) -> Completion<bool> {
        ordinary_define_own_property(self, key, desc)
    }
    fn has_property(&self, key: &PropertyKey) -> Completion<bool> {
        ordinary_has_property(self, key)
    }
    fn get(&self, key: &PropertyKey, receiver: &ECMAScriptValue) -> Completion<ECMAScriptValue> {
        ordinary_get(self, key, receiver)
    }
    fn set(&self, key: PropertyKey, v: ECMAScriptValue, receiver: &ECMAScriptValue) -> Completion<bool> {
        ordinary_set(self, key, v, receiver)
    }
    fn delete(&self, key: &PropertyKey) -> Completion<bool> {
        ordinary_delete(self, key)
    }
    fn own_property_keys(&self) -> Completion<Vec<PropertyKey>> {
        Ok(ordinary_own_property_keys(self))
    }
}

impl CallableObject for FunctionObject {
    /// "Call" the function object with the passed parameters.
    ///
    /// This is really the implementation of the CALL instruction for function objects.
    ///
    /// Essentially what we're doing here is setting up a child execution context, pushing our arguments onto that
    /// context's stack, and then switching to that context to execute compiled code. Unlike what the "implementation
    /// steps" suggest, we do _not_ return the value from the called function here; all we're really doing is
    /// everything up to parameter evaluation, at which point the compiled code takes over. Return values are handled
    /// by the EXIT_FUNCTION instruction.
    ///
    /// Compare with [Function Object's \[\[Call\]\]
    /// Method](https://tc39.es/ecma262/#sec-ecmascript-function-objects-call-thisargument-argumentslist) in ECMA-262.
    fn call(&self, self_object: &Object, this_argument: &ECMAScriptValue, arguments_list: &[ECMAScriptValue]) {
        // [[Call]] ( thisArgument, argumentsList )
        //
        // The [[Call]] internal method of an ECMAScript function object F takes arguments thisArgument (an ECMAScript
        // language value) and argumentsList (a List of ECMAScript language values) and returns either a normal
        // completion containing an ECMAScript language value or a throw completion. It performs the following steps
        // when called:
        //
        //   1. Let callerContext be the running execution context.
        //   2. Let calleeContext be PrepareForOrdinaryCall(F, undefined).
        //   3. Assert: calleeContext is now the running execution context.
        //   4. If F.[[IsClassConstructor]] is true, then
        //      a. Let error be a newly created TypeError object.
        //      b. NOTE: error is created in calleeContext with F's associated Realm Record.
        //      c. Remove calleeContext from the execution context stack and restore callerContext as the running
        //         execution context.
        //      d. Return ThrowCompletion(error).
        //   5. Perform OrdinaryCallBindThis(F, calleeContext, thisArgument).
        //   6. Let result be Completion(OrdinaryCallEvaluateBody(F, argumentsList)).
        //   ...
        prepare_for_ordinary_call(self_object, None);
        if self.function_data.borrow().is_class_constructor {
            let error = create_type_error("constructors must use 'new'.");
            pop_execution_context();
            ec_push(Err(error));
            return;
        }
        ordinary_call_bind_this(self_object, this_argument.clone());
        ordinary_call_evaluate_body(self_object, arguments_list);
    }

    fn end_evaluation(&self, result: FullCompletion) {
        // (From [[Call]])
        //  6. Let result be Completion(OrdinaryCallEvaluateBody(F, argumentsList)).
        //  7. Remove calleeContext from the execution context stack and restore callerContext as the running
        //     execution context.
        //  8. If result.[[Type]] is return, return result.[[Value]].
        //  9. ReturnIfAbrupt(result).
        // 10. Return undefined.

        // (From [[Construct]])
        //  8. Let result be Completion(OrdinaryCallEvaluateBody(F, argumentsList)).
        //  9. Remove calleeContext from the execution context stack and restore callerContext as the running
        //     execution context.
        // 10. If result.[[Type]] is return, then
        //      a. If Type(result.[[Value]]) is Object, return result.[[Value]].
        //      b. If kind is base, return thisArgument.
        //      c. If result.[[Value]] is not undefined, throw a TypeError exception.
        // 11. Else, ReturnIfAbrupt(result).
        // 12. Let thisBinding be ? constructorEnv.GetThisBinding().
        // 13. Assert: Type(thisBinding) is Object.
        // 14. Return thisBinding.
        if ec_stack_len() == 0 {
            // From [[Call]]
            pop_execution_context();
            ec_push(if let Err(AbruptCompletion::Return { value }) = result {
                Ok(value.into())
            } else if result.is_err() {
                result
            } else {
                Ok(ECMAScriptValue::Undefined.into())
            });
        } else {
            // From [[Construct]]
            let this_argument = ECMAScriptValue::try_from(
                ec_pop().expect("Must be at least two things on the stack").expect("which must not be errors"),
            )
            .expect("the first of which must be a value");
            let constructor_env: Rc<dyn EnvironmentRecord> = ec_pop()
                .expect("Must be at least one more item on the stack")
                .expect("which must not be an error")
                .try_into()
                .expect("And which must be an environment record");
            assert_eq!(ec_stack_len(), 0);
            pop_execution_context();
            if let Err(AbruptCompletion::Return { value }) = result {
                if value.is_object() {
                    ec_push(Ok(value.into()));
                    return;
                }
                if !this_argument.is_null() {
                    ec_push(Ok(this_argument.into()));
                    return;
                }
                if !value.is_undefined() {
                    let err = create_type_error("Constructors must return objects");
                    ec_push(Err(err));
                    return;
                }
            } else if let Err(e) = result {
                ec_push(Err(e));
                return;
            }
            let this_binding = constructor_env.get_this_binding();
            ec_push(this_binding.map(NormalCompletion::from));
        }
    }

    fn construct(&self, self_object: &Object, arguments_list: &[ECMAScriptValue], new_target: &Object) {
        // [[Construct]] ( argumentsList, newTarget )
        //
        // The [[Construct]] internal method of an ECMAScript function object F takes arguments argumentsList
        // (a List of ECMAScript language values) and newTarget (a constructor) and returns either a normal
        // completion containing an Object or a throw completion. It performs the following steps when called:
        //
        //  1. Let callerContext be the running execution context.
        //  2. Let kind be F.[[ConstructorKind]].
        //  3. If kind is base, then
        //      a. Let thisArgument be ? OrdinaryCreateFromConstructor(newTarget, "%Object.prototype%").
        //  4. Let calleeContext be PrepareForOrdinaryCall(F, newTarget).
        //  5. Assert: calleeContext is now the running execution context.
        //  6. If kind is base, then
        //      a. Perform OrdinaryCallBindThis(F, calleeContext, thisArgument).
        //      b. Let initializeResult be Completion(InitializeInstanceElements(thisArgument, F)).
        //      c. If initializeResult is an abrupt completion, then
        //           i. Remove calleeContext from the execution context stack and restore callerContext as the
        //              running execution context.
        //          ii. Return ? initializeResult.
        //  7. Let constructorEnv be the LexicalEnvironment of calleeContext.
        //  8. Let result be Completion(OrdinaryCallEvaluateBody(F, argumentsList)).
        let kind = self.function_data().borrow().constructor_kind;
        let this_argument = if kind == ConstructorKind::Base {
            let ta = new_target.ordinary_create_from_constructor(IntrinsicId::ObjectPrototype, &[]);
            match ta {
                Err(err) => {
                    ec_push(Err(err));
                    return;
                }
                Ok(obj) => {
                    // Provide some values for after the constructor returns...
                    Some(obj)
                }
            }
        } else {
            // Provide some values for after the constructor returns...
            None
        };

        prepare_for_ordinary_call(self_object, Some(new_target.clone()));
        if kind == ConstructorKind::Base {
            ordinary_call_bind_this(self_object, this_argument.clone().expect("previously created").into());
            let initialize_result = initialize_instance_elements(this_argument.as_ref().unwrap(), self_object);
            if let Err(err) = initialize_result {
                pop_execution_context();
                ec_push(Err(err));
                return;
            }
        }
        let constructor_env = current_lexical_environment().expect("A lexical environment must exist");
        ec_push(Ok(NormalCompletion::Environment(constructor_env)));
        ec_push(Ok(match this_argument {
            None => ECMAScriptValue::Null.into(),
            Some(obj) => ECMAScriptValue::Object(obj).into(),
        }));
        ordinary_call_evaluate_body(self_object, arguments_list);
    }

    fn complete_call(&self) -> Completion<ECMAScriptValue> {
        let empty = String::new();
        let fod = self.function_data.borrow();
        let text = fod.script_or_module.as_ref().map_or(&empty, ScriptOrModule::source_text);
        execute(text)
    }
}

pub fn initialize_instance_elements(this_argument: &Object, constructor: &Object) -> Completion<()> {
    // InitializeInstanceElements ( O, constructor )
    // The abstract operation InitializeInstanceElements takes arguments O (an Object) and constructor (an ECMAScript
    // function object) and returns either a normal completion containing unused or a throw completion. It performs the
    // following steps when called:
    //
    //  1. Let methods be the value of constructor.[[PrivateMethods]].
    //  2. For each PrivateElement method of methods, do
    //      a. Perform ? PrivateMethodOrAccessorAdd(O, method).
    //  3. Let fields be the value of constructor.[[Fields]].
    //  4. For each element fieldRecord of fields, do
    //      a. Perform ? DefineField(O, fieldRecord).
    //  5. Return unused.
    let data = constructor.o.to_function_obj().unwrap().function_data().borrow();
    for method in &data.private_methods {
        private_method_or_accessor_add(this_argument, method.clone())?;
    }
    for field_record in &data.fields {
        define_field(this_argument, field_record)?;
    }
    Ok(())
}

impl FunctionInterface for FunctionObject {
    fn function_data(&self) -> &RefCell<FunctionObjectData> {
        &self.function_data
    }
}

impl FunctionObject {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        prototype: Option<Object>,
        environment: Rc<dyn EnvironmentRecord>,
        private_environment: Option<Rc<RefCell<PrivateEnvironmentRecord>>>,
        formal_parameters: ParamSource,
        ecmascript_code: BodySource,
        constructor_kind: ConstructorKind,
        realm: Rc<RefCell<Realm>>,
        script_or_module: Option<ScriptOrModule>,
        this_mode: ThisMode,
        strict: bool,
        home_object: Option<Object>,
        source_text: &str,
        fields: Vec<ClassFieldDefinitionRecord>,
        private_methods: Vec<Rc<PrivateElement>>,
        class_field_initializer_name: ClassName,
        is_class_constructor: bool,
        compiled: Rc<Chunk>,
    ) -> Self {
        Self {
            common: RefCell::new(CommonObjectData::new(prototype, true, FUNCTION_OBJECT_SLOTS)),
            function_data: RefCell::new(FunctionObjectData {
                environment,
                private_environment,
                formal_parameters,
                ecmascript_code,
                constructor_kind,
                realm,
                script_or_module,
                this_mode,
                strict,
                home_object,
                source_text: source_text.to_string(),
                is_class_constructor,
                fields,
                private_methods,
                class_field_initializer_name,
                is_constructor: false,
                compiled,
            }),
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub fn object(
        prototype: Option<Object>,
        environment: Rc<dyn EnvironmentRecord>,
        private_environment: Option<Rc<RefCell<PrivateEnvironmentRecord>>>,
        formal_parameters: ParamSource,
        ecmascript_code: BodySource,
        constructor_kind: ConstructorKind,
        realm: Rc<RefCell<Realm>>,
        script_or_module: Option<ScriptOrModule>,
        this_mode: ThisMode,
        strict: bool,
        home_object: Option<Object>,
        source_text: &str,
        fields: Vec<ClassFieldDefinitionRecord>,
        private_methods: Vec<Rc<PrivateElement>>,
        class_field_initializer_name: ClassName,
        is_class_constructor: bool,
        compiled: Rc<Chunk>,
    ) -> Object {
        Object {
            o: Rc::new(Self::new(
                prototype,
                environment,
                private_environment,
                formal_parameters,
                ecmascript_code,
                constructor_kind,
                realm,
                script_or_module,
                this_mode,
                strict,
                home_object,
                source_text,
                fields,
                private_methods,
                class_field_initializer_name,
                is_class_constructor,
                compiled,
            )),
        }
    }
}

/// Establish an execution context for user function calls, and put it on the execution context stack
///
/// The function being called is `func`, and any target for New expressions is in `new_target`.
///
/// See [PrepareForOrdinaryCall](https://tc39.es/ecma262/#sec-prepareforordinarycall) from ECMA-262.
pub fn prepare_for_ordinary_call(func: &Object, new_target: Option<Object>) {
    // PrepareForOrdinaryCall ( F, newTarget )
    //
    // The abstract operation PrepareForOrdinaryCall takes arguments F (a function object) and newTarget (an Object
    // or undefined) and returns an execution context. It performs the following steps when called:
    //
    //   1. Let callerContext be the running execution context.
    //   2. Let calleeContext be a new ECMAScript code execution context.
    //   3. Set the Function of calleeContext to F.
    //   4. Let calleeRealm be F.[[Realm]].
    //   5. Set the Realm of calleeContext to calleeRealm.
    //   6. Set the ScriptOrModule of calleeContext to F.[[ScriptOrModule]].
    //   7. Let localEnv be NewFunctionEnvironment(F, newTarget).
    //   8. Set the LexicalEnvironment of calleeContext to localEnv.
    //   9. Set the VariableEnvironment of calleeContext to localEnv.
    //  10. Set the PrivateEnvironment of calleeContext to F.[[PrivateEnvironment]].
    //  11. If callerContext is not already suspended, suspend callerContext.
    //  12. Push calleeContext onto the execution context stack; calleeContext is now the running execution context.
    //  13. NOTE: Any exception objects produced after this point are associated with calleeRealm.
    //  14. Return calleeContext.

    // Pull out all the pieces from function data that we need
    let (callee_realm, s_or_m, private_environment, name) = {
        let function_data = func
            .o
            .to_function_obj()
            .expect("PrepareForOrdinaryCall works only with function objects")
            .function_data()
            .borrow();
        let callee_realm = Rc::clone(&function_data.realm);
        let s_or_m = function_data.script_or_module.clone();
        let privates = function_data.private_environment.clone();
        let name = nameify(&function_data.source_text, 50);
        (callee_realm, s_or_m, privates, name)
    };

    // New context for function call, with the basics captured from the function object
    let mut callee_context = ExecutionContext::new(Some(func.clone()), callee_realm, s_or_m);

    // New environment record. Function bindings will be stored in here.
    let env_name = name;
    let local_env = FunctionEnvironmentRecord::new(func.clone(), new_target, env_name);
    let env_ptr: Rc<dyn EnvironmentRecord> = Rc::new(local_env);

    // Connect the environment record into the execution context. Note that for functions, the lexical and variable
    // binding space is (initially) identical.
    callee_context.lexical_environment = Some(Rc::clone(&env_ptr));
    callee_context.variable_environment = Some(env_ptr);

    // And also the private environment. These are effectively a kind of "class" variable, as they persist through
    // multiple function calls, and potentially different functions.
    callee_context.private_environment = private_environment;

    // Push onto the EC stack; this is now the real deal, though no compiled chunks have been attached yet. (Should
    // that happen here?)
    push_execution_context(callee_context);
}

/// Bind the "this" value for the current function call.
///
/// The function's `this_mode` controls what happens here. `Lexical` "this" mode means this function does
/// nothing. (There is no "this".) `Strict` "this" mode uses the provided `this_argument` with no change.
fn ordinary_call_bind_this(func: &Object, this_argument: ECMAScriptValue) {
    // OrdinaryCallBindThis ( F, calleeContext, thisArgument )
    //
    // The abstract operation OrdinaryCallBindThis takes arguments F (a function object), calleeContext (an
    // execution context), and thisArgument (an ECMAScript language value) and returns unused. It performs the
    // following steps when called:
    //
    //   1. Let thisMode be F.[[ThisMode]].
    //   2. If thisMode is lexical, return unused.
    //   3. Let calleeRealm be F.[[Realm]].
    //   4. Let localEnv be the LexicalEnvironment of calleeContext.
    //   5. If thisMode is strict, let thisValue be thisArgument.
    //   6. Else,
    //      a. If thisArgument is undefined or null, then
    //            i. Let globalEnv be calleeRealm.[[GlobalEnv]].
    //           ii. Assert: globalEnv is a global Environment Record.
    //          iii. Let thisValue be globalEnv.[[GlobalThisValue]].
    //      b. Else,
    //            i. Let thisValue be ! ToObject(thisArgument).
    //           ii. NOTE: ToObject produces wrapper objects using calleeRealm.
    //   7. Assert: localEnv is a function Environment Record.
    //   8. Assert: The next step never returns an abrupt completion because localEnv.[[ThisBindingStatus]] is not
    //      initialized.
    //   9. Perform ! localEnv.BindThisValue(thisValue).
    //  10. Return unused.
    let (this_mode, callee_realm) = {
        let function_data = func
            .o
            .to_function_obj()
            .expect("OrdinaryCallBindThis works only with function objects")
            .function_data()
            .borrow();
        let this_mode = function_data.this_mode;
        if this_mode == ThisMode::Lexical {
            return;
        }
        (this_mode, function_data.realm.clone())
    };
    let local_env = current_lexical_environment().expect("Context must have a lexical environment");
    let this_value = if this_mode == ThisMode::Strict {
        this_argument
    } else if this_argument == ECMAScriptValue::Undefined || this_argument == ECMAScriptValue::Null {
        let global_env =
            callee_realm.borrow().global_env.clone().expect("A global environment must exist for this realm");
        global_env.get_this_binding().expect("This binding must exist")
    } else {
        ECMAScriptValue::from(to_object(this_argument).expect("Must be objectifiable"))
    };
    local_env.bind_this_value(this_value).expect("This binding should be uninitialized");
}

fn ordinary_call_evaluate_body(func: &Object, args: &[ECMAScriptValue]) {
    // OrdinaryCallEvaluateBody ( F, argumentsList )
    //
    // The abstract operation OrdinaryCallEvaluateBody takes arguments F (a function object) and argumentsList (a
    // List) and returns either a normal completion containing an ECMAScript language value or an abrupt
    // completion. It performs the following steps when called:
    //
    // 1. Return ? EvaluateBody of F.[[ECMAScriptCode]] with arguments F and argumentsList.

    // So "EvaluateBody" is what's compiled into this function's chunk; all we really need to do here is put the
    // arguments on the stack and set up the execution context for running code.
    let func_val = NormalCompletion::from(func.clone());
    let data = func
        .o
        .to_function_obj()
        .expect("OrdinaryCallEvaluateBody only works for function objects")
        .function_data()
        .borrow();
    let chunk = Rc::clone(&data.compiled);
    prepare_running_ec_for_execution(chunk);
    ec_push(Ok(func_val));
    for item in args {
        ec_push(Ok(item.clone().into()));
    }
    ec_push(Ok(args.len().into()));
}

pub fn nameify(src: &str, limit: usize) -> String {
    let minimized = src.trim().split(|c: char| c.is_whitespace()).filter(|x| !x.is_empty()).join(" ");
    let (always_shown, maybe_shown): (Vec<_>, Vec<_>) =
        minimized.chars().enumerate().partition(|&(idx, _)| idx < limit - 3);

    let always_shown = always_shown.into_iter().map(|(_, ch)| ch).collect::<String>();
    let (maybe_shown, never_shown): (Vec<_>, Vec<_>) = maybe_shown.iter().partition(|(idx, _)| *idx < limit);

    if never_shown.is_empty() {
        format!("{always_shown}{}", maybe_shown.into_iter().map(|(_, ch)| ch).collect::<String>())
    } else {
        format!("{always_shown}...")
    }
}

// SetFunctionName ( F, name [ , prefix ] )
//
// The abstract operation SetFunctionName takes arguments F (a function object) and name (a property key or Private
// Name) and optional argument prefix (a String). It adds a "name" property to F. It performs the following steps when
// called:
//
//      1. Assert: F is an extensible object that does not have a "name" own property.
//      2. If Type(name) is Symbol, then
//          a. Let description be name's [[Description]] value.
//          b. If description is undefined, set name to the empty String.
//          c. Else, set name to the string-concatenation of "[", description, and "]".
//      3. Else if name is a Private Name, then
//          a. Set name to name.[[Description]].
//      4. If F has an [[InitialName]] internal slot, then
//          a. Set F.[[InitialName]] to name.
//      5. If prefix is present, then
//          a. Set name to the string-concatenation of prefix, the code unit 0x0020 (SPACE), and name.
//          b. If F has an [[InitialName]] internal slot, then
//              i. Optionally, set F.[[InitialName]] to name.
//      6. Return ! DefinePropertyOrThrow(F, "name", PropertyDescriptor { [[Value]]: name, [[Writable]]: false,
//         [[Enumerable]]: false, [[Configurable]]: true }).
pub fn set_function_name(func: &Object, name: FunctionName, prefix: Option<JSString>) {
    let name_before_prefix = match name {
        FunctionName::String(s) => s,
        FunctionName::PrivateName(pn) => pn.description,
        FunctionName::Symbol(sym) => sym.description().map_or_else(
            || JSString::from(""),
            |description| {
                let mut n: Vec<u16> = vec!['[' as u16];
                n.extend_from_slice(description.as_slice());
                n.push(']' as u16);
                JSString::from(n)
            },
        ),
    };
    let name_after_prefix = match prefix {
        None => name_before_prefix,
        Some(pfx) => {
            let mut name: Vec<u16> = Vec::with_capacity(pfx.len() + name_before_prefix.len() + 1);
            name.extend_from_slice(pfx.as_slice());
            name.push(' ' as u16);
            name.extend_from_slice(name_before_prefix.as_slice());
            JSString::from(name)
        }
    };
    if let Some(builtin) = func.o.to_builtin_function_obj() {
        builtin.builtin_function_data().borrow_mut().initial_name = Some(FunctionName::from(name_after_prefix.clone()));
    }
    define_property_or_throw(
        func,
        "name",
        PotentialPropertyDescriptor::new()
            .value(name_after_prefix)
            .writable(false)
            .enumerable(false)
            .configurable(true),
    )
    .unwrap();
}

// SetFunctionLength ( F, length )
//
// The abstract operation SetFunctionLength takes arguments F (a function object) and length (a non-negative integer or
// +∞). It adds a "length" property to F. It performs the following steps when called:
//
//      1. Assert: F is an extensible object that does not have a "length" own property.
//      2. Return ! DefinePropertyOrThrow(F, "length", PropertyDescriptor { [[Value]]: 𝔽(length), [[Writable]]: false,
//         [[Enumerable]]: false, [[Configurable]]: true }).
pub fn set_function_length(func: &Object, length: f64) {
    define_property_or_throw(
        func,
        "length",
        PotentialPropertyDescriptor::new().value(length).writable(false).enumerable(false).configurable(true),
    )
    .unwrap();
}

//#############################################
//# BUILT IN FUNCTIONS
//#############################################

// A small little arguments iterator, useful for built-in functions.
// When you have
// fn builtin_function(..., arguments: &[ECMAScriptValue]) -> ...
// Then in your code you can say:
//      let mut args = FuncArgs::from(arguments);
//      let first_arg = args.next_arg();
//      let second_arg = args.next_arg();
// etc. If the args are there, you get them, if the arguments array is short, then you get undefined.
// args.remaining() returns an iterator over the "rest" of the args (since "next_arg" won't tell if you've "gotten to the end")
pub struct FuncArgs<'a> {
    iterator: std::slice::Iter<'a, ECMAScriptValue>,
    count: usize,
}
impl<'a> From<&'a [ECMAScriptValue]> for FuncArgs<'a> {
    fn from(source: &'a [ECMAScriptValue]) -> Self {
        let count = source.len();
        Self { iterator: source.iter(), count }
    }
}
impl<'a> FuncArgs<'a> {
    pub fn next_arg(&mut self) -> ECMAScriptValue {
        self.iterator.next().cloned().unwrap_or(ECMAScriptValue::Undefined)
    }
    pub fn count(&self) -> usize {
        self.count
    }
    pub fn remaining(&mut self) -> &mut std::slice::Iter<'a, ECMAScriptValue> {
        &mut self.iterator
    }
}

#[derive(Debug, Clone)]
pub enum FunctionName {
    String(JSString),
    Symbol(Symbol),
    PrivateName(PrivateName),
}

impl fmt::Display for FunctionName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FunctionName::String(s) => write!(f, "{s}"),
            FunctionName::Symbol(sym) => {
                write!(f, "[Symbol({})]", sym.description().unwrap_or_else(|| JSString::from("")))
            }
            FunctionName::PrivateName(pn) => write!(f, "#{}", pn.description),
        }
    }
}

impl From<PropertyKey> for FunctionName {
    fn from(source: PropertyKey) -> Self {
        match source {
            PropertyKey::String(s) => FunctionName::String(s),
            PropertyKey::Symbol(s) => FunctionName::Symbol(s),
        }
    }
}
impl From<PrivateName> for FunctionName {
    fn from(source: PrivateName) -> Self {
        FunctionName::PrivateName(source)
    }
}

impl From<JSString> for FunctionName {
    fn from(source: JSString) -> Self {
        FunctionName::String(source)
    }
}

impl From<&str> for FunctionName {
    fn from(source: &str) -> Self {
        Self::String(JSString::from(source))
    }
}

impl TryFrom<FunctionName> for PropertyKey {
    type Error = anyhow::Error;

    fn try_from(value: FunctionName) -> Result<Self, Self::Error> {
        match value {
            FunctionName::String(s) => Ok(PropertyKey::from(s)),
            FunctionName::Symbol(s) => Ok(PropertyKey::from(s)),
            FunctionName::PrivateName(_) => Err(anyhow!("PrivateName can't be converted to PropertyKey")),
        }
    }
}

impl TryFrom<NormalCompletion> for FunctionName {
    type Error = anyhow::Error;

    fn try_from(value: NormalCompletion) -> Result<Self, Self::Error> {
        match value {
            NormalCompletion::Value(ECMAScriptValue::String(s)) => Ok(FunctionName::String(s)),
            NormalCompletion::Value(ECMAScriptValue::Symbol(s)) => Ok(FunctionName::Symbol(s)),
            NormalCompletion::PrivateName(pn) => Ok(FunctionName::PrivateName(pn)),
            NormalCompletion::Value(_)
            | NormalCompletion::Empty
            | NormalCompletion::Reference(_)
            | NormalCompletion::Environment(_)
            | NormalCompletion::IteratorRecord(_)
            | NormalCompletion::PrivateElement(_) => bail!("Completion type not valid for FunctionName"),
        }
    }
}

pub struct BuiltInFunctionData {
    pub realm: Rc<RefCell<Realm>>,
    pub initial_name: Option<FunctionName>,
    pub steps: fn(&ECMAScriptValue, Option<&Object>, &[ECMAScriptValue]) -> Completion<ECMAScriptValue>,
    pub is_constructor: bool,
}

impl fmt::Debug for BuiltInFunctionData {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), std::fmt::Error> {
        f.debug_struct("BuiltInFunctionData")
            .field("realm", &self.realm)
            .field("initial_name", &self.initial_name)
            .field("steps", &"<steps>")
            .field("is_constructor", &self.is_constructor)
            .finish()
    }
}

impl BuiltInFunctionData {
    pub fn new(
        realm: Rc<RefCell<Realm>>,
        initial_name: Option<FunctionName>,
        steps: fn(&ECMAScriptValue, Option<&Object>, &[ECMAScriptValue]) -> Completion<ECMAScriptValue>,
        is_constructor: bool,
    ) -> Self {
        Self { realm, initial_name, steps, is_constructor }
    }
}

#[derive(Debug)]
pub struct BuiltInFunctionObject {
    common: RefCell<CommonObjectData>,
    builtin_data: RefCell<BuiltInFunctionData>,
}

impl<'a> From<&'a BuiltInFunctionObject> for &'a dyn ObjectInterface {
    fn from(obj: &'a BuiltInFunctionObject) -> Self {
        obj
    }
}

impl BuiltinFunctionInterface for BuiltInFunctionObject {
    fn builtin_function_data(&self) -> &RefCell<BuiltInFunctionData> {
        &self.builtin_data
    }
}

impl BuiltInFunctionObject {
    pub fn new(
        prototype: Option<Object>,
        extensible: bool,
        realm: Rc<RefCell<Realm>>,
        initial_name: Option<FunctionName>,
        steps: fn(&ECMAScriptValue, Option<&Object>, &[ECMAScriptValue]) -> Completion<ECMAScriptValue>,
        is_constructor: bool,
    ) -> Self {
        Self {
            common: RefCell::new(CommonObjectData::new(prototype, extensible, BUILTIN_FUNCTION_SLOTS)),
            builtin_data: RefCell::new(BuiltInFunctionData::new(realm, initial_name, steps, is_constructor)),
        }
    }

    pub fn object(
        prototype: Option<Object>,
        extensible: bool,
        realm: Rc<RefCell<Realm>>,
        initial_name: Option<FunctionName>,
        steps: fn(&ECMAScriptValue, Option<&Object>, &[ECMAScriptValue]) -> Completion<ECMAScriptValue>,
        is_constructor: bool,
    ) -> Object {
        Object { o: Rc::new(Self::new(prototype, extensible, realm, initial_name, steps, is_constructor)) }
    }
}

pub trait BuiltinFunctionInterface: CallableObject {
    fn builtin_function_data(&self) -> &RefCell<BuiltInFunctionData>;
}

impl ObjectInterface for BuiltInFunctionObject {
    fn common_object_data(&self) -> &RefCell<CommonObjectData> {
        &self.common
    }
    fn uses_ordinary_get_prototype_of(&self) -> bool {
        true
    }
    fn id(&self) -> usize {
        self.common.borrow().objid
    }
    fn to_function_obj(&self) -> Option<&dyn FunctionInterface> {
        None
    }
    fn to_callable_obj(&self) -> Option<&dyn CallableObject> {
        Some(self)
    }
    fn to_constructable(&self) -> Option<&dyn CallableObject> {
        let is_c = self.builtin_function_data().borrow().is_constructor;
        if is_c {
            Some(self)
        } else {
            None
        }
    }
    fn to_builtin_function_obj(&self) -> Option<&dyn BuiltinFunctionInterface> {
        Some(self)
    }
    fn is_callable_obj(&self) -> bool {
        true
    }

    fn get_prototype_of(&self) -> Completion<Option<Object>> {
        Ok(ordinary_get_prototype_of(self))
    }
    fn set_prototype_of(&self, obj: Option<Object>) -> Completion<bool> {
        Ok(ordinary_set_prototype_of(self, obj))
    }
    fn is_extensible(&self) -> Completion<bool> {
        Ok(ordinary_is_extensible(self))
    }
    fn prevent_extensions(&self) -> Completion<bool> {
        Ok(ordinary_prevent_extensions(self))
    }
    fn get_own_property(&self, key: &PropertyKey) -> Completion<Option<PropertyDescriptor>> {
        Ok(ordinary_get_own_property(self, key))
    }
    fn define_own_property(&self, key: PropertyKey, desc: PotentialPropertyDescriptor) -> Completion<bool> {
        ordinary_define_own_property(self, key, desc)
    }
    fn has_property(&self, key: &PropertyKey) -> Completion<bool> {
        ordinary_has_property(self, key)
    }
    fn get(&self, key: &PropertyKey, receiver: &ECMAScriptValue) -> Completion<ECMAScriptValue> {
        ordinary_get(self, key, receiver)
    }
    fn set(&self, key: PropertyKey, v: ECMAScriptValue, receiver: &ECMAScriptValue) -> Completion<bool> {
        ordinary_set(self, key, v, receiver)
    }
    fn delete(&self, key: &PropertyKey) -> Completion<bool> {
        ordinary_delete(self, key)
    }
    fn own_property_keys(&self) -> Completion<Vec<PropertyKey>> {
        Ok(ordinary_own_property_keys(self))
    }
}

impl CallableObject for BuiltInFunctionObject {
    // [[Call]] ( thisArgument, argumentsList )
    //
    // The [[Call]] internal method of a built-in function object F takes arguments thisArgument (an ECMAScript language
    // value) and argumentsList (a List of ECMAScript language values). It performs the following steps when called:
    //
    //      1. Let callerContext be the running execution context.
    //      2. If callerContext is not already suspended, suspend callerContext.
    //      3. Let calleeContext be a new execution context.
    //      4. Set the Function of calleeContext to F.
    //      5. Let calleeRealm be F.[[Realm]].
    //      6. Set the Realm of calleeContext to calleeRealm.
    //      7. Set the ScriptOrModule of calleeContext to null.
    //      8. Perform any necessary implementation-defined initialization of calleeContext.
    //      9. Push calleeContext onto the execution context stack; calleeContext is now the running execution context.
    //      10. Let result be the Completion Record that is the result of evaluating F in a manner that conforms to the
    //          specification of F. thisArgument is the this value, argumentsList provides the named parameters, and the
    //          NewTarget value is undefined.
    //      11. Remove calleeContext from the execution context stack and restore callerContext as the running execution
    //          context.
    //      12. Return result.
    //
    // NOTE     | When calleeContext is removed from the execution context stack it must not be destroyed if it has been
    //          | suspended and retained by an accessible generator object for later resumption.
    fn call(&self, self_object: &Object, this_argument: &ECMAScriptValue, arguments_list: &[ECMAScriptValue]) {
        assert_eq!(self.id(), self_object.o.id(), "self and self_object must refer to the same object");
        let callee_context =
            ExecutionContext::new(Some(self_object.clone()), self.builtin_data.borrow().realm.clone(), None);
        push_execution_context(callee_context);
        let result = (self.builtin_data.borrow().steps)(this_argument, None, arguments_list);
        pop_execution_context();

        ec_push(result.map(NormalCompletion::from));
    }

    // [[Construct]] ( argumentsList, newTarget )
    //
    // The [[Construct]] internal method of a built-in function object F takes arguments argumentsList (a List of
    // ECMAScript language values) and newTarget (a constructor). The steps performed are the same as [[Call]] (see
    // 10.3.1) except that step 10 is replaced by:
    //
    // 10. Let result be the Completion Record that is the result of evaluating F in a manner that conforms to the
    //     specification of F. The this value is uninitialized, argumentsList provides the named parameters, and
    //     newTarget provides the NewTarget value.
    fn construct(&self, self_object: &Object, arguments_list: &[ECMAScriptValue], new_target: &Object) {
        assert_eq!(self.id(), self_object.o.id(), "self and self_object must refer to the same object");
        let callee_context =
            ExecutionContext::new(Some(self_object.clone()), self.builtin_data.borrow().realm.clone(), None);
        push_execution_context(callee_context);
        let result = (self.builtin_data.borrow().steps)(&ECMAScriptValue::Undefined, Some(new_target), arguments_list);
        pop_execution_context();

        ec_push(result.map(NormalCompletion::from));
    }

    fn end_evaluation(&self, _: FullCompletion) {
        panic!("end_evaluation called for builtin callable")
    }

    fn complete_call(&self) -> Completion<ECMAScriptValue> {
        ec_pop()
            .expect("Call must return a Completion")
            .map(|nc| ECMAScriptValue::try_from(nc).expect("Call must return a language value"))
    }
}

// CreateBuiltinFunction ( behaviour, length, name, internalSlotsList [ , realm [ , prototype [ , prefix ] ] ] )
//
// The abstract operation CreateBuiltinFunction takes arguments behaviour, length (a non-negative integer or +∞), name
// (a property key), and internalSlotsList (a List of names of internal slots) and optional arguments realm (a Realm
// Record), prototype (an Object or null), and prefix (a String). internalSlotsList contains the names of additional
// internal slots that must be defined as part of the object. This operation creates a built-in function object. It
// performs the following steps when called:
//
//      1. Assert: behaviour is either an Abstract Closure, a set of algorithm steps, or some other definition of a
//         function's behaviour provided in this specification.
//      2. If realm is not present, set realm to the current Realm Record.
//      3. Assert: realm is a Realm Record.
//      4. If prototype is not present, set prototype to realm.[[Intrinsics]].[[%Function.prototype%]].
//      5. Let func be a new built-in function object that, when called, performs the action described by behaviour
//         using the provided arguments as the values of the corresponding parameters specified by behaviour. The new
//         function object has internal slots whose names are the elements of internalSlotsList, and an [[InitialName]]
//         internal slot.
//      6. Set func.[[Realm]] to realm.
//      7. Set func.[[Prototype]] to prototype.
//      8. Set func.[[Extensible]] to true.
//      9. Set func.[[InitialName]] to null.
//      10. Perform ! SetFunctionLength(func, length).
//      11. If prefix is not present, then
//          a. Perform ! SetFunctionName(func, name).
//      12. Else,
//          a. Perform ! SetFunctionName(func, name, prefix).
//      13. Return func.
//
// Each built-in function defined in this specification is created by calling the CreateBuiltinFunction abstract
// operation.
#[allow(clippy::too_many_arguments)]
pub fn create_builtin_function(
    behavior: fn(&ECMAScriptValue, Option<&Object>, &[ECMAScriptValue]) -> Completion<ECMAScriptValue>,
    is_constructor: bool,
    length: f64,
    name: PropertyKey,
    _internal_slots_list: &[InternalSlotName],
    realm: Option<Rc<RefCell<Realm>>>,
    prototype: Option<Object>,
    prefix: Option<JSString>,
) -> Object {
    let realm_to_use = realm.unwrap_or_else(|| current_realm_record().unwrap());
    let prototype_to_use = prototype.unwrap_or_else(|| realm_to_use.borrow().intrinsics.function_prototype.clone());
    let func =
        BuiltInFunctionObject::object(Some(prototype_to_use), true, realm_to_use, None, behavior, is_constructor);
    set_function_length(&func, length);
    set_function_name(&func, FunctionName::from(name), prefix);
    func
}

impl FunctionDeclaration {
    #[allow(unused_variables)]
    pub fn instantiate_function_object(
        &self,
        env: Rc<dyn EnvironmentRecord>,
        private_env: Option<Rc<RefCell<PrivateEnvironmentRecord>>>,
        strict: bool,
        text: &str,
        self_as_rc: Rc<Self>,
    ) -> Completion<ECMAScriptValue> {
        // Runtime Semantics: InstantiateOrdinaryFunctionObject
        //
        // The syntax-directed operation InstantiateOrdinaryFunctionObject takes arguments env and privateEnv
        // and returns a function object. It is defined piecewise over the following productions:
        //
        // FunctionDeclaration : function BindingIdentifier ( FormalParameters ) { FunctionBody }
        //  1. Let name be StringValue of BindingIdentifier.
        //  2. Let sourceText be the source text matched by FunctionDeclaration.
        //  3. Let F be OrdinaryFunctionCreate(%Function.prototype%, sourceText, FormalParameters,
        //     FunctionBody, non-lexical-this, env, privateEnv).
        //  4. Perform SetFunctionName(F, name).
        //  5. Perform MakeConstructor(F).
        //  6. Return F.
        //
        // FunctionDeclaration : function ( FormalParameters ) { FunctionBody }
        //  1. Let sourceText be the source text matched by FunctionDeclaration.
        //  2. Let F be OrdinaryFunctionCreate(%Function.prototype%, sourceText, FormalParameters,
        //     FunctionBody, non-lexical-this, env, privateEnv).
        //  3. Perform SetFunctionName(F, "default").
        //  4. Perform MakeConstructor(F).
        //  5. Return F.
        //
        // NOTE: An anonymous FunctionDeclaration can only occur as part of an export default declaration, and
        // its function code is therefore always strict mode code.

        let name = match &self.ident {
            None => JSString::from("default"),
            Some(id) => id.string_value(),
        };
        let strict = strict || self.body.function_body_contains_use_strict();
        let span = self.location().span;
        let source_text = text[span.starting_index..(span.starting_index + span.length)].to_string();
        let params = ParamSource::from(Rc::clone(&self.params));
        let body = BodySource::from(Rc::clone(&self.body));
        let chunk_name = nameify(&source_text, 50);
        let mut compiled = Chunk::new(chunk_name);
        let function_data = StashedFunctionData {
            source_text,
            params,
            body,
            strict,
            to_compile: FunctionSource::from(self_as_rc),
            this_mode: ThisLexicality::NonLexicalThis,
        };
        let compilation_status = self.body.compile_body(&mut compiled, text, &function_data);
        if let Err(err) = compilation_status {
            let typeerror = create_type_error(err.to_string());
            return Err(typeerror);
        }
        for line in compiled.disassemble() {
            println!("{line}");
        }

        let function_prototype = intrinsic(IntrinsicId::FunctionPrototype);

        let closure = ordinary_function_create(
            function_prototype,
            function_data.source_text.as_str(),
            function_data.params.clone(),
            function_data.body.clone(),
            ThisLexicality::NonLexicalThis,
            env,
            private_env,
            function_data.strict,
            Rc::new(compiled),
        );
        set_function_name(&closure, name.into(), None);
        make_constructor(&closure, None);

        Ok(closure.into())
    }
}

impl GeneratorDeclaration {
    #[allow(unused_variables, clippy::needless_pass_by_value)]
    pub fn instantiate_function_object(
        &self,
        env: Rc<dyn EnvironmentRecord>,
        private_env: Option<Rc<RefCell<PrivateEnvironmentRecord>>>,
        strict: bool,
        text: &str,
        self_as_rc: Rc<Self>,
    ) -> Completion<ECMAScriptValue> {
        todo!()
    }
}

impl AsyncFunctionDeclaration {
    #[allow(unused_variables, clippy::needless_pass_by_value)]
    pub fn instantiate_function_object(
        &self,

        env: Rc<dyn EnvironmentRecord>,
        private_env: Option<Rc<RefCell<PrivateEnvironmentRecord>>>,
        strict: bool,
        text: &str,
        self_as_rc: Rc<Self>,
    ) -> Completion<ECMAScriptValue> {
        todo!()
    }
}

impl AsyncGeneratorDeclaration {
    #[allow(unused_variables, clippy::needless_pass_by_value)]
    pub fn instantiate_function_object(
        &self,

        env: Rc<dyn EnvironmentRecord>,
        private_env: Option<Rc<RefCell<PrivateEnvironmentRecord>>>,
        strict: bool,
        text: &str,
        self_as_rc: Rc<Self>,
    ) -> Completion<ECMAScriptValue> {
        todo!()
    }
}

/// Create a ECMAScript Function Object, as though from source code
///
/// See [OrdinaryFunctionCreate](https://tc39.es/ecma262/#sec-ordinaryfunctioncreate) from ECMA-262.
#[allow(clippy::too_many_arguments)]
pub fn ordinary_function_create(
    function_prototype: Object,
    source_text: &str,
    parameter_list: ParamSource,
    body: BodySource,
    this_mode: ThisLexicality,
    env: Rc<dyn EnvironmentRecord>,
    private_env: Option<Rc<RefCell<PrivateEnvironmentRecord>>>,
    strict: bool,
    compiled: Rc<Chunk>,
) -> Object {
    // OrdinaryFunctionCreate ( functionPrototype, sourceText, ParameterList, Body, thisMode, env, privateEnv )
    //
    // The abstract operation OrdinaryFunctionCreate takes arguments functionPrototype (an Object), sourceText (a
    // sequence of Unicode code points), ParameterList (a Parse Node), Body (a Parse Node), thisMode (lexical-this or
    // non-lexical-this), env (an Environment Record), and privateEnv (a PrivateEnvironment Record or null) and returns
    // a function object. It is used to specify the runtime creation of a new function with a default [[Call]] internal
    // method and no [[Construct]] internal method (although one may be subsequently added by an operation such as
    // MakeConstructor). sourceText is the source text of the syntactic definition of the function to be created. It
    // performs the following steps when called:
    //
    //  1. Let internalSlotsList be the internal slots listed in Table 33.
    //  2. Let F be OrdinaryObjectCreate(functionPrototype, internalSlotsList).
    //  3. Set F.[[Call]] to the definition specified in 10.2.1.
    //  4. Set F.[[SourceText]] to sourceText.
    //  5. Set F.[[FormalParameters]] to ParameterList.
    //  6. Set F.[[ECMAScriptCode]] to Body.
    //  7. If the source text matched by Body is strict mode code, let Strict be true; else let Strict be false.
    //  8. Set F.[[Strict]] to Strict.
    //  9. If thisMode is lexical-this, set F.[[ThisMode]] to lexical.
    //  10. Else if Strict is true, set F.[[ThisMode]] to strict.
    //  11. Else, set F.[[ThisMode]] to global.
    //  12. Set F.[[IsClassConstructor]] to false.
    //  13. Set F.[[Environment]] to env.
    //  14. Set F.[[PrivateEnvironment]] to privateEnv.
    //  15. Set F.[[ScriptOrModule]] to GetActiveScriptOrModule().
    //  16. Set F.[[Realm]] to the current Realm Record.
    //  17. Set F.[[HomeObject]] to undefined.
    //  18. Set F.[[Fields]] to a new empty List.
    //  19. Set F.[[PrivateMethods]] to a new empty List.
    //  20. Set F.[[ClassFieldInitializerName]] to empty.
    //  21. Let len be the ExpectedArgumentCount of ParameterList.
    //  22. Perform SetFunctionLength(F, len).
    //  23. Return F.
    let this_mode = match this_mode {
        ThisLexicality::LexicalThis => ThisMode::Lexical,
        ThisLexicality::NonLexicalThis => {
            if strict {
                ThisMode::Strict
            } else {
                ThisMode::Global
            }
        }
    };
    let script_or_module = get_active_script_or_module();
    let realm = current_realm_record().unwrap();
    let prototype = Some(function_prototype);

    let arg_count = parameter_list.expected_argument_count();

    let func = FunctionObject::object(
        prototype,
        env,
        private_env,
        parameter_list,
        body,
        ConstructorKind::Base,
        realm,
        script_or_module,
        this_mode,
        strict,
        None,
        source_text,
        Vec::<ClassFieldDefinitionRecord>::new(),
        Vec::<Rc<PrivateElement>>::new(),
        ClassName::Empty,
        false,
        compiled,
    );

    set_function_length(&func, arg_count);

    func
}

/// Transform a function object into a constructor
///
/// See [MakeConstructor](https://tc39.es/ecma262/#sec-makeconstructor) from ECMA-262.
pub fn make_constructor(func: &Object, args: Option<(bool, Object)>) {
    // MakeConstructor ( F [ , writablePrototype [ , prototype ] ] )
    //
    // The abstract operation MakeConstructor takes argument F (an ECMAScript function object or a built-in function
    // object) and optional arguments writablePrototype (a Boolean) and prototype (an Object) and returns unused. It
    // converts F into a constructor. It performs the following steps when called:
    //
    //  1. If F is an ECMAScript function object, then
    //      a. Assert: IsConstructor(F) is false.
    //      b. Assert: F is an extensible object that does not have a "prototype" own property.
    //      c. Set F.[[Construct]] to the definition specified in 10.2.2.
    //  2. Else,
    //      a. Set F.[[Construct]] to the definition specified in 10.3.2.
    //  3. Set F.[[ConstructorKind]] to base.
    //  4. If writablePrototype is not present, set writablePrototype to true.
    //  5. If prototype is not present, then
    //      a. Set prototype to OrdinaryObjectCreate(%Object.prototype%).
    //      b. Perform ! DefinePropertyOrThrow(prototype, "constructor", PropertyDescriptor { [[Value]]: F,
    //         [[Writable]]: writablePrototype, [[Enumerable]]: false, [[Configurable]]: true }).
    //  6. Perform ! DefinePropertyOrThrow(F, "prototype", PropertyDescriptor { [[Value]]: prototype, [[Writable]]:
    //     writablePrototype, [[Enumerable]]: false, [[Configurable]]: false }).
    //  7. Return unused.
    let (writable_prototype_arg, prototype_arg) = match args {
        None => (None, None),
        Some((writable_prototype, prototype)) => (Some(writable_prototype), Some(prototype)),
    };
    let f = func.clone();
    match func.o.to_function_obj() {
        Some(func_obj) => {
            let mut func_data = func_obj.function_data().borrow_mut();
            assert!(!func_data.is_constructor);
            func_data.is_constructor = true;
            func_data.constructor_kind = ConstructorKind::Base;
        }
        None => match func.o.to_builtin_function_obj() {
            Some(bi_obj) => {
                let mut bi_data = bi_obj.builtin_function_data().borrow_mut();
                bi_data.is_constructor = true;
                //bi_data.constructor_kind = ConstructorKind::Base;
            }
            None => unreachable!(),
        },
    }
    let writable_prototype = writable_prototype_arg.unwrap_or(true);
    let prototype = match prototype_arg {
        Some(p) => p,
        None => {
            let obj_proto = intrinsic(IntrinsicId::ObjectPrototype);
            let p = ordinary_object_create(Some(obj_proto), &[]);
            define_property_or_throw(
                &p,
                "constructor",
                PotentialPropertyDescriptor::new()
                    .value(ECMAScriptValue::from(f.clone()))
                    .writable(writable_prototype)
                    .enumerable(false)
                    .configurable(true),
            )
            .expect("Valid defs don't throw");
            p
        }
    };
    define_property_or_throw(
        &f,
        "prototype",
        PotentialPropertyDescriptor::new()
            .value(ECMAScriptValue::from(prototype))
            .writable(writable_prototype)
            .enumerable(false)
            .configurable(false),
    )
    .expect("Valid defs don't throw");
}

fn function_prototype_call(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Function.prototype.call ( thisArg, ...args )
    // This method performs the following steps when called:
    //
    //  1. Let func be the this value.
    //  2. If IsCallable(func) is false, throw a TypeError exception.
    //  3. Perform PrepareForTailCall().
    //  4. Return ? Call(func, thisArg, args).
    //
    // NOTE 1: The thisArg value is passed without modification as the this value. This is a change from Edition 3,
    // where an undefined or null thisArg is replaced with the global object and ToObject is applied to all other
    // values and that result is passed as the this value. Even though the thisArg is passed without modification,
    // non-strict functions still perform these transformations upon entry to the function.
    //
    // NOTE 2: If func is an arrow function or a bound function exotic object then the thisArg will be ignored by the
    // function [[Call]] in step 4.
    let (first_arg, remaining): (_, &[ECMAScriptValue]) =
        if arguments.is_empty() { (&ECMAScriptValue::Undefined, &[]) } else { (&arguments[0], &arguments[1..]) };
    let func = this_value;
    if is_callable(func) {
        call(func, first_arg, remaining)
    } else {
        Err(create_type_error("Function.prototype.call requires that 'this' be a function"))
    }
}

pub fn provision_function_intrinsic(realm: &Rc<RefCell<Realm>>) {
    //let object_prototype = realm.borrow().intrinsics.object_prototype.clone();
    let function_prototype = realm.borrow().intrinsics.function_prototype.clone();

    // The Function constructor:
    //
    // * is %Function%.
    // * is the initial value of the "Function" property of the global object.
    // * creates and initializes a new function object when called as a function rather than as a constructor.
    //   Thus the function call Function(…) is equivalent to the object creation expression new Function(…)
    //   with the same arguments.
    // * may be used as the value of an extends clause of a class definition. Subclass constructors that
    //   intend to inherit the specified Function behaviour must include a super call to the Function
    //   constructor to create and initialize a subclass instance with the internal slots necessary for
    //   built-in function behaviour. All ECMAScript syntactic forms for defining function objects create
    //   instances of Function. There is no syntactic means to create instances of Function subclasses except
    //   for the built-in GeneratorFunction, AsyncFunction, and AsyncGeneratorFunction subclasses.

    // The Function constructor:
    //
    // * is itself a built-in function object.
    // * has a [[Prototype]] internal slot whose value is %Function.prototype%.
    // * has a "length" property whose value is 1𝔽.
    let function_constructor = create_builtin_function(
        function_constructor_function,
        true,
        1.0,
        PropertyKey::from("Function"),
        BUILTIN_FUNCTION_SLOTS,
        Some(realm.clone()),
        Some(function_prototype.clone()),
        None,
    );

    // Function.prototype
    // The value of Function.prototype is the Function prototype object.
    //
    // This property has the attributes { [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: false
    // }.
    let ppd = PotentialPropertyDescriptor::new()
        .value(function_prototype.clone())
        .clone()
        .writable(false)
        .enumerable(false)
        .configurable(false);
    define_property_or_throw(&function_constructor, "prototype", ppd).unwrap();

    // Properties of the Function Prototype Object
    macro_rules! prototype_function {
        ( $steps:expr, $name:expr, $length:expr ) => {
            let key = PropertyKey::from($name);
            let function_object = create_builtin_function(
                $steps,
                false,
                $length as f64,
                key.clone(),
                BUILTIN_FUNCTION_SLOTS,
                Some(realm.clone()),
                Some(function_prototype.clone()),
                None,
            );
            define_property_or_throw(
                &function_prototype,
                key,
                PotentialPropertyDescriptor::new()
                    .value(function_object)
                    .writable(true)
                    .enumerable(false)
                    .configurable(true),
            )
            .unwrap();
        };
    }
    prototype_function!(function_prototype_apply, "apply", 2);
    prototype_function!(function_prototype_bind, "bind", 1);
    prototype_function!(function_prototype_call, "call", 1);
    prototype_function!(function_prototype_to_string, "toString", 0);

    // Function.prototype.constructor
    // The initial value of Function.prototype.constructor is %Function%.
    let ppd = PotentialPropertyDescriptor::new()
        .value(function_constructor.clone())
        .writable(true)
        .enumerable(false)
        .configurable(true);
    define_property_or_throw(&function_prototype, "constructor", ppd).unwrap();

    let has_instance = create_builtin_function(
        function_prototype_has_instance,
        false,
        1.0,
        "[Symbol.hasInstance]".into(),
        BUILTIN_FUNCTION_SLOTS,
        Some(realm.clone()),
        Some(function_prototype.clone()),
        None,
    );
    let ppd =
        PotentialPropertyDescriptor::new().value(has_instance).writable(false).enumerable(false).configurable(false);
    define_property_or_throw(&function_prototype, wks(WksId::HasInstance), ppd).unwrap();

    realm.borrow_mut().intrinsics.function = function_constructor;
}

fn function_constructor_function(
    _this_value: &ECMAScriptValue,
    new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Function ( ...parameterArgs, bodyArg )
    // The last argument (if any) specifies the body (executable code) of a function; any preceding arguments
    // specify formal parameters.
    //
    // This function performs the following steps when called:
    //
    //  1. Let C be the active function object.
    //  2. If bodyArg is not present, set bodyArg to the empty String.
    //  3. Return ? CreateDynamicFunction(C, NewTarget, NORMAL, parameterArgs, bodyArg).
    let empty = ECMAScriptValue::from("");
    let (parameter_args, body_arg): (_, _) =
        if let [rest @ .., last] = arguments { (rest, last) } else { (&[], &empty) };
    let c = active_function_object().expect("A function should be running");
    create_dynamic_function(&c, new_target, FunctionKind::Normal, parameter_args, body_arg).map(ECMAScriptValue::Object)
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum FunctionKind {
    Normal,
    Generator,
    Async,
    AsyncGenerator,
}

fn create_dynamic_function(
    constructor: &Object,
    new_target: Option<&Object>,
    kind: FunctionKind,
    parameter_args: &[ECMAScriptValue],
    body_arg: &ECMAScriptValue,
) -> Completion<Object> {
    let new_target = new_target.unwrap_or(constructor);
    let (prefix, fallback_proto) = match kind {
        FunctionKind::Normal => ("function", IntrinsicId::FunctionPrototype),
        FunctionKind::Generator => ("function*", IntrinsicId::GeneratorFunctionPrototype),
        FunctionKind::Async => ("async function", IntrinsicId::AsyncFunctionPrototype),
        FunctionKind::AsyncGenerator => ("async function*", IntrinsicId::AsyncGeneratorFunctionPrototype),
    };
    let parameter_strings = parameter_args.iter().map(|v| to_string(v.clone())).collect::<Result<Vec<_>, _>>()?;
    let body_string = to_string(body_arg.clone())?;
    let current_realm = current_realm_record().expect("A realm should exist");
    let p = if parameter_strings.is_empty() {
        String::new()
    } else {
        let mut parts = parameter_strings[0].to_string();
        let mut strings = &parameter_strings[1..];
        while let [item, rest @ ..] = strings {
            parts.push(',');
            parts.push_str(&item.to_string());
            strings = rest;
        }
        parts
    };
    let body_parse_string = JSString::from("\n").concat(body_string).concat("\n");
    let source_string = JSString::from(prefix)
        .concat(" anonymous(")
        .concat(p.clone())
        .concat("\n) {")
        .concat(body_parse_string.clone())
        .concat("}");
    let source_text = String::from(source_string);
    let parameters = parse_text(
        &p,
        match kind {
            FunctionKind::Normal => ParseGoal::FormalParameters(YieldAllowed::No, AwaitAllowed::No),
            FunctionKind::Generator => ParseGoal::FormalParameters(YieldAllowed::Yes, AwaitAllowed::No),
            FunctionKind::Async => ParseGoal::FormalParameters(YieldAllowed::No, AwaitAllowed::Yes),
            FunctionKind::AsyncGenerator => ParseGoal::FormalParameters(YieldAllowed::Yes, AwaitAllowed::Yes),
        },
        false,
        false,
    );
    let parameters = match parameters {
        ParsedText::Errors(mut errs) => {
            return Err(AbruptCompletion::Throw { value: ECMAScriptValue::Object(errs.swap_remove(0)) });
        }
        ParsedText::FormalParameters(fp) => fp,
        _ => unreachable!(),
    };
    let body_parse_string = String::from(body_parse_string);
    let body = parse_text(
        &body_parse_string,
        match kind {
            FunctionKind::Normal => ParseGoal::FunctionBody(YieldAllowed::No, AwaitAllowed::No),
            FunctionKind::Generator => ParseGoal::GeneratorBody,
            FunctionKind::Async => ParseGoal::AsyncFunctionBody,
            FunctionKind::AsyncGenerator => ParseGoal::AsyncGeneratorBody,
        },
        false,
        false,
    );
    let body = match body {
        ParsedText::Errors(mut errs) => {
            return Err(AbruptCompletion::Throw { value: ECMAScriptValue::Object(errs.swap_remove(0)) });
        }
        ParsedText::FunctionBody(fb) => BodySource::from(fb),
        ParsedText::GeneratorBody(gb) => BodySource::from(gb),
        ParsedText::AsyncFunctionBody(afb) => BodySource::from(afb),
        ParsedText::AsyncGeneratorBody(agb) => BodySource::from(agb),
        _ => unreachable!(),
    };
    let body_contains_use_strict = body.contains_use_strict();
    let function_source = match parse_text(
        &source_text,
        match kind {
            FunctionKind::Normal => ParseGoal::FunctionExpression,
            FunctionKind::Generator => ParseGoal::GeneratorExpression,
            FunctionKind::Async => ParseGoal::AsyncFunctionExpression,
            FunctionKind::AsyncGenerator => ParseGoal::AsyncGeneratorExpression,
        },
        body_contains_use_strict,
        false,
    ) {
        ParsedText::Errors(mut errs) => {
            return Err(AbruptCompletion::Throw { value: ECMAScriptValue::Object(errs.swap_remove(0)) });
        }
        ParsedText::FunctionExpression(fe) => FunctionSource::FunctionExpression(fe),
        ParsedText::GeneratorExpression(ge) => FunctionSource::GeneratorExpression(ge),
        ParsedText::AsyncFunctionExpression(afe) => FunctionSource::AsyncFunctionExpression(afe),
        ParsedText::AsyncGeneratorExpression(age) => FunctionSource::AsyncGeneratorExpression(age),
        _ => unreachable!(),
    };
    let proto = new_target.get_prototype_from_constructor(fallback_proto)?;
    let env = current_realm.borrow().global_env.clone().expect("There should be a global environment");

    let chunk_name = nameify(&source_text, 50);
    let mut compiled = Chunk::new(chunk_name);
    let function_data = StashedFunctionData {
        source_text: source_text.clone(),
        params: ParamSource::from(parameters.clone()),
        body: body.clone(),
        strict: body_contains_use_strict,
        to_compile: function_source,
        this_mode: ThisLexicality::NonLexicalThis,
    };
    let compilation_status = match &body {
        BodySource::Function(fb) => fb.compile_body(&mut compiled, &source_text, &function_data),
        BodySource::Generator(_) => todo!(),
        BodySource::AsyncFunction(_) => todo!(),
        BodySource::AsyncGenerator(_) => todo!(),
        _ => unreachable!(),
    };
    if let Err(err) = compilation_status {
        let typeerror = create_type_error(err.to_string());
        return Err(typeerror);
    }
    for line in compiled.disassemble() {
        println!("{line}");
    }

    let f = ordinary_function_create(
        proto,
        &source_text,
        ParamSource::from(parameters),
        body,
        ThisLexicality::NonLexicalThis,
        env,
        None,
        false,
        Rc::new(compiled),
    );
    set_function_name(&f, FunctionName::from("anonymous"), None);
    match kind {
        FunctionKind::Normal => {
            make_constructor(&f, None);
        }
        FunctionKind::Generator => {
            let prototype =
                ordinary_object_create(Some(intrinsic(IntrinsicId::GeneratorFunctionPrototypePrototype)), &[]);
            define_property_or_throw(
                &f,
                "prototype",
                PotentialPropertyDescriptor::new()
                    .value(prototype)
                    .writable(true)
                    .enumerable(false)
                    .configurable(false),
            )
            .unwrap();
        }
        FunctionKind::Async => {}
        FunctionKind::AsyncGenerator => {
            let prototype =
                ordinary_object_create(Some(intrinsic(IntrinsicId::AsyncGeneratorFunctionPrototypePrototype)), &[]);
            define_property_or_throw(
                &f,
                "prototype",
                PotentialPropertyDescriptor::new()
                    .value(prototype)
                    .writable(true)
                    .enumerable(false)
                    .configurable(false),
            )
            .unwrap();
        }
    }
    Ok(f)
}

fn function_prototype_apply(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Function.prototype.apply ( thisArg, argArray )
    // This method performs the following steps when called:
    //
    //  1. Let func be the this value.
    //  2. If IsCallable(func) is false, throw a TypeError exception.
    //  3. If argArray is either undefined or null, then
    //      a. Perform PrepareForTailCall().
    //      b. Return ? Call(func, thisArg).
    //  4. Let argList be ? CreateListFromArrayLike(argArray).
    //  5. Perform PrepareForTailCall().
    //  6. Return ? Call(func, thisArg, argList).
    let mut args = FuncArgs::from(arguments);
    let this_arg = args.next_arg();
    let arg_array = args.next_arg();

    let func = this_value;
    if is_callable(func) {
        let arg_list = if arg_array == ECMAScriptValue::Undefined || arg_array == ECMAScriptValue::Null {
            vec![]
        } else {
            create_list_from_array_like(arg_array, None)?
        };
        call(func, &this_arg, &arg_list)
    } else {
        Err(create_type_error("Function.prototype.apply requires that 'this' be callable"))
    }
}

fn function_prototype_bind(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}

fn function_prototype_to_string(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Function.prototype.toString ( )
    // This method performs the following steps when called:
    //
    //  1. Let func be the this value.
    //  2. If func is an Object, func has a [[SourceText]] internal slot, func.[[SourceText]] is a sequence of
    //     Unicode code points, and HostHasSourceTextAvailable(func) is true, then
    //      a. Return CodePointsToString(func.[[SourceText]]).
    //  3. If func is a built-in function object, return an implementation-defined String source code
    //     representation of func. The representation must have the syntax of a NativeFunction. Additionally,
    //     if func has an [[InitialName]] internal slot and func.[[InitialName]] is a String, the portion of
    //     the returned String that would be matched by NativeFunctionAccessoropt PropertyName must be the
    //     value of func.[[InitialName]].
    //  4. If func is an Object and IsCallable(func) is true, return an implementation-defined String source
    //     code representation of func. The representation must have the syntax of a NativeFunction.
    //  5. Throw a TypeError exception.
    //
    // NativeFunction :
    //  function NativeFunctionAccessor[opt] PropertyName[~Yield, ~Await]opt
    //      ( FormalParameters[~Yield, ~Await] ) { [ native code ] }
    //
    // NativeFunctionAccessor :
    //  get
    //  set

    const ERRMSG: &str = "Function.prototype.toString requires that 'this' be a Function";

    match this_value {
        ECMAScriptValue::Object(obj) => {
            if let Some(func) = obj.o.to_function_obj() {
                let fdata = func.function_data().borrow();
                Ok(fdata.source_text.clone().into())
            } else if let Some(bif) = obj.o.to_builtin_function_obj() {
                let bif_data = bif.builtin_function_data().borrow();
                if let Some(initial_name) = &bif_data.initial_name {
                    Ok(format!("function {initial_name}() {{ [native code] }}").into())
                } else {
                    Ok("function () { [native code] }".into())
                }
            //} else if is_callable(&obj.into()) {
            //    Nothing currently does this (maybe bound function objects will?)
            //    Relying on test-262 to tell me when I finally build something that needs this.
            //    Ok("function () { [native code] }".into())
            } else {
                Err(create_type_error(ERRMSG))
            }
        }
        ECMAScriptValue::Undefined
        | ECMAScriptValue::Null
        | ECMAScriptValue::Boolean(_)
        | ECMAScriptValue::String(_)
        | ECMAScriptValue::Number(_)
        | ECMAScriptValue::BigInt(_)
        | ECMAScriptValue::Symbol(_) => Err(create_type_error(ERRMSG)),
    }
}

fn function_prototype_has_instance(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Function.prototype [ @@hasInstance ] ( V )
    // This method performs the following steps when called:
    //
    //  1. Let F be the this value.
    //  2. Return ? OrdinaryHasInstance(F, V).
    let mut args = FuncArgs::from(arguments);
    let v = args.next_arg();
    ordinary_has_instance(this_value, &v).map(ECMAScriptValue::from)
}

pub fn make_method(f: &dyn FunctionInterface, home_object: Object) {
    // MakeMethod ( F, homeObject )
    // The abstract operation MakeMethod takes arguments F (an ECMAScript function object) and homeObject (an Object)
    // and returns UNUSED. It configures F as a method. It performs the following steps when called:
    //
    // 1. Set F.[[HomeObject]] to homeObject.
    // 2. Return UNUSED.
    f.function_data().borrow_mut().home_object = Some(home_object);
}

#[cfg(test)]
mod tests;
