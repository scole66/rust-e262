use super::*;
use crate::parser::testhelp::*;
use ahash::AHashMap;
use itertools::Itertools;
use regex::Regex;
use std::cell::{Cell, RefCell};
use std::fmt::{self, Debug, Write as _};
use std::hash::{BuildHasher, Hash};
use std::io::Result as IoResult;
use std::io::Write;
use std::rc::Rc;
use std::sync::LazyLock;

pub struct MockWriter<T>
where
    T: Write,
{
    writer: T,
    pub count: usize,
    target: usize,
    pub error_generated: bool,
}
impl<T> std::io::Write for MockWriter<T>
where
    T: Write,
{
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.count += 1;
        if self.count >= self.target {
            self.error_generated = true;
            Err(std::io::Error::other("oh no!"))
        } else {
            self.writer.write(buf)
        }
    }
    fn flush(&mut self) -> std::io::Result<()> {
        self.writer.flush()
    }
}
impl<T> MockWriter<T>
where
    T: Write,
{
    pub fn new(writer: T, errat: usize) -> Self {
        MockWriter { writer, count: 0, target: errat, error_generated: false }
    }
}

pub fn display_error_validate(item: impl fmt::Display) {
    let mut target = 1;
    loop {
        let mut writer = MockWriter::new(Vec::new(), target);
        let result = write!(&mut writer, "{item}");
        assert!(result.is_err() || !writer.error_generated);
        if !writer.error_generated {
            break;
        }
        target += 1;
    }
}

pub fn printer_validate<U>(func: U)
where
    U: Fn(&mut MockWriter<Vec<u8>>) -> IoResult<()>,
{
    let mut target = 1;
    loop {
        let mut writer = MockWriter::new(Vec::new(), target);
        let result = func(&mut writer);
        assert!(result.is_err() || !writer.error_generated);
        if !writer.error_generated {
            break;
        }
        target += 1;
    }
}

pub fn unwind_error_object(kind: &str, err: &Object) -> String {
    assert!(err.is_error_object());
    let name = err.get(&PropertyKey::from("name")).expect("Error object was missing 'name' property");
    assert!(matches!(name, ECMAScriptValue::String(_)));
    if let ECMAScriptValue::String(name_value) = name {
        assert_eq!(name_value, kind);
    }
    let message = err.get(&PropertyKey::from("message")).expect("Error object was missing 'message' property");
    assert!(matches!(message, ECMAScriptValue::String(_)));
    match message {
        ECMAScriptValue::String(message_value) => String::from(message_value),
        _ => {
            unreachable!()
        }
    }
}

pub fn unwind_error(kind: &str, completion: AbruptCompletion) -> String {
    assert!(matches!(completion, AbruptCompletion::Throw { value: ECMAScriptValue::Object(_) }));
    match completion {
        AbruptCompletion::Throw { value: ECMAScriptValue::Object(err) } => unwind_error_object(kind, &err),
        _ => {
            unreachable!()
        }
    }
}

pub fn unwind_type_error(completion: AbruptCompletion) -> String {
    unwind_error("TypeError", completion)
}

#[expect(dead_code)]
pub fn unwind_syntax_error(completion: AbruptCompletion) -> String {
    unwind_error("SyntaxError", completion)
}

pub fn unwind_syntax_error_object(err: &Object) -> String {
    unwind_error_object("SyntaxError", err)
}

pub fn unwind_reference_error(completion: AbruptCompletion) -> String {
    unwind_error("ReferenceError", completion)
}

#[expect(dead_code)]
pub fn unwind_reference_error_object(err: &Object) -> String {
    unwind_error_object("ReferenceError", err)
}

pub fn unwind_range_error(completion: AbruptCompletion) -> String {
    unwind_error("RangeError", completion)
}

#[expect(dead_code)]
pub fn unwind_range_error_object(err: &Object) -> String {
    unwind_error_object("RangeError", err)
}

pub fn calculate_hash<F: BuildHasher, T: Hash>(factory: &F, t: &T) -> u64 {
    factory.hash_one(t)
}

pub fn setup_test_agent() {
    let sym_registry = Rc::new(RefCell::new(SymbolRegistry::new()));
    AGENT.with(|agent| {
        agent.reset();
        agent.set_global_symbol_registry(sym_registry);
    });
    initialize_host_defined_realm(0, true);
}

pub fn setup_runnable_state() {
    setup_test_agent();

    let realm = current_realm_record().unwrap();
    let global_env = realm.borrow().global_env.clone();
    let mut context = ExecutionContext::new(None, Rc::clone(&realm), None);
    context.lexical_environment = global_env.clone().map(|g| g as Rc<dyn EnvironmentRecord>);
    context.variable_environment = global_env.clone().map(|g| g as Rc<dyn EnvironmentRecord>);
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ThrowsOrNot {
    Throws,
    BehavesNormally,
}
impl From<bool> for ThrowsOrNot {
    fn from(value: bool) -> Self {
        if value { ThrowsOrNot::Throws } else { ThrowsOrNot::BehavesNormally }
    }
}

#[derive(Debug)]
pub struct TestObject {
    common: RefCell<CommonObjectData>,
    get_prototype_of: ThrowsOrNot,
    set_prototype_of: ThrowsOrNot,
    is_extensible: ThrowsOrNot,
    prevent_extensions: ThrowsOrNot,
    get_own_property: (ThrowsOrNot, Option<PropertyKey>),
    define_own_property: (ThrowsOrNot, Option<PropertyKey>),
    has_property: (ThrowsOrNot, Option<PropertyKey>),
    get: (ThrowsOrNot, Option<PropertyKey>),
    set: (ThrowsOrNot, Option<PropertyKey>),
    delete: (ThrowsOrNot, Option<PropertyKey>),
    own_property_keys: ThrowsOrNot,
}

impl<'a> From<&'a TestObject> for &'a dyn ObjectInterface {
    fn from(obj: &'a TestObject) -> Self {
        obj
    }
}

impl ObjectInterface for TestObject {
    fn common_object_data(&self) -> &RefCell<CommonObjectData> {
        &self.common
    }
    fn uses_ordinary_get_prototype_of(&self) -> bool {
        matches!(
            (self.get_prototype_of, self.set_prototype_of),
            (ThrowsOrNot::BehavesNormally, ThrowsOrNot::BehavesNormally)
        )
    }
    fn id(&self) -> usize {
        self.common.borrow().objid
    }

    fn get_prototype_of(&self) -> Completion<Option<Object>> {
        if self.get_prototype_of == ThrowsOrNot::Throws {
            Err(create_type_error("[[GetPrototypeOf]] called on TestObject"))
        } else {
            Ok(ordinary_get_prototype_of(self))
        }
    }
    fn set_prototype_of(&self, obj: Option<Object>) -> Completion<bool> {
        if self.set_prototype_of == ThrowsOrNot::Throws {
            Err(create_type_error("[[SetPrototypeOf]] called on TestObject"))
        } else {
            Ok(ordinary_set_prototype_of(self, obj))
        }
    }
    fn is_extensible(&self) -> Completion<bool> {
        if self.is_extensible == ThrowsOrNot::Throws {
            Err(create_type_error("[[IsExtensible]] called on TestObject"))
        } else {
            Ok(ordinary_is_extensible(self))
        }
    }
    fn prevent_extensions(&self) -> Completion<bool> {
        if self.prevent_extensions == ThrowsOrNot::Throws {
            Err(create_type_error("[[PreventExtensions]] called on TestObject"))
        } else {
            Ok(ordinary_prevent_extensions(self))
        }
    }
    fn get_own_property(&self, key: &PropertyKey) -> Completion<Option<PropertyDescriptor>> {
        if self.get_own_property.0 == ThrowsOrNot::Throws && self.get_own_property.1.as_ref().is_none_or(|k| *k == *key)
        {
            Err(create_type_error("[[GetOwnProperty]] called on TestObject"))
        } else {
            Ok(ordinary_get_own_property(self, key))
        }
    }
    fn define_own_property(&self, key: PropertyKey, desc: PotentialPropertyDescriptor) -> Completion<bool> {
        if self.define_own_property.0 == ThrowsOrNot::Throws
            && self.define_own_property.1.as_ref().is_none_or(|k| *k == key)
        {
            Err(create_type_error("[[DefineOwnProperty]] called on TestObject"))
        } else {
            ordinary_define_own_property(self, key, desc)
        }
    }
    fn has_property(&self, key: &PropertyKey) -> Completion<bool> {
        if self.has_property.0 == ThrowsOrNot::Throws && self.has_property.1.as_ref().is_none_or(|k| *k == *key) {
            Err(create_type_error("[[HasProperty]] called on TestObject"))
        } else {
            ordinary_has_property(self, key)
        }
    }
    fn get(&self, key: &PropertyKey, receiver: &ECMAScriptValue) -> Completion<ECMAScriptValue> {
        if self.get.0 == ThrowsOrNot::Throws && self.get.1.as_ref().is_none_or(|k| *k == *key) {
            Err(create_type_error("[[Get]] called on TestObject"))
        } else {
            ordinary_get(self, key, receiver)
        }
    }
    fn set(&self, key: PropertyKey, value: ECMAScriptValue, receiver: &ECMAScriptValue) -> Completion<bool> {
        if self.set.0 == ThrowsOrNot::Throws && self.set.1.as_ref().is_none_or(|k| *k == key) {
            Err(create_type_error("[[Set]] called on TestObject"))
        } else {
            ordinary_set(self, key, value, receiver)
        }
    }
    fn delete(&self, key: &PropertyKey) -> Completion<bool> {
        if self.delete.0 == ThrowsOrNot::Throws && self.delete.1.as_ref().is_none_or(|k| *k == *key) {
            Err(create_type_error("[[Delete]] called on TestObject"))
        } else {
            ordinary_delete(self, key)
        }
    }
    fn own_property_keys(&self) -> Completion<Vec<PropertyKey>> {
        if self.own_property_keys == ThrowsOrNot::Throws {
            Err(create_type_error("[[OwnPropertyKeys]] called on TestObject"))
        } else {
            Ok(ordinary_own_property_keys(self))
        }
    }
}

#[derive(PartialEq, Eq)]
pub enum FunctionId {
    GetPrototypeOf,
    SetPrototypeOf,
    IsExtensible,
    PreventExtensions,
    GetOwnProperty(Option<PropertyKey>),
    DefineOwnProperty(Option<PropertyKey>),
    HasProperty(Option<PropertyKey>),
    Get(Option<PropertyKey>),
    Set(Option<PropertyKey>),
    Delete(Option<PropertyKey>),
    OwnPropertyKeys,
}

macro_rules! matcher_gen {
    ( $name:ident, $ftype:ident ) => {
        fn $name(item: &FunctionId) -> (bool, Option<PropertyKey>) {
            match item {
                FunctionId::$ftype(rval) => (true, rval.clone()),
                _ => (false, None),
            }
        }
    };
}
impl TestObject {
    matcher_gen!(get_match, Get);
    matcher_gen!(set_match, Set);
    matcher_gen!(get_own_match, GetOwnProperty);
    matcher_gen!(define_own_match, DefineOwnProperty);
    matcher_gen!(has_prop_match, HasProperty);
    matcher_gen!(delete_match, Delete);
    fn check_for_key(
        matcher: fn(&FunctionId) -> (bool, Option<PropertyKey>),
        throwers: &[FunctionId],
    ) -> (ThrowsOrNot, Option<PropertyKey>) {
        for item in throwers {
            if let (true, rval) = matcher(item) {
                return (ThrowsOrNot::Throws, rval);
            }
        }
        (ThrowsOrNot::BehavesNormally, None)
    }
    pub fn new(prototype: Option<Object>, throwers: &[FunctionId]) -> Self {
        Self {
            common: RefCell::new(CommonObjectData::new(prototype, true, ORDINARY_OBJECT_SLOTS)),
            get_prototype_of: throwers.contains(&FunctionId::GetPrototypeOf).into(),
            set_prototype_of: throwers.contains(&FunctionId::SetPrototypeOf).into(),
            is_extensible: throwers.contains(&FunctionId::IsExtensible).into(),
            prevent_extensions: throwers.contains(&FunctionId::PreventExtensions).into(),
            get_own_property: TestObject::check_for_key(TestObject::get_own_match, throwers),
            define_own_property: TestObject::check_for_key(TestObject::define_own_match, throwers),
            has_property: TestObject::check_for_key(TestObject::has_prop_match, throwers),
            get: TestObject::check_for_key(TestObject::get_match, throwers),
            set: TestObject::check_for_key(TestObject::set_match, throwers),
            delete: TestObject::check_for_key(TestObject::delete_match, throwers),
            own_property_keys: throwers.contains(&FunctionId::OwnPropertyKeys).into(),
        }
    }
    pub fn object(throwers: &[FunctionId]) -> Object {
        let prototype = intrinsic(IntrinsicId::ObjectPrototype);
        Object { o: Rc::new(Self::new(Some(prototype), throwers)) }
    }
}

type GetPrototypeOfFunction = fn(this: &AdaptableObject) -> Completion<Option<Object>>;
type SetPrototypeOfFunction = fn(this: &AdaptableObject, obj: Option<Object>) -> Completion<bool>;
type IsExtensibleFunction = fn(this: &AdaptableObject) -> Completion<bool>;
type PreventExtensionsFunction = fn(this: &AdaptableObject) -> Completion<bool>;
type GetOwnPropertyFunction = fn(this: &AdaptableObject, key: &PropertyKey) -> Completion<Option<PropertyDescriptor>>;
type DefineOwnPropertyFunction =
    fn(this: &AdaptableObject, key: PropertyKey, desc: PotentialPropertyDescriptor) -> Completion<bool>;
type HasPropertyFunction = fn(this: &AdaptableObject, key: &PropertyKey) -> Completion<bool>;
type GetFunction =
    fn(this: &AdaptableObject, key: &PropertyKey, receiver: &ECMAScriptValue) -> Completion<ECMAScriptValue>;
type SetFunction = fn(
    this: &AdaptableObject,
    key: PropertyKey,
    value: ECMAScriptValue,
    receiver: &ECMAScriptValue,
) -> Completion<bool>;
type DeleteFunction = fn(this: &AdaptableObject, key: &PropertyKey) -> Completion<bool>;
type OwnPropertyKeysFunction = fn(this: &AdaptableObject) -> Completion<Vec<PropertyKey>>;

pub struct AdaptableObject {
    common: RefCell<CommonObjectData>,
    get_prototype_of_override: Option<GetPrototypeOfFunction>,
    set_prototype_of_override: Option<SetPrototypeOfFunction>,
    is_extensible_override: Option<IsExtensibleFunction>,
    prevent_extensions_override: Option<PreventExtensionsFunction>,
    get_own_property_override: Option<GetOwnPropertyFunction>,
    define_own_property_override: Option<DefineOwnPropertyFunction>,
    has_property_override: Option<HasPropertyFunction>,
    get_override: Option<GetFunction>,
    set_override: Option<SetFunction>,
    delete_override: Option<DeleteFunction>,
    own_property_keys_override: Option<OwnPropertyKeysFunction>,
    pub something: Cell<u64>, // Just a place for instances of this to hold state
}

impl fmt::Debug for AdaptableObject {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("AdaptableObject")
            .field("common", &self.common)
            .field("get_prototype_of_override", &self.get_prototype_of_override.and(Some("replacement function")))
            .field("set_prototype_of_override", &self.set_prototype_of_override.and(Some("replacement function")))
            .field("is_extensible_override", &self.is_extensible_override.and(Some("replacement function")))
            .field("prevent_extensions_override", &self.prevent_extensions_override.and(Some("replacement function")))
            .field("get_own_property_override", &self.get_own_property_override.and(Some("replacement function")))
            .field("define_own_property_override", &self.define_own_property_override.and(Some("replacement function")))
            .field("has_property_override", &self.has_property_override.and(Some("replacement function")))
            .field("get_override", &self.get_override.and(Some("replacement function")))
            .field("set_override", &self.set_override.and(Some("replacement function")))
            .field("delete_override", &self.delete_override.and(Some("replacement function")))
            .field("own_property_keys_override", &self.own_property_keys_override.and(Some("replacement function")))
            .field("something", &self.something.get())
            .finish()
    }
}

impl<'a> From<&'a AdaptableObject> for &'a dyn ObjectInterface {
    fn from(obj: &'a AdaptableObject) -> Self {
        obj
    }
}

impl ObjectInterface for AdaptableObject {
    fn common_object_data(&self) -> &RefCell<CommonObjectData> {
        &self.common
    }
    fn uses_ordinary_get_prototype_of(&self) -> bool {
        self.get_prototype_of_override.is_none() && self.set_prototype_of_override.is_none()
    }
    fn id(&self) -> usize {
        self.common.borrow().objid
    }

    fn get_prototype_of(&self) -> Completion<Option<Object>> {
        match &self.get_prototype_of_override {
            Some(func) => func(self),
            None => Ok(ordinary_get_prototype_of(self)),
        }
    }

    fn set_prototype_of(&self, obj: Option<Object>) -> Completion<bool> {
        match &self.set_prototype_of_override {
            Some(func) => func(self, obj),
            None => Ok(ordinary_set_prototype_of(self, obj)),
        }
    }
    fn is_extensible(&self) -> Completion<bool> {
        match &self.is_extensible_override {
            Some(func) => func(self),
            None => Ok(ordinary_is_extensible(self)),
        }
    }
    fn prevent_extensions(&self) -> Completion<bool> {
        match &self.prevent_extensions_override {
            Some(func) => func(self),
            None => Ok(ordinary_prevent_extensions(self)),
        }
    }
    fn get_own_property(&self, key: &PropertyKey) -> Completion<Option<PropertyDescriptor>> {
        match &self.get_own_property_override {
            Some(func) => func(self, key),
            None => Ok(ordinary_get_own_property(self, key)),
        }
    }
    fn define_own_property(&self, key: PropertyKey, desc: PotentialPropertyDescriptor) -> Completion<bool> {
        match &self.define_own_property_override {
            Some(func) => func(self, key, desc),
            None => ordinary_define_own_property(self, key, desc),
        }
    }
    fn has_property(&self, key: &PropertyKey) -> Completion<bool> {
        match &self.has_property_override {
            Some(func) => func(self, key),
            None => ordinary_has_property(self, key),
        }
    }
    fn get(&self, key: &PropertyKey, receiver: &ECMAScriptValue) -> Completion<ECMAScriptValue> {
        match &self.get_override {
            Some(func) => func(self, key, receiver),
            None => ordinary_get(self, key, receiver),
        }
    }
    fn set(&self, key: PropertyKey, value: ECMAScriptValue, receiver: &ECMAScriptValue) -> Completion<bool> {
        match &self.set_override {
            Some(func) => func(self, key, value, receiver),
            None => ordinary_set(self, key, value, receiver),
        }
    }
    fn delete(&self, key: &PropertyKey) -> Completion<bool> {
        match &self.delete_override {
            Some(func) => func(self, key),
            None => ordinary_delete(self, key),
        }
    }
    fn own_property_keys(&self) -> Completion<Vec<PropertyKey>> {
        match &self.own_property_keys_override {
            Some(func) => func(self),
            None => Ok(ordinary_own_property_keys(self)),
        }
    }
}

#[derive(Default)]
#[expect(clippy::struct_field_names)]
pub struct AdaptableMethods {
    pub get_prototype_of_override: Option<GetPrototypeOfFunction>,
    pub set_prototype_of_override: Option<SetPrototypeOfFunction>,
    pub is_extensible_override: Option<IsExtensibleFunction>,
    pub prevent_extensions_override: Option<PreventExtensionsFunction>,
    pub get_own_property_override: Option<GetOwnPropertyFunction>,
    pub define_own_property_override: Option<DefineOwnPropertyFunction>,
    pub has_property_override: Option<HasPropertyFunction>,
    pub get_override: Option<GetFunction>,
    pub set_override: Option<SetFunction>,
    pub delete_override: Option<DeleteFunction>,
    pub own_property_keys_override: Option<OwnPropertyKeysFunction>,
}

impl AdaptableObject {
    pub fn new(prototype: Option<Object>, methods: &AdaptableMethods) -> Self {
        Self {
            common: RefCell::new(CommonObjectData::new(prototype, true, ORDINARY_OBJECT_SLOTS)),
            get_prototype_of_override: methods.get_prototype_of_override,
            set_prototype_of_override: methods.set_prototype_of_override,
            is_extensible_override: methods.is_extensible_override,
            prevent_extensions_override: methods.prevent_extensions_override,
            get_own_property_override: methods.get_own_property_override,
            define_own_property_override: methods.define_own_property_override,
            has_property_override: methods.has_property_override,
            get_override: methods.get_override,
            set_override: methods.set_override,
            delete_override: methods.delete_override,
            own_property_keys_override: methods.own_property_keys_override,
            something: Cell::new(0),
        }
    }
    pub fn object(methods: &AdaptableMethods) -> Object {
        let prototype = intrinsic(IntrinsicId::ObjectPrototype);
        Object { o: Rc::new(Self::new(Some(prototype), methods)) }
    }
}

// error
pub fn faux_errors(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    Err(create_type_error("Test Sentinel"))
}

pub fn make_toprimitive_throw_obj() -> Object {
    let realm = current_realm_record().unwrap();
    let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
    let function_proto = intrinsic(IntrinsicId::FunctionPrototype);
    let target = ordinary_object_create(Some(object_prototype));
    let to_prim_sym = wks(WksId::ToPrimitive);
    let key = PropertyKey::from(to_prim_sym);
    let fcn = create_builtin_function(
        Box::new(faux_errors),
        None,
        0_f64,
        "[Symbol toPrimitive]".into(),
        BUILTIN_FUNCTION_SLOTS,
        Some(realm),
        Some(function_proto),
        None,
    );
    define_property_or_throw(
        &target,
        key,
        PotentialPropertyDescriptor::new()
            .value(ECMAScriptValue::from(fcn))
            .writable(true)
            .configurable(true)
            .enumerable(false),
    )
    .unwrap();
    target
}

use crate::object::define_property_or_throw;
use crate::realm::{Realm, create_realm};

pub fn create_named_realm(name: &str) -> Rc<RefCell<Realm>> {
    let r = create_realm(9999);
    let op = r.borrow().intrinsics.get(IntrinsicId::ObjectPrototype);
    define_property_or_throw(
        &op,
        "name",
        PotentialPropertyDescriptor::new().value(name).writable(false).enumerable(false).configurable(false),
    )
    .unwrap();

    r
}
pub fn get_realm_name() -> String {
    let op = intrinsic(IntrinsicId::ObjectPrototype);
    let name = op.get(&"name".into()).unwrap();
    to_string(name).unwrap().into()
}

pub fn serr<T>(msg: &str) -> Result<T, String> {
    Err(msg.to_string())
}

#[expect(clippy::unnecessary_wraps)]
pub fn sok<T>(msg: &str) -> Result<String, T> {
    Ok(msg.to_string())
}

#[expect(clippy::unnecessary_wraps)]
pub fn ssok<T>(msg: &str) -> Result<Option<String>, T> {
    Ok(Some(msg.to_string()))
}

#[expect(clippy::unnecessary_wraps)]
pub fn vok<T>(val: impl Into<ECMAScriptValue>) -> Result<ECMAScriptValue, T> {
    Ok(val.into())
}

pub fn disasm_filt(s: &str) -> Option<String> {
    if s.starts_with('=') {
        return None;
    }
    Some(s.split_whitespace().join(" "))
}

pub fn disasm_filter2(val: (&str, usize)) -> Option<(String, usize)> {
    let s = val.0;
    disasm_filt(s).map(|s| (s, val.1))
}

fn find_subslice<T: PartialEq>(haystack: &[T], needle: &[T]) -> Option<usize> {
    haystack.windows(needle.len()).position(|window| window == needle)
}

static JUMP_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"(JUMP.*) (-?\d+)").unwrap());

pub fn chunk_dump(outer: &Chunk, inner: &[&Chunk]) -> Vec<String> {
    let mut outer_result =
        outer.repr_with_size().iter().map(|(s, n)| (s.as_str(), *n)).filter_map(disasm_filter2).collect::<Vec<_>>();
    for (index, chunk) in inner.iter().enumerate() {
        let inner_size = chunk.pos();
        let inner_insns =
            chunk.repr_with_size().iter().map(|(s, n)| (s.as_str(), *n)).filter_map(disasm_filter2).collect::<Vec<_>>();
        let inner_count = inner_insns.len();

        // replace the part of outer_result that matches inner_insns with "<Inner Instructions <num>>"
        if let Some(replacement_index) = find_subslice(&outer_result, &inner_insns) {
            assert!(replacement_index + inner_count <= outer_result.len());
            outer_result[replacement_index] = (format!("<{} Instructions {}>", chunk.name, index + 1), inner_size);
            outer_result.drain(replacement_index + 1..replacement_index + inner_count);
        }
    }

    // For each "jump" instruction in outer_result, find its target in the outer_result array, and change the numbers to labels
    let mut jump_targets: AHashMap<isize, String> = AHashMap::new();
    let mut address = 0;
    let mut label_index = 1;

    for (insn, size) in &mut outer_result {
        if let Some(caps) = JUMP_RE.captures(insn.as_str()) {
            let target = caps.get(2).unwrap().as_str().parse::<isize>().unwrap()
                + isize::try_from(address).unwrap()
                + isize::try_from(*size).unwrap();
            let target_name = if let Some(name) = jump_targets.get(&target) {
                name.clone()
            } else {
                let name = format!("l{label_index}");
                jump_targets.insert(target, name.clone());
                label_index += 1;
                name
            };
            let new_insn = format!("{} {}", &caps[1], target_name);
            *insn = new_insn;
        }
        address += *size;
    }

    // Now we need to add the labels to the outer_result
    let mut address = 0;
    for (insn, size) in &mut outer_result {
        if let Some(name) = jump_targets.get(&address) {
            let new_insn = format!("{name}: {insn}");
            *insn = new_insn;
        }
        address += isize::try_from(*size).unwrap();
    }
    // And the final one, if needed.
    if let Some(name) = jump_targets.get(&address) {
        outer_result.push((format!("{name}:"), 0));
    }

    outer_result.into_iter().map(|(s, _)| s).collect::<Vec<_>>()
}

#[derive(Debug, PartialEq)]
pub struct IdealizedPropertyDescriptor {
    pub configurable: bool,
    pub enumerable: bool,
    pub writable: Option<bool>,
    pub value: Option<ECMAScriptValue>,
    pub get: Option<ECMAScriptValue>,
    pub set: Option<ECMAScriptValue>,
}

impl From<PropertyDescriptor> for IdealizedPropertyDescriptor {
    fn from(pd: PropertyDescriptor) -> Self {
        let (writable, value, get, set) = match pd.property {
            PropertyKind::Data(dp) => (Some(dp.writable), Some(dp.value), None, None),
            PropertyKind::Accessor(ap) => (None, None, Some(ap.get), Some(ap.set)),
        };
        IdealizedPropertyDescriptor {
            configurable: pd.configurable,
            enumerable: pd.enumerable,
            writable,
            value,
            get,
            set,
        }
    }
}

#[macro_export]
macro_rules! tbd_function {
    ( $name:ident ) => {
        #[test]
        #[should_panic(expected = "not yet implemented")]
        fn $name() {
            setup_test_agent();
            super::$name(&ECMAScriptValue::Undefined, None, &[]).unwrap();
        }
    };
}
#[macro_export]
macro_rules! false_function {
    ( $name:ident ) => {
        #[test]
        fn $name() {
            setup_test_agent();
            let obj = make();
            assert!(!obj.o.$name());
        }
    };
}
#[macro_export]
macro_rules! none_function {
    ( $name:ident ) => {
        #[test]
        fn $name() {
            setup_test_agent();
            let obj = make();
            assert!(obj.o.$name().is_none());
        }
    };
}
#[macro_export]
macro_rules! default_get_prototype_of_test {
    ( $proto:ident ) => {
        #[test]
        fn get_prototype_of() {
            setup_test_agent();
            let obj = make();
            let proto = obj.o.get_prototype_of().unwrap().unwrap();
            assert_eq!(proto, intrinsic(IntrinsicId::$proto));
        }
    };
}
#[macro_export]
macro_rules! default_set_prototype_of_test {
    () => {
        #[test]
        fn set_prototype_of() {
            setup_test_agent();
            let obj = make();
            let res = obj.o.set_prototype_of(None).unwrap();
            assert!(res);
            assert!(obj.o.get_prototype_of().unwrap().is_none());
        }
    };
}
#[macro_export]
macro_rules! default_is_extensible_test {
    () => {
        #[test]
        fn is_extensible() {
            setup_test_agent();
            let obj = make();
            let res = obj.o.is_extensible().unwrap();
            assert!(res);
        }
    };
}
#[macro_export]
macro_rules! default_prevent_extensions_test {
    () => {
        #[test]
        fn prevent_extensions() {
            setup_test_agent();
            let obj = make();
            let res = obj.o.prevent_extensions().unwrap();
            assert!(res);
            assert!(!obj.o.is_extensible().unwrap());
        }
    };
}
#[macro_export]
macro_rules! default_delete_test {
    () => {
        #[test]
        fn delete() {
            setup_test_agent();
            let obj = make();
            let res = obj.o.delete(&PropertyKey::from("rust")).unwrap();
            assert_eq!(res, true);
        }
    };
}
#[macro_export]
macro_rules! default_id_test {
    () => {
        #[test]
        fn id() {
            setup_test_agent();
            let obj = make();
            let obj2 = make();
            assert_ne!(obj.o.id(), obj2.o.id());
        }
    };
}
#[macro_export]
macro_rules! default_has_property_test {
    () => {
        #[test]
        fn has_property() {
            setup_test_agent();
            let obj = make();
            let res = obj.o.has_property(&PropertyKey::from("test_sentinel")).unwrap();
            assert_eq!(res, false);
            obj.o
                .define_own_property(
                    "test_sentinel".into(),
                    PotentialPropertyDescriptor::new()
                        .value("present")
                        .writable(true)
                        .enumerable(true)
                        .configurable(true),
                )
                .unwrap();
            let res2 = obj.o.has_property(&PropertyKey::from("test_sentinel")).unwrap();
            assert_eq!(res2, true);
        }
    };
}
#[macro_export]
macro_rules! default_uses_ordinary_get_prototype_of_test {
    () => {
        #[test]
        fn uses_ordinary_get_prototype_of() {
            setup_test_agent();
            let obj = make();
            assert!(obj.o.uses_ordinary_get_prototype_of());
        }
    };
}
#[macro_export]
macro_rules! default_get_own_property_test {
    () => {
        #[test_case::test_case("test_sentinel" => Some(IdealizedPropertyDescriptor{configurable: true, enumerable: true, writable: Some(true), value: Some(ECMAScriptValue::from("present")), get: None, set: None}); "key present")]
        #[test_case("color" => None; "key not present")]
        fn get_own_property(key: &str) -> Option<IdealizedPropertyDescriptor> {
            setup_test_agent();
            let obj = make();
            obj.o
                .define_own_property(

                    "test_sentinel".into(),
                    PotentialPropertyDescriptor::new().value("present").writable(true).enumerable(true).configurable(true),
                )
                .unwrap();
            obj.o.get_own_property(& &key.into()).unwrap().map(IdealizedPropertyDescriptor::from)
        }
    }
}
#[macro_export]
macro_rules! default_define_own_property_test {
    () => {
        use ahash::AHashMap;

        #[test_case::test_case(
                            PotentialPropertyDescriptor::new()
                                .value(ECMAScriptValue::from(67))
                                .writable(true)
                                .configurable(true)
                                .enumerable(true),
                            "sixty-seven"
                            => (
                                true,
                                [
                                    (
                                        PropertyKey::from("sixty-seven"),
                                        IdealizedPropertyDescriptor {
                                            configurable: true,
                                            enumerable: true,
                                            writable: Some(true),
                                            value: Some(ECMAScriptValue::from(67)),
                                            get: None,
                                            set: None
                                        }
                                    )
                                    ].into_iter().collect::<AHashMap<PropertyKey, IdealizedPropertyDescriptor>>()
                                ); "ordinary property"
                        )]
        fn define_own_property(
            new_value: PotentialPropertyDescriptor,
            key: &str,
        ) -> (bool, AHashMap<PropertyKey, IdealizedPropertyDescriptor>) {
            setup_test_agent();
            let obj = make();

            let success = obj.o.define_own_property(key.into(), new_value).unwrap();
            let properties = obj
                .o
                .common_object_data()
                .borrow()
                .properties
                .iter()
                .map(|(a, b)| (a.clone(), IdealizedPropertyDescriptor::from(b.clone())))
                .collect::<AHashMap<_, _>>();
            (success, properties)
        }
    };
}
#[macro_export]
macro_rules! default_get_test {
    ( $key_on_proto:expr_2021, $val_on_proto:expr_2021 ) => {
        #[test_case::test_case(|| "test_sentinel".into() => ECMAScriptValue::from("present"); "exists")]
        #[test_case::test_case(|| "friendliness".into() => ECMAScriptValue::Undefined; "doesn't exist")]
        #[test_case::test_case($key_on_proto => $val_on_proto; "from prototype")]
        fn get(make_key: impl FnOnce() -> PropertyKey) -> ECMAScriptValue {
            setup_test_agent();
            let obj = make();
            let key = make_key();
            obj.o
                .define_own_property(
                    "test_sentinel".into(),
                    PotentialPropertyDescriptor::new()
                        .value("present")
                        .writable(true)
                        .enumerable(true)
                        .configurable(true),
                )
                .unwrap();

            let receiver = ECMAScriptValue::from(obj.clone());
            obj.o.get(&key, &receiver).unwrap()
        }
    };
}
#[macro_export]
macro_rules! default_set_test {
    () => {
        #[test_case::test_case(
                            776, "numberly"
                            => (
                                true,
                                [
                                    (
                                        PropertyKey::from("numberly"),
                                        IdealizedPropertyDescriptor {
                                            configurable: true,
                                            enumerable: true,
                                            writable: Some(true),
                                            value: Some(ECMAScriptValue::from(776)),
                                            get: None,
                                            set: None
                                        }
                                    )
                                    ].into_iter().collect::<AHashMap<PropertyKey, IdealizedPropertyDescriptor>>()
                            ); "ordinary set"
                        )]
        fn set(
            new_val: impl Into<ECMAScriptValue>,
            key: impl Into<PropertyKey>,
        ) -> (bool, AHashMap<PropertyKey, IdealizedPropertyDescriptor>) {
            setup_test_agent();
            let obj = make();
            let receiver = ECMAScriptValue::Object(obj.clone());
            let success = obj.o.set(key.into(), new_val.into(), &receiver).unwrap();
            let properties = obj
                .o
                .common_object_data()
                .borrow()
                .properties
                .iter()
                .map(|(a, b)| (a.clone(), IdealizedPropertyDescriptor::from(b.clone())))
                .collect::<AHashMap<_, _>>();
            (success, properties)
        }
    };
}
#[macro_export]
macro_rules! default_kind_test {
    () => {
        #[test]
        fn kind() {
            setup_test_agent();
            let obj = make();
            assert_eq!(obj.o.kind(), ObjectTag::Object);
        }
    };
}
#[macro_export]
macro_rules! default_own_property_keys_test {
    () => {
        #[test]
        fn own_property_keys() {
            setup_test_agent();
            let obj = make();

            let to_prim = wks(WksId::ToPrimitive);
            let species = wks(WksId::Species);

            obj.o
                .define_own_property(
                    "60".into(),
                    PotentialPropertyDescriptor::new().value("q").writable(true).enumerable(true).configurable(true),
                )
                .unwrap();
            obj.o
                .define_own_property(
                    "6".into(),
                    PotentialPropertyDescriptor::new().value("s").writable(true).enumerable(true).configurable(true),
                )
                .unwrap();
            obj.o
                .define_own_property(
                    "zebra".into(),
                    PotentialPropertyDescriptor::new().value(0).writable(true).enumerable(true).configurable(true),
                )
                .unwrap();
            obj.o
                .define_own_property(
                    "alpha".into(),
                    PotentialPropertyDescriptor::new().value(1).writable(true).enumerable(true).configurable(true),
                )
                .unwrap();
            obj.o
                .define_own_property(
                    to_prim.clone().into(),
                    PotentialPropertyDescriptor::new().value(2).writable(true).enumerable(true).configurable(true),
                )
                .unwrap();
            obj.o
                .define_own_property(
                    species.clone().into(),
                    PotentialPropertyDescriptor::new().value(3).writable(true).enumerable(true).configurable(true),
                )
                .unwrap();

            let keys = obj.o.own_property_keys().unwrap();

            assert_eq!(
                keys,
                vec!["6".into(), "60".into(), "zebra".into(), "alpha".into(), to_prim.into(), species.into()]
            );
        }
    };
}

pub fn data_validation(
    pd: PropertyDescriptor,
    writable: bool,
    enumerable: bool,
    configurable: bool,
) -> ECMAScriptValue {
    assert_eq!(pd.enumerable, enumerable);
    assert_eq!(pd.configurable, configurable);
    match pd.property {
        PropertyKind::Data(DataProperty { value, writable: pd_writable }) => {
            assert_eq!(pd_writable, writable);
            value
        }
        PropertyKind::Accessor(_) => {
            panic!("Expected data property but found accessor property");
        }
    }
}

pub fn func_validation(
    pd: PropertyDescriptor,
    name: impl Into<ECMAScriptValue>,
    length: impl Into<ECMAScriptValue>,
) -> ECMAScriptValue {
    let func = data_validation(pd, true, false, true);
    assert_eq!(func.get(&"name".into()).unwrap(), name.into());
    assert_eq!(func.get(&"length".into()).unwrap(), length.into());
    func
}

pub fn getter_validation(
    pd: PropertyDescriptor,
    name: impl Into<ECMAScriptValue>,
    length: impl Into<ECMAScriptValue>,
) -> ECMAScriptValue {
    assert!(!pd.enumerable);
    assert!(pd.configurable);
    match pd.property {
        PropertyKind::Accessor(AccessorProperty { get, set }) => {
            assert!(set.is_undefined());
            assert_eq!(get.get(&"name".into()).unwrap(), name.into());
            assert_eq!(get.get(&"length".into()).unwrap(), length.into());
            get
        }
        PropertyKind::Data(_) => {
            panic!("Expected accessor property but found data property");
        }
    }
}

impl ECMAScriptValue {
    pub fn test_result_string(&self) -> String {
        match self {
            ECMAScriptValue::Undefined
            | ECMAScriptValue::Null
            | ECMAScriptValue::Boolean(_)
            | ECMAScriptValue::String(_)
            | ECMAScriptValue::Number(_)
            | ECMAScriptValue::BigInt(_)
            | ECMAScriptValue::Symbol(_) => format!("{self}"),
            ECMAScriptValue::Object(o) => {
                let keys = ordinary_own_property_keys(o);
                let mut r = String::new();
                let mut first = true;
                for key in keys {
                    let value = o.get(&key);
                    if first {
                        first = false;
                    } else {
                        r.push(',');
                    }
                    let named_function_was_added = match value.as_ref() {
                        Ok(v) => {
                            if is_callable(v) {
                                let name = to_string(
                                    to_object(v.clone())
                                        .unwrap()
                                        .get(&"name".into())
                                        .unwrap_or(ECMAScriptValue::Undefined),
                                )
                                .unwrap_or_else(|_| JSString::from("undefined"));
                                if name == "undefined" {
                                    false
                                } else {
                                    write!(r, "{key}:function {name}").unwrap();
                                    true
                                }
                            } else {
                                false
                            }
                        }
                        _ => false,
                    };
                    if !named_function_was_added {
                        let value_str = value.map_or_else(unwind_any_error, |val| format!("{val}"));
                        write!(r, "{key}:{value_str}").unwrap();
                    }
                }
                r
            }
        }
    }
}

// This is really for debugging. It's the output structure from propdump.
#[derive(Debug, PartialEq)]
pub enum PropertyInfoKind {
    Accessor { getter: ECMAScriptValue, setter: ECMAScriptValue },
    Data { value: ECMAScriptValue, writable: bool },
}
#[derive(Debug, PartialEq)]
pub struct PropertyInfo {
    pub name: PropertyKey,
    pub enumerable: bool,
    pub configurable: bool,
    pub kind: PropertyInfoKind,
}

impl fmt::Display for PropertyInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:", to_string(ECMAScriptValue::from(self.name.clone())).unwrap())?;
        match &self.kind {
            PropertyInfoKind::Accessor { getter, setter } => write!(f, "[[Get]]: {getter}, [[Set]]: {setter} ("),
            PropertyInfoKind::Data { value, writable } => write!(f, "{} ({}", value, if *writable { 'w' } else { '-' }),
        }?;
        write!(f, "{}{})", if self.enumerable { 'e' } else { '-' }, if self.configurable { 'c' } else { '-' })
    }
}

impl CommonObjectData {
    pub fn propdump(&self) -> Vec<PropertyInfo> {
        // Dump the properties as a simplified data structure, in a reproducable way. For testing, mostly.
        // (Allows for Eq style tests, heedless of the internal structure of a property descriptor; also sorted in order of addition to object.)
        let mut keys: Vec<&PropertyKey> = self.properties.keys().collect();
        keys.sort_by_cached_key(|a| self.properties.get(*a).unwrap().spot);
        let mut result = vec![];
        for key in keys {
            let prop = self.properties.get(key).unwrap();
            result.push(PropertyInfo {
                name: key.clone(),
                enumerable: prop.enumerable,
                configurable: prop.configurable,
                kind: match &prop.property {
                    PropertyKind::Data(DataProperty { value, writable }) => {
                        PropertyInfoKind::Data { value: value.clone(), writable: *writable }
                    }
                    PropertyKind::Accessor(AccessorProperty { get, set }) => {
                        PropertyInfoKind::Accessor { getter: get.clone(), setter: set.clone() }
                    }
                },
            });
        }
        result
    }
}

mod test_object {
    use super::*;

    fn make() -> Object {
        TestObject::object(&[])
    }

    false_function!(is_proxy_object);
    false_function!(is_symbol_object);
    none_function!(to_proxy_object);
    none_function!(to_symbol_obj);
}

mod adaptable_object {
    use super::*;

    fn make() -> Object {
        AdaptableObject::object(&AdaptableMethods::default())
    }

    false_function!(is_proxy_object);
    false_function!(is_symbol_object);
    none_function!(to_proxy_object);
    none_function!(to_symbol_obj);
}

pub fn make_fer(src: &str, new_target: Option<Object>) -> (Object, FunctionEnvironmentRecord) {
    let closure = make_ecmascript_function(src);
    (closure.clone(), FunctionEnvironmentRecord::new(closure, new_target, "environment_tag".to_string()))
}

pub fn make_ecmascript_function(src: &str) -> Object {
    let ae = Maker::new(src).assignment_expression();
    let this_mode = if ae.contains(ParseNodeKind::ArrowFunction) || ae.contains(ParseNodeKind::AsyncArrowFunction) {
        ThisLexicality::LexicalThis
    } else {
        ThisLexicality::NonLexicalThis
    };
    let node = ae.function_definition().unwrap();
    let params = node.params();
    let body = node.body();

    let realm = current_realm_record().unwrap();
    let global_env = realm.borrow().global_env.clone().unwrap();
    let function_prototype = intrinsic(IntrinsicId::FunctionPrototype);
    let chunk = Rc::new(Chunk::new("empty"));
    ordinary_function_create(function_prototype, src, params, body, this_mode, global_env, None, true, chunk)
}

pub mod integration;
