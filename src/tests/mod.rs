use super::*;
use itertools::Itertools;
use std::cell::{Cell, RefCell};
use std::fmt::{self, Debug};
use std::hash::{BuildHasher, Hash, Hasher};
use std::io::Result as IoResult;
use std::io::Write;
use std::rc::Rc;

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
            Err(std::io::Error::new(std::io::ErrorKind::Other, "oh no!"))
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

pub fn unwind_error_object(kind: &str, err: Object) -> String {
    assert!(err.o.to_error_obj().is_some());
    let name = err.get(&PropertyKey::from("name")).expect("Error object was missing 'name' property");
    assert!(matches!(name, ECMAScriptValue::String(_)));
    if let ECMAScriptValue::String(name_value) = name {
        assert_eq!(name_value, kind);
    }
    let message = err.get(&PropertyKey::from("message")).expect("Error object was missing 'message' property");
    assert!(matches!(message, ECMAScriptValue::String(_)));
    if let ECMAScriptValue::String(message_value) = message {
        String::from(message_value)
    } else {
        unreachable!()
    }
}

pub fn unwind_error(kind: &str, completion: AbruptCompletion) -> String {
    assert!(matches!(completion, AbruptCompletion::Throw { value: ECMAScriptValue::Object(_) }));
    if let AbruptCompletion::Throw { value: ECMAScriptValue::Object(err) } = completion {
        unwind_error_object(kind, err)
    } else {
        unreachable!()
    }
}

pub fn unwind_type_error(completion: AbruptCompletion) -> String {
    unwind_error("TypeError", completion)
}

pub fn unwind_syntax_error(completion: AbruptCompletion) -> String {
    unwind_error("SyntaxError", completion)
}

pub fn unwind_syntax_error_object(err: Object) -> String {
    unwind_error_object("SyntaxError", err)
}

pub fn unwind_reference_error(completion: AbruptCompletion) -> String {
    unwind_error("ReferenceError", completion)
}

pub fn unwind_reference_error_object(err: Object) -> String {
    unwind_error_object("ReferenceError", err)
}

pub fn unwind_range_error(completion: AbruptCompletion) -> String {
    unwind_error("RangeError", completion)
}

pub fn unwind_range_error_object(err: Object) -> String {
    unwind_error_object("RangeError", err)
}

pub fn calculate_hash<F: BuildHasher, T: Hash>(factory: &F, t: &T) -> u64 {
    let mut s = factory.build_hasher();
    t.hash(&mut s);
    s.finish()
}

pub fn setup_test_agent() {
    let sym_registry = Rc::new(RefCell::new(SymbolRegistry::new()));
    AGENT.with(|agent| {
        agent.reset();
        agent.set_global_symbol_registry(sym_registry);
    });
    initialize_host_defined_realm(true);
}

#[derive(Debug)]
pub struct TestObject {
    common: RefCell<CommonObjectData>,
    get_prototype_of_throws: bool,
    set_prototype_of_throws: bool,
    is_extensible_throws: bool,
    prevent_extensions_throws: bool,
    get_own_property_throws: (bool, Option<PropertyKey>),
    define_own_property_throws: (bool, Option<PropertyKey>),
    has_property_throws: (bool, Option<PropertyKey>),
    get_throws: (bool, Option<PropertyKey>),
    set_throws: (bool, Option<PropertyKey>),
    delete_throws: (bool, Option<PropertyKey>),
    own_property_keys_throws: bool,
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
        !(self.get_prototype_of_throws || self.set_prototype_of_throws)
    }
    fn id(&self) -> usize {
        self.common.borrow().objid
    }

    fn get_prototype_of(&self) -> Completion<Option<Object>> {
        if self.get_prototype_of_throws {
            Err(create_type_error("[[GetPrototypeOf]] called on TestObject"))
        } else {
            Ok(ordinary_get_prototype_of(self))
        }
    }
    fn set_prototype_of(&self, obj: Option<Object>) -> Completion<bool> {
        if self.set_prototype_of_throws {
            Err(create_type_error("[[SetPrototypeOf]] called on TestObject"))
        } else {
            Ok(ordinary_set_prototype_of(self, obj))
        }
    }
    fn is_extensible(&self) -> Completion<bool> {
        if self.is_extensible_throws {
            Err(create_type_error("[[IsExtensible]] called on TestObject"))
        } else {
            Ok(ordinary_is_extensible(self))
        }
    }
    fn prevent_extensions(&self) -> Completion<bool> {
        if self.prevent_extensions_throws {
            Err(create_type_error("[[PreventExtensions]] called on TestObject"))
        } else {
            Ok(ordinary_prevent_extensions(self))
        }
    }
    fn get_own_property(&self, key: &PropertyKey) -> Completion<Option<PropertyDescriptor>> {
        if self.get_own_property_throws.0 && self.get_own_property_throws.1.as_ref().map_or(true, |k| *k == *key) {
            Err(create_type_error("[[GetOwnProperty]] called on TestObject"))
        } else {
            Ok(ordinary_get_own_property(self, key))
        }
    }
    fn define_own_property(&self, key: PropertyKey, desc: PotentialPropertyDescriptor) -> Completion<bool> {
        if self.define_own_property_throws.0 && self.define_own_property_throws.1.as_ref().map_or(true, |k| *k == key) {
            Err(create_type_error("[[DefineOwnProperty]] called on TestObject"))
        } else {
            ordinary_define_own_property(self, key, desc)
        }
    }
    fn has_property(&self, key: &PropertyKey) -> Completion<bool> {
        if self.has_property_throws.0 && self.has_property_throws.1.as_ref().map_or(true, |k| *k == *key) {
            Err(create_type_error("[[HasProperty]] called on TestObject"))
        } else {
            ordinary_has_property(self, key)
        }
    }
    fn get(&self, key: &PropertyKey, receiver: &ECMAScriptValue) -> Completion<ECMAScriptValue> {
        if self.get_throws.0 && self.get_throws.1.as_ref().map_or(true, |k| *k == *key) {
            Err(create_type_error("[[Get]] called on TestObject"))
        } else {
            ordinary_get(self, key, receiver)
        }
    }
    fn set(&self, key: PropertyKey, value: ECMAScriptValue, receiver: &ECMAScriptValue) -> Completion<bool> {
        if self.set_throws.0 && self.set_throws.1.as_ref().map_or(true, |k| *k == key) {
            Err(create_type_error("[[Set]] called on TestObject"))
        } else {
            ordinary_set(self, key, value, receiver)
        }
    }
    fn delete(&self, key: &PropertyKey) -> Completion<bool> {
        if self.delete_throws.0 && self.delete_throws.1.as_ref().map_or(true, |k| *k == *key) {
            Err(create_type_error("[[Delete]] called on TestObject"))
        } else {
            ordinary_delete(self, key)
        }
    }
    fn own_property_keys(&self) -> Completion<Vec<PropertyKey>> {
        if self.own_property_keys_throws {
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
    ) -> (bool, Option<PropertyKey>) {
        for item in throwers {
            match matcher(item) {
                (true, rval) => {
                    return (true, rval);
                }
                (false, _) => {}
            }
        }
        (false, None)
    }
    pub fn new(prototype: Option<Object>, throwers: &[FunctionId]) -> Self {
        Self {
            common: RefCell::new(CommonObjectData::new(prototype, true, ORDINARY_OBJECT_SLOTS)),
            get_prototype_of_throws: throwers.contains(&FunctionId::GetPrototypeOf),
            set_prototype_of_throws: throwers.contains(&FunctionId::SetPrototypeOf),
            is_extensible_throws: throwers.contains(&FunctionId::IsExtensible),
            prevent_extensions_throws: throwers.contains(&FunctionId::PreventExtensions),
            get_own_property_throws: TestObject::check_for_key(TestObject::get_own_match, throwers),
            define_own_property_throws: TestObject::check_for_key(TestObject::define_own_match, throwers),
            has_property_throws: TestObject::check_for_key(TestObject::has_prop_match, throwers),
            get_throws: TestObject::check_for_key(TestObject::get_match, throwers),
            set_throws: TestObject::check_for_key(TestObject::set_match, throwers),
            delete_throws: TestObject::check_for_key(TestObject::delete_match, throwers),
            own_property_keys_throws: throwers.contains(&FunctionId::OwnPropertyKeys),
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
    pub fn new(prototype: Option<Object>, methods: AdaptableMethods) -> Self {
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
    pub fn object(methods: AdaptableMethods) -> Object {
        let prototype = intrinsic(IntrinsicId::ObjectPrototype);
        Object { o: Rc::new(Self::new(Some(prototype), methods)) }
    }
}

// error
pub fn faux_errors(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    Err(create_type_error("Test Sentinel"))
}

pub fn make_toprimitive_throw_obj() -> Object {
    let realm = current_realm_record().unwrap();
    let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
    let function_proto = intrinsic(IntrinsicId::FunctionPrototype);
    let target = ordinary_object_create(Some(object_prototype), &[]);
    let to_prim_sym = wks(WksId::ToPrimitive);
    let key = PropertyKey::from(to_prim_sym);
    let fcn = create_builtin_function(
        faux_errors,
        false,
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
use crate::realm::{create_realm, Realm};

pub fn create_named_realm(name: &str) -> Rc<RefCell<Realm>> {
    let r = create_realm();
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

pub fn sok<T>(msg: &str) -> Result<String, T> {
    Ok(msg.to_string())
}

pub fn ssok<T>(msg: &str) -> Result<Option<String>, T> {
    Ok(Some(msg.to_string()))
}

pub fn vok<T>(val: impl Into<ECMAScriptValue>) -> Result<ECMAScriptValue, T> {
    Ok(val.into())
}

pub fn disasm_filt(s: String) -> Option<String> {
    if s.starts_with('=') {
        return None;
    }
    Some(s.split_whitespace().join(" "))
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
            super::$name(ECMAScriptValue::Undefined, None, &[]).unwrap();
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
    ( $key_on_proto:expr, $val_on_proto:expr ) => {
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
    if let PropertyKind::Data(DataProperty { value, writable: pd_writable }) = pd.property {
        assert_eq!(pd_writable, writable);
        value
    } else {
        panic!("Expected data property but found accessor property");
    }
}

pub fn func_validation(
    pd: PropertyDescriptor,
    name: impl Into<ECMAScriptValue>,
    length: impl Into<ECMAScriptValue>,
) -> ECMAScriptValue {
    let func = data_validation(pd, true, false, true);
    assert_eq!(getv(&func, &"name".into()).unwrap(), name.into());
    assert_eq!(getv(&func, &"length".into()).unwrap(), length.into());
    func
}

pub fn getter_validation(
    pd: PropertyDescriptor,
    name: impl Into<ECMAScriptValue>,
    length: impl Into<ECMAScriptValue>,
) -> ECMAScriptValue {
    assert!(!pd.enumerable);
    assert!(pd.configurable);
    if let PropertyKind::Accessor(AccessorProperty { get, set }) = pd.property {
        assert!(set.is_undefined());
        assert_eq!(getv(&get, &"name".into()).unwrap(), name.into());
        assert_eq!(getv(&get, &"length".into()).unwrap(), length.into());
        get
    } else {
        panic!("Expected accessor property but found data property");
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
                    if !first {
                        r.push(',');
                    } else {
                        first = false;
                    }
                    let value_str = value.map(|val| format!("{val}")).unwrap_or_else(unwind_any_error);
                    r.push_str(&format!("{key}:{value_str}"));
                }
                r
            }
        }
    }
}

pub fn which_intrinsic(to_be_checked: &Object) -> Option<IntrinsicId> {
    let realm_ref = current_realm_record().unwrap();
    let realm = realm_ref.borrow();
    realm.intrinsics.which(to_be_checked)
}

mod integration;
