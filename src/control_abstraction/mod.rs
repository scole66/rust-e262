use super::*;
use futures::future::LocalBoxFuture;
use genawaiter::{
    rc::{Co, Gen},
    Coroutine,
};
use std::cell::Cell;
use std::cell::RefCell;
use std::error::Error;
use std::fmt;
use std::future::Future;
use std::pin::Pin;

pub fn provision_iterator_prototype(realm: &Rc<RefCell<Realm>>) {
    let object_prototype = realm.borrow().intrinsics.object_prototype.clone();
    let function_prototype = realm.borrow().intrinsics.function_prototype.clone();

    // The %IteratorPrototype% Object
    //
    // The %IteratorPrototype% object:
    //
    //  * has a [[Prototype]] internal slot whose value is %Object.prototype%.
    //  * is an ordinary object.
    //
    // NOTE All objects defined in this specification that implement the Iterator interface also inherit
    // from %IteratorPrototype%. ECMAScript code may also define objects that inherit from
    // %IteratorPrototype%. The %IteratorPrototype% object provides a place where additional methods that
    // are applicable to all iterator objects may be added.
    //
    // The following expression is one way that ECMAScript code can access the %IteratorPrototype% object:
    //
    //      Object.getPrototypeOf(Object.getPrototypeOf([][Symbol.iterator]()))

    let iterator_prototype = ordinary_object_create(Some(object_prototype));

    // Prototype Function Properties
    macro_rules! prototype_function {
        ( $steps:expr, $name:expr, $length:expr ) => {
            let key = PropertyKey::from($name);
            let function_object = create_builtin_function(
                Box::new($steps),
                None,
                $length,
                key.clone(),
                BUILTIN_FUNCTION_SLOTS,
                Some(realm.clone()),
                Some(function_prototype.clone()),
                None,
            );
            define_property_or_throw(
                &iterator_prototype,
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
    prototype_function!(iterator_prototype_iterator, wks(WksId::Iterator), 0.0);

    realm.borrow_mut().intrinsics.iterator_prototype = iterator_prototype;
}

pub fn provision_generator_function_intrinsics(realm: &Rc<RefCell<Realm>>) {
    let function = realm.borrow().intrinsics.function.clone();
    let function_prototype = realm.borrow().intrinsics.function_prototype.clone();
    let iterator_prototype = realm.borrow().intrinsics.iterator_prototype.clone();

    // The GeneratorFunction Constructor
    //
    // The GeneratorFunction constructor:
    //
    //  * is %GeneratorFunction%.
    //  * is a subclass of Function.
    //  * creates and initializes a new GeneratorFunction when called as a function rather than as a
    //    constructor. Thus the function call GeneratorFunction (…) is equivalent to the object creation
    //    expression new GeneratorFunction (…) with the same arguments.
    //  * may be used as the value of an extends clause of a class definition. Subclass constructors that
    //    intend to inherit the specified GeneratorFunction behaviour must include a super call to the
    //    GeneratorFunction constructor to create and initialize subclass instances with the internal
    //    slots necessary for built-in GeneratorFunction behaviour. All ECMAScript syntactic forms for
    //    defining generator function objects create direct instances of GeneratorFunction. There is no
    //    syntactic means to create instances of GeneratorFunction subclasses.

    // Properties of the GeneratorFunction Constructor
    //
    // The GeneratorFunction constructor:
    //
    //  * is a standard built-in function object that inherits from the Function constructor.
    //  * has a [[Prototype]] internal slot whose value is %Function%.
    //  * has a "name" property whose value is "GeneratorFunction".
    let generator_function_constructor = create_builtin_function(
        Box::new(generator_function),
        Some(ConstructorKind::Base),
        1.0,
        "GeneratorFunction".into(),
        BUILTIN_FUNCTION_SLOTS,
        Some(realm.clone()),
        Some(function),
        None,
    );

    macro_rules! constructor_data {
        ( $name:expr, $value:expr, $writable:expr, $enumerable:expr, $configurable:expr ) => {
            define_property_or_throw(
                &generator_function_constructor,
                $name,
                PotentialPropertyDescriptor::new()
                    .value(ECMAScriptValue::from($value))
                    .writable($writable)
                    .enumerable($enumerable)
                    .configurable($configurable),
            )
            .unwrap();
        };
    }

    // Properties of the GeneratorFunction Prototype Object
    //
    // The GeneratorFunction prototype object:
    //
    //  * is %GeneratorFunction.prototype% (see Figure 6).
    //  * is an ordinary object.
    //  * is not a function object and does not have an [[ECMAScriptCode]] internal slot or any other of
    //    the internal slots listed in Table 33 or Table 86.
    //  * has a [[Prototype]] internal slot whose value is %Function.prototype%.
    let generator_function_prototype = ordinary_object_create(Some(function_prototype.clone()));

    macro_rules! gf_prototype_data {
        ( $name:expr, $value:expr, $writable:expr, $enumerable:expr, $configurable:expr ) => {
            define_property_or_throw(
                &generator_function_prototype,
                $name,
                PotentialPropertyDescriptor::new()
                    .value(ECMAScriptValue::from($value))
                    .writable($writable)
                    .enumerable($enumerable)
                    .configurable($configurable),
            )
            .unwrap();
        };
    }
    let to_string_tag = wks(WksId::ToStringTag);
    gf_prototype_data!("constructor", generator_function_constructor.clone(), false, false, true);
    gf_prototype_data!(to_string_tag.clone(), "GeneratorFunction", false, false, true);
    constructor_data!("prototype", generator_function_prototype.clone(), false, false, false);

    // Properties of the Generator Prototype Object
    //
    // The Generator prototype object:
    //
    //  * is %GeneratorFunction.prototype.prototype%.
    //  * is an ordinary object.
    //  * is not a Generator instance and does not have a [[GeneratorState]] internal slot.
    //  * has a [[Prototype]] internal slot whose value is %IteratorPrototype%.
    //  * has properties that are indirectly inherited by all Generator instances.
    let generator_prototype = ordinary_object_create(Some(iterator_prototype));
    gf_prototype_data!("prototype", generator_prototype.clone(), false, false, true);

    macro_rules! gen_prototype_data {
        ( $name:expr, $value:expr, $writable:expr, $enumerable:expr, $configurable:expr ) => {
            define_property_or_throw(
                &generator_prototype,
                $name,
                PotentialPropertyDescriptor::new()
                    .value(ECMAScriptValue::from($value))
                    .writable($writable)
                    .enumerable($enumerable)
                    .configurable($configurable),
            )
            .unwrap();
        };
    }

    gen_prototype_data!("constructor", generator_function_prototype.clone(), false, false, true);
    gen_prototype_data!(to_string_tag, "Generator", false, false, true);

    macro_rules! gen_prototype_function {
        ( $steps:expr, $name:expr, $length:expr ) => {{
            let key = PropertyKey::from($name);
            let function_object = create_builtin_function(
                Box::new($steps),
                None,
                $length,
                key.clone(),
                BUILTIN_FUNCTION_SLOTS,
                Some(realm.clone()),
                Some(function_prototype.clone()),
                None,
            );
            define_property_or_throw(
                &generator_prototype,
                key,
                PotentialPropertyDescriptor::new()
                    .value(function_object.clone())
                    .writable(true)
                    .enumerable(false)
                    .configurable(true),
            )
            .unwrap();
            function_object
        }};
    }
    let generator_prototype_next_obj = gen_prototype_function!(generator_prototype_next, "next", 1.0);
    gen_prototype_function!(generator_prototype_return, "return", 1.0);
    gen_prototype_function!(generator_prototype_throw, "throw", 1.0);

    realm.borrow_mut().intrinsics.generator_function = generator_function_constructor;
    realm.borrow_mut().intrinsics.generator_function_prototype = generator_function_prototype;
    realm.borrow_mut().intrinsics.generator_function_prototype_prototype = generator_prototype;
    realm.borrow_mut().intrinsics.generator_function_prototype_prototype_next = generator_prototype_next_obj;
}

#[expect(clippy::unnecessary_wraps)]
fn iterator_prototype_iterator(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // %IteratorPrototype% [ @@iterator ] ( )
    //
    // This function performs the following steps when called:
    //
    //  1. Return the this value.
    Ok(this_value.clone())
}

fn generator_function(
    _this_value: &ECMAScriptValue,
    new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // GeneratorFunction ( ...parameterArgs, bodyArg )
    // The last argument (if any) specifies the body (executable code) of a function; any preceding arguments
    // specify formal parameters.
    //
    // This function performs the following steps when called:
    //
    //  1. Let C be the active function object.
    //  2. If bodyArg is not present, set bodyArg to the empty String.
    //  3. Return ? CreateDynamicFunction(C, NewTarget, GENERATOR, parameterArgs, bodyArg).
    let empty = ECMAScriptValue::from("");
    let (parameter_args, body_arg): (_, _) =
        if let [rest @ .., last] = arguments { (rest, last) } else { (&[], &empty) };
    let c = active_function_object().expect("A function should be running");
    create_dynamic_function(&c, new_target, FunctionKind::Generator, parameter_args, body_arg)
        .map(ECMAScriptValue::Object)
}

fn generator_prototype_next(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Generator.prototype.next ( value )
    //  1. Return ? GeneratorResume(this value, value, empty).
    let mut args = FuncArgs::from(arguments);
    let value = args.next_arg();
    generator_resume(this_value, value, "")
}

fn generator_prototype_return(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // %GeneratorPrototype%.return ( value )
    // This method performs the following steps when called:
    //
    //  1. Let g be the this value.
    //  2. Let C be Completion Record { [[Type]]: RETURN, [[Value]]: value, [[Target]]: EMPTY }.
    //  3. Return ? GeneratorResumeAbrupt(g, C, EMPTY).
    let mut args = FuncArgs::from(arguments);
    let value = args.next_arg();
    generator_resume_abrupt(this_value, AbruptCompletion::Return { value }, "")
}

fn generator_prototype_throw(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // %GeneratorPrototype%.throw ( exception )
    // This method performs the following steps when called:
    //
    //  1. Let g be the this value.
    //  2. Let C be ThrowCompletion(exception).
    //  3. Return ? GeneratorResumeAbrupt(g, C, EMPTY).
    let mut args = FuncArgs::from(arguments);
    let exception = args.next_arg();
    generator_resume_abrupt(this_value, AbruptCompletion::Throw { value: exception }, "")
}

pub type ECMAClosure = Box<
    dyn Coroutine<Yield = ECMAScriptValue, Resume = Completion<ECMAScriptValue>, Return = Completion<ECMAScriptValue>>
        + Unpin,
>;

pub fn create_iter_result_object(value: ECMAScriptValue, done: bool) -> Object {
    // CreateIterResultObject ( value, done )
    //
    // The abstract operation CreateIterResultObject takes arguments value (an ECMAScript language value)
    // and done (a Boolean) and returns an Object that conforms to the IteratorResult interface. It
    // creates an object that conforms to the IteratorResult interface. It performs the following steps
    // when called:
    //
    //  1. Let obj be OrdinaryObjectCreate(%Object.prototype%).
    //  2. Perform ! CreateDataPropertyOrThrow(obj, "value", value).
    //  3. Perform ! CreateDataPropertyOrThrow(obj, "done", done).
    //  4. Return obj.
    let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
    let obj = ordinary_object_create(Some(object_prototype));
    obj.create_data_property_or_throw("value", value).unwrap();
    obj.create_data_property_or_throw("done", done).unwrap();
    obj
}

async fn list_iterator(
    co: Co<ECMAScriptValue, Completion<ECMAScriptValue>>,
    data: Vec<ECMAScriptValue>,
) -> Completion<ECMAScriptValue> {
    //  1. For each element E of list, do
    //      a. Perform ? GeneratorYield(CreateIterResultObject(E, false)).
    //  2. Return undefined.
    for value in data {
        let res = ECMAScriptValue::from(create_iter_result_object(value, false));
        generator_yield(&co, res).await?;
    }
    Ok(ECMAScriptValue::Undefined)
}

pub fn create_list_iterator_record(data: Vec<ECMAScriptValue>) -> IteratorRecord {
    // CreateListIteratorRecord ( list )
    // The abstract operation CreateListIteratorRecord takes argument list (a List) and returns an
    // Iterator Record. It creates an Iterator (27.1.1.2) object record whose next method returns the
    // successive elements of list. It performs the following steps when called:
    //
    //  1. Let closure be a new Abstract Closure with no parameters that captures list and performs the
    //     following steps when called:
    //      a. For each element E of list, do
    //          i. Perform ? GeneratorYield(CreateIterResultObject(E, false)).
    //      b. Return undefined.
    //  2. Let iterator be CreateIteratorFromClosure(closure, empty, %IteratorPrototype%).
    //  3. Return the Iterator Record { [[Iterator]]: iterator, [[NextMethod]]:
    //     %GeneratorFunction.prototype.prototype.next%, [[Done]]: false }.
    //
    // NOTE: The list iterator object is never directly accessible to ECMAScript code.
    let closure = |co| list_iterator(co, data);
    let iterator =
        create_iterator_from_closure(asyncfn_wrap(closure), "", Some(intrinsic(IntrinsicId::IteratorPrototype)));
    IteratorRecord {
        iterator,
        next_method: intrinsic(IntrinsicId::GeneratorFunctionPrototypePrototypeNext).into(),
        done: Cell::new(false),
    }
}

async fn gen_caller(
    generator: Object,
    co: Co<ECMAScriptValue, Completion<ECMAScriptValue>>,
    closure: AsyncFnPtr,
) -> Completion<ECMAScriptValue> {
    let result = closure(co).await;
    AGENT.with(|agent| agent.execution_context_stack.borrow_mut().pop());
    generator.o.to_generator_object().unwrap().generator_data.borrow_mut().generator_state = GeneratorState::Completed;
    let result_value = match &result {
        Ok(_) => ECMAScriptValue::Undefined,
        Err(e) => match e {
            AbruptCompletion::Return { value } => value.clone(),
            AbruptCompletion::Break { .. } | AbruptCompletion::Continue { .. } => {
                panic!("Invalid generator return value")
            }
            AbruptCompletion::Throw { .. } => return result,
        },
    };
    Ok(create_iter_result_object(result_value, true).into())
}
type AsyncFnPtr = Box<
    dyn FnOnce(
        Co<ECMAScriptValue, Completion<ECMAScriptValue>>,
    ) -> LocalBoxFuture<'static, Completion<ECMAScriptValue>>,
>;
pub fn asyncfn_wrap<F, Fut>(func: F) -> AsyncFnPtr
where
    F: 'static,
    F: FnOnce(Co<ECMAScriptValue, Completion<ECMAScriptValue>>) -> Fut,
    Fut: 'static,
    Fut: Future<Output = Completion<ECMAScriptValue>>,
{
    Box::new(|context| Box::pin(func(context)))
}

pub fn create_iterator_from_closure(
    closure: AsyncFnPtr,
    generator_brand: &str,
    generator_prototype: Option<Object>,
) -> Object {
    // CreateIteratorFromClosure ( closure, generatorBrand, generatorPrototype )
    // The abstract operation CreateIteratorFromClosure takes arguments closure (an Abstract Closure with
    // no parameters), generatorBrand, and generatorPrototype (an Object) and returns a Generator. It
    // performs the following steps when called:
    //
    //   1. NOTE: closure can contain uses of the Yield shorthand to yield an IteratorResult object.
    //   2. Let internalSlotsList be « [[GeneratorState]], [[GeneratorContext]], [[GeneratorBrand]] ».
    //   3. Let generator be OrdinaryObjectCreate(generatorPrototype, internalSlotsList).
    //   4. Set generator.[[GeneratorBrand]] to generatorBrand.
    //   5. Set generator.[[GeneratorState]] to undefined.
    //   6. Let callerContext be the running execution context.
    //   7. Let calleeContext be a new execution context.
    //   8. Set the Function of calleeContext to null.
    //   9. Set the Realm of calleeContext to the current Realm Record.
    //  10. Set the ScriptOrModule of calleeContext to callerContext's ScriptOrModule.
    //  11. If callerContext is not already suspended, suspend callerContext.
    //  12. Push calleeContext onto the execution context stack; calleeContext is now the running
    //      execution context.
    //  13. Perform GeneratorStart(generator, closure).
    //  14. Remove calleeContext from the execution context stack and restore callerContext as the running
    //      execution context.
    //  15. Return generator.
    let generator = GeneratorObject::object(generator_prototype, GeneratorState::Undefined, generator_brand);
    let callee_context = ExecutionContext::new(None, current_realm_record().unwrap(), current_script_or_module());
    push_execution_context(callee_context);
    let gen_closure = Box::new(Gen::new(|co| gen_caller(generator.clone(), co, closure)));

    generator_start_from_closure(&generator, gen_closure);
    // callerContext should be back on the top already.
    generator
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum GeneratorState {
    Undefined,
    SuspendedStart,
    SuspendedYield,
    Executing,
    Completed,
}
#[derive(Debug)]
pub struct GeneratorData {
    pub generator_state: GeneratorState,
    pub generator_context: Option<ExecutionContext>,
    pub generator_brand: String,
}
#[derive(Debug)]
pub struct GeneratorObject {
    common: RefCell<CommonObjectData>,
    pub generator_data: RefCell<GeneratorData>,
}

impl<'a> From<&'a GeneratorObject> for &'a dyn ObjectInterface {
    fn from(obj: &'a GeneratorObject) -> Self {
        obj
    }
}

impl ObjectInterface for GeneratorObject {
    fn common_object_data(&self) -> &RefCell<CommonObjectData> {
        &self.common
    }
    fn uses_ordinary_get_prototype_of(&self) -> bool {
        true
    }
    fn id(&self) -> usize {
        self.common.borrow().objid
    }
    fn is_generator_object(&self) -> bool {
        true
    }
    fn to_generator_object(&self) -> Option<&GeneratorObject> {
        Some(self)
    }
    fn get_prototype_of(&self) -> Completion<Option<Object>> {
        Ok(ordinary_get_prototype_of(self))
    }

    // [[SetPrototypeOf]] ( V )
    //
    // The [[SetPrototypeOf]] internal method of an ordinary object O takes argument V (an Object or null). It performs
    // the following steps when called:
    //
    //  1. Return ! OrdinarySetPrototypeOf(O, V).
    fn set_prototype_of(&self, obj: Option<Object>) -> Completion<bool> {
        Ok(ordinary_set_prototype_of(self, obj))
    }

    // [[IsExtensible]] ( )
    //
    // The [[IsExtensible]] internal method of an ordinary object O takes no arguments. It performs the following steps
    // when called:
    //
    //  1. Return ! OrdinaryIsExtensible(O).
    fn is_extensible(&self) -> Completion<bool> {
        Ok(ordinary_is_extensible(self))
    }

    // [[PreventExtensions]] ( )
    //
    // The [[PreventExtensions]] internal method of an ordinary object O takes no arguments. It performs the following
    // steps when called:
    //
    //  1. Return ! OrdinaryPreventExtensions(O).
    fn prevent_extensions(&self) -> Completion<bool> {
        Ok(ordinary_prevent_extensions(self))
    }

    // [[GetOwnProperty]] ( P )
    //
    // The [[GetOwnProperty]] internal method of an ordinary object O takes argument P (a property key). It performs the
    // following steps when called:
    //
    //  1. Return ! OrdinaryGetOwnProperty(O, P).
    fn get_own_property(&self, key: &PropertyKey) -> Completion<Option<PropertyDescriptor>> {
        Ok(ordinary_get_own_property(self, key))
    }

    // [[DefineOwnProperty]] ( P, Desc )
    //
    // The [[DefineOwnProperty]] internal method of an ordinary object O takes arguments P (a property key) and Desc (a
    // Property Descriptor). It performs the following steps when called:
    //
    //  1. Return ? OrdinaryDefineOwnProperty(O, P, Desc).
    fn define_own_property(&self, key: PropertyKey, desc: PotentialPropertyDescriptor) -> Completion<bool> {
        ordinary_define_own_property(self, key, desc)
    }

    // [[HasProperty]] ( P )
    //
    // The [[HasProperty]] internal method of an ordinary object O takes argument P (a property key). It performs the
    // following steps when called:
    //
    //  1. Return ? OrdinaryHasProperty(O, P).
    fn has_property(&self, key: &PropertyKey) -> Completion<bool> {
        ordinary_has_property(self, key)
    }

    // [[Get]] ( P, Receiver )
    //
    // The [[Get]] internal method of an ordinary object O takes arguments P (a property key) and Receiver (an
    // ECMAScript language value). It performs the following steps when called:
    //
    //  1. Return ? OrdinaryGet(O, P, Receiver).
    fn get(&self, key: &PropertyKey, receiver: &ECMAScriptValue) -> Completion<ECMAScriptValue> {
        ordinary_get(self, key, receiver)
    }

    // [[Set]] ( P, V, Receiver )
    //
    // The [[Set]] internal method of an ordinary object O takes arguments P (a property key), V (an ECMAScript language
    // value), and Receiver (an ECMAScript language value). It performs the following steps when called:
    //
    //  1. Return ? OrdinarySet(O, P, V, Receiver).
    fn set(&self, key: PropertyKey, v: ECMAScriptValue, receiver: &ECMAScriptValue) -> Completion<bool> {
        ordinary_set(self, key, v, receiver)
    }

    // [[Delete]] ( P )
    //
    // The [[Delete]] internal method of an ordinary object O takes argument P (a property key). It performs the
    // following steps when called:
    //
    //  1. Return ? OrdinaryDelete(O, P).
    fn delete(&self, key: &PropertyKey) -> Completion<bool> {
        ordinary_delete(self, key)
    }

    // [[OwnPropertyKeys]] ( )
    //
    // The [[OwnPropertyKeys]] internal method of an ordinary object O takes no arguments. It performs the following
    // steps when called:
    //
    // 1. Return ! OrdinaryOwnPropertyKeys(O).
    fn own_property_keys(&self) -> Completion<Vec<PropertyKey>> {
        Ok(ordinary_own_property_keys(self))
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum GeneratorError {
    BrandMismatch,
    AlreadyActive,
    NotAGenerator,
}

impl fmt::Display for GeneratorError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GeneratorError::BrandMismatch => write!(f, "Generator brand mismatch"),
            GeneratorError::AlreadyActive => write!(f, "Generator is already executing"),
            GeneratorError::NotAGenerator => write!(f, "Generator required"),
        }
    }
}

impl Error for GeneratorError {}

impl GeneratorObject {
    pub fn new(prototype: Option<Object>, state: GeneratorState, brand: &str) -> Self {
        Self {
            common: RefCell::new(CommonObjectData::new(prototype, true, GENERATOR_OBJECT_SLOTS)),
            generator_data: RefCell::new(GeneratorData {
                generator_state: state,
                generator_context: None,
                generator_brand: brand.to_owned(),
            }),
        }
    }
    pub fn object(prototype: Option<Object>, state: GeneratorState, brand: &str) -> Object {
        Object { o: Rc::new(Self::new(prototype, state, brand)) }
    }

    pub fn validate(&self, generator_brand: &str) -> Result<GeneratorState, GeneratorError> {
        // GeneratorValidate ( generator, generatorBrand )
        // The abstract operation GeneratorValidate takes arguments generator and generatorBrand and returns
        // either a normal completion containing either suspendedStart, suspendedYield, or completed, or a
        // throw completion. It performs the following steps when called:
        //
        //  1. Perform ? RequireInternalSlot(generator, [[GeneratorState]]).
        //  2. Perform ? RequireInternalSlot(generator, [[GeneratorBrand]]).
        //  3. If generator.[[GeneratorBrand]] is not the same value as generatorBrand, throw a TypeError
        //     exception.
        //  4. Assert: generator also has a [[GeneratorContext]] internal slot.
        //  5. Let state be generator.[[GeneratorState]].
        //  6. If state is executing, throw a TypeError exception.
        //  7. Return state.

        // This is steps 3-7. (If you want to call the function GeneratorValidate like the specification does,
        // use generator_validate.)
        let data = self.generator_data.borrow();
        if data.generator_brand != generator_brand {
            Err(GeneratorError::BrandMismatch)
        } else if data.generator_state == GeneratorState::Executing {
            Err(GeneratorError::AlreadyActive)
        } else {
            Ok(data.generator_state)
        }
    }
}

pub fn generator_validate(generator: &ECMAScriptValue, generator_brand: &str) -> Completion<GeneratorState> {
    // GeneratorValidate ( generator, generatorBrand )
    // The abstract operation GeneratorValidate takes arguments generator and generatorBrand and returns
    // either a normal completion containing either suspendedStart, suspendedYield, or completed, or a
    // throw completion. It performs the following steps when called:
    //
    //  1. Perform ? RequireInternalSlot(generator, [[GeneratorState]]).
    //  2. Perform ? RequireInternalSlot(generator, [[GeneratorBrand]]).
    //  3. If generator.[[GeneratorBrand]] is not the same value as generatorBrand, throw a TypeError
    //     exception.
    //  4. Assert: generator also has a [[GeneratorContext]] internal slot.
    //  5. Let state be generator.[[GeneratorState]].
    //  6. If state is executing, throw a TypeError exception.
    //  7. Return state.
    match generator {
        ECMAScriptValue::Object(o) => {
            o.o.to_generator_object().ok_or(GeneratorError::NotAGenerator).and_then(|gen| gen.validate(generator_brand))
        }
        _ => Err(GeneratorError::NotAGenerator),
    }
    .map_err(|e| create_type_error(e.to_string()))
}

pub fn generator_start_from_function_body(generator: &Object, func: &dyn FunctionInterface, text: &str) {
    // This is like generator_start_from_closure, except that our "closure" performs:
    //    i. Let result be Completion(Evaluation of generatorBody).
    // where that evaluation is an asynchronous routine that might relinquish control during its execution.
    let fd = func.function_data();
    let closure = fd.borrow().into_async_closure(text);
    let generator_in_closure = generator.clone();
    let gen_closure = Box::new(Gen::new(|co| gen_caller(generator_in_closure, co, closure)));
    generator_start_from_closure(generator, gen_closure);
    ec_push(Ok(generator.clone().into()));
}

impl FunctionObjectData {
    pub fn into_async_closure(&self, text: &str) -> AsyncFnPtr {
        let text = text.to_owned();
        let closure = move |co| execute(co, text);
        asyncfn_wrap(closure)
    }
}

pub fn generator_start_from_closure(generator: &Object, generator_body: ECMAClosure) {
    // GeneratorStart ( generator, generatorBody )
    // The abstract operation GeneratorStart takes arguments generator and generatorBody (a FunctionBody
    // Parse Node or an Abstract Closure with no parameters) and returns unused. It performs the following
    // steps when called:
    //
    //  1. Assert: The value of generator.[[GeneratorState]] is undefined.
    //  2. Let genContext be the running execution context.
    //  3. Set the Generator component of genContext to generator.
    //  4. Set the code evaluation state of genContext such that when evaluation is resumed for that
    //     execution context the following steps will be performed:
    //      a. If generatorBody is a Parse Node, then
    //           i. Let result be Completion(Evaluation of generatorBody).
    //      b. Else,
    //           i. Assert: generatorBody is an Abstract Closure with no parameters.
    //          ii. Let result be generatorBody().
    //      c. Assert: If we return here, the generator either threw an exception or performed either an
    //         implicit or explicit return.
    //      d. Remove genContext from the execution context stack and restore the execution context that
    //         is at the top of the execution context stack as the running execution context.
    //      e. Set generator.[[GeneratorState]] to completed.
    //      f. Once a generator enters the completed state it never leaves it and its associated execution
    //         context is never resumed. Any execution state associated with generator can be discarded at
    //         this point.
    //      g. If result.[[Type]] is normal, let resultValue be undefined.
    //      h. Else if result.[[Type]] is return, let resultValue be result.[[Value]].
    //      i. Else,
    //           i. Assert: result.[[Type]] is throw.
    //          ii. Return ? result.
    //      j. Return CreateIterResultObject(resultValue, true).
    //  5. Set generator.[[GeneratorContext]] to genContext.
    //  6. Set generator.[[GeneratorState]] to suspendedStart.
    //  7. Return unused.

    // This winds up being a bit different for Rust...

    // So in Rust, the Execution Context needs to live in only one place. In the case of starting a
    // generator from a closure, the generator context is actually popped off the stack immediately
    // following the call to GeneratorStart. We therefore accelerate that a bit and take it off the stack
    // _now_, so that we can put it into the generator object instead.
    //
    // Something else will need to happen for user-constructed generators.
    let inner_generator = generator.o.to_generator_object().unwrap();
    let mut gdata = inner_generator.generator_data.borrow_mut();
    assert_eq!(gdata.generator_state, GeneratorState::Undefined);
    assert!(gdata.generator_context.is_none());
    AGENT.with(|agent| gdata.generator_context = agent.execution_context_stack.borrow_mut().pop());
    gdata.generator_state = GeneratorState::SuspendedStart;
    let gc = gdata.generator_context.as_mut().expect("Unstarted generators should already have their contexts");
    gc.generator = Some(generator.clone());
    gc.gen_closure = Some(generator_body);
}

pub fn generator_resume(
    generator: &ECMAScriptValue,
    value: ECMAScriptValue,
    generator_brand: &str,
) -> Completion<ECMAScriptValue> {
    // GeneratorResume ( generator, value, generatorBrand )
    // The abstract operation GeneratorResume takes arguments generator, value, and generatorBrand and returns
    // either a normal completion containing an ECMAScript language value or a throw completion. It performs
    // the following steps when called:
    //
    //   1. Let state be ? GeneratorValidate(generator, generatorBrand).
    //   2. If state is completed, return CreateIterResultObject(undefined, true).
    //   3. Assert: state is either suspendedStart or suspendedYield.
    //   4. Let genContext be generator.[[GeneratorContext]].
    //   5. Let methodContext be the running execution context.
    //   6. Suspend methodContext.
    //   7. Set generator.[[GeneratorState]] to executing.
    //   8. Push genContext onto the execution context stack; genContext is now the running execution context.
    //   9. Resume the suspended evaluation of genContext using NormalCompletion(value) as the result of the
    //      operation that suspended it. Let result be the value returned by the resumed computation.
    //  10. Assert: When we return here, genContext has already been removed from the execution context stack
    //      and methodContext is the currently running execution context.
    //  11. Return ? result.
    let state = generator_validate(generator, generator_brand)?;
    if state == GeneratorState::Completed {
        return Ok(create_iter_result_object(ECMAScriptValue::Undefined, true).into());
    }
    assert!(state == GeneratorState::SuspendedStart || state == GeneratorState::SuspendedYield);

    let obj = Object::try_from(generator).expect("generator previously validated");
    let mut co = {
        let mut gdata =
            obj.o.to_generator_object().expect("generator previously validated").generator_data.borrow_mut();
        let gen_context = gdata.generator_context.take().expect("suspended generators hold their context");
        AGENT.with(|agent| {
            agent.execution_context_stack.borrow_mut().push(gen_context);
            gdata.generator_state = GeneratorState::Executing;
            let ec_stack_len = agent.execution_context_stack.borrow().len();
            {
                let mut ec_stack = agent.execution_context_stack.borrow_mut();
                ec_stack[ec_stack_len - 1].gen_closure.take().expect("generator has closure?")
            }
        })
    };
    let result = {
        let pinned = Pin::new(co.as_mut());
        pinned.resume_with(Ok(value))
    };
    // If we get back here, the generator has been suspended.
    let mut gdata = obj.o.to_generator_object().expect("generator previously validated").generator_data.borrow_mut();
    if let Some(gen_context) = gdata.generator_context.as_mut() {
        gen_context.gen_closure = Some(co);
    }

    match result {
        genawaiter::GeneratorState::Yielded(y) => Ok(y),
        genawaiter::GeneratorState::Complete(c) => c,
    }
}

pub fn generator_resume_abrupt(
    generator: &ECMAScriptValue,
    abrupt_completion: AbruptCompletion,
    generator_brand: &str,
) -> Completion<ECMAScriptValue> {
    // GeneratorResumeAbrupt ( generator, abruptCompletion, generatorBrand )
    //
    // The abstract operation GeneratorResumeAbrupt takes arguments generator (an ECMAScript language value),
    // abruptCompletion (a return completion or a throw completion), and generatorBrand (a String or empty)
    // and returns either a normal completion containing an ECMAScript language value or a throw completion.
    // It performs the following steps when called:
    //
    //   1. Let state be ? GeneratorValidate(generator, generatorBrand).
    //   2. If state is suspendedStart, then
    //      a. Set generator.[[GeneratorState]] to completed.
    //      b. NOTE: Once a generator enters the completed state it never leaves it and its associated
    //         execution context is never resumed. Any execution state associated with generator can be
    //         discarded at this point.
    //      c. Set state to completed.
    //   3. If state is completed, then
    //      a. If abruptCompletion.[[Type]] is return, then
    //          i. Return CreateIterResultObject(abruptCompletion.[[Value]], true).
    //      b. Return ? abruptCompletion.
    //   4. Assert: state is suspendedYield.
    //   5. Let genContext be generator.[[GeneratorContext]].
    //   6. Let methodContext be the running execution context.
    //   7. Suspend methodContext.
    //   8. Set generator.[[GeneratorState]] to executing.
    //   9. Push genContext onto the execution context stack; genContext is now the running execution context.
    //  10. Resume the suspended evaluation of genContext using abruptCompletion as the result of the
    //      operation that suspended it. Let result be the Completion Record returned by the resumed
    //      computation.
    //  11. Assert: When we return here, genContext has already been removed from the execution context stack
    //      and methodContext is the currently running execution context.
    //  12. Return ? result.
    let state = generator_validate(generator, generator_brand)?;

    if state == GeneratorState::SuspendedStart || state == GeneratorState::Completed {
        if state == GeneratorState::SuspendedStart {
            let obj = Object::try_from(generator).expect("generator previously validated");
            let mut gdata =
                obj.o.to_generator_object().expect("generator previously validated").generator_data.borrow_mut();
            gdata.generator_state = GeneratorState::Completed;
            gdata.generator_context = None;
        }
        if let AbruptCompletion::Return { value } = abrupt_completion {
            return Ok(create_iter_result_object(value, true).into());
        }
        return Err(abrupt_completion);
    }

    let obj = Object::try_from(generator).expect("generator previously validated");
    let mut co = {
        let mut gdata =
            obj.o.to_generator_object().expect("generator previously validated").generator_data.borrow_mut();
        let gen_context = gdata.generator_context.take().expect("suspended generators hold their context");
        AGENT.with(|agent| {
            agent.execution_context_stack.borrow_mut().push(gen_context);
            gdata.generator_state = GeneratorState::Executing;
            let ec_stack_len = agent.execution_context_stack.borrow().len();
            {
                let mut ec_stack = agent.execution_context_stack.borrow_mut();
                ec_stack[ec_stack_len - 1].gen_closure.take().expect("generator has closure?")
            }
        })
    };
    let result = {
        let pinned = Pin::new(co.as_mut());
        pinned.resume_with(Err(abrupt_completion))
    };
    // If we get back here, the generator has been suspended.
    let mut gdata = obj.o.to_generator_object().expect("generator previously validated").generator_data.borrow_mut();
    if let Some(gen_context) = gdata.generator_context.as_mut() {
        gen_context.gen_closure = Some(co);
    }

    match result {
        genawaiter::GeneratorState::Yielded(y) => Ok(y),
        genawaiter::GeneratorState::Complete(c) => c,
    }
}

pub async fn generator_yield(
    co: &Co<ECMAScriptValue, Completion<ECMAScriptValue>>,
    iter_next_obj: ECMAScriptValue,
) -> Completion<ECMAScriptValue> {
    // GeneratorYield ( iterNextObj )
    //
    // The abstract operation GeneratorYield takes argument iterNextObj (an Object that conforms to the
    // IteratorResult interface) and returns either a normal completion containing an ECMAScript language
    // value or an abrupt completion. It performs the following steps when called:
    //
    //  1. Let genContext be the running execution context.
    //  2. Assert: genContext is the execution context of a generator.
    //  3. Let generator be the value of the Generator component of genContext.
    //  4. Assert: GetGeneratorKind() is sync.
    //  5. Set generator.[[GeneratorState]] to suspendedYield.
    //  6. Remove genContext from the execution context stack and restore the execution context that is at the
    //     top of the execution context stack as the running execution context.
    //  7. Let callerContext be the running execution context.
    //  8. Resume callerContext passing NormalCompletion(iterNextObj). If genContext is ever resumed again,
    //     let resumptionValue be the Completion Record with which it is resumed.
    //  9. Assert: If control reaches here, then genContext is the running execution context again.
    // 10. Return resumptionValue.

    let gen_context =
        AGENT.with(|agent| agent.execution_context_stack.borrow_mut().pop()).expect("should be running in a context");
    assert!(gen_context.generator.is_some());
    let generator = gen_context.generator.clone().expect("running generator must have matching execution context");
    let inner_generator = generator.o.to_generator_object().expect("object must be a generator");
    {
        let mut gdata = inner_generator.generator_data.borrow_mut();
        gdata.generator_state = GeneratorState::SuspendedYield;
        gdata.generator_context = Some(gen_context);
    }
    co.yield_(iter_next_obj).await
}

// Iterator Records
//
// An Iterator Record is a Record value used to encapsulate an Iterator or
// AsyncIterator along with the next method.
//
// Iterator Records have the fields listed in Table 15.
//
// Table 15: Iterator Record Fields
// +----------------+----------------+---------------------------------------------------------------------+
// | Field Name     | Value          | Meaning                                                             |
// +----------------+----------------+---------------------------------------------------------------------+
// | [[Iterator]]   | an Object      | An object that conforms to the Iterator or AsyncIterator interface. |
// +----------------+----------------+---------------------------------------------------------------------+
// | [[NextMethod]] | an ECMAScript  | The next method of the [[Iterator]] object.                         |
// |                | language value |                                                                     |
// +----------------+----------------+---------------------------------------------------------------------+
// | [[Done]]       | a Boolean      | Whether the iterator has been closed.                               |
// +----------------+----------------+---------------------------------------------------------------------+
pub struct IteratorRecord {
    pub iterator: Object,
    pub next_method: ECMAScriptValue,
    pub done: Cell<bool>,
}

impl fmt::Debug for IteratorRecord {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("IteratorRecord")
            .field("iterator", &ConciseObject::from(&self.iterator))
            .field("next_method", &ConciseValue::from(&self.next_method))
            .field("done", &self.done)
            .finish()
    }
}

struct ConciseValue<'a>(&'a ECMAScriptValue);
impl<'a> fmt::Debug for ConciseValue<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.concise(f)
    }
}
impl<'a> From<&'a ECMAScriptValue> for ConciseValue<'a> {
    fn from(value: &'a ECMAScriptValue) -> Self {
        Self(value)
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum IteratorKind {
    Sync,
    Async,
}

pub fn get_iterator_from_method(obj: &ECMAScriptValue, method: &ECMAScriptValue) -> Completion<IteratorRecord> {
    // GetIteratorFromMethod ( obj, method )
    //
    // The abstract operation GetIteratorFromMethod takes arguments obj (an
    // ECMAScript language value) and method (a function object) and returns
    // either a normal completion containing an Iterator Record or a throw
    // completion. It performs the following steps when called:
    //
    //  1. Let iterator be ? Call(method, obj).
    //  2. If iterator is not an Object, throw a TypeError exception.
    //  3. Let nextMethod be ? GetV(iterator, "next").
    //  4. Let iteratorRecord be the Iterator Record { [[Iterator]]: iterator,
    //     [[NextMethod]]: nextMethod, [[Done]]: false }.
    //  5. Return iteratorRecord.
    let iterator = call(method, obj, &[])?;
    if !iterator.is_object() {
        return Err(create_type_error("not an object"));
    }
    let next_method = iterator.get(&"next".into())?;
    let iterator = Object::try_from(iterator).expect("iterator previously proved");
    Ok(IteratorRecord { iterator, next_method, done: Cell::new(false) })
}

pub fn get_iterator(obj: &ECMAScriptValue, kind: IteratorKind) -> Completion<IteratorRecord> {
    // GetIterator ( obj, kind )
    //
    // The abstract operation GetIterator takes arguments obj (an ECMAScript
    // language value) and kind (sync or async) and returns either a normal
    // completion containing an Iterator Record or a throw completion. It
    // performs the following steps when called:
    //
    //  1. If kind is async, then
    //      a. Let method be ? GetMethod(obj, @@asyncIterator).
    //      b. If method is undefined, then
    //          i. Let syncMethod be ? GetMethod(obj, @@iterator).
    //          ii. If syncMethod is undefined, throw a TypeError exception.
    //          iii. Let syncIteratorRecord be ? GetIteratorFromMethod(obj, syncMethod).
    //          iv. Return CreateAsyncFromSyncIterator(syncIteratorRecord).
    //  2. Otherwise, let method be ? GetMethod(obj, @@iterator).
    //  3. If method is undefined, throw a TypeError exception.
    //  4. Return ? GetIteratorFromMethod(obj, method).
    let method = match kind {
        IteratorKind::Async => todo!(),
        IteratorKind::Sync => {
            let iter_symbol = wks(WksId::Iterator);
            obj.get_method(&iter_symbol.into())?
        }
    };
    if method.is_undefined() {
        return Err(create_type_error("object is not iterable"));
    }
    get_iterator_from_method(obj, &method)
}

impl IteratorRecord {
    pub fn concise(&self) -> String {
        format!(
            "IR(iter: {:?}; next: {:?}; {})",
            ConciseObject::from(&self.iterator),
            ConciseValue::from(&self.next_method),
            if self.done.get() { "DONE" } else { "unfinished" }
        )
    }

    fn next(&self, value: Option<ECMAScriptValue>) -> Completion<Object> {
        // IteratorNext ( iteratorRecord [ , value ] )
        //
        // The abstract operation IteratorNext takes argument iteratorRecord (an
        // Iterator Record) and optional argument value (an ECMAScript language
        // value) and returns either a normal completion containing an Object or
        // a throw completion. It performs the following steps when called:
        //
        //  1. If value is not present, then
        //      a. Let result be ? Call(iteratorRecord.[[NextMethod]], iteratorRecord.[[Iterator]]).
        //  2. Else,
        //      a. Let result be ? Call(iteratorRecord.[[NextMethod]], iteratorRecord.[[Iterator]], « value »).
        //  3. If result is not an Object, throw a TypeError exception.
        //  4. Return result.
        let next_method = &self.next_method;
        let iterator = ECMAScriptValue::from(&self.iterator);
        let result = match value {
            Some(value) => call(next_method, &iterator, &[value])?,
            None => call(next_method, &iterator, &[])?,
        };
        Object::try_from(result).map_err(|_| create_type_error("not an iterator result"))
    }

    pub fn step(&self) -> Completion<Option<Object>> {
        // IteratorStep ( iteratorRecord )
        //
        // The abstract operation IteratorStep takes argument iteratorRecord (an
        // Iterator Record) and returns either a normal completion containing
        // either an Object or false, or a throw completion. It requests the
        // next value from iteratorRecord.[[Iterator]] by calling
        // iteratorRecord.[[NextMethod]] and returns either false indicating
        // that the iterator has reached its end or the IteratorResult object if
        // a next value is available. It performs the following steps when
        // called:
        //
        //  1. Let result be ? IteratorNext(iteratorRecord).
        //  2. Let done be ? IteratorComplete(result).
        //  3. If done is true, return false.
        //  4. Return result.
        let result = self.next(None)?;
        let done = iterator_complete(&result)?;
        if done {
            Ok(None)
        } else {
            Ok(Some(result))
        }
    }

    pub fn close<X>(&self, completion: Completion<X>) -> Completion<X>
    where
        X: From<ECMAScriptValue>,
    {
        // IteratorClose ( iteratorRecord, completion )
        // The abstract operation IteratorClose takes arguments iteratorRecord (an Iterator Record) and completion (a
        // Completion Record) and returns a Completion Record. It is used to notify an iterator that it should perform
        // any actions it would normally perform when it has reached its completed state. It performs the following
        // steps when called:
        //
        //  1. Assert: iteratorRecord.[[Iterator]] is an Object.
        //  2. Let iterator be iteratorRecord.[[Iterator]].
        //  3. Let innerResult be Completion(GetMethod(iterator, "return")).
        //  4. If innerResult.[[Type]] is normal, then
        //      a. Let return be innerResult.[[Value]].
        //      b. If return is undefined, return ? completion.
        //      c. Set innerResult to Completion(Call(return, iterator)).
        //  5. If completion.[[Type]] is throw, return ? completion.
        //  6. If innerResult.[[Type]] is throw, return ? innerResult.
        //  7. If innerResult.[[Value]] is not an Object, throw a TypeError exception.
        //  8. Return ? completion.
        let iterator = &ECMAScriptValue::from(&self.iterator);
        let inner_result = match iterator.get_method(&"return".into()) {
            Ok(return_v) => {
                if return_v.is_undefined() {
                    return completion;
                }
                call(&return_v, iterator, &[])
            }
            Err(e) => Err(e),
        };
        if matches!(completion, Err(AbruptCompletion::Throw { .. })) {
            return completion;
        }
        if matches!(inner_result, Err(AbruptCompletion::Throw { .. })) {
            return inner_result.map(X::from);
        }
        let value = inner_result.expect("result from call should be throw or value");
        if !value.is_object() {
            return Err(create_type_error("iterator return method returned non object"));
        }
        completion
    }

    pub fn step_value(&self) -> Completion<Option<ECMAScriptValue>> {
        // IteratorStepValue ( iteratorRecord )
        // The abstract operation IteratorStepValue takes argument iteratorRecord (an Iterator Record) and returns
        // either a normal completion containing either an ECMAScript language value or done, or a throw completion. It
        // requests the next value from iteratorRecord.[[Iterator]] by calling iteratorRecord.[[NextMethod]] and returns
        // either done indicating that the iterator has reached its end or the value from the IteratorResult object if a
        // next value is available. It performs the following steps when called:
        //
        // 1. Let result be ? IteratorStep(iteratorRecord).
        // 2. If result is done, then
        //    a. Return done.
        // 3. Let value be Completion(IteratorValue(result)).
        // 4. If value is a throw completion, then
        //    a. Set iteratorRecord.[[Done]] to true.
        // 5. Return ? value.
        let result = self.step()?;
        match result {
            None => Ok(None),
            Some(result) => {
                let value = iterator_value(&result);
                if matches!(value, Err(AbruptCompletion::Throw { .. })) {
                    self.done.set(true);
                }
                value.map(Option::Some)
            }
        }
    }
}

pub fn iterator_complete(iter_result: &Object) -> Completion<bool> {
    // IteratorComplete ( iterResult )
    //
    // The abstract operation IteratorComplete takes argument iterResult (an
    // Object) and returns either a normal completion containing a Boolean or a
    // throw completion. It performs the following steps when called:
    //
    //  1. Return ToBoolean(? Get(iterResult, "done")).
    Ok(to_boolean(iter_result.get(&"done".into())?))
}

pub fn iterator_value(iter_result: &Object) -> Completion<ECMAScriptValue> {
    // IteratorValue ( iterResult )
    //
    // The abstract operation IteratorValue takes argument iterResult (an
    // Object) and returns either a normal completion containing an ECMAScript
    // language value or a throw completion. It performs the following steps
    // when called:
    //
    //  1. Return ? Get(iterResult, "value").
    iter_result.get(&"value".into())
}

pub fn iterator_step(iterator_record: &IteratorRecord) -> Completion<Option<Object>> {
    iterator_record.step()
}

pub fn iterator_close<X>(iterator_record: &IteratorRecord, completion: Completion<X>) -> Completion<X>
where
    X: From<ECMAScriptValue>,
{
    iterator_record.close(completion)
}

pub fn iterator_step_value(iterator_record: &IteratorRecord) -> Completion<Option<ECMAScriptValue>> {
    iterator_record.step_value()
}

#[cfg(test)]
mod tests;
