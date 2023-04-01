use super::*;
use std::cell::RefCell;
use std::error::Error;
use std::fmt;

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

    let iterator_prototype = ordinary_object_create(Some(object_prototype), &[]);

    // Prototype Function Properties
    macro_rules! prototype_function {
        ( $steps:expr, $name:expr, $length:expr ) => {
            let key = PropertyKey::from($name);
            let function_object = create_builtin_function(
                $steps,
                false,
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
        generator_function,
        true,
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
    let generator_function_prototype = ordinary_object_create(Some(function_prototype.clone()), &[]);

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
    let generator_prototype = ordinary_object_create(Some(iterator_prototype), &[]);
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
        ( $steps:expr, $name:expr, $length:expr ) => {
            let key = PropertyKey::from($name);
            let function_object = create_builtin_function(
                $steps,
                false,
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
                    .value(function_object)
                    .writable(true)
                    .enumerable(false)
                    .configurable(true),
            )
            .unwrap();
        };
    }
    gen_prototype_function!(generator_prototype_next, "next", 1.0);
    gen_prototype_function!(generator_prototype_return, "return", 1.0);
    gen_prototype_function!(generator_prototype_throw, "throw", 1.0);

    realm.borrow_mut().intrinsics.generator_function = generator_function_constructor;
    realm.borrow_mut().intrinsics.generator_function_prototype = generator_function_prototype;
    realm.borrow_mut().intrinsics.generator_function_prototype_prototype = generator_prototype;
}

fn iterator_prototype_iterator(
    this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // %IteratorPrototype% [ @@iterator ] ( )
    //
    // This function performs the following steps when called:
    //
    //  1. Return the this value.
    Ok(this_value)
}

fn generator_function(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}

fn generator_prototype_next(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}

fn generator_prototype_return(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}

fn generator_prototype_throw(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}

fn create_iter_result_object(value: ECMAScriptValue, done: bool) -> Object {
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
    let obj = ordinary_object_create(Some(object_prototype), &[]);
    create_data_property_or_throw(&obj, "value", value).unwrap();
    create_data_property_or_throw(&obj, "done", done).unwrap();
    obj
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
    fn is_ordinary(&self) -> bool {
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

#[derive(Debug, PartialEq, Eq)]
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
    pub fn object(prototype: Option<Object>, state: GeneratorState, brand: &str) -> Object {
        Object {
            o: Rc::new(Self {
                common: RefCell::new(CommonObjectData::new(prototype, true, GENERATOR_OBJECT_SLOTS)),
                generator_data: RefCell::new(GeneratorData {
                    generator_state: state,
                    generator_context: None,
                    generator_brand: brand.to_owned(),
                }),
            }),
        }
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
        // use Agent::generator_validate.)
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

pub fn generator_validate(generator: ECMAScriptValue, generator_brand: &str) -> Completion<GeneratorState> {
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

// Iterator Records
//
// An Iterator Record is a Record value used to encapsulate an Iterator or
// AsyncIterator along with the next method.
//
// Iterator Records have the fields listed in Table 15.
//
// Table 15: Iterator Record Fields
// +----------------+-------------------+---------------------------------------------------------------------+
// | Field Name     | Value             | Meaning                                                             |
// +----------------+-------------------+---------------------------------------------------------------------+
// | [[Iterator]]   | an Object         | An object that conforms to the Iterator or AsyncIterator interface. |
// | [[NextMethod]] | a function object | The next method of the [[Iterator]] object.                         |
// | [[Done]]       | a Boolean         | Whether the iterator has been closed.                               |
// +----------------+-------------------+---------------------------------------------------------------------+

pub struct ECMAIterator {
    iterator: ECMAScriptValue,
    next_method: ECMAScriptValue,
    done: bool,
}

#[derive(PartialEq, Eq)]
pub enum IteratorKind {
    Sync,
    Async,
}

fn get_iterator_from_method(obj: &ECMAScriptValue, method: &ECMAScriptValue) -> Completion<ECMAIterator> {
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
    let next_method = getv(&iterator, &"next".into())?;
    Ok(ECMAIterator { iterator, next_method, done: false })
}

fn get_iterator(obj: &ECMAScriptValue, kind: IteratorKind) -> Completion<ECMAIterator> {
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
            get_method(obj, &iter_symbol.into())?
        }
    };
    if method.is_undefined() {
        return Err(create_type_error("not an iterator"));
    }
    get_iterator_from_method(obj, &method)
}

impl ECMAIterator {
    fn next(&self, value: Option<ECMAScriptValue>) -> Completion<ECMAScriptValue> {
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
        let result = match value {
            Some(value) => call(&self.next_method, &self.iterator, &[value])?,
            None => call(&self.next_method, &self.iterator, &[])?,
        };
        if !result.is_object() {
            Err(create_type_error("not an iterator result"))
        } else {
            Ok(result)
        }
    }

    fn step(&self) -> Completion<ECMAScriptValue> {
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
        let result = Object::try_from(self.next(None)?).expect("next should return an iterator result object");
        let done = iterator_complete(&result)?;
        match done {
            true => Ok(false.into()),
            false => Ok(result.into()),
        }
    }

    fn close(&self, completion: Completion<ECMAScriptValue>) -> Completion<ECMAScriptValue> {
        let iterator = &self.iterator;
        let inner_result = get_method(iterator, &"return".into());
        let inner_result = if let Ok(return_v) = inner_result {
            if return_v.is_undefined() {
                return completion;
            }
            call(&return_v, iterator, &[])
        } else {
            inner_result
        };
        if matches!(completion, Err(AbruptCompletion::Throw { .. })) {
            return completion;
        }
        let value = match &inner_result {
            Ok(value)
            | Err(AbruptCompletion::Break { value: NormalCompletion::Value(value), target: _ })
            | Err(AbruptCompletion::Continue { value: NormalCompletion::Value(value), target: _ })
            | Err(AbruptCompletion::Return { value }) => value,
            Err(AbruptCompletion::Throw { .. }) => return inner_result,
            _ => unreachable!(),
        };
        if !value.is_object() {
            return Err(create_type_error("iterator return method returned non object"));
        }
        completion
    }
}

fn iterator_next(iterator_record: &ECMAIterator, value: Option<ECMAScriptValue>) -> Completion<ECMAScriptValue> {
    iterator_record.next(value)
}

fn iterator_complete(iter_result: &Object) -> Completion<bool> {
    // IteratorComplete ( iterResult )
    //
    // The abstract operation IteratorComplete takes argument iterResult (an
    // Object) and returns either a normal completion containing a Boolean or a
    // throw completion. It performs the following steps when called:
    //
    //  1. Return ToBoolean(? Get(iterResult, "done")).
    Ok(to_boolean(get(iter_result, &"done".into())?))
}

fn iterator_value(iter_result: &Object) -> Completion<ECMAScriptValue> {
    // IteratorValue ( iterResult )
    //
    // The abstract operation IteratorValue takes argument iterResult (an
    // Object) and returns either a normal completion containing an ECMAScript
    // language value or a throw completion. It performs the following steps
    // when called:
    //
    //  1. Return ? Get(iterResult, "value").
    get(iter_result, &"value".into())
}

fn iterator_step(iterator_record: &ECMAIterator) -> Completion<ECMAScriptValue> {
    iterator_record.step()
}

fn iterator_close(
    iterator_record: &ECMAIterator,
    completion: Completion<ECMAScriptValue>,
) -> Completion<ECMAScriptValue> {
    iterator_record.close(completion)
}
#[cfg(test)]
mod tests;
