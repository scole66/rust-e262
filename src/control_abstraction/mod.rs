use super::*;
use std::cell::RefCell;
use std::error::Error;
use std::fmt;

impl Agent {
    pub fn provision_iterator_prototype(&self, realm: &Rc<RefCell<Realm>>) {
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

        let iterator_prototype = ordinary_object_create(self, Some(object_prototype), &[]);

        // Prototype Function Properties
        macro_rules! prototype_function {
            ( $steps:expr, $name:expr, $length:expr ) => {
                let key = PropertyKey::from($name);
                let function_object = create_builtin_function(
                    self,
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
                    self,
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
        prototype_function!(iterator_prototype_iterator, self.wks(WksId::Iterator), 0.0);

        realm.borrow_mut().intrinsics.iterator_prototype = iterator_prototype;
    }

    pub fn provision_generator_function_intrinsics(&self, realm: &Rc<RefCell<Realm>>) {
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
            self,
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
                    self,
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
        let generator_function_prototype = ordinary_object_create(self, Some(function_prototype.clone()), &[]);

        macro_rules! gf_prototype_data {
            ( $name:expr, $value:expr, $writable:expr, $enumerable:expr, $configurable:expr ) => {
                define_property_or_throw(
                    self,
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
        let to_string_tag = self.wks(WksId::ToStringTag);
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
        let generator_prototype = ordinary_object_create(self, Some(iterator_prototype), &[]);
        gf_prototype_data!("prototype", generator_prototype.clone(), false, false, true);

        macro_rules! gen_prototype_data {
            ( $name:expr, $value:expr, $writable:expr, $enumerable:expr, $configurable:expr ) => {
                define_property_or_throw(
                    self,
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
                    self,
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
                    self,
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
}

fn iterator_prototype_iterator(
    _agent: &Agent,
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
    _agent: &Agent,
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}

fn generator_prototype_next(
    _agent: &Agent,
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}

fn generator_prototype_return(
    _agent: &Agent,
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}

fn generator_prototype_throw(
    _agent: &Agent,
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}

impl Agent {
    fn create_iter_result_object(&self, value: ECMAScriptValue, done: bool) -> Object {
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
        let object_prototype = self.intrinsic(IntrinsicId::ObjectPrototype);
        let obj = ordinary_object_create(self, Some(object_prototype), &[]);
        create_data_property_or_throw(self, &obj, "value", value).unwrap();
        create_data_property_or_throw(self, &obj, "done", done).unwrap();
        obj
    }
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
    fn get_prototype_of(&self, _agent: &Agent) -> Completion<Option<Object>> {
        Ok(ordinary_get_prototype_of(self))
    }

    // [[SetPrototypeOf]] ( V )
    //
    // The [[SetPrototypeOf]] internal method of an ordinary object O takes argument V (an Object or null). It performs
    // the following steps when called:
    //
    //  1. Return ! OrdinarySetPrototypeOf(O, V).
    fn set_prototype_of(&self, _agent: &Agent, obj: Option<Object>) -> Completion<bool> {
        Ok(ordinary_set_prototype_of(self, obj))
    }

    // [[IsExtensible]] ( )
    //
    // The [[IsExtensible]] internal method of an ordinary object O takes no arguments. It performs the following steps
    // when called:
    //
    //  1. Return ! OrdinaryIsExtensible(O).
    fn is_extensible(&self, _agent: &Agent) -> Completion<bool> {
        Ok(ordinary_is_extensible(self))
    }

    // [[PreventExtensions]] ( )
    //
    // The [[PreventExtensions]] internal method of an ordinary object O takes no arguments. It performs the following
    // steps when called:
    //
    //  1. Return ! OrdinaryPreventExtensions(O).
    fn prevent_extensions(&self, _agent: &Agent) -> Completion<bool> {
        Ok(ordinary_prevent_extensions(self))
    }

    // [[GetOwnProperty]] ( P )
    //
    // The [[GetOwnProperty]] internal method of an ordinary object O takes argument P (a property key). It performs the
    // following steps when called:
    //
    //  1. Return ! OrdinaryGetOwnProperty(O, P).
    fn get_own_property(&self, _agent: &Agent, key: &PropertyKey) -> Completion<Option<PropertyDescriptor>> {
        Ok(ordinary_get_own_property(self, key))
    }

    // [[DefineOwnProperty]] ( P, Desc )
    //
    // The [[DefineOwnProperty]] internal method of an ordinary object O takes arguments P (a property key) and Desc (a
    // Property Descriptor). It performs the following steps when called:
    //
    //  1. Return ? OrdinaryDefineOwnProperty(O, P, Desc).
    fn define_own_property(
        &self,
        agent: &Agent,
        key: PropertyKey,
        desc: PotentialPropertyDescriptor,
    ) -> Completion<bool> {
        ordinary_define_own_property(agent, self, key, desc)
    }

    // [[HasProperty]] ( P )
    //
    // The [[HasProperty]] internal method of an ordinary object O takes argument P (a property key). It performs the
    // following steps when called:
    //
    //  1. Return ? OrdinaryHasProperty(O, P).
    fn has_property(&self, agent: &Agent, key: &PropertyKey) -> Completion<bool> {
        ordinary_has_property(agent, self, key)
    }

    // [[Get]] ( P, Receiver )
    //
    // The [[Get]] internal method of an ordinary object O takes arguments P (a property key) and Receiver (an
    // ECMAScript language value). It performs the following steps when called:
    //
    //  1. Return ? OrdinaryGet(O, P, Receiver).
    fn get(&self, agent: &Agent, key: &PropertyKey, receiver: &ECMAScriptValue) -> Completion<ECMAScriptValue> {
        ordinary_get(agent, self, key, receiver)
    }

    // [[Set]] ( P, V, Receiver )
    //
    // The [[Set]] internal method of an ordinary object O takes arguments P (a property key), V (an ECMAScript language
    // value), and Receiver (an ECMAScript language value). It performs the following steps when called:
    //
    //  1. Return ? OrdinarySet(O, P, V, Receiver).
    fn set(&self, agent: &Agent, key: PropertyKey, v: ECMAScriptValue, receiver: &ECMAScriptValue) -> Completion<bool> {
        ordinary_set(agent, self, key, v, receiver)
    }

    // [[Delete]] ( P )
    //
    // The [[Delete]] internal method of an ordinary object O takes argument P (a property key). It performs the
    // following steps when called:
    //
    //  1. Return ? OrdinaryDelete(O, P).
    fn delete(&self, agent: &Agent, key: &PropertyKey) -> Completion<bool> {
        ordinary_delete(agent, self, key)
    }

    // [[OwnPropertyKeys]] ( )
    //
    // The [[OwnPropertyKeys]] internal method of an ordinary object O takes no arguments. It performs the following
    // steps when called:
    //
    // 1. Return ! OrdinaryOwnPropertyKeys(O).
    fn own_property_keys(&self, _agent: &Agent) -> Completion<Vec<PropertyKey>> {
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
    pub fn object(agent: &Agent, prototype: Option<Object>, state: GeneratorState, brand: &str) -> Object {
        Object {
            o: Rc::new(Self {
                common: RefCell::new(CommonObjectData::new(agent, prototype, true, GENERATOR_OBJECT_SLOTS)),
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

impl Agent {
    pub fn generator_validate(&self, generator: ECMAScriptValue, generator_brand: &str) -> Completion<GeneratorState> {
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
                o.o.to_generator_object()
                    .ok_or(GeneratorError::NotAGenerator)
                    .and_then(|gen| gen.validate(generator_brand))
            }
            _ => Err(GeneratorError::NotAGenerator),
        }
        .map_err(|e| create_type_error(self, e.to_string()))
    }
}

#[cfg(test)]
mod tests;
