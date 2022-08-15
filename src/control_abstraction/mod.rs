use super::*;

impl Agent {
    pub fn provision_iterator_prototype(&mut self, realm: &Rc<RefCell<Realm>>) {
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

    pub fn provision_generator_function_intrinsics(&mut self, realm: &Rc<RefCell<Realm>>) {
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
            Some(function.clone()),
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
        let generator_prototype = ordinary_object_create(self, Some(iterator_prototype.clone()), &[]);
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
    _agent: &mut Agent,
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
    _agent: &mut Agent,
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}

fn generator_prototype_next(
    _agent: &mut Agent,
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}

fn generator_prototype_return(
    _agent: &mut Agent,
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}

fn generator_prototype_throw(
    _agent: &mut Agent,
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}

#[cfg(test)]
mod tests;
