use super::*;

// Bound Function Exotic Objects
// A bound function exotic object is an exotic object that wraps another function object. A bound function exotic object
// is callable (it has a [[Call]] internal method and may have a [[Construct]] internal method). Calling a bound
// function exotic object generally results in a call of its wrapped function.
//
// An object is a bound function exotic object if its [[Call]] and (if applicable) [[Construct]] internal methods use
// the following implementations, and its other essential internal methods use the definitions found in 10.1. These
// methods are installed in BoundFunctionCreate.
//
// Bound function exotic objects do not have the internal slots of ECMAScript function objects listed in Table 30.
// Instead they have the internal slots listed in Table 31, in addition to [[Prototype]] and [[Extensible]].
//
// Table 31: Internal Slots of Bound Function Exotic Objects
// +=========================+======================+=================================================================+
// | Internal Slot           | Type                 | Description                                                     |
// +=========================+======================+=================================================================+
// | [[BoundTargetFunction]] | a callable Object    | The wrapped function object.                                    |
// +-------------------------+----------------------+-----------------------------------------------------------------+
// | [[BoundThis]]           | an ECMAScript        | The value that is always passed as the this value when calling  |
// |                         | language value       | the wrapped function.                                           |
// +-------------------------+----------------------+-----------------------------------------------------------------+
// | [[BoundArguments]]      | a List of ECMAScript | A list of values whose elements are used as the first arguments |
// |                         | language values      | to any call to the wrapped function.                            |
// +-------------------------+----------------------+-----------------------------------------------------------------+

#[derive(Debug)]
pub(crate) struct BoundFunctionObject {
    common: RefCell<CommonObjectData>,
    pub(crate) bound_target_function: Object,
    pub(crate) bound_this: ECMAScriptValue,
    pub(crate) bound_arguments: Box<[ECMAScriptValue]>,
}

impl<'a> From<&'a BoundFunctionObject> for &'a dyn ObjectInterface {
    fn from(obj: &'a BoundFunctionObject) -> Self {
        obj
    }
}

impl ObjectInterface for BoundFunctionObject {
    fn as_object_interface(&self) -> &dyn ObjectInterface {
        self
    }
    fn common_object_data(&self) -> &RefCell<CommonObjectData> {
        &self.common
    }

    fn to_callable_obj(&self) -> Option<&dyn CallableObject> {
        Some(self)
    }

    fn is_callable_obj(&self) -> bool {
        true
    }

    fn kind(&self) -> ObjectTag {
        ObjectTag::Function
    }

    fn to_constructable(&self) -> Option<&dyn CallableObject> {
        let bound_target = &self.bound_target_function;
        if bound_target.is_constructor() { Some(self) } else { None }
    }

    fn to_bound_function_object(&self) -> Option<&BoundFunctionObject> {
        Some(self)
    }
}

impl BoundFunctionObject {
    pub(crate) fn new(
        prototype: Option<Object>,
        target_function: Object,
        bound_this: ECMAScriptValue,
        bound_args: &[ECMAScriptValue],
    ) -> Self {
        Self {
            common: RefCell::new(CommonObjectData::new(prototype, true, BOUND_FUNCTION_OBJECT_SLOTS)),
            bound_target_function: target_function,
            bound_this,
            bound_arguments: Box::from(bound_args),
        }
    }

    /// Create a new Bound Function Exotic Object
    ///
    /// Parameters:
    ///   * target_function: This is the function to bind
    ///   * bound_this: This is the "this" values used when the bound function is called
    ///   * bound_args: These are the first arguments passed to the bound function when it is called; arguments in the
    ///     call itself follow.
    ///
    /// See also BoundFunctionCreate in [ECMA-262](https://tc39.es/ecma262/multipage/ordinary-and-exotic-objects-behaviours.html#sec-boundfunctioncreate).
    pub(crate) fn create(
        target_function: Object,
        bound_this: ECMAScriptValue,
        bound_args: &[ECMAScriptValue],
    ) -> Completion<Object> {
        // BoundFunctionCreate ( targetFunction, boundThis, boundArgs )
        // The abstract operation BoundFunctionCreate takes arguments targetFunction (a function object), boundThis (an
        // ECMAScript language value), and boundArgs (a List of ECMAScript language values) and returns either a normal
        // completion containing a function object or a throw completion. It is used to specify the creation of new
        // bound function exotic objects. It performs the following steps when called:
        //
        //  1. Let proto be ? targetFunction.[[GetPrototypeOf]]().
        //  2. Let internalSlotsList be the list-concatenation of « [[Prototype]], [[Extensible]] » and the internal
        //     slots listed in Table 31.
        //  3. Let obj be MakeBasicObject(internalSlotsList).
        //  4. Set obj.[[Prototype]] to proto.
        //  5. Set obj.[[Call]] as described in 10.4.1.1.
        //  6. If IsConstructor(targetFunction) is true, then
        //      a. Set obj.[[Construct]] as described in 10.4.1.2.
        //  7. Set obj.[[BoundTargetFunction]] to targetFunction.
        //  8. Set obj.[[BoundThis]] to boundThis.
        //  9. Set obj.[[BoundArguments]] to boundArgs.
        //  10. Return obj.
        let proto = target_function.o.get_prototype_of()?;
        Ok(Object { o: Rc::new(Self::new(proto, target_function, bound_this, bound_args)) })
    }
}

impl CallableObject for BoundFunctionObject {
    fn call(&self, _: &Object, _: &ECMAScriptValue, arguments_list: &[ECMAScriptValue]) {
        // [[Call]] ( thisArgument, argumentsList )
        // The [[Call]] internal method of a bound function exotic object F takes arguments thisArgument (an ECMAScript
        // language value) and argumentsList (a List of ECMAScript language values) and returns either a normal
        // completion containing an ECMAScript language value or a throw completion. It performs the following steps
        // when called:
        //
        //  1. Let target be F.[[BoundTargetFunction]].
        //  2. Let boundThis be F.[[BoundThis]].
        //  3. Let boundArgs be F.[[BoundArguments]].
        //  4. Let args be the list-concatenation of boundArgs and argumentsList.
        //  5. Return ? Call(target, boundThis, args).
        let target = ECMAScriptValue::from(self.bound_target_function.clone());
        let bound_this = &self.bound_this;
        let bound_args = self.bound_arguments.as_ref();
        let mut args = Vec::from(bound_args);
        args.extend_from_slice(arguments_list);
        let result = call(&target, bound_this, &args);
        ec_push(result.map(NormalCompletion::from));
    }

    fn construct(&self, self_object: &Object, arguments_list: &[ECMAScriptValue], new_target: &Object) {
        // [[Construct]] ( argumentsList, newTarget )
        // The [[Construct]] internal method of a bound function exotic object F takes arguments argumentsList (a List
        // of ECMAScript language values) and newTarget (a constructor) and returns either a normal completion
        // containing an Object or a throw completion. It performs the following steps when called:
        //
        //  1. Let target be F.[[BoundTargetFunction]].
        //  2. Assert: IsConstructor(target) is true.
        //  3. Let boundArgs be F.[[BoundArguments]].
        //  4. Let args be the list-concatenation of boundArgs and argumentsList.
        //  5. If SameValue(F, newTarget) is true, set newTarget to target.
        //  6. Return ? Construct(target, args, newTarget).
        let target = &self.bound_target_function.clone();
        let bound_args = self.bound_arguments.as_ref();
        let mut args = Vec::from(bound_args);
        args.extend_from_slice(arguments_list);
        let nt = if self_object == new_target { target } else { new_target };
        let result = construct(target, &args, Some(nt));
        ec_push(result.map(NormalCompletion::from));
    }

    fn end_evaluation(&self, _: FullCompletion) {
        panic!("end_evaluation called for bound function")
    }

    fn complete_call(&self) -> Completion<ECMAScriptValue> {
        ec_pop()
            .expect("Call must return a Completion")
            .map(|nc| ECMAScriptValue::try_from(nc).expect("Call must return a language value"))
    }
}
