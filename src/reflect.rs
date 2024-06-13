use super::*;

pub fn provision_reflect_intrinsic(realm: &Rc<RefCell<Realm>>) {
    // The Reflect object:
    //
    //  * is %Reflect%.
    //  * is the initial value of the "Reflect" property of the global object.
    //  * is an ordinary object.
    //  * has a [[Prototype]] internal slot whose value is %Object.prototype%.
    //  * is not a function object.
    //  * does not have a [[Construct]] internal method; it cannot be used as a constructor with the new operator.
    //  * does not have a [[Call]] internal method; it cannot be invoked as a function.

    let object_prototype = realm.borrow().intrinsics.object_prototype.clone();
    let reflect = ordinary_object_create(Some(object_prototype), &[]);
    realm.borrow_mut().intrinsics.reflect = reflect.clone();

    let function_prototype = realm.borrow().intrinsics.function_prototype.clone();
    macro_rules! function_property {
        ( $steps:expr, $name:expr, $length:expr ) => {
            let key = PropertyKey::from($name);
            let function_object = create_builtin_function(
                $steps,
                false,
                f64::from($length),
                key.clone(),
                BUILTIN_FUNCTION_SLOTS,
                Some(realm.clone()),
                Some(function_prototype.clone()),
                None,
            );
            define_property_or_throw(
                &reflect,
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

    // Reflect.apply ( target, thisArgument, argumentsList )
    function_property!(reflect_apply, "apply", 3);
    // Reflect.construct ( target, argumentsList [ , newTarget ] )
    function_property!(reflect_construct, "construct", 2);
    // Reflect.defineProperty ( target, propertyKey, attributes )
    function_property!(reflect_define_property, "defineProperty", 3);
    // Reflect.deleteProperty ( target, propertyKey )
    function_property!(reflect_delete_property, "deleteProperty", 2);
    // Reflect.get ( target, propertyKey [ , receiver ] )
    function_property!(reflect_get, "get", 2);
    // Reflect.getOwnPropertyDescriptor ( target, propertyKey )
    function_property!(reflect_get_own_property_descriptor, "getOwnPropertyDescriptor", 2);
    // Reflect.getPrototypeOf ( target )
    function_property!(reflect_get_prototype_of, "getPrototypeOf", 1);
    // Reflect.has ( target, propertyKey )
    function_property!(reflect_has, "has", 2);
    // Reflect.isExtensible ( target )
    function_property!(reflect_is_extensible, "isExtensible", 1);
    // Reflect.ownKeys ( target )
    function_property!(reflect_own_keys, "ownKeys", 1);
    // Reflect.preventExtensions ( target )
    function_property!(reflect_prevent_extensions, "preventExtensions", 1);
    // Reflect.set ( target, propertyKey, V [ , receiver ] )
    function_property!(reflect_set, "set", 3);
    // Reflect.setPrototypeOf ( target, proto )
    function_property!(reflect_set_prototype_of, "setPrototypeOf", 2);

    // Reflect [ @@toStringTag ]
    define_property_or_throw(
        &reflect,
        wks(WksId::ToStringTag),
        PotentialPropertyDescriptor::new().value("Reflect").writable(false).enumerable(false).configurable(true),
    )
    .unwrap();
}

fn reflect_apply(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Reflect.apply ( target, thisArgument, argumentsList )
    // This function performs the following steps when called:
    //
    // 1. If IsCallable(target) is false, throw a TypeError exception.
    // 2. Let args be ? CreateListFromArrayLike(argumentsList).
    // 3. Perform PrepareForTailCall().
    // 4. Return ? Call(target, thisArgument, args).
    let mut args = FuncArgs::from(arguments);
    let target = args.next_arg();

    if is_callable(&target) {
        let this_argument = args.next_arg();
        let arguments_list = args.next_arg();
        let call_args = create_list_from_array_like(arguments_list, None)?;
        call(&target, &this_argument, &call_args)
    } else {
        Err(create_type_error("Reflect.apply requires a callable target"))
    }
}

fn reflect_construct(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Reflect.construct ( target, argumentsList [ , newTarget ] )
    // This function performs the following steps when called:
    //
    // 1. If IsConstructor(target) is false, throw a TypeError exception.
    // 2. If newTarget is not present, set newTarget to target.
    // 3. Else if IsConstructor(newTarget) is false, throw a TypeError exception.
    // 4. Let args be ? CreateListFromArrayLike(argumentsList).
    // 5. Return ? Construct(target, args, newTarget).
    let arg_count = arguments.len();
    let mut arguments = FuncArgs::from(arguments);
    let target = arguments.next_arg();
    if is_constructor(&target) {
        let arguments_list = arguments.next_arg();
        let new_target = arguments.next_arg();
        let target_as_object = to_object(target).expect("constructors should be objects");
        let nt = if arg_count <= 2 {
            &target_as_object
        } else {
            new_target
                .as_constructor()
                .ok_or_else(|| create_type_error("Reflect.construct: newTarget, if supplied, must be a constructor"))?
        };

        let args = create_list_from_array_like(arguments_list, None)?;
        construct(&target_as_object, &args, Some(nt))
    } else {
        Err(create_type_error("Reflect.construct: target must be a constructor"))
    }
}

fn reflect_define_property(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Reflect.defineProperty ( target, propertyKey, attributes )
    // This function performs the following steps when called:
    //
    // 1. If target is not an Object, throw a TypeError exception.
    // 2. Let key be ? ToPropertyKey(propertyKey).
    // 3. Let desc be ? ToPropertyDescriptor(attributes).
    // 4. Return ? target.[[DefineOwnProperty]](key, desc).
    let mut args = FuncArgs::from(arguments);
    if let ECMAScriptValue::Object(target) = args.next_arg() {
        let key = to_property_key(args.next_arg())?;
        let desc = to_property_descriptor(&args.next_arg())?;
        Ok(ECMAScriptValue::from(target.o.define_own_property(key, desc)?))
    } else {
        Err(create_type_error("Reflect.defineProperty: target must be an object"))
    }
}

fn reflect_delete_property(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Reflect.deleteProperty ( target, propertyKey )
    // This function performs the following steps when called:
    //
    // 1. If target is not an Object, throw a TypeError exception.
    // 2. Let key be ? ToPropertyKey(propertyKey).
    // 3. Return ? target.[[Delete]](key).
    let mut args = FuncArgs::from(arguments);
    if let ECMAScriptValue::Object(target) = args.next_arg() {
        let key = to_property_key(args.next_arg())?;
        Ok(ECMAScriptValue::from(target.o.delete(&key)?))
    } else {
        Err(create_type_error("Reflect.deleteProperty: target must be an object"))
    }
}

fn reflect_get(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Reflect.get ( target, propertyKey [ , receiver ] )
    // This function performs the following steps when called:
    //
    // 1. If target is not an Object, throw a TypeError exception.
    // 2. Let key be ? ToPropertyKey(propertyKey).
    // 3. If receiver is not present, then
    //      a. Set receiver to target.
    // 4. Return ? target.[[Get]](key, receiver).
    let num_args = arguments.len();
    let mut args = FuncArgs::from(arguments);
    if let ECMAScriptValue::Object(target) = args.next_arg() {
        let key = to_property_key(args.next_arg())?;
        let receiver = if num_args < 3 { ECMAScriptValue::Object(target.clone()) } else { args.next_arg() };
        target.o.get(&key, &receiver)
    } else {
        Err(create_type_error("Reflect.get: target must be an object"))
    }
}

fn reflect_get_own_property_descriptor(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Reflect.getOwnPropertyDescriptor ( target, propertyKey )
    // This function performs the following steps when called:
    //
    // 1. If target is not an Object, throw a TypeError exception.
    // 2. Let key be ? ToPropertyKey(propertyKey).
    // 3. Let desc be ? target.[[GetOwnProperty]](key).
    // 4. Return FromPropertyDescriptor(desc).
    let mut args = FuncArgs::from(arguments);
    if let ECMAScriptValue::Object(target) = args.next_arg() {
        let key = to_property_key(args.next_arg())?;
        let desc = target.o.get_own_property(&key)?;
        Ok(ECMAScriptValue::to_obj_or_undefined(from_property_descriptor(desc)))
    } else {
        Err(create_type_error("Reflect.getOwnPropertyDescriptor: target must be an object"))
    }
}

fn reflect_get_prototype_of(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Reflect.getPrototypeOf ( target )
    // This function performs the following steps when called:
    //
    // 1. If target is not an Object, throw a TypeError exception.
    // 2. Return ? target.[[GetPrototypeOf]]().
    let mut args = FuncArgs::from(arguments);
    if let ECMAScriptValue::Object(target) = args.next_arg() {
        Ok(ECMAScriptValue::to_obj_or_null(target.o.get_prototype_of()?))
    } else {
        Err(create_type_error("Reflect.getPrototypeOf: target must be an object"))
    }
}

fn reflect_has(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Reflect.has ( target, propertyKey )
    // This function performs the following steps when called:
    //
    // 1. If target is not an Object, throw a TypeError exception.
    // 2. Let key be ? ToPropertyKey(propertyKey).
    // 3. Return ? target.[[HasProperty]](key).
    let mut args = FuncArgs::from(arguments);
    if let ECMAScriptValue::Object(target) = args.next_arg() {
        let key = to_property_key(args.next_arg())?;
        Ok(ECMAScriptValue::from(target.o.has_property(&key)?))
    } else {
        Err(create_type_error("Reflect.has: target must be an object"))
    }
}

fn reflect_is_extensible(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Reflect.isExtensible ( target )
    // This function performs the following steps when called:
    //
    // 1. If target is not an Object, throw a TypeError exception.
    // 2. Return ? target.[[IsExtensible]]().
    let mut args = FuncArgs::from(arguments);
    if let ECMAScriptValue::Object(target) = args.next_arg() {
        Ok(ECMAScriptValue::from(target.o.is_extensible()?))
    } else {
        Err(create_type_error("Reflect.isExtensible: target must be an object"))
    }
}

fn reflect_own_keys(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Reflect.ownKeys ( target )
    // This function performs the following steps when called:
    //
    // 1. If target is not an Object, throw a TypeError exception.
    // 2. Let keys be ? target.[[OwnPropertyKeys]]().
    // 3. Return CreateArrayFromList(keys).
    let mut args = FuncArgs::from(arguments);
    if let ECMAScriptValue::Object(target) = args.next_arg() {
        let keys = target.o.own_property_keys()?.iter().map(ECMAScriptValue::from).collect::<Vec<_>>();
        Ok(ECMAScriptValue::from(create_array_from_list(&keys)))
    } else {
        Err(create_type_error("Reflect.ownKeys: target must be an object"))
    }
}

fn reflect_prevent_extensions(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Reflect.preventExtensions ( target )
    // This function performs the following steps when called:
    //
    // 1. If target is not an Object, throw a TypeError exception.
    // 2. Return ? target.[[PreventExtensions]]().
    let mut args = FuncArgs::from(arguments);
    if let ECMAScriptValue::Object(target) = args.next_arg() {
        Ok(ECMAScriptValue::from(target.o.prevent_extensions()?))
    } else {
        Err(create_type_error("Reflect.preventExtensions: target must be an object"))
    }
}

fn reflect_set(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Reflect.set ( target, propertyKey, V [ , receiver ] )
    // This function performs the following steps when called:
    //
    // 1. If target is not an Object, throw a TypeError exception.
    // 2. Let key be ? ToPropertyKey(propertyKey).
    // 3. If receiver is not present, then
    //      a. Set receiver to target.
    // 4. Return ? target.[[Set]](key, V, receiver).
    let num_args = arguments.len();
    let mut args = FuncArgs::from(arguments);
    if let ECMAScriptValue::Object(target) = args.next_arg() {
        let key = to_property_key(args.next_arg())?;
        let v = args.next_arg();
        let receiver = if num_args < 4 { ECMAScriptValue::from(target.clone()) } else { args.next_arg() };
        Ok(ECMAScriptValue::from(target.o.set(key, v, &receiver)?))
    } else {
        Err(create_type_error("Reflect.set: target must be an object"))
    }
}

fn reflect_set_prototype_of(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Reflect.setPrototypeOf ( target, proto )
    // This function performs the following steps when called:
    //
    // 1. If target is not an Object, throw a TypeError exception.
    // 2. If proto is not an Object and proto is not null, throw a TypeError exception.
    // 3. Return ? target.[[SetPrototypeOf]](proto).
    let mut args = FuncArgs::from(arguments);
    if let ECMAScriptValue::Object(target) = args.next_arg() {
        let proto = match args.next_arg() {
            ECMAScriptValue::Object(obj) => Some(obj),
            ECMAScriptValue::Null => None,
            _ => return Err(create_type_error("Reflect.setPrototypeOf: proto must be an object or null")),
        };
        Ok(ECMAScriptValue::from(target.o.set_prototype_of(proto)?))
    } else {
        Err(create_type_error("Reflect.setPrototypeOf: target must be an object"))
    }
}

#[cfg(test)]
mod tests;
