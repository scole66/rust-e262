use super::*;
use counter::Counter;

#[derive(Debug)]
pub struct ProxyItems {
    pub proxy_handler: Object,
    pub proxy_target: Object,
}
#[derive(Debug)]
pub struct ProxyObject {
    common: RefCell<CommonObjectData>,
    pub proxy_items: RefCell<Option<ProxyItems>>,
}

impl ObjectInterface for ProxyObject {
    fn common_object_data(&self) -> &RefCell<CommonObjectData> {
        &self.common
    }
    fn uses_ordinary_get_prototype_of(&self) -> bool {
        false
    }
    fn id(&self) -> usize {
        self.common.borrow().objid
    }
    fn to_proxy_object(&self) -> Option<&ProxyObject> {
        Some(self)
    }
    fn is_proxy_object(&self) -> bool {
        true
    }

    fn to_callable_obj(&self) -> Option<&dyn CallableObject> {
        let proxy_items = self.proxy_items.borrow();

        if proxy_items.as_ref().map_or(false, |items| items.proxy_target.o.is_callable_obj()) {
            Some(self)
        } else {
            None
        }
    }

    fn is_callable_obj(&self) -> bool {
        let proxy_items = self.proxy_items.borrow();

        proxy_items.as_ref().map_or(false, |items| items.proxy_target.o.is_callable_obj())
    }
    fn kind(&self) -> ObjectTag {
        let proxy_items = self.proxy_items.borrow();

        proxy_items.as_ref().map_or(ObjectTag::Object, |items| items.proxy_target.o.kind())
    }

    fn to_constructable(&self) -> Option<&dyn CallableObject> {
        let proxy_items = self.proxy_items.borrow();
        if proxy_items.as_ref().map_or(false, |items| items.proxy_target.is_constructor()) {
            Some(self)
        } else {
            None
        }
    }

    fn get_prototype_of(&self) -> Completion<Option<Object>> {
        // [[GetPrototypeOf]] ( )
        // The [[GetPrototypeOf]] internal method of a Proxy exotic object O takes no arguments and returns
        // either a normal completion containing either an Object or null, or a throw completion. It performs
        // the following steps when called:
        //
        //  1. Perform ? ValidateNonRevokedProxy(O).
        //  2. Let target be O.[[ProxyTarget]].
        //  3. Let handler be O.[[ProxyHandler]].
        //  4. Assert: handler is an Object.
        //  5. Let trap be ? GetMethod(handler, "getPrototypeOf").
        //  6. If trap is undefined, then
        //      a. Return ? target.[[GetPrototypeOf]]().
        //  7. Let handlerProto be ? Call(trap, handler, « target »).
        //  8. If handlerProto is not an Object and handlerProto is not null, throw a TypeError exception.
        //  9. Let extensibleTarget be ? IsExtensible(target).
        //  10. If extensibleTarget is true, return handlerProto.
        //  11. Let targetProto be ? target.[[GetPrototypeOf]]().
        //  12. If SameValue(handlerProto, targetProto) is false, throw a TypeError exception.
        //  13. Return handlerProto.
        //
        // NOTE
        // [[GetPrototypeOf]] for Proxy objects enforces the following invariants:
        //
        // * The result of [[GetPrototypeOf]] must be either an Object or null.
        // * If the target object is not extensible, [[GetPrototypeOf]] applied to the Proxy object must
        //   return the same value as [[GetPrototypeOf]] applied to the Proxy object's target object.
        let (proxy_target, proxy_handler) = self.validate_non_revoked()?;
        let target = &proxy_target;
        let handler = ECMAScriptValue::from(proxy_handler);
        let trap = handler.get_method(&"getPrototypeOf".into())?;
        if matches!(trap, ECMAScriptValue::Undefined) {
            return target.o.get_prototype_of();
        }
        let handler_proto = match call(&trap, &handler, &[target.clone().into()])? {
            ECMAScriptValue::Null => None,
            ECMAScriptValue::Object(obj) => Some(obj),
            ECMAScriptValue::Undefined
            | ECMAScriptValue::Boolean(_)
            | ECMAScriptValue::String(_)
            | ECMAScriptValue::Number(_)
            | ECMAScriptValue::BigInt(_)
            | ECMAScriptValue::Symbol(_) => {
                return Err(create_type_error("proxy error: getPrototypeOf must return an object or null"))
            }
        };

        if !is_extensible(target)? {
            let target_proto = target.o.get_prototype_of()?;
            if target_proto != handler_proto {
                return Err(create_type_error("proxy error: non-extensible targets cannot change prototypes"));
            }
        }
        Ok(handler_proto)
    }

    fn set_prototype_of(&self, v: Option<Object>) -> Completion<bool> {
        // [[SetPrototypeOf]] ( V )
        // The [[SetPrototypeOf]] internal method of a Proxy exotic object O takes argument V (an Object or
        // null) and returns either a normal completion containing a Boolean or a throw completion. It
        // performs the following steps when called:
        //
        //  1. Perform ? ValidateNonRevokedProxy(O).
        //  2. Let target be O.[[ProxyTarget]].
        //  3. Let handler be O.[[ProxyHandler]].
        //  4. Assert: handler is an Object.
        //  5. Let trap be ? GetMethod(handler, "setPrototypeOf").
        //  6. If trap is undefined, then
        //      a. Return ? target.[[SetPrototypeOf]](V).
        //  7. Let booleanTrapResult be ToBoolean(? Call(trap, handler, « target, V »)).
        //  8. If booleanTrapResult is false, return false.
        //  9. Let extensibleTarget be ? IsExtensible(target).
        //  10. If extensibleTarget is true, return true.
        //  11. Let targetProto be ? target.[[GetPrototypeOf]]().
        //  12. If SameValue(V, targetProto) is false, throw a TypeError exception.
        //  13. Return true.
        //
        // NOTE
        // [[SetPrototypeOf]] for Proxy objects enforces the following invariants:
        //
        // * The result of [[SetPrototypeOf]] is a Boolean value.
        // * If the target object is not extensible, the argument value must be the same as the result of
        //   [[GetPrototypeOf]] applied to target object.
        let (proxy_target, proxy_handler) = self.validate_non_revoked()?;
        let target = &proxy_target;
        let handler = ECMAScriptValue::from(proxy_handler);
        let trap = handler.get_method(&"setPrototypeOf".into())?;
        if matches!(trap, ECMAScriptValue::Undefined) {
            return target.o.set_prototype_of(v);
        }
        let boolean_trap_result = to_boolean(call(
            &trap,
            &handler,
            &[
                ECMAScriptValue::from(target),
                match &v {
                    Some(obj) => ECMAScriptValue::from(obj),
                    None => ECMAScriptValue::Null,
                },
            ],
        )?);
        if !boolean_trap_result {
            return Ok(false);
        }
        let extensible_target = is_extensible(target)?;
        if extensible_target {
            return Ok(true);
        }
        let target_proto = target.o.get_prototype_of()?;
        if target_proto != v {
            return Err(create_type_error("proxy error: can't change prototype for non-extensible object"));
        }
        Ok(true)
    }

    fn is_extensible(&self) -> Completion<bool> {
        // [[IsExtensible]] ( )
        // The [[IsExtensible]] internal method of a Proxy exotic object O takes no arguments and returns
        // either a normal completion containing a Boolean or a throw completion. It performs the following
        // steps when called:
        //
        //  1. Perform ? ValidateNonRevokedProxy(O).
        //  2. Let target be O.[[ProxyTarget]].
        //  3. Let handler be O.[[ProxyHandler]].
        //  4. Assert: handler is an Object.
        //  5. Let trap be ? GetMethod(handler, "isExtensible").
        //  6. If trap is undefined, then
        //      a. Return ? IsExtensible(target).
        //  7. Let booleanTrapResult be ToBoolean(? Call(trap, handler, « target »)).
        //  8. Let targetResult be ? IsExtensible(target).
        //  9. If SameValue(booleanTrapResult, targetResult) is false, throw a TypeError exception.
        //  10. Return booleanTrapResult.
        // NOTE
        // [[IsExtensible]] for Proxy objects enforces the following invariants:
        //
        // * The result of [[IsExtensible]] is a Boolean value.
        // * [[IsExtensible]] applied to the Proxy object must return the same value as [[IsExtensible]]
        //   applied to the Proxy object's target object with the same argument.
        let (proxy_target, proxy_handler) = self.validate_non_revoked()?;
        let target = &proxy_target;
        let handler = ECMAScriptValue::from(proxy_handler);
        let trap = handler.get_method(&"isExtensible".into())?;
        if matches!(trap, ECMAScriptValue::Undefined) {
            return target.o.is_extensible();
        }
        let boolean_trap_result = to_boolean(call(&trap, &handler, &[ECMAScriptValue::from(target)])?);
        let target_result = is_extensible(target)?;
        if boolean_trap_result != target_result {
            return Err(create_type_error("proxy error: extensible property cannot be changed"));
        }
        Ok(boolean_trap_result)
    }

    fn prevent_extensions(&self) -> Completion<bool> {
        // [[PreventExtensions]] ( )
        // The [[PreventExtensions]] internal method of a Proxy exotic object O takes no arguments and returns
        // either a normal completion containing a Boolean or a throw completion. It performs the following
        // steps when called:
        //
        //  1. Perform ? ValidateNonRevokedProxy(O).
        //  2. Let target be O.[[ProxyTarget]].
        //  3. Let handler be O.[[ProxyHandler]].
        //  4. Assert: handler is an Object.
        //  5. Let trap be ? GetMethod(handler, "preventExtensions").
        //  6. If trap is undefined, then
        //      a. Return ? target.[[PreventExtensions]]().
        //  7. Let booleanTrapResult be ToBoolean(? Call(trap, handler, « target »)).
        //  8. If booleanTrapResult is true, then
        //      a. Let extensibleTarget be ? IsExtensible(target).
        //      b. If extensibleTarget is true, throw a TypeError exception.
        //  9. Return booleanTrapResult.
        //
        // NOTE
        // [[PreventExtensions]] for Proxy objects enforces the following invariants:
        //
        //  * The result of [[PreventExtensions]] is a Boolean value.
        //  * [[PreventExtensions]] applied to the Proxy object only returns true if [[IsExtensible]] applied
        //    to the Proxy object's target object is false.
        let (proxy_target, proxy_handler) = self.validate_non_revoked()?;
        let target = &proxy_target;
        let handler = ECMAScriptValue::from(proxy_handler);
        let trap = handler.get_method(&"preventExtensions".into())?;
        if matches!(trap, ECMAScriptValue::Undefined) {
            return target.o.prevent_extensions();
        }
        let boolean_trap_result = to_boolean(call(&trap, &handler, &[ECMAScriptValue::from(target)])?);
        if boolean_trap_result {
            let extensible_target = is_extensible(target)?;
            if extensible_target {
                return Err(create_type_error("proxy error: extensible property cannot be changed"));
            }
        }
        Ok(boolean_trap_result)
    }

    fn get_own_property(&self, key: &PropertyKey) -> Completion<Option<PropertyDescriptor>> {
        // [[GetOwnProperty]] ( P )
        // The [[GetOwnProperty]] internal method of a Proxy exotic object O takes argument P (a property key)
        // and returns either a normal completion containing either a Property Descriptor or undefined, or a
        // throw completion. It performs the following steps when called:
        //
        //  1. Perform ? ValidateNonRevokedProxy(O).
        //  2. Let target be O.[[ProxyTarget]].
        //  3. Let handler be O.[[ProxyHandler]].
        //  4. Assert: handler is an Object.
        //  5. Let trap be ? GetMethod(handler, "getOwnPropertyDescriptor").
        //  6. If trap is undefined, then
        //      a. Return ? target.[[GetOwnProperty]](P).
        //  7. Let trapResultObj be ? Call(trap, handler, « target, P »).
        //  8. If trapResultObj is not an Object and trapResultObj is not undefined, throw a TypeError
        //     exception.
        //  9. Let targetDesc be ? target.[[GetOwnProperty]](P).
        //  10. If trapResultObj is undefined, then
        //      a. If targetDesc is undefined, return undefined.
        //      b. If targetDesc.[[Configurable]] is false, throw a TypeError exception.
        //      c. Let extensibleTarget be ? IsExtensible(target).
        //      d. If extensibleTarget is false, throw a TypeError exception.
        //      e. Return undefined.
        //  11. Let extensibleTarget be ? IsExtensible(target).
        //  12. Let resultDesc be ? ToPropertyDescriptor(trapResultObj).
        //  13. Perform CompletePropertyDescriptor(resultDesc).
        //  14. Let valid be IsCompatiblePropertyDescriptor(extensibleTarget, resultDesc, targetDesc).
        //  15. If valid is false, throw a TypeError exception.
        //  16. If resultDesc.[[Configurable]] is false, then
        //      a. If targetDesc is undefined or targetDesc.[[Configurable]] is true, then
        //          i. Throw a TypeError exception.
        //      b. If resultDesc has a [[Writable]] field and resultDesc.[[Writable]] is false, then
        //          i. Assert: targetDesc has a [[Writable]] field.
        //          ii. If targetDesc.[[Writable]] is true, throw a TypeError exception.
        //  17. Return resultDesc.
        //
        // NOTE
        // [[GetOwnProperty]] for Proxy objects enforces the following invariants:
        //
        //  * The result of [[GetOwnProperty]] must be either an Object or undefined.
        //  * A property cannot be reported as non-existent, if it exists as a non-configurable own property
        //    of the target object.
        //  * A property cannot be reported as non-existent, if it exists as an own property of a
        //    non-extensible target object.
        //  * A property cannot be reported as existent, if it does not exist as an own property of the target
        //    object and the target object is not extensible.
        //  * A property cannot be reported as non-configurable, unless it exists as a non-configurable own
        //    property of the target object.
        //  * A property cannot be reported as both non-configurable and non-writable, unless it exists as a
        //    non-configurable, non-writable own property of the target object.
        let (proxy_target, proxy_handler) = self.validate_non_revoked()?;
        let target = &proxy_target;
        let handler = ECMAScriptValue::from(proxy_handler);
        let trap = handler.get_method(&"getOwnPropertyDescriptor".into())?;
        if matches!(trap, ECMAScriptValue::Undefined) {
            return target.o.get_own_property(key);
        }
        let trap_result_obj = call(&trap, &handler, &[ECMAScriptValue::from(target), ECMAScriptValue::from(key)])?;
        if !matches!(trap_result_obj, ECMAScriptValue::Undefined)
            && !matches!(trap_result_obj, ECMAScriptValue::Object(_))
        {
            return Err(create_type_error(
                "proxy error: The result of [[GetOwnProperty]] must be either an Object or undefined.",
            ));
        }
        let target_desc = target.o.get_own_property(key)?;
        if matches!(trap_result_obj, ECMAScriptValue::Undefined) {
            return match target_desc {
                None => Ok(None),
                Some(pd) => {
                    if pd.configurable {
                        let extensible_target = is_extensible(target)?;
                        if extensible_target {
                            Ok(None)
                        } else {
                            Err(create_type_error("proxy error: A property cannot be reported as existent, if it does not exist as an own property of the target object and the target object is not extensible."))
                        }
                    } else {
                        Err(create_type_error("proxy error: A property cannot be reported as non-existent, if it exists as a non-configurable own property of the target object."))
                    }
                }
            };
        }
        let extensible_target = is_extensible(target)?;
        let result_desc = to_property_descriptor(&trap_result_obj)?.complete();
        let valid =
            is_compatible_property_descriptor(extensible_target, result_desc.clone().into(), target_desc.as_ref());
        if !valid {
            return Err(create_type_error("proxy error: A property cannot be reported as existent, if it does not exist as an own property of the target object and the target object is not extensible."));
        }
        if !result_desc.configurable {
            if target_desc.is_none() || target_desc.as_ref().unwrap().configurable {
                return Err(create_type_error("proxy error: A property cannot be reported as non-configurable, unless it exists as a non-configurable own property of the target object."));
            }
            if !result_desc.is_writable().unwrap_or(true) && target_desc.as_ref().unwrap().is_writable().unwrap() {
                return Err(create_type_error("proxy error: A property cannot be reported as both non-configurable and non-writable, unless it exists as a non-configurable, non-writable own property of the target object."));
            }
        }

        Ok(Some(result_desc))
    }

    fn define_own_property(&self, key: PropertyKey, desc: PotentialPropertyDescriptor) -> Completion<bool> {
        // [[DefineOwnProperty]] ( P, Desc )
        // The [[DefineOwnProperty]] internal method of a Proxy exotic object O takes arguments P (a property
        // key) and Desc (a Property Descriptor) and returns either a normal completion containing a Boolean
        // or a throw completion. It performs the following steps when called:
        //
        //  1. Perform ? ValidateNonRevokedProxy(O).
        //  2. Let target be O.[[ProxyTarget]].
        //  3. Let handler be O.[[ProxyHandler]].
        //  4. Assert: handler is an Object.
        //  5. Let trap be ? GetMethod(handler, "defineProperty").
        //  6. If trap is undefined, then
        //      a. Return ? target.[[DefineOwnProperty]](P, Desc).
        //  7. Let descObj be FromPropertyDescriptor(Desc).
        //  8. Let booleanTrapResult be ToBoolean(? Call(trap, handler, « target, P, descObj »)).
        //  9. If booleanTrapResult is false, return false.
        //  10. Let targetDesc be ? target.[[GetOwnProperty]](P).
        //  11. Let extensibleTarget be ? IsExtensible(target).
        //  12. If Desc has a [[Configurable]] field and Desc.[[Configurable]] is false, then
        //      a. Let settingConfigFalse be true.
        //  13. Else,
        //      a. Let settingConfigFalse be false.
        //  14. If targetDesc is undefined, then
        //      a. If extensibleTarget is false, throw a TypeError exception.
        //      b. If settingConfigFalse is true, throw a TypeError exception.
        //  15. Else,
        //      a. If IsCompatiblePropertyDescriptor(extensibleTarget, Desc, targetDesc) is false, throw a
        //         TypeError exception.
        //      b. If settingConfigFalse is true and targetDesc.[[Configurable]] is true, throw a TypeError
        //         exception.
        //      c. If IsDataDescriptor(targetDesc) is true, targetDesc.[[Configurable]] is false, and
        //         targetDesc.[[Writable]] is true, then
        //          i. If Desc has a [[Writable]] field and Desc.[[Writable]] is false, throw a TypeError
        //             exception.
        //  16. Return true.
        //
        // NOTE
        // [[DefineOwnProperty]] for Proxy objects enforces the following invariants:
        //
        //  * The result of [[DefineOwnProperty]] is a Boolean value.
        //  * A property cannot be added, if the target object is not extensible.
        //  * A property cannot be non-configurable, unless there exists a corresponding non-configurable own
        //    property of the target object.
        //  * A non-configurable property cannot be non-writable, unless there exists a corresponding
        //    non-configurable, non-writable own property of the target object.
        //  * If a property has a corresponding target object property then applying the Property Descriptor
        //    of the property to the target object using [[DefineOwnProperty]] will not throw an exception.
        let (proxy_target, proxy_handler) = self.validate_non_revoked()?;
        let target = &proxy_target;
        let handler = ECMAScriptValue::from(proxy_handler);
        let trap = handler.get_method(&"defineProperty".into())?;
        if matches!(trap, ECMAScriptValue::Undefined) {
            return target.o.define_own_property(key, desc);
        }
        let desc_obj = from_property_descriptor(Some(desc.clone())).expect("desc object comes back");
        let boolean_trap_result =
            to_boolean(call(&trap, &handler, &[target.into(), key.clone().into(), desc_obj.into()])?);
        if !boolean_trap_result {
            return Ok(false);
        }
        let target_desc = target.o.get_own_property(&key)?;
        let extensible_target = is_extensible(target)?;
        let setting_config_false = match &desc.configurable {
            Some(configurable) => !configurable,
            None => false,
        };
        match target_desc {
            Some(target_desc) => {
                if !is_compatible_property_descriptor(extensible_target, desc.clone(), Some(&target_desc)) {
                    return Err(create_type_error("proxy error: If a property has a corresponding target object property then applying the Property Descriptor of the property to the target object using [[DefineOwnProperty]] will not throw an exception."));
                }
                if setting_config_false && target_desc.configurable {
                    return Err(create_type_error("proxy error: A property cannot be non-configurable, unless there exists a corresponding non-configurable own property of the target object."));
                }
                if matches!(
                    target_desc,
                    PropertyDescriptor {
                        property: PropertyKind::Data(DataProperty { value: _, writable: true }),
                        configurable: false,
                        enumerable: _,
                        spot: _
                    }
                ) && matches!(desc.writable, Some(false))
                {
                    return Err(create_type_error("proxy error: A non-configurable property cannot be non-writable, unless there exists a corresponding non-configurable, non-writable own property of the target object."));
                }
            }
            None => {
                if !extensible_target {
                    return Err(create_type_error(
                        "proxy error: A property cannot be added, if the target object is not extensible.",
                    ));
                }
                if setting_config_false {
                    return Err(create_type_error("proxy error: A property cannot be non-configurable, unless there exists a corresponding non-configurable own property of the target object."));
                }
            }
        }

        Ok(true)
    }

    fn has_property(&self, key: &PropertyKey) -> Completion<bool> {
        // [[HasProperty]] ( P )
        // The [[HasProperty]] internal method of a Proxy exotic object O takes argument P (a property key)
        // and returns either a normal completion containing a Boolean or a throw completion. It performs the
        // following steps when called:
        //
        //  1. Perform ? ValidateNonRevokedProxy(O).
        //  2. Let target be O.[[ProxyTarget]].
        //  3. Let handler be O.[[ProxyHandler]].
        //  4. Assert: handler is an Object.
        //  5. Let trap be ? GetMethod(handler, "has").
        //  6. If trap is undefined, then
        //      a. Return ? target.[[HasProperty]](P).
        //  7. Let booleanTrapResult be ToBoolean(? Call(trap, handler, « target, P »)).
        //  8. If booleanTrapResult is false, then
        //      a. Let targetDesc be ? target.[[GetOwnProperty]](P).
        //      b. If targetDesc is not undefined, then
        //          i. If targetDesc.[[Configurable]] is false, throw a TypeError exception.
        //          ii. Let extensibleTarget be ? IsExtensible(target).
        //          iii. If extensibleTarget is false, throw a TypeError exception.
        //  9. Return booleanTrapResult.
        // NOTE
        // [[HasProperty]] for Proxy objects enforces the following invariants:
        //
        //  * The result of [[HasProperty]] is a Boolean value.
        //  * A property cannot be reported as non-existent, if it exists as a non-configurable own property
        //    of the target object.
        //  * A property cannot be reported as non-existent, if it exists as an own property of the target
        //    object and the target object is not extensible.
        let (proxy_target, proxy_handler) = self.validate_non_revoked()?;
        let target = &proxy_target;
        let handler = ECMAScriptValue::from(proxy_handler);
        let trap = handler.get_method(&"has".into())?;
        if matches!(trap, ECMAScriptValue::Undefined) {
            return target.o.has_property(key);
        }
        let boolean_trap_result = to_boolean(call(&trap, &handler, &[target.into(), key.into()])?);
        if !boolean_trap_result {
            let target_desc = target.o.get_own_property(key)?;
            if let Some(target_desc) = target_desc {
                if !target_desc.configurable {
                    return Err(create_type_error("proxy error: A property cannot be reported as non-existent, if it exists as a non-configurable own property of the target object."));
                }
                if !is_extensible(target)? {
                    return Err(create_type_error("proxy error: A property cannot be reported as non-existent, if it exists as an own property of the target object and the target object is not extensible."));
                }
            }
        }
        Ok(boolean_trap_result)
    }

    fn get(&self, key: &PropertyKey, receiver: &ECMAScriptValue) -> Completion<ECMAScriptValue> {
        // [[Get]] ( P, Receiver )
        // The [[Get]] internal method of a Proxy exotic object O takes arguments P (a property key) and
        // Receiver (an ECMAScript language value) and returns either a normal completion containing an
        // ECMAScript language value or a throw completion. It performs the following steps when called:
        //
        //  1. Perform ? ValidateNonRevokedProxy(O).
        //  2. Let target be O.[[ProxyTarget]].
        //  3. Let handler be O.[[ProxyHandler]].
        //  4. Assert: handler is an Object.
        //  5. Let trap be ? GetMethod(handler, "get").
        //  6. If trap is undefined, then
        //      a. Return ? target.[[Get]](P, Receiver).
        //  7. Let trapResult be ? Call(trap, handler, « target, P, Receiver »).
        //  8. Let targetDesc be ? target.[[GetOwnProperty]](P).
        //  9. If targetDesc is not undefined and targetDesc.[[Configurable]] is false, then
        //      a. If IsDataDescriptor(targetDesc) is true and targetDesc.[[Writable]] is false, then
        //          i. If SameValue(trapResult, targetDesc.[[Value]]) is false, throw a TypeError exception.
        //      b. If IsAccessorDescriptor(targetDesc) is true and targetDesc.[[Get]] is undefined, then
        //          i. If trapResult is not undefined, throw a TypeError exception.
        //  10. Return trapResult.
        // NOTE
        // [[Get]] for Proxy objects enforces the following invariants:
        //
        //  * The value reported for a property must be the same as the value of the corresponding target
        //    object property if the target object property is a non-writable, non-configurable own data
        //    property.
        //  * The value reported for a property must be undefined if the corresponding target object property
        //    is a non-configurable own accessor property that has undefined as its [[Get]] attribute.
        let (proxy_target, proxy_handler) = self.validate_non_revoked()?;
        let target = &proxy_target;
        let handler = ECMAScriptValue::from(proxy_handler);
        let trap = handler.get_method(&"get".into())?;
        if matches!(trap, ECMAScriptValue::Undefined) {
            return target.o.get(key, receiver);
        }
        let trap_result = call(&trap, &handler, &[target.into(), key.into(), receiver.clone()])?;
        let target_desc = target.o.get_own_property(key)?;
        if let Some(PropertyDescriptor {
            configurable: false,
            property: PropertyKind::Data(DataProperty { writable: false, value }),
            ..
        }) = target_desc
        {
            if !trap_result.same_value(&value) {
                return Err(create_type_error("proxy error: The value reported for a property must be the same as the value of the corresponding target object property if the target object property is a non-writable, non-configurable own data property."));
            }
        } else if matches!(
            target_desc,
            Some(PropertyDescriptor {
                configurable: false,
                property: PropertyKind::Accessor(AccessorProperty { get: ECMAScriptValue::Undefined, .. }),
                ..
            })
        ) && !matches!(trap_result, ECMAScriptValue::Undefined)
        {
            return Err(create_type_error("proxy error: The value reported for a property must be undefined if the corresponding target object property is a non-configurable own accessor property that has undefined as its [[Get]] attribute."));
        }

        Ok(trap_result)
    }

    fn set(&self, key: PropertyKey, value: ECMAScriptValue, receiver: &ECMAScriptValue) -> Completion<bool> {
        // [[Set]] ( P, V, Receiver )
        // The [[Set]] internal method of a Proxy exotic object O takes arguments P (a property key), V (an ECMAScript
        // language value), and Receiver (an ECMAScript language value) and returns either a normal completion
        // containing a Boolean or a throw completion. It performs the following steps when called:
        //
        //  1. Perform ? ValidateNonRevokedProxy(O).
        //  2. Let target be O.[[ProxyTarget]].
        //  3. Let handler be O.[[ProxyHandler]].
        //  4. Assert: handler is an Object.
        //  5. Let trap be ? GetMethod(handler, "set").
        //  6. If trap is undefined, then
        //      a. Return ? target.[[Set]](P, V, Receiver).
        //  7. Let booleanTrapResult be ToBoolean(? Call(trap, handler, « target, P, V, Receiver »)).
        //  8. If booleanTrapResult is false, return false.
        //  9. Let targetDesc be ? target.[[GetOwnProperty]](P).
        //  10. If targetDesc is not undefined and targetDesc.[[Configurable]] is false, then
        //      a. If IsDataDescriptor(targetDesc) is true and targetDesc.[[Writable]] is false, then
        //          i. If SameValue(V, targetDesc.[[Value]]) is false, throw a TypeError exception.
        //      b. If IsAccessorDescriptor(targetDesc) is true, then
        //          i. If targetDesc.[[Set]] is undefined, throw a TypeError exception.
        //  11. Return true.
        //
        // NOTE
        // [[Set]] for Proxy objects enforces the following invariants:
        //
        //  * The result of [[Set]] is a Boolean value.
        //  * Cannot change the value of a property to be different from the value of the corresponding target object
        //    property if the corresponding target object property is a non-writable, non-configurable own data
        //    property.
        //  * Cannot set the value of a property if the corresponding target object property is a non-configurable own
        //    accessor property that has undefined as its [[Set]] attribute.
        let (proxy_target, proxy_handler) = self.validate_non_revoked()?;
        let target = &proxy_target;
        let handler = ECMAScriptValue::from(proxy_handler);
        let trap = handler.get_method(&"set".into())?;
        if matches!(trap, ECMAScriptValue::Undefined) {
            return target.o.set(key, value, receiver);
        }
        let boolean_trap_result =
            to_boolean(call(&trap, &handler, &[target.into(), key.clone().into(), value.clone(), receiver.clone()])?);
        if !boolean_trap_result {
            return Ok(false);
        }
        let target_desc = target.o.get_own_property(&key)?;
        if let Some(PropertyDescriptor {
            configurable: false,
            property: PropertyKind::Data(DataProperty { writable: false, value: target_value }),
            ..
        }) = target_desc
        {
            if !value.same_value(&target_value) {
                return Err(create_type_error("proxy error: Cannot change the value of a property to be different from the value of the corresponding target object property if the corresponding target object property is a non-writable, non-configurable own data property."));
            }
        } else if matches!(
            target_desc,
            Some(PropertyDescriptor {
                configurable: false,
                property: PropertyKind::Accessor(AccessorProperty { set: ECMAScriptValue::Undefined, .. }),
                ..
            })
        ) {
            return Err(create_type_error("proxy error: Cannot set the value of a property if the corresponding target object property is a non-configurable own accessor property that has undefined as its [[Set]] attribute."));
        }

        Ok(true)
    }

    fn delete(&self, key: &PropertyKey) -> Completion<bool> {
        // [[Delete]] ( P )
        // The [[Delete]] internal method of a Proxy exotic object O takes argument P (a property key) and returns
        // either a normal completion containing a Boolean or a throw completion. It performs the following steps when
        // called:
        //
        //  1. Perform ? ValidateNonRevokedProxy(O).
        //  2. Let target be O.[[ProxyTarget]].
        //  3. Let handler be O.[[ProxyHandler]].
        //  4. Assert: handler is an Object.
        //  5. Let trap be ? GetMethod(handler, "deleteProperty").
        //  6. If trap is undefined, then
        //      a. Return ? target.[[Delete]](P).
        //  7. Let booleanTrapResult be ToBoolean(? Call(trap, handler, « target, P »)).
        //  8. If booleanTrapResult is false, return false.
        //  9. Let targetDesc be ? target.[[GetOwnProperty]](P).
        //  10. If targetDesc is undefined, return true.
        //  11. If targetDesc.[[Configurable]] is false, throw a TypeError exception.
        //  12. Let extensibleTarget be ? IsExtensible(target).
        //  13. If extensibleTarget is false, throw a TypeError exception.
        //  14. Return true.
        //
        // NOTE
        // [[Delete]] for Proxy objects enforces the following invariants:
        //
        //  * The result of [[Delete]] is a Boolean value.
        //  * A property cannot be reported as deleted, if it exists as a non-configurable own property of the target
        //    object.
        //  * A property cannot be reported as deleted, if it exists as an own property of the target object and the
        //    target object is non-extensible.
        let (proxy_target, proxy_handler) = self.validate_non_revoked()?;
        let target = &proxy_target;
        let handler = ECMAScriptValue::from(proxy_handler);
        let trap = handler.get_method(&"deleteProperty".into())?;
        if matches!(trap, ECMAScriptValue::Undefined) {
            return target.o.delete(key);
        }
        let boolean_trap_result = to_boolean(call(&trap, &handler, &[target.into(), key.into()])?);
        if !boolean_trap_result {
            return Ok(false);
        }
        let target_desc = target.o.get_own_property(key)?;
        match target_desc {
            None => Ok(true),
            Some(target_desc) => {
                if target_desc.configurable {
                    let extensible_target = is_extensible(target)?;
                    if extensible_target {
                        Ok(true)
                    } else {
                        Err(create_type_error("proxy error: A property cannot be reported as deleted, if it exists as an own property of the target object and the target object is non-extensible."))
                    }
                } else {
                    Err(create_type_error("proxy error: A property cannot be reported as deleted, if it exists as a non-configurable own property of the target object."))
                }
            }
        }
    }

    fn own_property_keys(&self) -> Completion<Vec<PropertyKey>> {
        // [[OwnPropertyKeys]] ( )
        // The [[OwnPropertyKeys]] internal method of a Proxy exotic object O takes no arguments and returns either a
        // normal completion containing a List of property keys or a throw completion. It performs the following steps
        // when called:
        //
        //  1. Perform ? ValidateNonRevokedProxy(O).
        //  2. Let target be O.[[ProxyTarget]].
        //  3. Let handler be O.[[ProxyHandler]].
        //  4. Assert: handler is an Object.
        //  5. Let trap be ? GetMethod(handler, "ownKeys").
        //  6. If trap is undefined, then
        //      a. Return ? target.[[OwnPropertyKeys]]().
        //  7. Let trapResultArray be ? Call(trap, handler, « target »).
        //  8. Let trapResult be ? CreateListFromArrayLike(trapResultArray, « String, Symbol »).
        //  9. If trapResult contains any duplicate entries, throw a TypeError exception.
        //  10. Let extensibleTarget be ? IsExtensible(target).
        //  11. Let targetKeys be ? target.[[OwnPropertyKeys]]().
        //  12. Assert: targetKeys is a List of property keys.
        //  13. Assert: targetKeys contains no duplicate entries.
        //  14. Let targetConfigurableKeys be a new empty List.
        //  15. Let targetNonconfigurableKeys be a new empty List.
        //  16. For each element key of targetKeys, do
        //      a. Let desc be ? target.[[GetOwnProperty]](key).
        //      b. If desc is not undefined and desc.[[Configurable]] is false, then
        //          i. Append key to targetNonconfigurableKeys.
        //      c. Else,
        //          i. Append key to targetConfigurableKeys.
        //  17. If extensibleTarget is true and targetNonconfigurableKeys is empty, then
        //      a. Return trapResult.
        //  18. Let uncheckedResultKeys be a List whose elements are the elements of trapResult.
        //  19. For each element key of targetNonconfigurableKeys, do
        //      a. If uncheckedResultKeys does not contain key, throw a TypeError exception.
        //      b. Remove key from uncheckedResultKeys.
        //  20. If extensibleTarget is true, return trapResult.
        //  21. For each element key of targetConfigurableKeys, do
        //      a. If uncheckedResultKeys does not contain key, throw a TypeError exception.
        //      b. Remove key from uncheckedResultKeys.
        //  22. If uncheckedResultKeys is not empty, throw a TypeError exception.
        //  23. Return trapResult.
        //
        // NOTE
        // [[OwnPropertyKeys]] for Proxy objects enforces the following invariants:
        //
        //  * The result of [[OwnPropertyKeys]] is a List.
        //  * The returned List contains no duplicate entries.
        //  * The Type of each result List element is either String or Symbol.
        //  * The result List must contain the keys of all non-configurable own properties of the target object.
        //  * If the target object is not extensible, then the result List must contain all the keys of the own
        //    properties of the target object and no other values.
        let (proxy_target, proxy_handler) = self.validate_non_revoked()?;
        let target = &proxy_target;
        let handler = ECMAScriptValue::from(proxy_handler);
        let trap = handler.get_method(&"ownKeys".into())?;
        if matches!(trap, ECMAScriptValue::Undefined) {
            return target.o.own_property_keys();
        }
        let trap_result_array = call(&trap, &handler, &[target.into()])?;
        let trap_result =
            create_list_from_array_like(trap_result_array, Some(&[ValueKind::String, ValueKind::Symbol]))?
                .into_iter()
                .map(|v| PropertyKey::try_from(v).expect("values are keys"))
                .collect::<Vec<_>>();
        let has_duplicates = trap_result.iter().collect::<Counter<_>>().into_iter().filter(|&(_, n)| n > 1).count() > 0;
        if has_duplicates {
            return Err(create_type_error("proxy error: The returned List may not contain duplicate entries."));
        }
        let extensible_target = is_extensible(target)?;
        let target_keys = target.o.own_property_keys()?;
        let mut target_configurable_keys = vec![];
        let mut target_nonconfigurable_keys = vec![];
        for key in target_keys {
            let desc = target.o.get_own_property(&key)?;
            if matches!(desc, Some(PropertyDescriptor { configurable: false, .. })) {
                target_nonconfigurable_keys.push(key);
            } else {
                target_configurable_keys.push(key);
            }
        }
        if extensible_target && target_nonconfigurable_keys.is_empty() {
            return Ok(trap_result);
        }
        let mut unchecked_result_keys = trap_result.clone();
        for key in target_nonconfigurable_keys {
            match unchecked_result_keys.iter().position(|k| k.eq(&key)) {
                None => {
                    return Err(create_type_error(
                        "proxy error: The result List must contain the keys of all non-configurable own \
                        properties of the target object.",
                    ));
                }
                Some(idx) => {
                    unchecked_result_keys.swap_remove(idx);
                }
            }
        }
        if extensible_target {
            return Ok(trap_result);
        }
        for key in target_configurable_keys {
            match unchecked_result_keys.iter().position(|k| k.eq(&key)) {
                None => {
                    return Err(create_type_error(
                        "proxy error: If the target object is not extensible, then the result List must \
                        contain all the keys of the own properties of the target object and no other values.",
                    ));
                }
                Some(idx) => {
                    unchecked_result_keys.swap_remove(idx);
                }
            }
        }
        if !unchecked_result_keys.is_empty() {
            return Err(create_type_error(
                "proxy error: If the target object is not extensible, then the result List must contain all \
                the keys of the own properties of the target object and no other values.",
            ));
        }
        Ok(trap_result)
    }
}

impl CallableObject for ProxyObject {
    fn call(&self, _: &Object, this_argument: &ECMAScriptValue, arguments_list: &[ECMAScriptValue]) {
        let result = self.proxy_call(this_argument, arguments_list);
        ec_push(result.map(NormalCompletion::from));
    }

    fn construct(&self, _self_object: &Object, arguments_list: &[ECMAScriptValue], new_target: &Object) {
        let result = self.proxy_construct(arguments_list, new_target);
        ec_push(result.map(NormalCompletion::from));
    }

    fn end_evaluation(&self, _result: FullCompletion) {
        unreachable!()
    }

    fn complete_call(&self) -> Completion<ECMAScriptValue> {
        unreachable!()
    }
}

impl ProxyObject {
    pub fn new(target_and_handler: Option<(Object, Object)>) -> Self {
        Self {
            common: RefCell::new(CommonObjectData::new(None, false, PROXY_OBJECT_SLOTS)),
            proxy_items: RefCell::new(
                target_and_handler.map(|(t, h)| ProxyItems { proxy_handler: h, proxy_target: t }),
            ),
        }
    }

    pub fn validate_non_revoked(&self) -> Completion<(Object, Object)> {
        // ValidateNonRevokedProxy ( proxy )
        // The abstract operation ValidateNonRevokedProxy takes argument proxy (a Proxy exotic object) and
        // returns either a normal completion containing UNUSED or a throw completion. It throws a TypeError
        // exception if proxy has been revoked. It performs the following steps when called:
        //
        //  1. If proxy.[[ProxyTarget]] is null, throw a TypeError exception.
        //  2. Assert: proxy.[[ProxyHandler]] is not null.
        //  3. Return UNUSED.
        match self.proxy_items.borrow().as_ref() {
            None => Err(create_type_error("Proxy has been revoked")),
            Some(items) => Ok((items.proxy_target.clone(), items.proxy_handler.clone())),
        }
    }

    pub fn object(target_and_handler: Option<(Object, Object)>) -> Object {
        Object { o: Rc::new(Self::new(target_and_handler)) }
    }

    pub fn revoke(&self) {
        *self.proxy_items.borrow_mut() = None;
    }

    fn proxy_call(
        &self,
        this_argument: &ECMAScriptValue,
        arguments_list: &[ECMAScriptValue],
    ) -> Completion<ECMAScriptValue> {
        // [[Call]] ( thisArgument, argumentsList )
        // The [[Call]] internal method of a Proxy exotic object O takes arguments thisArgument (an ECMAScript language
        // value) and argumentsList (a List of ECMAScript language values) and returns either a normal completion
        // containing an ECMAScript language value or a throw completion. It performs the following steps when called:
        //
        //  1. Perform ? ValidateNonRevokedProxy(O).
        //  2. Let target be O.[[ProxyTarget]].
        //  3. Let handler be O.[[ProxyHandler]].
        //  4. Assert: handler is an Object.
        //  5. Let trap be ? GetMethod(handler, "apply").
        //  6. If trap is undefined, then
        //      a. Return ? Call(target, thisArgument, argumentsList).
        //  7. Let argArray be CreateArrayFromList(argumentsList).
        //  8. Return ? Call(trap, handler, « target, thisArgument, argArray »).
        // NOTE
        // A Proxy exotic object only has a [[Call]] internal method if the initial value of its [[ProxyTarget]]
        // internal slot is an object that has a [[Call]] internal method.
        assert!(self.is_callable_obj());
        let (proxy_target, proxy_handler) = self.validate_non_revoked()?;
        let target = &proxy_target;
        let handler = ECMAScriptValue::from(proxy_handler);
        let trap = handler.get_method(&"apply".into())?;
        if matches!(trap, ECMAScriptValue::Undefined) {
            return call(&ECMAScriptValue::Object(target.clone()), this_argument, arguments_list);
        }
        let arg_array = create_array_from_list(arguments_list);
        call(
            &trap,
            &handler,
            &[ECMAScriptValue::Object(target.clone()), this_argument.clone(), ECMAScriptValue::Object(arg_array)],
        )
    }

    fn proxy_construct(&self, arguments_list: &[ECMAScriptValue], new_target: &Object) -> Completion<ECMAScriptValue> {
        // [[Construct]] ( argumentsList, newTarget )
        // The [[Construct]] internal method of a Proxy exotic object O takes arguments argumentsList (a List of
        // ECMAScript language values) and newTarget (a constructor) and returns either a normal completion containing
        // an Object or a throw completion. It performs the following steps when called:
        //
        //  1. Perform ? ValidateNonRevokedProxy(O).
        //  2. Let target be O.[[ProxyTarget]].
        //  3. Assert: IsConstructor(target) is true.
        //  4. Let handler be O.[[ProxyHandler]].
        //  5. Assert: handler is an Object.
        //  6. Let trap be ? GetMethod(handler, "construct").
        //  7. If trap is undefined, then
        //      a. Return ? Construct(target, argumentsList, newTarget).
        //  8. Let argArray be CreateArrayFromList(argumentsList).
        //  9. Let newObj be ? Call(trap, handler, « target, argArray, newTarget »).
        //  10. If newObj is not an Object, throw a TypeError exception.
        //  11. Return newObj.
        // NOTE 1
        // A Proxy exotic object only has a [[Construct]] internal method if the initial value of its [[ProxyTarget]]
        // internal slot is an object that has a [[Construct]] internal method.
        //
        // NOTE 2
        // [[Construct]] for Proxy objects enforces the following invariants:
        //  * The result of [[Construct]] must be an Object.
        assert!(self.to_constructable().is_some());
        let (proxy_target, proxy_handler) = self.validate_non_revoked()?;
        let target = &proxy_target;
        let handler = ECMAScriptValue::from(proxy_handler);
        let trap = handler.get_method(&"construct".into())?;
        if matches!(trap, ECMAScriptValue::Undefined) {
            return construct(target, arguments_list, Some(new_target));
        }
        let arg_array = create_array_from_list(arguments_list);
        let new_obj = call(
            &trap,
            &handler,
            &[
                ECMAScriptValue::Object(target.clone()),
                ECMAScriptValue::Object(arg_array),
                ECMAScriptValue::Object(new_target.clone()),
            ],
        )?;
        if !new_obj.is_object() {
            return Err(create_type_error("proxy error: A constructor must return an object"));
        }
        Ok(new_obj)
    }
}

pub fn proxy_create(target: ECMAScriptValue, handler: ECMAScriptValue) -> Completion<ECMAScriptValue> {
    match (target, handler) {
        (ECMAScriptValue::Object(tgt), ECMAScriptValue::Object(hand)) => {
            Ok(ProxyObject::object(Some((tgt, hand))).into())
        }
        (ECMAScriptValue::Object(_), _) => Err(create_type_error("Proxy handler must be an object")),
        (_, ECMAScriptValue::Object(_)) => Err(create_type_error("Proxy target must be an object")),
        _ => Err(create_type_error("Proxy target and handler must be objects")),
    }
}

pub fn provision_proxy_intrinsic(realm: &Rc<RefCell<Realm>>) {
    let function_prototype = realm.borrow().intrinsics.function_prototype.clone();

    // The Proxy constructor:
    //
    //  * is %Proxy%.
    //  * is the initial value of the "Proxy" property of the global object.
    //  * creates and initializes a new Proxy object when called as a constructor.
    //  * is not intended to be called as a function and will throw an exception when called in that manner.

    // Properties of the Proxy Constructor
    // The Proxy constructor:
    //
    //  * has a [[Prototype]] internal slot whose value is %Function.prototype%.
    //  * does not have a "prototype" property because Proxy objects do not have a [[Prototype]] internal slot
    //    that requires initialization.

    let proxy_constructor = create_builtin_function(
        proxy_constructor_function,
        true,
        2_f64,
        PropertyKey::from("Proxy"),
        BUILTIN_FUNCTION_SLOTS,
        Some(realm.clone()),
        Some(function_prototype.clone()),
        None,
    );
    realm.borrow_mut().intrinsics.proxy = proxy_constructor.clone();

    // Constructor Function Properties
    macro_rules! constructor_function {
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
                &proxy_constructor,
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
    constructor_function!(proxy_revocable, "revocable", 2.0);
}

fn proxy_constructor_function(
    _this_value: &ECMAScriptValue,
    new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Proxy ( target, handler )
    // This function performs the following steps when called:
    //
    //  1. If NewTarget is undefined, throw a TypeError exception.
    //  2. Return ? ProxyCreate(target, handler).
    let mut args = FuncArgs::from(arguments);
    let target = args.next_arg();
    let handler = args.next_arg();

    match new_target {
        None => Err(create_type_error("Proxy may not be called as a function")),
        Some(_) => proxy_create(target, handler),
    }
}

fn proxy_revocable(
    _: &ECMAScriptValue,
    _: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Proxy.revocable ( target, handler )
    // This function creates a revocable Proxy object.
    //
    // It performs the following steps when called:
    //
    //  1. Let proxy be ? ProxyCreate(target, handler).
    //  2. Let revokerClosure be a new Abstract Closure with no parameters that captures nothing and performs the
    //     following steps when called:
    //      a. Let F be the active function object.
    //      b. Let p be F.[[RevocableProxy]].
    //      c. If p is null, return undefined.
    //      d. Set F.[[RevocableProxy]] to null.
    //      e. Assert: p is a Proxy exotic object.
    //      f. Set p.[[ProxyTarget]] to null.
    //      g. Set p.[[ProxyHandler]] to null.
    //      h. Return undefined.
    //  3. Let revoker be CreateBuiltinFunction(revokerClosure, 0, "", « [[RevocableProxy]] »).
    //  4. Set revoker.[[RevocableProxy]] to proxy.
    //  5. Let result be OrdinaryObjectCreate(%Object.prototype%).
    //  6. Perform ! CreateDataPropertyOrThrow(result, "proxy", proxy).
    //  7. Perform ! CreateDataPropertyOrThrow(result, "revoke", revoker).
    //  8. Return result.
    #[allow(clippy::unnecessary_wraps)]
    fn revoker_closure(_: &ECMAScriptValue, _: Option<&Object>, _: &[ECMAScriptValue]) -> Completion<ECMAScriptValue> {
        let f = active_function_object().expect("A function should be running.");
        let f = f.o.to_builtin_function_with_revocable_proxy_slot().expect("This should be a revokable proxy");
        let p = f.revokable_proxy_get();
        if let Some(p) = p {
            f.revokable_proxy_set(None);
            let p = p.o.to_proxy_object().expect("Object should be a proxy");
            p.revoke();
        }
        Ok(ECMAScriptValue::Undefined)
    }
    let mut args = FuncArgs::from(arguments);
    let target = args.next_arg();
    let handler = args.next_arg();
    let proxy = to_object(proxy_create(target, handler)?).expect("Proxy should already be an object");
    let revoker =
        BuiltinFunctionWithRevokableProxySlot::create(revoker_closure, 0.0, PropertyKey::from(""), proxy.clone());
    let result = ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)));
    result.create_data_property_or_throw("proxy", proxy)?;
    result.create_data_property_or_throw("revoke", revoker)?;
    Ok(ECMAScriptValue::Object(result))
}

#[derive(Debug)]
pub struct BuiltinFunctionWithRevokableProxySlot {
    func: BuiltInFunctionObject,
    revokable_proxy: RefCell<Option<Object>>,
}

impl BuiltinFunctionWithRevokableProxySlot {
    pub fn revokable_proxy_get(&self) -> Option<Object> {
        self.revokable_proxy.borrow().clone()
    }

    pub fn revokable_proxy_set(&self, item: Option<Object>) {
        *self.revokable_proxy.borrow_mut() = item;
    }

    pub fn new(
        prototype: Option<Object>,
        extensible: bool,
        realm: Rc<RefCell<Realm>>,
        initial_name: Option<FunctionName>,
        steps: fn(&ECMAScriptValue, Option<&Object>, &[ECMAScriptValue]) -> Completion<ECMAScriptValue>,
        is_constructor: bool,
        revokable_proxy: Option<Object>,
    ) -> Self {
        Self {
            func: BuiltInFunctionObject::new(prototype, extensible, realm, initial_name, steps, is_constructor),
            revokable_proxy: RefCell::new(revokable_proxy),
        }
    }

    pub fn object(
        prototype: Option<Object>,
        extensible: bool,
        realm: Rc<RefCell<Realm>>,
        initial_name: Option<FunctionName>,
        steps: fn(&ECMAScriptValue, Option<&Object>, &[ECMAScriptValue]) -> Completion<ECMAScriptValue>,
        is_constructor: bool,
        revokable_proxy: Option<Object>,
    ) -> Object {
        Object {
            o: Rc::new(Self::new(prototype, extensible, realm, initial_name, steps, is_constructor, revokable_proxy)),
        }
    }

    pub fn create(
        behavior: fn(&ECMAScriptValue, Option<&Object>, &[ECMAScriptValue]) -> Completion<ECMAScriptValue>,
        length: f64,
        name: PropertyKey,
        proxy: Object,
    ) -> Object {
        let realm_to_use = current_realm_record().unwrap();
        let prototype_to_use = realm_to_use.borrow().intrinsics.function_prototype.clone();
        let func = Self::object(Some(prototype_to_use), true, realm_to_use, None, behavior, false, Some(proxy));
        set_function_length(&func, length);
        set_function_name(&func, FunctionName::from(name), None);
        func
    }
}

impl ObjectInterface for BuiltinFunctionWithRevokableProxySlot {
    fn common_object_data(&self) -> &RefCell<CommonObjectData> {
        self.func.common_object_data()
    }

    fn uses_ordinary_get_prototype_of(&self) -> bool {
        self.func.uses_ordinary_get_prototype_of()
    }

    fn id(&self) -> usize {
        self.func.id()
    }

    fn get_prototype_of(&self) -> Completion<Option<Object>> {
        self.func.get_prototype_of()
    }

    fn set_prototype_of(&self, obj: Option<Object>) -> Completion<bool> {
        self.func.set_prototype_of(obj)
    }

    fn is_extensible(&self) -> Completion<bool> {
        self.func.is_extensible()
    }

    fn prevent_extensions(&self) -> Completion<bool> {
        self.func.prevent_extensions()
    }

    fn get_own_property(&self, key: &PropertyKey) -> Completion<Option<PropertyDescriptor>> {
        self.func.get_own_property(key)
    }

    fn define_own_property(&self, key: PropertyKey, desc: PotentialPropertyDescriptor) -> Completion<bool> {
        self.func.define_own_property(key, desc)
    }

    fn has_property(&self, key: &PropertyKey) -> Completion<bool> {
        self.func.has_property(key)
    }

    fn get(&self, key: &PropertyKey, receiver: &ECMAScriptValue) -> Completion<ECMAScriptValue> {
        self.func.get(key, receiver)
    }

    fn set(&self, key: PropertyKey, value: ECMAScriptValue, receiver: &ECMAScriptValue) -> Completion<bool> {
        self.func.set(key, value, receiver)
    }

    fn delete(&self, key: &PropertyKey) -> Completion<bool> {
        self.func.delete(key)
    }

    fn own_property_keys(&self) -> Completion<Vec<PropertyKey>> {
        self.func.own_property_keys()
    }

    fn to_builtin_function_with_revocable_proxy_slot(&self) -> Option<&Self> {
        Some(self)
    }

    fn to_callable_obj(&self) -> Option<&dyn CallableObject> {
        self.func.to_callable_obj()
    }
    fn to_constructable(&self) -> Option<&dyn CallableObject> {
        self.func.to_constructable()
    }
    fn to_builtin_function_obj(&self) -> Option<&dyn BuiltinFunctionInterface> {
        self.func.to_builtin_function_obj()
    }
    fn is_callable_obj(&self) -> bool {
        self.func.is_callable_obj()
    }
    fn kind(&self) -> ObjectTag {
        if self.is_callable_obj() {
            ObjectTag::Function
        } else {
            ObjectTag::Object
        }
    }
}

impl CallableObject for BuiltinFunctionWithRevokableProxySlot {
    fn call(&self, self_object: &Object, this_argument: &ECMAScriptValue, arguments_list: &[ECMAScriptValue]) {
        self.func.call(self_object, this_argument, arguments_list);
    }

    fn construct(&self, self_object: &Object, arguments_list: &[ECMAScriptValue], new_target: &Object) {
        self.func.construct(self_object, arguments_list, new_target);
    }

    fn end_evaluation(&self, result: FullCompletion) {
        self.func.end_evaluation(result);
    }

    fn complete_call(&self) -> Completion<ECMAScriptValue> {
        self.func.complete_call()
    }
}

impl BuiltinFunctionInterface for BuiltinFunctionWithRevokableProxySlot {
    fn builtin_function_data(&self) -> &RefCell<BuiltInFunctionData> {
        self.func.builtin_function_data()
    }
}

#[cfg(test)]
mod tests;
