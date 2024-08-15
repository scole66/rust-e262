use super::*;

pub struct ParameterMap {
    env: Rc<dyn EnvironmentRecord>,
    properties: Vec<Option<JSString>>,
}

impl std::fmt::Debug for ParameterMap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ParameterMap")
            .field("env", &format!("{:?}", &self.env)) // turn off "alterate" flag for env
            .field("properties", &self.properties)
            .finish()
    }
}

impl ParameterMap {
    pub fn new(env: Rc<dyn EnvironmentRecord>) -> Self {
        ParameterMap { env, properties: Vec::new() }
    }

    pub fn add_mapped_name(&mut self, name: JSString, loc: usize) {
        if self.properties.len() <= loc {
            self.properties.resize_with(loc + 1, Default::default);
        }
        self.properties[loc] = Some(name);
    }

    fn idx_from_key(key: &PropertyKey) -> Option<usize> {
        if let Ok(s) = JSString::try_from(key) {
            if let Ok(idx) = String::from(s).parse::<usize>() {
                return Some(idx);
            }
        }
        None
    }

    pub fn to_index(&self, key: &PropertyKey) -> Option<usize> {
        ParameterMap::idx_from_key(key).filter(|&idx| idx < self.properties.len() && self.properties[idx].is_some())
    }

    pub fn delete(&mut self, idx: usize) {
        self.properties[idx] = None;
    }

    pub fn get(&self, idx: usize) -> Completion<ECMAScriptValue> {
        let name = self.properties[idx].as_ref().expect("Get only used on existing values");
        self.env.get_binding_value(name, false)
    }

    pub fn set(&self, idx: usize, value: ECMAScriptValue) -> Completion<()> {
        let name = self.properties[idx].as_ref().expect("Set only used on existing values").clone();
        self.env.set_mutable_binding(name, value, false)
    }
}

#[derive(Debug)]
pub struct ArgumentsObject {
    common: RefCell<CommonObjectData>,
    pub parameter_map: Option<RefCell<ParameterMap>>,
}

impl<'a> From<&'a ArgumentsObject> for &'a dyn ObjectInterface {
    fn from(obj: &'a ArgumentsObject) -> Self {
        obj
    }
}

impl ObjectInterface for ArgumentsObject {
    fn common_object_data(&self) -> &RefCell<CommonObjectData> {
        &self.common
    }
    fn uses_ordinary_get_prototype_of(&self) -> bool {
        true
    }
    fn id(&self) -> usize {
        self.common.borrow().objid
    }
    fn kind(&self) -> ObjectTag {
        ObjectTag::Arguments
    }
    fn to_arguments_object(&self) -> Option<&ArgumentsObject> {
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

    /// Returns the property descriptor corresponding to the given key, or None if the key does not exist.
    ///
    /// See [GetOwnProperty](https://tc39.es/ecma262/#sec-arguments-exotic-objects-getownproperty-p) from ECMA-262.
    fn get_own_property(&self, key: &PropertyKey) -> Completion<Option<PropertyDescriptor>> {
        // [[GetOwnProperty]] ( P )
        //
        // The [[GetOwnProperty]] internal method of an arguments exotic object args takes argument P (a property key)
        // and returns a normal completion containing either a Property Descriptor or undefined. It performs the
        // following steps when called:
        //
        //  1. Let desc be OrdinaryGetOwnProperty(args, P).
        //  2. If desc is undefined, return desc.
        //  3. Let map be args.[[ParameterMap]].
        //  4. If map is undefined, return desc.
        //  5. Let isMapped be ! HasOwnProperty(map, P).
        //  6. If isMapped is true, then
        //      a. Set desc.[[Value]] to ! Get(map, P).
        //  7. Return desc.
        let desc = ordinary_get_own_property(self, key);
        match desc {
            None => Ok(None),
            Some(mut desc) => match &self.parameter_map {
                None => Ok(Some(desc)),
                Some(map) => {
                    let map = map.borrow();
                    if let Some(idx) = map.to_index(key) {
                        let value = map.get(idx).expect("Property must exist, as we just checked");
                        let writable = desc.is_writable().expect("Property cannot be an accessor");
                        desc.property = PropertyKind::Data(DataProperty { value, writable });
                    }
                    Ok(Some(desc))
                }
            },
        }
    }

    // [[DefineOwnProperty]] ( P, Desc )
    //
    // The [[DefineOwnProperty]] internal method of an arguments exotic object args takes arguments P (a property key)
    // and Desc (a Property Descriptor) and returns a normal completion containing a Boolean. It performs the following
    // steps when called:
    //
    //  1. Let map be args.[[ParameterMap]].
    //  2. Let isMapped be ! HasOwnProperty(map, P).
    //  3. Let newArgDesc be Desc.
    //  4. If isMapped is true and IsDataDescriptor(Desc) is true, then
    //      a. If Desc does not have a [[Value]] field, and Desc has a [[Writable]] field, and Desc.[[Writable]] is
    //         false, then
    //           i. Set newArgDesc to a copy of Desc.
    //          ii. Set newArgDesc.[[Value]] to ! Get(map, P).
    //  5. Let allowed be ! OrdinaryDefineOwnProperty(args, P, newArgDesc).
    //  6. If allowed is false, return false.
    //  7. If isMapped is true, then
    //      a. If IsAccessorDescriptor(Desc) is true, then
    //           i. Perform ! map.[[Delete]](P).
    //      b. Else,
    //           i. If Desc has a [[Value]] field, then
    //              1. Assert: The following Set will succeed, since formal parameters mapped by arguments objects are
    //                 always writable.
    //              2. Perform ! Set(map, P, Desc.[[Value]], false).
    //          ii. If Desc has a [[Writable]] field and Desc.[[Writable]] is false, then
    //              1. Perform ! map.[[Delete]](P).
    //  8. Return true.
    fn define_own_property(&self, key: PropertyKey, desc: PotentialPropertyDescriptor) -> Completion<bool> {
        match &self.parameter_map {
            None => ordinary_define_own_property(self, key, desc),
            Some(map) => {
                let mut new_arg_desc = desc.clone();
                {
                    let map = map.borrow();
                    let maybe_index = map.to_index(&key);
                    if let Some(idx) = maybe_index {
                        if desc.is_data_descriptor() && desc.value.is_none() && desc.writable == Some(false) {
                            new_arg_desc.value = Some(map.get(idx).expect("Property must exist, as we just checked"));
                        }
                    }
                }
                let allowed = ordinary_define_own_property(self, key.clone(), new_arg_desc).expect("Simple Object");
                if allowed {
                    let mut map = map.borrow_mut();
                    if let Some(idx) = map.to_index(&key) {
                        if desc.is_accessor_descriptor() || (desc.value.is_none() && desc.writable == Some(false)) {
                            map.delete(idx);
                        } else if let Some(value) = desc.value {
                            map.set(idx, value)
                                .expect("formal parameters mapped by arguments objects are always writable");
                        }
                    }
                }
                Ok(allowed)
            }
        }
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

    /// Retrieves the value of a property from an object, following the prototype chain
    ///
    /// See [Get](https://tc39.es/ecma262/#sec-arguments-exotic-objects-get-p-receiver) in ECMA-262.
    fn get(&self, key: &PropertyKey, receiver: &ECMAScriptValue) -> Completion<ECMAScriptValue> {
        // [[Get]] ( P, Receiver )
        //
        // The [[Get]] internal method of an arguments exotic object args takes arguments P (a property key) and
        // Receiver (an ECMAScript language value) and returns either a normal completion containing an ECMAScript
        // language value or a throw completion. It performs the following steps when called:
        //
        //  1. Let map be args.[[ParameterMap]].
        //  2. Let isMapped be ! HasOwnProperty(map, P).
        //  3. If isMapped is false, then
        //      a. Return ? OrdinaryGet(args, P, Receiver).
        //  4. Else,
        //      a. Assert: map contains a formal parameter mapping for P.
        //      b. Return ! Get(map, P).
        match &self.parameter_map {
            None => ordinary_get(self, key, receiver),
            Some(map) => {
                let map = map.borrow();
                match map.to_index(key) {
                    None => ordinary_get(self, key, receiver),
                    Some(idx) => Ok(map.get(idx).expect("Assert: map contains a formal mapping for P.")),
                }
            }
        }
    }

    // [[Set]] ( P, V, Receiver )
    //
    // The [[Set]] internal method of an ordinary object O takes arguments P (a property key), V (an ECMAScript language
    // value), and Receiver (an ECMAScript language value). It performs the following steps when called:
    //
    //  1. Return ? OrdinarySet(O, P, V, Receiver).
    fn set(&self, key: PropertyKey, v: ECMAScriptValue, receiver: &ECMAScriptValue) -> Completion<bool> {
        // [[Set]] ( P, V, Receiver )
        //
        // The [[Set]] internal method of an arguments exotic object args takes arguments P (a property key), V (an
        // ECMAScript language value), and Receiver (an ECMAScript language value) and returns either a normal
        // completion containing a Boolean or a throw completion. It performs the following steps when called:
        //
        //  1. If SameValue(args, Receiver) is false, then
        //      a. Let isMapped be false.
        //  2. Else,
        //      a. Let map be args.[[ParameterMap]].
        //      b. Let isMapped be ! HasOwnProperty(map, P).
        //  3. If isMapped is true, then
        //      a. Assert: The following Set will succeed, since formal parameters mapped by arguments objects are
        //         always writable.
        //      b. Perform ! Set(map, P, V, false).
        //  4. Return ? OrdinarySet(args, P, V, Receiver).
        if let Some(pmap) = self
            .parameter_map
            .as_ref()
            .filter(|_| matches!(receiver, ECMAScriptValue::Object(o) if o.o.id() == self.id()))
        {
            let map = pmap.borrow_mut();
            if let Some(idx) = map.to_index(&key) {
                map.set(idx, v.clone()).expect("formal parameters mapped by arguments objects are always writable");
            }
        }
        ordinary_set(self, key, v, receiver)
    }

    // [[Delete]] ( P )
    //
    // The [[Delete]] internal method of an arguments exotic object args takes argument P (a property key) and returns
    // either a normal completion containing a Boolean or a throw completion. It performs the following steps when
    // called:
    //
    //  1. Let map be args.[[ParameterMap]].
    //  2. Let isMapped be ! HasOwnProperty(map, P).
    //  3. Let result be ? OrdinaryDelete(args, P).
    //  4. If result is true and isMapped is true, then
    //      a. Perform ! map.[[Delete]](P).
    //  5. Return result.
    fn delete(&self, key: &PropertyKey) -> Completion<bool> {
        // Note: ordinary_delete only fails if its call to o.[[GetOwnProperty]]
        // fails. And the ArgumentsObject::GetOwnProperty routine, just above,
        // cannot fail. Thus: we don't need to pass back an error from
        // ordinary_delete, and ArgumentsObject::Delete cannot fail, either!
        let result = ordinary_delete(self, key).expect("Arguments Objects can always delete");
        if let Some(map) = self.parameter_map.as_ref().filter(|_| result) {
            let mut pmap = map.borrow_mut();
            if let Some(idx) = pmap.to_index(key) {
                pmap.delete(idx);
            }
        }
        Ok(result)
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

impl ArgumentsObject {
    pub fn object(parameter_map: Option<ParameterMap>) -> Object {
        let prototype = Some(intrinsic(IntrinsicId::ObjectPrototype));
        Object { o: Rc::new(Self::new(prototype, parameter_map)) }
    }

    pub fn new(prototype: Option<Object>, parameter_map: Option<ParameterMap>) -> Self {
        Self {
            common: RefCell::new(CommonObjectData::new(prototype, true, ARGUMENTS_OBJECT_SLOTS)),
            parameter_map: parameter_map.map(RefCell::new),
        }
    }
}

// CreateMappedArgumentsObject ( func, formals, argumentsList, env )
//
// The abstract operation CreateMappedArgumentsObject takes arguments func (an Object), formals (a Parse Node),
// argumentsList (a List), and env (an Environment Record) and returns an arguments exotic object. It performs the
// following steps when called:
//
// [ 🏃 = runtime; 🧑‍💻 = compile]
//
//🧑‍💻  1. Assert: formals does not contain a rest parameter, any binding patterns, or any initializers. It may contain
//🧑‍💻     duplicate identifiers.
//🏃  2. Let len be the number of elements in argumentsList.
//🏃  3. Let obj be MakeBasicObject(« [[Prototype]], [[Extensible]], [[ParameterMap]] »).
//🏃  4. Set obj.[[GetOwnProperty]] as specified in 10.4.4.1.
//🏃  5. Set obj.[[DefineOwnProperty]] as specified in 10.4.4.2.
//🏃  6. Set obj.[[Get]] as specified in 10.4.4.3.
//🏃  7. Set obj.[[Set]] as specified in 10.4.4.4.
//🏃  8. Set obj.[[Delete]] as specified in 10.4.4.5.
//🏃  9. Set obj.[[Prototype]] to %Object.prototype%.
//🏃 10. Let map be OrdinaryObjectCreate(null).
//🏃 11. Set obj.[[ParameterMap]] to map.
//🧑‍💻 12. Let parameterNames be the BoundNames of formals.
//🧑‍💻 13. Let numberOfParameters be the number of elements in parameterNames.
//🏃 14. Let index be 0.
//🏃 15. Repeat, while index < len,
//🏃     a. Let val be argumentsList[index].
//🏃     b. Perform ! CreateDataPropertyOrThrow(obj, ! ToString(𝔽(index)), val).
//🏃     c. Set index to index + 1.
//🏃 16. Perform ! DefinePropertyOrThrow(obj, "length", PropertyDescriptor { [[Value]]: 𝔽(len), [[Writable]]: true,
//🏃     [[Enumerable]]: false, [[Configurable]]: true }).
//🧑‍💻 17. Let mappedNames be a new empty List.
//🧑‍💻 18. Set index to numberOfParameters - 1.
//🧑‍💻 19. Repeat, while index ≥ 0,
//🧑‍💻     a. Let name be parameterNames[index].
//🧑‍💻     b. If name is not an element of mappedNames, then
//🧑‍💻          i. Add name as an element of the list mappedNames.
//🧑‍💻         ii. If index < len, then
//🏃             1. Let g be MakeArgGetter(name, env).
//🏃             2. Let p be MakeArgSetter(name, env).
//🏃             3. Perform ! map.[[DefineOwnProperty]](! ToString(𝔽(index)), PropertyDescriptor { [[Set]]: p, [[Get]]:
//🏃                g, [[Enumerable]]: false, [[Configurable]]: true }).
//🧑‍💻     c. Set index to index - 1.
//🏃 20. Perform ! DefinePropertyOrThrow(obj, @@iterator, PropertyDescriptor { [[Value]]: %Array.prototype.values%,
//🏃     [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: true }).
//🏃 21. Perform ! DefinePropertyOrThrow(obj, "callee", PropertyDescriptor { [[Value]]: func, [[Writable]]: true,
//🏃     [[Enumerable]]: false, [[Configurable]]: true }).
//🏃 22. Return obj.

// Steps 2-16 + 20-21: CreateMappedArgsObj; On the stack should be: the function args and the function object. Steps 10-11 are really "Attach an empty ParameterMap". On exit the args obj is on the stack.
// Steps 19.b.ii.1-3: AttachMappedArg; a two-argument insn: (string index; arg number); arg obj should be on top of the stack. (It stays on the stack at insn exit.)

// So the sequence looks like:
// PUSH function obj
// PUSH arg0
// PUSH arg1
// PUSH arg2
// PUSH 3
// CreateMappedArgsObj
// AttachMappedArg arg2 2
// AttachMappedArg arg1 1
// AttachMappedArg arg0 0

#[cfg(test)]
mod tests;
