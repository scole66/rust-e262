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

#[cfg(test)]
mod tests;
