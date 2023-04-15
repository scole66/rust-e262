use super::*;

/// Iterate over the iterable and use the function in adder to add the values to target
///
/// The parameter iterable is expected to be an object that implements an @@iterator method that returns an
/// iterator object that produces a two element array-like object whose first element is a value that will be
/// used as a Map key and whose second element is the value to associate with that key.
///
/// See [AddEntriesFromIterable](https://tc39.es/ecma262/#sec-add-entries-from-iterable) in ECMA-262.
fn add_entries_from_iterable(
    target: &ECMAScriptValue,
    iterable: &ECMAScriptValue,
    adder: &ECMAScriptValue,
) -> Completion<ECMAScriptValue> {
    // AddEntriesFromIterable ( target, iterable, adder )
    // The abstract operation AddEntriesFromIterable takes arguments target (an Object), iterable (an
    // ECMAScript language value, but not undefined or null), and adder (a function object) and returns either
    // a normal completion containing an ECMAScript language value or a throw completion. adder will be
    // invoked, with target as the receiver. It performs the following steps when called:
    //
    //  1. Let iteratorRecord be ? GetIterator(iterable, sync).
    //  2. Repeat,
    //      a. Let next be ? IteratorStep(iteratorRecord).
    //      b. If next is false, return target.
    //      c. Let nextItem be ? IteratorValue(next).
    //      d. If nextItem is not an Object, then
    //          i. Let error be ThrowCompletion(a newly created TypeError object).
    //          ii. Return ? IteratorClose(iteratorRecord, error).
    //      e. Let k be Completion(Get(nextItem, "0")).
    //      f. IfAbruptCloseIterator(k, iteratorRecord).
    //      g. Let v be Completion(Get(nextItem, "1")).
    //      h. IfAbruptCloseIterator(v, iteratorRecord).
    //      i. Let status be Completion(Call(adder, target, « k, v »)).
    //      j. IfAbruptCloseIterator(status, iteratorRecord).

    let iterator_record = get_iterator(iterable, IteratorKind::Sync)?;
    loop {
        match iterator_step(&iterator_record)? {
            None => break Ok(target.clone()),
            Some(next) => {
                let next_item = iterator_value(&next)?;
                match next_item {
                    ECMAScriptValue::Undefined
                    | ECMAScriptValue::Null
                    | ECMAScriptValue::Boolean(_)
                    | ECMAScriptValue::String(_)
                    | ECMAScriptValue::Number(_)
                    | ECMAScriptValue::BigInt(_)
                    | ECMAScriptValue::Symbol(_) => {
                        let error = create_type_error("Iterator for mapping adder must return an object value");
                        return iterator_close(&iterator_record, Err(error));
                    }
                    ECMAScriptValue::Object(next_item) => {
                        let k = match get(&next_item, &"0".into()) {
                            Ok(val) => val,
                            Err(err) => {
                                return iterator_close(&iterator_record, Err(err));
                            }
                        };
                        let v = match get(&next_item, &"1".into()) {
                            Ok(val) => val,
                            Err(err) => {
                                return iterator_close(&iterator_record, Err(err));
                            }
                        };
                        match call(adder, target, &[k, v]) {
                            Ok(_) => (),
                            Err(err) => {
                                return iterator_close(&iterator_record, Err(err));
                            }
                        }
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests;
