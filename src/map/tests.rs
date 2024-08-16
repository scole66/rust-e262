use super::*;
use crate::tests::*;

mod add_entries_from_iterable {
    use super::*;
    use test_case::test_case;

    fn good_adder(
        this_value: &ECMAScriptValue,
        _new_target: Option<&Object>,
        arguments: &[ECMAScriptValue],
    ) -> Completion<ECMAScriptValue> {
        let mut args = FuncArgs::from(arguments);
        let key = to_string(args.next_arg())?;
        let value = args.next_arg();
        let this = to_object(this_value.clone())?;
        this.set(key, value, true)?;
        Ok(ECMAScriptValue::Undefined)
    }

    fn good_objs() -> (ECMAScriptValue, ECMAScriptValue, ECMAScriptValue) {
        let object_proto = intrinsic(IntrinsicId::ObjectPrototype);
        let target = ordinary_object_create(Some(object_proto));
        let data = create_array_from_list(&[true.into(), 3.into(), "blue".into()]);
        let generator = create_array_iterator(data, KeyValueKind::KeyValue);
        let adder = create_builtin_function(
            good_adder,
            false,
            2.0,
            "good_adder".into(),
            BUILTIN_FUNCTION_SLOTS,
            None,
            None,
            None,
        );

        (target.into(), generator.into(), adder.into())
    }

    fn good_iterator_bad_adder() -> (ECMAScriptValue, ECMAScriptValue, ECMAScriptValue) {
        let object_proto = intrinsic(IntrinsicId::ObjectPrototype);
        let target = ordinary_object_create(Some(object_proto));
        let data = create_array_from_list(&[true.into(), 3.into(), "blue".into()]);
        let generator = create_array_iterator(data, KeyValueKind::KeyValue);
        let thrower = intrinsic(IntrinsicId::ThrowTypeError);

        (target.into(), generator.into(), thrower.into())
    }

    fn iterator_next_error() -> (ECMAScriptValue, ECMAScriptValue, ECMAScriptValue) {
        let object_proto = intrinsic(IntrinsicId::ObjectPrototype);
        let target = ordinary_object_create(Some(object_proto));
        let iterator_proto = intrinsic(IntrinsicId::IteratorPrototype);
        let iterator = ordinary_object_create(Some(iterator_proto));
        let next_behavior = intrinsic(IntrinsicId::ThrowTypeError);
        iterator.create_data_property_or_throw("next", next_behavior).unwrap();

        (target.into(), iterator.into(), ECMAScriptValue::Undefined)
    }

    fn iterator_next_with_bad_iterator_result() -> (ECMAScriptValue, ECMAScriptValue, ECMAScriptValue) {
        let object_proto = intrinsic(IntrinsicId::ObjectPrototype);
        let target = ordinary_object_create(Some(object_proto));
        let iterator_proto = intrinsic(IntrinsicId::IteratorPrototype);
        let iterator = ordinary_object_create(Some(iterator_proto));
        let next_behavior = |_: &ECMAScriptValue, _: Option<&Object>, _: &[ECMAScriptValue]| {
            let object_proto = intrinsic(IntrinsicId::ObjectPrototype);
            let obj = ordinary_object_create(Some(object_proto));
            let thrower = intrinsic(IntrinsicId::ThrowTypeError);
            let value_ppd = PotentialPropertyDescriptor::new().get(thrower);
            define_property_or_throw(&obj, "value", value_ppd).unwrap();
            let result: Completion<ECMAScriptValue> = Ok(obj.into());
            result
        };
        let next = create_builtin_function(next_behavior, false, 0.0, "next".into(), &[], None, None, None);
        iterator.create_data_property_or_throw("next", next).unwrap();

        (target.into(), iterator.into(), ECMAScriptValue::Undefined)
    }
    fn iterator_next_non_object_value() -> (ECMAScriptValue, ECMAScriptValue, ECMAScriptValue) {
        let object_proto = intrinsic(IntrinsicId::ObjectPrototype);
        let target = ordinary_object_create(Some(object_proto));
        let iterator_proto = intrinsic(IntrinsicId::IteratorPrototype);
        let iterator = ordinary_object_create(Some(iterator_proto));
        let next_behavior = |_: &ECMAScriptValue, _: Option<&Object>, _: &[ECMAScriptValue]| {
            let object_proto = intrinsic(IntrinsicId::ObjectPrototype);
            let obj = ordinary_object_create(Some(object_proto));
            obj.create_data_property_or_throw("value", "oopsie").unwrap();
            let result: Completion<ECMAScriptValue> = Ok(obj.into());
            result
        };
        let next = create_builtin_function(next_behavior, false, 0.0, "next".into(), &[], None, None, None);
        iterator.create_data_property_or_throw("next", next).unwrap();

        (target.into(), iterator.into(), ECMAScriptValue::Undefined)
    }
    fn iterator_next_bad_zeroth() -> (ECMAScriptValue, ECMAScriptValue, ECMAScriptValue) {
        let object_proto = intrinsic(IntrinsicId::ObjectPrototype);
        let target = ordinary_object_create(Some(object_proto));
        let iterator_proto = intrinsic(IntrinsicId::IteratorPrototype);
        let iterator = ordinary_object_create(Some(iterator_proto));
        let next_behavior = |_: &ECMAScriptValue, _: Option<&Object>, _: &[ECMAScriptValue]| {
            let object_proto = intrinsic(IntrinsicId::ObjectPrototype);
            let obj = ordinary_object_create(Some(object_proto.clone()));
            let items = ordinary_object_create(Some(object_proto));
            let error_ppd = PotentialPropertyDescriptor::new().get(intrinsic(IntrinsicId::ThrowTypeError));
            define_property_or_throw(&items, "0", error_ppd).unwrap();
            obj.create_data_property_or_throw("value", items).unwrap();
            let result: Completion<ECMAScriptValue> = Ok(obj.into());
            result
        };
        let next = create_builtin_function(next_behavior, false, 0.0, "next".into(), &[], None, None, None);
        iterator.create_data_property_or_throw("next", next).unwrap();

        (target.into(), iterator.into(), ECMAScriptValue::Undefined)
    }
    fn iterator_next_bad_first() -> (ECMAScriptValue, ECMAScriptValue, ECMAScriptValue) {
        let object_proto = intrinsic(IntrinsicId::ObjectPrototype);
        let target = ordinary_object_create(Some(object_proto));
        let iterator_proto = intrinsic(IntrinsicId::IteratorPrototype);
        let iterator = ordinary_object_create(Some(iterator_proto));
        let next_behavior = |_: &ECMAScriptValue, _: Option<&Object>, _: &[ECMAScriptValue]| {
            let object_proto = intrinsic(IntrinsicId::ObjectPrototype);
            let obj = ordinary_object_create(Some(object_proto.clone()));
            let items = ordinary_object_create(Some(object_proto));
            let error_ppd = PotentialPropertyDescriptor::new().get(intrinsic(IntrinsicId::ThrowTypeError));
            define_property_or_throw(&items, "1", error_ppd).unwrap();
            items.create_data_property_or_throw("0", "key").unwrap();
            obj.create_data_property_or_throw("value", items).unwrap();
            let result: Completion<ECMAScriptValue> = Ok(obj.into());
            result
        };
        let next = create_builtin_function(next_behavior, false, 0.0, "next".into(), &[], None, None, None);
        iterator.create_data_property_or_throw("next", next).unwrap();

        (target.into(), iterator.into(), ECMAScriptValue::Undefined)
    }

    #[test_case(|| {
      (ECMAScriptValue::Undefined, ECMAScriptValue::Undefined, ECMAScriptValue::Undefined)
    } => serr("TypeError: Undefined and null cannot be converted to objects"); "get_iterator errors")]
    #[test_case(good_objs => sok("0:true,1:3,2:blue"); "typical success")]
    #[test_case(good_iterator_bad_adder => serr("TypeError: Generic TypeError"); "adder throws")]
    #[test_case(iterator_next_error => serr("TypeError: Generic TypeError"); "iterator_step throws")]
    #[test_case(iterator_next_with_bad_iterator_result => serr("TypeError: Generic TypeError"); "iterator_value throws")]
    #[test_case(iterator_next_non_object_value => serr("TypeError: Iterator for mapping adder must return an object value"); "bad value type")]
    #[test_case(iterator_next_bad_zeroth => serr("TypeError: Generic TypeError"); "get from 0 throws")]
    #[test_case(iterator_next_bad_first => serr("TypeError: Generic TypeError"); "get from 1 throws")]
    fn call(make_vals: impl FnOnce() -> (ECMAScriptValue, ECMAScriptValue, ECMAScriptValue)) -> Result<String, String> {
        setup_test_agent();
        let (target, iterable, adder) = make_vals();

        let res =
            super::add_entries_from_iterable(&target, &iterable, &adder).map_err(unwind_any_error)?.try_into().unwrap();

        let repr = match res {
            ECMAScriptValue::Undefined
            | ECMAScriptValue::Null
            | ECMAScriptValue::Boolean(_)
            | ECMAScriptValue::String(_)
            | ECMAScriptValue::Number(_)
            | ECMAScriptValue::BigInt(_)
            | ECMAScriptValue::Symbol(_) => format!("{res:?}"),
            ECMAScriptValue::Object(o) => {
                let keys = ordinary_own_property_keys(&o);
                let mut r = String::new();
                let mut first = true;
                for key in keys {
                    let value = o.get(&key).map_err(unwind_any_error)?;
                    if first {
                        first = false;
                    } else {
                        r.push(',');
                    }
                    r.push_str(&format!("{key}:{value}"));
                }
                r
            }
        };
        Ok(repr)
    }
}
