use super::*;
use crate::errors::create_type_error_object;
use crate::object::{ordinary_object_create, set, DeadObject};
use crate::realm::IntrinsicId;
use crate::tests::{test_agent, unwind_type_error};
use crate::values::{to_number, to_string};

mod prototype {
    use super::*;

    mod valueof {
        use super::*;

        #[test]
        fn happy() {
            let mut agent = test_agent();
            let value = ECMAScriptValue::from(10);

            let result = object_prototype_value_of(&mut agent, value, None, &[]).unwrap();
            match &result {
                ECMAScriptValue::Object(obj) => {
                    assert!(obj.o.is_number_object());
                    assert_eq!(to_number(&mut agent, result).unwrap(), 10.0);
                }
                _ => {
                    panic!("Object.prototype.valueOf did not return an object. (Got: {:?})", result);
                }
            }
        }
        #[test]
        fn err() {
            let mut agent = test_agent();
            let result = object_prototype_value_of(&mut agent, ECMAScriptValue::Undefined, None, &[]).unwrap_err();
            assert_eq!(unwind_type_error(&mut agent, result), "Undefined and null cannot be converted to objects");
        }
    }

    mod to_string {
        use super::*;
        use test_case::test_case;

        fn greasy(agent: &mut Agent) -> ECMAScriptValue {
            // Return an object whose @@toStringTag property has the value "Grease"
            let to_string_tag_symbol = agent.wks(WksId::ToStringTag);
            let obj = ordinary_object_create(agent, None, &[]);
            set(agent, &obj, PropertyKey::from(to_string_tag_symbol), ECMAScriptValue::from("Grease"), false).unwrap();
            ECMAScriptValue::from(obj)
        }

        #[test_case(|_| ECMAScriptValue::Undefined => "[object Undefined]"; "undefined")]
        #[test_case(|_| ECMAScriptValue::Null => "[object Null]"; "null")]
        #[test_case(|_| ECMAScriptValue::from(99) => "[object Number]"; "number")]
        #[test_case(|_| ECMAScriptValue::from(true) => "[object Boolean]"; "boolean")]
        #[test_case(|agent| ECMAScriptValue::from(create_type_error_object(agent, "test_error")) => "[object Error]"; "error object")]
        #[test_case(|agent| ECMAScriptValue::from(agent.intrinsic(IntrinsicId::Boolean)) => "[object Function]"; "callable object")]
        #[test_case(|agent| ECMAScriptValue::from(ordinary_object_create(agent, None, &[])) => "[object Object]"; "ordinary object")]
        #[test_case(greasy => "[object Grease]"; "to-string-tag")]
        #[test_case(|agent| ECMAScriptValue::from(DeadObject::object(agent)) => "get called on DeadObject"; "throw getting tag")]
        fn f(make: fn(agent: &mut Agent) -> ECMAScriptValue) -> String {
            let mut agent = test_agent();
            let value = make(&mut agent);
            match object_prototype_to_string(&mut agent, value, None, &[]) {
                Ok(ok) => match ok {
                    ECMAScriptValue::String(s) => String::from(s),
                    _ => panic!("Object.prototype.toString did not return a string. (Got: {:?})", ok),
                },
                Err(err) => unwind_type_error(&mut agent, err),
            }
        }
    }
}

mod constructor {
    use super::*;
    use test_case::test_case;

    #[test_case(|a| Some(ordinary_object_create(a, None, &[])), &[ECMAScriptValue::from(10)] => "10"; "new target but no active function")]
    #[test_case(|a| {
        let obj = ordinary_object_create(a, None, &[]);
        a.running_execution_context_mut().unwrap().function = Some(obj.clone());
        Some(obj)
    }, &[ECMAScriptValue::from(11)] => "11"; "related new target")]
    #[test_case(|a| {
        let obj = ordinary_object_create(a, None, &[]);
        a.running_execution_context_mut().unwrap().function = Some(obj.clone());
        Some(ordinary_object_create(a, None, &[]))
    }, &[ECMAScriptValue::from(12)] => "[object Object]"; "unrelated new target")]
    #[test_case(|_| None, &[ECMAScriptValue::Null] => "[object Object]"; "null value")]
    #[test_case(|_| None, &[ECMAScriptValue::Undefined] => "[object Object]"; "undefined value")]
    fn function(new_target: fn(&mut Agent) -> Option<Object>, args: &[ECMAScriptValue]) -> String {
        let mut agent = test_agent();
        let nt = new_target(&mut agent);

        match object_constructor_function(&mut agent, ECMAScriptValue::Undefined, nt.as_ref(), args) {
            Ok(ok) => match ok {
                ECMAScriptValue::Object(obj) => String::from(to_string(&mut agent, obj.into()).unwrap()),
                _ => panic!("Object() did not return an object. (Got: {:?})", ok),
            },
            Err(err) => unwind_type_error(&mut agent, err),
        }
    }
}
