use super::*;
use crate::agent::*;
use crate::values::*;
use num::BigInt;

mod update_expression {
    use super::*;

    mod post_increment {
        use super::*;
        use test_case::test_case;

        #[test_case("let a = 10;let b = a++;({ a, b })" => (11.into(), 10.into()); "number")]
        #[test_case("let a = 10n;let b = a++;({ a, b })" => (BigInt::from(11).into(), BigInt::from(10).into()); "bigint")]
        fn normal(src: &str) -> (ECMAScriptValue, ECMAScriptValue) {
            let mut agent = test_agent();
            let result = process_ecmascript(&mut agent, src).unwrap();

            let result_obj = to_object(&mut agent, result).unwrap();
            let a = get(&mut agent, &result_obj, &"a".into()).unwrap();
            let b = get(&mut agent, &result_obj, &"b".into()).unwrap();
            (a, b)
        }

        #[test_case("a.b++" => "Thrown: ReferenceError: Unresolvable Reference"; "LHS errs")]
        #[test_case("({}).b.c++;" => "Thrown: TypeError: Undefined and null cannot be converted to objects"; "GetValue_errs")]
        #[test_case("let a={ toString: undefined, valueOf: undefined };a++;" => "Thrown: TypeError: Cannot convert object to primitive value"; "ToNumeric errs")]
        #[test_case("const a=0;a++;" => "Thrown: TypeError: Cannot change read-only value"; "PutValue errs")]
        fn errors(src: &str) -> String {
            let mut agent = test_agent();
            let result = process_ecmascript(&mut agent, src).unwrap_err();
            result.to_string()
        }
    }

    mod post_decrement {
        use super::*;
        use test_case::test_case;

        #[test_case("let a = 10;let b = a--;({ a, b })" => (9.into(), 10.into()); "number")]
        #[test_case("let a = 10n;let b = a--;({ a, b })" => (BigInt::from(9).into(), BigInt::from(10).into()); "bigint")]
        fn normal(src: &str) -> (ECMAScriptValue, ECMAScriptValue) {
            let mut agent = test_agent();
            let result = process_ecmascript(&mut agent, src).unwrap();

            let result_obj = to_object(&mut agent, result).unwrap();
            let a = get(&mut agent, &result_obj, &"a".into()).unwrap();
            let b = get(&mut agent, &result_obj, &"b".into()).unwrap();
            (a, b)
        }

        #[test_case("a.b--" => "Thrown: ReferenceError: Unresolvable Reference"; "LHS errs")]
        #[test_case("({}).b.c--;" => "Thrown: TypeError: Undefined and null cannot be converted to objects"; "GetValue_errs")]
        #[test_case("let a={ toString: undefined, valueOf: undefined };a--;" => "Thrown: TypeError: Cannot convert object to primitive value"; "ToNumeric errs")]
        #[test_case("const a=0;a--;" => "Thrown: TypeError: Cannot change read-only value"; "PutValue errs")]
        fn errors(src: &str) -> String {
            let mut agent = test_agent();
            let result = process_ecmascript(&mut agent, src).unwrap_err();
            result.to_string()
        }
    }

    mod pre_increment {
        use super::*;
        use test_case::test_case;

        #[test_case("let a = 10;let b = ++a;({ a, b })" => (11.into(), 11.into()); "number")]
        #[test_case("let a = 10n;let b = ++a;({ a, b })" => (BigInt::from(11).into(), BigInt::from(11).into()); "bigint")]
        fn normal(src: &str) -> (ECMAScriptValue, ECMAScriptValue) {
            let mut agent = test_agent();
            let result = process_ecmascript(&mut agent, src).unwrap();

            let result_obj = to_object(&mut agent, result).unwrap();
            let a = get(&mut agent, &result_obj, &"a".into()).unwrap();
            let b = get(&mut agent, &result_obj, &"b".into()).unwrap();
            (a, b)
        }

        #[test_case("++a.b" => "Thrown: ReferenceError: Unresolvable Reference"; "LHS errs")]
        #[test_case("++({}).b.c;" => "Thrown: TypeError: Undefined and null cannot be converted to objects"; "GetValue_errs")]
        #[test_case("let a={ toString: undefined, valueOf: undefined };++a;" => "Thrown: TypeError: Cannot convert object to primitive value"; "ToNumeric errs")]
        #[test_case("const a=0;++a;" => "Thrown: TypeError: Cannot change read-only value"; "PutValue errs")]
        fn errors(src: &str) -> String {
            let mut agent = test_agent();
            let result = process_ecmascript(&mut agent, src).unwrap_err();
            result.to_string()
        }
    }

    mod pre_decrement {
        use super::*;
        use test_case::test_case;

        #[test_case("let a = 10;let b = --a;({ a, b })" => (9.into(), 9.into()); "number")]
        #[test_case("let a = 10n;let b = --a;({ a, b })" => (BigInt::from(9).into(), BigInt::from(9).into()); "bigint")]
        fn normal(src: &str) -> (ECMAScriptValue, ECMAScriptValue) {
            let mut agent = test_agent();
            let result = process_ecmascript(&mut agent, src).unwrap();

            let result_obj = to_object(&mut agent, result).unwrap();
            let a = get(&mut agent, &result_obj, &"a".into()).unwrap();
            let b = get(&mut agent, &result_obj, &"b".into()).unwrap();
            (a, b)
        }

        #[test_case("--a.b" => "Thrown: ReferenceError: Unresolvable Reference"; "LHS errs")]
        #[test_case("--({}).b.c;" => "Thrown: TypeError: Undefined and null cannot be converted to objects"; "GetValue_errs")]
        #[test_case("let a={ toString: undefined, valueOf: undefined };--a;" => "Thrown: TypeError: Cannot convert object to primitive value"; "ToNumeric errs")]
        #[test_case("const a=0;--a;" => "Thrown: TypeError: Cannot change read-only value"; "PutValue errs")]
        fn errors(src: &str) -> String {
            let mut agent = test_agent();
            let result = process_ecmascript(&mut agent, src).unwrap_err();
            result.to_string()
        }
    }
}

mod unary_expression {
    use super::*;

    mod delete {
        use super::*;
        use test_case::test_case;

        #[test_case("delete 3;" => ECMAScriptValue::from(true); "non-reference")]
        #[test_case("delete a;" => ECMAScriptValue::from(true); "non-strict unresolvable delete")]
        #[test_case("delete Boolean.prototype;" => ECMAScriptValue::from(false); "non-strict non-configurable")]
        #[test_case("let obj={a:1,b:2}; delete obj.b;" => ECMAScriptValue::from(true); "Legit delete")]
        #[test_case("let h=9; delete h;" => ECMAScriptValue::from(false); "denied top-level delete")]
        fn normal(src: &str) -> ECMAScriptValue {
            let mut agent = test_agent();
            let result = process_ecmascript(&mut agent, src).unwrap();
            result
        }
        #[test_case("delete a.b;" => "Thrown: ReferenceError: Unresolvable Reference"; "error from expression")]
        #[test_case("'use strict'; delete Boolean.prototype" => "Thrown: TypeError: property not deletable"; "strict, not deletable")]
        fn errors(src: &str) -> String {
            let mut agent = test_agent();
            let result = process_ecmascript(&mut agent, src).unwrap_err();
            result.to_string()
        }
    }
}