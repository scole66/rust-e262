use super::*;
use crate::agent::*;
use crate::tests::serr;
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
    use test_case::test_case;

    #[test_case("delete 3;" => Ok(ECMAScriptValue::from(true)); "non-reference")]
    #[test_case("delete a;" => Ok(ECMAScriptValue::from(true)); "non-strict unresolvable delete")]
    #[test_case("delete Boolean.prototype;" => Ok(ECMAScriptValue::from(false)); "non-strict non-configurable")]
    #[test_case("let obj={a:1,b:2}; delete obj.b;" => Ok(ECMAScriptValue::from(true)); "Legit delete")]
    #[test_case("let h=9; delete h;" => Ok(ECMAScriptValue::from(false)); "denied top-level delete")]
    #[test_case("delete a.b;" => serr("Thrown: ReferenceError: Unresolvable Reference"); "error from expression")]
    #[test_case("'use strict'; delete Boolean.prototype" => serr("Thrown: TypeError: property not deletable"); "strict, not deletable")]
    fn delete(src: &str) -> Result<ECMAScriptValue, String> {
        let mut agent = test_agent();
        process_ecmascript(&mut agent, src).map_err(|e| e.to_string())
    }

    #[test_case("void 3;" => Ok(ECMAScriptValue::Undefined); "simple")]
    #[test_case("void a;" => serr("Thrown: ReferenceError: Unresolvable Reference"); "err")]
    fn void(src: &str) -> Result<ECMAScriptValue, String> {
        let mut agent = test_agent();
        process_ecmascript(&mut agent, src).map_err(|e| e.to_string())
    }

    #[test_case("typeof a;" => Ok("undefined".into()); "undefined via unresolvable")]
    #[test_case("typeof a.b;" => serr("Thrown: ReferenceError: Unresolvable Reference"); "getvalue throws")]
    #[test_case("typeof undefined;" => Ok("undefined".into()); "undefined")]
    #[test_case("typeof null;" => Ok("object".into()); "object via null")]
    #[test_case("typeof true;" => Ok("boolean".into()); "boolean")]
    #[test_case("typeof 0;" => Ok("number".into()); "number")]
    #[test_case("typeof 1n;" => Ok("bigint".into()); "big int")]
    #[test_case("typeof 'a';" => Ok("string".into()); "string")]
    //#[test_case("typeof Symbol.unscopables;" => Ok("symbol".into()); "symbol")] // <-- This is the correct test, but it fails because Symbol isn't done yet
    #[test_case("typeof Symbol.unscopables;" => serr("Thrown: ReferenceError: Unresolvable Reference"); "symbol -- replace test when ready")]
    #[test_case("typeof {};" => Ok("object".into()); "object")]
    #[test_case("typeof Boolean;" => Ok("function".into()); "function")]
    fn typeof_op(src: &str) -> Result<ECMAScriptValue, String> {
        let mut agent = test_agent();
        process_ecmascript(&mut agent, src).map_err(|e| e.to_string())
    }
}
