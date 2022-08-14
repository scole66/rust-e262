use super::*;
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
    #[test_case("typeof Symbol.unscopables;" => Ok("symbol".into()); "symbol")]
    #[test_case("typeof {};" => Ok("object".into()); "object")]
    #[test_case("typeof Boolean;" => Ok("function".into()); "function")]
    fn typeof_op(src: &str) -> Result<ECMAScriptValue, String> {
        let mut agent = test_agent();
        process_ecmascript(&mut agent, src).map_err(|e| e.to_string())
    }
}

mod member_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("m.a.c" => serr("Thrown: ReferenceError: Unresolvable Reference"); "member expression throws")]
    #[test_case("m.a" => serr("Thrown: ReferenceError: Unresolvable Reference"); "getvalue throws")]
    #[test_case("let m={a: 10}; m.a;" => vok(10); "id member exp")]
    #[test_case("m.a[c]" => serr("Thrown: ReferenceError: Unresolvable Reference"); "exp syntax; member exp throws")]
    #[test_case("m[c]" => serr("Thrown: ReferenceError: Unresolvable Reference"); "exp syntax; getvalue throws")]
    #[test_case("let m={};m[c]" => serr("Thrown: ReferenceError: Unresolvable Reference"); "exp syntax; getname throws")]
    #[test_case("let m={},c={toString:undefined, valueOf:undefined};m[c]" => serr("Thrown: TypeError: Cannot convert object to primitive value"); "exp syntax; tokey throws")]
    #[test_case("let m={'1':99};m[1]" => vok(99); "exp member exp")]
    fn run(src: &str) -> Result<ECMAScriptValue, String> {
        let mut agent = test_agent();
        process_ecmascript(&mut agent, src).map_err(|e| e.to_string())
    }
}

mod exponentiation_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("3**2" => vok(9); "Simple square")]
    //#[test_case("NaN**NaN" => vok(f64::NAN); "NaN**NaN")]
    #[test_case("NaN**0" => vok(1); "NaN**0")]
    #[test_case("Infinity ** 1" => vok(f64::INFINITY); "Infinity**1")]
    #[test_case("Infinity ** -3" => vok(0); "Infinity**-3")]
    #[test_case("(-Infinity) ** 3" => vok(f64::NEG_INFINITY); "-Inf ** 3")]
    #[test_case("(-Infinity) ** 8" => vok(f64::INFINITY) ; "-Inf ** 8")]
    fn run(src: &str) -> Result<ECMAScriptValue, String> {
        let mut agent = test_agent();
        process_ecmascript(&mut agent, src).map_err(|e| e.to_string())
    }
}

mod if_statement {
    use super::*;
    use test_case::test_case;

    #[test_case("if (true) 3;" => vok(3); "no else; true path")]
    #[test_case("if (false) 3;" => vok(ECMAScriptValue::Undefined); "no else; false path")]
    #[test_case("if (true) 1; else -1;" => vok(1); "has else; true path")]
    #[test_case("if (false) 1; else -1;" => vok(-1); "has else; false path")]
    #[test_case("if (a) 1; else -1;" => serr("Thrown: ReferenceError: Unresolvable Reference"); "err in expr")]
    #[test_case("if (true) a; else -1;" => serr("Thrown: ReferenceError: Unresolvable Reference"); "err in stmt1")]
    #[test_case("if (false) 1; else a;" => serr("Thrown: ReferenceError: Unresolvable Reference"); "err in stmt2")]
    fn run(src: &str) -> Result<ECMAScriptValue, String> {
        let mut agent = test_agent();
        process_ecmascript(&mut agent, src).map_err(|e| e.to_string())
    }
}

mod do_while {
    use super::*;
    use test_case::test_case;

    #[test_case("do { 1; } while (false);" => vok(1); "runs once")]
    #[test_case("let idx=0, result=''; do { result = result + idx++; } while (idx < 10);" => vok("0123456789"); "proof of iteration")]
    #[test_case("let idx=0, result=''; do { result = result + idx++; if (idx >= 5) break; } while (idx < 10);" => vok("01234"); "break works")]
    #[test_case("let idx=0, result=''; do { result = result + idx++; if (idx % 2 == 0) continue; result = result + '-'; } while (idx < 10);" => vok("0-12-34-56-78-9"); "continue works")]
    #[test_case("do a; while (false);" => serr("Thrown: ReferenceError: Unresolvable Reference"); "err in stmt")]
    #[test_case("do null; while (a);" => serr("Thrown: ReferenceError: Unresolvable Reference"); "err in expr")]
    fn run(src: &str) -> Result<ECMAScriptValue, String> {
        let mut agent = test_agent();
        process_ecmascript(&mut agent, src).map_err(|e| e.to_string())
    }
}

mod labelled_statement {
    use super::*;
    use test_case::test_case;

    #[test_case(r"
        const limit=2;
        let row=0, result='Result:';
        outer: do {
            let col = 0;
            const in_row = row++;
            inner: do {
                result = result + ' (' + col + ', ' + in_row + ')';
                col ++;
                if (col > limit)
                    continue outer;
            } while (true);
        } while (row <= limit);
        result;" => vok("Result: (0, 0) (1, 0) (2, 0) (0, 1) (1, 1) (2, 1) (0, 2) (1, 2) (2, 2)"); "labelled continue")]
    #[test_case(r"
        let result = '';
        outer_block: {
            inner_block: {
                result = result + '1 ';
                break outer_block; // breaks out of both inner_block and outer_block
                result = result + ':-('; // skipped
            }
            result = result + ' 2'; // skipped
        }
        result;
    " => vok("1 "); "loopless targeted break")]
    fn run(src: &str) -> Result<ECMAScriptValue, String> {
        let mut agent = test_agent();
        process_ecmascript(&mut agent, src).map_err(|e| e.to_string())
    }
}

mod function_prototype_call {
    use super::*;
    use test_case::test_case;

    #[test_case("(x => x * 2).call(undefined, 99);" => vok(198); "call a user defined func")]
    #[test_case("Number.prototype.toString.call(991)" => vok("991"); "call a builtin func")]
    fn run(src: &str) -> Result<ECMAScriptValue, String> {
        let mut agent = test_agent();
        process_ecmascript(&mut agent, src).map_err(|e| e.to_string())
    }
}
