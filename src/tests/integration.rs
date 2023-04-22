use super::*;
use num::BigInt;
use test_case::test_case;

mod update_expression {
    use super::*;

    mod post_increment {
        use super::*;
        use test_case::test_case;

        #[test_case("let a = 10;let b = a++;({ a, b })" => (11.into(), 10.into()); "number")]
        #[test_case("let a = 10n;let b = a++;({ a, b })" => (BigInt::from(11).into(), BigInt::from(10).into()); "bigint")]
        fn normal(src: &str) -> (ECMAScriptValue, ECMAScriptValue) {
            setup_test_agent();
            let result = process_ecmascript(src).unwrap();

            let result_obj = to_object(result).unwrap();
            let a = get(&result_obj, &"a".into()).unwrap();
            let b = get(&result_obj, &"b".into()).unwrap();
            (a, b)
        }

        #[test_case("a.b++" => "Thrown: ReferenceError: Unresolvable Reference"; "LHS errs")]
        #[test_case("({}).b.c++;" => "Thrown: TypeError: Undefined and null cannot be converted to objects"; "GetValue_errs")]
        #[test_case("let a={ toString: undefined, valueOf: undefined };a++;" => "Thrown: TypeError: Cannot convert object to primitive value"; "ToNumeric errs")]
        #[test_case("const a=0;a++;" => "Thrown: TypeError: Cannot change read-only value"; "PutValue errs")]
        fn errors(src: &str) -> String {
            setup_test_agent();
            let result = process_ecmascript(src).unwrap_err();
            result.to_string()
        }
    }

    mod post_decrement {
        use super::*;
        use test_case::test_case;

        #[test_case("let a = 10;let b = a--;({ a, b })" => (9.into(), 10.into()); "number")]
        #[test_case("let a = 10n;let b = a--;({ a, b })" => (BigInt::from(9).into(), BigInt::from(10).into()); "bigint")]
        fn normal(src: &str) -> (ECMAScriptValue, ECMAScriptValue) {
            setup_test_agent();
            let result = process_ecmascript(src).unwrap();

            let result_obj = to_object(result).unwrap();
            let a = get(&result_obj, &"a".into()).unwrap();
            let b = get(&result_obj, &"b".into()).unwrap();
            (a, b)
        }

        #[test_case("a.b--" => "Thrown: ReferenceError: Unresolvable Reference"; "LHS errs")]
        #[test_case("({}).b.c--;" => "Thrown: TypeError: Undefined and null cannot be converted to objects"; "GetValue_errs")]
        #[test_case("let a={ toString: undefined, valueOf: undefined };a--;" => "Thrown: TypeError: Cannot convert object to primitive value"; "ToNumeric errs")]
        #[test_case("const a=0;a--;" => "Thrown: TypeError: Cannot change read-only value"; "PutValue errs")]
        fn errors(src: &str) -> String {
            setup_test_agent();
            let result = process_ecmascript(src).unwrap_err();
            result.to_string()
        }
    }

    mod pre_increment {
        use super::*;
        use test_case::test_case;

        #[test_case("let a = 10;let b = ++a;({ a, b })" => (11.into(), 11.into()); "number")]
        #[test_case("let a = 10n;let b = ++a;({ a, b })" => (BigInt::from(11).into(), BigInt::from(11).into()); "bigint")]
        fn normal(src: &str) -> (ECMAScriptValue, ECMAScriptValue) {
            setup_test_agent();
            let result = process_ecmascript(src).unwrap();

            let result_obj = to_object(result).unwrap();
            let a = get(&result_obj, &"a".into()).unwrap();
            let b = get(&result_obj, &"b".into()).unwrap();
            (a, b)
        }

        #[test_case("++a.b" => "Thrown: ReferenceError: Unresolvable Reference"; "LHS errs")]
        #[test_case("++({}).b.c;" => "Thrown: TypeError: Undefined and null cannot be converted to objects"; "GetValue_errs")]
        #[test_case("let a={ toString: undefined, valueOf: undefined };++a;" => "Thrown: TypeError: Cannot convert object to primitive value"; "ToNumeric errs")]
        #[test_case("const a=0;++a;" => "Thrown: TypeError: Cannot change read-only value"; "PutValue errs")]
        fn errors(src: &str) -> String {
            setup_test_agent();
            let result = process_ecmascript(src).unwrap_err();
            result.to_string()
        }
    }

    mod pre_decrement {
        use super::*;
        use test_case::test_case;

        #[test_case("let a = 10;let b = --a;({ a, b })" => (9.into(), 9.into()); "number")]
        #[test_case("let a = 10n;let b = --a;({ a, b })" => (BigInt::from(9).into(), BigInt::from(9).into()); "bigint")]
        fn normal(src: &str) -> (ECMAScriptValue, ECMAScriptValue) {
            setup_test_agent();
            let result = process_ecmascript(src).unwrap();

            let result_obj = to_object(result).unwrap();
            let a = get(&result_obj, &"a".into()).unwrap();
            let b = get(&result_obj, &"b".into()).unwrap();
            (a, b)
        }

        #[test_case("--a.b" => "Thrown: ReferenceError: Unresolvable Reference"; "LHS errs")]
        #[test_case("--({}).b.c;" => "Thrown: TypeError: Undefined and null cannot be converted to objects"; "GetValue_errs")]
        #[test_case("let a={ toString: undefined, valueOf: undefined };--a;" => "Thrown: TypeError: Cannot convert object to primitive value"; "ToNumeric errs")]
        #[test_case("const a=0;--a;" => "Thrown: TypeError: Cannot change read-only value"; "PutValue errs")]
        fn errors(src: &str) -> String {
            setup_test_agent();
            let result = process_ecmascript(src).unwrap_err();
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
        setup_test_agent();
        process_ecmascript(src).map_err(|e| e.to_string())
    }

    #[test_case("void 3;" => Ok(ECMAScriptValue::Undefined); "simple")]
    #[test_case("void a;" => serr("Thrown: ReferenceError: Unresolvable Reference"); "err")]
    fn void(src: &str) -> Result<ECMAScriptValue, String> {
        setup_test_agent();
        process_ecmascript(src).map_err(|e| e.to_string())
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
        setup_test_agent();
        process_ecmascript(src).map_err(|e| e.to_string())
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
        setup_test_agent();
        process_ecmascript(src).map_err(|e| e.to_string())
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
        setup_test_agent();
        process_ecmascript(src).map_err(|e| e.to_string())
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
        setup_test_agent();
        process_ecmascript(src).map_err(|e| e.to_string())
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
        setup_test_agent();
        process_ecmascript(src).map_err(|e| e.to_string())
    }
}

#[test_case("while(false); 1" => vok(1); "runs zero")]
#[test_case("let i=0, result=[]; while(i<10){result[i]=i;i++;} result.toString()" => vok("0,1,2,3,4,5,6,7,8,9"); "proof of iteration")]
#[test_case("let i=0, result=''; while(i<10){result=result+i++; if (i>=5) break;}" => vok("01234"); "break works")]
#[test_case("let i=0, result=''; while(i<10){result=result+i++;if (i%2==0)continue; result = result+'-';}" => vok("0-12-34-56-78-9"); "continue works")]
#[test_case("let i=0,result=''; outer: while(i<10){let j=0; i++; while(j<10) {result=result+j++; if (j>i-1) continue outer;}}" => vok("0010120123012340123450123456012345670123456780123456789"); "labelled continue works")]
fn while_statement(src: &str) -> Result<ECMAScriptValue, String> {
    setup_test_agent();
    process_ecmascript(src).map_err(|e| e.to_string())
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
        setup_test_agent();
        process_ecmascript(src).map_err(|e| e.to_string())
    }
}

#[test_case(r"new String('9876543210')[3];" => vok("6"); "as-object string items as indexed properties")]
#[test_case(r"new String('9876543210').length;" => vok(10); "as-object string length")]
#[test_case(r"'9876543210'[2];" => vok("7"); "coerced string indexed properties")]
#[test_case(r"'9876543210'.length;" => vok(10); "coerced string length")]
fn string_exotic_object(src: &str) -> Result<ECMAScriptValue, String> {
    setup_test_agent();
    process_ecmascript(src).map_err(|e| e.to_string())
}

#[test_case("String()" => vok(""); "no args")]
#[test_case("String('hhhh')" => vok("hhhh"); "one string")]
#[test_case("typeof new String('apple')" => vok("object"); "constructed")]
#[test_case("new String(Symbol('oops'))" => serr("Thrown: TypeError: Symbols may not be converted to strings"); "error in constructor")]
#[test_case("String(Symbol('oops'))" => vok("Symbol(oops)"); "symbol in function")]
fn string_constructor(src: &str) -> Result<ECMAScriptValue, String> {
    setup_test_agent();
    process_ecmascript(src).map_err(|e| e.to_string())
}

#[test_case("String.fromCharCode(112, 97, 115, 115)" => vok("pass"); "normal")]
#[test_case("String.fromCharCode()" => vok(""); "empty")]
#[test_case("String.fromCharCode(112, 97, 115, 115, Symbol('a'), 88)" => serr("Thrown: TypeError: Symbol values cannot be converted to Number values"); "bad args")]
#[test_case("String.fromCharCode.name" => vok("fromCharCode"); "name")]
#[test_case("String.fromCharCode.length" => vok(1); "length")]
fn string_from_char_code(src: &str) -> Result<ECMAScriptValue, String> {
    setup_test_agent();
    process_ecmascript(src).map_err(|e| e.to_string())
}

#[test_case("'12345'.indexOf('2')" => vok(1); "found a match")]
#[test_case("'12345'.indexOf('25')" => vok(-1); "no match")]
#[test_case("'12345'.indexOf('23', 2)" => vok(-1); "start after substring")]
#[test_case("'123..123'.indexOf('23', 3)" => vok(6); "second match")]
#[test_case("String.prototype.indexOf.call(null)" => serr("Thrown: TypeError: Undefined and null are not allowed in this context"); "fail location 1")]
#[test_case("String.prototype.indexOf.call(Symbol('a'))" => serr("Thrown: TypeError: Symbols may not be converted to strings"); "fail location 2")]
#[test_case("'a'.indexOf(Symbol('a'))" => serr("Thrown: TypeError: Symbols may not be converted to strings"); "fail location 3")]
#[test_case("'a'.indexOf('a', Symbol('a'))" => serr("Thrown: TypeError: Symbol values cannot be converted to Number values"); "fail location 4")]
#[test_case("'what if things are undefined?'.indexOf()" => vok(19); "first arg missing")]
fn string_prototype_index_of(src: &str) -> Result<ECMAScriptValue, String> {
    setup_test_agent();
    process_ecmascript(src).map_err(|e| e.to_string())
}

#[test_case("String.prototype.toString.call(0)" => serr("Thrown: TypeError: String.prototype.toString requires that 'this' be a String"); "bad this")]
#[test_case("new String('alpha').toString()" => vok("alpha"); "string object")]
#[test_case("'alpha'.toString()" => vok("alpha"); "string literal")]
fn string_prototype_to_string(src: &str) -> Result<ECMAScriptValue, String> {
    setup_test_agent();
    process_ecmascript(src).map_err(|e| e.to_string())
}

#[test_case("String.prototype.valueOf.call(0)" => serr("Thrown: TypeError: String.prototype.valueOf requires that 'this' be a String"); "bad this")]
#[test_case("new String('alpha').valueOf()" => vok("alpha"); "string object")]
#[test_case("'alpha'.valueOf()" => vok("alpha"); "string literal")]
fn string_prototype_value_of(src: &str) -> Result<ECMAScriptValue, String> {
    setup_test_agent();
    process_ecmascript(src).map_err(|e| e.to_string())
}

#[test_case("(x => x * 2).call(undefined, 99);" => vok(198); "call a user defined func")]
#[test_case("Number.prototype.toString.call(991)" => vok("991"); "call a builtin func")]
fn function_prototype_call(src: &str) -> Result<ECMAScriptValue, String> {
    setup_test_agent();
    process_ecmascript(src).map_err(|e| e.to_string())
}

#[test_case("let a=[]; 'length: ' + a.length + ', 0: ' + a[0]" => vok("length: 0, 0: undefined"); "empty array literal")]
#[test_case("let a=[1,2]; 'length: ' + a.length + ', 0: ' + a[0] + ', 1: ' + a[1]" => vok("length: 2, 0: 1, 1: 2"); "simple element list")]
#[test_case("let a=[,,]; 'length: ' + a.length + ', 0: ' + a[0] + ', 1: ' + a[1]" => vok("length: 2, 0: undefined, 1: undefined"); "only elisions")]
#[test_case("let a=[,10]; 'length: ' + a.length + ', 0: ' + a[0] + ', 1: ' + a[1]" => vok("length: 2, 0: undefined, 1: 10"); "elision at start")]
#[test_case("let a=[-10,,10]; 'length: ' + a.length + ', 0: ' + a[0] + ', 1: ' + a[1] + ', 2: ' + a[2]" => vok("length: 3, 0: -10, 1: undefined, 2: 10"); "elision in middle")]
#[test_case("let a=[10,,]; 'length: ' + a.length + ', 0: ' + a[0] + ', 1: ' + a[1]" => vok("length: 2, 0: 10, 1: undefined"); "elision at end")]
#[test_case("let a=[(() => { throw 'exception' })()];" => serr("Thrown: exception"); "exceptions in elements")]
#[test_case("let a=[1,(() => { throw 'exception' })()];" => serr("Thrown: exception"); "exceptions in tail elements")]
#[test_case("const x=10, y=20; const a=[x,y]; 'length: ' + a.length + ', 0: ' + a[0] + ', 1: ' + a[1]" => vok("length: 2, 0: 10, 1: 20"); "id refs")]
#[test_case("[100].toString()" => vok("100"); "element")]
#[test_case("[,100].toString()" => vok(",100"); "elision+element")]
#[test_case("let a=100; [a].toString()" => vok("100"); "fallible element")]
#[test_case("let a=[100]; [...a].toString()" => vok("100"); "spread")]
#[test_case("let a=[100]; [,...a].toString()" => vok(",100"); "elision+spread")]
#[test_case("[100,200].toString()" => vok("100,200"); "list+element")]
#[test_case("let a=100; [a,200].toString()" => vok("100,200"); "list+element; list fallible")]
#[test_case("let a=200; [100,a].toString()" => vok("100,200"); "list+element; element fallible")]
#[test_case("[100,,200].toString()" => vok("100,,200"); "list+elision+element")]
#[test_case("let a=[200]; [100,...a].toString()" => vok("100,200"); "list+spread")]
#[test_case("let b=100, a=[200]; [b,...a].toString()" => vok("100,200"); "list+spread; list fallible")]
#[test_case("let a=[200]; [100,,...a].toString()" => vok("100,,200"); "list+elision+spread")]
#[test_case("[...0]" => serr("Thrown: TypeError: object is not iterable"); "invalid spread target")]
fn array_literal(src: &str) -> Result<ECMAScriptValue, String> {
    setup_test_agent();
    process_ecmascript(src).map_err(|e| e.to_string())
}

#[test_case("Number(0)" => vok(0); "CallExpression: MemberExpression Arguments")]
#[test_case("let a=(b)=>''+b; a()" => vok("undefined"); "empty arguments")]
#[test_case("(()=>()=>3)()()" => vok(3); "CallExpression: CallExpression Arguments")]
fn call_expression(src: &str) -> Result<ECMAScriptValue, String> {
    setup_test_agent();
    process_ecmascript(src).map_err(|e| e.to_string())
}

#[test_case("'['+(Array()).toString()+']'" => vok("[]"); "Array()")]
#[test_case("(Array(10)).toString()" => vok(",,,,,,,,,"); "Array(10)")]
#[test_case("(new Array(1,2)).toString()" => vok("1,2"); "new Array(1,2)")]
#[test_case("(new Array(true)).toString()" => vok("true"); "new Array(true)")]
#[test_case("Array(54.3)" => serr("Thrown: RangeError: Bad length in array construction"); "Array(54.3)")]
fn array_constructor_function(src: &str) -> Result<ECMAScriptValue, String> {
    setup_test_agent();
    process_ecmascript(src).map_err(|e| e.to_string())
}

// Arguments: ( )
#[test_case("f()" => vok(""); "empty list")]
// Arguments: ( ArgumentList )
#[test_case("f(1)" => vok("1"); "one item, infallible")]
#[test_case("f(a)" => serr("Thrown: ReferenceError: Unresolvable Reference"); "one item, fallible, fails")]
#[test_case("const a=99;f(a)" => vok("99"); "one item, fallible, succeeds")]
// Arguments: ( ArgumentList , )
#[test_case("f(1,)" => vok("1"); "one item, comma, infallible")]
#[test_case("f(a,)" => serr("Thrown: ReferenceError: Unresolvable Reference"); "one item, comma, fallible, fails")]
#[test_case("const a=99;f(a,)" => vok("99"); "one item, comma, fallible, succeeds")]
// ArgumentList: AssignmentExpression (handled above)
// ArgumentList: ... AssignmentExpression
#[test_case("const a=[10];f(...a)" => vok("10"); "spread; fallible ref; succeeds")]
#[test_case("f(...a)" => serr("Thrown: ReferenceError: Unresolvable Reference"); "spread; fallible ref; fails")]
#[test_case("const a=true;f(...a)" => serr("Thrown: TypeError: object is not iterable"); "spread; fallible ref; iter fails")]
#[test_case("f(...10)" => serr("Thrown: TypeError: object is not iterable"); "spread; infallible expr; iter fails")]
#[test_case("f(...[])" => vok(""); "spread; infallible expr; iter successful")]
// ArgumentList: ArgumentList , AssignmentExpression
#[test_case("f('a', 'b')" => vok("a,b"); "list-exp; infallible both")]
#[test_case("const a='a'; f(a, 'b')" => vok("a,b"); "list-exp; list fallible; succeeds")]
#[test_case("f(a, 'b')" => serr("Thrown: ReferenceError: Unresolvable Reference"); "list-exp; list fails")]
#[test_case("const b='b'; f('a', b)" => vok("a,b"); "list-exp; exp ref; exp succeeds")]
#[test_case("f('a', b)" => serr("Thrown: ReferenceError: Unresolvable Reference"); "list-exp; exp ref; exp fails")]
#[test_case("f('a', t())" => serr("Thrown: thrown"); "list-exp; exp not ref; exp fails")]
#[test_case("f('a', c())" => vok("a,c"); "list-exp; exp not ref; exp succeeds")]
// ArgumentList: ArgumentList , ... AssignmentExpression
#[test_case("f(1, ...[1])" => vok("1,1"); "list-spread; all infallible; successful")]
#[test_case("f(1, ...3)" => serr("Thrown: TypeError: object is not iterable"); "list-spread; all infallible; iter fails")]
#[test_case("f(a, ...[1])" => serr("Thrown: ReferenceError: Unresolvable Reference"); "list-spread; list is ref; list fails")]
#[test_case("const a='a'; f(a, ...[1])" => vok("a,1"); "list-spread; list is ref; successful")]
#[test_case("f(c(),...['b'])" => vok("c,b"); "list-spread; list is non-ref, fallible; successful")]
#[test_case("f(t(),...['b'])" => serr("Thrown: thrown"); "list-spread; list non-ref, list fails")]
#[test_case("f(c(),...t())" => serr("Thrown: thrown"); "list-spread; spread non-ref, fails")]
#[test_case("f(c(),...a)" => serr("Thrown: ReferenceError: Unresolvable Reference"); "list-spread; spread ref, fails")]
#[test_case("const a=['a'];f(c(),...a)" => vok("c,a"); "list-spread; spread-ref; successful")]
#[test_case("f('b', ...['a', 'c'], 'm', ...['x', 'y', 'z'])" => vok("b,a,c,m,x,y,z"); "spread element arguments")]
fn argument_list(src: &str) -> Result<ECMAScriptValue, String> {
    setup_test_agent();
    let program = format!(
        "
        function f() {{
            let r = [];
            for (let i = 0; i < arguments.length; i++)
                r[i] = arguments[i];
            return r.toString();
        }}
        function t() {{
            throw 'thrown';
        }}
        let c = () => 'c';
        {src}
        "
    );
    process_ecmascript(&program).map_err(|e| e.to_string())
}

// ############# Random "it didn't work right" source text #############
// This first item is 4/23/2023: the stack is messed up for errors in function parameters
#[test_case("function id(x=(()=>{throw 'howdy';})()) {
        return x;
    }
    id()"
    => serr("Thrown: howdy")
    ; "handle errors in function parameter evaluation")]
fn code(src: &str) -> Result<ECMAScriptValue, String> {
    setup_test_agent();
    process_ecmascript(src).map_err(|e| e.to_string())
}
