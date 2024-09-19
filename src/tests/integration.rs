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
            let a = result_obj.get(&"a".into()).unwrap();
            let b = result_obj.get(&"b".into()).unwrap();
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
            let a = result_obj.get(&"a".into()).unwrap();
            let b = result_obj.get(&"b".into()).unwrap();
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
            let a = result_obj.get(&"a".into()).unwrap();
            let b = result_obj.get(&"b".into()).unwrap();
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
            let a = result_obj.get(&"a".into()).unwrap();
            let b = result_obj.get(&"b".into()).unwrap();
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

// Optional Chains
//  OptionalExpression: MemberExpression OptionalChain
//    a[4]?.b -> ReferenceError
//    new Proxy({}, {"get": () => { throw new TypeError("value-get-fail"); }}).c?.b -> TypeError
//    undefined?.b -> Undefined
//    null?.b -> Undefined
//    ({"b": 10})?.b -> 10
//    ({"b": 10})?.[(() => { throw 'chain fail'; })()] -> error
#[test_case("a[4]?.b" => serr("Thrown: ReferenceError: Unresolvable Reference"); "oe: me oc ; me eval fails")]
#[test_case(
    "new Proxy({}, {'get': () => { throw new TypeError('value-get-fail'); }}).c?.b"
    => serr("Thrown: TypeError: value-get-fail");
    "oe: me oc; get-value fails"
)]
#[test_case("undefined?.b" => Ok(ECMAScriptValue::Undefined); "oe: me oc ; me undefined value")]
#[test_case("null?.b" => Ok(ECMAScriptValue::Undefined); "oe: me oc ; me null value")]
#[test_case("({'b': 10})?.b" => vok(10); "oe: me oc ; me ok, oc ok")]
#[test_case(
    "({'b': 10})?.[(() => { throw 'chain fail';})()]"
    => serr("Thrown: chain fail");
    "oe: me oc ; oc fails"
)]
//  OptionalExpression: CallExpression OptionalChain
//    a()?.b -> ReferenceError
//    (() => new Proxy({}, {"get": () => { throw 'value-get-fail'; }}))().c?.b -> error
//    (() => null)()?.b -> Undefined
//    (() => undefined)()?.b -> Undefined
//    (() => ({"b": 10}))()?.b -> 10
//    (() => ({"b": 10}))()?.[(() => { throw 'chain fail'; })()] -> error
#[test_case("a()?.b" => serr("Thrown: ReferenceError: Unresolvable Reference"); "oe: ce oc ; ce eval fails")]
#[test_case(
    "(() => new Proxy({}, {'get': () => { throw 'value-get-fail'; }}))().c?.b"
    => serr("Thrown: value-get-fail");
    "oe: ce oc ; ce get-value fails"
)]
#[test_case("(() => null)()?.b" => Ok(ECMAScriptValue::Undefined); "oe: ce oc ; ce null value")]
#[test_case("(() => undefined)()?.b" => Ok(ECMAScriptValue::Undefined); "oe: ce oc ; ce undefined value")]
#[test_case("(() => ({'b': 10}))()?.b" => vok(10); "oe: ce oc; oc ok")]
#[test_case("(() => ({'b': 10}))()?.[(() => { throw 'chain fail'; })()]" => serr("Thrown: chain fail"))]
//  OptionalExpression: OptionalExpression OptionalChain
//    a[4]?.c?.d -> Uncaught ReferenceError (evaluation of OptionalExpression fails)
//    new Proxy({}, {'get': () => { throw 0; } })?.x?.y -> Uncaught 0 (GetValue(baseReference) fails)
//    undefined?.a?.b -> undefined
//    ({'a': null})?.a?.b -> undefined
//    ({'a': 'b'})?.a?.length -> 1
//    ({'a': 'b'})?.a?.[(() => { throw 0; })()] -> Uncaught 0 (ChainEvaluation of OptionalChain fails)
#[test_case(
    "a[4]?.c?.d"
    => serr("Thrown: ReferenceError: Unresolvable Reference");
    "oe: oe oc; oe eval fails"
)]
#[test_case(
    "new Proxy({}, {'get': () => { throw 0; } })?.x?.y"
    => serr("Thrown: 0");
    "oe: oe oc; GetValue(baseReference) fails"
)]
#[test_case("undefined?.a?.b" => Ok(ECMAScriptValue::Undefined); "oe: oe oc; baseValue is undefined")]
#[test_case("({'a': null})?.a?.b" => Ok(ECMAScriptValue::Undefined); "oe: oe oc; baseValue is null")]
#[test_case("({'a': 'b'})?.a?.length" => vok(1); "oe: oe oc; ChainEvaluation of OptionalChain successful")]
#[test_case(
    "({'a': 'b'})?.a?.[(() => { throw 0; })()]"
    => serr("Thrown: 0");
    "oe: oe oc; ChainEvaluation of OptionalChain fails"
)]
//  OptionalChain: ?. Arguments
//    String?.('a')  -> 'a'  (EvaluateCall succeeds)
//    (() => { throw 0; })?.()  -> Uncaught 0 (EvaluateCall fails)
#[test_case("String?.('a')" => vok("a"); "oc: args; call ok")]
#[test_case("(() => { throw 0; })?.()" => serr("Thrown: 0"); "oc: args; call fails")]
//  OptionalChain: ?. [ Expression ]
//    [3]?.[0] -> 3 (EvaluatePropertyAccessWithExpressionKey succeeds)
//    [3]?.[(() => { throw 0; })()] -> Uncaught 0 (EvaluatePropertyAccessWithExpressionKey fails)
#[test_case("[3]?.[0]" => vok(3); "oc: expr; expr ok")]
#[test_case("[3]?.[(() => { throw 0; })()]" => serr("Thrown: 0"); "oc: expr; expr fails")]
//  OptionalChain: ?. IdentifierName
//    ({'a': 3})?.a -> 3 (cannot fail, as it only makes a reference)
#[test_case("({'a': 3})?.a" => vok(3); "oc: id; cannot fail")]
//  OptionalChain: ?. PrivateIdentifier
//    Todo: this needs classes
//  OptionalChain: OptionalChain Arguments
//    (() => { throw 0; })?.()() -> Uncaught 0 (ChainEvaluation of OptionalChain fails)
//    (new Proxy({}, {'get': () => { throw 0; }}))?.a()  -> Uncaught 0 (GetValue(newReference) fails)
//    ({'a':3})?.a() -> Uncaught TypeError (EvaluateCall fails)
//    String?.fromCharCode(60) -> '<' (EvaluateCall ok)
#[test_case("(() => { throw 0; })?.()()" => serr("Thrown: 0"); "oc: oc args; oc fails")]
#[test_case(
    "(new Proxy({}, {'get': () => { throw 0; }}))?.a()"
    => serr("Thrown: 0");
    "oc: oc args; GetValue(newReference) fails"
)]
#[test_case("({'a':3})?.a()" => serr("Thrown: TypeError: not an object"); "oc: oc args; EvaluateCall fails")]
#[test_case("String?.fromCharCode(60)" => vok("<"); "oc: oc args; no errors")]
//  OptionalChain: OptionalChain [ Expression ]
//    (() => {throw 0;})?.()[1] -> Uncaught 0 (ChainEvaluation of OptionalChain fails)
//    (new Proxy({}, {'get': () => { throw 0; }}))?.a[1] -> Uncaught 0 (GetValue(newReference) fails)
//    ({'a':1})?.a[(() => { throw 0; })()] -> Uncaught 0 (EvaluatePropertyAccessWithExpressionKey fails)
//    ({'a':[9]})?.a[0] -> 9 (EvaluatePropertyAccessWithExpressionKey ok)
#[test_case("(() => {throw 0;})?.()[1]" => serr("Thrown: 0"); "oc: oc exp; oc fails")]
#[test_case(
    "(new Proxy({}, {'get': () => { throw 0; }}))?.a[1]"
    => serr("Thrown: 0");
    "oc: oc exp; getValue fails"
)]
#[test_case("({'a':1})?.a[(() => { throw 0; })()]" => serr("Thrown: 0"); "oc: oc exp; exp fails")]
#[test_case("({'a':[9]})?.a[0]" => vok(9); "oc: oc exp; success")]
//  OptionalChain: OptionalChain . IdentifierName
//    (() => { throw 0; })?.().a -> Uncaught 0 (ChainEvaluation of OptionalChain fails)
//    (new Proxy({}, {'get': () => { throw 0; }}))?.a.a -> Uncaught 0 (GetValue(newReference) fails)
//    ({'a':{'b':'c'}})?.a.b => 'c' (EvaluatePropertyAccessWithIdentifierKey ok)
#[test_case("(() => { throw 0; })?.().a" => serr("Thrown: 0"); "oc: oc id; oc fails")]
#[test_case(
    "(new Proxy({}, {'get': () => { throw 0; }}))?.a.a"
    => serr("Thrown: 0");
    "oc: oc id; getValue fails"
)]
#[test_case("({'a':{'b':'c'}})?.a.b" => vok("c"); "oc: oc id; success")]
//  OptionalChain: OptionalChain . PrivateIdentifier
//    Todo: this needs classes

// Array Functions
// Array.isArray
//   Array.isArray(13) -> false
//   Array.isArray([1, 2, 3]) -> true
//   Array.isArray((()=>{let p=Proxy.revocable({},{});p.revoke();return p.proxy;})()) -> TypeError
#[test_case("Array.isArray(13)" => vok(false); "Array.isArray on not-array")]
#[test_case("Array.isArray([1, 2, 3])" => vok(true); "Array.isArray on an array")]
#[test_case(
    "Array.isArray((()=>{let p=Proxy.revocable({},{});p.revoke();return p.proxy;})())"
    => serr("Thrown: TypeError: Proxy has been revoked");
    "Array.isArray throws"
)]
// get Array[@@species]
//   Array[Symbol.species] == Array    -> true
#[test_case("Array[Symbol.species] == Array" => vok(true); "get Array[@@species]")]
// Array.prototype
//  Array.prototype.pop
//   { let a = [1, 2, 3]; let r = a.pop(); a.length == 2 && r == 3 && !a.hasOwnProperty('2') } -> true
//   [].pop() -> undefined
//   Array.prototype.pop.call(undefined) -> TypeError (ToObject fails)
//   Array.prototype.pop.call({ length: Symbol.iterator }) -> TypeError (LengthOfArrayLike fails)
//   { let a = { length: -1 }; let r = Array.prototype.pop.call(a); a.length == 0 && r === undefined } -> true
//   { let a = { length: -1 }; Object.freeze(a); Array.prototype.pop.call(a); } -> TypeError (first Set fails)
//   Array.prototype.pop.call(new Proxy({}, {'get':(r,key)=>{if(key=='length'){return 3;}throw 0;}})) -> Uncaught 0 (Get fails)
//   { let a = [1,2]; Object.freeze(a); a.pop() } -> TypeError (DeletePropertyOrThrow fails)
//   Array.prototype.pop.call(new Proxy([1],{'set':()=>{throw 0;}})) -> Uncaught 0 (second Set fails)
#[test_case(
    "{ let a = [1, 2, 3]; let r = a.pop(); a.length == 2 && r == 3 && !a.hasOwnProperty('2') }"
    => vok(true);
    "Array.prototype.pop nonempty list"
)]
#[test_case("[].pop()" => vok(ECMAScriptValue::Undefined); "Array.prototype.pop empty list")]
#[test_case(
    "Array.prototype.pop.call(undefined)"
    => serr("Thrown: TypeError: Undefined and null cannot be converted to objects");
    "Array.prototype.pop: ToObject throws"
)]
#[test_case(
    "Array.prototype.pop.call({ length: Symbol.iterator })"
    => serr("Thrown: TypeError: Symbol values cannot be converted to Number values");
    "Array.prototype.pop: LengthOfArrayLike throws"
)]
#[test_case(
    "{ let a = { length: -1 }; let r = Array.prototype.pop.call(a); a.length == 0 && r === undefined }"
    => vok(true);
    "Array.prototype.pop: setting length to zero on an empty arraylike"
)]
#[test_case(
    "{ let a = { length: -1 }; Object.freeze(a); Array.prototype.pop.call(a); }"
    => serr("Thrown: TypeError: Cannot add property, for one of many different possible reasons");
    "Array.prototype.pop: first Set throws"
)]
#[test_case(
    "Array.prototype.pop.call(new Proxy({}, {'get':(r,key)=>{if(key=='length'){return 3;}throw 0;}}))"
    => serr("Thrown: 0");
    "Array.prototype.pop: Get throws"
)]
#[test_case(
    "{ let a = [1,2]; Object.freeze(a); a.pop() }"
    => serr("Thrown: TypeError: Property could not be deleted");
    "Array.prototype.pop: DeletePropertyOrThrow throws"
)]
#[test_case(
    "Array.prototype.pop.call(new Proxy([1],{'set':()=>{throw 0;}}))"
    => serr("Thrown: 0");
    "Array.prototype.pop: second Set throws"
)]
//  Array.prototype.push
//   { let a = [1, 2, 3]; let r = a.push(4, 5, 6); r === 6 && a.join(',') === "1,2,3,4,5,6"; } -> true
//
#[test_case(
    "{ let a = [1, 2, 3]; let r = a.push(4, 5, 6); r === 6 && a.join(',') === '1,2,3,4,5,6'; }"
    => vok(true);
    "Array.prototype.push: success"
)]
#[test_case(
    "Array.prototype.push.call(undefined)"
    => serr("Thrown: TypeError: Undefined and null cannot be converted to objects");
    "Array.prototype.push: ToObject throws"
)]
#[test_case(
    "Array.prototype.push.call({ length: Symbol.iterator })"
    => serr("Thrown: TypeError: Symbol values cannot be converted to Number values");
    "Array.prototype.push: LengthOfArrayLike throws"
)]
#[test_case(
    "Array.prototype.push.call({'length': 2**53 - 1}, 10)"
    => serr("Thrown: TypeError: Array too large");
    "Array.prototype.push: length check"
)]
#[test_case(
    "{ let a = [1, 2, 3]; Object.freeze(a); Array.prototype.push.call(a, 10); }"
    => serr("Thrown: TypeError: Cannot add property, for one of many different possible reasons");
    "Array.prototype.push: first Set fails"
)]
#[test_case(
    r"
    Array.prototype.push.call(
        new Proxy(
            {
                'length': 0
            },
            {
                'set': (target, key, value) => {
                    if (key == 'length') { throw 0; }
                    target[key]=value;
                    return true;
                }
            }
        ),
        1
    )
    "
    => serr("Thrown: 0");
    "Array.prototype.push: second Set fails"
)]
// MethodDefinition: get ClassElementName ( ) { FunctionBody }
#[test_case("let o = { get a() { return 3; } }; o.a" => vok(3); "method_def_get_call_works")]
#[test_case("let o = { get a() { return 3; } }; Object.getOwnPropertyDescriptor(o, 'a').get.name" => vok("get a"); "method_def_get_name")]
#[test_case("let o = { get a() { return 3; } }; Object.getOwnPropertyDescriptor(o, 'a').get.length" => vok(0); "method_def_get_length")]
#[test_case("let o = { get [(()=>{throw 0;})()]() { return 3; } };" => serr("Thrown: 0"); "method_def_get_name_throws")]
// Reflect
#[test_case("Reflect.apply(Array.prototype.map, [1, 2, 3], [x => x * 2]).join(', ')" => vok("2, 4, 6"); "Reflect.apply builtin function")]
#[test_case("Reflect.apply(x => x / 3, undefined, [9])" => vok(3); "Reflect.apply function object")]
#[test_case("Reflect.apply(10, undefined, [])" => serr("Thrown: TypeError: Reflect.apply requires a callable target"); "Reflect.apply: target not callable")]
#[test_case("Reflect.apply(x => x * 2, undefined, undefined)" => serr("Thrown: TypeError: CreateListFromArrayLike called on non-object"); "Reflect.apply CreateListFromArrayLike throws")]
#[test_case("Reflect.apply(x => { throw 'oops'; }, undefined, [])" => serr("Thrown: oops"); "Reflect.apply call throws")]
#[test_case("Reflect.construct(Array, [1, 2, 3]).join(', ')" => vok("1, 2, 3"); "Reflect.construct: builtin, no new.target")]
#[test_case("let a = Reflect.construct(Array, [1, 2, 3], String); `${a.trim.name}; ${Array.isArray(a)}`" => vok("trim; true"); "Reflect.construct builtin with new.target")]
#[test_case("Reflect.construct(function (x) { return { value: x }; }, [27]).value" => vok(27); "Reflect.construct function object")]
#[test_case("Reflect.construct({}, [])" => serr("Thrown: TypeError: Reflect.construct: target must be a constructor"); "Reflect.construct: target must be a constructor")]
#[test_case("Reflect.construct(Array, [1, 2, 3], Reflect)" => serr("Thrown: TypeError: Reflect.construct: newTarget, if supplied, must be a constructor"); "Reflect.construct: new.target not constructor")]
#[test_case("Reflect.construct(Array, undefined)" => serr("Thrown: TypeError: CreateListFromArrayLike called on non-object"); "Reflect.construct: CreateListFromArrayLike throws")]
#[test_case("Reflect.construct(function (x) { throw 'oops'; }, [1, 2, 3])" => serr("Thrown: oops"); "Reflect.construct: construct call throws")]
#[test_case("let a={}; let result = Reflect.defineProperty(a, 'key', { value: 'some value' }); `Result: ${result}; a.key: ${a.key}`" => vok("Result: true; a.key: some value"); "Reflect.defineProperty; normal")]
#[test_case("Reflect.defineProperty(10, 'key', undefined)" => serr("Thrown: TypeError: Reflect.defineProperty: target must be an object"); "Reflect.defineProperty: target not an object")]
#[test_case("Reflect.defineProperty({}, {toString() { throw 'nope'; }}, { value: 'some value' })" => serr("Thrown: nope"); "Reflect.defineProperty: ToPropertyKey throws")]
#[test_case("Reflect.defineProperty({}, 'key', undefined)" => serr("Thrown: TypeError: Must be an object"); "Reflect.defineProperty: ToPropertyDescriptor throws")]
// RelationalExpression : RelationalExpression in ShiftExpression
#[test_case("(() => { throw 'oops'; })() in String" => serr("Thrown: oops"); "in-operator: Evaluation(RelationalExpression) throws")]
#[test_case("({ get a() { throw 'oops'; }}).a in String" => serr("Thrown: oops"); "in-operator: GetValue(lref) throws")]
#[test_case("'a' in (() => { throw 'oops'; })()" => serr("Thrown: oops"); "in-operator: Evaluation(ShiftExpression) throws")]
#[test_case("'a' in ({ get b() { throw 'oops'; }}).b" => serr("Thrown: oops"); "in-operator: GetValue(rref) throws")]
#[test_case("'a' in 3" => serr("Thrown: TypeError: Right-hand side of 'in' must be an object"); "in-operator: rval not an object")]
#[test_case("({ [Symbol.toPrimitive]: () => { throw 'oops'; }}) in String" => serr("Thrown: oops"); "in-operator: ToPropertyKey(lval) throws")] // 5/28/2024: in operator incorrectly handles exceptions
#[test_case("'a' in new Proxy({}, { has(p) { throw 'oops'; }})" => serr("Thrown: oops"); "in-operator: HasProperty(...) throws")]
#[test_case("'a' in ({a: 10})" => Ok(ECMAScriptValue::Boolean(true)); "in-operator: success (true)")]
#[test_case("'b' in ({a: 10})" => Ok(ECMAScriptValue::Boolean(false)); "in-operator: success (false)")]
// Number.isInteger
#[test_case("Number.isInteger(20)" => Ok(ECMAScriptValue::Boolean(true)); "value 20")]
#[test_case("Number.isInteger(20.0001)" => Ok(ECMAScriptValue::Boolean(false)); "value 20.0001")]
#[test_case("Number.isInteger(Infinity)" => Ok(ECMAScriptValue::Boolean(false)); "value Infinity")]
#[test_case("Number.isInteger('sixteen')" => Ok(ECMAScriptValue::Boolean(false)); "value 'sixteen'")]
// Boolean.toString
#[test_case("true.toString()" => Ok(ECMAScriptValue::from("true")); "Boolean.prototype.toString(true)")]
#[test_case("false.toString()" => Ok(ECMAScriptValue::from("false")); "Boolean.prototype.toString(false)")]
#[test_case("Boolean.prototype.toString.call(0)" => serr("Thrown: TypeError: Value is not boolean"); "Boolean.prototype.toString non-boolean this")]
// Boolean.valueOf
#[test_case("true.valueOf()" => Ok(ECMAScriptValue::from(true)); "Boolean.prototype.valueOf(true)")]
#[test_case("false.valueOf()" => Ok(ECMAScriptValue::from(false)); "Boolean.prototype.valueOf(false)")]
#[test_case("Boolean.prototype.valueOf.call(0)" => serr("Thrown: TypeError: Value is not boolean"); "Boolean.prototype.valueOf non-boolean this")]
// Boolean constructor
#[test_case("Boolean(10)" => Ok(ECMAScriptValue::from(true)); "Boolean as func")]
#[test_case("new Boolean(true).valueOf()" => Ok(ECMAScriptValue::from(true)); "Boolean as constructor")]
#[test_case(
    "function X() { this.x = 0; }
     x = new Proxy(X, { 'get' : (obj, key) => { throw `get(${obj}, ${key})`; }});
     Reflect.construct(Boolean, [true], x)"
    => serr("Thrown: get(function X() { this.x = 0; }, prototype)");
    "boolean construction fails in new-from-constructor"
)]
// this_boolean_value
#[test_case("Boolean.prototype.valueOf.call(new Object())" => serr("Thrown: TypeError: Object has no boolean value"); "not-boolean object in this_boolean_value")]
// Boolean::uses_ordinary_get_prototype_of
#[test_case("Object.setPrototypeOf(new String(''), new Boolean(true)).valueOf()" => serr("Thrown: TypeError: Object has no boolean value"); "Boolean::uses_ordinary_get_prototype_of")]
// is_boolean_obj on boolean object
#[test_case("Object.prototype.toString.call(new Boolean(true))" => Ok(ECMAScriptValue::from("[object Boolean]")); "BooleanObject::is_boolean_object coverage")]
#[test_case("Object.getPrototypeOf(Object.setPrototypeOf(new Boolean(), null))" => Ok(ECMAScriptValue::Null); "BooleanObject::set_prototype_of coverage")]
#[test_case("Object.isFrozen(Object.freeze(new Boolean()))" => Ok(ECMAScriptValue::from(true)); "BooleanObject::prevent_extensions coverage")]
#[test_case("let bo = new Boolean(); bo.prop = 10; bo.prop" => Ok(ECMAScriptValue::from(10)); "BooleanObject::set coverage")]
#[test_case("Reflect.has(new Boolean(), 'mystery')" => Ok(ECMAScriptValue::from(false)); "BooleanObject::has_property coverage")]
#[test_case("let bo = new Boolean(); bo.prop = 10; delete bo.prop; Object.hasOwnProperty(bo, 'prop')" => Ok(ECMAScriptValue::from(false)); "BooleanObject::delete coverage")]
#[test_case("Number.prototype.valueOf.call(new Boolean())" => serr("Thrown: TypeError: Number method called with non-number receiver"); "BooleanObject::to_number_obj coverage")]
#[test_case("String.prototype.valueOf.call(new Boolean())" => serr("Thrown: TypeError: String.prototype.valueOf requires that 'this' be a String"); "BooleanObject::to_string_obj coverage")]
// Class Stuff
#[test_case("class C {}; `${C.prototype.constructor === C}, ${Object.getPrototypeOf(new C()) === C.prototype}`" => Ok(ECMAScriptValue::from("true, true")); "class: empty")]
#[test_case("const C = class {}; `${C.prototype.constructor === C}, ${Object.getPrototypeOf(new C()) === C.prototype}`" => Ok(ECMAScriptValue::from("true, true")); "class expression: empty")]
#[test_case("let C = ({cls: class{}}).cls; `${C.prototype.constructor === C}, ${Object.getPrototypeOf(new C()) === C.prototype}`" => Ok(ECMAScriptValue::from("true, true")); "class object literal: empty")]
#[test_case("class C extends Boolean {}; `${C.prototype.constructor === C}, ${Object.getPrototypeOf(new C()) === C.prototype}, ${Object.getPrototypeOf(C) === Boolean}`" => Ok(ECMAScriptValue::from("true, true, true")); "class extends: empty")]
#[test_case("class C extends @@~ {}" => serr("During compilation: [SyntaxError: @@~ token detected. aborting compilation.]"); "class: ClassHeritage compile fails")]
#[test_case(
    "class C {
        constructor(val) {
            this.value = val;
        }
    };
    const c = new C('sentinel');
    c.value
    "
    => Ok(ECMAScriptValue::from("sentinel"));
    "class: has constructor function (no heritage)"
)]
#[test_case(
    "
    class C {
        #left;
        #right;
        constructor(left, right) {
            this.#left = left;
            this.#right = right;
        }
        build() {
            return `${this.#left} ~~~ ${this.#right}`;
        }
    }
    const c = new C('fire', 'truck');
    c.build()
    "
    => Ok(ECMAScriptValue::from("fire ~~~ truck"));
    "class: constructor plus private elements"
)]
#[test_case(
    "
    class C {
        constructor(left, right = @@~) {
        }
    }
    "
    => serr("Thrown: TypeError: @@~ token detected. aborting compilation.");
    "class: constructor define-method throws"
)]
#[test_case(
    "
    class C {
        static [@@~];
    }
    "
    => serr("During compilation: [SyntaxError: @@~ token detected. aborting compilation.]");
    "class: class element compile fails"
)]
#[test_case(
    "
    class C extends String {
        constructor(a, b) {
            super(a);
            this.other = b;
        }
        build() {
            return this + this.other;
        }
    };
    const ss = new C('one', 'two');
    ss.build()
    "
    => Ok(ECMAScriptValue::from("onetwo"));
    "class: heritage + fields"
)]
#[test_case(
    "
    class C extends String {
        constructor(a, b) {
            super(a);
            this.other = b;
        }
        build() {
            return @@~;
        }
    };
    const ss = new C('one', 'two');
    ss.build()
    "
    => serr("Thrown: TypeError: @@~ token detected. aborting compilation.");
    "class: method compile fails"
)]
// ############# Random "it didn't work right" source text #############
// This first item is 4/23/2023: the stack is messed up for errors in function parameters
#[test_case("function id(x=(()=>{throw 'howdy';})()) {
                return x;
            }
            id()"
    => serr("Thrown: howdy")
    ; "handle errors in function parameter evaluation")]
// This next pair is 4/23/2023: the stack is messed up for errors in binding patterns for function parameters
// when errors happen
#[test_case("function foo([{a=50, b, c}]) {
                return [a, b, c];
            }
            foo(1, 2, 3, 4, 5)"
=> serr("Thrown: TypeError: object is not iterable")
; "handle errors in function parameter binding patterns")]
#[test_case("function foo([{a=50, b, c}] = [{a:-1,b:-2,c:-3}]) {
               return [a, b, c];
            }
            foo(1, 2, 3, 4, 5)"
=> serr("Thrown: TypeError: object is not iterable")
; "handle errors in function parameter binding patterns with initializers")]
// 4/24/2023: if an array pattern had unfinished iterators, it would fail
#[test_case("var [] = [];" => Ok(ECMAScriptValue::Undefined); "incomplete iterators finished")]
#[test_case(
    "var v = 1; for (let v of [v]) ;"
    => serr("Thrown: ReferenceError: Binding not initialized");
    "let decl doesn't see outer scope"
)]
// 1/1/2024: argument processing doesn't handle errors
#[test_case("(function (p = (() => { throw 'oops'; })(), q) {})()" => serr("Thrown: oops"); "errs in args")]
// 5/28/2024: method definitions with arrow function values on symbol names don't work.
#[test_case("String({ [Symbol.toPrimitive]: () => { return \"me\"; } })" => Ok(ECMAScriptValue::from("me")); "arrow function name")]
// 6/12/2024: bad arguments in generator call
#[test_case("(function *(a=(()=>{throw 'oops';})()){})()" => serr("Thrown: oops"); "errs in generator args")]
// 6/13/2024: destructuring rest failure
#[test_case("(function([...x]=['test']){return x[0];})()" => Ok(ECMAScriptValue::from("test")); "destructuring rest failure")]
// 6/13/2024: prototype chain for generator expressions
#[test_case("Object.getOwnPropertyDescriptor(Object.getPrototypeOf(Object.getPrototypeOf((function*(){})())),Symbol.toStringTag)?.value" => Ok(ECMAScriptValue::from("Generator")); "prototype chain for generator expressions")]
// 9/15/2024: arguments object not working?
#[test_case(
    "
    function foo(a)
    {
      a = 1;
      return arguments[0];
    }

    foo(10)
    "
    => Ok(ECMAScriptValue::from(1));
    "arguments object (mapped)"
)]
// 9/18/2024: private method generators
#[test_case(
    "
    class C {
        *#method(a) { yield a; }
        get method() { return this.#method; }
    };
    (new C()).method(789).next().value
    "
    => Ok(ECMAScriptValue::from(789));
    "class: generator method with private name"
)]
// 9/19/2024: static methods not attached correctly
#[test_case(
    "
    class C {
      static #item(a) { return a; }
      static get item() { return this.#item; }
    }
    C.item(982)
    "
    => Ok(ECMAScriptValue::from(982));
    "class: static method with private name not attached correctly"
)]
fn code(src: &str) -> Result<ECMAScriptValue, String> {
    setup_test_agent();
    process_ecmascript(src).map_err(|e| e.to_string())
}
