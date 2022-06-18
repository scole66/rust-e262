use super::*;
use crate::chunk::Chunk;
use crate::parser::testhelp::{svec, Maker};
use crate::strings::JSString;
use crate::tests::*;
use num::BigInt;
use std::rc::Rc;

mod insn {
    use super::*;
    use test_case::test_case;

    #[test_case(Insn::Nop => "NOP"; "Nop instruction")]
    #[test_case(Insn::String => "STRING"; "String instruction")]
    #[test_case(Insn::Resolve => "RESOLVE"; "Resolve instruction")]
    #[test_case(Insn::StrictResolve => "STRICT_RESOLVE"; "StrictResolve instruction")]
    #[test_case(Insn::This => "THIS"; "This instruction")]
    #[test_case(Insn::Null => "NULL"; "Null instruction")]
    #[test_case(Insn::True => "TRUE"; "True instruction")]
    #[test_case(Insn::False => "FALSE"; "False instruction")]
    #[test_case(Insn::Float => "FLOAT"; "Float instruction")]
    #[test_case(Insn::Bigint => "BIGINT"; "Bigint instruction")]
    #[test_case(Insn::GetValue => "GET_VALUE"; "GetValue instruction")]
    #[test_case(Insn::JumpIfAbrupt => "JUMP_IF_ABRUPT"; "JumpIfAbrupt instruction")]
    #[test_case(Insn::UpdateEmpty => "UPDATE_EMPTY"; "UpdateEmpty instruction")]
    #[test_case(Insn::Undefined => "UNDEFINED"; "Undefined instruction")]
    #[test_case(Insn::Empty => "EMPTY"; "Empty instruction")]
    #[test_case(Insn::PutValue => "PUT_VALUE"; "PutValue instruction")]
    #[test_case(Insn::Jump => "JUMP"; "Jump instruction")]
    #[test_case(Insn::JumpIfNormal => "JUMP_IF_NORMAL"; "JumpIfNormal instruction")]
    #[test_case(Insn::Call => "CALL"; "Call instruction")]
    #[test_case(Insn::Swap => "SWAP"; "Swap instruction")]
    #[test_case(Insn::Pop => "POP"; "Pop instruction")]
    #[test_case(Insn::Pop2Push3 => "POP2_PUSH3"; "Pop2Push3 instruction")]
    #[test_case(Insn::Dup => "DUP"; "Dup instruction")]
    #[test_case(Insn::Unwind => "UNWIND"; "Unwind instruction")]
    #[test_case(Insn::Ref => "REF"; "Ref instruction")]
    #[test_case(Insn::StrictRef => "STRICT_REF"; "StrictRef instruction")]
    #[test_case(Insn::InitializeReferencedBinding => "IRB"; "InitializeReferencedBinding instruction")]
    #[test_case(Insn::Object => "OBJECT"; "Object instruction")]
    #[test_case(Insn::CreateDataProperty => "CR_PROP"; "CreateDataProperty instruction")]
    #[test_case(Insn::SetPrototype => "SET_PROTO"; "SetPrototype instruction")]
    #[test_case(Insn::ToPropertyKey => "TO_KEY"; "ToPropertyKey instruction")]
    #[test_case(Insn::CopyDataProps => "COPY_DATA_PROPS"; "CopyDataProps instruction")]
    #[test_case(Insn::ToNumeric => "TO_NUMERIC"; "ToNumeric instruction")]
    #[test_case(Insn::Increment => "INCREMENT"; "Increment instruction")]
    #[test_case(Insn::Decrement => "DECREMENT"; "Decrement instruction")]
    #[test_case(Insn::PreDecrement => "PRE_DECREMENT"; "PreDecrement instruction")]
    #[test_case(Insn::PreIncrement => "PRE_INCREMENT"; "PreIncrement instruction")]
    #[test_case(Insn::Delete => "DELETE"; "Delete instruction")]
    #[test_case(Insn::Void => "VOID"; "Void instruction")]
    #[test_case(Insn::TypeOf => "TYPEOF"; "TypeOf instruction")]
    #[test_case(Insn::UnaryPlus => "UNARY_PLUS"; "UnaryPlus instruction")]
    #[test_case(Insn::UnaryMinus => "UNARY_MINUS"; "UnaryMinus instruction")]
    #[test_case(Insn::UnaryComplement => "UNARY_COMPLEMENT"; "UnaryComplement instruction")]
    #[test_case(Insn::UnaryNot => "UNARY_NOT"; "UnaryNot instruction")]
    #[test_case(Insn::Exponentiate => "EXPONENTIATE"; "Exponentiate instruction")]
    #[test_case(Insn::Multiply => "MULTIPLY"; "Multiply instruction")]
    #[test_case(Insn::Divide => "DIVIDE"; "Divide instruction")]
    #[test_case(Insn::Modulo => "MODULO"; "Modulo instruction")]
    #[test_case(Insn::Add => "ADD"; "Add instruction")]
    #[test_case(Insn::Subtract => "SUBTRACT"; "Subtract instruction")]
    fn display(insn: Insn) -> String {
        format!("{insn}")
    }

    #[test]
    fn debug() {
        assert_ne!(format!("{:?}", Insn::Bigint), "");
    }

    #[test_case(Insn::Null, Insn::Null => true; "equality")]
    #[test_case(Insn::Null, Insn::Float => false; "not equal")]
    fn eq(left: Insn, right: Insn) -> bool {
        left == right
    }

    #[test]
    fn u16_from() {
        let n1 = u16::from(Insn::False);
        let n2 = u16::from(Insn::String);
        assert_ne!(n1, n2);
    }

    #[test]
    fn try_from() {
        let insns = vec![Insn::String, Insn::Resolve, Insn::StrictResolve, Insn::UpdateEmpty];
        let ids = insns.iter().map(|&i| u16::from(i)).collect::<Vec<u16>>();
        let recovered = ids.iter().map(|&id| Insn::try_from(id).unwrap()).collect::<Vec<_>>();
        assert_eq!(insns, recovered);
    }

    #[test]
    #[allow(clippy::clone_on_copy)]
    fn clone() {
        let insn = Insn::String;
        let i2 = insn.clone();
        assert_eq!(insn, i2);
    }
}

mod compiler_status_flags {
    use super::*;

    #[test]
    fn default() {
        let csf = CompilerStatusFlags::default();
        assert_eq!(csf.can_be_reference, false);
        assert_eq!(csf.can_be_abrupt, false);
    }

    #[test]
    fn debug() {
        assert_ne!(format!("{:?}", CompilerStatusFlags::new()), "");
    }

    #[test]
    #[allow(clippy::clone_on_copy)]
    fn clone() {
        let csf1 = CompilerStatusFlags { can_be_abrupt: false, can_be_reference: true };
        let csf2 = csf1.clone();
        assert_eq!(csf1.can_be_abrupt, csf2.can_be_abrupt);
        assert_eq!(csf1.can_be_reference, csf2.can_be_reference);
    }

    #[test]
    fn new() {
        let csf1 = CompilerStatusFlags::default();
        let csf2 = CompilerStatusFlags::new();
        assert_eq!(csf1.can_be_abrupt, csf2.can_be_abrupt);
        assert_eq!(csf1.can_be_reference, csf2.can_be_reference);
    }

    #[test]
    fn abrupt() {
        let csf = CompilerStatusFlags::new().abrupt();
        assert_eq!(csf.can_be_abrupt, true);
    }

    #[test]
    fn reference() {
        let csf = CompilerStatusFlags::new().reference();
        assert_eq!(csf.can_be_reference, true);
    }
}

// A note about compile tests: These are really unit tests; they check that all the code paths are run, and that the
// opcodes output match what we wanted the opcodes to be. They do not test whether we chose the right opcodes to
// complete the requested task! Checking that really feels like a later stage integration task. (Both the compiler and
// the runtime executor need to be functioning. That's all probably based more or less on test-262.)

fn full_chunk(n: &str) -> Chunk {
    let mut c = Chunk::new(n);
    c.floats = vec![56878142.0; 65536];
    c.strings = Vec::with_capacity(65536);
    c.bigints = Vec::with_capacity(65536);
    for nbr in 0..65536 {
        c.strings.push(JSString::from(""));
        c.bigints.push(Rc::new(BigInt::from(nbr)));
    }
    c
}

fn almost_full_chunk(n: &str, slots_left: usize) -> Chunk {
    let mut c = Chunk::new(n);
    const LIMIT: usize = 65536;
    c.floats.resize(LIMIT - slots_left.min(LIMIT), 7489305.0);
    c.strings.resize(LIMIT - slots_left.min(LIMIT), JSString::from("filler"));
    c.bigints.resize(LIMIT - slots_left.min(LIMIT), Rc::new(BigInt::from(783)));
    c
}

mod identifier_reference {
    use super::*;

    mod compile {
        use super::*;
        use test_case::test_case;

        #[test_case("id", false => svec(&[
            "STRING 0 (id)",
            "RESOLVE"
        ]); "identifier, non-strict")]
        #[test_case("id", true => svec(&[
            "STRING 0 (id)",
            "STRICT_RESOLVE"
        ]); "identifier, strict")]
        #[test_case("yield", false => svec(&[
            "STRING 0 (yield)",
            "RESOLVE"
        ]); "yield, non-strict")]
        #[test_case("yield", true => svec(&[
            "STRING 0 (yield)",
            "STRICT_RESOLVE"
        ]); "yield, strict")]
        #[test_case("await", false => svec(&[
            "STRING 0 (await)",
            "RESOLVE"
        ]); "await, non-strict")]
        #[test_case("await", true => svec(&[
            "STRING 0 (await)",
            "STRICT_RESOLVE"
        ]); "await, strict")]
        fn normal(src: &str, strict: bool) -> Vec<String> {
            let node = Maker::new(src).yield_ok(false).await_ok(false).identifier_reference();
            let mut c = Chunk::new("identifier test");
            node.compile(&mut c, strict).unwrap();
            c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
        }

        #[test]
        fn error() {
            let node = Maker::new("id").identifier_reference();
            let mut c = full_chunk("name");
            let result = node.compile(&mut c, false).unwrap_err().to_string();
            assert_eq!(result, "Out of room for strings in this compilation unit");
        }
    }
}

mod primary_expression {
    use super::*;

    mod compile {
        use super::*;
        use test_case::test_case;

        #[test_case("id", true => svec(&[
            "STRING 0 (id)",
            "STRICT_RESOLVE"
        ]); "identifier ref, strict")]
        #[test_case("id", false => svec(&[
            "STRING 0 (id)",
            "RESOLVE"
        ]); "identifier ref, non strict")]
        #[test_case("this", true => svec(&[
            "THIS"
        ]); "this binding")]
        #[test_case("(id)", true => svec(&[
            "STRING 0 (id)",
            "STRICT_RESOLVE"
        ]); "strict parens")]
        #[test_case("(id)", false => svec(&[
            "STRING 0 (id)",
            "RESOLVE"
        ]); "non-strict parens")]
        #[test_case("true", true => svec(&[
            "TRUE"
        ]); "literal")]
        #[test_case("({})", true => svec(&["OBJECT"]); "object literal")]
        #[test_case("class {}", true => panics "not yet implemented"; "anything else")]
        fn normal(src: &str, strict: bool) -> Vec<String> {
            let node = Maker::new(src).primary_expression();
            let mut c = Chunk::new("pe");
            node.compile(&mut c, strict).unwrap();
            c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
        }
    }
}

mod literal {
    use super::*;

    mod compile {
        use super::*;
        use test_case::test_case;

        #[test_case("null" => svec(&["NULL"]); "null literal")]
        #[test_case("true" => svec(&["TRUE"]); "true literal")]
        #[test_case("false" => svec(&["FALSE"]); "false literal")]
        #[test_case("'apple'" => svec(&["STRING 0 (apple)"]); "string literal")]
        #[test_case("10" => svec(&["FLOAT 0 (10)"]); "number literal")]
        #[test_case("10n" => svec(&["BIGINT 0 (10)"]); "bigint literal")]
        fn normal(src: &str) -> Vec<String> {
            let node = Maker::new(src).literal();
            let mut c = Chunk::new("lit");
            node.compile(&mut c).unwrap();
            c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
        }

        #[test_case("'apple'" => "Out of room for strings in this compilation unit"; "string")]
        #[test_case("100" => "Out of room for floats in this compilation unit"; "float")]
        #[test_case("78907890n" => "Out of room for big ints in this compilation unit"; "bigint")]
        fn error(src: &str) -> String {
            let node = Maker::new(src).literal();
            let mut c = full_chunk("test");
            node.compile(&mut c).unwrap_err().to_string()
        }

        #[test]
        fn debug_lots_of_noops() {
            let node = Maker::new("@@@").literal();
            let mut c = Chunk::new("debug_lots_of_noops");
            node.compile(&mut c).unwrap();
            assert_eq!(c.opcodes.len(), 32769);
            assert_eq!(c.opcodes[0], Insn::Nop.into());
            for x in 1..32768 {
                assert_eq!(c.opcodes[x], c.opcodes[x - 1]);
            }
        }

        #[test]
        fn filled_string_table() {
            let node = Maker::new("@@!").literal();
            let mut c = Chunk::new("filled_string_table");
            node.compile(&mut c).unwrap();
            // The point of this literal is to fill the string table -- such that the call to add a string to the table
            // will fail. So that's what we test.
            assert!(c.add_to_string_pool("test".into()).is_err())
        }

        #[test]
        fn filled_float_table() {
            let node = Maker::new("@@#").literal();
            let mut c = Chunk::new("filled_float_table");
            node.compile(&mut c).unwrap();
            assert!(c.add_to_float_pool(0.0).is_err());
        }

        #[test]
        fn filled_bigint_table() {
            let node = Maker::new("@@$").literal();
            let mut c = Chunk::new("filled_float_table");
            node.compile(&mut c).unwrap();
            assert!(c.add_to_bigint_pool(Rc::new(BigInt::from(882))).is_err());
        }

        #[test]
        fn mystery_debug() {
            let node = Maker::new("@@z").literal();
            let mut c = Chunk::new("mystery_debug");
            node.compile(&mut c).unwrap();
            assert!(c.opcodes.is_empty());
        }
    }
}

mod parenthesized_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("(id)", true => svec(&["STRING 0 (id)", "STRICT_RESOLVE"]); "strict")]
    #[test_case("(id)", false => svec(&["STRING 0 (id)", "RESOLVE"]); "non-strict")]
    fn compile(src: &str, strict: bool) -> Vec<String> {
        let node = Maker::new(src).parenthesized_expression();
        let mut c = Chunk::new("x");
        node.compile(&mut c, strict).unwrap();
        c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
    }
}

mod object_literal {
    use super::*;
    use test_case::test_case;

    #[test_case("{}", true => svec(&["OBJECT"]); "empty")]
    #[test_case("{a}", true => svec(&["OBJECT", "STRING 0 (a)", "STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "CR_PROP"]); "strict elements")]
    #[test_case("{a,}", false => svec(&["OBJECT", "STRING 0 (a)", "STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "CR_PROP"]); "non-strict elements")]
    fn compile(src: &str, strict: bool) -> Vec<String> {
        let node = Maker::new(src).object_literal();
        let mut c = Chunk::new("x");
        node.compile(&mut c, strict).unwrap();
        c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
    }
}

mod property_definition_list {
    use super::*;
    use test_case::test_case;

    #[test_case("a", true => Ok((svec(&[
        "STRING 0 (a)", "STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "CR_PROP"
    ]), true, false)); "one item, strict")]
    #[test_case("a", false => Ok((svec(&[
        "STRING 0 (a)", "STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "CR_PROP"
    ]), true, false)); "one item, non-strict")]
    #[test_case("b:1", false => Ok((svec(&[
        "STRING 0 (b)", "FLOAT 0 (1)", "CR_PROP"
    ]), false, false)); "one item, errorfree")]
    #[test_case("a,b", true => Ok((svec(&[
        "STRING 0 (a)", "STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "CR_PROP", "JUMP_IF_ABRUPT 13", "STRING 1 (b)", "STRING 1 (b)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "CR_PROP"
    ]), true, false)); "list, strict")]
    #[test_case("a,b", false => Ok((svec(&[
        "STRING 0 (a)", "STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "CR_PROP", "JUMP_IF_ABRUPT 13", "STRING 1 (b)", "STRING 1 (b)", "RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "CR_PROP"
    ]), true, false)); "list, non-strict")]
    #[test_case("a:1,b", false => Ok((svec(&[
        "STRING 0 (a)", "FLOAT 0 (1)", "CR_PROP", "STRING 1 (b)", "STRING 1 (b)", "RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "CR_PROP"
    ]), true, false)); "potential errors in first")]
    #[test_case("a,b:1", false => Ok((svec(&[
        "STRING 0 (a)", "STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "CR_PROP", "JUMP_IF_ABRUPT 5", "STRING 1 (b)", "FLOAT 0 (1)", "CR_PROP"
    ]), true, false)); "potential errors in second")]
    #[test_case("a:0,b:1", false => Ok((svec(&[
        "STRING 0 (a)", "FLOAT 0 (0)", "CR_PROP", "STRING 1 (b)", "FLOAT 1 (1)", "CR_PROP"
    ]), false, false)); "error free list")]
    #[test_case("a,b:@@@", false => serr("out of range integral type conversion attempted"); "jump fails")]
    #[test_case("[@@!]:0,a,b", false => serr("Out of room for strings in this compilation unit"); "filled string table")]
    fn compile(src: &str, strict: bool) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).property_definition_list();
        let mut c = Chunk::new("x");
        node.property_definition_evaluation(&mut c, strict)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.can_be_abrupt,
                    status.can_be_reference,
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod property_definition {
    use super::*;
    use test_case::test_case;

    #[test_case("a", true, None => Ok((svec(&[
        "STRING 0 (a)", "STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "CR_PROP"
    ]), true, false)); "id ref, strict")]
    #[test_case("a", false, None => Ok((svec(&[
        "STRING 0 (a)", "STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "CR_PROP"
    ]), true, false)); "id ref, non-strict")]
    #[test_case("a", false, Some(0) => serr("Out of room for strings in this compilation unit"); "first string has no room")]
    #[test_case("a=1", false, None => panics "unreachable"; "cover initialized name")]
    #[test_case("a:1", true, None => Ok((svec(&[
        "STRING 0 (a)", "FLOAT 0 (1)", "CR_PROP"
    ]), false, false)); "name:property; no possibility of error, strict")]
    #[test_case("[a]:1", true, None => Ok((svec(&[
        "STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 1", "TO_KEY", "JUMP_IF_NORMAL 4", "UNWIND 1", "JUMP 3", "FLOAT 0 (1)", "CR_PROP"
    ]), true, false)); "name:property; potential error in name, strict")]
    #[test_case("[a]:1", false, None => Ok((svec(&[
        "STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 1", "TO_KEY", "JUMP_IF_NORMAL 4", "UNWIND 1", "JUMP 3", "FLOAT 0 (1)", "CR_PROP"
    ]), true, false)); "name:property; potential error in name, non-strict")]
    #[test_case("[q]:33", false, Some(0) => serr("Out of room for strings in this compilation unit"); "pn compile errors out")]
    #[test_case("a: function () {}", true, None => panics "not yet implemented"; "anonymous function def")]
    #[test_case("a:b", false, Some(1) => serr("Out of room for strings in this compilation unit"); "ae compile errors out")]
    #[test_case("a:b", false, None => Ok((svec(&[
        "STRING 0 (a)", "STRING 1 (b)", "RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "CR_PROP"
    ]), true, false)); "name:value, ae can error; not-strict")]
    #[test_case("a:b", true, None => Ok((svec(&[
        "STRING 0 (a)", "STRING 1 (b)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "CR_PROP"
    ]), true, false)); "name:value, ae can error; strict")]
    #[test_case("[a]:@@@", true, None => serr("out of range integral type conversion attempted"); "jump too far")]
    #[test_case("__proto__:null", true, None => Ok((svec(&["NULL", "SET_PROTO"]), false, false)); "proto-setter")]
    #[test_case("a(){}", true, None => panics "not yet implemented"; "method def")]
    #[test_case("...a", true, None => Ok((svec(&[
        "STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 1", "JUMP 1", "COPY_DATA_PROPS"
    ]), true, false)); "rest object, strict")]
    #[test_case("...a", false, None => Ok((svec(&[
        "STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 1", "JUMP 1", "COPY_DATA_PROPS"
    ]), true, false)); "rest object, non-strict")]
    #[test_case("...a", false, Some(0) => serr("Out of room for strings in this compilation unit"); "rest object, ae errs")]
    #[test_case("...true", false, None => Ok((svec(&["TRUE", "COPY_DATA_PROPS"]), true, false)); "rest object, not reference")]
    fn compile(
        src: &str,
        strict: bool,
        string_spots_avail: Option<usize>,
    ) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).property_definition();
        let mut c = if let Some(spot_count) = string_spots_avail {
            almost_full_chunk("x", spot_count)
        } else {
            Chunk::new("x")
        };
        node.property_definition_evaluation(&mut c, strict)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.can_be_abrupt,
                    status.can_be_reference,
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod property_name {
    use super::*;
    use test_case::test_case;

    #[test_case("a", true, None => Ok((svec(&["STRING 0 (a)"]), false, false)); "literal property name")]
    #[test_case("[a]", true, None => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 1", "TO_KEY"]), true, false)); "computed property name; strict")]
    #[test_case("[a]", false, None => Ok((svec(&["STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 1", "TO_KEY"]), true, false)); "computed property name; non-strict")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).property_name();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.can_be_abrupt,
                    status.can_be_reference,
                )
            })
            .map_err(|e| e.to_string())
    }

    #[test_case("[a]" => false; "computed")]
    #[test_case("__proto__" => true; "literal, match")]
    #[test_case("a" => false; "literal, no match")]
    fn is_literal_proto(src: &str) -> bool {
        Maker::new(src).property_name().is_literal_proto()
    }
}

mod literal_property_name {
    use super::*;
    use test_case::test_case;

    #[test_case("__proto__" => true; "id proto")]
    #[test_case("a" => false; "id not")]
    #[test_case("'__proto__'" => true; "string proto")]
    #[test_case("'a'" => false; "string not")]
    #[test_case("1" => false; "number")]
    fn is_literal_proto(src: &str) -> bool {
        Maker::new(src).literal_property_name().is_literal_proto()
    }

    #[test_case("a", None => Ok((svec(&["STRING 0 (a)"]), false, false)); "id")]
    #[test_case("'a'", None => Ok((svec(&["STRING 0 (a)"]), false, false)); "string")]
    #[test_case("1", None => Ok((svec(&["STRING 0 (1)"]), false, false)); "number")]
    #[test_case("a", Some(0) => serr("Out of room for strings in this compilation unit"); "id; err")]
    #[test_case("'a'", Some(0) => serr("Out of room for strings in this compilation unit"); "string; err")]
    #[test_case("1", Some(0) => serr("Out of room for strings in this compilation unit"); "number; err")]
    fn compile(src: &str, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).literal_property_name();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.can_be_abrupt,
                    status.can_be_reference,
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod computed_property_name {
    use super::*;
    use test_case::test_case;

    #[test_case("[a]", true, None => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 1", "TO_KEY"]), true, false)); "reference expression; strict")]
    #[test_case("[a]", false, None => Ok((svec(&["STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 1", "TO_KEY"]), true, false)); "reference expression; non-strict")]
    #[test_case("[a]", true, Some(0) => serr("Out of room for strings in this compilation unit"); "err in expr")]
    #[test_case("[1]", true, None => Ok((svec(&["FLOAT 0 (1)", "TO_KEY"]), true, false)); "error-free expr")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).computed_property_name();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.can_be_abrupt,
                    status.can_be_reference,
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod member_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true, None => Ok((svec(&["STRING 0 (id)", "STRICT_RESOLVE"]), true, true)); "fall-thru strict")]
    #[test_case("id", false, None => Ok((svec(&["STRING 0 (id)", "RESOLVE"]), true, true)); "fall-thru non strict")]
    #[test_case("a.b", true, None => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "STRING 1 (b)",
        "STRICT_REF"
    ]), true, true)); "member property; strict")]
    #[test_case("a.b", false, None => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "STRING 1 (b)",
        "REF"
    ]), true, true)); "member property; non-strict")]
    #[test_case("a.b", true, Some(1) => serr("Out of room for strings in this compilation unit"); "no space for id")]
    #[test_case("a.b", true, Some(0) => serr("Out of room for strings in this compilation unit"); "no space for base")]
    #[test_case("(1).b", true, None => Ok((svec(&["FLOAT 0 (1)", "STRING 0 (b)", "STRICT_REF"]), false, true)); "error-free base")]
    #[test_case("a[b]", true, None => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 18",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 8",
        "TO_KEY",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 1",
        "STRICT_REF"
    ]), true, true)); "strict member exp")]
    #[test_case("a[b]", false, None => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 18",
        "STRING 1 (b)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 8",
        "TO_KEY",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 1",
        "REF"
    ]), true, true)); "non-strict member exp")]
    #[test_case("a[b]", true, Some(0) => serr("Out of room for strings in this compilation unit"); "no space for base (expression)")]
    #[test_case("(1)[1]", true, None => Ok((svec(&["FLOAT 0 (1)", "FLOAT 0 (1)", "TO_KEY", "JUMP_IF_NORMAL 4", "UNWIND 1", "JUMP 1", "STRICT_REF"]), true, true)); "no errors in base (expression)")]
    #[test_case("b[a]", true, Some(1) => serr("Out of room for strings in this compilation unit"); "no space for expression (expression)")]
    #[test_case("a[@@@]", true, None => serr("out of range integral type conversion attempted"); "bad jump (expression)")]
    #[test_case("a`${b}`", true, None => panics "not yet implemented"; "template")]
    #[test_case("super.a", true, None => panics "not yet implemented"; "super ref")]
    #[test_case("new.target", true, None => panics "not yet implemented"; "meta")]
    #[test_case("new a()", true, None => panics "not yet implemented"; "new-args")]
    #[test_case("a.#pid", true, None => panics "not yet implemented"; "private")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).member_expression();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.can_be_abrupt,
                    status.can_be_reference,
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod new_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true => svec(&["STRING 0 (id)", "STRICT_RESOLVE"]); "fall-thru strict")]
    #[test_case("id", false => svec(&["STRING 0 (id)", "RESOLVE"]); "fall-thru non strict")]
    #[test_case("new a", true => panics "not yet implemented"; "new exp")]
    fn compile(src: &str, strict: bool) -> Vec<String> {
        let node = Maker::new(src).new_expression();
        let mut c = Chunk::new("x");
        node.compile(&mut c, strict).unwrap();
        c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
    }
}

mod call_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("a()", true, None => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "DUP", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 1", "JUMP 3", "FLOAT 0 (0)", "CALL"]), true, false)); "call-expression; strict")]
    #[test_case("a()", false, None => Ok((svec(&["STRING 0 (a)", "RESOLVE", "DUP", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 1", "JUMP 3", "FLOAT 0 (0)", "CALL"]), true, false)); "call-expression; non-strict")]
    #[test_case("super()", true, None => panics "not yet implemented"; "super call")]
    #[test_case("import(a)", true, None => panics "not yet implemented"; "import call")]
    #[test_case("a()()", true, None => panics "not yet implemented"; "call-on-call")]
    #[test_case("a()[b]", true, None => panics "not yet implemented"; "expr-on-call")]
    #[test_case("a().b", true, None => panics "not yet implemented"; "property-on-call")]
    #[test_case("a()`${b}`", true, None => panics "not yet implemented"; "template-on-call")]
    #[test_case("a().#pid", true, None => panics "not yet implemented"; "private-on-call")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).call_expression();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.can_be_abrupt,
                    status.can_be_reference,
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod call_member_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("a()", true, None => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "DUP", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 1", "JUMP 3", "FLOAT 0 (0)", "CALL"]), true, false)); "no args; strict")]
    #[test_case("a()", false, None => Ok((svec(&["STRING 0 (a)", "RESOLVE", "DUP", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 1", "JUMP 3", "FLOAT 0 (0)", "CALL"]), true, false)); "no args; non-strict")]
    #[test_case("a()", true, Some(0) => serr("Out of room for strings in this compilation unit"); "no space for ME")]
    #[test_case("(1)()", true, None => Ok((svec(&["FLOAT 0 (1)", "DUP", "FLOAT 1 (0)", "CALL"]), true, false)); "me not reference")]
    #[test_case("a(b)", true, Some(1) => serr("Out of room for strings in this compilation unit"); "no space for args")]
    #[test_case("a(c)", true, None => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "DUP", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 1", "JUMP 15", "STRING 1 (c)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 2", "FLOAT 0 (1)", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "CALL"]), true, false)); "args can error; strict")]
    #[test_case("a(c)", false, None => Ok((svec(&["STRING 0 (a)", "RESOLVE", "DUP", "GET_VALUE", "JUMP_IF_NORMAL 4", "UNWIND 1", "JUMP 15", "STRING 1 (c)", "RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 2", "FLOAT 0 (1)", "JUMP_IF_NORMAL 4", "UNWIND 2", "JUMP 1", "CALL"]), true, false)); "args can error; non-strict")]
    #[test_case("a(@@@)", true, None => serr("out of range integral type conversion attempted"); "bad jump (args too complex)")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).call_member_expression();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.can_be_abrupt,
                    status.can_be_reference,
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod left_hand_side_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true => svec(&["STRING 0 (id)", "STRICT_RESOLVE"]); "fall-thru strict")]
    #[test_case("id", false => svec(&["STRING 0 (id)", "RESOLVE"]); "fall-thru non strict")]
    #[test_case("a()", true => svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "DUP",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 3",
        "FLOAT 0 (0)",
        "CALL"
    ]); "call strict")]
    #[test_case("a()", false => svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "DUP",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "UNWIND 1",
        "JUMP 3",
        "FLOAT 0 (0)",
        "CALL"
    ]); "call non-strict")]
    #[test_case("a?.b", true => panics "not yet implemented"; "optional")]
    fn compile(src: &str, strict: bool) -> Vec<String> {
        let node = Maker::new(src).left_hand_side_expression();
        let mut c = Chunk::new("x");
        node.compile(&mut c, strict).unwrap();
        c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
    }
}

mod arguments {
    use super::*;
    use test_case::test_case;

    #[test_case("()", true, None => Ok((svec(&["FLOAT 0 (0)"]), false, false)); "empty")]
    #[test_case("()", true, Some(0) => serr("Out of room for floats in this compilation unit"); "empty; no space for args")]
    #[test_case("(1)", true, None => Ok((svec(&["FLOAT 0 (1)", "FLOAT 0 (1)"]), false, false)); "one arg; no errors")]
    #[test_case("(a)", true, Some(0) => serr("Out of room for strings in this compilation unit"); "no room for args")]
    #[test_case("(999)", true, Some(1) => serr("Out of room for floats in this compilation unit"); "no room for length")]
    #[test_case("(a,)", true, None => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 2", "FLOAT 0 (1)"]), true, false)); "error-able args; strict")]
    #[test_case("(a,)", false, None => Ok((svec(&["STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 2", "FLOAT 0 (1)"]), true, false)); "error-able args; non-strict")]
    fn argument_list_evaluation(
        src: &str,
        strict: bool,
        spots_avail: Option<usize>,
    ) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).arguments();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.argument_list_evaluation(&mut c, strict)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.can_be_abrupt,
                    status.can_be_reference,
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod argument_list {
    use super::*;
    use test_case::test_case;

    #[test_case("a", true, None => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE"]), 1, true, false)); "item/reference/strict")]
    #[test_case("a", false, None => Ok((svec(&["STRING 0 (a)", "RESOLVE", "GET_VALUE"]), 1, true, false)); "item/reference/non-strict")]
    #[test_case("true", true, None => Ok((svec(&["TRUE"]), 1, false, false)); "item/literal")]
    #[test_case("a", true, Some(0) => serr("Out of room for strings in this compilation unit"); "no room for item")]
    #[test_case("...a", true, None => panics "not yet implemented"; "...a -style object unpacking")]
    #[test_case("true, false", true, None => Ok((svec(&["TRUE", "FALSE"]), 2, false, false)); "list/noref")]
    #[test_case("a,b", true, None => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 8", "STRING 1 (b)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 2", "UNWIND 1"]), 2, true, false)); "errable items, strict")]
    #[test_case("a,b", false, None => Ok((svec(&["STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 8", "STRING 1 (b)", "RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 2", "UNWIND 1"]), 2, true, false)); "errable items, non-strict")]
    #[test_case("a,b", true, Some(0) => serr("Out of room for strings in this compilation unit"); "no room for first list")]
    #[test_case("a,b", true, Some(1) => serr("Out of room for strings in this compilation unit"); "no room for last item")]
    #[test_case("a,@@@", true, None => serr("out of range integral type conversion attempted"); "jump too far")]
    #[test_case("a,...b", true, None => panics "not yet implemented"; "list + rest")]
    fn argument_list_evaluation(
        src: &str,
        strict: bool,
        spots_avail: Option<usize>,
    ) -> Result<(Vec<String>, u16, bool, bool), String> {
        let node = Maker::new(src).argument_list();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.argument_list_evaluation(&mut c, strict)
            .map(|(count, status)| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
                    count,
                    status.can_be_abrupt,
                    status.can_be_reference,
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod update_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true, None => Ok((svec(&["STRING 0 (id)", "STRICT_RESOLVE"]), true, true)); "fall-thru strict")]
    #[test_case("id", false, None => Ok((svec(&["STRING 0 (id)", "RESOLVE"]), true, true)); "fall-thru non strict")]
    #[test_case("a++", true, None => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "DUP",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "SWAP",
        "POP",
        "JUMP 11",
        "TO_NUMERIC",
        "JUMP_IF_NORMAL 4",
        "SWAP",
        "POP",
        "JUMP 4",
        "POP2_PUSH3",
        "INCREMENT",
        "PUT_VALUE",
        "UPDATE_EMPTY",
    ]), true, false)); "post-increment, strict")]
    #[test_case("a++", false, None => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "DUP",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "SWAP",
        "POP",
        "JUMP 11",
        "TO_NUMERIC",
        "JUMP_IF_NORMAL 4",
        "SWAP",
        "POP",
        "JUMP 4",
        "POP2_PUSH3",
        "INCREMENT",
        "PUT_VALUE",
        "UPDATE_EMPTY",
    ]), true, false)); "post-increment, non-strict")]
    #[test_case("a--", true, None => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "DUP",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "SWAP",
        "POP",
        "JUMP 11",
        "TO_NUMERIC",
        "JUMP_IF_NORMAL 4",
        "SWAP",
        "POP",
        "JUMP 4",
        "POP2_PUSH3",
        "DECREMENT",
        "PUT_VALUE",
        "UPDATE_EMPTY",
    ]), true, false)); "post-decrement, strict")]
    #[test_case("a--", false, None => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "DUP",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "SWAP",
        "POP",
        "JUMP 11",
        "TO_NUMERIC",
        "JUMP_IF_NORMAL 4",
        "SWAP",
        "POP",
        "JUMP 4",
        "POP2_PUSH3",
        "DECREMENT",
        "PUT_VALUE",
        "UPDATE_EMPTY",
    ]), true, false)); "post-decrement, non-strict")]
    #[test_case("++a", true, None => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "PRE_INCREMENT"]), true, false)); "pre-increment, strict")]
    #[test_case("++a", false, None => Ok((svec(&["STRING 0 (a)", "RESOLVE", "PRE_INCREMENT"]), true, false)); "pre-increment, non-strict")]
    #[test_case("--a", true, None => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "PRE_DECREMENT"]), true, false)); "pre-decrement, strict")]
    #[test_case("--a", false, None => Ok((svec(&["STRING 0 (a)", "RESOLVE", "PRE_DECREMENT"]), true, false)); "pre-decrement, non-strict")]
    #[test_case("++a", true, Some(0) => serr("Out of room for strings in this compilation unit"); "pre-op, err in subexpr")]
    #[test_case("a++", true, Some(0) => serr("Out of room for strings in this compilation unit"); "post-op, err in subexpr")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).update_expression();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.can_be_abrupt,
                    status.can_be_reference,
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod unary_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true, None => Ok((svec(&["STRING 0 (id)", "STRICT_RESOLVE"]), true, true)); "fall-thru strict")]
    #[test_case("id", false, None => Ok((svec(&["STRING 0 (id)", "RESOLVE"]), true, true)); "fall-thru non strict")]
    #[test_case("-a", true, None => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "UNARY_MINUS"]), true, false)); "negate; strict")]
    #[test_case("-a", false, None => Ok((svec(&["STRING 0 (a)", "RESOLVE", "UNARY_MINUS"]), true, false)); "negate; non-strict")]
    #[test_case("-a", false, Some(0) => serr("Out of room for strings in this compilation unit"); "negate; compile fail")]
    #[test_case("+a", true, None => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "UNARY_PLUS"]), true, false)); "tonumber; strict")]
    #[test_case("+a", false, None => Ok((svec(&["STRING 0 (a)", "RESOLVE", "UNARY_PLUS"]), true, false)); "tonumber; non-strict")]
    #[test_case("+a", false, Some(0) => serr("Out of room for strings in this compilation unit"); "tonumber; compile fail")]
    #[test_case("!a", true, None => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "UNARY_NOT"]), true, false)); "not; strict")]
    #[test_case("!a", false, None => Ok((svec(&["STRING 0 (a)", "RESOLVE", "UNARY_NOT"]), true, false)); "not; non-strict")]
    #[test_case("!a", false, Some(0) => serr("Out of room for strings in this compilation unit"); "not; compile fail")]
    #[test_case("~a", true, None => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "UNARY_COMPLEMENT"]), true, false)); "complement; strict")]
    #[test_case("~a", false, None => Ok((svec(&["STRING 0 (a)", "RESOLVE", "UNARY_COMPLEMENT"]), true, false)); "complement; non-strict")]
    #[test_case("~a", false, Some(0) => serr("Out of room for strings in this compilation unit"); "complement; compile fail")]
    #[test_case("void a", true, None => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "VOID"]), true, false)); "void; strict")]
    #[test_case("void a", false, None => Ok((svec(&["STRING 0 (a)", "RESOLVE", "VOID"]), true, false)); "void; non-strict")]
    #[test_case("void a", false, Some(0) => serr("Out of room for strings in this compilation unit"); "void; compile fail")]
    #[test_case("typeof a", true, None => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "TYPEOF"]), true, false)); "typeof; strict")]
    #[test_case("typeof a", false, None => Ok((svec(&["STRING 0 (a)", "RESOLVE", "TYPEOF"]), true, false)); "typeof; non-strict")]
    #[test_case("typeof a", false, Some(0) => serr("Out of room for strings in this compilation unit"); "typeof; compile fail")]
    #[test_case("delete a", true, None => Ok((svec(&["STRING 0 (a)", "STRICT_RESOLVE", "DELETE"]), true, false)); "delete; strict")]
    #[test_case("delete a", false, None => Ok((svec(&["STRING 0 (a)", "RESOLVE", "DELETE"]), true, false)); "delete; non-strict")]
    #[test_case("delete a", false, Some(0) => serr("Out of room for strings in this compilation unit"); "delete; compile fail")]
    #[test_case("await a", true, None => panics "not yet implemented"; "await; strict")]
    #[test_case("await a", false, None => panics "not yet implemented"; "await; non-strict")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).unary_expression();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.can_be_abrupt,
                    status.can_be_reference,
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod exponentiation_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true => svec(&["STRING 0 (id)", "STRICT_RESOLVE"]); "fall-thru strict")]
    #[test_case("id", false => svec(&["STRING 0 (id)", "RESOLVE"]); "fall-thru non strict")]
    #[test_case("a**8", true => svec(&["STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 3", "FLOAT 0 (8)", "EXPONENTIATE"]); "exponent expr; non-strict")]
    fn compile(src: &str, strict: bool) -> Vec<String> {
        let node = Maker::new(src).exponentiation_expression();
        let mut c = Chunk::new("x");
        node.compile(&mut c, strict).unwrap();
        c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
    }
}

mod multiplicative_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true => svec(&["STRING 0 (id)", "STRICT_RESOLVE"]); "fall-thru strict")]
    #[test_case("id", false => svec(&["STRING 0 (id)", "RESOLVE"]); "fall-thru non strict")]
    #[test_case("a*8", true => svec(&["STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 3", "FLOAT 0 (8)", "MULTIPLY"]); "multiply expr; strict")]
    #[test_case("a/8", true => svec(&["STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 3", "FLOAT 0 (8)", "DIVIDE"]); "divide expr; strict")]
    #[test_case("a%8", true => svec(&["STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 3", "FLOAT 0 (8)", "MODULO"]); "modulo expr; strict")]
    fn compile(src: &str, strict: bool) -> Vec<String> {
        let node = Maker::new(src).multiplicative_expression();
        let mut c = Chunk::new("x");
        node.compile(&mut c, strict).unwrap();
        c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
    }
}

mod additive_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true => svec(&["STRING 0 (id)", "STRICT_RESOLVE"]); "fall-thru strict")]
    #[test_case("id", false => svec(&["STRING 0 (id)", "RESOLVE"]); "fall-thru non strict")]
    #[test_case("a+3", true => svec(&["STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 3", "FLOAT 0 (3)", "ADD"]); "add expr")]
    #[test_case("a-3", true => svec(&["STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_ABRUPT 3", "FLOAT 0 (3)", "SUBTRACT"]); "sub expr")]
    fn compile(src: &str, strict: bool) -> Vec<String> {
        let node = Maker::new(src).additive_expression();
        let mut c = Chunk::new("x");
        node.compile(&mut c, strict).unwrap();
        c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
    }
}

mod shift_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true => svec(&["STRING 0 (id)", "STRICT_RESOLVE"]); "fall-thru strict")]
    #[test_case("id", false => svec(&["STRING 0 (id)", "RESOLVE"]); "fall-thru non strict")]
    #[test_case("a<<2", true => panics "not yet implemented"; "shift expr")]
    fn compile(src: &str, strict: bool) -> Vec<String> {
        let node = Maker::new(src).shift_expression();
        let mut c = Chunk::new("x");
        node.compile(&mut c, strict).unwrap();
        c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
    }
}

mod relational_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true => svec(&["STRING 0 (id)", "STRICT_RESOLVE"]); "fall-thru strict")]
    #[test_case("id", false => svec(&["STRING 0 (id)", "RESOLVE"]); "fall-thru non strict")]
    #[test_case("a<b", true => panics "not yet implemented"; "relational expr")]
    fn compile(src: &str, strict: bool) -> Vec<String> {
        let node = Maker::new(src).relational_expression();
        let mut c = Chunk::new("x");
        node.compile(&mut c, strict).unwrap();
        c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
    }
}

mod equality_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true => svec(&["STRING 0 (id)", "STRICT_RESOLVE"]); "fall-thru strict")]
    #[test_case("id", false => svec(&["STRING 0 (id)", "RESOLVE"]); "fall-thru non strict")]
    #[test_case("a==43", true => panics "not yet implemented"; "equality expr")]
    fn compile(src: &str, strict: bool) -> Vec<String> {
        let node = Maker::new(src).equality_expression();
        let mut c = Chunk::new("x");
        node.compile(&mut c, strict).unwrap();
        c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
    }
}

mod bitwise_and_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true => svec(&["STRING 0 (id)", "STRICT_RESOLVE"]); "fall-thru strict")]
    #[test_case("id", false => svec(&["STRING 0 (id)", "RESOLVE"]); "fall-thru non strict")]
    #[test_case("a&8", true => panics "not yet implemented"; "bit and expr")]
    fn compile(src: &str, strict: bool) -> Vec<String> {
        let node = Maker::new(src).bitwise_and_expression();
        let mut c = Chunk::new("x");
        node.compile(&mut c, strict).unwrap();
        c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
    }
}

mod bitwise_xor_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true => svec(&["STRING 0 (id)", "STRICT_RESOLVE"]); "fall-thru strict")]
    #[test_case("id", false => svec(&["STRING 0 (id)", "RESOLVE"]); "fall-thru non strict")]
    #[test_case("a^6", true => panics "not yet implemented"; "bit xor expr")]
    fn compile(src: &str, strict: bool) -> Vec<String> {
        let node = Maker::new(src).bitwise_xor_expression();
        let mut c = Chunk::new("x");
        node.compile(&mut c, strict).unwrap();
        c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
    }
}

mod bitwise_or_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true => svec(&["STRING 0 (id)", "STRICT_RESOLVE"]); "fall-thru strict")]
    #[test_case("id", false => svec(&["STRING 0 (id)", "RESOLVE"]); "fall-thru non strict")]
    #[test_case("a|8", true => panics "not yet implemented"; "bit or expr")]
    fn compile(src: &str, strict: bool) -> Vec<String> {
        let node = Maker::new(src).bitwise_or_expression();
        let mut c = Chunk::new("x");
        node.compile(&mut c, strict).unwrap();
        c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
    }
}

mod logical_and_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true => svec(&["STRING 0 (id)", "STRICT_RESOLVE"]); "fall-thru strict")]
    #[test_case("id", false => svec(&["STRING 0 (id)", "RESOLVE"]); "fall-thru non strict")]
    #[test_case("a&&b", true => panics "not yet implemented"; "land expr")]
    fn compile(src: &str, strict: bool) -> Vec<String> {
        let node = Maker::new(src).logical_and_expression();
        let mut c = Chunk::new("x");
        node.compile(&mut c, strict).unwrap();
        c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
    }
}

mod logical_or_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true => svec(&["STRING 0 (id)", "STRICT_RESOLVE"]); "fall-thru strict")]
    #[test_case("id", false => svec(&["STRING 0 (id)", "RESOLVE"]); "fall-thru non strict")]
    #[test_case("a||m", true => panics "not yet implemented"; "lor expr")]
    fn compile(src: &str, strict: bool) -> Vec<String> {
        let node = Maker::new(src).logical_or_expression();
        let mut c = Chunk::new("x");
        node.compile(&mut c, strict).unwrap();
        c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
    }
}

mod short_circuit_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true => svec(&["STRING 0 (id)", "STRICT_RESOLVE"]); "fall-thru strict")]
    #[test_case("id", false => svec(&["STRING 0 (id)", "RESOLVE"]); "fall-thru non strict")]
    #[test_case("a??7", true => panics "not yet implemented"; "coalesce expr")]
    fn compile(src: &str, strict: bool) -> Vec<String> {
        let node = Maker::new(src).short_circuit_expression();
        let mut c = Chunk::new("x");
        node.compile(&mut c, strict).unwrap();
        c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
    }
}

mod conditional_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true => svec(&["STRING 0 (id)", "STRICT_RESOLVE"]); "fall-thru strict")]
    #[test_case("id", false => svec(&["STRING 0 (id)", "RESOLVE"]); "fall-thru non strict")]
    #[test_case("a?1:0", true => panics "not yet implemented"; "conditional expr")]
    fn compile(src: &str, strict: bool) -> Vec<String> {
        let node = Maker::new(src).conditional_expression();
        let mut c = Chunk::new("x");
        node.compile(&mut c, strict).unwrap();
        c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
    }
}

mod assignment_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true, None => Ok((svec(&["STRING 0 (id)", "STRICT_RESOLVE"]), true, true)); "fall-thru strict")]
    #[test_case("id", false, None => Ok((svec(&["STRING 0 (id)", "RESOLVE"]), true, true)); "fall-thru non strict")]
    #[test_case("a=6", true, None => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 5",
        "FLOAT 0 (6)",
        "POP2_PUSH3",
        "PUT_VALUE",
        "UPDATE_EMPTY"
    ]), true, false)); "strict assignment expr")]
    #[test_case("a=6", false, None => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 5",
        "FLOAT 0 (6)",
        "POP2_PUSH3",
        "PUT_VALUE",
        "UPDATE_EMPTY"
    ]), true, false)); "non-strict assignment expr")]
    #[test_case("a=1", true, Some(0) => serr("Out of room for strings in this compilation unit"); "lhse errs")]
    #[test_case("a=function(){}", true, None => panics "not yet implemented"; "anonymous func")]
    #[test_case("a=b", true, Some(1) => serr("Out of room for strings in this compilation unit"); "ae errs")]
    #[test_case("a=b", true, None => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 13",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "SWAP",
        "POP",
        "JUMP 3",
        "POP2_PUSH3",
        "PUT_VALUE",
        "UPDATE_EMPTY"
    ]), true, false)); "ae is reference; strict")]
    #[test_case("a=b", false, None => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 13",
        "STRING 1 (b)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "SWAP",
        "POP",
        "JUMP 3",
        "POP2_PUSH3",
        "PUT_VALUE",
        "UPDATE_EMPTY"
    ]), true, false)); "ae is reference; non-strict")]
    #[test_case("a+=3", true, None => panics "not yet implemented"; "mutating assignment")]
    #[test_case("a=@@@", true, None => serr("out of range integral type conversion attempted"); "ae is too big")]
    #[test_case("1=0", true, None => Ok((svec(&[
        "FLOAT 0 (1)",
        "FLOAT 1 (0)",
        "POP2_PUSH3",
        "PUT_VALUE",
        "UPDATE_EMPTY"
    ]), true, false)); "lhse not abrupt")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).assignment_expression();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.can_be_abrupt,
                    status.can_be_reference,
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true => svec(&["STRING 0 (id)", "STRICT_RESOLVE"]); "fall-thru strict")]
    #[test_case("id", false => svec(&["STRING 0 (id)", "RESOLVE"]); "fall-thru non strict")]
    #[test_case("a,9", true => panics "not yet implemented"; "comma expr")]
    fn compile(src: &str, strict: bool) -> Vec<String> {
        let node = Maker::new(src).expression();
        let mut c = Chunk::new("x");
        node.compile(&mut c, strict).unwrap();
        c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
    }
}

mod expression_statement {
    use super::*;

    mod compile {
        use super::*;
        use test_case::test_case;

        #[test_case("id;", true => svec(&["STRING 0 (id)", "STRICT_RESOLVE", "GET_VALUE"]); "strict")]
        #[test_case("id;", false => svec(&["STRING 0 (id)", "RESOLVE", "GET_VALUE"]); "non strict")]
        fn normal(src: &str, strict: bool) -> Vec<String> {
            let node = Maker::new(src).expression_statement();
            let mut c = Chunk::new("x");
            node.compile(&mut c, strict).unwrap();
            c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
        }

        #[test_case("a;" => "Out of room for strings in this compilation unit"; "no room")]
        fn error(src: &str) -> String {
            let node = Maker::new(src).expression_statement();
            let mut c = full_chunk("x");
            node.compile(&mut c, true).unwrap_err().to_string()
        }
    }
}

mod statement_list {
    use super::*;
    use test_case::test_case;

    #[test_case("id;", true, None => Ok((svec(&["STRING 0 (id)", "STRICT_RESOLVE", "GET_VALUE"]), true, false)); "id-strict")]
    #[test_case("id;", false, None => Ok((svec(&["STRING 0 (id)", "RESOLVE", "GET_VALUE"]), true, false)); "id-non-strict")]
    #[test_case("a; b;", true, None => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 5",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "UPDATE_EMPTY"
    ]), true, false)); "strict list")]
    #[test_case("a; b;", false, None => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 5",
        "STRING 1 (b)",
        "RESOLVE",
        "GET_VALUE",
        "UPDATE_EMPTY"
    ]), true, false)); "non-strict list")]
    #[test_case("a;3;", true, Some(0) => serr("Out of room for strings in this compilation unit"); "err in list")]
    #[test_case("true;b;", true, Some(0) => serr("Out of room for strings in this compilation unit"); "err in item")]
    #[test_case("a;@@@;", true, None => serr("out of range integral type conversion attempted"); "item is too big")]
    #[test_case("1;b;", true, None => Ok((svec(&[
        "FLOAT 0 (1)",
        "STRING 0 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "UPDATE_EMPTY"
    ]), true, false)); "list can't return abruptly")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).statement_list();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.can_be_abrupt,
                    status.can_be_reference,
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod statement_list_item {
    use super::*;
    use test_case::test_case;

    #[test_case("a;", true => svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
    ]); "strict stmt")]
    #[test_case("a;", false => svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
    ]); "non-strict stmt")]
    #[test_case("let a;", true => svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "UNDEFINED",
        "IRB",
        "JUMP_IF_ABRUPT 2",
        "POP",
        "EMPTY",
    ]); "strict decl")]
    #[test_case("let a;", false => svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "UNDEFINED",
        "IRB",
        "JUMP_IF_ABRUPT 2",
        "POP",
        "EMPTY",
    ]); "non-strict decl")]
    fn compile(src: &str, strict: bool) -> Vec<String> {
        let node = Maker::new(src).statement_list_item();
        let mut c = Chunk::new("x");
        node.compile(&mut c, strict).unwrap();
        c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
    }
}

mod statement {
    use super::*;
    use test_case::test_case;

    #[test_case("a;", true => svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
    ]); "strict stmt")]
    #[test_case("a;", false => svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
    ]); "non-strict stmt")]
    #[test_case("debugger;", true => panics "not yet implemented"; "other")]
    fn compile(src: &str, strict: bool) -> Vec<String> {
        let node = Maker::new(src).statement();
        let mut c = Chunk::new("x");
        node.compile(&mut c, strict).unwrap();
        c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
    }
}

mod declaration {
    use super::*;
    use test_case::test_case;

    #[test_case("class bob{}", true => panics "not yet implemented"; "class decl")]
    #[test_case("function bob(){}", true => panics "not yet implemented"; "hoistable")]
    #[test_case("const a=0;", true => svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "FLOAT 0 (0)",
        "IRB",
        "JUMP_IF_ABRUPT 2",
        "POP",
        "EMPTY",
    ]); "strict lexical")]
    #[test_case("const a=0;", false => svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "FLOAT 0 (0)",
        "IRB",
        "JUMP_IF_ABRUPT 2",
        "POP",
        "EMPTY",
    ]); "non-strict lexical")]
    fn compile(src: &str, strict: bool) -> Vec<String> {
        let node = Maker::new(src).declaration();
        let mut c = Chunk::new("x");
        node.compile(&mut c, strict).unwrap();
        c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
    }
}

mod lexical_declaration {
    use super::*;
    use test_case::test_case;

    #[test_case("let a;", true, None => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "UNDEFINED",
        "IRB",
        "JUMP_IF_ABRUPT 2",
        "POP",
        "EMPTY"
    ]), true, false)); "strict; typical")]
    #[test_case("let a;", false, None => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "UNDEFINED",
        "IRB",
        "JUMP_IF_ABRUPT 2",
        "POP",
        "EMPTY"
    ]), true, false)); "non-strict; typical")]
    #[test_case("let a;", true, Some(0) => serr("Out of room for strings in this compilation unit"); "full string table")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).lexical_declaration();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.can_be_abrupt,
                    status.can_be_reference,
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod binding_list {
    use super::*;
    use test_case::test_case;

    #[test_case("a", true, None =>  Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "UNDEFINED",
        "IRB",
    ]), true, false)); "strict item")]
    #[test_case("a", false, None =>  Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "UNDEFINED",
        "IRB",
    ]), true, false)); "non-strict item")]
    #[test_case("a,b", true, None =>  Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "UNDEFINED",
        "IRB",
        "JUMP_IF_ABRUPT 6",
        "POP",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "UNDEFINED",
        "IRB",
    ]), true, false)); "strict list")]
    #[test_case("a,b", false, None =>  Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "UNDEFINED",
        "IRB",
        "JUMP_IF_ABRUPT 6",
        "POP",
        "STRING 1 (b)",
        "RESOLVE",
        "UNDEFINED",
        "IRB",
    ]), true, false)); "non-strict list")]
    #[test_case("a,b", true, Some(0) => serr("Out of room for strings in this compilation unit"); "no room on list")]
    #[test_case("a,b", true, Some(1) => serr("Out of room for strings in this compilation unit"); "no room on item")]
    #[test_case("a,b=@@@", true, None => serr("out of range integral type conversion attempted"); "branch too far")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).binding_list();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.can_be_abrupt,
                    status.can_be_reference,
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod lexical_binding {
    use super::*;
    use test_case::test_case;

    #[test_case("a", true, None => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "UNDEFINED",
        "IRB",
    ]), true, false)); "strict, no initializer")]
    #[test_case("a", false, None => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "UNDEFINED",
        "IRB",
    ]), true, false)); "non-strict, no initializer")]
    #[test_case("a", true, Some(0) => serr("Out of room for strings in this compilation unit"); "string table full")]
    #[test_case("a=function (){}", true, None => panics "not yet implemented"; "anonymous function")]
    #[test_case("a=b", true, Some(1) => serr("Out of room for strings in this compilation unit"); "string table full in initializer")]
    #[test_case("a=b", true, None => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "SWAP",
        "POP",
        "JUMP 1",
        "IRB"
    ]), true, false)); "resolvable initializer; strict")]
    #[test_case("a=b", false, None => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "STRING 1 (b)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "SWAP",
        "POP",
        "JUMP 1",
        "IRB"
    ]), true, false)); "resolvable initializer; non-strict")]
    #[test_case("a=0", true, None => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "FLOAT 0 (0)",
        "IRB"
    ]), true, false)); "literal initializer; strict")]
    #[test_case("a=0", false, None => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "FLOAT 0 (0)",
        "IRB"
    ]), true, false)); "literal initializer; non-strict")]
    #[test_case("{a}=b", true, None => panics "not yet implemented"; "pattern binding")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).lexical_binding();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.can_be_abrupt,
                    status.can_be_reference,
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod initializer {
    use super::*;
    use test_case::test_case;

    #[test_case("=a=b", true, None => Ok((svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 13",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "SWAP",
        "POP",
        "JUMP 3",
        "POP2_PUSH3",
        "PUT_VALUE",
        "UPDATE_EMPTY"
    ]), true, false)); "assignment expression as initializer; strict")]
    #[test_case("=a=b", false, None => Ok((svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 13",
        "STRING 1 (b)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "SWAP",
        "POP",
        "JUMP 3",
        "POP2_PUSH3",
        "PUT_VALUE",
        "UPDATE_EMPTY"
    ]), true, false)); "assignment expression as initializer; non-strict")]
    fn compile(src: &str, strict: bool, spots_avail: Option<usize>) -> Result<(Vec<String>, bool, bool), String> {
        let node = Maker::new(src).initializer();
        let mut c =
            if let Some(spot_count) = spots_avail { almost_full_chunk("x", spot_count) } else { Chunk::new("x") };
        node.compile(&mut c, strict)
            .map(|status| {
                (
                    c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
                    status.can_be_abrupt,
                    status.can_be_reference,
                )
            })
            .map_err(|e| e.to_string())
    }
}

mod script {
    use super::*;
    use test_case::test_case;

    #[test_case("a;" => svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
    ]); "stmt")]
    #[test_case("" => svec(&[]); "empty")]
    fn compile(src: &str) -> Vec<String> {
        let node = Maker::new(src).script();
        let mut c = Chunk::new("x");
        node.compile(&mut c).unwrap();
        c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
    }
}

mod script_body {
    use super::*;
    use test_case::test_case;

    #[test_case("a;" => svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
    ]); "stmt")]
    #[test_case("'use strict'; a;" => svec(&[
        "STRING 0 (use strict)",
        "STRING 1 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "UPDATE_EMPTY",
    ]); "use-strict added")]
    fn compile(src: &str) -> Vec<String> {
        let node = Maker::new(src).script_body();
        let mut c = Chunk::new("x");
        node.compile(&mut c).unwrap();
        c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
    }
}
