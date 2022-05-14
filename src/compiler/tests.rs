use super::*;
use crate::chunk::Chunk;
use crate::parser::testhelp::{svec, Maker};
use crate::strings::JSString;
use itertools::Itertools;
use num::BigInt;
use std::rc::Rc;

mod insn {
    use super::*;
    use test_case::test_case;

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
    #[test_case(Insn::Unwrap => "UNWRAP"; "Unwrap instruction")]
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

fn disasm_filt(s: String) -> Option<String> {
    if s.starts_with('=') {
        return None;
    }
    Some(s.split_whitespace().join(" "))
}

fn full_chunk(n: &str) -> Chunk {
    let mut c = Chunk::new(n);
    c.floats = vec![0.0; 65536];
    c.strings = Vec::with_capacity(65536);
    c.bigints = Vec::with_capacity(65536);
    for nbr in 0..65536 {
        c.strings.push(JSString::from(""));
        c.bigints.push(Rc::new(BigInt::from(nbr)));
    }
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
    #[test_case("{a}", true => svec(&["OBJECT", "STRING 0 (a)", "STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 6", "SWAP", "POP", "SWAP", "POP", "JUMP 1", "CR_PROP"]); "strict elements")]
    #[test_case("{a,}", false => svec(&["OBJECT", "STRING 0 (a)", "STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 6", "SWAP", "POP", "SWAP", "POP", "JUMP 1", "CR_PROP"]); "non-strict elements")]
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

    #[test_case("a", true => (svec(&[
        "STRING 0 (a)", "STRING 0 (a)", "STRICT_RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 6", "SWAP", "POP", "SWAP", "POP", "JUMP 1", "CR_PROP"
    ]), true, false); "one item, strict")]
    #[test_case("a", false => (svec(&[
        "STRING 0 (a)", "STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 6", "SWAP", "POP", "SWAP", "POP", "JUMP 1", "CR_PROP"
    ]), true, false); "one item, non-strict")]
    #[test_case("b:1", false => (svec(&[
        "STRING 0 (b)", "FLOAT 0 (1)", "CR_PROP"
    ]), false, false); "one item, errorfree")]
    #[test_case("a,b", true => (svec(&[
        "STRING 0 (a)",
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 6",
        "SWAP",
        "POP",
        "SWAP",
        "POP",
        "JUMP 1",
        "CR_PROP",
        "JUMP_IF_ABRUPT 15",
        "STRING 1 (b)",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 6",
        "SWAP",
        "POP",
        "SWAP",
        "POP",
        "JUMP 1",
        "CR_PROP"
    ]), true, false); "list, strict")]
    #[test_case("a,b", false => (svec(&[
        "STRING 0 (a)",
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 6",
        "SWAP",
        "POP",
        "SWAP",
        "POP",
        "JUMP 1",
        "CR_PROP",
        "JUMP_IF_ABRUPT 15",
        "STRING 1 (b)",
        "STRING 1 (b)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 6",
        "SWAP",
        "POP",
        "SWAP",
        "POP",
        "JUMP 1",
        "CR_PROP"
    ]), true, false); "list, non-strict")]
    #[test_case("a:1,b", false => (svec(&[
        "STRING 0 (a)", "FLOAT 0 (1)", "CR_PROP", "STRING 1 (b)", "STRING 1 (b)", "RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 6", "SWAP", "POP", "SWAP", "POP", "JUMP 1", "CR_PROP"
    ]), true, false); "potential errors in first")]
    #[test_case("a,b:1", false => (svec(&[
        "STRING 0 (a)", "STRING 0 (a)", "RESOLVE", "GET_VALUE", "JUMP_IF_NORMAL 6", "SWAP", "POP", "SWAP", "POP", "JUMP 1", "CR_PROP", "JUMP_IF_ABRUPT 5", "STRING 1 (b)", "FLOAT 0 (1)", "CR_PROP"
    ]), true, false); "potential errors in second")]
    #[test_case("a:0,b:1", false => (svec(&[
        "STRING 0 (a)", "FLOAT 0 (0)", "CR_PROP", "STRING 1 (b)", "FLOAT 1 (1)", "CR_PROP"
    ]), false, false); "error free list")]
    fn compile(src: &str, strict: bool) -> (Vec<String>, bool, bool) {
        let node = Maker::new(src).property_definition_list();
        let mut c = Chunk::new("x");
        let status = node.property_definition_evaluation(&mut c, strict).unwrap();
        (c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(), status.can_be_abrupt, status.can_be_reference)
    }
}

mod member_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true => svec(&["STRING 0 (id)", "STRICT_RESOLVE"]); "fall-thru strict")]
    #[test_case("id", false => svec(&["STRING 0 (id)", "RESOLVE"]); "fall-thru non strict")]
    #[test_case("a.b", true => svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "STRING 1 (b)",
        "STRICT_REF"
    ]); "member property; strict")]
    #[test_case("a.b", false => svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 3",
        "STRING 1 (b)",
        "REF"
    ]); "member property; non-strict")]
    #[test_case("a[b]", true => svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 18",
        "STRING 1 (b)",
        "STRICT_RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "SWAP",
        "POP",
        "JUMP 8",
        "TO_KEY",
        "JUMP_IF_NORMAL 4",
        "SWAP",
        "POP",
        "JUMP 1",
        "STRICT_REF"
    ]); "strict member exp")]
    #[test_case("a[b]", false => svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_ABRUPT 18",
        "STRING 1 (b)",
        "RESOLVE",
        "GET_VALUE",
        "JUMP_IF_NORMAL 4",
        "SWAP",
        "POP",
        "JUMP 8",
        "TO_KEY",
        "JUMP_IF_NORMAL 4",
        "SWAP",
        "POP",
        "JUMP 1",
        "REF"
    ]); "non-strict member exp")]
    fn compile(src: &str, strict: bool) -> Vec<String> {
        let node = Maker::new(src).member_expression();
        let mut c = Chunk::new("x");
        node.compile(&mut c, strict).unwrap();
        c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
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
        "UNWRAP 1",
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
        "UNWRAP 1",
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

mod update_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true => svec(&["STRING 0 (id)", "STRICT_RESOLVE"]); "fall-thru strict")]
    #[test_case("id", false => svec(&["STRING 0 (id)", "RESOLVE"]); "fall-thru non strict")]
    #[test_case("a++", true => svec(&[
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
    ]); "post-increment, strict")]
    #[test_case("a++", false => svec(&[
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
    ]); "post-increment, non-strict")]
    #[test_case("a--", true => svec(&[
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
    ]); "post-decrement, strict")]
    #[test_case("a--", false => svec(&[
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
    ]); "post-decrement, non-strict")]
    #[test_case("++a", true => svec(&["STRING 0 (a)", "STRICT_RESOLVE", "PRE_INCREMENT"]); "pre-increment, strict")]
    #[test_case("++a", false => svec(&["STRING 0 (a)", "RESOLVE", "PRE_INCREMENT"]); "pre-increment, non-strict")]
    #[test_case("--a", true => svec(&["STRING 0 (a)", "STRICT_RESOLVE", "PRE_DECREMENT"]); "pre-decrement, strict")]
    #[test_case("--a", false => svec(&["STRING 0 (a)", "RESOLVE", "PRE_DECREMENT"]); "pre-decrement, non-strict")]
    fn compile(src: &str, strict: bool) -> Vec<String> {
        let node = Maker::new(src).update_expression();
        let mut c = Chunk::new("x");
        node.compile(&mut c, strict).unwrap();
        c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
    }
}

mod unary_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true => svec(&["STRING 0 (id)", "STRICT_RESOLVE"]); "fall-thru strict")]
    #[test_case("id", false => svec(&["STRING 0 (id)", "RESOLVE"]); "fall-thru non strict")]
    #[test_case("-a", true => panics "not yet implemented"; "negate; strict")]
    #[test_case("-a", false => panics "not yet implemented"; "negate; non-strict")]
    #[test_case("+a", true => panics "not yet implemented"; "tonumber; strict")]
    #[test_case("+a", false => panics "not yet implemented"; "tonumber; non-strict")]
    #[test_case("!a", true => panics "not yet implemented"; "not; strict")]
    #[test_case("!a", false => panics "not yet implemented"; "not; non-strict")]
    #[test_case("~a", true => panics "not yet implemented"; "complement; strict")]
    #[test_case("~a", false => panics "not yet implemented"; "complement; non-strict")]
    #[test_case("void a", true => svec(&["STRING 0 (a)", "STRICT_RESOLVE", "VOID"]); "void; strict")]
    #[test_case("void a", false => svec(&["STRING 0 (a)", "RESOLVE", "VOID"]); "void; non-strict")]
    #[test_case("typeof a", true => svec(&["STRING 0 (a)", "STRICT_RESOLVE", "TYPEOF"]); "typeof; strict")]
    #[test_case("typeof a", false => svec(&["STRING 0 (a)", "RESOLVE", "TYPEOF"]); "typeof; non-strict")]
    #[test_case("delete a", true => svec(&["STRING 0 (a)", "STRICT_RESOLVE", "DELETE"]); "delete; strict")]
    #[test_case("delete a", false => svec(&["STRING 0 (a)", "RESOLVE", "DELETE"]); "delete; non-strict")]
    #[test_case("await a", true => panics "not yet implemented"; "await; strict")]
    #[test_case("await a", false => panics "not yet implemented"; "await; non-strict")]
    fn compile(src: &str, strict: bool) -> Vec<String> {
        let node = Maker::new(src).unary_expression();
        let mut c = Chunk::new("x");
        node.compile(&mut c, strict).unwrap();
        c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
    }
}

mod exponentiation_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("id", true => svec(&["STRING 0 (id)", "STRICT_RESOLVE"]); "fall-thru strict")]
    #[test_case("id", false => svec(&["STRING 0 (id)", "RESOLVE"]); "fall-thru non strict")]
    #[test_case("a**8", true => panics "not yet implemented"; "exponent expr")]
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
    #[test_case("a*8", true => panics "not yet implemented"; "multiply expr")]
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
    #[test_case("a+3", true => panics "not yet implemented"; "add expr")]
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

    #[test_case("id", true => svec(&["STRING 0 (id)", "STRICT_RESOLVE"]); "fall-thru strict")]
    #[test_case("id", false => svec(&["STRING 0 (id)", "RESOLVE"]); "fall-thru non strict")]
    #[test_case("a=6", true => svec(&[
        "STRING 0 (a)",
        "STRICT_RESOLVE",
        "JUMP_IF_ABRUPT 5",
        "FLOAT 0 (6)",
        "POP2_PUSH3",
        "PUT_VALUE",
        "UPDATE_EMPTY"
    ]); "strict assignment expr")]
    #[test_case("a=6", false => svec(&[
        "STRING 0 (a)",
        "RESOLVE",
        "JUMP_IF_ABRUPT 5",
        "FLOAT 0 (6)",
        "POP2_PUSH3",
        "PUT_VALUE",
        "UPDATE_EMPTY"
    ]); "non-strict assignment expr")]
    fn compile(src: &str, strict: bool) -> Vec<String> {
        let node = Maker::new(src).assignment_expression();
        let mut c = Chunk::new("x");
        node.compile(&mut c, strict).unwrap();
        c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
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

    mod compile {
        use super::*;
        use test_case::test_case;

        #[test_case("id;", true => svec(&["STRING 0 (id)", "STRICT_RESOLVE", "GET_VALUE"]); "id-strict")]
        #[test_case("id;", false => svec(&["STRING 0 (id)", "RESOLVE", "GET_VALUE"]); "id-non-strict")]
        #[test_case("a; b;", true => svec(&[
            "STRING 0 (a)", 
            "STRICT_RESOLVE", 
            "GET_VALUE", 
            "JUMP_IF_ABRUPT 5", 
            "STRING 1 (b)", 
            "STRICT_RESOLVE", 
            "GET_VALUE", 
            "UPDATE_EMPTY"
        ]); "strict list")]
        #[test_case("a; b;", false => svec(&[
            "STRING 0 (a)", 
            "RESOLVE", 
            "GET_VALUE", 
            "JUMP_IF_ABRUPT 5", 
            "STRING 1 (b)", 
            "RESOLVE", 
            "GET_VALUE", 
            "UPDATE_EMPTY"
        ]); "non-strict list")]
        fn normal(src: &str, strict: bool) -> Vec<String> {
            let node = Maker::new(src).statement_list();
            let mut c = Chunk::new("x");
            node.compile(&mut c, strict).unwrap();
            c.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>()
        }

        #[test_case("a;3;" => "Out of room for strings in this compilation unit"; "err in list")]
        #[test_case("true;b;" => "Out of room for strings in this compilation unit"; "err in item")]
        fn full_chunk_errs(src: &str) -> String {
            let node = Maker::new(src).statement_list();
            let mut c = full_chunk("x");
            node.compile(&mut c, true).unwrap_err().to_string()
        }

        #[test]
        #[ignore] // blows out the stack
        #[should_panic(expected = "Not yet implemented")]
        fn long_jump() {
            let mut source = String::with_capacity(32768 * 5 + 3 + 1);
            source.push_str("a;{");
            for _ in 0..32768 {
                source.push_str("true;");
            }
            source.push('}');
            let node = Maker::new(source.as_str()).statement_list();
            let mut c = Chunk::new("x");
            assert_eq!(node.compile(&mut c, true).unwrap_err().to_string(), "too far");
        }
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
