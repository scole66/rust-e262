#![expect(clippy::bool_assert_comparison)]
use super::*;
use crate::parser::testhelp::*;
use indoc::indoc;
use std::ops::Deref;

mod chunk {
    use super::*;
    use test_case::test_case;

    #[test]
    fn default() {
        let c: Chunk = Chunk::default();

        assert_eq!(c.name, "");
        assert!(c.strings.is_empty());
        assert!(c.opcodes.is_empty());
        assert!(c.floats.is_empty());
        assert!(c.bigints.is_empty());
    }

    #[test]
    fn debug() {
        let c = Chunk::new("debug", 1);
        assert_ne!(format!("{c:?}"), "");
    }

    #[test_case("tomato"; "string slice")]
    #[test_case(String::from("radish"); "String")]
    fn new(name: impl Into<String> + Clone) {
        let c = Chunk::new(name.clone(), 1);

        assert_eq!(c.name, name.into());
        assert!(c.strings.is_empty());
        assert!(c.opcodes.is_empty());
        assert!(c.floats.is_empty());
        assert!(c.bigints.is_empty());
        assert!(c.function_object_data.is_empty());
    }

    #[test_case(&[1.0] => (vec![0], vec![1.0]); "one item")]
    #[test_case(&[1.0, 7.8, 9.1, 7.8] => (vec![0, 1, 2, 1], vec![1.0, 7.8, 9.1]); "with duplicate")]
    fn add_to_float_pool(inputs: &[f64]) -> (Vec<u16>, Vec<f64>) {
        let mut c = Chunk::new("test", 1);
        let mut results = vec![];
        for f in inputs {
            results.push(c.add_to_float_pool(*f).unwrap());
        }
        (results, c.floats.clone())
    }

    #[test_case(&["bob"] => (vec![0], svec(&["bob"])); "one item")]
    #[test_case(&["green", "red", "blue", "red"] => (vec![0, 1, 2, 1], svec(&["green", "red", "blue"])); "with duplicate")]
    fn add_to_string_pool(inputs: &[&str]) -> (Vec<u16>, Vec<String>) {
        let mut c = Chunk::new("test", 1);
        let mut results = vec![];
        for item in inputs {
            results.push(c.add_to_string_pool(JSString::from(*item)).unwrap());
        }
        (results, c.strings.iter().cloned().map(String::from).collect::<Vec<_>>())
    }

    #[test_case(&[BigInt::from(10)] => (vec![0], vec![BigInt::from(10)]); "one item")]
    #[test_case(&[BigInt::from(100), BigInt::from(20000), BigInt::from(99999), BigInt::from(20000)] => (vec![0, 1, 2, 1], vec![BigInt::from(100), BigInt::from(20000), BigInt::from(99999)]); "with duplicate")]
    fn add_to_bigint_pool(inputs: &[BigInt]) -> (Vec<u16>, Vec<BigInt>) {
        let mut c = Chunk::new("test", 1);
        let mut results = vec![];
        for item in inputs {
            results.push(c.add_to_bigint_pool(Rc::new(item.clone())).unwrap());
        }
        (results, c.bigints.iter().map(|b| b.deref().clone()).collect::<Vec<_>>())
    }

    #[test_case(&[&["bob"]] => (vec![0], vec![sset(&["bob"])]); "one item")]
    #[test_case(&[&["green", "red"], &["blue"], &["red", "green"]] => (vec![0, 1, 0], vec![sset(&["green", "red"]), sset(&["blue"])]); "with duplicate")]
    fn add_to_string_set_pool(inputs: &[&[&str]]) -> (Vec<u16>, Vec<AHashSet<String>>) {
        let mut c = Chunk::new("test", 1);
        let mut results = vec![];
        for item in inputs {
            let labels = item.iter().map(|&s| JSString::from(s)).collect::<Vec<_>>();
            results.push(c.add_to_string_set_pool(&labels).unwrap());
        }
        (
            results,
            c.string_sets
                .iter()
                .cloned()
                .map(|labelset| labelset.into_iter().map(String::from).collect::<AHashSet<String>>())
                .collect::<Vec<_>>(),
        )
    }

    mod add_to_pool {
        use super::*;

        #[test]
        fn too_many_floats() {
            let mut c = Chunk::new("test", 1);
            c.floats = vec![0.0; 65537]; // cheating: we're adding lots of duplicates that shouldn't be there
            let res = c.add_to_float_pool(1.0).unwrap_err();
            assert_eq!(res.to_string(), "Out of room for floats in this compilation unit");
        }

        #[test]
        fn too_many_strings() {
            let mut c = Chunk::new("too much", 1);
            c.strings = Vec::<JSString>::with_capacity(65536);
            for _ in 0..65536 {
                c.strings.push(JSString::from(""));
            }
            let res = c.add_to_string_pool("bob".into()).unwrap_err();
            assert_eq!(res.to_string(), "Out of room for strings in this compilation unit");
        }

        #[test]
        fn too_many_bigints() {
            let mut c = Chunk::new("too much", 1);
            c.bigints = Vec::<Rc<BigInt>>::with_capacity(65536);
            for _ in 0..65536 {
                c.bigints.push(Rc::new(BigInt::from(0)));
            }
            let res = c.add_to_bigint_pool(Rc::new(BigInt::from(10))).unwrap_err();
            assert_eq!(res.to_string(), "Out of room for big ints in this compilation unit");
        }

        #[test]
        fn too_many_string_sets() {
            let mut c = Chunk::new("too much", 1);
            c.string_sets = Vec::<AHashSet<JSString>>::with_capacity(65536);
            for _ in 0..65536 {
                c.string_sets.push(AHashSet::<JSString>::new());
            }
            let res = c.add_to_string_set_pool(&[JSString::from("bob")]).unwrap_err();
            assert_eq!(res.to_string(), "Out of room for string sets in this compilation unit");
        }
    }

    mod add_to_func_stash {
        use super::*;

        #[test]
        fn typical() {
            let mut c = Chunk::new("too much", 1);
            let src = "function bob() {}";
            let parse_node = Maker::new(src).function_declaration();
            let data = StashedFunctionData {
                source_text: String::from(src),
                params: parse_node.params.clone().into(),
                body: parse_node.body.clone().into(),
                to_compile: parse_node.clone().into(),
                strict: true,
                this_mode: ThisLexicality::NonLexicalThis,
            };
            let res = c.add_to_func_stash(data).unwrap();
            assert_eq!(res, 0);
            assert_eq!(c.function_object_data.len(), 1);
            assert_eq!(c.function_object_data[0].this_mode, ThisLexicality::NonLexicalThis);
            assert_eq!(c.function_object_data[0].strict, true);
            assert_eq!(c.function_object_data[0].source_text, src);
            let p: Rc<FormalParameters> = c.function_object_data[0].params.clone().try_into().unwrap();
            assert!(Rc::ptr_eq(&p, &parse_node.params));
            let b: Rc<FunctionBody> = c.function_object_data[0].body.clone().try_into().unwrap();
            assert!(Rc::ptr_eq(&b, &parse_node.body));
            let tc: Rc<FunctionDeclaration> = c.function_object_data[0].to_compile.clone().try_into().unwrap();
            assert!(Rc::ptr_eq(&tc, &parse_node));
        }

        #[test]
        fn too_many_funcs() {
            let mut c = Chunk::new("too much", 1);
            c.function_object_data = Vec::<StashedFunctionData>::with_capacity(65536);
            let src = "function bob() {}";
            let parse_node = Maker::new(src).function_declaration();
            for _ in 0..65536 {
                c.function_object_data.push(StashedFunctionData {
                    source_text: String::from(src),
                    params: parse_node.params.clone().into(),
                    body: parse_node.body.clone().into(),
                    to_compile: parse_node.clone().into(),
                    strict: true,
                    this_mode: ThisLexicality::NonLexicalThis,
                });
            }
            let res = c
                .add_to_func_stash(StashedFunctionData {
                    source_text: String::from(src),
                    params: parse_node.params.clone().into(),
                    body: parse_node.body.clone().into(),
                    to_compile: parse_node.clone().into(),
                    strict: true,
                    this_mode: ThisLexicality::NonLexicalThis,
                })
                .unwrap_err();
            assert_eq!(res.to_string(), "Out of room for more functions!");
        }
    }

    #[test]
    fn op() {
        let mut c = Chunk::new("op", 1);
        let orig_op_count = c.opcodes.len();

        c.op(Insn::Null, 37);

        assert_eq!(c.opcodes.len(), orig_op_count + 1);
        assert_eq!(c.opcodes[c.opcodes.len() - 1], u16::from(Insn::Null));
        assert_eq!(c.line_numbers.len(), c.opcodes.len());
        assert_eq!(c.line_numbers[c.line_numbers.len() - 1], 37);
    }

    #[test]
    fn op_plus_arg() {
        let mut c = Chunk::new("op_plus_arg", 1);
        let orig_op_count = c.opcodes.len();

        c.op_plus_arg(Insn::True, 377, 10);

        assert_eq!(c.opcodes.len(), orig_op_count + 2);
        assert_eq!(c.opcodes[orig_op_count], u16::from(Insn::True));
        assert_eq!(c.opcodes[orig_op_count + 1], 377);
        assert_eq!(c.line_numbers.len(), c.opcodes.len());
        assert_eq!(c.line_numbers[orig_op_count], 10);
        assert_eq!(c.line_numbers[orig_op_count + 1], 10);
    }

    #[test]
    fn op_plus_two_args() {
        let mut c = Chunk::new("op_plus_2arg", 1);
        let orig_op_count = c.opcodes.len();

        c.op_plus_two_args(Insn::True, 377, 887, 9);

        assert_eq!(c.opcodes.len(), orig_op_count + 3);
        assert_eq!(c.opcodes[orig_op_count], u16::from(Insn::True));
        assert_eq!(c.opcodes[orig_op_count + 1], 377);
        assert_eq!(c.opcodes[orig_op_count + 2], 887);
        assert_eq!(c.line_numbers.len(), c.opcodes.len());
        assert_eq!(c.line_numbers[orig_op_count], 9);
        assert_eq!(c.line_numbers[orig_op_count + 1], 9);
        assert_eq!(c.line_numbers[orig_op_count + 2], 9);
    }

    #[test]
    fn op_jump() {
        let mut c = Chunk::new("op_jump", 1);
        let orig_op_count = c.opcodes.len();

        let result = c.op_jump(Insn::JumpIfAbrupt, 10);

        assert_eq!(result, orig_op_count + 1);
        assert_eq!(c.opcodes.len(), orig_op_count + 2);
        assert_eq!(c.opcodes[orig_op_count], u16::from(Insn::JumpIfAbrupt));
        assert_eq!(c.opcodes[orig_op_count + 1], 0);
        assert_eq!(c.line_numbers.len(), c.opcodes.len());
        assert_eq!(c.line_numbers[orig_op_count], 10);
        assert_eq!(c.line_numbers[orig_op_count + 1], 10);
    }

    mod op_jump_back {
        use super::*;

        #[test]
        #[expect(clippy::cast_sign_loss)]
        fn normal() {
            let mut c = Chunk::new("op_jump_back", 1);

            let start = c.pos();
            c.op(Insn::Nop, 1);
            c.op(Insn::Nop, 1);
            c.op(Insn::Nop, 1);

            c.op_jump_back(Insn::JumpIfTrue, start, 2).unwrap();

            assert_eq!(c.opcodes.len(), 5);
            assert_eq!(c.opcodes[3], u16::from(Insn::JumpIfTrue));
            assert_eq!(c.opcodes[4], (-5_i16) as u16);
            assert_eq!(c.line_numbers.len(), c.opcodes.len());
            assert_eq!(c.line_numbers[3], 2);
            assert_eq!(c.line_numbers[4], 2);
        }

        #[test]
        fn overflow() {
            let mut c = Chunk::new("op_jump_back", 1);
            let start = c.pos();
            for _ in 1..34000 {
                c.op(Insn::Nop, 1);
            }
            let result = c.op_jump_back(Insn::JumpIfTrue, start, 3);
            assert_eq!(result.unwrap_err().to_string(), "out of range integral type conversion attempted");
        }
    }

    #[test]
    fn pos() {
        let mut c = Chunk::new("pos", 1);

        c.op(Insn::Nop, 7);
        c.op(Insn::Nop, 8);
        c.op(Insn::Nop, 9);

        assert_eq!(c.pos(), 3);
    }

    mod fixup {
        use super::*;

        #[test]
        fn normal() {
            let mut c = Chunk::new("fixup", 1);
            for _ in 0..10 {
                c.op(Insn::True, 10);
            }
            let mark = c.op_jump(Insn::JumpIfAbrupt, 11);
            for _ in 0..10 {
                c.op(Insn::False, 12);
            }
            c.fixup(mark).unwrap();

            let expected: Vec<u16> = vec![
                Insn::True.into(),
                Insn::True.into(),
                Insn::True.into(),
                Insn::True.into(),
                Insn::True.into(),
                Insn::True.into(),
                Insn::True.into(),
                Insn::True.into(),
                Insn::True.into(),
                Insn::True.into(),
                Insn::JumpIfAbrupt.into(),
                10,
                Insn::False.into(),
                Insn::False.into(),
                Insn::False.into(),
                Insn::False.into(),
                Insn::False.into(),
                Insn::False.into(),
                Insn::False.into(),
                Insn::False.into(),
                Insn::False.into(),
                Insn::False.into(),
            ];
            assert_eq!(c.opcodes, expected);
        }

        #[test]
        fn mark_too_large() {
            let mut c = Chunk::new("too large", 1);
            let result = c.fixup(100).unwrap_err().to_string();
            assert_eq!(result, "Fixup location out of range");
        }

        #[test]
        fn jump_too_far() {
            let mut c = Chunk::new("too far", 1);
            c.opcodes = vec![0; 33000];
            let result = c.fixup(0).unwrap_err().to_string();
            assert_eq!(result, "out of range integral type conversion attempted");
        }
    }

    #[test]
    fn disassemble() {
        let mut c = Chunk::new("disassemble", 1);
        let source_text = indoc! {"
            Line 1
            Line 2
            Line 3
            Line 4
            Line 5
            Line 6
            Line 7
            Line 8
            Line 9
            Line 10
            Line 11
            Line 12
            Line 13
            Line 14
            Line 15
            Line 16
            Line 17
            Line 18
            Line 19
            Line 20
        "};
        let string_idx = c.add_to_string_pool("charlie".into()).unwrap();
        let otherstring_idx = c.add_to_string_pool("regexp-flags".into()).unwrap();
        let bigint_idx = c.add_to_bigint_pool(Rc::new(BigInt::from(93939))).unwrap();
        let float_idx = c.add_to_float_pool(78.2).unwrap();
        let ls_idx = c
            .add_to_string_set_pool(&[JSString::from("beta"), JSString::from("zeta"), JSString::from("alpha")])
            .unwrap();
        c.op_plus_arg(Insn::String, string_idx, 1);
        c.op_plus_arg(Insn::Float, float_idx, 2);
        c.op_plus_arg(Insn::Bigint, bigint_idx, 3);
        let mark = c.op_jump(Insn::JumpIfAbrupt, 4);
        c.op(Insn::Resolve, 5);
        c.op(Insn::StrictResolve, 6);
        c.op(Insn::This, 7);
        c.op(Insn::Null, 8);
        c.op(Insn::True, 9);
        c.op(Insn::False, 10);
        c.op(Insn::GetValue, 11);
        c.op(Insn::UpdateEmpty, 12);
        c.fixup(mark).unwrap();
        c.op_plus_arg(Insn::Unwind, 3, 13);
        c.op_plus_arg(Insn::LoopContinues, ls_idx, 14);
        c.op_plus_two_args(Insn::AddMappedArgument, string_idx, 3, 15);
        c.op_plus_two_args(Insn::DefineGetter, 0, 0, 16);
        c.op_plus_two_args(Insn::DefineGetter, 1, 1, 17);
        c.op_plus_two_args(Insn::RegExpCreate, string_idx, otherstring_idx, 18);
        c.op_plus_arg(Insn::MakeSuperPropertyReference, 1, 19);
        c.op_plus_arg(Insn::MakeSuperPropertyReference, 0, 20);

        let result = c.disassemble(source_text);
        let expected = svec(&[
            "====== disassemble ======",
            "00001: Line 1",
            "    STRING                  0 (charlie)",
            "00002: Line 2",
            "    FLOAT                   0 (78.2)",
            "00003: Line 3",
            "    BIGINT                  0 (93939)",
            "00004: Line 4",
            "    JUMP_IF_ABRUPT          8",
            "00005: Line 5",
            "    RESOLVE",
            "00006: Line 6",
            "    STRICT_RESOLVE",
            "00007: Line 7",
            "    THIS",
            "00008: Line 8",
            "    NULL",
            "00009: Line 9",
            "    TRUE",
            "00010: Line 10",
            "    FALSE",
            "00011: Line 11",
            "    GET_VALUE",
            "00012: Line 12",
            "    UPDATE_EMPTY",
            "00013: Line 13",
            "    UNWIND                  3",
            "00014: Line 14",
            "    LOOP_CONT               [alpha, beta, zeta]",
            "00015: Line 15",
            "    AMA                     3 charlie",
            "00016: Line 16",
            "    DEF_GETTER              0 hidden",
            "00017: Line 17",
            "    DEF_GETTER              1 enumerable",
            "00018: Line 18",
            "    REGEXP                  /charlie/regexp-flags",
            "00019: Line 19",
            "    SUPER_REF               strict",
            "00020: Line 20",
            "    SUPER_REF               non-strict",
        ]);
        assert_eq!(result, expected);
    }

    #[test_case(|c| { c.op(Insn::Null, 1); } => Strictness::Indeterminate; "indeterminate")]
    #[test_case(|c| { c.op(Insn::Null, 1); c.op(Insn::StrictRef, 1); c.op(Insn::Resolve, 1); c.op(Insn::True, 1); } => Strictness::Mixed; "mixed")]
    #[test_case(|c| { c.op(Insn::Null, 1); c.op(Insn::Resolve, 1); c.op(Insn::True, 1); } => Strictness::NonStrict; "non-strict")]
    #[test_case(|c| { c.op(Insn::Null, 1); c.op(Insn::StrictRef, 1); c.op(Insn::True, 1); } => Strictness::Strict; "strict")]
    #[test_case(|c| { c.op(Insn::StrictCall, 1); } => Strictness::Strict; "strict call")]
    #[test_case(|c| { c.op(Insn::StrictResolve, 1); } => Strictness::Strict; "strict resolve")]
    #[test_case(|c| { c.op(Insn::Call, 1); } => Strictness::NonStrict; "nonstrict call")]
    #[test_case(|c| { c.op(Insn::Ref, 1); } => Strictness::NonStrict; "nonstrict ref")]
    fn analyze_strictness(make_chunk: impl Fn(&mut Chunk)) -> Strictness {
        let mut c = Chunk::new("analysis test", 1);
        make_chunk(&mut c);
        c.analyze_strictness()
    }

    #[test_case(|c| { c.op(Insn::Null, 1); } => vec![("NULL".to_string(), 1)]; "one instruction")]
    #[test_case(|c| { c.op_plus_arg(Insn::Jump, 10, 1); } => vec![("JUMP 10".to_string(), 2)]; "jump insn")]
    fn repr_with_size(make_chunk: impl Fn(&mut Chunk)) -> Vec<(String, usize)> {
        let mut c = Chunk::new("repr test", 1);
        make_chunk(&mut c);
        c.repr_with_size().iter().map(|(string, size)| (string.split_whitespace().join(" "), *size)).collect::<Vec<_>>()
    }

    #[test_case("bob" => "bob".to_string(); "bob")]
    #[test_case("alice" => "alice".to_string(); "alice")]
    fn set_name(new_name: &str) -> String {
        let mut c = Chunk::new("original", 1);
        c.set_name(new_name);
        c.name
    }

    #[test]
    fn dup_without_code() {
        let mut c1 = Chunk::new("first", 1);
        c1.op(Insn::Nop, 1);
        c1.add_to_string_pool(JSString::from("look, a string!")).unwrap();
        c1.add_to_float_pool(123.125).unwrap();
        c1.add_to_bigint_pool(Rc::new(BigInt::from(10))).unwrap();
        c1.add_to_string_set_pool(&[JSString::from("first"), JSString::from("second")]).unwrap();

        let src = "function bob() {}";
        let parse_node = Maker::new(src).function_declaration();
        let data = StashedFunctionData {
            source_text: String::from(src),
            params: parse_node.params.clone().into(),
            body: parse_node.body.clone().into(),
            to_compile: parse_node.clone().into(),
            strict: true,
            this_mode: ThisLexicality::NonLexicalThis,
        };

        c1.add_to_func_stash(data).unwrap();

        let c2 = Chunk::dup_without_code(&c1, "other");

        assert_eq!(c2.name.as_str(), "other");
        assert_eq!(c2.strings.as_slice(), &[JSString::from("look, a string!")]);
        assert!(c2.opcodes.is_empty());
        assert_eq!(c2.floats.as_slice(), &[123.125]);
        assert_eq!(c2.bigints.iter().map(ToString::to_string).collect::<Vec<_>>().as_slice(), &["10".to_string()]);
        let mut myset = AHashSet::new();
        myset.insert(JSString::from("first"));
        myset.insert(JSString::from("second"));
        assert_eq!(c2.string_sets.as_slice(), &[myset]);
        assert!(c2.function_object_data.is_empty());
    }
}

mod stashed_function_data {
    use super::*;

    #[test]
    fn debug() {
        let src = "function func_name(param1, param2, param3) { let a = thing1(param1); return a + param2 + param3; }";
        let fd = Maker::new(src).function_declaration();
        let sfd = StashedFunctionData {
            source_text: src.into(),
            params: fd.params.clone().into(),
            body: fd.body.clone().into(),
            to_compile: fd.clone().into(),
            strict: true,
            this_mode: ThisLexicality::NonLexicalThis,
        };
        assert_ne!(format!("{sfd:?}"), "");
    }

    #[test]
    fn clone() {
        let src = "function func_name(param1, param2, param3) { let a = thing1(param1); return a + param2 + param3; }";
        let fd = Maker::new(src).function_declaration();
        let sfd = StashedFunctionData {
            source_text: src.into(),
            params: fd.params.clone().into(),
            body: fd.body.clone().into(),
            to_compile: fd.clone().into(),
            strict: true,
            this_mode: ThisLexicality::NonLexicalThis,
        };
        let number2 = sfd.clone();

        assert_eq!(sfd, number2);
    }

    #[test]
    fn eq() {
        let src = "function func_name(param1, param2, param3) { let a = thing1(param1); return a + param2 + param3; }";
        let fd = Maker::new(src).function_declaration();
        let stash = StashedFunctionData {
            source_text: src.into(),
            params: fd.params.clone().into(),
            body: fd.body.clone().into(),
            to_compile: fd.clone().into(),
            strict: true,
            this_mode: ThisLexicality::NonLexicalThis,
        };
        let number2 = stash.clone();
        let src_other = "function a() { return 3; }";
        let fd_other = Maker::new(src_other).function_declaration();
        let stash_other = StashedFunctionData {
            source_text: src.into(),
            params: fd_other.params.clone().into(),
            body: fd_other.body.clone().into(),
            to_compile: fd_other.clone().into(),
            strict: true,
            this_mode: ThisLexicality::NonLexicalThis,
        };

        assert_eq!(stash == number2, true);
        assert_eq!(stash == stash_other, false);
        assert_eq!(stash != number2, false);
        assert_eq!(stash != stash_other, true);
    }
}

mod concise_chunk {
    use super::*;

    #[test]
    fn debug() {
        // The whole point of concise types is that they print on one line, even if in alternate mode. So
        // validate that.
        let c = Chunk::new("concise", 1);
        let cc = ConciseChunk(&c);
        assert!(format!("{c:#?}").lines().collect::<Vec<_>>().len() > 1); // asserts validity of the test

        // Check the concise formatter
        assert_eq!(format!("{cc:#?}").lines().collect::<Vec<_>>().len(), 1);
        assert_eq!(format!("{cc:?}").lines().collect::<Vec<_>>().len(), 1);
    }
}
