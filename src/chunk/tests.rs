use super::*;
use crate::parser::testhelp::*;
use std::ops::Deref;

mod chunk {
    use super::*;
    use test_case::test_case;

    #[test]
    fn default() {
        let c: Chunk = Default::default();

        assert_eq!(c.name, "");
        assert!(c.strings.is_empty());
        assert!(c.opcodes.is_empty());
        assert!(c.floats.is_empty());
        assert!(c.bigints.is_empty());
    }

    #[test]
    fn debug() {
        let c = Chunk::new("debug");
        assert_ne!(format!("{:?}", c), "");
    }

    #[test_case("tomato"; "string slice")]
    #[test_case(String::from("radish"); "String")]
    fn new(name: impl Into<String> + Clone) {
        let c = Chunk::new(name.clone());

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
        let mut c = Chunk::new("test");
        let mut results = vec![];
        for f in inputs {
            results.push(c.add_to_float_pool(*f).unwrap());
        }
        (results, c.floats.clone())
    }

    #[test_case(&["bob"] => (vec![0], svec(&["bob"])); "one item")]
    #[test_case(&["green", "red", "blue", "red"] => (vec![0, 1, 2, 1], svec(&["green", "red", "blue"])); "with duplicate")]
    fn add_to_string_pool(inputs: &[&str]) -> (Vec<u16>, Vec<String>) {
        let mut c = Chunk::new("test");
        let mut results = vec![];
        for item in inputs {
            results.push(c.add_to_string_pool(JSString::from(*item)).unwrap());
        }
        (results, c.strings.iter().cloned().map(String::from).collect::<Vec<_>>())
    }

    #[test_case(&[BigInt::from(10)] => (vec![0], vec![BigInt::from(10)]); "one item")]
    #[test_case(&[BigInt::from(100), BigInt::from(20000), BigInt::from(99999), BigInt::from(20000)] => (vec![0, 1, 2, 1], vec![BigInt::from(100), BigInt::from(20000), BigInt::from(99999)]); "with duplicate")]
    fn add_to_bigint_pool(inputs: &[BigInt]) -> (Vec<u16>, Vec<BigInt>) {
        let mut c = Chunk::new("test");
        let mut results = vec![];
        for item in inputs {
            results.push(c.add_to_bigint_pool(Rc::new(item.clone())).unwrap());
        }
        (results, c.bigints.iter().cloned().map(|b| b.deref().clone()).collect::<Vec<_>>())
    }

    #[test_case(&[&["bob"]] => (vec![0], vec![sset(&["bob"])]); "one item")]
    #[test_case(&[&["green", "red"], &["blue"], &["red", "green"]] => (vec![0, 1, 0], vec![sset(&["green", "red"]), sset(&["blue"])]); "with duplicate")]
    fn add_to_label_set_pool(inputs: &[&[&str]]) -> (Vec<u16>, Vec<AHashSet<String>>) {
        let mut c = Chunk::new("test");
        let mut results = vec![];
        for item in inputs {
            let labels = item.iter().map(|&s| JSString::from(s)).collect::<Vec<_>>();
            results.push(c.add_to_label_set_pool(&labels).unwrap());
        }
        (
            results,
            c.label_sets
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
            let mut c = Chunk::new("test");
            c.floats = vec![0.0; 65537]; // cheating: we're adding lots of duplicates that shouldn't be there
            let res = c.add_to_float_pool(1.0).unwrap_err();
            assert_eq!(res.to_string(), "Out of room for floats in this compilation unit");
        }

        #[test]
        fn too_many_strings() {
            let mut c = Chunk::new("too much");
            c.strings = Vec::<JSString>::with_capacity(65536);
            for _ in 0..65536 {
                c.strings.push(JSString::from(""));
            }
            let res = c.add_to_string_pool("bob".into()).unwrap_err();
            assert_eq!(res.to_string(), "Out of room for strings in this compilation unit");
        }

        #[test]
        fn too_many_bigints() {
            let mut c = Chunk::new("too much");
            c.bigints = Vec::<Rc<BigInt>>::with_capacity(65536);
            for _ in 0..65536 {
                c.bigints.push(Rc::new(BigInt::from(0)));
            }
            let res = c.add_to_bigint_pool(Rc::new(BigInt::from(10))).unwrap_err();
            assert_eq!(res.to_string(), "Out of room for big ints in this compilation unit");
        }

        #[test]
        fn too_many_label_sets() {
            let mut c = Chunk::new("too much");
            c.label_sets = Vec::<AHashSet<JSString>>::with_capacity(65536);
            for _ in 0..65536 {
                c.label_sets.push(AHashSet::<JSString>::new());
            }
            let res = c.add_to_label_set_pool(&[JSString::from("bob")]).unwrap_err();
            assert_eq!(res.to_string(), "Out of room for label sets in this compilation unit");
        }
    }

    mod add_to_func_stash {
        use super::*;

        #[test]
        fn typical() {
            let mut c = Chunk::new("too much");
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
            let mut c = Chunk::new("too much");
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
            assert_eq!(res.to_string(), "Out of room for more functions!")
        }
    }

    #[test]
    fn op() {
        let mut c = Chunk::new("op");
        let orig_op_count = c.opcodes.len();

        c.op(Insn::Null);

        assert_eq!(c.opcodes.len(), orig_op_count + 1);
        assert_eq!(c.opcodes[c.opcodes.len() - 1], u16::from(Insn::Null));
    }

    #[test]
    fn op_plus_arg() {
        let mut c = Chunk::new("op_plus_arg");
        let orig_op_count = c.opcodes.len();

        c.op_plus_arg(Insn::True, 377);

        assert_eq!(c.opcodes.len(), orig_op_count + 2);
        assert_eq!(c.opcodes[orig_op_count], u16::from(Insn::True));
        assert_eq!(c.opcodes[orig_op_count + 1], 377);
    }

    #[test]
    fn op_plus_two_args() {
        let mut c = Chunk::new("op_plus_2arg");
        let orig_op_count = c.opcodes.len();

        c.op_plus_two_args(Insn::True, 377, 887);

        assert_eq!(c.opcodes.len(), orig_op_count + 3);
        assert_eq!(c.opcodes[orig_op_count], u16::from(Insn::True));
        assert_eq!(c.opcodes[orig_op_count + 1], 377);
        assert_eq!(c.opcodes[orig_op_count + 2], 887);
    }

    #[test]
    fn op_jump() {
        let mut c = Chunk::new("op_jump");
        let orig_op_count = c.opcodes.len();

        let result = c.op_jump(Insn::JumpIfAbrupt);

        assert_eq!(result, orig_op_count + 1);
        assert_eq!(c.opcodes.len(), orig_op_count + 2);
        assert_eq!(c.opcodes[orig_op_count], u16::from(Insn::JumpIfAbrupt));
        assert_eq!(c.opcodes[orig_op_count + 1], 0);
    }

    mod op_jump_back {
        use super::*;

        #[test]
        fn normal() {
            let mut c = Chunk::new("op_jump_back");

            let start = c.pos();
            c.op(Insn::Nop);
            c.op(Insn::Nop);
            c.op(Insn::Nop);

            c.op_jump_back(Insn::JumpIfTrue, start).unwrap();

            assert_eq!(c.opcodes.len(), 5);
            assert_eq!(c.opcodes[3], u16::from(Insn::JumpIfTrue));
            assert_eq!(c.opcodes[4], (-5_i16) as u16);
        }

        #[test]
        fn overflow() {
            let mut c = Chunk::new("op_jump_back");
            let start = c.pos();
            for _ in 1..34000 {
                c.op(Insn::Nop);
            }
            let result = c.op_jump_back(Insn::JumpIfTrue, start);
            assert_eq!(result.unwrap_err().to_string(), "out of range integral type conversion attempted");
        }
    }

    #[test]
    fn pos() {
        let mut c = Chunk::new("pos");

        c.op(Insn::Nop);
        c.op(Insn::Nop);
        c.op(Insn::Nop);

        assert_eq!(c.pos(), 3);
    }

    mod fixup {
        use super::*;

        #[test]
        fn normal() {
            let mut c = Chunk::new("fixup");
            for _ in 0..10 {
                c.op(Insn::True);
            }
            let mark = c.op_jump(Insn::JumpIfAbrupt);
            for _ in 0..10 {
                c.op(Insn::False);
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
            let mut c = Chunk::new("too large");
            let result = c.fixup(100).unwrap_err().to_string();
            assert_eq!(result, "Fixup location out of range");
        }

        #[test]
        fn jump_too_far() {
            let mut c = Chunk::new("too far");
            c.opcodes = vec![0; 33000];
            let result = c.fixup(0).unwrap_err().to_string();
            assert_eq!(result, "out of range integral type conversion attempted")
        }
    }

    #[test]
    fn disassemble() {
        let mut c = Chunk::new("disassemble");
        let string_idx = c.add_to_string_pool("charlie".into()).unwrap();
        let bigint_idx = c.add_to_bigint_pool(Rc::new(BigInt::from(93939))).unwrap();
        let float_idx = c.add_to_float_pool(78.2).unwrap();
        let ls_idx = c
            .add_to_label_set_pool(&[JSString::from("beta"), JSString::from("zeta"), JSString::from("alpha")])
            .unwrap();
        c.op_plus_arg(Insn::String, string_idx);
        c.op_plus_arg(Insn::Float, float_idx);
        c.op_plus_arg(Insn::Bigint, bigint_idx);
        let mark = c.op_jump(Insn::JumpIfAbrupt);
        c.op(Insn::Resolve);
        c.op(Insn::StrictResolve);
        c.op(Insn::This);
        c.op(Insn::Null);
        c.op(Insn::True);
        c.op(Insn::False);
        c.op(Insn::GetValue);
        c.op(Insn::UpdateEmpty);
        c.fixup(mark).unwrap();
        c.op_plus_arg(Insn::Unwind, 3);
        c.op_plus_arg(Insn::LoopContinues, ls_idx);
        c.op_plus_two_args(Insn::AddMappedArgument, string_idx, 3);

        let result = c.disassemble();
        let expected = svec(&[
            "====== disassemble ======",
            "    STRING              0 (charlie)",
            "    FLOAT               0 (78.2)",
            "    BIGINT              0 (93939)",
            "    JUMP_IF_ABRUPT      8",
            "    RESOLVE",
            "    STRICT_RESOLVE",
            "    THIS",
            "    NULL",
            "    TRUE",
            "    FALSE",
            "    GET_VALUE",
            "    UPDATE_EMPTY",
            "    UNWIND              3",
            "    LOOP_CONT           [alpha, beta, zeta]",
            "    AMA                 3 charlie",
        ]);
        assert_eq!(result, expected);
    }
}

mod stashed_function_data {
    use super::*;

    /*
    pub struct StashedFunctionData {
    pub source_text: String,
    pub params: ParamSource,
    pub body: BodySource,
    pub to_compile: FunctionSource,
    pub strict: bool,
    pub this_mode: ThisLexicality,
    }
    */

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
        assert_ne!(format!("{:?}", sfd), "");
    }
}
