use super::*;
use crate::parser::testhelp::svec;
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

    #[test]
    fn new() {
        let c = Chunk::new("tomato");

        assert_eq!(c.name, "tomato");
        assert!(c.strings.is_empty());
        assert!(c.opcodes.is_empty());
        assert!(c.floats.is_empty());
        assert!(c.bigints.is_empty());
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
    fn op_jump() {
        let mut c = Chunk::new("op_jump");
        let orig_op_count = c.opcodes.len();

        let result = c.op_jump(Insn::JumpIfAbrupt);

        assert_eq!(result, orig_op_count + 1);
        assert_eq!(c.opcodes.len(), orig_op_count + 2);
        assert_eq!(c.opcodes[orig_op_count], u16::from(Insn::JumpIfAbrupt));
        assert_eq!(c.opcodes[orig_op_count + 1], 0);
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
        ]);
        assert_eq!(result, expected);
    }
}