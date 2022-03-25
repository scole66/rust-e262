#![allow(clippy::clone_on_copy)]
use super::*;
use ahash::AHasher;
use std::hash::{Hash, Hasher};

fn calculate_hash<T: Hash>(t: &T) -> u64 {
    let mut s = AHasher::new_with_keys(1234, 5678);
    t.hash(&mut s);
    s.finish()
}

#[test]
fn parse_goal_01() {
    format!("{:?} {:?}", ParseGoal::Script, ParseGoal::Module);
}
#[test]
fn parse_goal_02() {
    let left = ParseGoal::Script;
    let right_first = ParseGoal::Script;
    let right_second = ParseGoal::Module;
    assert_eq!(left, right_first);
    assert_ne!(left, right_second);
}
#[test]
fn parse_goal_03() {
    let a = ParseGoal::Script;
    let b = a.clone();
    assert_eq!(a, b);
}
#[test]
fn parse_goal_04() {
    let a = ParseGoal::default();
    assert_eq!(a, ParseGoal::Script);
}

#[test]
fn yield_await_key_01() {
    let left = YieldAwaitKey { scanner: Scanner { line: 10, column: 12, start_idx: 11 }, yield_flag: false, await_flag: false };
    let right = left.clone();
    let third = YieldAwaitKey { scanner: Scanner { line: 10, column: 12, start_idx: 11 }, yield_flag: false, await_flag: true };
    assert_eq!(left.eq(&right), true);
    assert_eq!(left.ne(&third), true);
    assert_ne!(calculate_hash(&left), calculate_hash(&third));
    assert_eq!(calculate_hash(&left), calculate_hash(&right));
    format!("{:?}", left);
}
#[test]
fn yield_await_tagged_key_01() {
    let left = YieldAwaitTaggedKey { scanner: Scanner { line: 10, column: 12, start_idx: 11 }, yield_flag: false, await_flag: false, tagged_flag: false };
    let right = left.clone();
    let third = YieldAwaitTaggedKey { scanner: Scanner { line: 10, column: 12, start_idx: 11 }, yield_flag: false, await_flag: true, tagged_flag: false };
    assert_eq!(left.eq(&right), true);
    assert_eq!(left.ne(&third), true);
    assert_ne!(calculate_hash(&left), calculate_hash(&third));
    assert_eq!(calculate_hash(&left), calculate_hash(&right));
    format!("{:?}", left);
}
#[test]
fn in_yield_await_key_01() {
    let left = InYieldAwaitKey { scanner: Scanner { line: 10, column: 12, start_idx: 11 }, in_flag: true, yield_flag: false, await_flag: false };
    let right = left.clone();
    let third = InYieldAwaitKey { scanner: Scanner { line: 10, column: 12, start_idx: 11 }, in_flag: true, yield_flag: false, await_flag: true };
    assert_eq!(left.eq(&right), true);
    assert_eq!(left.ne(&third), true);
    assert_ne!(calculate_hash(&left), calculate_hash(&third));
    assert_eq!(calculate_hash(&left), calculate_hash(&right));
    format!("{:?}", left);
}
#[test]
fn in_key_01() {
    let left = InKey { scanner: Scanner { line: 10, column: 12, start_idx: 11 }, in_flag: true };
    let right = left.clone();
    let third = InKey { scanner: Scanner { line: 10, column: 22, start_idx: 11 }, in_flag: true };
    assert_eq!(left.eq(&right), true);
    assert_eq!(left.ne(&third), true);
    assert_ne!(calculate_hash(&left), calculate_hash(&third));
    assert_eq!(calculate_hash(&left), calculate_hash(&right));
    format!("{:?}", left);
}
#[test]
fn in_await_key_01() {
    let left = InAwaitKey { scanner: Scanner { line: 10, column: 12, start_idx: 11 }, in_flag: true, await_flag: false };
    let right = left.clone();
    let third = InAwaitKey { scanner: Scanner { line: 10, column: 22, start_idx: 11 }, in_flag: true, await_flag: false };
    assert_eq!(left.eq(&right), true);
    assert_eq!(left.ne(&third), true);
    assert_ne!(calculate_hash(&left), calculate_hash(&third));
    assert_eq!(calculate_hash(&left), calculate_hash(&right));
    format!("{:?}", left);
}
#[test]
fn yield_await_return_key_01() {
    let left = YieldAwaitReturnKey { scanner: Scanner { line: 10, column: 12, start_idx: 11 }, yield_flag: true, await_flag: false, return_flag: true };
    let right = left.clone();
    let third = YieldAwaitReturnKey { scanner: Scanner { line: 10, column: 22, start_idx: 11 }, yield_flag: true, await_flag: false, return_flag: true };
    assert_eq!(left.eq(&right), true);
    assert_eq!(left.ne(&third), true);
    assert_ne!(calculate_hash(&left), calculate_hash(&third));
    assert_eq!(calculate_hash(&left), calculate_hash(&right));
    format!("{:?}", left);
}
#[test]
fn yield_key_01() {
    let left = YieldKey { scanner: Scanner { line: 10, column: 12, start_idx: 11 }, yield_flag: true };
    let right = left.clone();
    let third = YieldKey { scanner: Scanner { line: 10, column: 22, start_idx: 11 }, yield_flag: true };
    assert_eq!(left.eq(&right), true);
    assert_eq!(left.ne(&third), true);
    assert_ne!(calculate_hash(&left), calculate_hash(&third));
    assert_eq!(calculate_hash(&left), calculate_hash(&right));
    format!("{:?}", left);
}
#[test]
fn yield_await_default_key_01() {
    let left = YieldAwaitDefaultKey { scanner: Scanner { line: 10, column: 12, start_idx: 11 }, yield_flag: true, await_flag: true, default_flag: true };
    let right = left.clone();
    let third = YieldAwaitDefaultKey { scanner: Scanner { line: 10, column: 22, start_idx: 11 }, yield_flag: true, await_flag: true, default_flag: true };
    assert_eq!(left.eq(&right), true);
    assert_eq!(left.ne(&third), true);
    assert_ne!(calculate_hash(&left), calculate_hash(&third));
    assert_eq!(calculate_hash(&left), calculate_hash(&right));
    format!("{:?}", left);
}

#[test]
fn parser_01() {
    let p = Parser::new("program text", false, false, ParseGoal::Script);
    assert_eq!(p.strict, false);
    assert_eq!(p.direct, false);
    assert_eq!(p.source, "program text");
    assert_eq!(p.goal, ParseGoal::Script);
    assert!(p.arguments_cache.is_empty());
    assert!(p.arrow_formal_parameters_cache.is_empty());
    assert!(p.assignment_expression_cache.is_empty());
    assert!(p.async_function_body_cache.is_empty());
    assert!(p.async_generator_body_cache.is_empty());
    assert!(p.binding_element_cache.is_empty());
    assert!(p.binding_identifier_cache.is_empty());
    assert!(p.binding_pattern_cache.is_empty());
    assert!(p.binding_rest_element_cache.is_empty());
    assert!(p.binding_rest_property_cache.is_empty());
    assert!(p.bitwise_or_expression_cache.is_empty());
    assert!(p.block_cache.is_empty());
    assert!(p.call_expression_cache.is_empty());
    assert!(p.catch_parameter_cache.is_empty());
    assert!(p.class_tail_cache.is_empty());
    assert!(p.coalesce_expression_cache.is_empty());
    assert!(p.cover_call_expression_and_async_arrow_head_cache.is_empty());
    assert!(p.cpeaapl_cache.is_empty());
    assert!(p.elision_cache.is_empty());
    assert!(p.expression_body_cache.is_empty());
    assert!(p.expression_cache.is_empty());
    assert!(p.for_binding_cache.is_empty());
    assert!(p.formal_parameter_cache.is_empty());
    assert!(p.formal_parameters_cache.is_empty());
    assert!(p.function_body_cache.is_empty());
    assert!(p.function_declaration_cache.is_empty());
    assert!(p.generator_body_cache.is_empty());
    assert!(p.identifier_cache.is_empty());
    assert!(p.identifier_reference_cache.is_empty());
    assert!(p.initializer_cache.is_empty());
    assert!(p.label_identifier_cache.is_empty());
    assert!(p.lexical_declaration_cache.is_empty());
    assert!(p.lhs_cache.is_empty());
    assert!(p.lpn_cache.is_empty());
    assert!(p.member_expression_cache.is_empty());
    assert!(p.meta_property_cache.is_empty());
    assert!(p.method_definition_cache.is_empty());
    assert!(p.property_name_cache.is_empty());
    assert!(p.single_name_binding_cache.is_empty());
    assert!(p.statement_cache.is_empty());
    assert!(p.statement_list_cache.is_empty());
    assert!(p.template_literal_cache.is_empty());
    assert!(p.unary_expression_cache.is_empty());
    assert!(p.unique_formal_parameters_cache.is_empty());
    assert!(p.update_expression_cache.is_empty());
    assert!(p.variable_declaration_list_cache.is_empty());
}

#[test]
fn otherwise_01() {
    let item: Result<i32, ParseError> = Ok(10);
    let result = item.otherwise(|| Ok(1));
    assert!(result.is_ok());
    if let Ok(i) = result {
        assert_eq!(i, 10);
    }
}
#[test]
fn otherwise_02() {
    let item: Result<i32, ParseError> = Err(ParseError::new(PECode::Generic, (10, 20)));
    let result = item.otherwise(|| Ok(1));
    assert!(result.is_ok());
    if let Ok(i) = result {
        assert_eq!(i, 1);
    }
}
#[test]
fn otherwise_03() {
    let item: Result<i32, ParseError> = Err(ParseError::new(PECode::Generic, (10, 20)));
    let result = item.otherwise(|| Err(ParseError::new(PECode::ChainFailed, (20, 3))));
    assert!(result.is_err());
    if let Err(pe) = result {
        assert_eq!(pe.code, PECode::ChainFailed);
    }
}
#[test]
fn otherwise_04() {
    let item: Result<i32, ParseError> = Err(ParseError::new(PECode::Generic, (10, 20))); // earlier in time
    let result = item.otherwise(|| Err(ParseError::new(PECode::ChainFailed, (2, 3)))); // earlier in position
    assert!(result.is_err());
    if let Err(pe) = result {
        assert_eq!(pe.code, PECode::Generic);
    }
}
#[test]
fn otherwise_05() {
    let item: Result<i32, ParseError> = Err(ParseError::new(PECode::Generic, (10, 20))); // earlier in time
    let result = item.otherwise(|| Err(ParseError::new(PECode::ChainFailed, (10, 20)))); // later in time
    assert!(result.is_err());
    if let Err(pe) = result {
        assert_eq!(pe.code, PECode::Generic);
    }
}

#[test]
fn attkind_01() {
    format!("{:?} {:?}", ATTKind::Invalid, ATTKind::Simple);
    let a = ATTKind::Invalid;
    let b = a.clone();
    assert_eq!(a, b);
}

#[test]
fn scan_for_punct_01() {
    let res = scan_for_punct(Scanner::new(), ";;;;;", ScanGoal::InputElementDiv, Punctuator::Semicolon);
    assert!(res.is_ok());
    if let Ok(scanner) = res {
        assert_eq!(scanner, Scanner { line: 1, column: 2, start_idx: 1 });
    }
}
#[test]
fn scan_for_punct_02() {
    let res = scan_for_punct(Scanner::new(), ";;;;;", ScanGoal::InputElementDiv, Punctuator::LeftParen);
    assert!(res.is_err());
    if let Err(pe) = res {
        assert_eq!(pe, ParseError::new(PECode::PunctuatorExpected(Punctuator::LeftParen), 1));
    }
}

#[test]
fn scan_for_punct_set_01() {
    // Match at the end
    let res = scan_for_punct_set(Scanner::new(), ";;;;;", ScanGoal::InputElementDiv, &[Punctuator::Colon, Punctuator::Eq, Punctuator::Semicolon]);
    assert!(res.is_ok());
    if let Ok((p, scanner)) = res {
        assert_eq!(scanner, Scanner { line: 1, column: 2, start_idx: 1 });
        assert_eq!(p, Punctuator::Semicolon);
    }
}
#[test]
fn scan_for_punct_set_02() {
    // Match at the beginning
    let res = scan_for_punct_set(Scanner::new(), ":::::", ScanGoal::InputElementDiv, &[Punctuator::Colon, Punctuator::Eq, Punctuator::Semicolon]);
    assert!(res.is_ok());
    if let Ok((p, scanner)) = res {
        assert_eq!(scanner, Scanner { line: 1, column: 2, start_idx: 1 });
        assert_eq!(p, Punctuator::Colon);
    }
}
#[test]
fn scan_for_punct_set_03() {
    // Match somewhere in the middle
    let res = scan_for_punct_set(Scanner::new(), "[[[[[", ScanGoal::InputElementDiv, &[Punctuator::Colon, Punctuator::LeftBracket, Punctuator::Semicolon]);
    assert!(res.is_ok());
    if let Ok((p, scanner)) = res {
        assert_eq!(scanner, Scanner { line: 1, column: 2, start_idx: 1 });
        assert_eq!(p, Punctuator::LeftBracket);
    }
}
#[test]
fn scan_for_punct_set_04() {
    // No match
    let pe = scan_for_punct_set(Scanner::new(), "&&&&&", ScanGoal::InputElementDiv, &[Punctuator::Colon, Punctuator::Eq, Punctuator::Semicolon]).unwrap_err();
    assert_eq!(pe, ParseError::new(PECode::OneOfPunctuatorExpected(vec![Punctuator::Colon, Punctuator::Eq, Punctuator::Semicolon]), 1));
}

#[test]
fn scan_for_auto_semi_01() {
    let res = scan_for_auto_semi(Scanner::new(), "", ScanGoal::InputElementDiv).unwrap();
    assert_eq!(res, Scanner { line: 1, column: 1, start_idx: 0 });
}
#[test]
fn scan_for_auto_semi_02() {
    let res = scan_for_auto_semi(Scanner::new(), ";", ScanGoal::InputElementDiv).unwrap();
    assert_eq!(res, Scanner { line: 1, column: 2, start_idx: 1 });
}
#[test]
fn scan_for_auto_semi_03() {
    let res = scan_for_auto_semi(Scanner::new(), "}", ScanGoal::InputElementDiv).unwrap();
    assert_eq!(res, Scanner { line: 1, column: 1, start_idx: 0 });
}
#[test]
fn scan_for_auto_semi_04() {
    let res = scan_for_auto_semi(Scanner::new(), "\n0", ScanGoal::InputElementDiv).unwrap();
    assert_eq!(res, Scanner { line: 1, column: 1, start_idx: 0 });
}
#[test]
fn scan_for_auto_semi_05() {
    let res = scan_for_auto_semi(Scanner::new(), "0", ScanGoal::InputElementDiv).unwrap_err();
    assert_eq!(res, ParseError::new(PECode::PunctuatorExpected(Punctuator::Semicolon), 1));
}
#[test]
#[should_panic(expected = "Result::unwrap_err()` on an `Ok` value")] // This is an XFAIL -- it _should_ work, but the code's not there yet.
fn scan_for_auto_semi_06() {
    let res = scan_for_auto_semi(Scanner::new(), "'\\\n0'", ScanGoal::InputElementDiv).unwrap_err();
    assert_eq!(res, ParseError::new(PECode::PunctuatorExpected(Punctuator::Semicolon), 1));
}

#[test]
fn scan_for_keyword_01() {
    let scanner = scan_for_keyword(Scanner::new(), "class bob", ScanGoal::InputElementDiv, Keyword::Class).unwrap();
    assert_eq!(scanner, Scanner { line: 1, column: 6, start_idx: 5 });
}
#[test]
fn scan_for_keyword_02() {
    let res = scan_for_keyword(Scanner::new(), "class bob", ScanGoal::InputElementDiv, Keyword::For).unwrap_err();
    assert_eq!(res, ParseError::new(PECode::KeywordExpected(Keyword::For), 1));
}

#[test]
fn scan_for_keywords_01() {
    // Match at the beginning
    let (kwd, scan) = scan_for_keywords(Scanner::new(), "for (;;)", ScanGoal::InputElementDiv, &[Keyword::For, Keyword::Class, Keyword::Break]).unwrap();
    assert_eq!(kwd, Keyword::For);
    assert_eq!(scan, Scanner { line: 1, column: 4, start_idx: 3 });
}
#[test]
fn scan_for_keywords_02() {
    // Match at the end
    let res = scan_for_keywords(Scanner::new(), "break;", ScanGoal::InputElementDiv, &[Keyword::For, Keyword::Class, Keyword::Break]);
    assert!(res.is_ok());
    if let Ok((kwd, scan)) = res {
        assert_eq!(kwd, Keyword::Break);
        assert_eq!(scan, Scanner { line: 1, column: 6, start_idx: 5 });
    }
}
#[test]
fn scan_for_keywords_03() {
    // Match in the middle
    let res = scan_for_keywords(Scanner::new(), "class Transcendant", ScanGoal::InputElementDiv, &[Keyword::For, Keyword::Class, Keyword::Break]);
    assert!(res.is_ok());
    if let Ok((kwd, scan)) = res {
        assert_eq!(kwd, Keyword::Class);
        assert_eq!(scan, Scanner { line: 1, column: 6, start_idx: 5 });
    }
}
#[test]
fn scan_for_keywords_04() {
    // No match
    let res = scan_for_keywords(Scanner::new(), "import food", ScanGoal::InputElementDiv, &[Keyword::For, Keyword::Class, Keyword::Break]).unwrap_err();
    assert_eq!(res, ParseError::new(PECode::OneOfKeywordExpected([Keyword::For, Keyword::Class, Keyword::Break].to_vec()), 1));
}

#[test]
fn scan_for_identifiername_01() {
    let (id, after) = scan_for_identifiername(Scanner::new(), "rust", ScanGoal::InputElementDiv).unwrap();
    assert_eq!(after, Scanner { line: 1, column: 5, start_idx: 4 });
    assert_eq!(id, IdentifierData { string_value: JSString::from("rust"), keyword_id: None, line: 1, column: 1 });
}
#[test]
fn scan_for_identifiername_02() {
    let pe = scan_for_identifiername(Scanner::new(), "!!!!", ScanGoal::InputElementDiv).unwrap_err();
    assert_eq!(pe, ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::IdentifierName), 1));
}

#[test]
fn scan_for_private_identifier_01() {
    let (id, after) = scan_for_private_identifier(Scanner::new(), "#rust", ScanGoal::InputElementDiv).unwrap();
    assert_eq!(after, Scanner { line: 1, column: 6, start_idx: 5 });
    assert_eq!(id, IdentifierData { string_value: JSString::from("rust"), keyword_id: None, line: 1, column: 2 });
}
#[test]
fn scan_for_private_identifier_02() {
    let pe = scan_for_private_identifier(Scanner::new(), "!!!!", ScanGoal::InputElementDiv).unwrap_err();
    assert_eq!(pe, ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::PrivateIdentifier), 1));
}

#[test]
fn no_line_terminator_01() {
    let pe = no_line_terminator(Scanner::new(), "\n\nfor").unwrap_err();
    assert_eq!(pe, ParseError::new(PECode::ImproperNewline, 1));
}
#[test]
fn no_line_terminator_02() {
    let res = no_line_terminator(Scanner::new(), "for");
    assert!(res.is_ok());
}

#[test]
fn scan_for_eof_01() {
    let pe = scan_for_eof(Scanner::new(), "rust").unwrap_err();
    assert_eq!(pe, ParseError::new(PECode::EoFExpected, 1));
}
#[test]
fn scan_for_eof_02() {
    let res = scan_for_eof(Scanner::new(), "").unwrap();
    assert_eq!(res.line, 1);
    assert_eq!(res.column, 1);
    assert_eq!(res.start_idx, 0);
}

mod parse_node_kind {
    use super::*;
    use test_case::test_case;

    #[test]
    fn debug() {
        assert_ne!(format!("{:?}", ParseNodeKind::Literal), "");
    }
    #[test_case(ParseNodeKind::Literal, ParseNodeKind::This => false; "not equal")]
    #[test_case(ParseNodeKind::Literal, ParseNodeKind::Literal => true; "equal")]
    fn eq(a: ParseNodeKind, b: ParseNodeKind) -> bool {
        a == b
    }
    #[test_case(ParseNodeKind::Literal, ParseNodeKind::This => true; "not equal")]
    #[test_case(ParseNodeKind::Literal, ParseNodeKind::Literal => false; "equal")]
    fn ne(a: ParseNodeKind, b: ParseNodeKind) -> bool {
        a != b
    }
    #[test]
    fn clone() {
        let p1 = ParseNodeKind::Literal;
        let p2 = p1;
        assert_eq!(p1, p2);
        assert_eq!(p1, p1.clone());
    }
    #[test_case(ParseNodeKind::AssignmentExpression, ParseNodeKind::ConciseBody => false; "not equal")]
    #[test_case(ParseNodeKind::AssignmentExpression, ParseNodeKind::AssignmentExpression => true; "equal")]
    fn hash(a: ParseNodeKind, b: ParseNodeKind) -> bool {
        calculate_hash(&a) == calculate_hash(&b)
    }
}
#[test]
fn parse_text_01() {
    let mut agent = Agent::new();
    agent.initialize_host_defined_realm();
    let res = parse_text(&mut agent, "0;", ParseGoal::Script);
    assert!(matches!(res, ParsedText::Script(_)));
}
#[test]
fn parse_text_02() {
    let mut agent = Agent::new();
    agent.initialize_host_defined_realm();
    let res = parse_text(&mut agent, "for", ParseGoal::Script);
    assert!(matches!(res, ParsedText::Errors(_)));
}
#[test]
fn parse_text_03() {
    let mut agent = Agent::new();
    agent.initialize_host_defined_realm();
    let res = parse_text(&mut agent, "let x; let x;", ParseGoal::Script);
    assert!(matches!(res, ParsedText::Errors(_)));
}
#[test]
#[should_panic(expected = "not yet implemented")]
fn parse_text_04() {
    let mut agent = Agent::new();
    agent.initialize_host_defined_realm();
    parse_text(&mut agent, "let x; let x;", ParseGoal::Module);
}
