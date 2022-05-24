#![allow(clippy::clone_on_copy)]
use super::*;
use crate::tests::test_agent;
use ahash::AHasher;
use std::hash::{Hash, Hasher};
use test_case::test_case;

fn calculate_hash<T: Hash>(t: &T) -> u64 {
    let mut s = AHasher::new_with_keys(1234, 5678);
    t.hash(&mut s);
    s.finish()
}

impl From<u32> for Location {
    fn from(src: u32) -> Self {
        Location { starting_line: 1, starting_column: src, span: Span { starting_index: src as usize - 1, length: 0 } }
    }
}
impl From<(u32, u32)> for Location {
    fn from(src: (u32, u32)) -> Self {
        let (line, column) = src;
        // This "all previous lines are 256 chars" is a bit unrealistic, but it makes for unsurprising tests. (We can't
        // guarantee, in a test context, that the values of line & column are consistent with starting index. Line 20,
        // column 10 is definitely after line 10 column 50, but if all we're doing is comparing starting indexes, how
        // do we know? Making lines really long helps that intuition make better tests.)
        Location {
            starting_line: line,
            starting_column: column,
            span: Span { starting_index: (line - 1) as usize * 256 + column as usize - 1, length: 0 },
        }
    }
}
impl From<&str> for Location {
    fn from(src: &str) -> Self {
        Location {
            starting_line: 1,
            starting_column: 1,
            span: Span {
                starting_index: 0,
                length: src.len()
            }
        }
    }
}
impl From<(u32,u32,u32)> for Location {
    fn from(src: (u32, u32, u32)) -> Self {
        let (line, column, length) = src;
        // This "all previous lines are 256 chars" is a bit unrealistic, but it makes for unsurprising tests. (We can't
        // guarantee, in a test context, that the values of line & column are consistent with starting index. Line 20,
        // column 10 is definitely after line 10 column 50, but if all we're doing is comparing starting indexes, how
        // do we know? Making lines really long helps that intuition make better tests.)
        Location {
            starting_line: line,
            starting_column: column,
            span: Span { starting_index: (line - 1) as usize * 256 + column as usize - 1, length: length as usize},
        }
    }
}

impl ParseError {
    pub fn unpack(&self, loc: impl Into<Location>) -> (PECode, i32) {
        let expected_loc = loc.into();
        let spot = self.location.starting_column as i32 - expected_loc.starting_column as i32;
        (self.code.clone(), spot)
    }
}

mod pe_code {
    use super::*;
    use test_case::test_case;

    #[test_case(PECode::Generic => "error"; "generic")]
    #[test_case(PECode::EoFExpected => "end-of-file expected"; "EoFExpected")]
    #[test_case(PECode::ImproperNewline => "newline not allowed here"; "ImproperNewline")]
    #[test_case(PECode::InvalidIdentifier => "not an identifier"; "InvalidIdentifier")]
    #[test_case(PECode::KeywordExpected(Keyword::For) => "‘for’ expected"; "Keyword")]
    #[test_case(PECode::KeywordUsedAsIdentifier(Keyword::Throw) => "‘throw’ is a reserved word and may not be used as an identifier"; "KeywordUsedAsIdentifier")]
    #[test_case(PECode::OneOfKeywordExpected(vec![Keyword::Catch, Keyword::Throw, Keyword::Finally]) => "one of [‘catch’, ‘throw’, ‘finally’] expected"; "OneOfKeywordExpected")]
    #[test_case(PECode::OneOfPunctuatorExpected(vec![Punctuator::Semicolon, Punctuator::Colon]) => "one of [‘;’, ‘:’] expected"; "OneOfPunctuatorExpected")]
    #[test_case(PECode::PunctuatorExpected(Punctuator::Semicolon) => "‘;’ expected"; "PunctuatorExpected")]
    #[test_case(PECode::AssignmentExpressionOrSpreadElementExpected => "AssignmentExpression or SpreadElement expected"; "AssignmentExpressionOrSpreadElementExpected")]
    #[test_case(PECode::CommaLeftBracketElementListExpected => "‘,’, ‘]’, or an ElementList expected"; "CommaLeftBracketElementListExpected")]
    #[test_case(PECode::IdentifierStringNumberExpected => "Identifier, String, or Number expected"; "IdentifierStringNumberExpected")]
    #[test_case(PECode::ExpressionSpreadOrRPExpected => "Expression, spread pattern, or closing paren expected"; "ExpressionSpreadOrRPExpected")]
    #[test_case(PECode::BindingIdOrPatternExpected => "BindingIdentifier or BindingPattern expected"; "BindingIdOrPatternExpected")]
    #[test_case(PECode::NewOrMEExpected => "‘new’ or MemberExpression expected"; "NewOrMEExpected")]
    #[test_case(PECode::ChainFailed => "‘(’, ‘[’, ‘`’, or an identifier name was expected (optional chaining failed)"; "ChainFailed")]
    #[test_case(PECode::IdOrFormalsExpected => "Identifier or Formal Parameters expected"; "IdOrFormalsExpected")]
    #[test_case(PECode::ObjectAssignmentPatternEndFailure => "‘}’, an AssignmentRestProperty, or an AssignmentPropertyList expected"; "ObjectAssignmentPatternEndFailure")]
    #[test_case(PECode::ArrayAssignmentPatternEndFailure => "‘,’, ‘]’, or an AssignmentElementList expected"; "ArrayAssignmentPatternEndFailure")]
    #[test_case(PECode::IdRefOrPropertyNameExpected => "IdentifierReference or PropertyName expected"; "IdRefOrPropertyNameExpected")]
    #[test_case(PECode::InvalidCoalesceExpression => "Invalid Coalesce Expression"; "InvalidCoalesceExpression")]
    #[test_case(PECode::ImproperExpression => "Improper Expression"; "ImproperExpression")]
    #[test_case(PECode::DeclarationOrStatementExpected => "Declaration or Statement expected"; "DeclarationOrStatementExpected")]
    #[test_case(PECode::ParseNodeExpected(ParseNodeKind::AssignmentExpression) => "AssignmentExpression expected"; "ParseNodeExpected")]
    #[test_case(PECode::OpenOrIdentExpected => "‘[’, ‘{’, or an identifier expected"; "OpenOrIdentExpected")]
    #[test_case(PECode::ForStatementDefinitionError => "‘var’, LexicalDeclaration, or Expression expected"; "ForStatementDefinitionError")]
    #[test_case(PECode::ForInOfDefinitionError => "‘let’, ‘var’, or a LeftHandSideExpression expected"; "ForInOfDefinitionError")]
    #[test_case(PECode::CaseBlockCloseExpected => "‘}’, ‘case’, or ‘default’ expected"; "CaseBlockCloseExpected")]
    #[test_case(PECode::TryBlockError => "Catch or Finally block expected"; "TryBlockError")]
    fn display(code: PECode) -> String {
        format!("{code}")
    }

    #[test_case(PECode::Generic => with |s| assert_ne!(s, ""); "generic")]
    fn debug(code: PECode) -> String {
        format!("{:?}", code)
    }

    #[test_case(PECode::Generic, PECode::Generic => true; "equal")]
    #[test_case(PECode::TryBlockError, PECode::CaseBlockCloseExpected => false; "unequal")]
    fn eq(left: PECode, right: PECode) -> bool {
        left == right
    }

    #[test_case(PECode::Generic, PECode::Generic => false; "equal")]
    #[test_case(PECode::TryBlockError, PECode::CaseBlockCloseExpected => true; "unequal")]
    fn ne(left: PECode, right: PECode) -> bool {
        left != right
    }

    #[test]
    fn default() {
        assert_eq!(PECode::default(), PECode::Generic)
    }

    #[test]
    fn clone() {
        let code_a = PECode::TryBlockError;
        let code_b = code_a.clone();
        assert_eq!(code_a, code_b);
    }

    #[test]
    fn hash() {
        let code_a = PECode::OneOfKeywordExpected(vec![Keyword::Of, Keyword::In]);
        let code_b = PECode::Generic;

        let hash_a = calculate_hash(&code_a);
        let hash_b = calculate_hash(&code_b);

        assert_ne!(hash_a, hash_b);
        assert_eq!(hash_a, calculate_hash(&code_a));
    }
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
    let left = YieldAwaitKey {
        scanner: Scanner { line: 10, column: 12, start_idx: 11 },
        yield_flag: false,
        await_flag: false,
    };
    let right = left.clone();
    let third =
        YieldAwaitKey { scanner: Scanner { line: 10, column: 12, start_idx: 11 }, yield_flag: false, await_flag: true };
    assert_eq!(left.eq(&right), true);
    assert_eq!(left.ne(&third), true);
    assert_ne!(calculate_hash(&left), calculate_hash(&third));
    assert_eq!(calculate_hash(&left), calculate_hash(&right));
    format!("{:?}", left);
}
#[test]
fn yield_await_tagged_key_01() {
    let left = YieldAwaitTaggedKey {
        scanner: Scanner { line: 10, column: 12, start_idx: 11 },
        yield_flag: false,
        await_flag: false,
        tagged_flag: false,
    };
    let right = left.clone();
    let third = YieldAwaitTaggedKey {
        scanner: Scanner { line: 10, column: 12, start_idx: 11 },
        yield_flag: false,
        await_flag: true,
        tagged_flag: false,
    };
    assert_eq!(left.eq(&right), true);
    assert_eq!(left.ne(&third), true);
    assert_ne!(calculate_hash(&left), calculate_hash(&third));
    assert_eq!(calculate_hash(&left), calculate_hash(&right));
    format!("{:?}", left);
}
#[test]
fn in_yield_await_key_01() {
    let left = InYieldAwaitKey {
        scanner: Scanner { line: 10, column: 12, start_idx: 11 },
        in_flag: true,
        yield_flag: false,
        await_flag: false,
    };
    let right = left.clone();
    let third = InYieldAwaitKey {
        scanner: Scanner { line: 10, column: 12, start_idx: 11 },
        in_flag: true,
        yield_flag: false,
        await_flag: true,
    };
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
    let left =
        InAwaitKey { scanner: Scanner { line: 10, column: 12, start_idx: 11 }, in_flag: true, await_flag: false };
    let right = left.clone();
    let third =
        InAwaitKey { scanner: Scanner { line: 10, column: 22, start_idx: 11 }, in_flag: true, await_flag: false };
    assert_eq!(left.eq(&right), true);
    assert_eq!(left.ne(&third), true);
    assert_ne!(calculate_hash(&left), calculate_hash(&third));
    assert_eq!(calculate_hash(&left), calculate_hash(&right));
    format!("{:?}", left);
}
#[test]
fn yield_await_return_key_01() {
    let left = YieldAwaitReturnKey {
        scanner: Scanner { line: 10, column: 12, start_idx: 11 },
        yield_flag: true,
        await_flag: false,
        return_flag: true,
    };
    let right = left.clone();
    let third = YieldAwaitReturnKey {
        scanner: Scanner { line: 10, column: 22, start_idx: 11 },
        yield_flag: true,
        await_flag: false,
        return_flag: true,
    };
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
    let left = YieldAwaitDefaultKey {
        scanner: Scanner { line: 10, column: 12, start_idx: 11 },
        yield_flag: true,
        await_flag: true,
        default_flag: true,
    };
    let right = left.clone();
    let third = YieldAwaitDefaultKey {
        scanner: Scanner { line: 10, column: 22, start_idx: 11 },
        yield_flag: true,
        await_flag: true,
        default_flag: true,
    };
    assert_eq!(left.eq(&right), true);
    assert_eq!(left.ne(&third), true);
    assert_ne!(calculate_hash(&left), calculate_hash(&third));
    assert_eq!(calculate_hash(&left), calculate_hash(&right));
    format!("{:?}", left);
}

#[test]
fn parser_01() {
    let p = Parser::new("program text", false, ParseGoal::Script);
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
    if let Ok((location, scanner)) = res {
        assert_eq!(scanner, Scanner { line: 1, column: 2, start_idx: 1 });
        assert_eq!(
            location,
            Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 1 } }
        );
    }
}
#[test]
fn scan_for_punct_02() {
    let res = scan_for_punct(Scanner::new(), ";;;;;", ScanGoal::InputElementDiv, Punctuator::LeftParen);
    assert!(res.is_err());
    if let Err(pe) = res {
        assert_eq!(pe, ParseError::new(PECode::PunctuatorExpected(Punctuator::LeftParen), ";"));
    }
}

#[test]
fn scan_for_punct_set_01() {
    // Match at the end
    let res = scan_for_punct_set(
        Scanner::new(),
        ";;;;;",
        ScanGoal::InputElementDiv,
        &[Punctuator::Colon, Punctuator::Eq, Punctuator::Semicolon],
    );
    assert!(res.is_ok());
    if let Ok((p, location, scanner)) = res {
        assert_eq!(scanner, Scanner { line: 1, column: 2, start_idx: 1 });
        assert_eq!(p, Punctuator::Semicolon);
        assert_eq!(
            location,
            Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 1 } }
        );
    }
}
#[test]
fn scan_for_punct_set_02() {
    // Match at the beginning
    let res = scan_for_punct_set(
        Scanner::new(),
        ":::::",
        ScanGoal::InputElementDiv,
        &[Punctuator::Colon, Punctuator::Eq, Punctuator::Semicolon],
    );
    assert!(res.is_ok());
    if let Ok((p, location, scanner)) = res {
        assert_eq!(scanner, Scanner { line: 1, column: 2, start_idx: 1 });
        assert_eq!(p, Punctuator::Colon);
        assert_eq!(
            location,
            Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 1 } }
        );
    }
}
#[test]
fn scan_for_punct_set_03() {
    // Match somewhere in the middle
    let res = scan_for_punct_set(
        Scanner::new(),
        "[[[[[",
        ScanGoal::InputElementDiv,
        &[Punctuator::Colon, Punctuator::LeftBracket, Punctuator::Semicolon],
    );
    assert!(res.is_ok());
    if let Ok((p, location, scanner)) = res {
        assert_eq!(scanner, Scanner { line: 1, column: 2, start_idx: 1 });
        assert_eq!(p, Punctuator::LeftBracket);
        assert_eq!(
            location,
            Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 1 } }
        );
    }
}
#[test]
fn scan_for_punct_set_04() {
    // No match
    let pe = scan_for_punct_set(
        Scanner::new(),
        "&&&&&",
        ScanGoal::InputElementDiv,
        &[Punctuator::Colon, Punctuator::Eq, Punctuator::Semicolon],
    )
    .unwrap_err();
    assert_eq!(
        pe,
        ParseError::new(
            PECode::OneOfPunctuatorExpected(vec![Punctuator::Colon, Punctuator::Eq, Punctuator::Semicolon]),
            "&&"
        )
    );
}

#[test]
fn scan_for_auto_semi_01() {
    let (location, res) = scan_for_auto_semi(Scanner::new(), "", ScanGoal::InputElementDiv).unwrap();
    assert_eq!(res, Scanner { line: 1, column: 1, start_idx: 0 });
    assert_eq!(location, Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 0 } })
}
#[test]
fn scan_for_auto_semi_02() {
    let (location, res) = scan_for_auto_semi(Scanner::new(), ";", ScanGoal::InputElementDiv).unwrap();
    assert_eq!(res, Scanner { line: 1, column: 2, start_idx: 1 });
    assert_eq!(location, Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 1 } })
}
#[test]
fn scan_for_auto_semi_03() {
    let (location, res) = scan_for_auto_semi(Scanner::new(), "}", ScanGoal::InputElementDiv).unwrap();
    assert_eq!(res, Scanner { line: 1, column: 1, start_idx: 0 });
    assert_eq!(location, Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 0 } })
}
#[test]
fn scan_for_auto_semi_04() {
    let (location, res) = scan_for_auto_semi(Scanner::new(), "\n0", ScanGoal::InputElementDiv).unwrap();
    assert_eq!(res, Scanner { line: 1, column: 1, start_idx: 0 });
    assert_eq!(location, Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 0 } })
}
#[test]
fn scan_for_auto_semi_05() {
    let probe = "0";
    let err = scan_for_auto_semi(Scanner::new(), probe, ScanGoal::InputElementDiv).unwrap_err();
    assert_eq!(err, ParseError::new(PECode::PunctuatorExpected(Punctuator::Semicolon), probe));
}
#[test]
fn scan_for_auto_semi_06() {
    let probe = "'\\\n0'";
    let err = scan_for_auto_semi(Scanner::new(), probe, ScanGoal::InputElementDiv).unwrap_err();
    assert_eq!(err, ParseError::new(PECode::PunctuatorExpected(Punctuator::Semicolon), probe));
}

#[test]
fn scan_for_keyword_01() {
    let (location, scanner) =
        scan_for_keyword(Scanner::new(), "class bob", ScanGoal::InputElementDiv, Keyword::Class).unwrap();
    assert_eq!(scanner, Scanner { line: 1, column: 6, start_idx: 5 });
    assert_eq!(location, Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 5 } })
}
#[test]
fn scan_for_keyword_02() {
    let res = scan_for_keyword(Scanner::new(), "class bob", ScanGoal::InputElementDiv, Keyword::For).unwrap_err();
    assert_eq!(res, ParseError::new(PECode::KeywordExpected(Keyword::For), "class"));
}

#[test]
fn scan_for_keywords_01() {
    // Match at the beginning
    let (kwd, location, scan) = scan_for_keywords(
        Scanner::new(),
        "for (;;)",
        ScanGoal::InputElementDiv,
        &[Keyword::For, Keyword::Class, Keyword::Break],
    )
    .unwrap();
    assert_eq!(kwd, Keyword::For);
    assert_eq!(scan, Scanner { line: 1, column: 4, start_idx: 3 });
    assert_eq!(
        location,
        Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 3 } }
    );
}
#[test]
fn scan_for_keywords_02() {
    // Match at the end
    let res = scan_for_keywords(
        Scanner::new(),
        "break;",
        ScanGoal::InputElementDiv,
        &[Keyword::For, Keyword::Class, Keyword::Break],
    );
    assert!(res.is_ok());
    if let Ok((kwd, location, scan)) = res {
        assert_eq!(kwd, Keyword::Break);
        assert_eq!(scan, Scanner { line: 1, column: 6, start_idx: 5 });
        assert_eq!(
            location,
            Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 5 } }
        );
    }
}
#[test]
fn scan_for_keywords_03() {
    // Match in the middle
    let res = scan_for_keywords(
        Scanner::new(),
        "class Transcendant",
        ScanGoal::InputElementDiv,
        &[Keyword::For, Keyword::Class, Keyword::Break],
    );
    assert!(res.is_ok());
    if let Ok((kwd, location, scan)) = res {
        assert_eq!(kwd, Keyword::Class);
        assert_eq!(scan, Scanner { line: 1, column: 6, start_idx: 5 });
        assert_eq!(
            location,
            Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 5 } }
        );
    }
}
#[test]
fn scan_for_keywords_04() {
    // No match
    let res = scan_for_keywords(
        Scanner::new(),
        "import food",
        ScanGoal::InputElementDiv,
        &[Keyword::For, Keyword::Class, Keyword::Break],
    )
    .unwrap_err();
    assert_eq!(
        res,
        ParseError::new(PECode::OneOfKeywordExpected([Keyword::For, Keyword::Class, Keyword::Break].to_vec()), "import")
    );
}

#[test]
fn scan_for_identifiername_01() {
    let (id, location, after) = scan_for_identifiername(Scanner::new(), "rust", ScanGoal::InputElementDiv).unwrap();
    assert_eq!(after, Scanner { line: 1, column: 5, start_idx: 4 });
    assert_eq!(id, IdentifierData { string_value: JSString::from("rust"), keyword_id: None });
    assert_eq!(
        location,
        Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 4 } }
    );
}
#[test]
fn scan_for_identifiername_02() {
    let pe = scan_for_identifiername(Scanner::new(), "!!!!", ScanGoal::InputElementDiv).unwrap_err();
    assert_eq!(pe, ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::IdentifierName), "!"));
}

#[test]
fn scan_for_private_identifier_01() {
    let (id, location, after) =
        scan_for_private_identifier(Scanner::new(), "#rust", ScanGoal::InputElementDiv).unwrap();
    assert_eq!(after, Scanner { line: 1, column: 6, start_idx: 5 });
    assert_eq!(id, IdentifierData { string_value: JSString::from("#rust"), keyword_id: None });
    assert_eq!(
        location,
        Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 5 } }
    );
}
#[test]
fn scan_for_private_identifier_02() {
    let pe = scan_for_private_identifier(Scanner::new(), "!!!!", ScanGoal::InputElementDiv).unwrap_err();
    assert_eq!(pe, ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::PrivateIdentifier), "!"));
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
    let probe = "rust";
    let pe = scan_for_eof(Scanner::new(), probe).unwrap_err();
    assert_eq!(pe, ParseError::new(PECode::EoFExpected, probe));
}
#[test]
fn scan_for_eof_02() {
    let (location, res) = scan_for_eof(Scanner::new(), "").unwrap();
    assert_eq!(res.line, 1);
    assert_eq!(res.column, 1);
    assert_eq!(res.start_idx, 0);
    assert_eq!(
        location,
        Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 0 } }
    );
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

    #[test_case(ParseNodeKind::ArrowFunction => "ArrowFunction"; "ArrowFunction")]
    #[test_case(ParseNodeKind::AssignmentExpression => "AssignmentExpression"; "AssignmentExpression")]
    #[test_case(ParseNodeKind::AssignmentOperator => "AssignmentOperator"; "AssignmentOperator")]
    #[test_case(ParseNodeKind::AssignmentPattern => "AssignmentPattern"; "AssignmentPattern")]
    #[test_case(ParseNodeKind::AsyncArrowFunction => "AsyncArrowFunction"; "AsyncArrowFunction")]
    #[test_case(ParseNodeKind::AsyncConciseBody => "AsyncConciseBody"; "AsyncConciseBody")]
    #[test_case(ParseNodeKind::AwaitExpression => "AwaitExpression"; "AwaitExpression")]
    #[test_case(ParseNodeKind::BindingElement => "BindingElement"; "BindingElement")]
    #[test_case(ParseNodeKind::BindingPattern => "BindingPattern"; "BindingPattern")]
    #[test_case(ParseNodeKind::BindingProperty => "BindingProperty"; "BindingProperty")]
    #[test_case(ParseNodeKind::BlockStatement => "BlockStatement"; "BlockStatement")]
    #[test_case(ParseNodeKind::BreakableStatement => "BreakableStatement"; "BreakableStatement")]
    #[test_case(ParseNodeKind::BreakStatement => "BreakStatement"; "BreakStatement")]
    #[test_case(ParseNodeKind::CallExpression => "CallExpression"; "CallExpression")]
    #[test_case(ParseNodeKind::CatchParameter => "CatchParameter"; "CatchParameter")]
    #[test_case(ParseNodeKind::ClassBody => "ClassBody"; "ClassBody")]
    #[test_case(ParseNodeKind::ClassElement => "ClassElement"; "ClassElement")]
    #[test_case(ParseNodeKind::ClassElementName => "ClassElementName"; "ClassElementName")]
    #[test_case(ParseNodeKind::ClassHeritage => "ClassHeritage"; "ClassHeritage")]
    #[test_case(ParseNodeKind::ConciseBody => "ConciseBody"; "ConciseBody")]
    #[test_case(ParseNodeKind::ConditionalExpression => "ConditionalExpression"; "ConditionalExpression")]
    #[test_case(ParseNodeKind::ContinueStatement => "ContinueStatement"; "ContinueStatement")]
    #[test_case(ParseNodeKind::DebuggerStatement => "DebuggerStatement"; "DebuggerStatement")]
    #[test_case(ParseNodeKind::Declaration => "Declaration"; "Declaration")]
    #[test_case(ParseNodeKind::EmptyStatement => "EmptyStatement"; "EmptyStatement")]
    #[test_case(ParseNodeKind::ExponentiationExpression => "ExponentiationExpression"; "ExponentiationExpression")]
    #[test_case(ParseNodeKind::Expression => "Expression"; "Expression")]
    #[test_case(ParseNodeKind::ExpressionBody => "ExpressionBody"; "ExpressionBody")]
    #[test_case(ParseNodeKind::ExpressionStatement => "ExpressionStatement"; "ExpressionStatement")]
    #[test_case(ParseNodeKind::ForBinding => "ForBinding"; "ForBinding")]
    #[test_case(ParseNodeKind::HoistableDeclaration => "HoistableDeclaration"; "HoistableDeclaration")]
    #[test_case(ParseNodeKind::IdentifierName => "IdentifierName"; "IdentifierName")]
    #[test_case(ParseNodeKind::IfStatement => "IfStatement"; "IfStatement")]
    #[test_case(ParseNodeKind::IterationStatement => "IterationStatement"; "IterationStatement")]
    #[test_case(ParseNodeKind::LabelledItem => "LabelledItem"; "LabelledItem")]
    #[test_case(ParseNodeKind::LabelledStatement => "LabelledStatement"; "LabelledStatement")]
    #[test_case(ParseNodeKind::LeftHandSideExpression => "LeftHandSideExpression"; "LeftHandSideExpression")]
    #[test_case(ParseNodeKind::LexicalBinding => "LexicalBinding"; "LexicalBinding")]
    #[test_case(ParseNodeKind::Literal => "Literal"; "Literal")]
    #[test_case(ParseNodeKind::MemberExpression => "MemberExpression"; "MemberExpression")]
    #[test_case(ParseNodeKind::MethodDefinition => "MethodDefinition"; "MethodDefinition")]
    #[test_case(ParseNodeKind::NewTarget => "NewTarget"; "NewTarget")]
    #[test_case(ParseNodeKind::NoSubstitutionTemplate => "NoSubstitutionTemplate"; "NoSubstitutionTemplate")]
    #[test_case(ParseNodeKind::ObjectBindingPattern => "ObjectBindingPattern"; "ObjectBindingPattern")]
    #[test_case(ParseNodeKind::OptionalExpression => "OptionalExpression"; "OptionalExpression")]
    #[test_case(ParseNodeKind::PrimaryExpression => "PrimaryExpression"; "PrimaryExpression")]
    #[test_case(ParseNodeKind::PrivateIdentifier => "PrivateIdentifier"; "PrivateIdentifier")]
    #[test_case(ParseNodeKind::PropertyName => "PropertyName"; "PropertyName")]
    #[test_case(ParseNodeKind::RegularExpression => "RegularExpression"; "RegularExpression")]
    #[test_case(ParseNodeKind::RelationalExpression => "RelationalExpression"; "RelationalExpression")]
    #[test_case(ParseNodeKind::ReturnStatement => "ReturnStatement"; "ReturnStatement")]
    #[test_case(ParseNodeKind::ScriptBody => "ScriptBody"; "ScriptBody")]
    #[test_case(ParseNodeKind::Statement => "Statement"; "Statement")]
    #[test_case(ParseNodeKind::StatementList => "StatementList"; "StatementList")]
    #[test_case(ParseNodeKind::StatementListItem => "StatementListItem"; "StatementListItem")]
    #[test_case(ParseNodeKind::SubstitutionTemplate => "SubstitutionTemplate"; "SubstitutionTemplate")]
    #[test_case(ParseNodeKind::Super => "Super"; "Super pnk")]
    #[test_case(ParseNodeKind::SuperCall => "SuperCall"; "SuperCall")]
    #[test_case(ParseNodeKind::SuperProperty => "SuperProperty"; "SuperProperty")]
    #[test_case(ParseNodeKind::TemplateLiteral => "TemplateLiteral"; "TemplateLiteral")]
    #[test_case(ParseNodeKind::TemplateMiddle => "TemplateMiddle"; "TemplateMiddle")]
    #[test_case(ParseNodeKind::TemplateSpans => "TemplateSpans"; "TemplateSpans")]
    #[test_case(ParseNodeKind::TemplateTail => "TemplateTail"; "TemplateTail")]
    #[test_case(ParseNodeKind::This => "This"; "This")]
    #[test_case(ParseNodeKind::ThrowStatement => "ThrowStatement"; "ThrowStatement")]
    #[test_case(ParseNodeKind::TryStatement => "TryStatement"; "TryStatement")]
    #[test_case(ParseNodeKind::UnaryExpression => "UnaryExpression"; "UnaryExpression")]
    #[test_case(ParseNodeKind::UpdateExpression => "UpdateExpression"; "UpdateExpression")]
    #[test_case(ParseNodeKind::VariableDeclaration => "VariableDeclaration"; "VariableDeclaration")]
    #[test_case(ParseNodeKind::VariableStatement => "VariableStatement"; "VariableStatement")]
    #[test_case(ParseNodeKind::WithStatement => "WithStatement"; "WithStatement")]
    #[test_case(ParseNodeKind::YieldExpression => "YieldExpression"; "YieldExpression")]
    fn display(pnk: ParseNodeKind) -> String {
        format!("{pnk}")
    }
}
#[test]
fn parse_text_01() {
    let mut agent = test_agent();
    let res = parse_text(&mut agent, "0;", ParseGoal::Script);
    assert!(matches!(res, ParsedText::Script(_)));
}
#[test]
fn parse_text_02() {
    let mut agent = test_agent();
    let res = parse_text(&mut agent, "for", ParseGoal::Script);
    assert!(matches!(res, ParsedText::Errors(_)));
}
#[test]
fn parse_text_03() {
    let mut agent = test_agent();
    let res = parse_text(&mut agent, "let x; let x;", ParseGoal::Script);
    assert!(matches!(res, ParsedText::Errors(_)));
}
#[test]
#[should_panic(expected = "not yet implemented")]
fn parse_text_04() {
    let mut agent = test_agent();
    parse_text(&mut agent, "let x; let x;", ParseGoal::Module);
}

#[test_case(&["a"] => Vec::<String>::new(); "no dups")]
#[test_case(&[] => Vec::<String>::new(); "empty list")]
#[test_case(&["a", "b", "c", "b", "a"] => vec!["b", "a"]; "dup order different")]
#[test_case(&["a", "b", "c", "a", "b"] => vec!["a", "b"]; "dup order same")]
#[test_case(&["a", "b", "c", "a", "b", "c", "b", "a"] => vec!["a", "b", "c"]; "many dups")]
fn duplicates(inputs: &[&str]) -> Vec<String> {
    let idents = inputs.iter().map(|&s| JSString::from(s)).collect::<Vec<_>>();
    super::duplicates(&idents).into_iter().map(|s| format!("{}", s)).collect::<Vec<_>>()
}

mod parse_error {
    use super::*;
    use test_case::test_case;

    #[test_case(ParseError::new(PECode::Generic, 1) => with |s| assert_ne!(s, ""); "generic")]
    #[test_case(ParseError::new(PECode::OneOfKeywordExpected(vec![Keyword::In, Keyword::Of]), 1) => with |s| assert_ne!(s, ""); "complex")]
    fn debug(pe: ParseError) -> String {
        format!("{:?}", pe)
    }

    #[test]
    fn default() {
        let pe = ParseError::default();

        assert_eq!(pe.code, PECode::Generic);
        assert_eq!(pe.location, Location::default());
    }

    #[test_case(ParseError::new(PECode::Generic, 7), ParseError::new(PECode::Generic, 7) => true; "equal")]
    #[test_case(ParseError::new(PECode::Generic, 2), ParseError::new(PECode::EoFExpected, 9) => false; "unequal")]
    fn eq(e1: ParseError, e2: ParseError) -> bool {
        e1 == e2
    }

    #[test_case(ParseError::new(PECode::Generic, 7), ParseError::new(PECode::Generic, 7) => false; "equal")]
    #[test_case(ParseError::new(PECode::Generic, 2), ParseError::new(PECode::EoFExpected, 9) => true; "unequal")]
    fn ne(e1: ParseError, e2: ParseError) -> bool {
        e1 != e2
    }

    #[test]
    fn clone() {
        let e1 = ParseError::new(PECode::Generic, 1);
        let e2 = ParseError::new(PECode::OneOfKeywordExpected(vec![Keyword::For, Keyword::Const]), 1);

        let e3 = e2.clone();
        assert_eq!(e3, e2);
        assert_ne!(e3, e1);
    }

    #[test_case(ParseError::new(PECode::Generic, 88) => "error"; "generic")]
    fn display(err: ParseError) -> String {
        format!("{err}")
    }

    mod new {
        use super::*;
        use test_case::test_case;

        #[test_case(PECode::Generic, (10, 20) => (PECode::Generic, Location::from((10, 20))); "pair")]
        fn pair(code: PECode, loc: (u32, u32)) -> (PECode, Location) {
            let pe = ParseError::new(code, loc);
            (pe.code, pe.location)
        }

        #[test_case(PECode::Generic, Scanner::new() => (PECode::Generic, Location::from(Scanner::new())); "scanner")]
        fn scanner(code: PECode, scanner: Scanner) -> (PECode, Location) {
            let pe = ParseError::new(code, scanner);
            (pe.code, pe.location)
        }
    }

    #[test_case(ParseError::new(PECode::Generic, 77), ParseError::new(PECode::EoFExpected, 77) => Ordering::Equal; "equal")]
    #[test_case(ParseError::new(PECode::Generic, 70), ParseError::new(PECode::EoFExpected, 77) => Ordering::Less; "less")]
    #[test_case(ParseError::new(PECode::Generic, 767), ParseError::new(PECode::EoFExpected, 77) => Ordering::Greater; "greater")]
    fn compare(e1: ParseError, e2: ParseError) -> Ordering {
        ParseError::compare(&e1, &e2)
    }

    #[test_case(None, None => Ordering::Equal; "all none")]
    #[test_case(None, Some(ParseError::new(PECode::Generic, 1)) => Ordering::Less; "None vs Item")]
    #[test_case(Some(ParseError::new(PECode::Generic, 1)), None => Ordering::Greater; "Item vs None")]
    #[test_case(Some(ParseError::new(PECode::Generic, 10)), Some(ParseError::new(PECode::Generic, 11)) => Ordering::Less; "Item vs Item")]
    fn compare_option(e1: Option<ParseError>, e2: Option<ParseError>) -> Ordering {
        ParseError::compare_option(&e1, &e2)
    }
}

mod location {
    use super::*;
    use test_case::test_case;

    #[test]
    fn debug() {
        let loc = Location::default();
        assert_ne!(format!("{:?}", loc), "");
    }

    #[test]
    fn default() {
        let loc = Location::default();
        assert_eq!(loc.starting_line, 1);
        assert_eq!(loc.starting_column, 1);
        assert_eq!(loc.span, Span::default());
    }

    #[test_case(Location::from(99), Location::from(99) => true; "equal")]
    #[test_case(Location::from(10), Location::from(13) => false; "unequal")]
    fn eq(left: Location, right: Location) -> bool {
        left == right
    }

    #[test_case(Location::from(99), Location::from(99) => false; "equal")]
    #[test_case(Location::from(10), Location::from(13) => true; "unequal")]
    fn ne(left: Location, right: Location) -> bool {
        left != right
    }

    #[test]
    fn clone() {
        let loc = Location::from((99, 33));
        let loc2 = loc.clone();
        assert_eq!(loc, loc2);
    }

    #[test]
    fn hash() {
        let loc1 = Location::from((99, 33));
        let loc2 = Location::from((100, 200));
        let loc3 = Location::from((99, 33));

        assert_eq!(calculate_hash(&loc1), calculate_hash(&loc3));
        assert_ne!(calculate_hash(&loc2), calculate_hash(&loc3));
    }

    mod from {
        use super::*;
        use test_case::test_case;

        #[test_case(Scanner { line: 10, column: 6, start_idx: 50 } => Location { starting_line: 10, starting_column: 6, span: Span { starting_index: 50, length: 0 }}; "basic")]
        fn scanner(s: Scanner) -> Location {
            s.into()
        }

        #[test_case(&Scanner { line: 10, column: 6, start_idx: 50 } => Location { starting_line: 10, starting_column: 6, span: Span { starting_index: 50, length: 0 }}; "basic ref")]
        fn refscanner(s: &Scanner) -> Location {
            s.into()
        }
    }

    #[test_case(Location::from(78), Location::from(78) => Some(Ordering::Equal); "equal")]
    #[test_case(Location::from(78), Location::from(1) => Some(Ordering::Greater); "gt")]
    #[test_case(Location::from(78), Location::from(99) => Some(Ordering::Less); "lt")]
    fn partial_cmp(left: Location, right: Location) -> Option<Ordering> {
        left.partial_cmp(&right)
    }

    #[test_case(Location::from(78), Location::from(78) => Ordering::Equal; "equal")]
    #[test_case(Location::from(78), Location::from(1) => Ordering::Greater; "gt")]
    #[test_case(Location::from(78), Location::from(99) => Ordering::Less; "lt")]
    fn cmp(left: Location, right: Location) -> Ordering {
        left.cmp(&right)
    }
}

mod span {
    use super::*;
    use test_case::test_case;

    #[test]
    fn default() {
        let span = Span::default();
        assert_eq!(span.starting_index, 0);
        assert_eq!(span.length, 0);
    }

    #[test]
    fn debug() {
        let span = Span::default();
        assert_ne!(format!("{:?}", span), "");
    }

    #[test]
    fn clone() {
        let span1 = Span { starting_index: 10, length: 20 };
        let span2 = span1.clone();
        assert_eq!(span1, span2);
    }

    #[test]
    fn hash() {
        let span1 = Span { starting_index: 100, length: 32 };
        let span2 = Span { starting_index: 101, length: 31 };
        let span3 = Span { starting_index: 100, length: 32 };

        assert_eq!(calculate_hash(&span1), calculate_hash(&span3));
        assert_ne!(calculate_hash(&span2), calculate_hash(&span1));
    }

    #[test_case(Span{starting_index:100, length:32}, Span{starting_index:100, length:32} => true; "equal")]
    #[test_case(Span{starting_index:100, length:32}, Span{starting_index:101, length:32} => false; "unequal")]
    fn eq(left: Span, right: Span) -> bool {
        left == right
    }

    #[test_case(Span{starting_index:100, length:32}, Span{starting_index:100, length:32} => false; "equal")]
    #[test_case(Span{starting_index:100, length:32}, Span{starting_index:101, length:32} => true; "unequal")]
    fn ne(left: Span, right: Span) -> bool {
        left != right
    }
}
