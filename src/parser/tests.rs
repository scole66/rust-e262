#![expect(clippy::bool_assert_comparison)]
#![expect(clippy::clone_on_copy)]
use super::*;
use crate::testhelp::*;
use crate::tests::*;
use ahash::AHasher;
use itertools::Itertools;
use std::hash::{Hash, Hasher};
use test_case::test_case;

fn calculate_hash<T: Hash>(t: &T) -> u64 {
    let mut s = AHasher::default();
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
        Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: src.len() } }
    }
}
impl From<(u32, u32, u32)> for Location {
    fn from(src: (u32, u32, u32)) -> Self {
        let (line, column, length) = src;
        // This "all previous lines are 256 chars" is a bit unrealistic, but it makes for unsurprising tests. (We can't
        // guarantee, in a test context, that the values of line & column are consistent with starting index. Line 20,
        // column 10 is definitely after line 10 column 50, but if all we're doing is comparing starting indexes, how
        // do we know? Making lines really long helps that intuition make better tests.)
        Location {
            starting_line: line,
            starting_column: column,
            span: Span { starting_index: (line - 1) as usize * 256 + column as usize - 1, length: length as usize },
        }
    }
}

impl ParseError {
    pub(crate) fn unpack(&self, loc: impl Into<Location>) -> (PECode, i32) {
        let expected_loc = loc.into();
        let spot = i32::try_from(self.location.starting_column).expect("columns should be smaller than 2 billion")
            - i32::try_from(expected_loc.starting_column).expect("columns should be smaller than 2 billion");
        (self.code.clone(), spot)
    }
}

mod pe_code {
    use super::*;
    use test_case::test_case;

    #[test_case(&PECode::Generic => "error"; "generic")]
    #[test_case(&PECode::EoFExpected => "end-of-file expected"; "EoFExpected")]
    #[test_case(&PECode::ImproperNewline => "newline not allowed here"; "ImproperNewline")]
    #[test_case(&PECode::InvalidIdentifier => "not an identifier"; "InvalidIdentifier")]
    #[test_case(&PECode::KeywordExpected(Keyword::For) => "‘for’ expected"; "Keyword")]
    #[test_case(&PECode::KeywordUsedAsIdentifier(Keyword::Throw) => "‘throw’ is a reserved word and may not be used as an identifier"; "KeywordUsedAsIdentifier")]
    #[test_case(&PECode::OneOfKeywordExpected(vec![Keyword::Catch, Keyword::Throw, Keyword::Finally]) => "one of [‘catch’, ‘throw’, ‘finally’] expected"; "OneOfKeywordExpected")]
    #[test_case(&PECode::OneOfPunctuatorExpected(vec![Punctuator::Semicolon, Punctuator::Colon]) => "one of [‘;’, ‘:’] expected"; "OneOfPunctuatorExpected")]
    #[test_case(&PECode::PunctuatorExpected(Punctuator::Semicolon) => "‘;’ expected"; "PunctuatorExpected")]
    #[test_case(&PECode::AssignmentExpressionOrSpreadElementExpected => "AssignmentExpression or SpreadElement expected"; "AssignmentExpressionOrSpreadElementExpected")]
    #[test_case(&PECode::CommaLeftBracketElementListExpected => "‘,’, ‘]’, or an ElementList expected"; "CommaLeftBracketElementListExpected")]
    #[test_case(&PECode::IdentifierStringNumberExpected => "Identifier, String, or Number expected"; "IdentifierStringNumberExpected")]
    #[test_case(&PECode::ExpressionSpreadOrRPExpected => "Expression, spread pattern, or closing paren expected"; "ExpressionSpreadOrRPExpected")]
    #[test_case(&PECode::BindingIdOrPatternExpected => "BindingIdentifier or BindingPattern expected"; "BindingIdOrPatternExpected")]
    #[test_case(&PECode::NewOrMEExpected => "‘new’ or MemberExpression expected"; "NewOrMEExpected")]
    #[test_case(&PECode::ChainFailed => "‘(’, ‘[’, ‘`’, or an identifier name was expected (optional chaining failed)"; "ChainFailed")]
    #[test_case(&PECode::IdOrFormalsExpected => "Identifier or Formal Parameters expected"; "IdOrFormalsExpected")]
    #[test_case(&PECode::ObjectAssignmentPatternEndFailure => "‘}’, an AssignmentRestProperty, or an AssignmentPropertyList expected"; "ObjectAssignmentPatternEndFailure")]
    #[test_case(&PECode::ArrayAssignmentPatternEndFailure => "‘,’, ‘]’, or an AssignmentElementList expected"; "ArrayAssignmentPatternEndFailure")]
    #[test_case(&PECode::IdRefOrPropertyNameExpected => "IdentifierReference or PropertyName expected"; "IdRefOrPropertyNameExpected")]
    #[test_case(&PECode::InvalidCoalesceExpression => "Invalid Coalesce Expression"; "InvalidCoalesceExpression")]
    #[test_case(&PECode::ImproperExpression => "Improper Expression"; "ImproperExpression")]
    #[test_case(&PECode::DeclarationOrStatementExpected => "Declaration or Statement expected"; "DeclarationOrStatementExpected")]
    #[test_case(&PECode::ParseNodeExpected(ParseNodeKind::AssignmentExpression) => "AssignmentExpression expected"; "ParseNodeExpected")]
    #[test_case(&PECode::OpenOrIdentExpected => "‘[’, ‘{’, or an identifier expected"; "OpenOrIdentExpected")]
    #[test_case(&PECode::ForStatementDefinitionError => "‘var’, LexicalDeclaration, or Expression expected"; "ForStatementDefinitionError")]
    #[test_case(&PECode::ForInOfDefinitionError => "‘let’, ‘var’, or a LeftHandSideExpression expected"; "ForInOfDefinitionError")]
    #[test_case(&PECode::CaseBlockCloseExpected => "‘}’, ‘case’, or ‘default’ expected"; "CaseBlockCloseExpected")]
    #[test_case(&PECode::TryBlockError => "Catch or Finally block expected"; "TryBlockError")]
    fn display(code: &PECode) -> String {
        format!("{code}")
    }

    #[test_case(&PECode::Generic => with |s| assert_ne!(s, ""); "generic")]
    fn debug(code: &PECode) -> String {
        format!("{code:?}")
    }

    #[test_case(&PECode::Generic, &PECode::Generic => true; "equal")]
    #[test_case(&PECode::TryBlockError, &PECode::CaseBlockCloseExpected => false; "unequal")]
    fn eq(left: &PECode, right: &PECode) -> bool {
        left == right
    }

    #[test_case(&PECode::Generic, &PECode::Generic => false; "equal")]
    #[test_case(&PECode::TryBlockError, &PECode::CaseBlockCloseExpected => true; "unequal")]
    fn ne(left: &PECode, right: &PECode) -> bool {
        left != right
    }

    #[test]
    fn default() {
        assert_eq!(PECode::default(), PECode::Generic);
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
    assert_ne!(format!("{:?} {:?}", ParseGoal::Script, ParseGoal::Module), "");
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
    assert_ne!(format!("{left:?}"), "");
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
    assert_ne!(format!("{left:?}"), "");
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
    assert_ne!(format!("{left:?}"), "");
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
    assert_ne!(format!("{left:?}"), "");
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
    assert_ne!(format!("{left:?}"), "");
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
    assert_ne!(format!("{left:?}"), "");
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
    assert!(p.member_expression_cache.is_empty());
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
    assert_ne!(format!("{:?} {:?}", ATTKind::Invalid, ATTKind::Simple), "");
    let a = ATTKind::Invalid;
    let b = a.clone();
    assert_eq!(a, b);
}

#[test]
fn scan_for_punct_01() {
    let res = scan_for_punct(Scanner::new(), ";;;;;", InputElementGoal::Div, Punctuator::Semicolon);
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
    let res = scan_for_punct(Scanner::new(), ";;;;;", InputElementGoal::Div, Punctuator::LeftParen);
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
        InputElementGoal::Div,
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
        InputElementGoal::Div,
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
        InputElementGoal::Div,
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
        InputElementGoal::Div,
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
    let (location, res) = scan_for_auto_semi(Scanner::new(), "", InputElementGoal::Div).unwrap();
    assert_eq!(res, Scanner { line: 1, column: 1, start_idx: 0 });
    assert_eq!(
        location,
        Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 0 } }
    );
}
#[test]
fn scan_for_auto_semi_02() {
    let (location, res) = scan_for_auto_semi(Scanner::new(), ";", InputElementGoal::Div).unwrap();
    assert_eq!(res, Scanner { line: 1, column: 2, start_idx: 1 });
    assert_eq!(
        location,
        Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 1 } }
    );
}
#[test]
fn scan_for_auto_semi_03() {
    let (location, res) = scan_for_auto_semi(Scanner::new(), "}", InputElementGoal::Div).unwrap();
    assert_eq!(res, Scanner { line: 1, column: 1, start_idx: 0 });
    assert_eq!(
        location,
        Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 0 } }
    );
}
#[test]
fn scan_for_auto_semi_04() {
    let (location, res) = scan_for_auto_semi(Scanner::new(), "\n0", InputElementGoal::Div).unwrap();
    assert_eq!(res, Scanner { line: 1, column: 1, start_idx: 0 });
    assert_eq!(
        location,
        Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 0 } }
    );
}
#[test]
fn scan_for_auto_semi_05() {
    let probe = "0";
    let err = scan_for_auto_semi(Scanner::new(), probe, InputElementGoal::Div).unwrap_err();
    assert_eq!(err, ParseError::new(PECode::PunctuatorExpected(Punctuator::Semicolon), probe));
}
#[test]
fn scan_for_auto_semi_06() {
    let probe = "'\\\n0'";
    let err = scan_for_auto_semi(Scanner::new(), probe, InputElementGoal::Div).unwrap_err();
    assert_eq!(err, ParseError::new(PECode::PunctuatorExpected(Punctuator::Semicolon), probe));
}

#[test]
fn scan_for_keyword_01() {
    let (location, scanner) =
        scan_for_keyword(Scanner::new(), "class bob", InputElementGoal::Div, Keyword::Class).unwrap();
    assert_eq!(scanner, Scanner { line: 1, column: 6, start_idx: 5 });
    assert_eq!(
        location,
        Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 5 } }
    );
}
#[test]
fn scan_for_keyword_02() {
    let res = scan_for_keyword(Scanner::new(), "class bob", InputElementGoal::Div, Keyword::For).unwrap_err();
    assert_eq!(res, ParseError::new(PECode::KeywordExpected(Keyword::For), "class"));
}

#[test]
fn scan_for_keywords_01() {
    // Match at the beginning
    let (kwd, location, scan) = scan_for_keywords(
        Scanner::new(),
        "for (;;)",
        InputElementGoal::Div,
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
        InputElementGoal::Div,
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
        InputElementGoal::Div,
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
        InputElementGoal::Div,
        &[Keyword::For, Keyword::Class, Keyword::Break],
    )
    .unwrap_err();
    assert_eq!(
        res,
        ParseError::new(
            PECode::OneOfKeywordExpected([Keyword::For, Keyword::Class, Keyword::Break].to_vec()),
            "import"
        )
    );
}

#[test]
fn scan_for_identifiername_01() {
    let (id, location, after) = scan_for_identifiername(Scanner::new(), "rust", InputElementGoal::Div).unwrap();
    assert_eq!(after, Scanner { line: 1, column: 5, start_idx: 4 });
    assert_eq!(id, IdentifierData { string_value: JSString::from("rust"), keyword_id: None });
    assert_eq!(
        location,
        Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 4 } }
    );
}
#[test]
fn scan_for_identifiername_02() {
    let pe = scan_for_identifiername(Scanner::new(), "!!!!", InputElementGoal::Div).unwrap_err();
    assert_eq!(pe, ParseError::new(PECode::IdentifierNameExpected, "!"));
}

#[test]
fn scan_for_private_identifier_01() {
    let (id, location, after) = scan_for_private_identifier(Scanner::new(), "#rust", InputElementGoal::Div).unwrap();
    assert_eq!(after, Scanner { line: 1, column: 6, start_idx: 5 });
    assert_eq!(id, IdentifierData { string_value: JSString::from("#rust"), keyword_id: None });
    assert_eq!(
        location,
        Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 5 } }
    );
}
#[test]
fn scan_for_private_identifier_02() {
    let pe = scan_for_private_identifier(Scanner::new(), "!!!!", InputElementGoal::Div).unwrap_err();
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
    #[test_case(ParseNodeKind::Elisions => "Elisions"; "Elisions")]
    #[test_case(ParseNodeKind::EmptyStatement => "EmptyStatement"; "EmptyStatement")]
    #[test_case(ParseNodeKind::ExponentiationExpression => "ExponentiationExpression"; "ExponentiationExpression")]
    #[test_case(ParseNodeKind::Expression => "Expression"; "Expression")]
    #[test_case(ParseNodeKind::ExpressionBody => "ExpressionBody"; "ExpressionBody")]
    #[test_case(ParseNodeKind::ExpressionStatement => "ExpressionStatement"; "ExpressionStatement")]
    #[test_case(ParseNodeKind::ForBinding => "ForBinding"; "ForBinding")]
    #[test_case(ParseNodeKind::HoistableDeclaration => "HoistableDeclaration"; "HoistableDeclaration")]
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

#[test_case("0;", ParseGoal::Script, false, false => "Script(0 ;)"; "complete (if simple) script")]
#[test_case("for", ParseGoal::Script, false, false => "SyntaxError: 1:4: ‘(’ expected"; "incomplete script")]
#[test_case("let x; let x;", ParseGoal::Script, false, false => "SyntaxError: Duplicate lexically declared names"; "duplicate lexical name error")]
#[test_case("let x; let x;", ParseGoal::Module, false, false => panics "not yet implemented"; "module")]
#[test_case("await, yield", ParseGoal::FormalParameters(YieldAllowed::No, AwaitAllowed::No), false, false => "FormalParameters(await , yield)"; "formal parameters(~,~): strict/f; direct/f; ok syntax")]
#[test_case("await, yield", ParseGoal::FormalParameters(YieldAllowed::Yes, AwaitAllowed::No), false, false => "SyntaxError: identifier 'yield' not allowed when yield expressions are valid"; "formal parameters(+Yield, ~): strict/f, direct/f")]
#[test_case("await, b=yield", ParseGoal::FormalParameters(YieldAllowed::Yes, AwaitAllowed::No), false, false => "FormalParameters(await , b = yield)"; "formal parameters(+Yield, ~): strict/f, direct/f (ok syntax)")]
#[test_case("{ return 'value'; }", ParseGoal::FunctionBody(YieldAllowed::No, AwaitAllowed::No), true, false => "FunctionBody({ return 'value' ; })"; "function body")]
#[test_case("{ yield a; }", ParseGoal::GeneratorBody, true, false => "GeneratorBody({ yield a ; })"; "generator body")]
#[test_case("{ await a; }", ParseGoal::AsyncFunctionBody, true, false => "AsyncFunctionBody({ await a ; })"; "async function body")]
#[test_case("{ yield await a; }", ParseGoal::AsyncGeneratorBody, true, false => "AsyncGeneratorBody({ yield await a ; })"; "async generator body")]
#[test_case("function f() {}", ParseGoal::FunctionExpression, true, false => "FunctionExpression(function f ( ) { })"; "function expression")]
#[test_case("function* f() {}", ParseGoal::GeneratorExpression, true, false => "GeneratorExpression(function * f ( ) { })"; "generator expression")]
#[test_case("async function f() {}", ParseGoal::AsyncFunctionExpression, true, false => "AsyncFunctionExpression(async function f ( ) { })"; "async function expression")]
#[test_case("async function* f() {}", ParseGoal::AsyncGeneratorExpression, true, false => "AsyncGeneratorExpression(async function * f ( ) { })"; "async generator expression")]
#[test_case("run(); \n /* ", ParseGoal::Script, false, false => "SyntaxError: 1:7: end-of-file expected"; "unterminated comment")]
#[test_case("a)", ParseGoal::FormalParameters(YieldAllowed::No, AwaitAllowed::No), true, false => "SyntaxError: parameters had unparsed trailing text"; "parameters with extra at the end")]
#[test_case("function f() {} /*", ParseGoal::FunctionExpression, true, false => "SyntaxError: Unterminated /*-style comment. Started on line 1, column 17."; "function expression with trailing comment")]
fn parse_text(src: &str, goal: ParseGoal, strict: bool, direct: bool) -> String {
    setup_test_agent();
    let res = super::parse_text(src, goal, strict, direct);
    match res {
        ParsedText::Script(s) => format!("Script({s})"),
        ParsedText::Empty => "Empty".to_string(),
        ParsedText::Errors(errs) => errs.iter().map(unwind_any_error_object).join("; "),
        ParsedText::FormalParameters(formal_parameters) => format!("FormalParameters({formal_parameters})",),
        ParsedText::FunctionBody(function_body) => format!("FunctionBody({function_body})"),
        ParsedText::GeneratorBody(generator_body) => format!("GeneratorBody({generator_body})"),
        ParsedText::AsyncFunctionBody(async_function_body) => format!("AsyncFunctionBody({async_function_body})"),
        ParsedText::AsyncGeneratorBody(async_generator_body) => format!("AsyncGeneratorBody({async_generator_body})"),
        ParsedText::FunctionExpression(function_expression) => format!("FunctionExpression({function_expression})"),
        ParsedText::GeneratorExpression(generator_expression) => format!("GeneratorExpression({generator_expression})"),
        ParsedText::AsyncFunctionExpression(async_function_expression) => {
            format!("AsyncFunctionExpression({async_function_expression})")
        }
        ParsedText::AsyncGeneratorExpression(async_generator_expression) => {
            format!("AsyncGeneratorExpression({async_generator_expression})")
        }
        ParsedText::FunctionDeclaration(function_declaration) => {
            format!("FunctionDeclaration({function_declaration})")
        }
        ParsedText::GeneratorDeclaration(generator_declaration) => {
            format!("GeneratorDeclaration({generator_declaration})")
        }
        ParsedText::AsyncFunctionDeclaration(async_function_declaration) => {
            format!("AsyncFunctionDeclaration({async_function_declaration})")
        }
        ParsedText::AsyncGeneratorDeclaration(async_generator_declaration) => {
            format!("AsyncGeneratorDeclaration({async_generator_declaration})")
        }
        _ => format!("Something else {res:?} --- shouldn't happen in this test"),
    }
}

#[test_case(&["a"] => Vec::<String>::new(); "no dups")]
#[test_case(&[] => Vec::<String>::new(); "empty list")]
#[test_case(&["a", "b", "c", "b", "a"] => vec!["b", "a"]; "dup order different")]
#[test_case(&["a", "b", "c", "a", "b"] => vec!["a", "b"]; "dup order same")]
#[test_case(&["a", "b", "c", "a", "b", "c", "b", "a"] => vec!["a", "b", "c"]; "many dups")]
fn duplicates(inputs: &[&str]) -> Vec<String> {
    let idents = inputs.iter().map(|&s| JSString::from(s)).collect::<Vec<_>>();
    super::duplicates(&idents).into_iter().map(|s| format!("{s}")).collect::<Vec<_>>()
}

mod parse_error {
    use super::*;
    use test_case::test_case;

    #[test_case(&ParseError::new(PECode::Generic, 1) => with |s| assert_ne!(s, ""); "generic")]
    #[test_case(&ParseError::new(PECode::OneOfKeywordExpected(vec![Keyword::In, Keyword::Of]), 1) => with |s| assert_ne!(s, ""); "complex")]
    fn debug(pe: &ParseError) -> String {
        format!("{pe:?}")
    }

    #[test]
    fn default() {
        let pe = ParseError::default();

        assert_eq!(pe.code, PECode::Generic);
        assert_eq!(pe.location, Location::default());
    }

    #[test_case(&ParseError::new(PECode::Generic, 7), &ParseError::new(PECode::Generic, 7) => true; "equal")]
    #[test_case(&ParseError::new(PECode::Generic, 2), &ParseError::new(PECode::EoFExpected, 9) => false; "unequal")]
    fn eq(e1: &ParseError, e2: &ParseError) -> bool {
        e1 == e2
    }

    #[test_case(&ParseError::new(PECode::Generic, 7), &ParseError::new(PECode::Generic, 7) => false; "equal")]
    #[test_case(&ParseError::new(PECode::Generic, 2), &ParseError::new(PECode::EoFExpected, 9) => true; "unequal")]
    fn ne(e1: &ParseError, e2: &ParseError) -> bool {
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

    #[test_case(&ParseError::new(PECode::Generic, 88) => "error"; "generic")]
    fn display(err: &ParseError) -> String {
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

    #[test_case(&ParseError::new(PECode::Generic, 77), &ParseError::new(PECode::EoFExpected, 77) => Ordering::Equal; "equal")]
    #[test_case(&ParseError::new(PECode::Generic, 70), &ParseError::new(PECode::EoFExpected, 77) => Ordering::Less; "less")]
    #[test_case(&ParseError::new(PECode::Generic, 767), &ParseError::new(PECode::EoFExpected, 77) => Ordering::Greater; "greater")]
    fn compare(e1: &ParseError, e2: &ParseError) -> Ordering {
        ParseError::compare(e1, e2)
    }

    #[test_case(None, None => Ordering::Equal; "all none")]
    #[test_case(None, Some(&ParseError::new(PECode::Generic, 1)) => Ordering::Less; "None vs Item")]
    #[test_case(Some(&ParseError::new(PECode::Generic, 1)), None => Ordering::Greater; "Item vs None")]
    #[test_case(Some(&ParseError::new(PECode::Generic, 10)), Some(&ParseError::new(PECode::Generic, 11)) => Ordering::Less; "Item vs Item")]
    fn compare_option(e1: Option<&ParseError>, e2: Option<&ParseError>) -> Ordering {
        ParseError::compare_option(e1, e2)
    }
}

mod location {
    use super::*;
    use test_case::test_case;

    #[test]
    fn debug() {
        let loc = Location::default();
        assert_ne!(format!("{loc:?}"), "");
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
        assert_ne!(format!("{span:?}"), "");
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

mod await_allowed {
    use super::*;
    use test_case::test_case;

    #[test]
    #[expect(clippy::clone_on_copy)]
    fn clone() {
        let item = AwaitAllowed::Yes;
        let duplicate = item.clone();
        assert!(matches!(duplicate, AwaitAllowed::Yes));
    }

    #[test]
    fn debug() {
        let item = AwaitAllowed::Yes;
        assert_ne!(format!("{item:?}"), "");
    }

    #[test]
    fn default() {
        let item = AwaitAllowed::default();
        assert!(matches!(item, AwaitAllowed::No));
    }

    #[test_case(AwaitAllowed::Yes, AwaitAllowed::Yes => true; "both yes")]
    #[test_case(AwaitAllowed::No, AwaitAllowed::Yes => false; "they differ")]
    fn eq(left: AwaitAllowed, right: AwaitAllowed) -> bool {
        left == right
    }
}

mod yield_allowed {
    use super::*;
    use test_case::test_case;

    #[test]
    #[expect(clippy::clone_on_copy)]
    fn clone() {
        let item = YieldAllowed::Yes;
        let duplicate = item.clone();
        assert!(matches!(duplicate, YieldAllowed::Yes));
    }

    #[test]
    fn debug() {
        let item = YieldAllowed::Yes;
        assert_ne!(format!("{item:?}"), "");
    }

    #[test]
    fn default() {
        let item = YieldAllowed::default();
        assert!(matches!(item, YieldAllowed::No));
    }

    #[test_case(YieldAllowed::Yes, YieldAllowed::Yes => true; "both yes")]
    #[test_case(YieldAllowed::No, YieldAllowed::Yes => false; "they differ")]
    fn eq(left: YieldAllowed, right: YieldAllowed) -> bool {
        left == right
    }
}

mod parsed_text {
    use super::*;
    use test_case::test_case;

    #[test_case(
        || ParsedText::FormalParameters(Maker::new("").formal_parameters())
        => serr("Expected a Script or Syntax Errors");
        "not script"
    )]
    #[test_case(
        || ParsedText::Script(Maker::new("let a = 3;").script())
        => sok("let a = 3 ;");
        "a legit script"
    )]
    #[test_case(
        || ParsedText::Errors(vec![create_type_error_object("testing error a"), create_type_error_object("testing second")])
        => sok("TypeError: testing error a, TypeError: testing second");
        "and errors."
    )]
    fn try_from(make_text: impl FnOnce() -> ParsedText) -> Result<String, String> {
        setup_test_agent();
        let pt = make_text();
        let result: Result<Rc<Script>, Vec<Object>> = pt.try_into().map_err(|e: anyhow::Error| e.to_string())?;
        match result {
            Ok(script) => Ok(script.to_string()),
            Err(errs) => Ok(errs.iter().map(unwind_any_error_object).join(", ")),
        }
    }

    #[test_case(|| ParsedItem::Script(Maker::new("1;").script()) => "Script(1 ;)"; "script")]
    #[test_case(
        || ParsedItem::FormalParameters(Maker::new("...a").formal_parameters())
        => "FormalParameters(... a)";
        "formal parameters"
    )]
    #[test_case(
        || ParsedItem::FunctionBody(Maker::new("2;").function_body()) => "FunctionBody(2 ;)"; "function body"
    )]
    #[test_case(
        || ParsedItem::GeneratorBody(Maker::new("3;").generator_body())
        => "GeneratorBody(3 ;)";
        "generator_body"
    )]
    #[test_case(
        || ParsedItem::AsyncFunctionBody(Maker::new("4;").async_function_body())
        => "AsyncFunctionBody(4 ;)";
        "async_function_body"
    )]
    #[test_case(
        || ParsedItem::AsyncGeneratorBody(Maker::new("5;").async_generator_body())
        => "AsyncGeneratorBody(5 ;)";
        "async_generator_body"
    )]
    #[test_case(
        || ParsedItem::FunctionExpression(Maker::new("function x(){}").function_expression())
        => "FunctionExpression(function x ( ) { })";
        "function_expression"
    )]
    #[test_case(
        || ParsedItem::GeneratorExpression(Maker::new("function *x(){}").generator_expression())
        => "GeneratorExpression(function * x ( ) { })";
        "generator_expression"
    )]
    #[test_case(
        || ParsedItem::AsyncFunctionExpression(Maker::new("async function x(){}").async_function_expression())
        => "AsyncFunctionExpression(async function x ( ) { })";
        "async_function_expression"
    )]
    #[test_case(
        || ParsedItem::AsyncGeneratorExpression(
            Maker::new("async function *x(){}").async_generator_expression()
        )
        => "AsyncGeneratorExpression(async function * x ( ) { })";
        "async_generator_expression"
    )]
    fn from(make_item: impl FnOnce() -> ParsedItem) -> String {
        setup_test_agent();
        let item = make_item();
        let text = ParsedText::from(item);
        match text {
            ParsedText::Errors(errs) => format!("Errors({})", errs.iter().map(unwind_any_error_object).join(", ")),
            ParsedText::Empty => "Empty".to_string(),
            ParsedText::AdditiveExpression(node) => format!("AdditiveExpression({node})"),
            ParsedText::ArgumentList(node) => format!("ArgumentList({node})"),
            ParsedText::Arguments(node) => format!("Arguments({node})"),
            ParsedText::ArrayAssignmentPattern(node) => format!("ArrayAssignmentPattern({node})"),
            ParsedText::ArrayBindingPattern(node) => format!("ArrayBindingPattern({node})"),
            ParsedText::ArrayLiteral(node) => format!("ArrayLiteral({node})"),
            ParsedText::ArrowFormalParameters(node) => format!("ArrowFormalParameters({node})"),
            ParsedText::ArrowFunction(node) => format!("ArrowFunction({node})"),
            ParsedText::ArrowParameters(node) => format!("ArrowParameters({node})"),
            ParsedText::AssignmentElement(node) => format!("AssignmentElement({node})"),
            ParsedText::AssignmentElementList(node) => format!("AssignmentElementList({node})"),
            ParsedText::AssignmentElisionElement(node) => format!("AssignmentElisionElement({node})"),
            ParsedText::AssignmentExpression(node) => format!("AssignmentExpression({node})"),
            ParsedText::AssignmentPattern(node) => format!("AssignmentPattern({node})"),
            ParsedText::AssignmentProperty(node) => format!("AssignmentProperty({node})"),
            ParsedText::AssignmentPropertyList(node) => format!("AssignmentPropertyList({node})"),
            ParsedText::AssignmentRestElement(node) => format!("AssignmentRestElement({node})"),
            ParsedText::AssignmentRestProperty(node) => format!("AssignmentRestProperty({node})"),
            ParsedText::AsyncArrowBindingIdentifier(node) => format!("AsyncArrowBindingIdentifier({node})"),
            //ParsedText::AsyncArrowFunction(node) => format!("AsyncArrowFunction({node})"),
            //ParsedText::AsyncArrowHead(node) => format!("AsyncArrowHead({node})"),
            //ParsedText::AsyncConciseBody(node) => format!("AsyncConciseBody({node})"),
            ParsedText::AsyncFunctionBody(node) => format!("AsyncFunctionBody({node})"),
            ParsedText::AsyncFunctionDeclaration(node) => format!("AsyncFunctionDeclaration({node})"),
            ParsedText::AsyncFunctionExpression(node) => format!("AsyncFunctionExpression({node})"),
            ParsedText::AsyncGeneratorBody(node) => format!("AsyncGeneratorBody({node})"),
            ParsedText::AsyncGeneratorDeclaration(node) => format!("AsyncGeneratorDeclaration({node})"),
            ParsedText::AsyncGeneratorExpression(node) => format!("AsyncGeneratorExpression({node})"),
            //ParsedText::AsyncGeneratorMethod(node) => format!("AsyncGeneratorMethod({node})"),
            //ParsedText::AsyncMethod(node) => format!("AsyncMethod({node})"),
            //ParsedText::AwaitExpression(node) => format!("AwaitExpression({node})"),
            ParsedText::BindingElement(node) => format!("BindingElement({node})"),
            ParsedText::BindingElementList(node) => format!("BindingElementList({node})"),
            ParsedText::BindingElisionElement(node) => format!("BindingElisionElement({node})"),
            //ParsedText::BindingIdentifier(node) => format!("BindingIdentifier({node})"),
            ParsedText::BindingList(node) => format!("BindingList({node})"),
            ParsedText::BindingPattern(node) => format!("BindingPattern({node})"),
            ParsedText::BindingProperty(node) => format!("BindingProperty({node})"),
            ParsedText::BindingPropertyList(node) => format!("BindingPropertyList({node})"),
            ParsedText::BindingRestElement(node) => format!("BindingRestElement({node})"),
            //ParsedText::BindingRestProperty(node) => format!("BindingRestProperty({node})"),
            ParsedText::BitwiseANDExpression(node) => format!("BitwiseANDExpression({node})"),
            ParsedText::BitwiseORExpression(node) => format!("BitwiseORExpression({node})"),
            ParsedText::BitwiseXORExpression(node) => format!("BitwiseXORExpression({node})"),
            ParsedText::Block(node) => format!("Block({node})"),
            ParsedText::BlockStatement(node) => format!("BlockStatement({node})"),
            ParsedText::BreakableStatement(node) => format!("BreakableStatement({node})"),
            //ParsedText::BreakStatement(node) => format!("BreakStatement({node})"),
            ParsedText::CallExpression(node) => format!("CallExpression({node})"),
            ParsedText::CallMemberExpression(node) => format!("CallMemberExpression({node})"),
            ParsedText::CaseBlock(node) => format!("CaseBlock({node})"),
            ParsedText::CaseClause(node) => format!("CaseClause({node})"),
            //ParsedText::CaseClauses(node) => format!("CaseClauses({node})"),
            ParsedText::Catch(node) => format!("Catch({node})"),
            ParsedText::CatchParameter(node) => format!("CatchParameter({node})"),
            //ParsedText::ClassBody(node) => format!("ClassBody({node})"),
            ParsedText::ClassDeclaration(node) => format!("ClassDeclaration({node})"),
            //ParsedText::ClassElement(node) => format!("ClassElement({node})"),
            //ParsedText::ClassElementList(node) => format!("ClassElementList({node})"),
            ParsedText::ClassElementName(node) => format!("ClassElementName({node})"),
            //ParsedText::ClassExpression(node) => format!("ClassExpression({node})"),
            //ParsedText::ClassHeritage(node) => format!("ClassHeritage({node})"),
            //ParsedText::ClassStaticBlock(node) => format!("ClassStaticBlock({node})"),
            ParsedText::ClassStaticBlockBody(node) => format!("ClassStaticBlockBody({node})"),
            ParsedText::ClassStaticBlockStatementList(node) => format!("ClassStaticBlockStatementList({node})"),
            //ParsedText::ClassTail(node) => format!("ClassTail({node})"),
            ParsedText::CoalesceExpression(node) => format!("CoalesceExpression({node})"),
            ParsedText::CoalesceExpressionHead(node) => format!("CoalesceExpressionHead({node})"),
            ParsedText::ComputedPropertyName(node) => format!("ComputedPropertyName({node})"),
            //ParsedText::ConciseBody(node) => format!("ConciseBody({node})"),
            ParsedText::ConditionalExpression(node) => format!("ConditionalExpression({node})"),
            //ParsedText::ContinueStatement(node) => format!("ContinueStatement({node})"),
            //ParsedText::CoverInitializedName(node) => format!("CoverInitializedName({node})"),
            //ParsedText::CoverParenthesizedExpressionAndArrowParameterList(node) => {
            //    format!("CoverParenthesizedExpressionAndArrowParameterList({node})")
            //}
            //ParsedText::DebuggerStatement(node) => format!("DebuggerStatement({node})"),
            ParsedText::Declaration(node) => format!("Declaration({node})"),
            ParsedText::DefaultClause(node) => format!("DefaultClause({node})"),
            ParsedText::DestructuringAssignmentTarget(node) => format!("DestructuringAssignmentTarget({node})"),
            ParsedText::DoWhileStatement(node) => format!("DoWhileStatement({node})"),
            ParsedText::ElementList(node) => format!("ElementList({node})"),
            //ParsedText::Elisions(node) => format!("Elisions({node})"),
            //ParsedText::EmptyStatement(node) => format!("EmptyStatement({node})"),
            ParsedText::EqualityExpression(node) => format!("EqualityExpression({node})"),
            ParsedText::ExponentiationExpression(node) => format!("ExponentiationExpression({node})"),
            ParsedText::Expression(node) => format!("Expression({node})"),
            ParsedText::ExpressionBody(node) => format!("ExpressionBody({node})"),
            ParsedText::ExpressionStatement(node) => format!("ExpressionStatement({node})"),
            ParsedText::FieldDefinition(node) => format!("FieldDefinition({node})"),
            ParsedText::Finally(node) => format!("Finally({node})"),
            ParsedText::ForBinding(node) => format!("ForBinding({node})"),
            ParsedText::ForDeclaration(node) => format!("ForDeclaration({node})"),
            ParsedText::ForInOfStatement(node) => format!("ForInOfStatement({node})"),
            ParsedText::FormalParameter(node) => format!("FormalParameter({node})"),
            ParsedText::FormalParameterList(node) => format!("FormalParameterList({node})"),
            ParsedText::FormalParameters(node) => format!("FormalParameters({node})"),
            ParsedText::ForStatement(node) => format!("ForStatement({node})"),
            ParsedText::FunctionBody(node) => format!("FunctionBody({node})"),
            ParsedText::FunctionDeclaration(node) => format!("FunctionDeclaration({node})"),
            ParsedText::FunctionExpression(node) => format!("FunctionExpression({node})"),
            ParsedText::FunctionRestParameter(node) => format!("FunctionRestParameter({node})"),
            ParsedText::FunctionStatementList(node) => format!("FunctionStatementList({node})"),
            ParsedText::GeneratorBody(node) => format!("GeneratorBody({node})"),
            ParsedText::GeneratorDeclaration(node) => format!("GeneratorDeclaration({node})"),
            ParsedText::GeneratorExpression(node) => format!("GeneratorExpression({node})"),
            //ParsedText::GeneratorMethod(node) => format!("GeneratorMethod({node})"),
            ParsedText::IfStatement(node) => format!("IfStatement({node})"),
            ParsedText::Initializer(node) => format!("Initializer({node})"),
            ParsedText::IterationStatement(node) => format!("IterationStatement({node})"),
            ParsedText::LabelledItem(node) => format!("LabelledItem({node})"),
            ParsedText::LabelledStatement(node) => format!("LabelledStatement({node})"),
            ParsedText::LeftHandSideExpression(node) => format!("LeftHandSideExpression({node})"),
            ParsedText::LexicalBinding(node) => format!("LexicalBinding({node})"),
            ParsedText::LexicalDeclaration(node) => format!("LexicalDeclaration({node})"),
            //ParsedText::Literal(node) => format!("Literal({node})"),
            //ParsedText::LiteralPropertyName(node) => format!("LiteralPropertyName({node})"),
            ParsedText::LogicalANDExpression(node) => format!("LogicalANDExpression({node})"),
            ParsedText::LogicalORExpression(node) => format!("LogicalORExpression({node})"),
            ParsedText::MemberExpression(node) => format!("MemberExpression({node})"),
            //ParsedText::MetaProperty(node) => format!("MetaProperty({node})"),
            ParsedText::MethodDefinition(node) => format!("MethodDefinition({node})"),
            ParsedText::MultiplicativeExpression(node) => format!("MultiplicativeExpression({node})"),
            ParsedText::NewExpression(node) => format!("NewExpression({node})"),
            ParsedText::ObjectAssignmentPattern(node) => format!("ObjectAssignmentPattern({node})"),
            ParsedText::ObjectBindingPattern(node) => format!("ObjectBindingPattern({node})"),
            ParsedText::ObjectLiteral(node) => format!("ObjectLiteral({node})"),
            ParsedText::OptionalChain(node) => format!("OptionalChain({node})"),
            ParsedText::OptionalExpression(node) => format!("OptionalExpression({node})"),
            ParsedText::ParenthesizedExpression(node) => format!("ParenthesizedExpression({node})"),
            ParsedText::PrimaryExpression(node) => format!("PrimaryExpression({node})"),
            ParsedText::PropertyDefinition(node) => format!("PropertyDefinition({node})"),
            ParsedText::PropertyDefinitionList(node) => format!("PropertyDefinitionList({node})"),
            ParsedText::PropertyName(node) => format!("PropertyName({node})"),
            //ParsedText::PropertySetParameterList(node) => format!("PropertySetParameterList({node})"),
            ParsedText::RelationalExpression(node) => format!("RelationalExpression({node})"),
            ParsedText::ReturnStatement(node) => format!("ReturnStatement({node})"),
            ParsedText::Script(node) => format!("Script({node})"),
            ParsedText::ScriptBody(node) => format!("ScriptBody({node})"),
            ParsedText::ShiftExpression(node) => format!("ShiftExpression({node})"),
            ParsedText::ShortCircuitExpression(node) => format!("ShortCircuitExpression({node})"),
            ParsedText::SingleNameBinding(node) => format!("SingleNameBinding({node})"),
            ParsedText::SpreadElement(node) => format!("SpreadElement({node})"),
            ParsedText::Statement(node) => format!("Statement({node})"),
            ParsedText::StatementList(node) => format!("StatementList({node})"),
            ParsedText::StatementListItem(node) => format!("StatementListItem({node})"),
            ParsedText::SubstitutionTemplate(node) => format!("SubstitutionTemplate({node})"),
            //ParsedText::SuperCall(node) => format!("SuperCall({node})"),
            //ParsedText::SuperProperty(node) => format!("SuperProperty({node})"),
            ParsedText::SwitchStatement(node) => format!("SwitchStatement({node})"),
            ParsedText::TemplateLiteral(node) => format!("TemplateLiteral({node})"),
            ParsedText::TemplateMiddleList(node) => format!("TemplateMiddleList({node})"),
            ParsedText::TemplateSpans(node) => format!("TemplateSpans({node})"),
            ParsedText::ThrowStatement(node) => format!("ThrowStatement({node})"),
            ParsedText::TryStatement(node) => format!("TryStatement({node})"),
            ParsedText::UnaryExpression(node) => format!("UnaryExpression({node})"),
            ParsedText::UniqueFormalParameters(node) => format!("UniqueFormalParameters({node})"),
            ParsedText::UpdateExpression(node) => format!("UpdateExpression({node})"),
            ParsedText::VariableDeclaration(node) => format!("VariableDeclaration({node})"),
            ParsedText::VariableDeclarationList(node) => format!("VariableDeclarationList({node})"),
            ParsedText::VariableStatement(node) => format!("VariableStatement({node})"),
            ParsedText::WhileStatement(node) => format!("WhileStatement({node})"),
            //ParsedText::WithStatement(node) => format!("WithStatement({node})"),
            //ParsedText::YieldExpression(node) => format!("YieldExpression({node})"),
        }
    }
}

mod parsed_item {
    use super::*;
    use test_case::test_case;

    #[test_case(
        || ParsedItem::Script(Maker::new("'use strict'; arguments = 10;").script())
        => svec(&["SyntaxError: Invalid left-hand side in assignment"]);
        "script"
    )]
    #[test_case(
        || ParsedItem::FormalParameters(Maker::new("a, a").formal_parameters())
        => svec(&["SyntaxError: ‘a’ already defined"]);
        "formal parameters"
    )]
    #[test_case(
        || ParsedItem::FunctionBody(Maker::new("'use strict'; arguments = 10;").function_body())
        => svec(&["SyntaxError: Invalid left-hand side in assignment"]);
        "function body"
    )]
    #[test_case(
        || ParsedItem::GeneratorBody(Maker::new("'use strict'; arguments = 10;").generator_body())
        => svec(&["SyntaxError: Invalid left-hand side in assignment"]);
        "generator body"
    )]
    #[test_case(
        || ParsedItem::AsyncFunctionBody(Maker::new("'use strict'; arguments = 10;").async_function_body())
        => svec(&["SyntaxError: Invalid left-hand side in assignment"]);
        "async function body"
    )]
    #[test_case(
        || ParsedItem::AsyncGeneratorBody(Maker::new("'use strict'; arguments = 10;").async_generator_body())
        => svec(&["SyntaxError: Invalid left-hand side in assignment"]);
        "async generator body"
    )]
    #[test_case(
        || ParsedItem::FunctionExpression(Maker::new("function() {'use strict'; arguments = 10;}").function_expression())
        => svec(&["SyntaxError: Invalid left-hand side in assignment"]);
        "function Expression"
    )]
    #[test_case(
        || ParsedItem::GeneratorExpression(Maker::new("function *(){'use strict'; arguments = 10;}").generator_expression())
        => svec(&["SyntaxError: Invalid left-hand side in assignment"]);
        "generator Expression"
    )]
    #[test_case(
        || ParsedItem::AsyncFunctionExpression(Maker::new("async function () {'use strict'; arguments = 10;}").async_function_expression())
        => svec(&["SyntaxError: Invalid left-hand side in assignment"]);
        "async function Expression"
    )]
    #[test_case(
        || ParsedItem::AsyncGeneratorExpression(Maker::new("async function *(){'use strict'; arguments = 10;}").async_generator_expression())
        => svec(&["SyntaxError: Invalid left-hand side in assignment"]);
        "async generator Expression"
    )]
    fn early_errors(make_item: impl FnOnce() -> ParsedItem) -> Vec<String> {
        setup_test_agent();
        let mut errs = vec![];
        let item = make_item();
        item.early_errors(&mut errs, true);
        errs.iter().map(unwind_any_error_object).collect::<Vec<_>>()
    }
}
