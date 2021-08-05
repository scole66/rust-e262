#![allow(clippy::float_cmp)]
#![allow(clippy::clone_on_copy)]
use super::*;
use ahash::RandomState;
use num::traits::Zero;
use std::hash::{BuildHasher, Hash, Hasher};

#[test]
fn jsstring_from_str() {
    let s = JSString::from("This is a test");
    assert_eq!(s, "This is a test");
}
#[test]
fn jsstring_from_u16s() {
    let msg: [u16; 12] = [0x42, 0x72, 0x69, 0x6e, 0x64, 0x6F, 0x6C, 0x68, 0x61, 0x76, 0x65, 0x6e];
    let t = JSString::from(&msg[..]);
    assert_eq!(t, "Brindolhaven");
}

#[test]
fn skippables_empty() {
    let scanner = Scanner { line: 10, column: 80, start_idx: 0 };
    let result = skip_skippables(&scanner, "");
    let expected = Scanner { start_idx: 0, line: 10, column: 80 };
    assert_eq!(result, Ok(expected));
}
#[test]
fn skippables_no_leading_whitespace() {
    let scanner = Scanner { line: 10, column: 80, start_idx: 0 };
    let result = skip_skippables(&scanner, "abcd   uu");
    let expected = Scanner { start_idx: 0, line: 10, column: 80 };
    assert_eq!(result, Ok(expected));
}
#[test]
fn skippables_only_whitespace() {
    let result = skip_skippables(&Scanner { line: 1, column: 1, start_idx: 0 }, "\t\r\n\t\t\t");
    assert_eq!(result, Ok(Scanner { start_idx: 6, line: 2, column: 4 }));
}
#[test]
fn skippables_ends_on_eol() {
    let result = skip_skippables(&Scanner { start_idx: 0, line: 2, column: 2 }, "\n");
    assert_eq!(result, Ok(Scanner { start_idx: 1, line: 3, column: 1 }));
}
#[test]
fn skippables_ends_on_crlf() {
    let result = skip_skippables(&Scanner { start_idx: 0, line: 10, column: 1 }, "\r\n");
    assert_eq!(result, Ok(Scanner { start_idx: 2, line: 11, column: 1 }));
}
#[test]
fn skippables_ends_on_slash() {
    let result = skip_skippables(&Scanner { start_idx: 0, line: 10, column: 10 }, "   /");
    assert_eq!(result, Ok(Scanner { start_idx: 3, line: 10, column: 13 }));
}
#[test]
fn skippables_ends_in_doubleslash() {
    let result = skip_skippables(&Scanner { start_idx: 0, line: 3, column: 2 }, "\t\t//");
    assert_eq!(result, Ok(Scanner { start_idx: 4, line: 3, column: 6 }));
}
#[test]
fn skippables_not_comment() {
    let result = skip_skippables(&Scanner { start_idx: 0, line: 3, column: 2 }, "/+");
    assert_eq!(result, Ok(Scanner { start_idx: 0, line: 3, column: 2 }));
}
#[test]
fn skippables_ends_in_slc() {
    let result = skip_skippables(&Scanner { start_idx: 0, line: 3, column: 2 }, "\t\t// this ends the line");
    assert_eq!(result, Ok(Scanner { start_idx: 23, line: 3, column: 25 }));
}
#[test]
fn skippables_slc_then_white() {
    let result = skip_skippables(&Scanner { start_idx: 0, line: 3, column: 2 }, "\t\t// this ends the line\r\n\r\nblue");
    assert_eq!(result, Ok(Scanner { start_idx: 27, line: 5, column: 1 }));
}
#[test]
fn skippables_mlc() {
    let result = skip_skippables(&Scanner { start_idx: 0, line: 3, column: 2 }, "/**/");
    assert_eq!(result, Ok(Scanner { start_idx: 4, line: 3, column: 6 }));
}
#[test]
fn skippables_mlc_then_white() {
    let result = skip_skippables(&Scanner { start_idx: 0, line: 3, column: 2 }, "/* abcde */\t\t\nhat");
    assert_eq!(result, Ok(Scanner { start_idx: 14, line: 4, column: 1 }));
}
#[test]
fn skippables_mlc_with_newlines() {
    let result = skip_skippables(&Scanner { start_idx: 0, line: 3, column: 1 }, "/*\n * My Title\n */\nscarf");
    assert_eq!(result, Ok(Scanner { start_idx: 19, line: 6, column: 1 }));
}
#[test]
fn skippables_unterminated_mlc() {
    let result = skip_skippables(&Scanner { start_idx: 0, line: 3, column: 2 }, "/*\t\t// this ends the line\r\n\r\nblue");
    assert_eq!(result, Err(String::from("Unterminated /*-style comment. Started on line 3, column 2.")));
}
#[test]
fn skippables_mlc_eof1() {
    let result = skip_skippables(&Scanner { start_idx: 0, line: 3, column: 2 }, "/*");
    assert_eq!(result, Err(String::from("Unterminated /*-style comment. Started on line 3, column 2.")));
}
#[test]
fn skippables_mlc_eof2() {
    let result = skip_skippables(&Scanner { start_idx: 0, line: 3, column: 2 }, "/***");
    assert_eq!(result, Err(String::from("Unterminated /*-style comment. Started on line 3, column 2.")));
}
#[test]
fn skippables_mlc_eof3() {
    let result = skip_skippables(&Scanner { start_idx: 0, line: 3, column: 2 }, "/***\n");
    assert_eq!(result, Err(String::from("Unterminated /*-style comment. Started on line 3, column 2.")));
}
#[test]
fn skippables_mlc_eof4() {
    let result = skip_skippables(&Scanner { start_idx: 0, line: 3, column: 2 }, "/***\r\n");
    assert_eq!(result, Err(String::from("Unterminated /*-style comment. Started on line 3, column 2.")));
}
#[test]
fn new_simple() {
    let result = Scanner::new();
    assert_eq!(result, Scanner { start_idx: 0, line: 1, column: 1 });
}
#[test]
fn hex_four_digits_01() {
    let result = hex_four_digits(&Scanner::new(), "");
    assert_eq!(result, None);
}
#[test]
fn hex_four_digits_02() {
    let result = hex_four_digits(&Scanner::new(), "a");
    assert_eq!(result, None);
}
#[test]
fn hex_four_digits_03() {
    let result = hex_four_digits(&Scanner::new(), "3f");
    assert_eq!(result, None);
}
#[test]
fn hex_four_digits_04() {
    let result = hex_four_digits(&Scanner::new(), "3fA");
    assert_eq!(result, None);
}
#[test]
fn hex_four_digits_05() {
    let result = hex_four_digits(&Scanner::new(), "3fAU");
    assert_eq!(result, None);
}
#[test]
fn hex_four_digits_06() {
    let result = hex_four_digits(&Scanner::new(), "3fU");
    assert_eq!(result, None);
}
#[test]
fn hex_four_digits_07() {
    let result = hex_four_digits(&Scanner::new(), "aU");
    assert_eq!(result, None);
}
#[test]
fn hex_four_digits_08() {
    let result = hex_four_digits(&Scanner::new(), "U");
    assert_eq!(result, None);
}
#[test]
fn hex_four_digits_09() {
    let result = hex_four_digits(&Scanner::new(), "0089");
    assert_eq!(result, Some(Scanner { line: 1, column: 5, start_idx: 4 }));
}
#[test]
fn hex_four_digits_10() {
    let result = hex_four_digits(&Scanner::new(), "00896661");
    assert_eq!(result, Some(Scanner { line: 1, column: 5, start_idx: 4 }));
}
#[test]
fn is_hex_digit_01() {
    for ch in "0123456789abcdefABCDEF".chars() {
        assert_eq!(is_hex_digit(ch), true);
    }
    for ch in "!#$%&()*+,-./:;<=>?@GHIJKLMNOP^_`ghijklmnop".chars() {
        assert_eq!(is_hex_digit(ch), false);
    }
}
#[test]
fn code_point_01() {
    let result = code_point(&Scanner::new(), "");
    assert_eq!(result, None);
}
#[test]
fn code_point_02() {
    let result = code_point(&Scanner::new(), "5");
    assert_eq!(result, Some(Scanner { line: 1, column: 2, start_idx: 1 }));
}
#[test]
fn code_point_03() {
    let result = code_point(&Scanner::new(), "ffff");
    assert_eq!(result, Some(Scanner { line: 1, column: 5, start_idx: 4 }));
}
#[test]
fn code_point_04() {
    let result = code_point(&Scanner::new(), "fffff");
    assert_eq!(result, Some(Scanner { line: 1, column: 6, start_idx: 5 }));
}
#[test]
fn code_point_05() {
    let result = code_point(&Scanner::new(), "10ffff");
    assert_eq!(result, Some(Scanner { line: 1, column: 7, start_idx: 6 }));
}
#[test]
fn code_point_06() {
    let result = code_point(&Scanner::new(), "110000");
    assert_eq!(result, None);
}
#[test]
fn code_point_07() {
    let result = code_point(&Scanner::new(), "0000000098");
    assert_eq!(result, Some(Scanner { line: 1, column: 11, start_idx: 10 }));
}
#[test]
fn code_point_08() {
    let result = code_point(&Scanner::new(), "000000000000000000000000000000000000000098");
    assert_eq!(result, Some(Scanner { line: 1, column: 43, start_idx: 42 }));
}
#[test]
fn code_point_09() {
    let result = code_point(&Scanner::new(), "A000000000000000000000000000000000000000098");
    assert_eq!(result, None);
}
#[test]
fn unicode_escape_sequence_01() {
    for s in &["", "g", "u", "ug", "u0", "u0g", "u00", "u00g", "u000", "u000g", "u{", "u{0g", "u{}", "u{1234567890}"] {
        assert_eq!(unicode_escape_sequence(&Scanner::new(), s), None);
    }
}
#[test]
fn unicode_escape_sequence_02() {
    assert_eq!(unicode_escape_sequence(&Scanner::new(), "u0067"), Some(Scanner { line: 1, column: 6, start_idx: 5 }));
}
#[test]
fn unicode_escape_sequence_03() {
    assert_eq!(unicode_escape_sequence(&Scanner::new(), "u{0067}"), Some(Scanner { line: 1, column: 8, start_idx: 7 }));
}
#[test]
fn identifier_start_01() {
    for s in &["", "9", "9abc", "\\", "\\u{}", "\\u89zzz"] {
        assert_eq!(identifier_start(&Scanner::new(), s), Ok(None));
    }
}
#[test]
fn identifier_start_02() {
    for s in &["$name", "_name", "Gname"] {
        assert_eq!(identifier_start(&Scanner::new(), s), Ok(Some(Scanner { line: 1, column: 2, start_idx: 1 })))
    }
    assert_eq!(identifier_start(&Scanner::new(), "ꘐblue"), Ok(Some(Scanner { line: 1, column: 2, start_idx: 3 })));
}
#[test]
fn identifier_start_03() {
    assert_eq!(identifier_start(&Scanner::new(), "\\u0061blue"), Ok(Some(Scanner { line: 1, column: 7, start_idx: 6 })));
}
#[test]
fn identifier_start_04() {
    assert_eq!(identifier_start(&Scanner::new(), "\\u0024blue"), Ok(Some(Scanner { line: 1, column: 7, start_idx: 6 })));
}
#[test]
fn identifier_start_05() {
    assert_eq!(identifier_start(&Scanner::new(), "\\u005fblue"), Ok(Some(Scanner { line: 1, column: 7, start_idx: 6 })));
}
#[test]
fn identifier_start_06() {
    assert_eq!(identifier_start(&Scanner::new(), "\\u0095blue"), Err(String::from("1:1: Invalid Identifier Start Character '\\u{95}'")));
}

#[test]
fn identifier_name_01() {
    let (tok, scanner) = identifier_name(&Scanner::new(), "A\\u{0}ll").unwrap();
    assert_eq!(tok, Token::Error(String::from("1:2: Invalid Identifier Continuation Character '\\u{0}'")));
    assert_eq!(scanner, Scanner { line: 1, column: 2, start_idx: 1 });
}
#[test]
fn identifier_name_02() {
    let (tok, scanner) = identifier_name(&Scanner::new(), "\\u{0}ll").unwrap();
    assert_eq!(tok, Token::Error(String::from("1:1: Invalid Identifier Start Character '\\u{0}'")));
    assert_eq!(scanner, Scanner { line: 1, column: 1, start_idx: 0 });
}
#[test]
fn identifier_name_03() {
    let (tok, scanner) = identifier_name(&Scanner::new(), "ll\\").unwrap();
    assert_eq!(tok, Token::Identifier(IdentifierData { string_value: JSString::from("ll"), keyword_id: None, line: 1, column: 1 }));
    assert_eq!(scanner, Scanner { line: 1, column: 3, start_idx: 2 });
}
#[test]
fn identifier_name_04() {
    let (tok, scanner) = identifier_name(&Scanner::new(), "ll ").unwrap();
    assert_eq!(tok, Token::Identifier(IdentifierData { string_value: JSString::from("ll"), keyword_id: None, line: 1, column: 1 }));
    assert_eq!(scanner, Scanner { line: 1, column: 3, start_idx: 2 });
}

#[test]
fn radix_digits_01() {
    assert_eq!(radix_digits(&Scanner::new(), "43", false, is_digit), Some(Scanner { line: 1, column: 3, start_idx: 2 }));
}
#[test]
fn radix_digits_02() {
    assert_eq!(radix_digits(&Scanner::new(), "4_3", false, is_digit), Some(Scanner { line: 1, column: 2, start_idx: 1 }));
}
#[test]
fn radix_digits_03() {
    assert_eq!(radix_digits(&Scanner::new(), "43_", false, is_digit), Some(Scanner { line: 1, column: 3, start_idx: 2 }));
}
#[test]
fn radix_digits_04() {
    assert_eq!(radix_digits(&Scanner::new(), "_43", false, is_digit), None);
}
#[test]
fn radix_digits_05() {
    assert_eq!(radix_digits(&Scanner::new(), "43", true, is_digit), Some(Scanner { line: 1, column: 3, start_idx: 2 }));
}
#[test]
fn radix_digits_06() {
    assert_eq!(radix_digits(&Scanner::new(), "4_3", true, is_digit), Some(Scanner { line: 1, column: 4, start_idx: 3 }));
}
#[test]
fn radix_digits_07() {
    assert_eq!(radix_digits(&Scanner::new(), "43_", true, is_digit), Some(Scanner { line: 1, column: 3, start_idx: 2 }));
}
#[test]
fn radix_digits_08() {
    assert_eq!(radix_digits(&Scanner::new(), "_43", true, is_digit), None);
}
#[test]
fn radix_digits_09() {
    assert_eq!(radix_digits(&Scanner::new(), "4__3", true, is_digit), Some(Scanner { line: 1, column: 2, start_idx: 1 }));
}
#[test]
fn radix_digits_10() {
    assert_eq!(radix_digits(&Scanner::new(), "4__3", false, is_digit), Some(Scanner { line: 1, column: 2, start_idx: 1 }));
}
#[test]
fn radix_digits_11() {
    assert_eq!(radix_digits(&Scanner::new(), "xyz", false, is_digit), None);
}
#[test]
fn decimal_integer_empty() {
    assert_eq!(decimal_integer_literal(&Scanner::new(), ""), None)
}
#[test]
fn decimal_integer_0() {
    assert_eq!(decimal_integer_literal(&Scanner::new(), "0"), Some(Scanner { line: 1, column: 2, start_idx: 1 }))
}
#[test]
fn decimal_integer_4() {
    assert_eq!(decimal_integer_literal(&Scanner::new(), "4"), Some(Scanner { line: 1, column: 2, start_idx: 1 }))
}
#[test]
fn decimal_integer_4_3() {
    assert_eq!(decimal_integer_literal(&Scanner::new(), "4_3"), Some(Scanner { line: 1, column: 4, start_idx: 3 }))
}
#[test]
fn decimal_integer_43() {
    assert_eq!(decimal_integer_literal(&Scanner::new(), "43"), Some(Scanner { line: 1, column: 3, start_idx: 2 }))
}
#[test]
fn decimal_integer_56_() {
    assert_eq!(decimal_integer_literal(&Scanner::new(), "56_"), Some(Scanner { line: 1, column: 3, start_idx: 2 }))
}
#[test]
fn non_decimal_integer_literal_01() {
    assert_eq!(non_decimal_integer_literal(&Scanner::new(), "0x10", true), Some((NumberStyle::Hex, Scanner { line: 1, column: 5, start_idx: 4 })))
}
#[test]
fn numeric_literal_01() {
    assert_eq!(numeric_literal(&Scanner::new(), "0x10..."), Some((Token::Number(16.0), Scanner { line: 1, column: 5, start_idx: 4 })))
}
#[test]
fn numeric_literal_02() {
    assert_eq!(numeric_literal(&Scanner::new(), ".25"), Some((Token::Number(0.25), Scanner { line: 1, column: 4, start_idx: 3 })))
}
#[test]
fn numeric_literal_03() {
    assert_eq!(numeric_literal(&Scanner::new(), "0xabcdef"), Some((Token::Number(11259375.0), Scanner { line: 1, column: 9, start_idx: 8 })))
}
#[test]
fn numeric_literal_04() {
    assert_eq!(numeric_literal(&Scanner::new(), "0xFEDCBA"), Some((Token::Number(16702650.0), Scanner { line: 1, column: 9, start_idx: 8 })))
}
#[test]
fn numeric_literal_05() {
    assert!(numeric_literal(&Scanner::new(), "3in").is_none());
}
#[test]
fn numeric_literal_06() {
    let clusters = [392, 135, 832, 991, 123, 713, 820, 731, 861];
    let expected = clusters.iter().fold(BigInt::zero(), |acc, x| acc * 1000 + x);
    let result = numeric_literal(&Scanner::new(), "392_135_832_991_123_713_820_731_861n");

    assert_eq!(result, Some((Token::BigInt(expected), Scanner { line: 1, column: 37, start_idx: 36 })));
}
#[test]
fn numeric_literal_07() {
    let result = numeric_literal(&Scanner::new(), "0b1_1100_0111n");
    assert_eq!(result, Some((Token::BigInt(BigInt::zero() + 0x1c7), Scanner { line: 1, column: 15, start_idx: 14 })));
}
#[test]
fn numeric_literal_08() {
    let result = numeric_literal(&Scanner::new(), "0o3_4576_1000n");
    assert_eq!(result, Some((Token::BigInt(BigInt::zero() + 0x0397e200), Scanner { line: 1, column: 15, start_idx: 14 })));
}
#[test]
fn numeric_literal_09() {
    let result = numeric_literal(&Scanner::new(), "0x4576_1000n");
    assert_eq!(result, Some((Token::BigInt(BigInt::zero() + 0x45761000), Scanner { line: 1, column: 13, start_idx: 12 })));
}
#[test]
fn numeric_literal_10() {
    let result = numeric_literal(&Scanner::new(), "0b1010_1111_0010_0110");
    assert_eq!(result, Some((Token::Number(44838.0), Scanner { line: 1, column: 22, start_idx: 21 })));
}
#[test]
fn numeric_literal_11() {
    let result = numeric_literal(&Scanner::new(), "0o7773153152");
    assert_eq!(result, Some((Token::Number(1072485994.0), Scanner { line: 1, column: 13, start_idx: 12 })));
}

#[test]
fn bad_hex_char() {
    assert_eq!(HexChar::try_from('&'), Err("HexChar can only be used with hexidecimal digits!"));
}
#[test]
fn hex_char_debug_fmt() {
    let hc = HexChar('F');
    let result = format!("{:?}", hc);
    assert_eq!(result, "HexChar('F')");
}
#[test]
fn hex_char_partial_eq() {
    let hc1 = HexChar('1');
    let hc2 = HexChar('9');
    let hc3 = HexChar('1');
    assert_eq!(hc1, hc3);
    assert_ne!(hc1, hc2);
}
#[test]
fn hex_char_ne() {
    let hc1 = HexChar('1');
    let hc2 = HexChar('9');
    let hc3 = HexChar('1');

    assert_eq!(hc1 != hc2, true);
    assert_eq!(hc1 != hc3, false);
}
#[test]
fn hex_integer_literal_01() {
    let result = hex_integer_literal(&Scanner::new(), "0x00", false).unwrap();
    assert_eq!(result, Scanner { line: 1, column: 5, start_idx: 4 });
}
#[test]
fn hex_integer_literal_02() {
    let result = hex_integer_literal(&Scanner::new(), "0X00", false).unwrap();
    assert_eq!(result, Scanner { line: 1, column: 5, start_idx: 4 });
}
#[test]
fn hex_integer_literal_03() {
    let result = hex_integer_literal(&Scanner::new(), "blue", false);
    assert!(result.is_none());
}
#[test]
fn hex_integer_literal_04() {
    let result = hex_integer_literal(&Scanner::new(), "0blue", false);
    assert!(result.is_none());
}
#[test]
fn hex_integer_literal_05() {
    let result = hex_integer_literal(&Scanner::new(), "0xlue", false);
    assert!(result.is_none());
}
#[test]
fn scan_numeric() {
    let result = scan_token(&Scanner::new(), ".25", ScanGoal::InputElementRegExp);
    assert_eq!(result, (Token::Number(0.25), Scanner { line: 1, column: 4, start_idx: 3 }));
}
#[test]
fn scan_token_id_01() {
    let result = scan_token(&Scanner::new(), "\\u004Abc\\u004a\\u{1235}", ScanGoal::InputElementRegExp);
    assert_eq!(
        result,
        (Token::Identifier(IdentifierData { string_value: JSString::from("JbcJ\u{1235}"), keyword_id: None, line: 1, column: 1 }), Scanner { line: 1, column: 23, start_idx: 22 })
    );
}
fn keyword_test_helper(inp: &str, expected: Option<Keyword>) {
    let result = scan_token(&Scanner::new(), inp, ScanGoal::InputElementRegExp);
    assert_eq!(
        result,
        (
            Token::Identifier(IdentifierData { string_value: JSString::from(inp), keyword_id: expected, line: 1, column: 1 }),
            Scanner { line: 1, column: inp.len() as u32 + 1, start_idx: inp.len() }
        )
    );
}
#[test]
fn scan_token_keywords() {
    keyword_test_helper("await", Some(Keyword::Await));
    keyword_test_helper("break", Some(Keyword::Break));
    keyword_test_helper("case", Some(Keyword::Case));
    keyword_test_helper("catch", Some(Keyword::Catch));
    keyword_test_helper("class", Some(Keyword::Class));
    keyword_test_helper("const", Some(Keyword::Const));
    keyword_test_helper("continue", Some(Keyword::Continue));
    keyword_test_helper("debugger", Some(Keyword::Debugger));
    keyword_test_helper("default", Some(Keyword::Default));
    keyword_test_helper("delete", Some(Keyword::Delete));
    keyword_test_helper("do", Some(Keyword::Do));
    keyword_test_helper("else", Some(Keyword::Else));
    keyword_test_helper("enum", Some(Keyword::Enum));
    keyword_test_helper("export", Some(Keyword::Export));
    keyword_test_helper("extends", Some(Keyword::Extends));
    keyword_test_helper("false", Some(Keyword::False));
    keyword_test_helper("finally", Some(Keyword::Finally));
    keyword_test_helper("for", Some(Keyword::For));
    keyword_test_helper("function", Some(Keyword::Function));
    keyword_test_helper("if", Some(Keyword::If));
    keyword_test_helper("import", Some(Keyword::Import));
    keyword_test_helper("in", Some(Keyword::In));
    keyword_test_helper("instanceof", Some(Keyword::Instanceof));
    keyword_test_helper("new", Some(Keyword::New));
    keyword_test_helper("null", Some(Keyword::Null));
    keyword_test_helper("return", Some(Keyword::Return));
    keyword_test_helper("super", Some(Keyword::Super));
    keyword_test_helper("switch", Some(Keyword::Switch));
    keyword_test_helper("this", Some(Keyword::This));
    keyword_test_helper("throw", Some(Keyword::Throw));
    keyword_test_helper("true", Some(Keyword::True));
    keyword_test_helper("try", Some(Keyword::Try));
    keyword_test_helper("typeof", Some(Keyword::Typeof));
    keyword_test_helper("var", Some(Keyword::Var));
    keyword_test_helper("void", Some(Keyword::Void));
    keyword_test_helper("while", Some(Keyword::While));
    keyword_test_helper("with", Some(Keyword::With));
    keyword_test_helper("yield", Some(Keyword::Yield));
    keyword_test_helper("let", Some(Keyword::Let));
    keyword_test_helper("static", Some(Keyword::Static));
    keyword_test_helper("implements", Some(Keyword::Implements));
    keyword_test_helper("interface", Some(Keyword::Interface));
    keyword_test_helper("package", Some(Keyword::Package));
    keyword_test_helper("private", Some(Keyword::Private));
    keyword_test_helper("protected", Some(Keyword::Protected));
    keyword_test_helper("public", Some(Keyword::Public));
    keyword_test_helper("as", Some(Keyword::As));
    keyword_test_helper("async", Some(Keyword::Async));
    keyword_test_helper("from", Some(Keyword::From));
    keyword_test_helper("get", Some(Keyword::Get));
    keyword_test_helper("of", Some(Keyword::Of));
    keyword_test_helper("set", Some(Keyword::Set));
    keyword_test_helper("target", Some(Keyword::Target));
    keyword_test_helper("meta", Some(Keyword::Meta));
    // These are here to get code-coverage for a bunch of "no match" cases.
    keyword_test_helper("asphalt", None);
    keyword_test_helper("about", None);
    keyword_test_helper("cart", None);
    keyword_test_helper("cone", None);
    keyword_test_helper("cope", None);
    keyword_test_helper("central", None);
    keyword_test_helper("detail", None);
    keyword_test_helper("daily", None);
    keyword_test_helper("e1", None);
    keyword_test_helper("exhale", None);
    keyword_test_helper("felt", None);
    keyword_test_helper("i99", None);
    keyword_test_helper("impulse", None);
    keyword_test_helper("imbalance", None);
    keyword_test_helper("inline", None);
    keyword_test_helper("natural", None);
    keyword_test_helper("prattle", None);
    keyword_test_helper("pebble", None);
    keyword_test_helper("saturate", None);
    keyword_test_helper("tan", None);
    keyword_test_helper("that", None);
    keyword_test_helper("tree", None);
    keyword_test_helper("test", None);
    keyword_test_helper("very", None);
    keyword_test_helper("werewolf", None);
}
#[test]
fn optional_chaining_test_01() {
    let result = scan_token(&Scanner::new(), "?.", ScanGoal::InputElementRegExp);
    assert_eq!(result, (Token::Punctuator(Punctuator::QDot), Scanner { line: 1, column: 3, start_idx: 2 }));
}
#[test]
fn optional_chaining_test_02() {
    let result = scan_token(&Scanner::new(), "?.P", ScanGoal::InputElementRegExp);
    assert_eq!(result, (Token::Punctuator(Punctuator::QDot), Scanner { line: 1, column: 3, start_idx: 2 }));
}
#[test]
fn optional_chaining_test_03() {
    let result = scan_token(&Scanner::new(), "?.999", ScanGoal::InputElementRegExp);
    assert_eq!(result, (Token::Punctuator(Punctuator::Question), Scanner { line: 1, column: 2, start_idx: 1 }));
}
#[test]
fn optional_chaining_test_04() {
    let result = scan_token(&Scanner::new(), "?mulberry", ScanGoal::InputElementRegExp);
    assert_eq!(result, (Token::Punctuator(Punctuator::Question), Scanner { line: 1, column: 2, start_idx: 1 }));
}
fn punct_check(inp: &str, tok: Token) {
    let result = scan_token(&Scanner::new(), inp, ScanGoal::InputElementRegExp);
    assert_eq!(result, (tok, Scanner { line: 1, column: inp.chars().count() as u32 + 1, start_idx: inp.len() }));
}
#[test]
fn punctuator_validiation() {
    punct_check("{", Token::Punctuator(Punctuator::LeftBrace));
    punct_check("(", Token::Punctuator(Punctuator::LeftParen));
    punct_check(")", Token::Punctuator(Punctuator::RightParen));
    punct_check("[", Token::Punctuator(Punctuator::LeftBracket));
    punct_check("]", Token::Punctuator(Punctuator::RightBracket));
    punct_check(".", Token::Punctuator(Punctuator::Dot));
    punct_check(";", Token::Punctuator(Punctuator::Semicolon));
    punct_check(",", Token::Punctuator(Punctuator::Comma));
    punct_check("<", Token::Punctuator(Punctuator::Lt));
    punct_check(">", Token::Punctuator(Punctuator::Gt));
    punct_check("=", Token::Punctuator(Punctuator::Eq));
    punct_check("!", Token::Punctuator(Punctuator::Bang));
    punct_check("+", Token::Punctuator(Punctuator::Plus));
    punct_check("-", Token::Punctuator(Punctuator::Minus));
    punct_check("*", Token::Punctuator(Punctuator::Star));
    punct_check("&", Token::Punctuator(Punctuator::Amp));
    punct_check("|", Token::Punctuator(Punctuator::Pipe));
    punct_check("^", Token::Punctuator(Punctuator::Caret));
    punct_check("~", Token::Punctuator(Punctuator::Tilde));
    punct_check("?", Token::Punctuator(Punctuator::Question));
    punct_check(":", Token::Punctuator(Punctuator::Colon));
    punct_check("%", Token::Punctuator(Punctuator::Percent));
    punct_check("...", Token::Punctuator(Punctuator::Ellipsis));
    punct_check("<=", Token::Punctuator(Punctuator::LtEq));
    punct_check("<<=", Token::Punctuator(Punctuator::LtLtEq));
    punct_check("<<", Token::Punctuator(Punctuator::LtLt));
    punct_check(">=", Token::Punctuator(Punctuator::GtEq));
    punct_check(">>", Token::Punctuator(Punctuator::GtGt));
    punct_check(">>>", Token::Punctuator(Punctuator::GtGtGt));
    punct_check(">>>=", Token::Punctuator(Punctuator::GtGtGtEq));
    punct_check(">>=", Token::Punctuator(Punctuator::GtGtEq));
    punct_check("===", Token::Punctuator(Punctuator::EqEqEq));
    punct_check("==", Token::Punctuator(Punctuator::EqEq));
    punct_check("=>", Token::Punctuator(Punctuator::EqGt));
    punct_check("!=", Token::Punctuator(Punctuator::BangEq));
    punct_check("!==", Token::Punctuator(Punctuator::BangEqEq));
    punct_check("+=", Token::Punctuator(Punctuator::PlusEq));
    punct_check("++", Token::Punctuator(Punctuator::PlusPlus));
    punct_check("-=", Token::Punctuator(Punctuator::MinusEq));
    punct_check("--", Token::Punctuator(Punctuator::MinusMinus));
    punct_check("**=", Token::Punctuator(Punctuator::StarStarEq));
    punct_check("**", Token::Punctuator(Punctuator::StarStar));
    punct_check("*=", Token::Punctuator(Punctuator::StarEq));
    punct_check("%=", Token::Punctuator(Punctuator::PercentEq));
    punct_check("&&=", Token::Punctuator(Punctuator::AmpAmpEq));
    punct_check("&&", Token::Punctuator(Punctuator::AmpAmp));
    punct_check("&=", Token::Punctuator(Punctuator::AmpEq));
    punct_check("||=", Token::Punctuator(Punctuator::PipePipeEq));
    punct_check("||", Token::Punctuator(Punctuator::PipePipe));
    punct_check("|=", Token::Punctuator(Punctuator::PipeEq));
    punct_check("^=", Token::Punctuator(Punctuator::CaretEq));
    punct_check("??=", Token::Punctuator(Punctuator::QQEq));
    punct_check("??", Token::Punctuator(Punctuator::QQ));
}

fn punct_chk2(inp: &str, tok: Token, consumed: u32) {
    let result = scan_token(&Scanner::new(), inp, ScanGoal::InputElementRegExp);
    assert_eq!(result, (tok, Scanner { line: 1, column: consumed + 1, start_idx: consumed as usize }));
}

#[test]
fn punctuator_shortscans() {
    punct_chk2("..A", Token::Punctuator(Punctuator::Dot), 1);
}
#[test]
fn punctuator_nomatch() {
    let result = scan_token(&Scanner::new(), "@", ScanGoal::InputElementRegExp);
    let (token, scanner) = result;
    assert!(matches!(token, Token::Error(_)));
    assert_eq!(scanner, Scanner { line: 1, column: 1, start_idx: 0 });
}
#[test]
fn punctuator_display() {
    let pairs = vec![
        (Punctuator::LeftParen, "("),
        (Punctuator::RightParen, ")"),
        (Punctuator::LeftBrace, "{"),
        (Punctuator::RightBrace, "}"),
        (Punctuator::Dot, "."),
        (Punctuator::Ellipsis, "..."),
        (Punctuator::Comma, ","),
        (Punctuator::Semicolon, ";"),
        (Punctuator::LeftBracket, "["),
        (Punctuator::RightBracket, "]"),
        (Punctuator::Colon, ":"),
        (Punctuator::Tilde, "~"),
        (Punctuator::Minus, "-"),
        (Punctuator::MinusMinus, "--"),
        (Punctuator::MinusEq, "-="),
        (Punctuator::Plus, "+"),
        (Punctuator::PlusPlus, "++"),
        (Punctuator::PlusEq, "+="),
        (Punctuator::Slash, "/"),
        (Punctuator::SlashEq, "/="),
        (Punctuator::Star, "*"),
        (Punctuator::StarEq, "*="),
        (Punctuator::StarStar, "**"),
        (Punctuator::StarStarEq, "**="),
        (Punctuator::Amp, "&"),
        (Punctuator::AmpAmp, "&&"),
        (Punctuator::AmpAmpEq, "&&="),
        (Punctuator::AmpEq, "&="),
        (Punctuator::Lt, "<"),
        (Punctuator::LtEq, "<="),
        (Punctuator::LtLt, "<<"),
        (Punctuator::LtLtEq, "<<="),
        (Punctuator::Gt, ">"),
        (Punctuator::GtEq, ">="),
        (Punctuator::GtGt, ">>"),
        (Punctuator::GtGtGt, ">>>"),
        (Punctuator::GtGtEq, ">>="),
        (Punctuator::GtGtGtEq, ">>>="),
        (Punctuator::Eq, "="),
        (Punctuator::EqGt, "=>"),
        (Punctuator::EqEq, "=="),
        (Punctuator::EqEqEq, "==="),
        (Punctuator::Bang, "!"),
        (Punctuator::BangEq, "!="),
        (Punctuator::BangEqEq, "!=="),
        (Punctuator::Percent, "%"),
        (Punctuator::PercentEq, "%="),
        (Punctuator::Pipe, "|"),
        (Punctuator::PipePipe, "||"),
        (Punctuator::PipePipeEq, "||="),
        (Punctuator::PipeEq, "|="),
        (Punctuator::Caret, "^"),
        (Punctuator::CaretEq, "^="),
        (Punctuator::Question, "?"),
        (Punctuator::QDot, "?."),
        (Punctuator::QQ, "??"),
        (Punctuator::QQEq, "??="),
    ];
    for (p, display) in pairs {
        assert_eq!(format!("{}", p), display);
    }
}
#[test]
fn punctuator_clone() {
    let p = Punctuator::PipePipe;
    let p2 = p.clone();
    assert_eq!(p, p2);
}
#[test]
fn punctuator_debug() {
    assert_ne!(format!("{:?}", Punctuator::Semicolon), "");
}

#[test]
fn signed_integer_01() {
    let result = signed_integer(&Scanner::new(), "blue", true);
    assert_eq!(result, None);
}
#[test]
fn signed_integer_02() {
    let result = signed_integer(&Scanner::new(), "+10_9", false);
    assert_eq!(result, Some(Scanner { line: 1, column: 4, start_idx: 3 }));
}
#[test]
fn signed_integer_03() {
    let result = signed_integer(&Scanner::new(), "-3_3", true);
    assert_eq!(result, Some(Scanner { line: 1, column: 5, start_idx: 4 }));
}
#[test]
fn signed_integer_04() {
    let result = signed_integer(&Scanner::new(), "899", false);
    assert_eq!(result, Some(Scanner { line: 1, column: 4, start_idx: 3 }));
}

fn decimal_literal_helper(text: &str, count: u32) {
    let result = decimal_literal(&Scanner::new(), text);
    assert_eq!(result, Some(Scanner { line: 1, column: count + 1, start_idx: count as usize }));
}
#[test]
fn decimal_literal_01() {
    decimal_literal_helper("8_9.", 4);
}
#[test]
fn decimal_literal_02() {
    decimal_literal_helper("8_9.2_3", 7);
}
#[test]
fn decimal_literal_03() {
    decimal_literal_helper("8_9.e5_0", 8);
}
#[test]
fn decimal_literal_04() {
    decimal_literal_helper("8_9.2_3e5_0", 11);
}
#[test]
fn decimal_literal_05() {
    decimal_literal_helper(".2_3", 4);
}
#[test]
fn decimal_literal_06() {
    decimal_literal_helper(".2_3e5_0", 8);
}
#[test]
fn decimal_literal_07() {
    decimal_literal_helper("8_9", 3);
}
#[test]
fn decimal_literal_08() {
    decimal_literal_helper("8_9e5_0", 7);
}
#[test]
fn decimal_literal_09() {
    assert!(decimal_literal(&Scanner::new(), "blue").is_none());
}
#[test]
fn decimal_literal_10() {
    assert!(decimal_literal(&Scanner::new(), ".crazy").is_none());
}

fn nzd_helper(text: &str) {
    assert_eq!(non_zero_digit(&Scanner::new(), text), Some(Scanner { line: 1, column: 2, start_idx: 1 }));
}
#[test]
fn non_zero_digit_successes() {
    nzd_helper("1");
    nzd_helper("2");
    nzd_helper("3");
    nzd_helper("4");
    nzd_helper("5");
    nzd_helper("6");
    nzd_helper("7");
    nzd_helper("8");
    nzd_helper("9");
}
#[test]
fn non_zero_digit_zero() {
    assert!(non_zero_digit(&Scanner::new(), "0").is_none());
}
#[test]
fn non_zero_digit_bad() {
    assert!(non_zero_digit(&Scanner::new(), "Q").is_none());
}
#[test]
fn non_zero_digit_empty() {
    assert!(non_zero_digit(&Scanner::new(), "").is_none());
}

fn dbil_helper(text: &str) {
    assert_eq!(decimal_big_integer_literal(&Scanner::new(), text), Some(Scanner { line: 1, column: text.len() as u32 + 1, start_idx: text.len() }));
}
#[test]
fn decimal_big_integer_literal_success() {
    dbil_helper("0n");
    dbil_helper("3n");
    dbil_helper("2327n");
    dbil_helper("2_327n");
    dbil_helper("23_27n");
    dbil_helper("232_7n");
}
#[test]
fn decimal_big_integer_literal_failure() {
    let helper = |text| {
        assert!(decimal_big_integer_literal(&Scanner::new(), text).is_none());
    };
    helper("");
    helper("0");
    helper("0x");
    helper("9xn");
    helper("99xn");
    helper("6_8");
    helper("6_9xn");
    helper("4_n");
}

#[test]
fn bigify_binary() {
    assert_eq!(bigify(NumberStyle::Binary), NumberStyle::BigBinary);
}
#[test]
fn bigify_octal() {
    assert_eq!(bigify(NumberStyle::Octal), NumberStyle::BigOctal);
}
#[test]
fn bigify_hex() {
    assert_eq!(bigify(NumberStyle::Hex), NumberStyle::BigHex);
}
#[test]
fn bigify_decimal() {
    assert_eq!(bigify(NumberStyle::Decimal), NumberStyle::BigDecimal);
}
#[test]
fn bigify_other() {
    assert_eq!(bigify(NumberStyle::BigBinary), NumberStyle::BigBinary);
    assert_eq!(bigify(NumberStyle::BigDecimal), NumberStyle::BigDecimal);
    assert_eq!(bigify(NumberStyle::BigOctal), NumberStyle::BigOctal);
    assert_eq!(bigify(NumberStyle::BigHex), NumberStyle::BigHex);
}

#[test]
fn int_to_number_test() {
    assert_eq!(int_to_number("0", 10), 0.0);
    assert_eq!(int_to_number("10000", 16), 65536.0);
    assert_eq!(int_to_number("9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999", 10), f64::INFINITY);
}

#[test]
fn escape_sequence_test() {
    let singles = ['\'', '"', '\\', 'b', 'f', 'n', 'r', 't', 'v', 'a', 'Q', '😊'];
    for ch in singles.iter() {
        assert_eq!(escape_sequence(&Scanner::new(), &String::from(*ch)), Some(Scanner { line: 1, column: 2, start_idx: ch.len_utf8() }));
    }
    assert_eq!(escape_sequence(&Scanner::new(), "0--"), Some(Scanner { line: 1, column: 2, start_idx: 1 }));
    assert_eq!(escape_sequence(&Scanner::new(), "x7b7b"), Some(Scanner { line: 1, column: 4, start_idx: 3 }));
    assert_eq!(escape_sequence(&Scanner::new(), "u3333xxx"), Some(Scanner { line: 1, column: 6, start_idx: 5 }));
    assert!(escape_sequence(&Scanner::new(), "09").is_none());
    assert!(escape_sequence(&Scanner::new(), "xpot").is_none());
    assert!(escape_sequence(&Scanner::new(), "xfoot").is_none());
    assert!(escape_sequence(&Scanner::new(), "8").is_none());
}

#[test]
fn line_terminator_sequence_test() {
    let valid = ["\r", "\r\n", "\n", "\u{2028}", "\u{2029}"];
    for lt in valid.iter() {
        let result = line_terminator_sequence(&Scanner::new(), *lt);
        assert_eq!(result, Some(Scanner { line: 2, column: 1, start_idx: lt.len() }));
    }
    assert!(line_terminator_sequence(&Scanner::new(), "blue").is_none());
    assert_eq!(line_terminator_sequence(&Scanner::new(), "\rblue"), Some(Scanner { line: 2, column: 1, start_idx: 1 }));
}

#[test]
fn string_characters_test() {
    let result = string_characters(&Scanner::new(), "a\\n\\u{2029}'", '\'');
    assert_eq!(result, Some(Scanner { line: 1, column: 12, start_idx: 11 }));

    assert!(string_characters(&Scanner::new(), "\\u{3", '\'').is_none());
    assert!(string_characters(&Scanner::new(), "", '\'').is_none());
    assert_eq!(string_characters(&Scanner::new(), "1\\\n2\\\n3'\n", '\''), Some(Scanner { line: 3, column: 2, start_idx: 7 }));
}

#[test]
fn literal_string_value_test() {
    let (s, has_escapes) = literal_string_value("a\\a\\b\\t\\n\\v\\f\\r\\'\\\"\\\\\\x66\\u{211c}\\u211d\\\n\\\u{2028}\\\u{2029}\\\r\\\n\\\r\n\\0");
    assert_eq!(s, "aa\u{8}\t\n\u{b}\u{c}\r'\"\\f\u{211c}\u{211d}\u{0}");
    assert!(has_escapes);
}

#[test]
fn string_literal_test() {
    assert_eq!(string_literal(&Scanner::new(), "not_a_string"), None);
    assert_eq!(
        string_literal(&Scanner::new(), "''"),
        Some((Token::String(StringToken { value: JSString::from(""), delimiter: StringDelimiter::Single, raw: None }), Scanner { line: 1, column: 3, start_idx: 2 }))
    );
    assert_eq!(
        string_literal(&Scanner::new(), "\"\""),
        Some((Token::String(StringToken { value: JSString::from(""), delimiter: StringDelimiter::Double, raw: None }), Scanner { line: 1, column: 3, start_idx: 2 }))
    );
    assert_eq!(
        string_literal(&Scanner::new(), "'abcd'"),
        Some((Token::String(StringToken { value: JSString::from("abcd"), delimiter: StringDelimiter::Single, raw: None }), Scanner { line: 1, column: 7, start_idx: 6 }))
    );
    assert_eq!(
        string_literal(&Scanner::new(), "\"abcd\""),
        Some((Token::String(StringToken { value: JSString::from("abcd"), delimiter: StringDelimiter::Double, raw: None }), Scanner { line: 1, column: 7, start_idx: 6 }))
    );
    assert_eq!(
        string_literal(&Scanner::new(), "'\\r\\nboo'"),
        Some((
            Token::String(StringToken { value: JSString::from("\r\nboo"), delimiter: StringDelimiter::Single, raw: Some(String::from("\\r\\nboo")) }),
            Scanner { line: 1, column: 10, start_idx: 9 }
        ))
    );
}

#[test]
fn template_test() {
    assert_eq!(template(&Scanner::new(), "Q"), None);
}

#[test]
fn div_punctuator_test() {
    assert_eq!(div_punctuator(&Scanner::new(), "/", ScanGoal::InputElementDiv), Some((Token::Punctuator(Punctuator::Slash), Scanner { line: 1, column: 2, start_idx: 1 })));
    assert_eq!(div_punctuator(&Scanner::new(), "/", ScanGoal::InputElementRegExp), None);
    assert_eq!(div_punctuator(&Scanner::new(), "/", ScanGoal::InputElementRegExpOrTemplateTail), None);
    assert_eq!(div_punctuator(&Scanner::new(), "/", ScanGoal::InputElementTemplateTail), Some((Token::Punctuator(Punctuator::Slash), Scanner { line: 1, column: 2, start_idx: 1 })));
    assert_eq!(div_punctuator(&Scanner::new(), "/=", ScanGoal::InputElementDiv), Some((Token::Punctuator(Punctuator::SlashEq), Scanner { line: 1, column: 3, start_idx: 2 })));
    assert_eq!(div_punctuator(&Scanner::new(), "/=", ScanGoal::InputElementRegExp), None);
    assert_eq!(div_punctuator(&Scanner::new(), "/=", ScanGoal::InputElementRegExpOrTemplateTail), None);
    assert_eq!(div_punctuator(&Scanner::new(), "/=", ScanGoal::InputElementTemplateTail), Some((Token::Punctuator(Punctuator::SlashEq), Scanner { line: 1, column: 3, start_idx: 2 })));
    assert_eq!(div_punctuator(&Scanner::new(), "Q", ScanGoal::InputElementDiv), None);
    assert_eq!(div_punctuator(&Scanner::new(), "Q", ScanGoal::InputElementRegExp), None);
    assert_eq!(div_punctuator(&Scanner::new(), "Q", ScanGoal::InputElementRegExpOrTemplateTail), None);
    assert_eq!(div_punctuator(&Scanner::new(), "Q", ScanGoal::InputElementTemplateTail), None);
}

#[test]
fn right_brace_punctuator_test() {
    assert_eq!(right_brace_punctuator(&Scanner::new(), "}", ScanGoal::InputElementDiv), Some((Token::Punctuator(Punctuator::RightBrace), Scanner { line: 1, column: 2, start_idx: 1 })));
    assert_eq!(right_brace_punctuator(&Scanner::new(), "}", ScanGoal::InputElementRegExp), Some((Token::Punctuator(Punctuator::RightBrace), Scanner { line: 1, column: 2, start_idx: 1 })));
    assert_eq!(right_brace_punctuator(&Scanner::new(), "}", ScanGoal::InputElementTemplateTail), None);
    assert_eq!(right_brace_punctuator(&Scanner::new(), "}", ScanGoal::InputElementRegExpOrTemplateTail), None);
    assert_eq!(right_brace_punctuator(&Scanner::new(), "Q", ScanGoal::InputElementDiv), None);
    assert_eq!(right_brace_punctuator(&Scanner::new(), "Q", ScanGoal::InputElementRegExp), None);
    assert_eq!(right_brace_punctuator(&Scanner::new(), "Q", ScanGoal::InputElementRegExpOrTemplateTail), None);
    assert_eq!(right_brace_punctuator(&Scanner::new(), "Q", ScanGoal::InputElementTemplateTail), None);
}

#[test]
fn common_token_test() {
    assert_eq!(
        common_token(&Scanner::new(), "new"),
        Some((Token::Identifier(IdentifierData { column: 1, keyword_id: Some(Keyword::New), line: 1, string_value: JSString::from("new") }), Scanner { line: 1, column: 4, start_idx: 3 }))
    );
    assert_eq!(common_token(&Scanner::new(), "10"), Some((Token::Number(10.0), Scanner { line: 1, column: 3, start_idx: 2 })));
    assert_eq!(common_token(&Scanner::new(), "**"), Some((Token::Punctuator(Punctuator::StarStar), Scanner { line: 1, column: 3, start_idx: 2 })));
    assert_eq!(
        common_token(&Scanner::new(), "'truth'"),
        Some((Token::String(StringToken { value: JSString::from("truth"), delimiter: StringDelimiter::Single, raw: None }), Scanner { line: 1, column: 8, start_idx: 7 }))
    );
}
#[test]
fn common_token_test_nstemp() {
    let r = common_token(&Scanner::new(), "``");
    assert_eq!(
        r,
        Some((
            Token::NoSubstitutionTemplate(TemplateData { tv: Some(JSString::from("")), trv: JSString::from(""), starting_index: 0, byte_length: 2 }),
            Scanner { line: 1, column: 3, start_idx: 2 }
        ))
    )
}

#[test]
fn regular_expression_literal_test_01() {
    assert_eq!(regular_expression_literal(&Scanner::new(), "", ScanGoal::InputElementRegExp), None);
    assert_eq!(regular_expression_literal(&Scanner::new(), "", ScanGoal::InputElementRegExpOrTemplateTail), None);
    assert_eq!(regular_expression_literal(&Scanner::new(), "", ScanGoal::InputElementDiv), None);
    assert_eq!(regular_expression_literal(&Scanner::new(), "", ScanGoal::InputElementTemplateTail), None);
    assert_eq!(regular_expression_literal(&Scanner::new(), "/abcd/", ScanGoal::InputElementDiv), None);
    assert_eq!(regular_expression_literal(&Scanner::new(), "/abcd/", ScanGoal::InputElementTemplateTail), None);
}
#[test]
fn regular_expression_literal_test_02() {
    let result = regular_expression_literal(&Scanner::new(), "/abcd/", ScanGoal::InputElementRegExp);
    assert_eq!(result, Some((Token::RegularExpression(RegularExpressionData { body: String::from("abcd"), flags: String::from("") }), Scanner { line: 1, column: 7, start_idx: 6 })));
}
#[test]
fn regular_expression_literal_test_03() {
    let result = regular_expression_literal(&Scanner::new(), "/abcd/", ScanGoal::InputElementRegExpOrTemplateTail);
    assert_eq!(result, Some((Token::RegularExpression(RegularExpressionData { body: String::from("abcd"), flags: String::from("") }), Scanner { line: 1, column: 7, start_idx: 6 })));
}
#[test]
fn regular_expression_literal_test_04() {
    let result = regular_expression_literal(&Scanner::new(), "/\\//", ScanGoal::InputElementRegExpOrTemplateTail);
    assert_eq!(result, Some((Token::RegularExpression(RegularExpressionData { body: String::from("\\/"), flags: String::from("") }), Scanner { line: 1, column: 5, start_idx: 4 })));
}

#[test]
fn template_literal_test_01() {
    assert_eq!(template_substitution_tail(&Scanner::new(), "", ScanGoal::InputElementRegExp), None);
    assert_eq!(template_substitution_tail(&Scanner::new(), "", ScanGoal::InputElementRegExpOrTemplateTail), None);
    assert_eq!(template_substitution_tail(&Scanner::new(), "", ScanGoal::InputElementDiv), None);
    assert_eq!(template_substitution_tail(&Scanner::new(), "", ScanGoal::InputElementTemplateTail), None);
    assert_eq!(template_substitution_tail(&Scanner::new(), "} middle {", ScanGoal::InputElementDiv), None);
    assert_eq!(template_substitution_tail(&Scanner::new(), "} middle {", ScanGoal::InputElementRegExp), None);
}

#[test]
fn scan_token_test_01() {
    assert_eq!(scan_token(&Scanner::new(), "", ScanGoal::InputElementRegExp), (Token::Eof, Scanner { line: 1, column: 1, start_idx: 0 }));
    assert_eq!(scan_token(&Scanner::new(), "  /* nothing to see here */   ", ScanGoal::InputElementRegExp), (Token::Eof, Scanner { line: 1, column: 31, start_idx: 30 }));
    assert_eq!(scan_token(&Scanner::new(), "/=", ScanGoal::InputElementDiv), (Token::Punctuator(Punctuator::SlashEq), Scanner { line: 1, column: 3, start_idx: 2 }));
    assert_eq!(scan_token(&Scanner::new(), "}", ScanGoal::InputElementRegExp), (Token::Punctuator(Punctuator::RightBrace), Scanner { line: 1, column: 2, start_idx: 1 }));
}
#[test]
fn scan_token_panic_01() {
    let (token, scanner) = scan_token(&Scanner::new(), "@", ScanGoal::InputElementRegExp);
    assert!(matches!(token, Token::Error(_)));
    assert_eq!(scanner, Scanner { line: 1, column: 1, start_idx: 0 });
}

#[test]
fn thd_count_test_01() {
    assert!(THDCount::try_from(5).is_err())
}
#[test]
fn thd_count_test_02() {
    assert!(THDCount::try_from(4) == Ok(THDCount(4)));
}
#[test]
fn thd_count_test_03() {
    assert!(THDCount::try_from(0) == Ok(THDCount(0)));
}
#[test]
fn thd_count_test_04() {
    assert_ne!(format!("{:?}", THDCount(3)), "");
}
#[test]
fn thd_count_ne() {
    let t1 = THDCount(0);
    let t2 = THDCount(0);
    let t3 = THDCount(3);
    assert_eq!(t1 != t2, false);
    assert_eq!(t1 != t3, true);
}

#[test]
fn template_test_01() {
    let r = scan_token(&Scanner::new(), "``", ScanGoal::InputElementRegExp);
    let (token, scanner) = r;
    assert_eq!(scanner, Scanner { line: 1, column: 3, start_idx: 2 });
    assert_eq!(token, Token::NoSubstitutionTemplate(TemplateData { tv: Some(JSString::from("")), trv: JSString::from(""), starting_index: 0, byte_length: 2 }));
}
#[test]
fn template_test_02() {
    let r = scan_token(&Scanner::new(), "`a`", ScanGoal::InputElementRegExp);
    let (token, scanner) = r;
    assert_eq!(scanner, Scanner { line: 1, column: 4, start_idx: 3 });
    assert_eq!(token, Token::NoSubstitutionTemplate(TemplateData { tv: Some(JSString::from("a")), trv: JSString::from("a"), starting_index: 0, byte_length: 3 }));
}
#[test]
fn template_test_03() {
    let r = scan_token(&Scanner::new(), "`aa`", ScanGoal::InputElementRegExp);
    let (token, scanner) = r;
    assert_eq!(scanner, Scanner { line: 1, column: 5, start_idx: 4 });
    assert_eq!(token, Token::NoSubstitutionTemplate(TemplateData { tv: Some(JSString::from("aa")), trv: JSString::from("aa"), starting_index: 0, byte_length: 4 }));
}
#[test]
fn template_test_04() {
    let r = scan_token(&Scanner::new(), "`=\\0\\b\\t\\n\\v\\f\\r\\\"\\'\\\\\\x66\\u2288\\u{1f48b}\\\u{1f498}`", ScanGoal::InputElementRegExp);
    let (token, scanner) = r;
    assert_eq!(scanner, Scanner { line: 1, column: 45, start_idx: 47 });
    assert_eq!(
        token,
        Token::NoSubstitutionTemplate(TemplateData {
            tv: Some(JSString::from("=\u{0}\u{8}\u{9}\u{a}\u{b}\u{c}\u{d}\"'\\f\u{2288}\u{1f48b}\u{1f498}")),
            trv: JSString::from("=\\0\\b\\t\\n\\v\\f\\r\\\"\\'\\\\\\x66\\u2288\\u{1f48b}\\\u{1f498}"),
            starting_index: 0,
            byte_length: 47,
        })
    );
}
#[test]
fn template_test_05() {
    let r = scan_token(&Scanner::new(), "`\\ubob`", ScanGoal::InputElementRegExp);
    let (token, scanner) = r;
    assert_eq!(scanner, Scanner { line: 1, column: 8, start_idx: 7 });
    assert_eq!(token, Token::NoSubstitutionTemplate(TemplateData { tv: None, trv: JSString::from("\\ubob"), starting_index: 0, byte_length: 7 }));
}
#[test]
fn template_test_06() {
    let r = scan_token(&Scanner::new(), "`\\u{}`", ScanGoal::InputElementRegExp);
    let (token, scanner) = r;
    assert_eq!(scanner, Scanner { line: 1, column: 7, start_idx: 6 });
    assert_eq!(token, Token::NoSubstitutionTemplate(TemplateData { tv: None, trv: JSString::from("\\u{}"), starting_index: 0, byte_length: 6 }));
}
#[test]
fn template_test_07() {
    let r = scan_token(&Scanner::new(), "`\\u{9999999999999999999999999999999999999999999999999999999999}`", ScanGoal::InputElementRegExp);
    let (token, scanner) = r;
    assert_eq!(scanner, Scanner { line: 1, column: 65, start_idx: 64 });
    assert_eq!(
        token,
        Token::NoSubstitutionTemplate(TemplateData { tv: None, trv: JSString::from("\\u{9999999999999999999999999999999999999999999999999999999999}"), starting_index: 0, byte_length: 64 })
    );
}
#[test]
fn template_test_08() {
    let r = scan_token(&Scanner::new(), "`\\u{9999:`", ScanGoal::InputElementRegExp);
    let (token, scanner) = r;
    assert_eq!(scanner, Scanner { line: 1, column: 11, start_idx: 10 });
    assert_eq!(token, Token::NoSubstitutionTemplate(TemplateData { tv: None, trv: JSString::from("\\u{9999:"), starting_index: 0, byte_length: 10 }));
}
#[test]
fn template_test_09() {
    let r = scan_token(&Scanner::new(), "`\\", ScanGoal::InputElementRegExp);
    let (token, scanner) = r;
    assert_eq!(scanner, Scanner { line: 1, column: 1, start_idx: 0 });
    assert!(matches!(token, Token::Error(_)));
}
#[test]
fn template_test_10() {
    let r = scan_token(&Scanner::new(), "`\\03`", ScanGoal::InputElementRegExp);
    let (token, scanner) = r;
    assert_eq!(scanner, Scanner { line: 1, column: 6, start_idx: 5 });
    assert_eq!(token, Token::NoSubstitutionTemplate(TemplateData { tv: None, trv: JSString::from("\\03"), starting_index: 0, byte_length: 5 }));
}
#[test]
fn template_test_11() {
    let r = scan_token(&Scanner::new(), "`\\03 and escapes later? \\u{1f48b}?`", ScanGoal::InputElementRegExp);
    let (token, scanner) = r;
    assert_eq!(scanner, Scanner { line: 1, column: 36, start_idx: 35 });
    assert_eq!(token, Token::NoSubstitutionTemplate(TemplateData { tv: None, trv: JSString::from("\\03 and escapes later? \\u{1f48b}?"), starting_index: 0, byte_length: 35 }));
}
#[test]
fn template_test_12() {
    let r = scan_token(&Scanner::new(), "`one\\\ntwo\\\u{2028}three\\\u{2029}four\\\r\nfive\\\rsix`", ScanGoal::InputElementRegExp);
    let (token, scanner) = r;
    assert_eq!(scanner, Scanner { line: 6, column: 5, start_idx: 39 });
    assert_eq!(
        token,
        Token::NoSubstitutionTemplate(TemplateData {
            tv: Some(JSString::from("onetwothreefourfivesix")),
            trv: JSString::from("one\\\ntwo\\\u{2028}three\\\u{2029}four\\\nfive\\\nsix"),
            starting_index: 0,
            byte_length: 39,
        })
    );
}
#[test]
fn template_test_13() {
    let r = scan_token(&Scanner::new(), "`This ${thing} is great`", ScanGoal::InputElementRegExp);
    let (token, scanner) = r;
    assert_eq!(scanner, Scanner { line: 1, column: 9, start_idx: 8 });
    assert_eq!(token, Token::TemplateHead(TemplateData { tv: Some(JSString::from("This ")), trv: JSString::from("This "), starting_index: 0, byte_length: 8 }));
}

#[test]
fn template_test_14() {
    let r = scan_token(&Scanner::new(), "}${", ScanGoal::InputElementTemplateTail);
    let (token, scanner) = r;
    assert_eq!(scanner, Scanner { line: 1, column: 4, start_idx: 3 });
    assert_eq!(token, Token::TemplateMiddle(TemplateData { tv: Some(JSString::from("")), trv: JSString::from(""), starting_index: 0, byte_length: 3 }));
}
#[test]
fn template_test_15() {
    let r = scan_token(&Scanner::new(), "}`", ScanGoal::InputElementTemplateTail);
    let (token, scanner) = r;
    assert_eq!(scanner, Scanner { line: 1, column: 3, start_idx: 2 });
    assert_eq!(token, Token::TemplateTail(TemplateData { tv: Some(JSString::from("")), trv: JSString::from(""), starting_index: 0, byte_length: 2 }));
}

#[test]
fn charval_test() {
    assert_eq!(CharVal::try_from(0x10fffe), Ok(CharVal(0x10fffe)));
    assert!(CharVal::try_from(0x200000).is_err());
    assert_eq!(CharVal::from('\u{10ab32}'), CharVal(0x10ab32));
}
#[test]
fn charval_debug() {
    assert_ne!(format!("{:?}", CharVal(62)), "");
}
#[test]
fn charval_ne() {
    let cv1 = CharVal::from('c');
    let cv2 = CharVal::from('c');
    let cv3 = CharVal::from('X');

    assert_eq!(cv1 != cv2, false);
    assert_eq!(cv1 != cv3, true);
}

#[test]
fn regex_test_01() {
    let r = scan_token(&Scanner::new(), "/a/", ScanGoal::InputElementRegExp);
    let (token, scanner) = r;
    assert_eq!(scanner, Scanner { line: 1, column: 4, start_idx: 3 });
    assert_eq!(token, Token::RegularExpression(RegularExpressionData { body: String::from("a"), flags: String::new() }));
}

#[test]
fn regex_test_02() {
    let r = scan_token(&Scanner::new(), "/blue/green", ScanGoal::InputElementRegExp);
    let (token, scanner) = r;
    assert_eq!(scanner, Scanner { line: 1, column: 12, start_idx: 11 });
    assert_eq!(token, Token::RegularExpression(RegularExpressionData { body: String::from("blue"), flags: String::from("green") }));
}
#[test]
fn regex_test_03() {
    let scanner = Scanner { line: 1, column: 5, start_idx: 4 };
    let r = scan_token(&scanner, "####/blue/green", ScanGoal::InputElementRegExp);
    let (token, scanner) = r;
    assert_eq!(scanner, Scanner { line: 1, column: 16, start_idx: 15 });
    assert_eq!(token, Token::RegularExpression(RegularExpressionData { body: String::from("blue"), flags: String::from("green") }));
}
#[test]
fn scan_token_binary_digits_01() {
    let r = scan_token(&Scanner::new(), "0b01__01", ScanGoal::InputElementRegExp);
    let (token, scanner) = r;
    assert_eq!(scanner, Scanner { line: 1, column: 1, start_idx: 0 });
    assert_eq!(token, Token::Error(String::from("Unrecognized Token")));
}
#[test]
fn scan_token_binary_digits_02() {
    let r = scan_token(&Scanner::new(), "0bx", ScanGoal::InputElementRegExp);
    let (token, scanner) = r;
    assert_eq!(scanner, Scanner { line: 1, column: 1, start_idx: 0 });
    assert_eq!(token, Token::Error(String::from("Unrecognized Token")));
}
#[test]
fn scan_token_octal_digits_01() {
    let r = scan_token(&Scanner::new(), "0o01__01", ScanGoal::InputElementRegExp);
    let (token, scanner) = r;
    assert_eq!(scanner, Scanner { line: 1, column: 1, start_idx: 0 });
    assert_eq!(token, Token::Error(String::from("Unrecognized Token")));
}
#[test]
fn scan_token_octal_digits_02() {
    let r = scan_token(&Scanner::new(), "0ox", ScanGoal::InputElementRegExp);
    let (token, scanner) = r;
    assert_eq!(scanner, Scanner { line: 1, column: 1, start_idx: 0 });
    assert_eq!(token, Token::Error(String::from("Unrecognized Token")));
}
#[test]
fn scan_token_hex_digits_01() {
    let r = scan_token(&Scanner::new(), "0x01__01", ScanGoal::InputElementRegExp);
    let (token, scanner) = r;
    assert_eq!(scanner, Scanner { line: 1, column: 1, start_idx: 0 });
    assert_eq!(token, Token::Error(String::from("Unrecognized Token")));
}
#[test]
fn scan_token_hex_digits_02() {
    let r = scan_token(&Scanner::new(), "0xx", ScanGoal::InputElementRegExp);
    let (token, scanner) = r;
    assert_eq!(scanner, Scanner { line: 1, column: 1, start_idx: 0 });
    assert_eq!(token, Token::Error(String::from("Unrecognized Token")));
}
#[test]
fn scan_token_err() {
    let (token, scanner) = scan_token(&Scanner::new(), "/*", ScanGoal::InputElementRegExp);
    assert_eq!(scanner, Scanner { line: 1, column: 1, start_idx: 0 });
    assert_eq!(token, Token::Error(String::from("Unterminated /*-style comment. Started on line 1, column 1.")));
}

#[test]
fn number_style_debug() {
    format!("{:?}", NumberStyle::BigHex);
}
#[test]
fn number_style_clone() {
    let a = NumberStyle::Hex;
    let b = a.clone();
    assert_eq!(a, b);
}

#[test]
fn template_data_debug() {
    assert_ne!(format!("{:?}", TemplateData { tv: None, trv: JSString::from(""), starting_index: 0, byte_length: 0 }), "");
}
#[test]
fn template_data_display() {
    assert_eq!(format!("{}", TemplateData { tv: Some(JSString::from("blue")), trv: JSString::from("\u{12}\u{11}blue"), starting_index: 0, byte_length: 6 }), "\u{2426}\u{2426}blue");
}
#[test]
fn template_data_ne() {
    let td1 = TemplateData { tv: None, trv: JSString::from(""), starting_index: 0, byte_length: 0 };
    let td2 = TemplateData { tv: None, trv: JSString::from(""), starting_index: 0, byte_length: 0 };
    let td3 = TemplateData { tv: Some(JSString::from("rust")), trv: JSString::from("rust"), starting_index: 0, byte_length: 4 };

    assert_eq!(td1 != td2, false);
    assert_eq!(td1 != td3, true);
}

fn calculate_hash<T: Hash>(factory: &RandomState, t: &T) -> u64 {
    let mut s = factory.build_hasher();
    t.hash(&mut s);
    s.finish()
}
#[test]
fn scanner_hash() {
    let s1 = Scanner { line: 10, column: 50, start_idx: 49 };
    let s2 = Scanner { line: 10, column: 50, start_idx: 49 };
    let s3 = Scanner { line: 1, column: 1, start_idx: 0 };

    let factory = RandomState::new();

    assert_eq!(s1, s2);
    assert_eq!(calculate_hash(&factory, &s1), calculate_hash(&factory, &s2));
    assert_ne!(s1, s3);
    assert_ne!(calculate_hash(&factory, &s1), calculate_hash(&factory, &s3));
}
#[test]
fn scanner_debug() {
    assert_ne!(format!("{:?}", Scanner::new()), "");
}
#[test]
fn scanner_ne() {
    let s1 = Scanner { line: 10, column: 50, start_idx: 49 };
    let s2 = Scanner { line: 10, column: 50, start_idx: 49 };
    let s3 = Scanner { line: 1, column: 1, start_idx: 0 };

    assert_eq!(s1 != s2, false);
    assert_eq!(s1 != s3, true);
}
#[test]
#[allow(clippy::eq_op)]
fn scanner_ordering() {
    let line10col50 = Scanner { line: 10, column: 50, start_idx: 33 };
    let line1col1 = Scanner { line: 1, column: 1, start_idx: 0 };
    let line10col10 = Scanner { line: 10, column: 10, start_idx: 30 };

    assert_eq!(line1col1 < line10col50, true);
    assert_eq!(line10col50 < line1col1, false);
    assert_eq!(line10col10 < line10col50, true);
    assert_eq!(line10col50 < line10col10, false);
    assert_eq!(line10col50 < line10col50, false);
}
#[test]
fn scanner_clone() {
    let s1 = Scanner { line: 10, column: 50, start_idx: 33 };
    let s2 = s1.clone();

    assert_eq!(s2, s1);
}

#[test]
fn string_token_debug() {
    assert_ne!(format!("{:?}", StringToken { value: JSString::from("blue"), delimiter: StringDelimiter::Double, raw: None}), "");
}

#[test]
fn token_debug() {
    assert_ne!(format!("{:?}", Token::Eof), "");
}
#[test]
fn token_matches_punct() {
    let t1 = Token::Eof;
    let t2 = Token::Punctuator(Punctuator::Caret);

    assert_eq!(t1.matches_punct(Punctuator::Semicolon), false);
    assert_eq!(t2.matches_punct(Punctuator::Caret), true);
    assert_eq!(t2.matches_punct(Punctuator::Semicolon), false);
}
#[test]
fn token_matches_keyword() {
    let t1 = Token::Eof;
    let t2 = Token::Identifier(IdentifierData { string_value: JSString::from("as"), keyword_id: Some(Keyword::As), line: 1, column: 1 });

    assert_eq!(t1.matches_keyword(Keyword::As), false);
    assert_eq!(t2.matches_keyword(Keyword::As), true);
    assert_eq!(t2.matches_keyword(Keyword::This), false);
}
#[test]
fn token_display() {
    assert_eq!(format!("{}", Token::Eof), "");
    assert_eq!(format!("{}", Token::Punctuator(Punctuator::Ellipsis)), "...");
    assert_eq!(format!("{}", Token::Identifier(IdentifierData { string_value: JSString::from("bob"), keyword_id: None, line: 1, column: 1 })), "bob");
    assert_eq!(format!("{}", Token::Number(6.222)), "6.222");
    assert_eq!(format!("{}", Token::BigInt(BigInt::parse_bytes(b"9131551", 10).unwrap())), "9131551");
    assert_eq!(format!("{}", Token::String(StringToken { value: JSString::from("baloney"), delimiter: StringDelimiter::Single, raw: None })), "'baloney'");
    assert_eq!(format!("{}", Token::String(StringToken { value: JSString::from("baloney"), delimiter: StringDelimiter::Double, raw: Some(String::from("\\x62aloney")) })), "\"\\x62aloney\"");
    assert_eq!(format!("{}", Token::NoSubstitutionTemplate(TemplateData { tv: Some(JSString::from("rust")), trv: JSString::from("rust"), starting_index: 0, byte_length: 4 })), "rust");
    assert_eq!(format!("{}", Token::TemplateHead(TemplateData { tv: Some(JSString::from("rust")), trv: JSString::from("rust"), starting_index: 0, byte_length: 4 })), "rust");
    assert_eq!(format!("{}", Token::TemplateMiddle(TemplateData { tv: Some(JSString::from("rust")), trv: JSString::from("rust"), starting_index: 0, byte_length: 4 })), "rust");
    assert_eq!(format!("{}", Token::TemplateTail(TemplateData { tv: Some(JSString::from("rust")), trv: JSString::from("rust"), starting_index: 0, byte_length: 4 })), "rust");
    assert_eq!(format!("{}", Token::RegularExpression(RegularExpressionData { body: String::from("rust"), flags: String::from("ng") })), "/rust/ng");
    assert_eq!(format!("{}", Token::Error(String::from("syntax error"))), "\u{26a0}");
    assert_eq!(format!("{}", Token::PrivateIdentifier(IdentifierData { string_value: JSString::from("bob"), keyword_id: None, line: 1, column: 1 })), "#bob");
}
#[test]
fn token_ne() {
    let t1 = Token::Eof;
    let t2 = Token::Punctuator(Punctuator::Semicolon);
    let t3 = Token::Eof;

    assert_eq!(t1 != t2, true);
    assert_eq!(t1 != t3, false);
}

#[test]
fn keyword_clone() {
    let k1 = Keyword::Debugger;
    let k2 = k1.clone();
    assert_eq!(k1, k2);
}
#[test]
fn keyword_display() {
    let pairs = vec![
        (Keyword::Await, "await"),
        (Keyword::Break, "break"),
        (Keyword::Case, "case"),
        (Keyword::Catch, "catch"),
        (Keyword::Class, "class"),
        (Keyword::Const, "const"),
        (Keyword::Continue, "continue"),
        (Keyword::Debugger, "debugger"),
        (Keyword::Default, "default"),
        (Keyword::Delete, "delete"),
        (Keyword::Do, "do"),
        (Keyword::Else, "else"),
        (Keyword::Enum, "enum"),
        (Keyword::Export, "export"),
        (Keyword::Extends, "extends"),
        (Keyword::False, "false"),
        (Keyword::Finally, "finally"),
        (Keyword::For, "for"),
        (Keyword::Function, "function"),
        (Keyword::If, "if"),
        (Keyword::Import, "import"),
        (Keyword::In, "in"),
        (Keyword::Instanceof, "instanceof"),
        (Keyword::New, "new"),
        (Keyword::Null, "null"),
        (Keyword::Return, "return"),
        (Keyword::Super, "super"),
        (Keyword::Switch, "switch"),
        (Keyword::This, "this"),
        (Keyword::Throw, "throw"),
        (Keyword::True, "true"),
        (Keyword::Try, "try"),
        (Keyword::Typeof, "typeof"),
        (Keyword::Var, "var"),
        (Keyword::Void, "void"),
        (Keyword::While, "while"),
        (Keyword::With, "with"),
        (Keyword::Yield, "yield"),
        (Keyword::Let, "let"),
        (Keyword::Static, "static"),
        (Keyword::Implements, "implements"),
        (Keyword::Interface, "interface"),
        (Keyword::Package, "package"),
        (Keyword::Private, "private"),
        (Keyword::Protected, "protected"),
        (Keyword::Public, "public"),
        (Keyword::As, "as"),
        (Keyword::Async, "async"),
        (Keyword::From, "from"),
        (Keyword::Get, "get"),
        (Keyword::Of, "of"),
        (Keyword::Set, "set"),
        (Keyword::Target, "target"),
        (Keyword::Meta, "meta"),
    ];
    for (kwd, display) in pairs {
        assert_eq!(format!("{}", kwd), display);
    }
}

#[test]
fn identifier_data_matches() {
    let catch = IdentifierData { string_value: JSString::from("catch"), keyword_id: Some(Keyword::Catch), line: 1, column: 1 };
    let other = IdentifierData { string_value: JSString::from("other"), keyword_id: None, line: 1, column: 1 };
    assert_eq!(catch.matches(Keyword::Catch), true);
    assert_eq!(catch.matches(Keyword::If), false);
    assert_eq!(other.matches(Keyword::Catch), false);
}
#[test]
fn identifier_data_ne() {
    let catch = IdentifierData { string_value: JSString::from("catch"), keyword_id: Some(Keyword::Catch), line: 1, column: 1 };
    let catch2 = IdentifierData { string_value: JSString::from("catch"), keyword_id: Some(Keyword::Catch), line: 1, column: 1 };
    let other = IdentifierData { string_value: JSString::from("other"), keyword_id: None, line: 1, column: 1 };

    assert_eq!(catch != catch2, false);
    assert_eq!(catch != other, true);
}
#[test]
fn identifier_data_debug() {
    assert_ne!(format!("{:?}", IdentifierData { string_value: JSString::from("catch"), keyword_id: Some(Keyword::Catch), line: 1, column: 1 }), "");
}

#[test]
fn regular_expression_data_debug() {
    assert_ne!(format!("{:?}", RegularExpressionData { body: String::from("abcd"), flags: String::from("g") }), "");
}
#[test]
fn regular_expression_data_ne() {
    let rd1 = RegularExpressionData { body: String::from("rust"), flags: String::from("") };
    let rd2 = RegularExpressionData { body: String::from("rust"), flags: String::from("") };
    let rd3 = RegularExpressionData { body: String::from("rust"), flags: String::from("g") };

    assert_eq!(rd1 != rd2, false);
    assert_eq!(rd1 != rd3, true);
}

#[test]
fn scan_goal_debug() {
    assert_ne!(format!("{:?}", ScanGoal::InputElementRegExp), "");
}
#[test]
fn scan_goal_clone() {
    let sg1 = ScanGoal::InputElementRegExp;
    let sg2 = sg1.clone();

    assert_eq!(sg1, sg2);
}

#[test]
fn private_identifier_01() {
    let (tok, scan) = scan_token(&Scanner::new(), "#bobo", ScanGoal::InputElementRegExp);
    assert_eq!(scan, Scanner { line: 1, column: 6, start_idx: 5 });
    assert!(matches!(tok, Token::PrivateIdentifier(_)));
    if let Token::PrivateIdentifier(data) = tok {
        assert_eq!(data.column, 2);
        assert_eq!(data.keyword_id, None);
        assert_eq!(data.line, 1);
        assert_eq!(data.string_value, JSString::from("bobo"));
    }
}
#[test]
fn private_identifier_02() {
    let (tok, scan) = scan_token(&Scanner::new(), "#100", ScanGoal::InputElementRegExp);
    assert_eq!(scan, Scanner { line: 1, column: 1, start_idx: 0 });
    println!("{:?}", tok);
    assert!(matches!(tok, Token::Error(_)));
    if let Token::Error(msg) = tok {
        assert_eq!(msg, "Unrecognized Token");
    }
}
#[test]
fn private_identifier_03() {
    let (tok, scan) = scan_token(&Scanner::new(), "#ident\\u{20}aa", ScanGoal::InputElementRegExp);
    assert_eq!(scan, Scanner { line: 1, column: 7, start_idx: 6 });
    assert!(matches!(tok, Token::Error(_)));
    if let Token::Error(msg) = tok {
        assert_eq!(msg, "1:7: Invalid Identifier Continuation Character ' '");
    }
}
