pub mod ranges;
use num::bigint::BigInt;
use std::convert::TryFrom;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum ScanGoal {
    InputElementRegExpOrTemplateTail,
    InputElementRegExp,
    InputElementTemplateTail,
    InputElementDiv,
}

#[derive(PartialEq, Clone)]
pub struct JSString {
    string: Rc<Vec<u16>>,
}

impl fmt::Debug for JSString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{:?}", String::from_utf16_lossy(&self.string)))
    }
}

impl fmt::Display for JSString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{}", String::from_utf16_lossy(&self.string)))
    }
}

impl std::cmp::PartialEq<&str> for JSString {
    fn eq(&self, other: &&str) -> bool {
        let mut iter_vec = self.string.iter();
        let mut iter_chars = (*other).chars();
        loop {
            let left = iter_vec.next();
            let right = iter_chars.next();
            if left.is_none() && right.is_none() {
                return true;
            }
            if left.is_none() || right.is_none() {
                return false;
            }
            if *(left.unwrap()) as u32 != right.unwrap() as u32 {
                return false;
            }
        }
    }
}

impl From<&str> for JSString {
    fn from(source: &str) -> Self {
        let mut result = Vec::with_capacity(source.len());
        for val in source.encode_utf16() {
            result.push(val);
        }
        JSString {
            string: Rc::new(result),
        }
    }
}

impl From<&[u16]> for JSString {
    fn from(source: &[u16]) -> Self {
        let mut result = Vec::with_capacity(source.len());
        result.extend_from_slice(source);
        JSString {
            string: Rc::new(result),
        }
    }
}

impl JSString {
    pub fn take(source: Vec<u16>) -> JSString {
        JSString {
            string: Rc::new(source),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Keyword {
    Await,
    Break,
    Case,
    Catch,
    Class,
    Const,
    Continue,
    Debugger,
    Default,
    Delete,
    Do,
    Else,
    Enum,
    Export,
    Extends,
    False,
    Finally,
    For,
    Function,
    If,
    Import,
    In,
    Instanceof,
    New,
    Null,
    Return,
    Super,
    Switch,
    This,
    Throw,
    True,
    Try,
    Typeof,
    Var,
    Void,
    While,
    With,
    Yield,

    Let,
    Static,
    Implements,
    Interface,
    Package,
    Private,
    Protected,
    Public,
    As,
    Async,
    From,
    Get,
    Of,
    Set,
    Target,
    Meta,
}

#[derive(Debug, PartialEq)]
pub struct IdentifierData {
    pub string_value: JSString,
    pub keyword_id: Option<Keyword>,
    pub line: u32,
    pub column: u32,
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Eof,
    LeftParen,    // (
    RightParen,   // )
    LeftBrace,    // {
    RightBrace,   // }
    Dot,          // .
    Ellipsis,     // ...
    Comma,        // ,
    Semicolon,    // ;
    LeftBracket,  // [
    RightBracket, // ]
    Colon,        // :
    Tilde,        // ~
    Minus,        // -
    MinusMinus,   // --
    MinusEq,      // -=
    Plus,         // +
    PlusPlus,     // ++
    PlusEq,       // +=
    Slash,        // /
    SlashEq,      // /=
    Star,         // *
    StarEq,       // *=
    StarStar,     // **
    StarStarEq,   // **=
    Amp,          // &
    AmpAmp,       // &&
    AmpAmpEq,     // &&=
    AmpEq,        // &=
    Lt,           // <
    LtEq,         // <=
    LtLt,         // <<
    LtLtEq,       // <<=
    Gt,           // >
    GtEq,         // >=
    GtGt,         // >>
    GtGtGt,       // >>>
    GtGtEq,       // >>=
    GtGtGtEq,     // >>>=
    Eq,           // =
    EqGt,         // =>
    EqEq,         // ==
    EqEqEq,       // ===
    Bang,         // !
    BangEq,       // !=
    BangEqEq,     // !==
    Percent,      // %
    PercentEq,    // %=
    Pipe,         // |
    PipePipe,     // ||
    PipePipeEq,   // ||=
    PipeEq,       // |=
    Caret,        // ^
    CaretEq,      // ^=
    Question,     // ?
    QDot,         // ?.
    QQ,           // ??
    QQEq,         // ??=
    Identifier(IdentifierData),
    Number(f64),
    BigInt(BigInt),
    String(JSString),
    Error,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Scanner {
    pub line: u32,
    pub column: u32,
    pub start_idx: usize,
}
impl Scanner {
    pub fn new() -> Scanner {
        Scanner {
            line: 1,
            column: 1,
            start_idx: 0,
        }
    }
}
fn is_lineterm(ch: char) -> bool {
    ch == '\x0a' || ch == '\x0d' || ch == '\u{2028}' || ch == '\u{2029}'
}

fn is_whitespace(ch: char) -> bool {
    ch >= '\x09' && ch <= '\x0d'
        || ch == '\x20'
        || ch == '\u{00a0}'
        || ch == '\u{2028}'
        || ch == '\u{2029}'
        || ch == '\u{feff}'
        || ch == '\u{1680}'
        || ch >= '\u{2000}' && ch <= '\u{200a}'
        || ch == '\u{202f}'
        || ch == '\u{205f}'
        || ch == '\u{3000}'
}

fn is_single_escape_char(ch: char) -> bool {
    match ch {
        '\'' | '"' | '\\' | 'b' | 'f' | 'n' | 'r' | 't' | 'v' => true,
        _ => false,
    }
}

fn is_escape_char(ch: char) -> bool {
    match ch {
        '\'' | '"' | '\\' | 'b' | 'f' | 'n' | 'r' | 't' | 'v' | 'u' | 'x' | '0' | '1' | '2' | '3' | '4' | '5' | '6'
        | '7' | '8' | '9' => true,
        _ => false,
    }
}

// Given a scanner context, return a new context (over the same source string) which begins at the first
// character that should be matched as a token. The "line" and "column" values are updated as needed.
pub fn skip_skippables<'a>(scanner: &'a Scanner, source: &'a str) -> Result<Scanner, String> {
    let mut line = scanner.line;
    let mut column = scanner.column;
    let mut idx = scanner.start_idx;
    let mut iter = source[idx..].chars();
    let mut pending_idx;

    let mut ch;
    match iter.next() {
        None => {
            return Ok(Scanner {
                line,
                column,
                start_idx: idx,
            })
        }
        Some(c) => {
            ch = c;
            pending_idx = idx + ch.len_utf8();
        }
    }
    loop {
        if is_lineterm(ch) {
            line = line + 1;
            column = 1;
            let previous = ch;
            idx = pending_idx; // consume the ch
            match iter.next() {
                None => {
                    return Ok(Scanner {
                        line,
                        column,
                        start_idx: idx,
                    })
                }
                Some(c) => {
                    ch = c;
                    pending_idx = idx + ch.len_utf8();
                }
            }
            if previous == '\r' && ch == '\n' {
                idx = pending_idx;
                match iter.next() {
                    None => {
                        return Ok(Scanner {
                            line,
                            column,
                            start_idx: idx,
                        })
                    }
                    Some(c) => {
                        ch = c;
                        pending_idx = idx + ch.len_utf8();
                    }
                }
            }
            continue;
        }

        if is_whitespace(ch) {
            column = column + 1;
            idx = pending_idx;
            match iter.next() {
                None => {
                    return Ok(Scanner {
                        line,
                        column,
                        start_idx: idx,
                    })
                }
                Some(c) => {
                    ch = c;
                    pending_idx = idx + ch.len_utf8();
                }
            }
            continue;
        }

        if ch == '/' {
            let ch_next;
            match iter.next() {
                None => {
                    return Ok(Scanner {
                        line,
                        column,
                        start_idx: idx,
                    })
                }
                Some(c) => {
                    ch_next = c;
                    pending_idx = pending_idx + ch_next.len_utf8();
                }
            }
            match ch_next {
                '/' => {
                    // Single-line comment
                    column = column + 2;
                    idx = pending_idx;
                    loop {
                        match iter.next() {
                            None => {
                                return Ok(Scanner {
                                    line,
                                    column,
                                    start_idx: idx,
                                })
                            }
                            Some(c) => {
                                ch = c;
                                pending_idx = idx + ch.len_utf8();
                            }
                        }
                        if is_lineterm(ch) {
                            break;
                        }
                        column = column + 1;
                        idx = pending_idx;
                    }
                }
                '*' => {
                    // Multi-line comment
                    let comment_start_line = line;
                    let comment_start_column = column;
                    column = column + 2;
                    idx = pending_idx;

                    match iter.next() {
                        // If None comes back, this is actually a syntax error.
                        None => {
                            return Err(format!(
                                "Unterminated /*-style comment. Started on line {}, column {}.",
                                comment_start_line, comment_start_column
                            ))
                        }
                        Some(c) => {
                            ch = c;
                            pending_idx = idx + ch.len_utf8();
                        }
                    }

                    'comment: loop {
                        while ch == '*' {
                            column = column + 1;
                            idx = pending_idx;
                            match iter.next() {
                                // If None comes back, this is actually a syntax error.
                                None => {
                                    return Err(format!(
                                        "Unterminated /*-style comment. Started on line {}, column {}.",
                                        comment_start_line, comment_start_column
                                    ))
                                }
                                Some(c) => {
                                    ch = c;
                                    pending_idx = idx + ch.len_utf8();
                                }
                            }
                            if ch == '/' {
                                column = column + 1;
                                idx = pending_idx;
                                match iter.next() {
                                    None => {
                                        return Ok(Scanner {
                                            line,
                                            column,
                                            start_idx: idx,
                                        })
                                    }
                                    Some(c) => {
                                        ch = c;
                                        pending_idx = idx + ch.len_utf8();
                                    }
                                }
                                break 'comment;
                            }
                        }

                        if is_lineterm(ch) {
                            line = line + 1;
                            column = 1;
                            idx = pending_idx;
                            let previous = ch;
                            match iter.next() {
                                None => {
                                    return Err(format!(
                                        "Unterminated /*-style comment. Started on line {}, column {}.",
                                        comment_start_line, comment_start_column
                                    ))
                                }
                                Some(c) => {
                                    ch = c;
                                    pending_idx = idx + ch.len_utf8();
                                }
                            }
                            if previous == '\r' && ch == '\n' {
                                idx = pending_idx;
                                match iter.next() {
                                    None => {
                                        return Err(format!(
                                            "Unterminated /*-style comment. Started on line {}, column {}.",
                                            comment_start_line, comment_start_column
                                        ))
                                    }
                                    Some(c) => {
                                        ch = c;
                                        pending_idx = idx + ch.len_utf8();
                                    }
                                }
                            }
                            continue;
                        }

                        column = column + 1;
                        idx = pending_idx;
                        match iter.next() {
                            None => {
                                return Err(format!(
                                    "Unterminated /*-style comment. Started on line {}, column {}.",
                                    comment_start_line, comment_start_column
                                ))
                            }
                            Some(c) => {
                                ch = c;
                                pending_idx = idx + ch.len_utf8();
                            }
                        }
                    }
                }
                _ => {
                    return Ok(Scanner {
                        line,
                        column,
                        start_idx: idx,
                    })
                }
            }
            continue;
        }

        return Ok(Scanner {
            line,
            column,
            start_idx: idx,
        });
    }
}

fn is_digit(ch: char) -> bool {
    ch >= '0' && ch <= '9'
}

fn is_hex_digit(ch: char) -> bool {
    (ch >= '0' && ch <= '9') || (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F')
}

fn hex_four_digits(scanner: &Scanner, source: &str) -> Option<Scanner> {
    let mut iter = source[scanner.start_idx..].chars();
    let first = iter.next()?;
    let second = iter.next()?;
    let third = iter.next()?;
    let fourth = iter.next()?;
    if is_hex_digit(first) && is_hex_digit(second) && is_hex_digit(third) && is_hex_digit(fourth) {
        Some(Scanner {
            line: scanner.line,
            column: scanner.column + 4,
            start_idx: scanner.start_idx + first.len_utf8() + second.len_utf8() + third.len_utf8() + fourth.len_utf8(),
        })
    } else {
        None
    }
}

fn code_point(scanner: &Scanner, source: &str) -> Option<Scanner> {
    // One or more hex digit; MV should be <= 0x10ffff.
    let mut iter = source[scanner.start_idx..].chars();
    let mut count: usize = 0;
    loop {
        let opt_ch = iter.next();
        if let Some(ch) = opt_ch {
            if is_hex_digit(ch) {
                count += 1;
                continue;
            }
        }
        break;
    }
    if count > 0 {
        let parse_result = u32::from_str_radix(&source[scanner.start_idx..scanner.start_idx + count], 16);
        if let Ok(mv) = parse_result {
            if mv <= 0x10FFFF {
                return Some(Scanner {
                    line: scanner.line,
                    column: scanner.column + count as u32,
                    start_idx: scanner.start_idx + count,
                });
            }
        }
    }
    None
}

fn unicode_escape_sequence(scanner: &Scanner, source: &str) -> Option<Scanner> {
    let mut iter = source[scanner.start_idx..].chars();
    let first_ch = iter.next()?;
    if first_ch == 'u' {
        let second_char_idx = scanner.start_idx + first_ch.len_utf8();
        let hex_scanner = Scanner {
            line: scanner.line,
            column: scanner.column + 1,
            start_idx: second_char_idx,
        };
        let hex_option = hex_four_digits(&hex_scanner, source);
        match hex_option {
            Some(result) => Some(result),
            None => {
                let second_ch = iter.next()?;
                let third_char_idx = second_char_idx + second_ch.len_utf8();
                if second_ch == '{' {
                    let cp_scanner = Scanner {
                        line: scanner.line,
                        column: scanner.column + 2,
                        start_idx: third_char_idx,
                    };
                    let after_cp = code_point(&cp_scanner, source)?;
                    let last_char = source[after_cp.start_idx..].chars().next()?;
                    match last_char {
                        '}' => Some(Scanner {
                            line: scanner.line,
                            column: after_cp.column + 1,
                            start_idx: after_cp.start_idx + last_char.len_utf8(),
                        }),
                        _ => None,
                    }
                } else {
                    None
                }
            }
        }
    } else {
        None
    }
}

use std::char;
use std::str;

fn ues_char_value(source: &str) -> char {
    // We already know this is a valid Unicode Escape Sequence, so there's a lot of checking we don't do.
    let bytes = source.as_bytes();
    let value;
    if bytes[1] == '{' as u8 {
        value = u32::from_str_radix(str::from_utf8(&bytes[2..bytes.len() - 1]).unwrap(), 16).unwrap();
    } else {
        value = u32::from_str_radix(str::from_utf8(&bytes[1..5]).unwrap(), 16).unwrap();
    }
    char::from_u32(value).unwrap()
}

fn unicode_range_checker(ch: char, range: &[ranges::CharRange]) -> bool {
    for item in range {
        if ch >= item.first && ch <= item.last {
            return true;
        }
    }
    false
}

fn is_unicode_id_start(ch: char) -> bool {
    unicode_range_checker(ch, &ranges::ID_START)
}

fn is_unicode_id_continue(ch: char) -> bool {
    unicode_range_checker(ch, &ranges::ID_CONTINUE)
}

fn identifier_piece<F>(scanner: &Scanner, source: &str, validate: F, style: &str) -> Result<Option<Scanner>, String>
where
    F: Fn(char) -> bool,
{
    let mut idx = scanner.start_idx;
    let mut iter = source[idx..].chars();
    let ch;
    match iter.next() {
        None => return Ok(None),
        Some(c) => ch = c,
    }
    idx = idx + ch.len_utf8();
    if validate(ch) {
        Ok(Some(Scanner {
            line: scanner.line,
            column: scanner.column + 1,
            start_idx: idx,
        }))
    } else if ch == '\\' {
        let ues_scanner = Scanner {
            line: scanner.line,
            column: scanner.column + 1,
            start_idx: idx,
        };
        let after_scanner;
        match unicode_escape_sequence(&ues_scanner, source) {
            None => return Ok(None),
            Some(scanner) => after_scanner = scanner,
        };
        let ch_value = ues_char_value(&source[ues_scanner.start_idx..after_scanner.start_idx]);
        if !validate(ch_value) {
            return Err(format!(
                "{}:{}: Invalid Identifier {} Character {:?}",
                scanner.line, scanner.column, style, ch_value
            ));
        }
        Ok(Some(after_scanner))
    } else {
        Ok(None)
    }
}

fn identifier_start(scanner: &Scanner, source: &str) -> Result<Option<Scanner>, String> {
    // IdentifierStart::
    //      UnicodeIDStart
    //      $
    //      _
    //      \ UnicodeEscapeSequence
    let start_validation = |ch| ch == '$' || ch == '_' || is_unicode_id_start(ch);
    identifier_piece(scanner, source, start_validation, "Start")
}

fn identifier_part(scanner: &Scanner, source: &str) -> Result<Option<Scanner>, String> {
    // IdentifierPart::
    //      UnicodeIDContinue
    //      $
    //      \ UnicodeEscapeSequence
    //      <ZWNJ>
    //      <ZWJ>
    let continue_validation = |ch| ch == '$' || ch == '\u{200c}' || ch == '\u{200d}' || is_unicode_id_continue(ch);
    identifier_piece(scanner, source, continue_validation, "Continuation")
}

fn code_point_to_utf16_code_units(ch: char) -> Vec<u16> {
    let mut buf = [0; 2];
    let code_units = ch.encode_utf16(&mut buf);
    let mut result = Vec::new();
    result.extend_from_slice(code_units);
    result
}

#[derive(Debug, PartialEq)]
pub struct HexChar(char);
impl TryFrom<char> for HexChar {
    type Error = &'static str;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        if is_hex_digit(value) {
            Ok(HexChar(value))
        } else {
            Err("HexChar can only be used with hexidecimal digits!")
        }
    }
}

impl From<HexChar> for char {
    fn from(hc: HexChar) -> Self {
        let HexChar(ch) = hc;
        ch
    }
}

fn mv_of_hex_digit(digit: HexChar) -> u32 {
    let ch: char = digit.into();
    let code = ch as u32;
    if ch >= '0' && ch <= '9' {
        code - '0' as u32
    } else if ch >= 'A' && ch <= 'F' {
        code - 'A' as u32 + 10
    } else {
        code - 'a' as u32 + 10
    }
}

fn identifier_name_string_value(id_text: &str) -> JSString {
    let mut result: Vec<u16> = vec![];
    let mut iter = id_text.chars();
    loop {
        let ch;
        match iter.next() {
            None => break,
            Some(c) => ch = c,
        }
        if ch != '\\' {
            result.append(&mut code_point_to_utf16_code_units(ch));
        } else {
            // We know the strings are valid constructions, so we don't need to
            // check error conditions here. We'll rely on the panics from unwrap
            // to detect coding errors.
            iter.next().unwrap(); // The 'u' character
            let digit_or_brace = iter.next().unwrap(); // Either a hex char or an open curly brace
            let cp;
            if digit_or_brace == '{' {
                let mut val = 0;
                loop {
                    let ch = iter.next().unwrap();
                    if ch == '}' {
                        break;
                    }
                    val = val << 4 | mv_of_hex_digit(HexChar::try_from(ch).unwrap());
                }
                cp = char::from_u32(val).unwrap();
            } else {
                let second = HexChar::try_from(iter.next().unwrap()).unwrap();
                let third = HexChar::try_from(iter.next().unwrap()).unwrap();
                let fourth = HexChar::try_from(iter.next().unwrap()).unwrap();
                cp = char::from_u32(
                    mv_of_hex_digit(HexChar::try_from(digit_or_brace).unwrap()) << 12
                        | mv_of_hex_digit(second) << 8
                        | mv_of_hex_digit(third) << 4
                        | mv_of_hex_digit(fourth),
                )
                .unwrap();
            }
            result.append(&mut code_point_to_utf16_code_units(cp))
        }
    }
    JSString {
        string: Rc::new(result),
    }
}

fn keycomplete(source: &str, cmp: &str, kwd: Keyword) -> Option<Keyword> {
    if source == cmp {
        Some(kwd)
    } else {
        None
    }
}

fn identifier_name_keyword(source: &str) -> Option<Keyword> {
    let mut iter = source.chars();
    match iter.next() {
        Some('a') => match iter.next() {
            Some('s') => match iter.next() {
                None => Some(Keyword::As),
                Some('y') => keycomplete(&source[3..], "nc", Keyword::Async),
                _ => None,
            },
            Some('w') => keycomplete(&source[2..], "ait", Keyword::Await),
            _ => None,
        },
        Some('b') => keycomplete(&source[1..], "reak", Keyword::Break),
        Some('c') => match iter.next() {
            Some('a') => match iter.next() {
                Some('s') => keycomplete(&source[3..], "e", Keyword::Case),
                Some('t') => keycomplete(&source[3..], "ch", Keyword::Catch),
                _ => None,
            },
            Some('l') => keycomplete(&source[2..], "ass", Keyword::Class),
            Some('o') => match iter.next() {
                Some('n') => match iter.next() {
                    Some('s') => keycomplete(&source[4..], "t", Keyword::Const),
                    Some('t') => keycomplete(&source[4..], "inue", Keyword::Continue),
                    _ => None,
                },
                _ => None,
            },
            _ => None,
        },
        Some('d') => match iter.next() {
            Some('e') => match iter.next() {
                Some('b') => keycomplete(&source[3..], "ugger", Keyword::Debugger),
                Some('f') => keycomplete(&source[3..], "ault", Keyword::Default),
                Some('l') => keycomplete(&source[3..], "ete", Keyword::Delete),
                _ => None,
            },
            Some('o') => keycomplete(&source[2..], "", Keyword::Do),
            _ => None,
        },
        Some('e') => match iter.next() {
            Some('l') => keycomplete(&source[2..], "se", Keyword::Else),
            Some('n') => keycomplete(&source[2..], "um", Keyword::Enum),
            Some('x') => match iter.next() {
                Some('p') => keycomplete(&source[3..], "ort", Keyword::Export),
                Some('t') => keycomplete(&source[3..], "ends", Keyword::Extends),
                _ => None,
            },
            _ => None,
        },
        Some('f') => match iter.next() {
            Some('a') => keycomplete(&source[2..], "lse", Keyword::False),
            Some('i') => keycomplete(&source[2..], "nally", Keyword::Finally),
            Some('o') => keycomplete(&source[2..], "r", Keyword::For),
            Some('r') => keycomplete(&source[2..], "om", Keyword::From),
            Some('u') => keycomplete(&source[2..], "nction", Keyword::Function),
            _ => None,
        },
        Some('g') => keycomplete(&source[1..], "et", Keyword::Get),
        Some('i') => match iter.next() {
            Some('f') => keycomplete(&source[2..], "", Keyword::If),
            Some('m') => match iter.next() {
                Some('p') => match iter.next() {
                    Some('l') => keycomplete(&source[4..], "ements", Keyword::Implements),
                    Some('o') => keycomplete(&source[4..], "rt", Keyword::Import),
                    _ => None,
                },
                _ => None,
            },
            Some('n') => match iter.next() {
                None => Some(Keyword::In),
                Some('s') => keycomplete(&source[3..], "tanceof", Keyword::Instanceof),
                Some('t') => keycomplete(&source[3..], "erface", Keyword::Interface),
                _ => None,
            },
            _ => None,
        },
        Some('l') => keycomplete(&source[1..], "et", Keyword::Let),
        Some('m') => keycomplete(&source[1..], "eta", Keyword::Meta),
        Some('n') => match iter.next() {
            Some('e') => keycomplete(&source[2..], "w", Keyword::New),
            Some('u') => keycomplete(&source[2..], "ll", Keyword::Null),
            _ => None,
        },
        Some('o') => keycomplete(&source[1..], "f", Keyword::Of),
        Some('p') => match iter.next() {
            Some('a') => keycomplete(&source[2..], "ckage", Keyword::Package),
            Some('r') => match iter.next() {
                Some('i') => keycomplete(&source[3..], "vate", Keyword::Private),
                Some('o') => keycomplete(&source[3..], "tected", Keyword::Protected),
                _ => None,
            },
            Some('u') => keycomplete(&source[2..], "blic", Keyword::Public),
            _ => None,
        },
        Some('r') => keycomplete(&source[1..], "eturn", Keyword::Return),
        Some('s') => match iter.next() {
            Some('e') => keycomplete(&source[2..], "t", Keyword::Set),
            Some('t') => keycomplete(&source[2..], "atic", Keyword::Static),
            Some('u') => keycomplete(&source[2..], "per", Keyword::Super),
            Some('w') => keycomplete(&source[2..], "itch", Keyword::Switch),
            _ => None,
        },
        Some('t') => match iter.next() {
            Some('a') => keycomplete(&source[2..], "rget", Keyword::Target),
            Some('h') => match iter.next() {
                Some('i') => keycomplete(&source[3..], "s", Keyword::This),
                Some('r') => keycomplete(&source[3..], "ow", Keyword::Throw),
                _ => None,
            },
            Some('r') => match iter.next() {
                Some('u') => keycomplete(&source[3..], "e", Keyword::True),
                Some('y') => keycomplete(&source[3..], "", Keyword::Try),
                _ => None,
            },
            Some('y') => keycomplete(&source[2..], "peof", Keyword::Typeof),
            _ => None,
        },
        Some('v') => match iter.next() {
            Some('a') => keycomplete(&source[2..], "r", Keyword::Var),
            Some('o') => keycomplete(&source[2..], "id", Keyword::Void),
            _ => None,
        },
        Some('w') => match iter.next() {
            Some('h') => keycomplete(&source[2..], "ile", Keyword::While),
            Some('i') => keycomplete(&source[2..], "th", Keyword::With),
            _ => None,
        },
        Some('y') => keycomplete(&source[1..], "ield", Keyword::Yield),
        _ => None,
    }
}

pub fn identifier_name(scanner: &Scanner, source: &str) -> Result<Option<(Token, Scanner)>, String> {
    // IdentifierName ::
    //    IdentifierStart
    //    IdentifierName IdentifierPart
    // (I.e.: An IdentifierStart followed by any number of IdentifierParts)

    let is_result = identifier_start(scanner, source)?;
    let mut scanner_1;
    match is_result {
        None => return Ok(None),
        Some(scanner) => scanner_1 = scanner,
    };

    loop {
        let ip_result = identifier_part(&scanner_1, source)?;
        match ip_result {
            None => {
                break;
            }
            Some(after) => {
                scanner_1 = after;
            }
        }
    }

    Ok(Some((
        Token::Identifier(IdentifierData {
            string_value: identifier_name_string_value(&source[scanner.start_idx..scanner_1.start_idx]),
            keyword_id: identifier_name_keyword(&source[scanner.start_idx..scanner_1.start_idx]),
            line: scanner.line,
            column: scanner.column,
        }),
        scanner_1,
    )))
}

fn optional_chaining_punctuator(scanner: &Scanner, source: &str) -> Option<(Token, Scanner)> {
    let mut iter = source[scanner.start_idx..].chars();
    match iter.next() {
        Some('?') => match iter.next() {
            Some('.') => match iter.next() {
                Some('0'..='9') => None,
                _ => Some((
                    Token::QDot,
                    Scanner {
                        line: scanner.line,
                        column: scanner.column + 2,
                        start_idx: scanner.start_idx + 2,
                    },
                )),
            },
            _ => None,
        },
        _ => None,
    }
}

fn other_punctuator(scanner: &Scanner, source: &str) -> Option<(Token, Scanner)> {
    let mut iter = source[scanner.start_idx..].chars();
    let mt = |tk, delta| {
        Some((
            tk,
            Scanner {
                line: scanner.line,
                column: scanner.column + delta,
                start_idx: scanner.start_idx + delta as usize,
            },
        ))
    };
    match iter.next() {
        Some('{') => mt(Token::LeftBrace, 1),
        Some('(') => mt(Token::LeftParen, 1),
        Some(')') => mt(Token::RightParen, 1),
        Some('[') => mt(Token::LeftBracket, 1),
        Some(']') => mt(Token::RightBracket, 1),
        Some('.') => match iter.next() {
            Some('.') => match iter.next() {
                Some('.') => mt(Token::Ellipsis, 3),
                _ => mt(Token::Dot, 1),
            },
            _ => mt(Token::Dot, 1),
        },
        Some(';') => mt(Token::Semicolon, 1),
        Some(',') => mt(Token::Comma, 1),
        Some('<') => match iter.next() {
            Some('=') => mt(Token::LtEq, 2),
            Some('<') => match iter.next() {
                Some('=') => mt(Token::LtLtEq, 3),
                _ => mt(Token::LtLt, 2),
            },
            _ => mt(Token::Lt, 1),
        },
        Some('>') => match iter.next() {
            Some('=') => mt(Token::GtEq, 2),
            Some('>') => match iter.next() {
                Some('>') => match iter.next() {
                    Some('=') => mt(Token::GtGtGtEq, 4),
                    _ => mt(Token::GtGtGt, 3),
                },
                Some('=') => mt(Token::GtGtEq, 3),
                _ => mt(Token::GtGt, 2),
            },
            _ => mt(Token::Gt, 1),
        },
        Some('=') => match iter.next() {
            Some('=') => match iter.next() {
                Some('=') => mt(Token::EqEqEq, 3),
                _ => mt(Token::EqEq, 2),
            },
            Some('>') => mt(Token::EqGt, 2),
            _ => mt(Token::Eq, 1),
        },
        Some('!') => match iter.next() {
            Some('=') => match iter.next() {
                Some('=') => mt(Token::BangEqEq, 3),
                _ => mt(Token::BangEq, 2),
            },
            _ => mt(Token::Bang, 1),
        },
        Some('+') => match iter.next() {
            Some('+') => mt(Token::PlusPlus, 2),
            Some('=') => mt(Token::PlusEq, 2),
            _ => mt(Token::Plus, 1),
        },
        Some('-') => match iter.next() {
            Some('-') => mt(Token::MinusMinus, 2),
            Some('=') => mt(Token::MinusEq, 2),
            _ => mt(Token::Minus, 1),
        },
        Some('*') => match iter.next() {
            Some('*') => match iter.next() {
                Some('=') => mt(Token::StarStarEq, 3),
                _ => mt(Token::StarStar, 2),
            },
            Some('=') => mt(Token::StarEq, 2),
            _ => mt(Token::Star, 1),
        },
        Some('&') => match iter.next() {
            Some('&') => match iter.next() {
                Some('=') => mt(Token::AmpAmpEq, 3),
                _ => mt(Token::AmpAmp, 2),
            },
            Some('=') => mt(Token::AmpEq, 2),
            _ => mt(Token::Amp, 1),
        },
        Some('|') => match iter.next() {
            Some('|') => match iter.next() {
                Some('=') => mt(Token::PipePipeEq, 3),
                _ => mt(Token::PipePipe, 2),
            },
            Some('=') => mt(Token::PipeEq, 2),
            _ => mt(Token::Pipe, 1),
        },
        Some('^') => match iter.next() {
            Some('=') => mt(Token::CaretEq, 2),
            _ => mt(Token::Caret, 1),
        },
        Some('~') => mt(Token::Tilde, 1),
        Some('?') => match iter.next() {
            Some('?') => match iter.next() {
                Some('=') => mt(Token::QQEq, 3),
                _ => mt(Token::QQ, 2),
            },
            _ => mt(Token::Question, 1),
        },
        Some(':') => mt(Token::Colon, 1),
        Some('%') => match iter.next() {
            Some('=') => mt(Token::PercentEq, 2),
            _ => mt(Token::Percent, 1),
        },
        _ => None,
    }
}

fn punctuator(scanner: &Scanner, source: &str) -> Option<(Token, Scanner)> {
    optional_chaining_punctuator(scanner, source).or_else(|| other_punctuator(scanner, source))
}

fn decimal_integer_literal(scanner: &Scanner, source: &str) -> Option<Scanner> {
    match match_char(scanner, source, '0') {
        Some(after_zero) => Some(after_zero),
        None => {
            let mut iter = source[scanner.start_idx..].chars();
            let mut after_iter = *scanner;
            match iter.next() {
                Some(c) if c >= '1' && c <= '9' => {
                    after_iter.column = after_iter.column + 1;
                    after_iter.start_idx = after_iter.start_idx + 1;
                    let after_first_digit = after_iter;
                    match iter.next() {
                        Some('_') => {
                            after_iter.column = after_iter.column + 1;
                            after_iter.start_idx = after_iter.start_idx + 1;
                        }
                        _ => {}
                    }
                    match decimal_digits(&after_iter, source, true) {
                        Some(after_remaining) => Some(after_remaining),
                        None => Some(after_first_digit),
                    }
                }
                _ => None,
            }
        }
    }
}

fn match_char(scanner: &Scanner, source: &str, ch: char) -> Option<Scanner> {
    match source[scanner.start_idx..].chars().next() {
        Some(c) if c == ch => Some(Scanner {
            line: scanner.line,
            column: scanner.column + 1,
            start_idx: scanner.start_idx + c.len_utf8(),
        }),
        _ => None,
    }
}

use std::cmp::max;

fn radix_digits<F>(scanner: &Scanner, source: &str, sep: bool, validator: F) -> Option<Scanner>
where
    F: Fn(char) -> bool,
{
    // So: One or more digits. If sep is true, the separator may occur *between*
    // any two digits. (The separator may not be at the beginning nor at the
    // end, nor may there be multiple separators one after the other.)
    let mut latest = *scanner;
    let mut previous_was_digit = false;
    for ch in source[scanner.start_idx..].chars() {
        match ch {
            c if validator(c) => {
                latest.column = latest.column + 1;
                latest.start_idx = latest.start_idx + 1;
                previous_was_digit = true;
            }
            '_' => {
                if sep && previous_was_digit {
                    latest.column = latest.column + 1;
                    latest.start_idx = latest.start_idx + 1;
                    previous_was_digit = false;
                } else {
                    break;
                }
            }
            _ => {
                break;
            }
        }
    }
    if !previous_was_digit && latest.start_idx > scanner.start_idx {
        latest.start_idx = max(scanner.start_idx, latest.start_idx - 1);
        latest.column = max(scanner.column, latest.column - 1);
    }
    if scanner.start_idx == latest.start_idx {
        None
    } else {
        Some(latest)
    }
}

fn decimal_digits(scanner: &Scanner, source: &str, sep: bool) -> Option<Scanner> {
    // DecimalDigits[Sep]::
    //      DecimalDigit
    //      DecimalDigits[?Sep] DecimalDigit
    //      [+Sep]DecimalDigits[+Sep] NumericLiteralSeparator DecimalDigit
    radix_digits(scanner, source, sep, is_digit)
}

fn exponent_indicator(scanner: &Scanner, source: &str) -> Option<Scanner> {
    match_char(scanner, source, 'e').or_else(|| match_char(scanner, source, 'E'))
}

fn signed_integer(scanner: &Scanner, source: &str, sep: bool) -> Option<Scanner> {
    match match_char(scanner, source, '+') {
        Some(after_plus) => decimal_digits(&after_plus, source, sep),
        None => match match_char(scanner, source, '-') {
            Some(after_minus) => decimal_digits(&after_minus, source, sep),
            None => decimal_digits(scanner, source, sep),
        },
    }
}

fn exponent_part(scanner: &Scanner, source: &str, sep: bool) -> Option<Scanner> {
    exponent_indicator(scanner, source).and_then(|r| signed_integer(&r, source, sep))
}

fn decimal_literal(scanner: &Scanner, source: &str) -> Option<Scanner> {
    // DecimalLiteral::
    //      DecimalIntegerLiteral . DecimalDigits[+Sep]opt ExponentPart[+Sep]opt
    //      . DecimalDigits[+Sep] ExponentPart[+Sep]opt
    //      DecimalIntegerLiteral ExponentPart[+Sep]opt
    let mut latest_scanner;
    match decimal_integer_literal(scanner, source) {
        Some(after_leading_digits) => {
            latest_scanner = after_leading_digits;
            if let Some(after_dot) = match_char(&latest_scanner, source, '.') {
                latest_scanner = after_dot;
                if let Some(after_trailing_digits) = decimal_digits(&latest_scanner, source, true) {
                    latest_scanner = after_trailing_digits;
                }
            }
            if let Some(after_exponent) = exponent_part(&latest_scanner, source, true) {
                latest_scanner = after_exponent;
            }
            Some(latest_scanner)
        }
        None => {
            if let Some(after_dot) = match_char(scanner, source, '.') {
                latest_scanner = after_dot;
                if let Some(after_trailing_digits) = decimal_digits(&latest_scanner, source, true) {
                    latest_scanner = after_trailing_digits;
                    if let Some(after_exponent) = exponent_part(&latest_scanner, source, true) {
                        latest_scanner = after_exponent;
                    }
                    Some(latest_scanner)
                } else {
                    None
                }
            } else {
                None
            }
        }
    }
}

fn non_zero_digit(scanner: &Scanner, source: &str) -> Option<Scanner> {
    match source[scanner.start_idx..].chars().next() {
        Some(ch) if ch >= '1' && ch <= '9' => Some(Scanner {
            line: scanner.line,
            column: scanner.column + 1,
            start_idx: scanner.start_idx + 1,
        }),
        _ => None,
    }
}

fn decimal_big_integer_literal(scanner: &Scanner, source: &str) -> Option<Scanner> {
    match_char(scanner, source, '0')
        .and_then(|r| match_char(&r, source, 'n'))
        .or_else(|| {
            non_zero_digit(scanner, source)
                .and_then(|r| decimal_digits(&r, source, true).or_else(|| Some(r)))
                .and_then(|r| match_char(&r, source, 'n'))
        })
        .or_else(|| {
            non_zero_digit(scanner, source)
                .and_then(|r| match_char(&r, source, '_'))
                .and_then(|r| decimal_digits(&r, source, true))
                .and_then(|r| match_char(&r, source, 'n'))
        })
}

fn is_binary_digit(ch: char) -> bool {
    ch == '0' || ch == '1'
}

fn binary_digits(scanner: &Scanner, source: &str, sep: bool) -> Option<Scanner> {
    radix_digits(scanner, source, sep, is_binary_digit)
}

fn binary_integer_literal(scanner: &Scanner, source: &str, sep: bool) -> Option<Scanner> {
    match_char(scanner, source, '0')
        .and_then(|r| match_char(&r, source, 'b').or_else(|| match_char(&r, source, 'B')))
        .and_then(|r| binary_digits(&r, source, sep))
}

fn is_octal_digit(ch: char) -> bool {
    ch >= '0' && ch <= '7'
}

fn octal_digits(scanner: &Scanner, source: &str, sep: bool) -> Option<Scanner> {
    radix_digits(scanner, source, sep, is_octal_digit)
}

fn octal_integer_literal(scanner: &Scanner, source: &str, sep: bool) -> Option<Scanner> {
    match_char(scanner, source, '0')
        .and_then(|r| match_char(&r, source, 'o').or_else(|| match_char(&r, source, 'O')))
        .and_then(|r| octal_digits(&r, source, sep))
}

fn hex_digits(scanner: &Scanner, source: &str, sep: bool) -> Option<Scanner> {
    radix_digits(scanner, source, sep, is_hex_digit)
}

fn hex_integer_literal(scanner: &Scanner, source: &str, sep: bool) -> Option<Scanner> {
    match_char(scanner, source, '0')
        .and_then(|r| match_char(&r, source, 'x').or_else(|| match_char(&r, source, 'X')))
        .and_then(|r| hex_digits(&r, source, sep))
}

fn non_decimal_integer_literal(scanner: &Scanner, source: &str, sep: bool) -> Option<(NumberStyle, Scanner)> {
    binary_integer_literal(scanner, source, sep).map_or_else(
        || {
            octal_integer_literal(scanner, source, sep).map_or_else(
                || hex_integer_literal(scanner, source, sep).map(|r| (NumberStyle::Hex, r)),
                |r| Some((NumberStyle::Octal, r)),
            )
        },
        |r| Some((NumberStyle::Binary, r)),
    )
}

#[derive(Debug, PartialEq, Copy, Clone)]
enum NumberStyle {
    BigDecimal,
    BigBinary,
    BigOctal,
    BigHex,
    Binary,
    Octal,
    Hex,
    Decimal,
}

fn bigify(style: NumberStyle) -> NumberStyle {
    match style {
        NumberStyle::Binary => NumberStyle::BigBinary,
        NumberStyle::Octal => NumberStyle::BigOctal,
        NumberStyle::Hex => NumberStyle::BigHex,
        NumberStyle::Decimal => NumberStyle::BigDecimal,
        x => x,
    }
}

fn int_to_number(src: &str, radix: u32) -> f64 {
    match u64::from_str_radix(src, radix) {
        Ok(x) => x as f64,
        Err(_) => f64::INFINITY,
    }
}

fn strip_sep(src: &str) -> String {
    let mut x = String::with_capacity(src.len());
    for ch in src.chars() {
        if ch != '_' {
            x.push(ch);
        }
    }

    x.shrink_to_fit();
    x
}

fn numeric_literal(scanner: &Scanner, source: &str) -> Option<(Token, Scanner)> {
    let (number_style, after) = non_decimal_integer_literal(scanner, source, true)
        .and_then(|(style, scan)| match_char(&scan, source, 'n').map(|r| (bigify(style), r)))
        .or_else(|| {
            decimal_big_integer_literal(scanner, source).map_or_else(
                || {
                    non_decimal_integer_literal(scanner, source, true)
                        .or_else(|| decimal_literal(scanner, source).map(|r| (NumberStyle::Decimal, r)))
                },
                |r| Some((NumberStyle::BigDecimal, r)),
            )
        })?;

    // Numbers can't be followed immediately by digits or identifiers. "3in" is a syntax error.
    if let Some(ch) = source[after.start_idx..].chars().next() {
        if (ch >= '0' && ch <= '9') || is_unicode_id_start(ch) || ch == '$' || ch == '_' {
            return None;
        }
    }

    match number_style {
        NumberStyle::BigDecimal => Some((
            Token::BigInt(
                BigInt::parse_bytes(
                    strip_sep(&source[scanner.start_idx..after.start_idx - 1]).as_bytes(),
                    10,
                )
                .unwrap(),
            ),
            after,
        )),
        NumberStyle::BigBinary => Some((
            Token::BigInt(
                BigInt::parse_bytes(
                    strip_sep(&source[scanner.start_idx + 2..after.start_idx - 1]).as_bytes(),
                    2,
                )
                .unwrap(),
            ),
            after,
        )),
        NumberStyle::BigOctal => Some((
            Token::BigInt(
                BigInt::parse_bytes(
                    strip_sep(&source[scanner.start_idx + 2..after.start_idx - 1]).as_bytes(),
                    8,
                )
                .unwrap(),
            ),
            after,
        )),
        NumberStyle::BigHex => Some((
            Token::BigInt(
                BigInt::parse_bytes(
                    strip_sep(&source[scanner.start_idx + 2..after.start_idx - 1]).as_bytes(),
                    16,
                )
                .unwrap(),
            ),
            after,
        )),
        NumberStyle::Binary => Some((
            Token::Number(int_to_number(
                &strip_sep(&source[scanner.start_idx + 2..after.start_idx]),
                2,
            )),
            after,
        )),
        NumberStyle::Octal => Some((
            Token::Number(int_to_number(
                &strip_sep(&source[scanner.start_idx + 2..after.start_idx]),
                8,
            )),
            after,
        )),
        NumberStyle::Hex => Some((
            Token::Number(int_to_number(
                &strip_sep(&source[scanner.start_idx + 2..after.start_idx]),
                16,
            )),
            after,
        )),
        NumberStyle::Decimal => Some((
            Token::Number(
                strip_sep(&source[scanner.start_idx..after.start_idx])
                    .parse::<f64>()
                    .unwrap(),
            ),
            after,
        )),
    }
}

fn escape_sequence(scanner: &Scanner, source: &str) -> Option<Scanner> {
    let mut iter = source[scanner.start_idx..].chars();
    match iter.next() {
        // CharacterEscapeSequence
        Some(ch) if is_single_escape_char(ch) || !(is_escape_char(ch) || is_lineterm(ch)) => Some(Scanner {
            line: scanner.line,
            column: scanner.column + 1,
            start_idx: scanner.start_idx + ch.len_utf8(),
        }),
        // 0 [lookahead ∉ DecimalDigit]
        Some('0') => {
            let lookahead = iter.next();
            match lookahead {
                Some(ch) if ch >= '0' && ch <= '9' => None,
                _ => Some(Scanner {
                    line: scanner.line,
                    column: scanner.column + 1,
                    start_idx: scanner.start_idx + 1,
                }),
            }
        }
        // HexEscapeSequence
        Some('x') => {
            let ch1 = iter.next();
            match ch1 {
                Some(digit_1) if is_hex_digit(digit_1) => {
                    let ch2 = iter.next();
                    match ch2 {
                        Some(digit_2) if is_hex_digit(digit_2) => Some(Scanner {
                            line: scanner.line,
                            column: scanner.column + 3,
                            start_idx: scanner.start_idx + 3,
                        }),
                        _ => None,
                    }
                }
                _ => None,
            }
        }
        // UnicodeEscapeSequence
        Some('u') => unicode_escape_sequence(scanner, source),
        _ => None,
    }
}

fn line_terminator_sequence(scanner: &Scanner, source: &str) -> Option<Scanner> {
    let mut iter = source[scanner.start_idx..].chars();
    match iter.next() {
        Some('\r') => match iter.next() {
            Some('\n') => Some(Scanner {
                line: scanner.line + 1,
                column: 1,
                start_idx: scanner.start_idx + 2,
            }),
            _ => Some(Scanner {
                line: scanner.line + 1,
                column: 1,
                start_idx: scanner.start_idx + 1,
            }),
        },
        Some(ch) if ch == '\n' || ch == '\u{2028}' || ch == '\u{2029}' => Some(Scanner {
            line: scanner.line + 1,
            column: 1,
            start_idx: scanner.start_idx + ch.len_utf8(),
        }),
        _ => None,
    }
}

fn string_characters(scanner: &Scanner, source: &str, delim: char) -> Option<Scanner> {
    let mut iter = source[scanner.start_idx..].chars();
    let mut after = *scanner;
    loop {
        match iter.next() {
            Some(ch) if ch != delim && ch != '\\' && ch != '\r' && ch != '\n' => {
                after.column += 1;
                after.start_idx += ch.len_utf8();
            }
            Some('\\') => {
                after.column += 1;
                after.start_idx += 1;
                // This can come back poorly. If it does, this is a broken string, and we should return None.
                let after_escape =
                    escape_sequence(&after, source).or_else(|| line_terminator_sequence(&after, source))?;
                // That probably consumed characters, but our iterator doesn't know that it should have been advanced.
                // So consume some chars here to get us back in sync.
                let mut idx = after.start_idx;
                while idx < after_escape.start_idx {
                    let ch = iter.next().unwrap();
                    idx += ch.len_utf8();
                }
                after = after_escape;
            }
            _ => {
                break;
            }
        }
    }
    if after == *scanner {
        None
    } else {
        Some(after)
    }
}

fn literal_string_value(source: &str) -> JSString {
    let mut result: Vec<u16> = Vec::with_capacity(source.len());
    let mut chars = source.chars().peekable();
    loop {
        let ch = chars.next();
        match ch {
            None => {
                break;
            }
            Some('\\') => {
                let ch2 = chars.next().unwrap(); // Guaranteed not to panic, as string has already been validated.
                match ch2 {
                    '0' => result.push(0),
                    'b' => result.push(8),
                    't' => result.push(9),
                    'n' => result.push(10),
                    'v' => result.push(11),
                    'f' => result.push(12),
                    'r' => result.push(13),
                    '"' => result.push(0x22),
                    '\'' => result.push(0x27),
                    '\\' => result.push(0x5c),
                    'x' => {
                        let digit_1 = chars.next().unwrap().to_digit(16).unwrap() as u16;
                        let digit_2 = chars.next().unwrap().to_digit(16).unwrap() as u16;
                        let value = digit_1 * 16 + digit_2;
                        result.push(value);
                    }
                    'u' => {
                        let ch2 = chars.next().unwrap();
                        match ch2 {
                            '{' => {
                                let mut value: u32 = 0;
                                loop {
                                    let digit = chars.next().unwrap();
                                    if digit == '}' {
                                        result.extend(code_point_to_utf16_code_units(char::from_u32(value).unwrap()));
                                        break;
                                    }
                                    value = value * 16 + digit.to_digit(16).unwrap();
                                }
                            }
                            _ => {
                                let digit_1 = ch2.to_digit(16).unwrap();
                                let digit_2 = chars.next().unwrap().to_digit(16).unwrap();
                                let digit_3 = chars.next().unwrap().to_digit(16).unwrap();
                                let digit_4 = chars.next().unwrap().to_digit(16).unwrap();
                                let value = (digit_1 << 12) | (digit_2 << 8) | (digit_3 << 4) | digit_4;
                                result.push(value as u16);
                            }
                        }
                    }
                    '\n' | '\u{2028}' | '\u{2029}' => (),
                    '\r' => {
                        if let Some('\n') = chars.peek() {
                            chars.next();
                        }
                    }
                    _ => {
                        let mut buf = [0; 2];
                        let coded = ch2.encode_utf16(&mut buf);
                        result.extend_from_slice(coded);
                    }
                }
            }
            Some(c) => {
                let mut buf = [0; 2];
                let coded = c.encode_utf16(&mut buf);
                result.extend_from_slice(coded);
            }
        }
    }

    JSString::take(result)
}

fn string_literal(scanner: &Scanner, source: &str) -> Option<(Token, Scanner)> {
    let after = match_char(scanner, source, '"')
        .and_then(|r| string_characters(&r, source, '"').or_else(|| Some(r)))
        .and_then(|r| match_char(&r, source, '"'))
        .or_else(|| {
            match_char(scanner, source, '\'')
                .and_then(|r| string_characters(&r, source, '\'').or_else(|| Some(r)))
                .and_then(|r| match_char(&r, source, '\''))
        })?;
    let start_idx = scanner.start_idx + 1;
    let after_idx = after.start_idx - 1;
    assert!(after_idx >= start_idx);
    let value = literal_string_value(&source[start_idx..after_idx]);

    Some((Token::String(value), after))
}

fn template(scanner: &Scanner, source: &str) -> Option<(Token, Scanner)> {
    let after = match_char(scanner, source, '`')?;
    todo!();
}

fn common_token(scanner: &Scanner, source: &str) -> Result<Option<(Token, Scanner)>, String> {
    let mut r;
    r = identifier_name(scanner, source)?;
    if r.is_none() {
        r = numeric_literal(scanner, source);
        if r.is_none() {
            r = punctuator(scanner, source);
            if r.is_none() {
                r = string_literal(scanner, source);
                if r.is_none() {
                    r = template(scanner, source);
                }
            }
        }
    }
    Ok(r)
}

fn div_punctuator(scanner: &Scanner, source: &str, goal: ScanGoal) -> Option<(Token, Scanner)> {
    if goal == ScanGoal::InputElementDiv || goal == ScanGoal::InputElementTemplateTail {
        let mut iter = source[scanner.start_idx..].chars();
        match iter.next() {
            Some('/') => match iter.next() {
                Some('=') => Some((
                    Token::SlashEq,
                    Scanner {
                        line: scanner.line,
                        column: scanner.column + 2,
                        start_idx: scanner.start_idx + 2,
                    },
                )),
                _ => Some((
                    Token::Slash,
                    Scanner {
                        line: scanner.line,
                        column: scanner.column + 1,
                        start_idx: scanner.start_idx + 1,
                    },
                )),
            },
            _ => None,
        }
    } else {
        None
    }
}

fn right_brace_punctuator(scanner: &Scanner, source: &str, goal: ScanGoal) -> Option<(Token, Scanner)> {
    if goal == ScanGoal::InputElementDiv || goal == ScanGoal::InputElementRegExp {
        let ch = source[scanner.start_idx..].chars().next();
        if ch == Some('}') {
            Some((
                Token::RightBrace,
                Scanner {
                    line: scanner.line,
                    column: scanner.column + 1,
                    start_idx: scanner.start_idx + 1,
                },
            ))
        } else {
            None
        }
    } else {
        None
    }
}
fn regular_expression_literal(
    scanner: &Scanner,
    source: &str,
    goal: ScanGoal,
) -> Result<Option<(Token, Scanner)>, String> {
    if goal == ScanGoal::InputElementRegExp || goal == ScanGoal::InputElementRegExpOrTemplateTail {
        let ch = source[scanner.start_idx..].chars().next();
        if ch == Some('/') {
            todo!();
        } else {
            Ok(None)
        }
    } else {
        Ok(None)
    }
}
fn template_substitution_tail(
    scanner: &Scanner,
    source: &str,
    goal: ScanGoal,
) -> Result<Option<(Token, Scanner)>, String> {
    if goal == ScanGoal::InputElementRegExpOrTemplateTail || goal == ScanGoal::InputElementTemplateTail {
        let ch = source[scanner.start_idx..].chars().next();
        if ch == Some('}') {
            todo!();
        } else {
            Ok(None)
        }
    } else {
        Ok(None)
    }
}

pub fn scan_token(scanner: &Scanner, source: &str, goal: ScanGoal) -> Result<(Token, Scanner), String> {
    let after_skippable = skip_skippables(scanner, source)?;
    if after_skippable.start_idx >= source.len() {
        return Ok((Token::Eof, after_skippable));
    }

    let mut r;
    r = common_token(&after_skippable, source)?;
    if r.is_none() {
        r = div_punctuator(&after_skippable, source, goal);
        if r.is_none() {
            r = right_brace_punctuator(&after_skippable, source, goal);
            if r.is_none() {
                r = regular_expression_literal(&after_skippable, source, goal)?;
                if r.is_none() {
                    r = template_substitution_tail(&after_skippable, source, goal)?;
                    if r.is_none() {
                        return Err(format!(
                            "{}:{}: Invalid or Unexpected token",
                            after_skippable.line, after_skippable.column
                        ));
                    }
                }
            }
        }
    }
    Ok(r.unwrap())
}

#[cfg(test)]
mod tests {
    use super::*;
    use num::traits::Zero;

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
        let scanner = Scanner {
            line: 10,
            column: 80,
            start_idx: 0,
        };
        let result = skip_skippables(&scanner, "");
        let expected = Scanner {
            start_idx: 0,
            line: 10,
            column: 80,
        };
        assert_eq!(result, Ok(expected));
    }
    #[test]
    fn skippables_no_leading_whitespace() {
        let scanner = Scanner {
            line: 10,
            column: 80,
            start_idx: 0,
        };
        let result = skip_skippables(&scanner, "abcd   uu");
        let expected = Scanner {
            start_idx: 0,
            line: 10,
            column: 80,
        };
        assert_eq!(result, Ok(expected));
    }
    #[test]
    fn skippables_only_whitespace() {
        let result = skip_skippables(
            &Scanner {
                line: 1,
                column: 1,
                start_idx: 0,
            },
            "\t\r\n\t\t\t",
        );
        assert_eq!(
            result,
            Ok(Scanner {
                start_idx: 6,
                line: 2,
                column: 4
            })
        );
    }
    #[test]
    fn skippables_ends_on_eol() {
        let result = skip_skippables(
            &Scanner {
                start_idx: 0,
                line: 2,
                column: 2,
            },
            "\n",
        );
        assert_eq!(
            result,
            Ok(Scanner {
                start_idx: 1,
                line: 3,
                column: 1
            })
        );
    }
    #[test]
    fn skippables_ends_on_crlf() {
        let result = skip_skippables(
            &Scanner {
                start_idx: 0,
                line: 10,
                column: 1,
            },
            "\r\n",
        );
        assert_eq!(
            result,
            Ok(Scanner {
                start_idx: 2,
                line: 11,
                column: 1
            })
        );
    }
    #[test]
    fn skippables_ends_on_slash() {
        let result = skip_skippables(
            &Scanner {
                start_idx: 0,
                line: 10,
                column: 10,
            },
            "   /",
        );
        assert_eq!(
            result,
            Ok(Scanner {
                start_idx: 3,
                line: 10,
                column: 13
            })
        );
    }
    #[test]
    fn skippables_ends_in_doubleslash() {
        let result = skip_skippables(
            &Scanner {
                start_idx: 0,
                line: 3,
                column: 2,
            },
            "\t\t//",
        );
        assert_eq!(
            result,
            Ok(Scanner {
                start_idx: 4,
                line: 3,
                column: 6
            })
        );
    }
    #[test]
    fn skippables_not_comment() {
        let result = skip_skippables(
            &Scanner {
                start_idx: 0,
                line: 3,
                column: 2,
            },
            "/+",
        );
        assert_eq!(
            result,
            Ok(Scanner {
                start_idx: 0,
                line: 3,
                column: 2
            })
        );
    }
    #[test]
    fn skippables_ends_in_slc() {
        let result = skip_skippables(
            &Scanner {
                start_idx: 0,
                line: 3,
                column: 2,
            },
            "\t\t// this ends the line",
        );
        assert_eq!(
            result,
            Ok(Scanner {
                start_idx: 23,
                line: 3,
                column: 25
            })
        );
    }
    #[test]
    fn skippables_slc_then_white() {
        let result = skip_skippables(
            &Scanner {
                start_idx: 0,
                line: 3,
                column: 2,
            },
            "\t\t// this ends the line\r\n\r\nblue",
        );
        assert_eq!(
            result,
            Ok(Scanner {
                start_idx: 27,
                line: 5,
                column: 1
            })
        );
    }
    #[test]
    fn skippables_mlc() {
        let result = skip_skippables(
            &Scanner {
                start_idx: 0,
                line: 3,
                column: 2,
            },
            "/**/",
        );
        assert_eq!(
            result,
            Ok(Scanner {
                start_idx: 4,
                line: 3,
                column: 6
            })
        );
    }
    #[test]
    fn skippables_mlc_then_white() {
        let result = skip_skippables(
            &Scanner {
                start_idx: 0,
                line: 3,
                column: 2,
            },
            "/* abcde */\t\t\nhat",
        );
        assert_eq!(
            result,
            Ok(Scanner {
                start_idx: 14,
                line: 4,
                column: 1
            })
        );
    }
    #[test]
    fn skippables_mlc_with_newlines() {
        let result = skip_skippables(
            &Scanner {
                start_idx: 0,
                line: 3,
                column: 1,
            },
            "/*\n * My Title\n */\nscarf",
        );
        assert_eq!(
            result,
            Ok(Scanner {
                start_idx: 19,
                line: 6,
                column: 1
            })
        );
    }
    #[test]
    fn skippables_unterminated_mlc() {
        let result = skip_skippables(
            &Scanner {
                start_idx: 0,
                line: 3,
                column: 2,
            },
            "/*\t\t// this ends the line\r\n\r\nblue",
        );
        assert_eq!(
            result,
            Err(String::from(
                "Unterminated /*-style comment. Started on line 3, column 2."
            ))
        );
    }
    #[test]
    fn skippables_mlc_eof1() {
        let result = skip_skippables(
            &Scanner {
                start_idx: 0,
                line: 3,
                column: 2,
            },
            "/*",
        );
        assert_eq!(
            result,
            Err(String::from(
                "Unterminated /*-style comment. Started on line 3, column 2."
            ))
        );
    }
    #[test]
    fn skippables_mlc_eof2() {
        let result = skip_skippables(
            &Scanner {
                start_idx: 0,
                line: 3,
                column: 2,
            },
            "/***",
        );
        assert_eq!(
            result,
            Err(String::from(
                "Unterminated /*-style comment. Started on line 3, column 2."
            ))
        );
    }
    #[test]
    fn skippables_mlc_eof3() {
        let result = skip_skippables(
            &Scanner {
                start_idx: 0,
                line: 3,
                column: 2,
            },
            "/***\n",
        );
        assert_eq!(
            result,
            Err(String::from(
                "Unterminated /*-style comment. Started on line 3, column 2."
            ))
        );
    }
    #[test]
    fn skippables_mlc_eof4() {
        let result = skip_skippables(
            &Scanner {
                start_idx: 0,
                line: 3,
                column: 2,
            },
            "/***\r\n",
        );
        assert_eq!(
            result,
            Err(String::from(
                "Unterminated /*-style comment. Started on line 3, column 2."
            ))
        );
    }
    #[test]
    fn new_simple() {
        let result = Scanner::new();
        assert_eq!(
            result,
            Scanner {
                start_idx: 0,
                line: 1,
                column: 1
            }
        );
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
        assert_eq!(
            result,
            Some(Scanner {
                line: 1,
                column: 5,
                start_idx: 4
            })
        );
    }
    #[test]
    fn hex_four_digits_10() {
        let result = hex_four_digits(&Scanner::new(), "00896661");
        assert_eq!(
            result,
            Some(Scanner {
                line: 1,
                column: 5,
                start_idx: 4
            })
        );
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
        assert_eq!(
            result,
            Some(Scanner {
                line: 1,
                column: 2,
                start_idx: 1
            })
        );
    }
    #[test]
    fn code_point_03() {
        let result = code_point(&Scanner::new(), "ffff");
        assert_eq!(
            result,
            Some(Scanner {
                line: 1,
                column: 5,
                start_idx: 4
            })
        );
    }
    #[test]
    fn code_point_04() {
        let result = code_point(&Scanner::new(), "fffff");
        assert_eq!(
            result,
            Some(Scanner {
                line: 1,
                column: 6,
                start_idx: 5
            })
        );
    }
    #[test]
    fn code_point_05() {
        let result = code_point(&Scanner::new(), "10ffff");
        assert_eq!(
            result,
            Some(Scanner {
                line: 1,
                column: 7,
                start_idx: 6
            })
        );
    }
    #[test]
    fn code_point_06() {
        let result = code_point(&Scanner::new(), "110000");
        assert_eq!(result, None);
    }
    #[test]
    fn code_point_07() {
        let result = code_point(&Scanner::new(), "0000000098");
        assert_eq!(
            result,
            Some(Scanner {
                line: 1,
                column: 11,
                start_idx: 10
            })
        );
    }
    #[test]
    fn code_point_08() {
        let result = code_point(&Scanner::new(), "000000000000000000000000000000000000000098");
        assert_eq!(
            result,
            Some(Scanner {
                line: 1,
                column: 43,
                start_idx: 42
            })
        );
    }
    #[test]
    fn code_point_09() {
        let result = code_point(&Scanner::new(), "A000000000000000000000000000000000000000098");
        assert_eq!(result, None);
    }
    #[test]
    fn unicode_escape_sequence_01() {
        for s in &[
            "",
            "g",
            "u",
            "ug",
            "u0",
            "u0g",
            "u00",
            "u00g",
            "u000",
            "u000g",
            "u{",
            "u{0g",
            "u{}",
            "u{1234567890}",
        ] {
            assert_eq!(unicode_escape_sequence(&Scanner::new(), s), None);
        }
    }
    #[test]
    fn unicode_escape_sequence_02() {
        assert_eq!(
            unicode_escape_sequence(&Scanner::new(), "u0067"),
            Some(Scanner {
                line: 1,
                column: 6,
                start_idx: 5
            })
        );
    }
    #[test]
    fn unicode_escape_sequence_03() {
        assert_eq!(
            unicode_escape_sequence(&Scanner::new(), "u{0067}"),
            Some(Scanner {
                line: 1,
                column: 8,
                start_idx: 7
            })
        );
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
            assert_eq!(
                identifier_start(&Scanner::new(), s),
                Ok(Some(Scanner {
                    line: 1,
                    column: 2,
                    start_idx: 1
                }))
            )
        }
        assert_eq!(
            identifier_start(&Scanner::new(), "ꘐblue"),
            Ok(Some(Scanner {
                line: 1,
                column: 2,
                start_idx: 3
            }))
        );
    }
    #[test]
    fn identifier_start_03() {
        assert_eq!(
            identifier_start(&Scanner::new(), "\\u0061blue"),
            Ok(Some(Scanner {
                line: 1,
                column: 7,
                start_idx: 6
            }))
        );
    }
    #[test]
    fn identifier_start_04() {
        assert_eq!(
            identifier_start(&Scanner::new(), "\\u0024blue"),
            Ok(Some(Scanner {
                line: 1,
                column: 7,
                start_idx: 6
            }))
        );
    }
    #[test]
    fn identifier_start_05() {
        assert_eq!(
            identifier_start(&Scanner::new(), "\\u005fblue"),
            Ok(Some(Scanner {
                line: 1,
                column: 7,
                start_idx: 6
            }))
        );
    }
    #[test]
    fn identifier_start_06() {
        assert_eq!(
            identifier_start(&Scanner::new(), "\\u0095blue"),
            Err(String::from("1:1: Invalid Identifier Start Character '\\u{95}'"))
        );
    }

    #[test]
    fn radix_digits_01() {
        assert_eq!(
            radix_digits(&Scanner::new(), "43", false, is_digit),
            Some(Scanner {
                line: 1,
                column: 3,
                start_idx: 2
            })
        );
    }
    #[test]
    fn radix_digits_02() {
        assert_eq!(
            radix_digits(&Scanner::new(), "4_3", false, is_digit),
            Some(Scanner {
                line: 1,
                column: 2,
                start_idx: 1
            })
        );
    }
    #[test]
    fn radix_digits_03() {
        assert_eq!(
            radix_digits(&Scanner::new(), "43_", false, is_digit),
            Some(Scanner {
                line: 1,
                column: 3,
                start_idx: 2
            })
        );
    }
    #[test]
    fn radix_digits_04() {
        assert_eq!(radix_digits(&Scanner::new(), "_43", false, is_digit), None);
    }
    #[test]
    fn radix_digits_05() {
        assert_eq!(
            radix_digits(&Scanner::new(), "43", true, is_digit),
            Some(Scanner {
                line: 1,
                column: 3,
                start_idx: 2
            })
        );
    }
    #[test]
    fn radix_digits_06() {
        assert_eq!(
            radix_digits(&Scanner::new(), "4_3", true, is_digit),
            Some(Scanner {
                line: 1,
                column: 4,
                start_idx: 3
            })
        );
    }
    #[test]
    fn radix_digits_07() {
        assert_eq!(
            radix_digits(&Scanner::new(), "43_", true, is_digit),
            Some(Scanner {
                line: 1,
                column: 3,
                start_idx: 2
            })
        );
    }
    #[test]
    fn radix_digits_08() {
        assert_eq!(radix_digits(&Scanner::new(), "_43", true, is_digit), None);
    }
    #[test]
    fn radix_digits_09() {
        assert_eq!(
            radix_digits(&Scanner::new(), "4__3", true, is_digit),
            Some(Scanner {
                line: 1,
                column: 2,
                start_idx: 1
            })
        );
    }
    #[test]
    fn radix_digits_10() {
        assert_eq!(
            radix_digits(&Scanner::new(), "4__3", false, is_digit),
            Some(Scanner {
                line: 1,
                column: 2,
                start_idx: 1
            })
        );
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
        assert_eq!(
            decimal_integer_literal(&Scanner::new(), "0"),
            Some(Scanner {
                line: 1,
                column: 2,
                start_idx: 1
            })
        )
    }
    #[test]
    fn decimal_integer_4() {
        assert_eq!(
            decimal_integer_literal(&Scanner::new(), "4"),
            Some(Scanner {
                line: 1,
                column: 2,
                start_idx: 1
            })
        )
    }
    #[test]
    fn decimal_integer_4_3() {
        assert_eq!(
            decimal_integer_literal(&Scanner::new(), "4_3"),
            Some(Scanner {
                line: 1,
                column: 4,
                start_idx: 3
            })
        )
    }
    #[test]
    fn decimal_integer_43() {
        assert_eq!(
            decimal_integer_literal(&Scanner::new(), "43"),
            Some(Scanner {
                line: 1,
                column: 3,
                start_idx: 2
            })
        )
    }
    #[test]
    fn decimal_integer_56_() {
        assert_eq!(
            decimal_integer_literal(&Scanner::new(), "56_"),
            Some(Scanner {
                line: 1,
                column: 3,
                start_idx: 2
            })
        )
    }
    #[test]
    fn non_decimal_integer_literal_01() {
        assert_eq!(
            non_decimal_integer_literal(&Scanner::new(), "0x10", true),
            Some((
                NumberStyle::Hex,
                Scanner {
                    line: 1,
                    column: 5,
                    start_idx: 4
                }
            ))
        )
    }
    #[test]
    fn numeric_literal_01() {
        assert_eq!(
            numeric_literal(&Scanner::new(), "0x10..."),
            Some((
                Token::Number(16.0),
                Scanner {
                    line: 1,
                    column: 5,
                    start_idx: 4
                }
            ))
        )
    }
    #[test]
    fn numeric_literal_02() {
        assert_eq!(
            numeric_literal(&Scanner::new(), ".25"),
            Some((
                Token::Number(0.25),
                Scanner {
                    line: 1,
                    column: 4,
                    start_idx: 3
                }
            ))
        )
    }
    #[test]
    fn numeric_literal_03() {
        assert_eq!(
            numeric_literal(&Scanner::new(), "0xabcdef"),
            Some((
                Token::Number(11259375.0),
                Scanner {
                    line: 1,
                    column: 9,
                    start_idx: 8
                }
            ))
        )
    }
    #[test]
    fn numeric_literal_04() {
        assert_eq!(
            numeric_literal(&Scanner::new(), "0xFEDCBA"),
            Some((
                Token::Number(16702650.0),
                Scanner {
                    line: 1,
                    column: 9,
                    start_idx: 8
                }
            ))
        )
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

        assert_eq!(
            result,
            Some((
                Token::BigInt(expected),
                Scanner {
                    line: 1,
                    column: 37,
                    start_idx: 36
                }
            ))
        );
    }
    #[test]
    fn numeric_literal_07() {
        let result = numeric_literal(&Scanner::new(), "0b1_1100_0111n");
        assert_eq!(
            result,
            Some((
                Token::BigInt(BigInt::zero() + 0x1c7),
                Scanner {
                    line: 1,
                    column: 15,
                    start_idx: 14
                }
            ))
        );
    }
    #[test]
    fn numeric_literal_08() {
        let result = numeric_literal(&Scanner::new(), "0o3_4576_1000n");
        assert_eq!(
            result,
            Some((
                Token::BigInt(BigInt::zero() + 0x0397e200),
                Scanner {
                    line: 1,
                    column: 15,
                    start_idx: 14
                }
            ))
        );
    }
    #[test]
    fn numeric_literal_09() {
        let result = numeric_literal(&Scanner::new(), "0x4576_1000n");
        assert_eq!(
            result,
            Some((
                Token::BigInt(BigInt::zero() + 0x45761000),
                Scanner {
                    line: 1,
                    column: 13,
                    start_idx: 12
                }
            ))
        );
    }
    #[test]
    fn numeric_literal_10() {
        let result = numeric_literal(&Scanner::new(), "0b1010_1111_0010_0110");
        assert_eq!(
            result,
            Some((
                Token::Number(44838.0),
                Scanner {
                    line: 1,
                    column: 22,
                    start_idx: 21
                }
            ))
        );
    }
    #[test]
    fn numeric_literal_11() {
        let result = numeric_literal(&Scanner::new(), "0o7773153152");
        assert_eq!(
            result,
            Some((
                Token::Number(1072485994.0),
                Scanner {
                    line: 1,
                    column: 13,
                    start_idx: 12
                }
            ))
        );
    }

    #[test]
    fn bad_hex_char() {
        assert_eq!(
            HexChar::try_from('&'),
            Err("HexChar can only be used with hexidecimal digits!")
        );
    }
    #[test]
    fn hex_char_debug_fmt() {
        let hc = HexChar('F');
        let result = format!("{:?}", hc);
        assert_eq!(result, "HexChar('F')");
    }
    fn hex_char_partial_eq() {
        let hc1 = HexChar('1');
        let hc2 = HexChar('9');
        let hc3 = HexChar('1');
        assert_eq!(hc1, hc3);
        assert_ne!(hc1, hc2);
    }
    #[test]
    fn scan_numeric() {
        let result = scan_token(&Scanner::new(), ".25", ScanGoal::InputElementRegExp);
        assert_eq!(
            result,
            Ok((
                Token::Number(0.25),
                Scanner {
                    line: 1,
                    column: 4,
                    start_idx: 3
                }
            ))
        );
    }
    #[test]
    fn scan_token_id_01() {
        let result = scan_token(&Scanner::new(), "\\u004Abc", ScanGoal::InputElementRegExp);
        assert_eq!(
            result,
            Ok((
                Token::Identifier(IdentifierData {
                    string_value: JSString::from("Jbc"),
                    keyword_id: None,
                    line: 1,
                    column: 1
                }),
                Scanner {
                    line: 1,
                    column: 9,
                    start_idx: 8
                }
            ))
        );
    }
    fn keyword_test_helper(inp: &str, expected: Option<Keyword>) {
        let result = scan_token(&Scanner::new(), inp, ScanGoal::InputElementRegExp);
        assert_eq!(
            result,
            Ok((
                Token::Identifier(IdentifierData {
                    string_value: JSString::from(inp),
                    keyword_id: expected,
                    line: 1,
                    column: 1
                }),
                Scanner {
                    line: 1,
                    column: inp.len() as u32 + 1,
                    start_idx: inp.len()
                }
            ))
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
        keyword_test_helper("exhale", None);
        keyword_test_helper("felt", None);
        keyword_test_helper("impulse", None);
        keyword_test_helper("imbalance", None);
        keyword_test_helper("inline", None);
        keyword_test_helper("natural", None);
        keyword_test_helper("prattle", None);
        keyword_test_helper("pebble", None);
        keyword_test_helper("saturate", None);
        keyword_test_helper("that", None);
        keyword_test_helper("tree", None);
        keyword_test_helper("test", None);
        keyword_test_helper("very", None);
        keyword_test_helper("werewolf", None);
    }
    #[test]
    fn optional_chaining_test_01() {
        let result = scan_token(&Scanner::new(), "?.", ScanGoal::InputElementRegExp);
        assert_eq!(
            result,
            Ok((
                Token::QDot,
                Scanner {
                    line: 1,
                    column: 3,
                    start_idx: 2
                }
            ))
        );
    }
    #[test]
    fn optional_chaining_test_02() {
        let result = scan_token(&Scanner::new(), "?.P", ScanGoal::InputElementRegExp);
        assert_eq!(
            result,
            Ok((
                Token::QDot,
                Scanner {
                    line: 1,
                    column: 3,
                    start_idx: 2
                }
            ))
        );
    }
    #[test]
    fn optional_chaining_test_03() {
        let result = scan_token(&Scanner::new(), "?.999", ScanGoal::InputElementRegExp);
        assert_eq!(
            result,
            Ok((
                Token::Question,
                Scanner {
                    line: 1,
                    column: 2,
                    start_idx: 1
                }
            ))
        );
    }
    #[test]
    fn optional_chaining_test_04() {
        let result = scan_token(&Scanner::new(), "?mulberry", ScanGoal::InputElementRegExp);
        assert_eq!(
            result,
            Ok((
                Token::Question,
                Scanner {
                    line: 1,
                    column: 2,
                    start_idx: 1
                }
            ))
        );
    }
    fn punct_check(inp: &str, tok: Token) {
        let result = scan_token(&Scanner::new(), inp, ScanGoal::InputElementRegExp);
        assert_eq!(
            result,
            Ok((
                tok,
                Scanner {
                    line: 1,
                    column: inp.chars().count() as u32 + 1,
                    start_idx: inp.len()
                }
            ))
        );
    }
    #[test]
    fn punctuator_validiation() {
        punct_check("{", Token::LeftBrace);
        punct_check("(", Token::LeftParen);
        punct_check(")", Token::RightParen);
        punct_check("[", Token::LeftBracket);
        punct_check("]", Token::RightBracket);
        punct_check(".", Token::Dot);
        punct_check(";", Token::Semicolon);
        punct_check(",", Token::Comma);
        punct_check("<", Token::Lt);
        punct_check(">", Token::Gt);
        punct_check("=", Token::Eq);
        punct_check("!", Token::Bang);
        punct_check("+", Token::Plus);
        punct_check("-", Token::Minus);
        punct_check("*", Token::Star);
        punct_check("&", Token::Amp);
        punct_check("|", Token::Pipe);
        punct_check("^", Token::Caret);
        punct_check("~", Token::Tilde);
        punct_check("?", Token::Question);
        punct_check(":", Token::Colon);
        punct_check("%", Token::Percent);
        punct_check("...", Token::Ellipsis);
        punct_check("<=", Token::LtEq);
        punct_check("<<=", Token::LtLtEq);
        punct_check("<<", Token::LtLt);
        punct_check(">>", Token::GtGt);
        punct_check(">>>", Token::GtGtGt);
        punct_check(">>>=", Token::GtGtGtEq);
        punct_check(">>=", Token::GtGtEq);
        punct_check("===", Token::EqEqEq);
        punct_check("==", Token::EqEq);
        punct_check("=>", Token::EqGt);
        punct_check("!=", Token::BangEq);
        punct_check("+=", Token::PlusEq);
        punct_check("-=", Token::MinusEq);
        punct_check("**=", Token::StarStarEq);
        punct_check("**", Token::StarStar);
        punct_check("*=", Token::StarEq);
        punct_check("&&=", Token::AmpAmpEq);
        punct_check("&&", Token::AmpAmp);
        punct_check("&=", Token::AmpEq);
        punct_check("||=", Token::PipePipeEq);
        punct_check("||", Token::PipePipe);
        punct_check("|=", Token::PipeEq);
        punct_check("^=", Token::CaretEq);
        punct_check("??=", Token::QQEq);
        punct_check("??", Token::QQ);
    }

    fn punct_chk2(inp: &str, tok: Token, consumed: u32) {
        let result = scan_token(&Scanner::new(), inp, ScanGoal::InputElementRegExp);
        assert_eq!(
            result,
            Ok((
                tok,
                Scanner {
                    line: 1,
                    column: consumed + 1,
                    start_idx: consumed as usize
                }
            ))
        );
    }

    #[test]
    fn punctuator_shortscans() {
        punct_chk2("..A", Token::Dot, 1);
    }
    #[test]
    fn punctuator_nomatch() {
        let result = scan_token(&Scanner::new(), "@", ScanGoal::InputElementRegExp);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "1:1: Invalid or Unexpected token");
    }
    #[test]
    fn signed_integer_01() {
        let result = signed_integer(&Scanner::new(), "blue", true);
        assert_eq!(result, None);
    }
    #[test]
    fn signed_integer_02() {
        let result = signed_integer(&Scanner::new(), "+10_9", false);
        assert_eq!(
            result,
            Some(Scanner {
                line: 1,
                column: 4,
                start_idx: 3
            })
        );
    }
    #[test]
    fn signed_integer_03() {
        let result = signed_integer(&Scanner::new(), "-3_3", true);
        assert_eq!(
            result,
            Some(Scanner {
                line: 1,
                column: 5,
                start_idx: 4
            })
        );
    }
    #[test]
    fn signed_integer_04() {
        let result = signed_integer(&Scanner::new(), "899", false);
        assert_eq!(
            result,
            Some(Scanner {
                line: 1,
                column: 4,
                start_idx: 3
            })
        );
    }

    fn decimal_literal_helper(text: &str, count: u32) {
        let result = decimal_literal(&Scanner::new(), text);
        assert_eq!(
            result,
            Some(Scanner {
                line: 1,
                column: count + 1,
                start_idx: count as usize
            })
        );
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
        assert_eq!(
            non_zero_digit(&Scanner::new(), text),
            Some(Scanner {
                line: 1,
                column: 2,
                start_idx: 1
            })
        );
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
        assert_eq!(
            decimal_big_integer_literal(&Scanner::new(), text),
            Some(Scanner {
                line: 1,
                column: text.len() as u32 + 1,
                start_idx: text.len()
            })
        );
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
        assert_eq!(
            int_to_number(
                "9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999",
                10
            ),
            f64::INFINITY
        );
    }

    #[test]
    fn escape_sequence_test() {
        let singles = ['\'', '"', '\\', 'b', 'f', 'n', 'r', 't', 'v', 'a', 'Q', '😊'];
        for ch in singles.iter() {
            assert_eq!(
                escape_sequence(&Scanner::new(), &String::from(*ch)),
                Some(Scanner {
                    line: 1,
                    column: 2,
                    start_idx: ch.len_utf8()
                })
            );
        }
        assert_eq!(
            escape_sequence(&Scanner::new(), "0--"),
            Some(Scanner {
                line: 1,
                column: 2,
                start_idx: 1
            })
        );
        assert_eq!(
            escape_sequence(&Scanner::new(), "x7b7b"),
            Some(Scanner {
                line: 1,
                column: 4,
                start_idx: 3
            })
        );
        assert_eq!(
            escape_sequence(&Scanner::new(), "u3333xxx"),
            Some(Scanner {
                line: 1,
                column: 6,
                start_idx: 5
            })
        );
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
            assert_eq!(
                result,
                Some(Scanner {
                    line: 2,
                    column: 1,
                    start_idx: lt.len(),
                })
            );
        }
        assert!(line_terminator_sequence(&Scanner::new(), "blue").is_none());
        assert_eq!(
            line_terminator_sequence(&Scanner::new(), "\rblue"),
            Some(Scanner {
                line: 2,
                column: 1,
                start_idx: 1,
            })
        );
    }

    #[test]
    fn string_characters_test() {
        let result = string_characters(&Scanner::new(), "a\\n\\u{2029}'", '\'');
        assert_eq!(
            result,
            Some(Scanner {
                line: 1,
                column: 12,
                start_idx: 11
            })
        );

        assert!(string_characters(&Scanner::new(), "\\u{3", '\'').is_none());
        assert!(string_characters(&Scanner::new(), "", '\'').is_none());
        assert_eq!(
            string_characters(&Scanner::new(), "1\\\n2\\\n3'\n", '\''),
            Some(Scanner {
                line: 3,
                column: 2,
                start_idx: 7
            })
        );
    }

    #[test]
    fn literal_string_value_test() {
        let s = literal_string_value(
            "a\\a\\b\\t\\n\\v\\f\\r\\'\\\"\\\\\\x66\\u{211c}\\u211d\\\n\\\u{2028}\\\u{2029}\\\r\\\n\\\r\n\\0",
        );
        assert_eq!(s, "aa\u{8}\t\n\u{b}\u{c}\r'\"\\f\u{211c}\u{211d}\u{0}");
    }

    #[test]
    fn string_literal_test() {
        assert_eq!(string_literal(&Scanner::new(), "not_a_string"), None);
        assert_eq!(
            string_literal(&Scanner::new(), "''"),
            Some((
                Token::String(JSString::from("")),
                Scanner {
                    line: 1,
                    column: 3,
                    start_idx: 2
                }
            ))
        );
        assert_eq!(
            string_literal(&Scanner::new(), "\"\""),
            Some((
                Token::String(JSString::from("")),
                Scanner {
                    line: 1,
                    column: 3,
                    start_idx: 2
                }
            ))
        );
        assert_eq!(
            string_literal(&Scanner::new(), "'abcd'"),
            Some((
                Token::String(JSString::from("abcd")),
                Scanner {
                    line: 1,
                    column: 7,
                    start_idx: 6
                }
            ))
        );
        assert_eq!(
            string_literal(&Scanner::new(), "\"abcd\""),
            Some((
                Token::String(JSString::from("abcd")),
                Scanner {
                    line: 1,
                    column: 7,
                    start_idx: 6
                }
            ))
        );
    }

    #[test]
    fn template_test() {
        assert_eq!(template(&Scanner::new(), "Q"), None);
    }
    #[should_panic]
    #[test]
    fn template_test_panic() {
        template(&Scanner::new(), "``");
    }

    #[test]
    fn div_punctuator_test() {
        assert_eq!(
            div_punctuator(&Scanner::new(), "/", ScanGoal::InputElementDiv),
            Some((
                Token::Slash,
                Scanner {
                    line: 1,
                    column: 2,
                    start_idx: 1
                }
            ))
        );
        assert_eq!(div_punctuator(&Scanner::new(), "/", ScanGoal::InputElementRegExp), None);
        assert_eq!(
            div_punctuator(&Scanner::new(), "/", ScanGoal::InputElementRegExpOrTemplateTail),
            None
        );
        assert_eq!(
            div_punctuator(&Scanner::new(), "/", ScanGoal::InputElementTemplateTail),
            Some((
                Token::Slash,
                Scanner {
                    line: 1,
                    column: 2,
                    start_idx: 1
                }
            ))
        );
        assert_eq!(
            div_punctuator(&Scanner::new(), "/=", ScanGoal::InputElementDiv),
            Some((
                Token::SlashEq,
                Scanner {
                    line: 1,
                    column: 3,
                    start_idx: 2
                }
            ))
        );
        assert_eq!(
            div_punctuator(&Scanner::new(), "/=", ScanGoal::InputElementRegExp),
            None
        );
        assert_eq!(
            div_punctuator(&Scanner::new(), "/=", ScanGoal::InputElementRegExpOrTemplateTail),
            None
        );
        assert_eq!(
            div_punctuator(&Scanner::new(), "/=", ScanGoal::InputElementTemplateTail),
            Some((
                Token::SlashEq,
                Scanner {
                    line: 1,
                    column: 3,
                    start_idx: 2
                }
            ))
        );
        assert_eq!(div_punctuator(&Scanner::new(), "Q", ScanGoal::InputElementDiv), None);
        assert_eq!(div_punctuator(&Scanner::new(), "Q", ScanGoal::InputElementRegExp), None);
        assert_eq!(
            div_punctuator(&Scanner::new(), "Q", ScanGoal::InputElementRegExpOrTemplateTail),
            None
        );
        assert_eq!(
            div_punctuator(&Scanner::new(), "Q", ScanGoal::InputElementTemplateTail),
            None
        );
    }

    #[test]
    fn right_brace_punctuator_test() {
        assert_eq!(
            right_brace_punctuator(&Scanner::new(), "}", ScanGoal::InputElementDiv),
            Some((
                Token::RightBrace,
                Scanner {
                    line: 1,
                    column: 2,
                    start_idx: 1
                }
            ))
        );
        assert_eq!(
            right_brace_punctuator(&Scanner::new(), "}", ScanGoal::InputElementRegExp),
            Some((
                Token::RightBrace,
                Scanner {
                    line: 1,
                    column: 2,
                    start_idx: 1
                }
            ))
        );
        assert_eq!(
            right_brace_punctuator(&Scanner::new(), "}", ScanGoal::InputElementTemplateTail),
            None
        );
        assert_eq!(
            right_brace_punctuator(&Scanner::new(), "}", ScanGoal::InputElementRegExpOrTemplateTail),
            None
        );
        assert_eq!(
            right_brace_punctuator(&Scanner::new(), "Q", ScanGoal::InputElementDiv),
            None
        );
        assert_eq!(
            right_brace_punctuator(&Scanner::new(), "Q", ScanGoal::InputElementRegExp),
            None
        );
        assert_eq!(
            right_brace_punctuator(&Scanner::new(), "Q", ScanGoal::InputElementRegExpOrTemplateTail),
            None
        );
        assert_eq!(
            right_brace_punctuator(&Scanner::new(), "Q", ScanGoal::InputElementTemplateTail),
            None
        );
    }

    #[test]
    fn common_token_test() {
        assert_eq!(
            common_token(&Scanner::new(), "new"),
            Ok(Some((
                Token::Identifier(IdentifierData {
                    column: 1,
                    keyword_id: Some(Keyword::New),
                    line: 1,
                    string_value: JSString::from("new")
                }),
                Scanner {
                    line: 1,
                    column: 4,
                    start_idx: 3
                }
            )))
        );
        assert_eq!(
            common_token(&Scanner::new(), "10"),
            Ok(Some((
                Token::Number(10.0),
                Scanner {
                    line: 1,
                    column: 3,
                    start_idx: 2
                }
            )))
        );
        assert_eq!(
            common_token(&Scanner::new(), "**"),
            Ok(Some((
                Token::StarStar,
                Scanner {
                    line: 1,
                    column: 3,
                    start_idx: 2
                }
            )))
        );
        assert_eq!(
            common_token(&Scanner::new(), "'truth'"),
            Ok(Some((
                Token::String(JSString::from("truth")),
                Scanner {
                    line: 1,
                    column: 8,
                    start_idx: 7
                }
            )))
        );
    }
    #[test]
    #[should_panic]
    fn common_token_test_panic() {
        let r = common_token(&Scanner::new(), "``");
        drop(r);
    }

    #[test]
    fn regular_expression_literal_test_01() {
        assert_eq!(
            regular_expression_literal(&Scanner::new(), "", ScanGoal::InputElementRegExp),
            Ok(None)
        );
        assert_eq!(
            regular_expression_literal(&Scanner::new(), "", ScanGoal::InputElementRegExpOrTemplateTail),
            Ok(None)
        );
        assert_eq!(
            regular_expression_literal(&Scanner::new(), "", ScanGoal::InputElementDiv),
            Ok(None)
        );
        assert_eq!(
            regular_expression_literal(&Scanner::new(), "", ScanGoal::InputElementTemplateTail),
            Ok(None)
        );
        assert_eq!(
            regular_expression_literal(&Scanner::new(), "/abcd/", ScanGoal::InputElementDiv),
            Ok(None)
        );
        assert_eq!(
            regular_expression_literal(&Scanner::new(), "/abcd/", ScanGoal::InputElementTemplateTail),
            Ok(None)
        );
    }
    #[test]
    #[should_panic]
    fn regular_expression_literal_test_02() {
        drop(regular_expression_literal(
            &Scanner::new(),
            "/abcd/",
            ScanGoal::InputElementRegExp,
        ));
    }
    #[test]
    #[should_panic]
    fn regular_expression_literal_test_03() {
        drop(regular_expression_literal(
            &Scanner::new(),
            "/abcd/",
            ScanGoal::InputElementRegExpOrTemplateTail,
        ));
    }

    #[test]
    fn template_literal_test_01() {
        assert_eq!(
            template_substitution_tail(&Scanner::new(), "", ScanGoal::InputElementRegExp),
            Ok(None)
        );
        assert_eq!(
            template_substitution_tail(&Scanner::new(), "", ScanGoal::InputElementRegExpOrTemplateTail),
            Ok(None)
        );
        assert_eq!(
            template_substitution_tail(&Scanner::new(), "", ScanGoal::InputElementDiv),
            Ok(None)
        );
        assert_eq!(
            template_substitution_tail(&Scanner::new(), "", ScanGoal::InputElementTemplateTail),
            Ok(None)
        );
        assert_eq!(
            template_substitution_tail(&Scanner::new(), "} middle {", ScanGoal::InputElementDiv),
            Ok(None)
        );
        assert_eq!(
            template_substitution_tail(&Scanner::new(), "} middle {", ScanGoal::InputElementRegExp),
            Ok(None)
        );
    }
    #[test]
    #[should_panic]
    fn template_literal_test_02() {
        drop(template_substitution_tail(
            &Scanner::new(),
            "} middle {",
            ScanGoal::InputElementTemplateTail,
        ));
    }
    #[test]
    #[should_panic]
    fn template_literal_test_03() {
        drop(template_substitution_tail(
            &Scanner::new(),
            "} middle {",
            ScanGoal::InputElementRegExpOrTemplateTail,
        ));
    }

    #[test]
    fn scan_token_test_01() {
        assert_eq!(
            scan_token(&Scanner::new(), "", ScanGoal::InputElementRegExp),
            Ok((
                Token::Eof,
                Scanner {
                    line: 1,
                    column: 1,
                    start_idx: 0
                }
            ))
        );
        assert_eq!(
            scan_token(
                &Scanner::new(),
                "  /* nothing to see here */   ",
                ScanGoal::InputElementRegExp
            ),
            Ok((
                Token::Eof,
                Scanner {
                    line: 1,
                    column: 31,
                    start_idx: 30
                }
            ))
        );
        assert_eq!(
            scan_token(&Scanner::new(), "/=", ScanGoal::InputElementDiv),
            Ok((
                Token::SlashEq,
                Scanner {
                    line: 1,
                    column: 3,
                    start_idx: 2
                }
            ))
        );
        assert_eq!(
            scan_token(&Scanner::new(), "}", ScanGoal::InputElementRegExp),
            Ok((
                Token::RightBrace,
                Scanner {
                    line: 1,
                    column: 2,
                    start_idx: 1
                }
            ))
        );
    }
    #[test]
    #[should_panic]
    fn scan_token_panic_01() {
        let r = scan_token(&Scanner::new(), "/abcd/", ScanGoal::InputElementRegExp);
        drop(r);
    }
    #[test]
    #[should_panic]
    fn scan_token_panic_02() {
        let r = scan_token(
            &Scanner::new(),
            "} middle template {",
            ScanGoal::InputElementTemplateTail,
        );
        drop(r);
    }
}
