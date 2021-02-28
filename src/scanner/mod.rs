pub mod ranges;
use crate::strings::JSString;
use crate::values::number_to_string;
use num::bigint::BigInt;
use std::cmp::Ordering;
use std::convert::TryFrom;
use std::fmt;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum ScanGoal {
    InputElementRegExpOrTemplateTail,
    InputElementRegExp,
    InputElementTemplateTail,
    InputElementDiv,
}

#[derive(Debug, PartialEq, Copy, Clone)]
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

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Keyword::Await => f.write_str("await"),
            Keyword::Break => f.write_str("break"),
            Keyword::Case => f.write_str("case"),
            Keyword::Catch => f.write_str("catch"),
            Keyword::Class => f.write_str("class"),
            Keyword::Const => f.write_str("const"),
            Keyword::Continue => f.write_str("continue"),
            Keyword::Debugger => f.write_str("debugger"),
            Keyword::Default => f.write_str("default"),
            Keyword::Delete => f.write_str("delete"),
            Keyword::Do => f.write_str("do"),
            Keyword::Else => f.write_str("else"),
            Keyword::Enum => f.write_str("enum"),
            Keyword::Export => f.write_str("export"),
            Keyword::Extends => f.write_str("extends"),
            Keyword::False => f.write_str("false"),
            Keyword::Finally => f.write_str("finally"),
            Keyword::For => f.write_str("for"),
            Keyword::Function => f.write_str("function"),
            Keyword::If => f.write_str("if"),
            Keyword::Import => f.write_str("import"),
            Keyword::In => f.write_str("in"),
            Keyword::Instanceof => f.write_str("instanceof"),
            Keyword::New => f.write_str("new"),
            Keyword::Null => f.write_str("null"),
            Keyword::Return => f.write_str("return"),
            Keyword::Super => f.write_str("super"),
            Keyword::Switch => f.write_str("switch"),
            Keyword::This => f.write_str("this"),
            Keyword::Throw => f.write_str("throw"),
            Keyword::True => f.write_str("true"),
            Keyword::Try => f.write_str("try"),
            Keyword::Typeof => f.write_str("typeof"),
            Keyword::Var => f.write_str("var"),
            Keyword::Void => f.write_str("void"),
            Keyword::While => f.write_str("while"),
            Keyword::With => f.write_str("with"),
            Keyword::Yield => f.write_str("yield"),
            Keyword::Let => f.write_str("let"),
            Keyword::Static => f.write_str("static"),
            Keyword::Implements => f.write_str("implements"),
            Keyword::Interface => f.write_str("interface"),
            Keyword::Package => f.write_str("package"),
            Keyword::Private => f.write_str("private"),
            Keyword::Protected => f.write_str("protected"),
            Keyword::Public => f.write_str("public"),
            Keyword::As => f.write_str("as"),
            Keyword::Async => f.write_str("async"),
            Keyword::From => f.write_str("from"),
            Keyword::Get => f.write_str("get"),
            Keyword::Of => f.write_str("of"),
            Keyword::Set => f.write_str("set"),
            Keyword::Target => f.write_str("target"),
            Keyword::Meta => f.write_str("meta"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct IdentifierData {
    pub string_value: JSString,
    pub keyword_id: Option<Keyword>,
    pub line: u32,
    pub column: u32,
}

impl fmt::Display for IdentifierData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.string_value.fmt(f)
    }
}

impl IdentifierData {
    pub fn matches(&self, kwd: Keyword) -> bool {
        self.keyword_id == Some(kwd)
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Punctuator {
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
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Eof,
    Punctuator(Punctuator),
    Identifier(IdentifierData),
    Number(f64),
    BigInt(BigInt),
    String(JSString),
    NoSubstitutionTemplate(TemplateData),
    TemplateHead(TemplateData),
    TemplateMiddle(TemplateData),
    TemplateTail(TemplateData),
    RegularExpression(RegularExpressionData),
    Error(String),
}

impl fmt::Display for Punctuator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Punctuator::LeftParen => f.write_str("("),
            Punctuator::RightParen => f.write_str(")"),
            Punctuator::LeftBrace => f.write_str("{"),
            Punctuator::RightBrace => f.write_str("}"),
            Punctuator::Dot => f.write_str("."),
            Punctuator::Ellipsis => f.write_str("..."),
            Punctuator::Comma => f.write_str(","),
            Punctuator::Semicolon => f.write_str(";"),
            Punctuator::LeftBracket => f.write_str("["),
            Punctuator::RightBracket => f.write_str("]"),
            Punctuator::Colon => f.write_str(":"),
            Punctuator::Tilde => f.write_str("~"),
            Punctuator::Minus => f.write_str("-"),
            Punctuator::MinusMinus => f.write_str("--"),
            Punctuator::MinusEq => f.write_str("-="),
            Punctuator::Plus => f.write_str("+"),
            Punctuator::PlusPlus => f.write_str("++"),
            Punctuator::PlusEq => f.write_str("+="),
            Punctuator::Slash => f.write_str("/"),
            Punctuator::SlashEq => f.write_str("/="),
            Punctuator::Star => f.write_str("*"),
            Punctuator::StarEq => f.write_str("*="),
            Punctuator::StarStar => f.write_str("**"),
            Punctuator::StarStarEq => f.write_str("**="),
            Punctuator::Amp => f.write_str("&"),
            Punctuator::AmpAmp => f.write_str("&&"),
            Punctuator::AmpAmpEq => f.write_str("&&="),
            Punctuator::AmpEq => f.write_str("&="),
            Punctuator::Lt => f.write_str("<"),
            Punctuator::LtEq => f.write_str("<="),
            Punctuator::LtLt => f.write_str("<<"),
            Punctuator::LtLtEq => f.write_str("<<="),
            Punctuator::Gt => f.write_str(">"),
            Punctuator::GtEq => f.write_str(">="),
            Punctuator::GtGt => f.write_str(">>"),
            Punctuator::GtGtGt => f.write_str(">>>"),
            Punctuator::GtGtEq => f.write_str(">>="),
            Punctuator::GtGtGtEq => f.write_str(">>>="),
            Punctuator::Eq => f.write_str("="),
            Punctuator::EqGt => f.write_str("=>"),
            Punctuator::EqEq => f.write_str("=="),
            Punctuator::EqEqEq => f.write_str("==="),
            Punctuator::Bang => f.write_str("!"),
            Punctuator::BangEq => f.write_str("!="),
            Punctuator::BangEqEq => f.write_str("!=="),
            Punctuator::Percent => f.write_str("%"),
            Punctuator::PercentEq => f.write_str("%="),
            Punctuator::Pipe => f.write_str("|"),
            Punctuator::PipePipe => f.write_str("||"),
            Punctuator::PipePipeEq => f.write_str("||="),
            Punctuator::PipeEq => f.write_str("|="),
            Punctuator::Caret => f.write_str("^"),
            Punctuator::CaretEq => f.write_str("^="),
            Punctuator::Question => f.write_str("?"),
            Punctuator::QDot => f.write_str("?."),
            Punctuator::QQ => f.write_str("??"),
            Punctuator::QQEq => f.write_str("??="),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // render the characters which would create this token
        match self {
            Token::Eof => Ok(()),
            Token::Punctuator(p) => p.fmt(f),
            Token::Identifier(id) => id.fmt(f),
            Token::Number(val) => {
                let mut s = Vec::new();
                number_to_string(&mut s, *val).unwrap();
                String::from_utf8(s).unwrap().fmt(f)
            }
            Token::BigInt(val) => val.fmt(f),
            Token::String(val) => val.fmt(f),
            Token::NoSubstitutionTemplate(val) => val.fmt(f),
            Token::TemplateHead(val) => val.fmt(f),
            Token::TemplateMiddle(val) => val.fmt(f),
            Token::TemplateTail(val) => val.fmt(f),
            Token::RegularExpression(val) => val.fmt(f),
            Token::Error(_) => f.write_str("\u{26a0}"),
        }
    }
}

impl Token {
    pub fn matches_keyword(&self, kwd: Keyword) -> bool {
        matches!(self, Token::Identifier(id) if id.keyword_id == Some(kwd))
    }
    pub fn matches_punct(&self, punct: Punctuator) -> bool {
        matches!(self, Token::Punctuator(p) if *p == punct)
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Scanner {
    pub line: u32,
    pub column: u32,
    pub start_idx: usize,
}
impl Scanner {
    pub fn new() -> Scanner {
        Scanner { line: 1, column: 1, start_idx: 0 }
    }
}
impl PartialOrd for Scanner {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.line < other.line {
            Some(Ordering::Less)
        } else if self.line > other.line {
            Some(Ordering::Greater)
        } else {
            if self.column < other.column {
                Some(Ordering::Less)
            } else if self.column > other.column {
                Some(Ordering::Greater)
            } else {
                Some(Ordering::Equal)
            }
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
        '\'' | '"' | '\\' | 'b' | 'f' | 'n' | 'r' | 't' | 'v' | 'u' | 'x' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => true,
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
        None => return Ok(Scanner { line, column, start_idx: idx }),
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
                None => return Ok(Scanner { line, column, start_idx: idx }),
                Some(c) => {
                    ch = c;
                    pending_idx = idx + ch.len_utf8();
                }
            }
            if previous == '\r' && ch == '\n' {
                idx = pending_idx;
                match iter.next() {
                    None => return Ok(Scanner { line, column, start_idx: idx }),
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
                None => return Ok(Scanner { line, column, start_idx: idx }),
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
                None => return Ok(Scanner { line, column, start_idx: idx }),
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
                            None => return Ok(Scanner { line, column, start_idx: idx }),
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
                        None => return Err(format!("Unterminated /*-style comment. Started on line {}, column {}.", comment_start_line, comment_start_column)),
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
                                None => return Err(format!("Unterminated /*-style comment. Started on line {}, column {}.", comment_start_line, comment_start_column)),
                                Some(c) => {
                                    ch = c;
                                    pending_idx = idx + ch.len_utf8();
                                }
                            }
                            if ch == '/' {
                                column = column + 1;
                                idx = pending_idx;
                                match iter.next() {
                                    None => return Ok(Scanner { line, column, start_idx: idx }),
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
                                None => return Err(format!("Unterminated /*-style comment. Started on line {}, column {}.", comment_start_line, comment_start_column)),
                                Some(c) => {
                                    ch = c;
                                    pending_idx = idx + ch.len_utf8();
                                }
                            }
                            if previous == '\r' && ch == '\n' {
                                idx = pending_idx;
                                match iter.next() {
                                    None => return Err(format!("Unterminated /*-style comment. Started on line {}, column {}.", comment_start_line, comment_start_column)),
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
                            None => return Err(format!("Unterminated /*-style comment. Started on line {}, column {}.", comment_start_line, comment_start_column)),
                            Some(c) => {
                                ch = c;
                                pending_idx = idx + ch.len_utf8();
                            }
                        }
                    }
                }
                _ => return Ok(Scanner { line, column, start_idx: idx }),
            }
            continue;
        }

        return Ok(Scanner { line, column, start_idx: idx });
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
        Some(Scanner { line: scanner.line, column: scanner.column + 4, start_idx: scanner.start_idx + first.len_utf8() + second.len_utf8() + third.len_utf8() + fourth.len_utf8() })
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
                return Some(Scanner { line: scanner.line, column: scanner.column + count as u32, start_idx: scanner.start_idx + count });
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
        let hex_scanner = Scanner { line: scanner.line, column: scanner.column + 1, start_idx: second_char_idx };
        let hex_option = hex_four_digits(&hex_scanner, source);
        match hex_option {
            Some(result) => Some(result),
            None => {
                let second_ch = iter.next()?;
                let third_char_idx = second_char_idx + second_ch.len_utf8();
                if second_ch == '{' {
                    let cp_scanner = Scanner { line: scanner.line, column: scanner.column + 2, start_idx: third_char_idx };
                    let after_cp = code_point(&cp_scanner, source)?;
                    let last_char = source[after_cp.start_idx..].chars().next()?;
                    match last_char {
                        '}' => Some(Scanner { line: scanner.line, column: after_cp.column + 1, start_idx: after_cp.start_idx + last_char.len_utf8() }),
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
        Ok(Some(Scanner { line: scanner.line, column: scanner.column + 1, start_idx: idx }))
    } else if ch == '\\' {
        let ues_scanner = Scanner { line: scanner.line, column: scanner.column + 1, start_idx: idx };
        let after_scanner;
        match unicode_escape_sequence(&ues_scanner, source) {
            None => return Ok(None),
            Some(scanner) => after_scanner = scanner,
        };
        let ch_value = ues_char_value(&source[ues_scanner.start_idx..after_scanner.start_idx]);
        if !validate(ch_value) {
            return Err(format!("{}:{}: Invalid Identifier {} Character {:?}", scanner.line, scanner.column, style, ch_value));
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
    utf16_encode_code_point(CharVal::from(ch))
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
                cp = char::from_u32(mv_of_hex_digit(HexChar::try_from(digit_or_brace).unwrap()) << 12 | mv_of_hex_digit(second) << 8 | mv_of_hex_digit(third) << 4 | mv_of_hex_digit(fourth))
                    .unwrap();
            }
            result.append(&mut code_point_to_utf16_code_units(cp))
        }
    }
    JSString::from(result)
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

pub fn identifier_name(scanner: &Scanner, source: &str) -> Option<(Token, Scanner)> {
    // IdentifierName ::
    //    IdentifierStart
    //    IdentifierName IdentifierPart
    // (I.e.: An IdentifierStart followed by any number of IdentifierParts)

    let is_result = identifier_start(scanner, source);
    let mut scanner_1;
    match is_result {
        Err(msg) => {
            return Some((Token::Error(msg), *scanner));
        }
        Ok(None) => {
            return None;
        }
        Ok(Some(scanner)) => scanner_1 = scanner,
    };

    loop {
        let ip_result = identifier_part(&scanner_1, source);
        match ip_result {
            Err(msg) => {
                return Some((Token::Error(msg), scanner_1));
            }
            Ok(None) => {
                break;
            }
            Ok(Some(after)) => {
                scanner_1 = after;
            }
        }
    }

    Some((
        Token::Identifier(IdentifierData {
            string_value: identifier_name_string_value(&source[scanner.start_idx..scanner_1.start_idx]),
            keyword_id: identifier_name_keyword(&source[scanner.start_idx..scanner_1.start_idx]),
            line: scanner.line,
            column: scanner.column,
        }),
        scanner_1,
    ))
}

fn optional_chaining_punctuator(scanner: &Scanner, source: &str) -> Option<(Token, Scanner)> {
    let mut iter = source[scanner.start_idx..].chars();
    match iter.next() {
        Some('?') => match iter.next() {
            Some('.') => match iter.next() {
                Some('0'..='9') => None,
                _ => Some((Token::Punctuator(Punctuator::QDot), Scanner { line: scanner.line, column: scanner.column + 2, start_idx: scanner.start_idx + 2 })),
            },
            _ => None,
        },
        _ => None,
    }
}

fn other_punctuator(scanner: &Scanner, source: &str) -> Option<(Token, Scanner)> {
    let mut iter = source[scanner.start_idx..].chars();
    let mt = |tk, delta| Some((tk, Scanner { line: scanner.line, column: scanner.column + delta, start_idx: scanner.start_idx + delta as usize }));
    match iter.next() {
        Some('{') => mt(Token::Punctuator(Punctuator::LeftBrace), 1),
        Some('(') => mt(Token::Punctuator(Punctuator::LeftParen), 1),
        Some(')') => mt(Token::Punctuator(Punctuator::RightParen), 1),
        Some('[') => mt(Token::Punctuator(Punctuator::LeftBracket), 1),
        Some(']') => mt(Token::Punctuator(Punctuator::RightBracket), 1),
        Some('.') => match iter.next() {
            Some('.') => match iter.next() {
                Some('.') => mt(Token::Punctuator(Punctuator::Ellipsis), 3),
                _ => mt(Token::Punctuator(Punctuator::Dot), 1),
            },
            _ => mt(Token::Punctuator(Punctuator::Dot), 1),
        },
        Some(';') => mt(Token::Punctuator(Punctuator::Semicolon), 1),
        Some(',') => mt(Token::Punctuator(Punctuator::Comma), 1),
        Some('<') => match iter.next() {
            Some('=') => mt(Token::Punctuator(Punctuator::LtEq), 2),
            Some('<') => match iter.next() {
                Some('=') => mt(Token::Punctuator(Punctuator::LtLtEq), 3),
                _ => mt(Token::Punctuator(Punctuator::LtLt), 2),
            },
            _ => mt(Token::Punctuator(Punctuator::Lt), 1),
        },
        Some('>') => match iter.next() {
            Some('=') => mt(Token::Punctuator(Punctuator::GtEq), 2),
            Some('>') => match iter.next() {
                Some('>') => match iter.next() {
                    Some('=') => mt(Token::Punctuator(Punctuator::GtGtGtEq), 4),
                    _ => mt(Token::Punctuator(Punctuator::GtGtGt), 3),
                },
                Some('=') => mt(Token::Punctuator(Punctuator::GtGtEq), 3),
                _ => mt(Token::Punctuator(Punctuator::GtGt), 2),
            },
            _ => mt(Token::Punctuator(Punctuator::Gt), 1),
        },
        Some('=') => match iter.next() {
            Some('=') => match iter.next() {
                Some('=') => mt(Token::Punctuator(Punctuator::EqEqEq), 3),
                _ => mt(Token::Punctuator(Punctuator::EqEq), 2),
            },
            Some('>') => mt(Token::Punctuator(Punctuator::EqGt), 2),
            _ => mt(Token::Punctuator(Punctuator::Eq), 1),
        },
        Some('!') => match iter.next() {
            Some('=') => match iter.next() {
                Some('=') => mt(Token::Punctuator(Punctuator::BangEqEq), 3),
                _ => mt(Token::Punctuator(Punctuator::BangEq), 2),
            },
            _ => mt(Token::Punctuator(Punctuator::Bang), 1),
        },
        Some('+') => match iter.next() {
            Some('+') => mt(Token::Punctuator(Punctuator::PlusPlus), 2),
            Some('=') => mt(Token::Punctuator(Punctuator::PlusEq), 2),
            _ => mt(Token::Punctuator(Punctuator::Plus), 1),
        },
        Some('-') => match iter.next() {
            Some('-') => mt(Token::Punctuator(Punctuator::MinusMinus), 2),
            Some('=') => mt(Token::Punctuator(Punctuator::MinusEq), 2),
            _ => mt(Token::Punctuator(Punctuator::Minus), 1),
        },
        Some('*') => match iter.next() {
            Some('*') => match iter.next() {
                Some('=') => mt(Token::Punctuator(Punctuator::StarStarEq), 3),
                _ => mt(Token::Punctuator(Punctuator::StarStar), 2),
            },
            Some('=') => mt(Token::Punctuator(Punctuator::StarEq), 2),
            _ => mt(Token::Punctuator(Punctuator::Star), 1),
        },
        Some('&') => match iter.next() {
            Some('&') => match iter.next() {
                Some('=') => mt(Token::Punctuator(Punctuator::AmpAmpEq), 3),
                _ => mt(Token::Punctuator(Punctuator::AmpAmp), 2),
            },
            Some('=') => mt(Token::Punctuator(Punctuator::AmpEq), 2),
            _ => mt(Token::Punctuator(Punctuator::Amp), 1),
        },
        Some('|') => match iter.next() {
            Some('|') => match iter.next() {
                Some('=') => mt(Token::Punctuator(Punctuator::PipePipeEq), 3),
                _ => mt(Token::Punctuator(Punctuator::PipePipe), 2),
            },
            Some('=') => mt(Token::Punctuator(Punctuator::PipeEq), 2),
            _ => mt(Token::Punctuator(Punctuator::Pipe), 1),
        },
        Some('^') => match iter.next() {
            Some('=') => mt(Token::Punctuator(Punctuator::CaretEq), 2),
            _ => mt(Token::Punctuator(Punctuator::Caret), 1),
        },
        Some('~') => mt(Token::Punctuator(Punctuator::Tilde), 1),
        Some('?') => match iter.next() {
            Some('?') => match iter.next() {
                Some('=') => mt(Token::Punctuator(Punctuator::QQEq), 3),
                _ => mt(Token::Punctuator(Punctuator::QQ), 2),
            },
            _ => mt(Token::Punctuator(Punctuator::Question), 1),
        },
        Some(':') => mt(Token::Punctuator(Punctuator::Colon), 1),
        Some('%') => match iter.next() {
            Some('=') => mt(Token::Punctuator(Punctuator::PercentEq), 2),
            _ => mt(Token::Punctuator(Punctuator::Percent), 1),
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
        Some(c) if c == ch => Some(Scanner { line: scanner.line, column: scanner.column + 1, start_idx: scanner.start_idx + c.len_utf8() }),
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
        Some(ch) if ch >= '1' && ch <= '9' => Some(Scanner { line: scanner.line, column: scanner.column + 1, start_idx: scanner.start_idx + 1 }),
        _ => None,
    }
}

fn decimal_big_integer_literal(scanner: &Scanner, source: &str) -> Option<Scanner> {
    match_char(scanner, source, '0')
        .and_then(|r| match_char(&r, source, 'n'))
        .or_else(|| non_zero_digit(scanner, source).and_then(|r| decimal_digits(&r, source, true).or_else(|| Some(r))).and_then(|r| match_char(&r, source, 'n')))
        .or_else(|| non_zero_digit(scanner, source).and_then(|r| match_char(&r, source, '_')).and_then(|r| decimal_digits(&r, source, true)).and_then(|r| match_char(&r, source, 'n')))
}

fn is_binary_digit(ch: char) -> bool {
    ch == '0' || ch == '1'
}

fn binary_digits(scanner: &Scanner, source: &str, sep: bool) -> Option<Scanner> {
    radix_digits(scanner, source, sep, is_binary_digit)
}

fn binary_integer_literal(scanner: &Scanner, source: &str, sep: bool) -> Option<Scanner> {
    match_char(scanner, source, '0').and_then(|r| match_char(&r, source, 'b').or_else(|| match_char(&r, source, 'B'))).and_then(|r| binary_digits(&r, source, sep))
}

fn is_octal_digit(ch: char) -> bool {
    ch >= '0' && ch <= '7'
}

fn octal_digits(scanner: &Scanner, source: &str, sep: bool) -> Option<Scanner> {
    radix_digits(scanner, source, sep, is_octal_digit)
}

fn octal_integer_literal(scanner: &Scanner, source: &str, sep: bool) -> Option<Scanner> {
    match_char(scanner, source, '0').and_then(|r| match_char(&r, source, 'o').or_else(|| match_char(&r, source, 'O'))).and_then(|r| octal_digits(&r, source, sep))
}

fn hex_digits(scanner: &Scanner, source: &str, sep: bool) -> Option<Scanner> {
    radix_digits(scanner, source, sep, is_hex_digit)
}

fn hex_integer_literal(scanner: &Scanner, source: &str, sep: bool) -> Option<Scanner> {
    match_char(scanner, source, '0').and_then(|r| match_char(&r, source, 'x').or_else(|| match_char(&r, source, 'X'))).and_then(|r| hex_digits(&r, source, sep))
}

fn non_decimal_integer_literal(scanner: &Scanner, source: &str, sep: bool) -> Option<(NumberStyle, Scanner)> {
    binary_integer_literal(scanner, source, sep).map_or_else(
        || octal_integer_literal(scanner, source, sep).map_or_else(|| hex_integer_literal(scanner, source, sep).map(|r| (NumberStyle::Hex, r)), |r| Some((NumberStyle::Octal, r))),
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
    let (number_style, after) = non_decimal_integer_literal(scanner, source, true).and_then(|(style, scan)| match_char(&scan, source, 'n').map(|r| (bigify(style), r))).or_else(|| {
        decimal_big_integer_literal(scanner, source).map_or_else(
            || non_decimal_integer_literal(scanner, source, true).or_else(|| decimal_literal(scanner, source).map(|r| (NumberStyle::Decimal, r))),
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
        NumberStyle::BigDecimal => Some((Token::BigInt(BigInt::parse_bytes(strip_sep(&source[scanner.start_idx..after.start_idx - 1]).as_bytes(), 10).unwrap()), after)),
        NumberStyle::BigBinary => Some((Token::BigInt(BigInt::parse_bytes(strip_sep(&source[scanner.start_idx + 2..after.start_idx - 1]).as_bytes(), 2).unwrap()), after)),
        NumberStyle::BigOctal => Some((Token::BigInt(BigInt::parse_bytes(strip_sep(&source[scanner.start_idx + 2..after.start_idx - 1]).as_bytes(), 8).unwrap()), after)),
        NumberStyle::BigHex => Some((Token::BigInt(BigInt::parse_bytes(strip_sep(&source[scanner.start_idx + 2..after.start_idx - 1]).as_bytes(), 16).unwrap()), after)),
        NumberStyle::Binary => Some((Token::Number(int_to_number(&strip_sep(&source[scanner.start_idx + 2..after.start_idx]), 2)), after)),
        NumberStyle::Octal => Some((Token::Number(int_to_number(&strip_sep(&source[scanner.start_idx + 2..after.start_idx]), 8)), after)),
        NumberStyle::Hex => Some((Token::Number(int_to_number(&strip_sep(&source[scanner.start_idx + 2..after.start_idx]), 16)), after)),
        NumberStyle::Decimal => Some((Token::Number(strip_sep(&source[scanner.start_idx..after.start_idx]).parse::<f64>().unwrap()), after)),
    }
}

fn escape_sequence(scanner: &Scanner, source: &str) -> Option<Scanner> {
    let mut iter = source[scanner.start_idx..].chars();
    match iter.next() {
        // CharacterEscapeSequence
        Some(ch) if is_single_escape_char(ch) || !(is_escape_char(ch) || is_lineterm(ch)) => {
            Some(Scanner { line: scanner.line, column: scanner.column + 1, start_idx: scanner.start_idx + ch.len_utf8() })
        }
        // 0 [lookahead ∉ DecimalDigit]
        Some('0') => {
            let lookahead = iter.next();
            match lookahead {
                Some(ch) if ch >= '0' && ch <= '9' => None,
                _ => Some(Scanner { line: scanner.line, column: scanner.column + 1, start_idx: scanner.start_idx + 1 }),
            }
        }
        // HexEscapeSequence
        Some('x') => {
            let ch1 = iter.next();
            match ch1 {
                Some(digit_1) if is_hex_digit(digit_1) => {
                    let ch2 = iter.next();
                    match ch2 {
                        Some(digit_2) if is_hex_digit(digit_2) => Some(Scanner { line: scanner.line, column: scanner.column + 3, start_idx: scanner.start_idx + 3 }),
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
            Some('\n') => Some(Scanner { line: scanner.line + 1, column: 1, start_idx: scanner.start_idx + 2 }),
            _ => Some(Scanner { line: scanner.line + 1, column: 1, start_idx: scanner.start_idx + 1 }),
        },
        Some(ch) if ch == '\n' || ch == '\u{2028}' || ch == '\u{2029}' => Some(Scanner { line: scanner.line + 1, column: 1, start_idx: scanner.start_idx + ch.len_utf8() }),
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
                let after_escape = escape_sequence(&after, source).or_else(|| line_terminator_sequence(&after, source))?;
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

    JSString::from(result)
}

fn string_literal(scanner: &Scanner, source: &str) -> Option<(Token, Scanner)> {
    let after = match_char(scanner, source, '"')
        .and_then(|r| string_characters(&r, source, '"').or_else(|| Some(r)))
        .and_then(|r| match_char(&r, source, '"'))
        .or_else(|| match_char(scanner, source, '\'').and_then(|r| string_characters(&r, source, '\'').or_else(|| Some(r))).and_then(|r| match_char(&r, source, '\'')))?;
    let start_idx = scanner.start_idx + 1;
    let after_idx = after.start_idx - 1;
    assert!(after_idx >= start_idx);
    let value = literal_string_value(&source[start_idx..after_idx]);

    Some((Token::String(value), after))
}

#[derive(Debug, PartialEq)]
pub struct TemplateData {
    pub tv: Option<JSString>,
    pub trv: JSString,
    pub starting_index: usize,
    pub byte_length: usize,
}

impl fmt::Display for TemplateData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        format!("{}", self.trv).replace(char::is_control, "\u{2426}").fmt(f)
    }
}

fn push_utf16(buf: &mut Vec<u16>, ch: char) {
    let mut b = [0; 2];
    buf.extend_from_slice(ch.encode_utf16(&mut b));
}

#[derive(Debug, PartialEq)]
struct THDCount(usize);
impl TryFrom<usize> for THDCount {
    type Error = &'static str;

    fn try_from(value: usize) -> Result<Self, Self::Error> {
        if value <= 4 {
            Ok(THDCount(value))
        } else {
            Err("THDCount can only be used with values <= 4")
        }
    }
}

impl From<THDCount> for usize {
    fn from(tc: THDCount) -> Self {
        let THDCount(num) = tc;
        num
    }
}

fn template_hex_digits(iter: &mut std::iter::Peekable<std::str::Chars>, identifier: u16, count: THDCount, scanner: &Scanner) -> (Option<Vec<u16>>, Vec<u16>, Scanner, usize) {
    let mut accumulator = 0;
    let mut successful = true;
    let mut consumed = 1;
    let mut raw_chars = [identifier, 0, 0, 0, 0];
    for i in 0..usize::from(count) {
        let pot_digit = iter.next();
        if !pot_digit.map_or(false, |c| c.is_ascii_hexdigit()) {
            successful = false;
            break;
        }
        consumed += 1;
        accumulator = (accumulator << 4) + pot_digit.unwrap().to_digit(16).unwrap();
        raw_chars[i + 1] = pot_digit.unwrap() as u16;
    }
    (
        if successful { Some(vec![accumulator as u16]) } else { None },
        raw_chars[..consumed].to_vec(),
        Scanner { line: scanner.line, column: scanner.column + consumed as u32, start_idx: scanner.start_idx + consumed },
        consumed,
    )
}

#[derive(Debug, PartialEq)]
pub struct CharVal(u32);
impl TryFrom<u32> for CharVal {
    type Error = &'static str;

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        if value <= 0x10ffff {
            Ok(CharVal(value))
        } else {
            Err("CharVal can only be used with values <= 0x10ffff")
        }
    }
}

//impl From<u16> for CharVal {
//    fn from(val: u16) -> Self {
//        CharVal(val as u32)
//    }
//}

impl From<char> for CharVal {
    fn from(val: char) -> Self {
        CharVal(val as u32)
    }
}

//impl From<CharVal> for u32 {
//    fn from(cv: CharVal) -> Self {
//        let CharVal(num) = cv;
//        num
//    }
//}

fn utf16_encode_code_point(cv: CharVal) -> Vec<u16> {
    let CharVal(value) = cv;
    if value <= 0xffff {
        vec![value as u16]
    } else {
        vec![(((value - 0x10000) >> 10) + 0xD800) as u16, (((value - 0x10000) & 0x3ff) + 0xDC00) as u16]
    }
}

fn template_hex_digits_by_value(iter: &mut std::iter::Peekable<std::str::Chars>, scanner: &Scanner) -> (Option<Vec<u16>>, Vec<u16>, Scanner, usize) {
    let mut accumulator = 0;
    let mut consumed = 2;
    let mut raw_chars = vec!['u' as u16, '{' as u16];
    loop {
        let pot_digit = iter.next();
        if !pot_digit.map_or(false, |c| c.is_ascii_hexdigit()) {
            let tv;
            let cv = CharVal::try_from(accumulator);
            if consumed == 2 || pot_digit != Some('}') || cv.is_err() {
                tv = None;
            } else {
                // "unwrap" below is ok, because we've just validated the cv is_ok().
                tv = Some(utf16_encode_code_point(cv.unwrap()));
                raw_chars.push('}' as u16);
                consumed += 1;
            }
            return (tv, raw_chars, Scanner { line: scanner.line, column: scanner.column + consumed as u32, start_idx: scanner.start_idx + consumed }, consumed);
        } else {
            consumed += 1;
            let digit = pot_digit.unwrap(); // This is ok, because we've already validated as a hex digit char.
            let value = digit.to_digit(16).unwrap(); // This is also ok, because we've already validated as a hex digit char.

            // If the _current_ accumulator fits in a CharVal, add the new value to the accumulator. (This may produce
            // an invalid char; we're only checking to prevent overflow, which takes many additional digits.)
            match CharVal::try_from(accumulator) {
                Err(_) => {} // Value already not-a-character. Don't add any more.
                Ok(_) => {
                    // There's still room to build up a value. Add 4 more bits!
                    accumulator = (accumulator << 4) + value;
                }
            }
            raw_chars.push(digit as u16);
        }
    }
}

fn template_escape(scanner: Scanner, source: &str) -> (Option<Vec<u16>>, Vec<u16>, Scanner, usize) {
    let mut chars = source[scanner.start_idx..].chars().peekable();
    let single_char = |ch: char, val: u16, scanner: &Scanner| {
        (Some(vec![val]), utf16_encode_code_point(CharVal::from(ch)), Scanner { line: scanner.line, column: scanner.column + 1, start_idx: scanner.start_idx + ch.len_utf8() }, 1)
    };
    match chars.next() {
        None =>
        // This is "The file ended at a backslash while inside a template". It's gonna fail the Template token
        // match, because there's no terminating character sequence, so it doesn't much matter _what_ we return.
        {
            (None, vec![], scanner, 0)
        }
        Some('b') => single_char('b', 8, &scanner),
        Some('t') => single_char('t', 9, &scanner),
        Some('n') => single_char('n', 10, &scanner),
        Some('v') => single_char('v', 11, &scanner),
        Some('f') => single_char('f', 12, &scanner),
        Some('r') => single_char('r', 13, &scanner),
        Some('"') => single_char('"', 0x22, &scanner),
        Some('\'') => single_char('\'', 0x27, &scanner),
        Some('\\') => single_char('\\', 0x5c, &scanner),
        Some('0') if !chars.peek().map_or(false, |c| c.is_digit(10)) => single_char('0', 0, &scanner),
        Some(c) if c.is_digit(10) => {
            (None, utf16_encode_code_point(CharVal::from(c)), Scanner { line: scanner.line, column: scanner.column + 1, start_idx: scanner.start_idx + c.len_utf8() }, 1)
        }
        Some('x') => template_hex_digits(&mut chars, 'x' as u16, THDCount::try_from(2).unwrap(), &scanner),
        Some('u') => {
            let pot_brace_or_digit = chars.peek();
            if pot_brace_or_digit.map_or(false, |c| *c == '{') {
                // \u{digits} style
                chars.next();
                template_hex_digits_by_value(&mut chars, &scanner)
            } else {
                template_hex_digits(&mut chars, 'u' as u16, THDCount::try_from(4).unwrap(), &scanner)
            }
        }
        Some(c) if ['\n', '\u{2028}', '\u{2029}'].contains(&c) => {
            (Some(vec![]), utf16_encode_code_point(CharVal::from(c)), Scanner { line: scanner.line + 1, column: 1, start_idx: scanner.start_idx + c.len_utf8() }, 1)
        }
        Some('\r') => {
            let consumed;
            if chars.peek() == Some(&'\n') {
                consumed = 2;
            } else {
                consumed = 1;
            }
            (Some(vec![]), vec!['\n' as u16], Scanner { line: scanner.line + 1, column: 1, start_idx: scanner.start_idx + consumed }, consumed)
        }
        Some(c) => (
            Some(utf16_encode_code_point(CharVal::from(c))),
            utf16_encode_code_point(CharVal::from(c)),
            Scanner { line: scanner.line, column: scanner.column + 1, start_idx: scanner.start_idx + c.len_utf8() },
            1,
        ),
    }
}

fn template_characters(scanner: &Scanner, source: &str) -> (Option<JSString>, JSString, Scanner) {
    let mut tv: Option<Vec<u16>> = Some(Vec::new());
    let mut trv: Vec<u16> = Vec::new();
    let mut chars = source[scanner.start_idx..].chars().peekable();
    let mut current_scanner = *scanner;
    loop {
        let ch = chars.next();
        match ch {
            None | Some('`') => {
                return (tv.map(|t| JSString::from(t)), JSString::from(trv), current_scanner);
            }
            Some('$') if chars.peek() == Some(&'{') => {
                return (tv.map(|t| JSString::from(t)), JSString::from(trv), current_scanner);
            }
            Some('\\') => {
                current_scanner.column += 1;
                current_scanner.start_idx += 1;
                let (newtv, newtrv, after_escape, chars_consumed) = template_escape(current_scanner, source);
                for _ in 0..chars_consumed {
                    chars.next();
                }
                current_scanner = after_escape;
                push_utf16(&mut trv, '\\');
                trv.extend(newtrv.iter());
                if let Some(ttv) = &mut tv {
                    match &newtv {
                        Some(newttv) => {
                            ttv.extend(newttv.iter());
                        }
                        None => {
                            tv = None;
                        }
                    }
                }
            }
            Some(c) => {
                if let Some(ttv) = &mut tv {
                    push_utf16(ttv, c);
                }
                push_utf16(&mut trv, c);
                current_scanner.column += 1;
                current_scanner.start_idx += c.len_utf8();
            }
        }
    }
}

enum TemplateStyle {
    NoSubOrHead,
    MiddleOrTail,
}

fn template_token(scanner: &Scanner, source: &str, style: TemplateStyle) -> Option<(Token, Scanner)> {
    let first_match = match style {
        TemplateStyle::NoSubOrHead => '`',
        TemplateStyle::MiddleOrTail => '}',
    };
    let after_leading_quote = match_char(scanner, source, first_match)?;
    let (tv, trv, after_chars) = template_characters(&after_leading_quote, source);
    let pot_trailing_quote = match_char(&after_chars, source, '`');
    match pot_trailing_quote {
        Some(after_trailing_quote) => {
            let make_token = match style {
                TemplateStyle::NoSubOrHead => |td| Token::NoSubstitutionTemplate(td),
                TemplateStyle::MiddleOrTail => |td| Token::TemplateTail(td),
            };
            Some((make_token(TemplateData { tv, trv, starting_index: scanner.start_idx, byte_length: after_trailing_quote.start_idx - scanner.start_idx }), after_trailing_quote))
        }
        None => {
            let pot_template_head = match_char(&after_chars, source, '$').and_then(|r| match_char(&r, source, '{'));
            match pot_template_head {
                Some(after_template_head) => {
                    let make_token = match style {
                        TemplateStyle::NoSubOrHead => |td| Token::TemplateHead(td),
                        TemplateStyle::MiddleOrTail => |td| Token::TemplateMiddle(td),
                    };
                    Some((make_token(TemplateData { tv, trv, starting_index: scanner.start_idx, byte_length: after_template_head.start_idx - scanner.start_idx }), after_template_head))
                }
                None => None,
            }
        }
    }
}

fn template(scanner: &Scanner, source: &str) -> Option<(Token, Scanner)> {
    template_token(scanner, source, TemplateStyle::NoSubOrHead)
}

fn common_token(scanner: &Scanner, source: &str) -> Option<(Token, Scanner)> {
    let mut r;
    r = identifier_name(scanner, source);
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
    r
}

fn div_punctuator(scanner: &Scanner, source: &str, goal: ScanGoal) -> Option<(Token, Scanner)> {
    if goal == ScanGoal::InputElementDiv || goal == ScanGoal::InputElementTemplateTail {
        let mut iter = source[scanner.start_idx..].chars();
        match iter.next() {
            Some('/') => match iter.next() {
                Some('=') => Some((Token::Punctuator(Punctuator::SlashEq), Scanner { line: scanner.line, column: scanner.column + 2, start_idx: scanner.start_idx + 2 })),
                _ => Some((Token::Punctuator(Punctuator::Slash), Scanner { line: scanner.line, column: scanner.column + 1, start_idx: scanner.start_idx + 1 })),
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
            Some((Token::Punctuator(Punctuator::RightBrace), Scanner { line: scanner.line, column: scanner.column + 1, start_idx: scanner.start_idx + 1 }))
        } else {
            None
        }
    } else {
        None
    }
}
fn regular_expression_literal(scanner: &Scanner, source: &str, goal: ScanGoal) -> Option<(Token, Scanner)> {
    if goal == ScanGoal::InputElementRegExp || goal == ScanGoal::InputElementRegExpOrTemplateTail {
        let ch = source[scanner.start_idx..].chars().next();
        if ch == Some('/') {
            None //todo!();
        } else {
            None
        }
    } else {
        None
    }
}
fn template_substitution_tail(scanner: &Scanner, source: &str, goal: ScanGoal) -> Option<(Token, Scanner)> {
    if goal == ScanGoal::InputElementRegExpOrTemplateTail || goal == ScanGoal::InputElementTemplateTail {
        template_token(scanner, source, TemplateStyle::MiddleOrTail)
    } else {
        None
    }
}

#[derive(Debug, PartialEq)]
pub struct RegularExpressionData {
    body: String,
    flags: String,
}

impl fmt::Display for RegularExpressionData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "/{}/{}", self.body, self.flags)
    }
}

pub fn scan_token(scanner: &Scanner, source: &str, goal: ScanGoal) -> (Token, Scanner) {
    let skip_result = skip_skippables(scanner, source);
    match skip_result {
        Err(msg) => (Token::Error(msg), *scanner),
        Ok(after_skippable) => {
            if after_skippable.start_idx >= source.len() {
                (Token::Eof, after_skippable)
            } else {
                let mut r;
                r = common_token(&after_skippable, source);
                if r.is_none() {
                    r = div_punctuator(&after_skippable, source, goal);
                    if r.is_none() {
                        r = right_brace_punctuator(&after_skippable, source, goal);
                        if r.is_none() {
                            r = regular_expression_literal(&after_skippable, source, goal);
                            if r.is_none() {
                                r = template_substitution_tail(&after_skippable, source, goal);
                                if r.is_none() {
                                    r = Some((Token::Error(String::from("Unrecognized Token")), after_skippable));
                                }
                            }
                        }
                    }
                }
                r.unwrap()
            }
        }
    }
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
    fn scan_numeric() {
        let result = scan_token(&Scanner::new(), ".25", ScanGoal::InputElementRegExp);
        assert_eq!(result, (Token::Number(0.25), Scanner { line: 1, column: 4, start_idx: 3 }));
    }
    #[test]
    fn scan_token_id_01() {
        let result = scan_token(&Scanner::new(), "\\u004Abc", ScanGoal::InputElementRegExp);
        assert_eq!(result, (Token::Identifier(IdentifierData { string_value: JSString::from("Jbc"), keyword_id: None, line: 1, column: 1 }), Scanner { line: 1, column: 9, start_idx: 8 }));
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
        punct_check(">>", Token::Punctuator(Punctuator::GtGt));
        punct_check(">>>", Token::Punctuator(Punctuator::GtGtGt));
        punct_check(">>>=", Token::Punctuator(Punctuator::GtGtGtEq));
        punct_check(">>=", Token::Punctuator(Punctuator::GtGtEq));
        punct_check("===", Token::Punctuator(Punctuator::EqEqEq));
        punct_check("==", Token::Punctuator(Punctuator::EqEq));
        punct_check("=>", Token::Punctuator(Punctuator::EqGt));
        punct_check("!=", Token::Punctuator(Punctuator::BangEq));
        punct_check("+=", Token::Punctuator(Punctuator::PlusEq));
        punct_check("-=", Token::Punctuator(Punctuator::MinusEq));
        punct_check("**=", Token::Punctuator(Punctuator::StarStarEq));
        punct_check("**", Token::Punctuator(Punctuator::StarStar));
        punct_check("*=", Token::Punctuator(Punctuator::StarEq));
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
        let s = literal_string_value("a\\a\\b\\t\\n\\v\\f\\r\\'\\\"\\\\\\x66\\u{211c}\\u211d\\\n\\\u{2028}\\\u{2029}\\\r\\\n\\\r\n\\0");
        assert_eq!(s, "aa\u{8}\t\n\u{b}\u{c}\r'\"\\f\u{211c}\u{211d}\u{0}");
    }

    #[test]
    fn string_literal_test() {
        assert_eq!(string_literal(&Scanner::new(), "not_a_string"), None);
        assert_eq!(string_literal(&Scanner::new(), "''"), Some((Token::String(JSString::from("")), Scanner { line: 1, column: 3, start_idx: 2 })));
        assert_eq!(string_literal(&Scanner::new(), "\"\""), Some((Token::String(JSString::from("")), Scanner { line: 1, column: 3, start_idx: 2 })));
        assert_eq!(string_literal(&Scanner::new(), "'abcd'"), Some((Token::String(JSString::from("abcd")), Scanner { line: 1, column: 7, start_idx: 6 })));
        assert_eq!(string_literal(&Scanner::new(), "\"abcd\""), Some((Token::String(JSString::from("abcd")), Scanner { line: 1, column: 7, start_idx: 6 })));
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
        assert_eq!(
            right_brace_punctuator(&Scanner::new(), "}", ScanGoal::InputElementRegExp),
            Some((Token::Punctuator(Punctuator::RightBrace), Scanner { line: 1, column: 2, start_idx: 1 }))
        );
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
            Some((
                Token::Identifier(IdentifierData { column: 1, keyword_id: Some(Keyword::New), line: 1, string_value: JSString::from("new") }),
                Scanner { line: 1, column: 4, start_idx: 3 }
            ))
        );
        assert_eq!(common_token(&Scanner::new(), "10"), Some((Token::Number(10.0), Scanner { line: 1, column: 3, start_idx: 2 })));
        assert_eq!(common_token(&Scanner::new(), "**"), Some((Token::Punctuator(Punctuator::StarStar), Scanner { line: 1, column: 3, start_idx: 2 })));
        assert_eq!(common_token(&Scanner::new(), "'truth'"), Some((Token::String(JSString::from("truth")), Scanner { line: 1, column: 8, start_idx: 7 })));
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
        assert_eq!(result, None);
    }
    #[test]
    fn regular_expression_literal_test_03() {
        let result = regular_expression_literal(&Scanner::new(), "/abcd/", ScanGoal::InputElementRegExpOrTemplateTail);
        assert!(result.is_none());
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
        let (token, scanner) = scan_token(&Scanner::new(), "/abcd/", ScanGoal::InputElementRegExp);
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
            Token::NoSubstitutionTemplate(TemplateData {
                tv: None,
                trv: JSString::from("\\u{9999999999999999999999999999999999999999999999999999999999}"),
                starting_index: 0,
                byte_length: 64,
            })
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
}
