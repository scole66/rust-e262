pub mod ranges;
use super::*;
use num::bigint::BigInt;
use regex::Regex;
use std::char;
use std::cmp::Ordering;
use std::cmp::max;
use std::convert::TryFrom;
use std::fmt;
use std::str;
use std::sync::LazyLock;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ScanGoal {
    InputElementRegExpOrTemplateTail,
    InputElementRegExp,
    InputElementTemplateTail,
    InputElementDiv,
}

#[derive(Debug, PartialEq, Copy, Clone, Eq, Hash)]
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

#[derive(Debug, PartialEq, Eq)]
pub struct IdentifierData {
    pub string_value: JSString,
    pub keyword_id: Option<Keyword>,
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

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
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

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum StringDelimiter {
    Single,
    Double,
}
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct StringToken {
    pub value: JSString,
    pub delimiter: StringDelimiter,
    pub raw: Option<String>, // None if the string token had no escapes (in which case value == raw).
}

impl fmt::Display for StringToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // The "as source" form
        let delim = match self.delimiter {
            StringDelimiter::Single => '\'',
            StringDelimiter::Double => '"',
        };
        match &self.raw {
            None => write!(f, "{}{}{}", delim, self.value, delim),
            Some(s) => write!(f, "{delim}{s}{delim}"),
        }
    }
}

impl StringToken {
    pub fn has_legacy_octal_escapes(&self) -> bool {
        // Need to implement legacy octal syntax before this makes any sense.
        false
    }
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Eof,
    Punctuator(Punctuator),
    Identifier(IdentifierData),
    Number(f64),
    BigInt(BigInt),
    String(StringToken),
    NoSubstitutionTemplate(TemplateData),
    TemplateHead(TemplateData),
    TemplateMiddle(TemplateData),
    TemplateTail(TemplateData),
    RegularExpression(RegularExpressionData),
    PrivateIdentifier(IdentifierData),
    Error(String),
    Debug(DebugKind),
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
            Token::Identifier(id) | Token::PrivateIdentifier(id) => id.fmt(f),
            Token::Number(val) => {
                let mut s = Vec::new();
                number_to_string(&mut s, *val).unwrap();
                String::from_utf8(s).unwrap().fmt(f)
            }
            Token::BigInt(val) => val.fmt(f), // This needs a better "render as source" algorithm.
            Token::String(val) => val.fmt(f),
            Token::NoSubstitutionTemplate(val)  // This needs a better "render as source" algorithm.
            | Token::TemplateHead(val)
            | Token::TemplateMiddle(val)
            | Token::TemplateTail(val) => val.fmt(f),
            Token::RegularExpression(val) => val.fmt(f),
            Token::Error(_) => f.write_str("\u{26a0}"),
            Token::Debug(c) => write!(f, "@@{c}"),
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

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
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
impl Default for Scanner {
    fn default() -> Self {
        Self::new()
    }
}
impl PartialOrd for Scanner {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.line < other.line {
            Some(Ordering::Less)
        } else if self.line > other.line {
            Some(Ordering::Greater)
        } else if self.column < other.column {
            Some(Ordering::Less)
        } else if self.column > other.column {
            Some(Ordering::Greater)
        } else {
            Some(Ordering::Equal)
        }
    }
}

fn is_lineterm(ch: char) -> bool {
    ch == '\x0a' || ch == '\x0d' || ch == '\u{2028}' || ch == '\u{2029}'
}

fn is_whitespace(ch: char) -> bool {
    ('\x09'..='\x0d').contains(&ch)
        || ch == '\x20'
        || ch == '\u{00a0}'
        || ch == '\u{2028}'
        || ch == '\u{2029}'
        || ch == '\u{feff}'
        || ch == '\u{1680}'
        || ('\u{2000}'..='\u{200a}').contains(&ch)
        || ch == '\u{202f}'
        || ch == '\u{205f}'
        || ch == '\u{3000}'
}

fn is_single_escape_char(ch: char) -> bool {
    matches!(ch, '\'' | '"' | '\\' | 'b' | 'f' | 'n' | 'r' | 't' | 'v')
}

fn is_escape_char(ch: char) -> bool {
    matches!(
        ch,
        '\'' | '"'
            | '\\'
            | 'b'
            | 'f'
            | 'n'
            | 'r'
            | 't'
            | 'v'
            | 'u'
            | 'x'
            | '0'
            | '1'
            | '2'
            | '3'
            | '4'
            | '5'
            | '6'
            | '7'
            | '8'
            | '9'
    )
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
            line += 1;
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
            column += 1;
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
                    pending_idx += ch_next.len_utf8();
                }
            }
            match ch_next {
                '/' => {
                    // Single-line comment
                    column += 2;
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
                        column += 1;
                        idx = pending_idx;
                    }
                }
                '*' => {
                    // Multi-line comment
                    let comment_start_line = line;
                    let comment_start_column = column;
                    column += 2;
                    idx = pending_idx;

                    match iter.next() {
                        // If None comes back, this is actually a syntax error.
                        None => {
                            return Err(format!(
                                "Unterminated /*-style comment. Started on line {comment_start_line}, column {comment_start_column}."
                            ));
                        }
                        Some(c) => {
                            ch = c;
                            pending_idx = idx + ch.len_utf8();
                        }
                    }

                    'comment: loop {
                        while ch == '*' {
                            column += 1;
                            idx = pending_idx;
                            match iter.next() {
                                // If None comes back, this is actually a syntax error.
                                None => {
                                    return Err(format!(
                                        "Unterminated /*-style comment. Started on line {comment_start_line}, column {comment_start_column}."
                                    ));
                                }
                                Some(c) => {
                                    ch = c;
                                    pending_idx = idx + ch.len_utf8();
                                }
                            }
                            if ch == '/' {
                                column += 1;
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
                            line += 1;
                            column = 1;
                            idx = pending_idx;
                            let previous = ch;
                            match iter.next() {
                                None => {
                                    return Err(format!(
                                        "Unterminated /*-style comment. Started on line {comment_start_line}, column {comment_start_column}."
                                    ));
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
                                            "Unterminated /*-style comment. Started on line {comment_start_line}, column {comment_start_column}."
                                        ));
                                    }
                                    Some(c) => {
                                        ch = c;
                                        pending_idx = idx + ch.len_utf8();
                                    }
                                }
                            }
                            continue;
                        }

                        column += 1;
                        idx = pending_idx;
                        match iter.next() {
                            None => {
                                return Err(format!(
                                    "Unterminated /*-style comment. Started on line {comment_start_line}, column {comment_start_column}."
                                ));
                            }
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
    ch.is_ascii_digit()
}

fn is_hex_digit(ch: char) -> bool {
    ch.is_ascii_hexdigit()
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
        if let Some(ch) = opt_ch
            && is_hex_digit(ch)
        {
            count += 1;
            continue;
        }
        break;
    }
    if count > 0 {
        let parse_result = u32::from_str_radix(&source[scanner.start_idx..scanner.start_idx + count], 16);
        if let Ok(mv) = parse_result
            && mv <= 0x0010_FFFF
        {
            return Some(Scanner {
                line: scanner.line,
                column: u32::try_from(scanner.column as usize + count).expect("column should fit in 32 bits"),
                start_idx: scanner.start_idx + count,
            });
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
                    let cp_scanner =
                        Scanner { line: scanner.line, column: scanner.column + 2, start_idx: third_char_idx };
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

fn ues_char_value(source: &str) -> char {
    // We already know this is a valid Unicode Escape Sequence, so there's a lot of checking we don't do.
    let bytes = source.as_bytes();
    let value = if bytes[1] == b'{' {
        u32::from_str_radix(str::from_utf8(&bytes[2..bytes.len() - 1]).unwrap(), 16).unwrap()
    } else {
        u32::from_str_radix(str::from_utf8(&bytes[1..5]).unwrap(), 16).unwrap()
    };
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

pub fn is_unicode_id_start(ch: char) -> bool {
    unicode_range_checker(ch, ranges::ID_START)
}

pub fn is_unicode_id_continue(ch: char) -> bool {
    unicode_range_checker(ch, ranges::ID_CONTINUE)
}

fn identifier_piece<F>(scanner: &Scanner, source: &str, validate: F, style: &str) -> Result<Option<Scanner>, String>
where
    F: Fn(char) -> bool,
{
    let mut idx = scanner.start_idx;
    let mut iter = source[idx..].chars();
    let Some(ch) = iter.next() else {
        return Ok(None);
    };
    idx += ch.len_utf8();
    if validate(ch) {
        Ok(Some(Scanner { line: scanner.line, column: scanner.column + 1, start_idx: idx }))
    } else if ch == '\\' {
        let ues_scanner = Scanner { line: scanner.line, column: scanner.column + 1, start_idx: idx };
        let Some(after_scanner) = unicode_escape_sequence(&ues_scanner, source) else {
            return Ok(None);
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
    utf16_encode_code_point(CharVal::from(ch))
}

#[derive(Debug, PartialEq, Eq)]
pub struct HexChar(char);
impl TryFrom<char> for HexChar {
    type Error = &'static str;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        if is_hex_digit(value) { Ok(HexChar(value)) } else { Err("HexChar can only be used with hexidecimal digits!") }
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
    if ch.is_ascii_digit() {
        code - '0' as u32
    } else if ('A'..='F').contains(&ch) {
        code - 'A' as u32 + 10
    } else {
        code - 'a' as u32 + 10
    }
}

fn identifier_name_string_value(id_text: &str) -> JSString {
    let mut result: Vec<u16> = vec![];
    let mut iter = id_text.chars();
    loop {
        let Some(ch) = iter.next() else {
            break;
        };
        if ch == '\\' {
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
                    val = (val << 4) | mv_of_hex_digit(HexChar::try_from(ch).unwrap());
                }
                cp = char::from_u32(val).unwrap();
            } else {
                let second = HexChar::try_from(iter.next().unwrap()).unwrap();
                let third = HexChar::try_from(iter.next().unwrap()).unwrap();
                let fourth = HexChar::try_from(iter.next().unwrap()).unwrap();
                cp = char::from_u32(
                    (mv_of_hex_digit(HexChar::try_from(digit_or_brace).unwrap()) << 12)
                        | (mv_of_hex_digit(second) << 8)
                        | (mv_of_hex_digit(third) << 4)
                        | mv_of_hex_digit(fourth),
                )
                .unwrap();
            }
            result.append(&mut code_point_to_utf16_code_units(cp));
        } else {
            result.append(&mut code_point_to_utf16_code_units(ch));
        }
    }
    JSString::from(result)
}

fn keycomplete(source: &str, cmp: &str, kwd: Keyword) -> Option<Keyword> {
    if source == cmp { Some(kwd) } else { None }
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

fn identifier_internal(
    scanner: &Scanner,
    source: &str,
) -> Result<Option<(IdentifierData, Scanner)>, (String, Scanner)> {
    // IdentifierName ::
    //    IdentifierStart
    //    IdentifierName IdentifierPart
    // (I.e.: An IdentifierStart followed by any number of IdentifierParts)

    let is_result = identifier_start(scanner, source).map_err(|errmsg| (errmsg, *scanner))?;
    let Some(mut scanner_1) = is_result else {
        return Ok(None);
    };

    loop {
        let ip_result = identifier_part(&scanner_1, source).map_err(|errmsg| (errmsg, scanner_1))?;
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
        IdentifierData {
            string_value: identifier_name_string_value(&source[scanner.start_idx..scanner_1.start_idx]),
            keyword_id: identifier_name_keyword(&source[scanner.start_idx..scanner_1.start_idx]),
        },
        scanner_1,
    )))
}

pub fn identifier_name(scanner: &Scanner, source: &str) -> Option<(Token, Scanner)> {
    match identifier_internal(scanner, source) {
        Err((errmsg, scan)) => Some((Token::Error(errmsg), scan)),
        Ok(Some((data, scan))) => Some((Token::Identifier(data), scan)),
        Ok(None) => None,
    }
}

fn optional_chaining_punctuator(scanner: &Scanner, source: &str) -> Option<(Token, Scanner)> {
    let mut iter = source[scanner.start_idx..].chars();
    match iter.next() {
        Some('?') => match iter.next() {
            Some('.') => match iter.next() {
                Some('0'..='9') => None,
                _ => Some((
                    Token::Punctuator(Punctuator::QDot),
                    Scanner { line: scanner.line, column: scanner.column + 2, start_idx: scanner.start_idx + 2 },
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
                Some(c) if ('1'..='9').contains(&c) => {
                    after_iter.column += 1;
                    after_iter.start_idx += 1;
                    let after_first_digit = after_iter;
                    if let Some('_') = iter.next() {
                        after_iter.column += 1;
                        after_iter.start_idx += 1;
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
                latest.column += 1;
                latest.start_idx += 1;
                previous_was_digit = true;
            }
            '_' => {
                if sep && previous_was_digit {
                    latest.column += 1;
                    latest.start_idx += 1;
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
    if scanner.start_idx == latest.start_idx { None } else { Some(latest) }
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
        Some(ch) if ('1'..='9').contains(&ch) => {
            Some(Scanner { line: scanner.line, column: scanner.column + 1, start_idx: scanner.start_idx + 1 })
        }
        _ => None,
    }
}

fn decimal_big_integer_literal(scanner: &Scanner, source: &str) -> Option<Scanner> {
    match_char(scanner, source, '0')
        .and_then(|r| match_char(&r, source, 'n'))
        .or_else(|| {
            non_zero_digit(scanner, source)
                .and_then(|r| decimal_digits(&r, source, true).or(Some(r)))
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
    ('0'..='7').contains(&ch)
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

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
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
        Ok(x) =>
        {
            #[expect(clippy::cast_precision_loss)]
            if x < 1 << 53 {
                x as f64
            } else {
                f64::INFINITY
            }
        }
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
    if let Some(ch) = source[after.start_idx..].chars().next()
        && (ch.is_ascii_digit() || is_unicode_id_start(ch) || ch == '$' || ch == '_')
    {
        return None;
    }

    match number_style {
        NumberStyle::BigDecimal => Some((
            Token::BigInt(
                BigInt::parse_bytes(strip_sep(&source[scanner.start_idx..after.start_idx - 1]).as_bytes(), 10).unwrap(),
            ),
            after,
        )),
        NumberStyle::BigBinary => Some((
            Token::BigInt(
                BigInt::parse_bytes(strip_sep(&source[scanner.start_idx + 2..after.start_idx - 1]).as_bytes(), 2)
                    .unwrap(),
            ),
            after,
        )),
        NumberStyle::BigOctal => Some((
            Token::BigInt(
                BigInt::parse_bytes(strip_sep(&source[scanner.start_idx + 2..after.start_idx - 1]).as_bytes(), 8)
                    .unwrap(),
            ),
            after,
        )),
        NumberStyle::BigHex => Some((
            Token::BigInt(
                BigInt::parse_bytes(strip_sep(&source[scanner.start_idx + 2..after.start_idx - 1]).as_bytes(), 16)
                    .unwrap(),
            ),
            after,
        )),
        NumberStyle::Binary => {
            Some((Token::Number(int_to_number(&strip_sep(&source[scanner.start_idx + 2..after.start_idx]), 2)), after))
        }
        NumberStyle::Octal => {
            Some((Token::Number(int_to_number(&strip_sep(&source[scanner.start_idx + 2..after.start_idx]), 8)), after))
        }
        NumberStyle::Hex => {
            Some((Token::Number(int_to_number(&strip_sep(&source[scanner.start_idx + 2..after.start_idx]), 16)), after))
        }
        NumberStyle::Decimal => {
            Some((Token::Number(strip_sep(&source[scanner.start_idx..after.start_idx]).parse::<f64>().unwrap()), after))
        }
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
                Some(ch) if ch.is_ascii_digit() => None,
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
            Some('\n') => Some(Scanner { line: scanner.line + 1, column: 1, start_idx: scanner.start_idx + 2 }),
            _ => Some(Scanner { line: scanner.line + 1, column: 1, start_idx: scanner.start_idx + 1 }),
        },
        Some(ch) if ch == '\n' || ch == '\u{2028}' || ch == '\u{2029}' => {
            Some(Scanner { line: scanner.line + 1, column: 1, start_idx: scanner.start_idx + ch.len_utf8() })
        }
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
    if after == *scanner { None } else { Some(after) }
}

fn literal_string_value(source: &str) -> (JSString, bool) {
    let mut result: Vec<u16> = Vec::with_capacity(source.len());
    let mut chars = source.chars().peekable();
    let mut escapes: bool = false;
    loop {
        let ch = chars.next();
        match ch {
            None => {
                break;
            }
            Some('\\') => {
                escapes = true;
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
                        let digit_1 = u16::try_from(chars.next().unwrap().to_digit(16).unwrap())
                            .expect("digit should fit in a u16");
                        let digit_2 = u16::try_from(chars.next().unwrap().to_digit(16).unwrap())
                            .expect("digit should fit in a u16");
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
                                result.push(u16::try_from(value).expect("4 digits should fit in 16 bits"));
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

    (JSString::from(result), escapes)
}

fn string_literal(scanner: &Scanner, source: &str) -> Option<(Token, Scanner)> {
    let (after, delimiter) = match_char(scanner, source, '"')
        .and_then(|r| string_characters(&r, source, '"').or(Some(r)))
        .and_then(|r| match_char(&r, source, '"'))
        .map(|after| (after, StringDelimiter::Double))
        .or_else(|| {
            match_char(scanner, source, '\'')
                .and_then(|r| string_characters(&r, source, '\'').or(Some(r)))
                .and_then(|r| match_char(&r, source, '\''))
                .map(|after| (after, StringDelimiter::Single))
        })?;
    let start_idx = scanner.start_idx + 1;
    let after_idx = after.start_idx - 1;
    assert!(after_idx >= start_idx);
    let (value, contains_escapes_or_continuations) = literal_string_value(&source[start_idx..after_idx]);

    let st = StringToken {
        value,
        delimiter,
        raw: if contains_escapes_or_continuations { Some(String::from(&source[start_idx..after_idx])) } else { None },
    };

    Some((Token::String(st), after))
}

#[derive(Debug, PartialEq, Eq)]
pub struct TemplateData {
    pub tv: Option<JSString>,
    pub trv: JSString,
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

#[derive(Debug, PartialEq, Eq)]
struct THDCount(usize);
impl TryFrom<usize> for THDCount {
    type Error = &'static str;

    fn try_from(value: usize) -> Result<Self, Self::Error> {
        if value <= 4 { Ok(THDCount(value)) } else { Err("THDCount can only be used with values <= 4") }
    }
}

impl From<THDCount> for usize {
    fn from(tc: THDCount) -> Self {
        let THDCount(num) = tc;
        num
    }
}

fn template_hex_digits(
    iter: &mut std::iter::Peekable<std::str::Chars>,
    identifier: u16,
    count: THDCount,
    scanner: &Scanner,
) -> (Option<Vec<u16>>, Vec<u16>, Scanner, usize) {
    let mut accumulator = 0;
    let mut successful = true;
    let mut consumed = 1;
    let mut raw_chars = [identifier, 0, 0, 0, 0];
    for i in 0..usize::from(count) {
        let pot_digit = iter.next();
        if !pot_digit.is_some_and(|c| c.is_ascii_hexdigit()) {
            successful = false;
            break;
        }
        consumed += 1;
        accumulator = (accumulator << 4) + pot_digit.unwrap().to_digit(16).unwrap();
        raw_chars[i + 1] = pot_digit.unwrap() as u16;
    }
    (
        if successful {
            Some(vec![u16::try_from(accumulator).expect("accumulator should be small enough")])
        } else {
            None
        },
        raw_chars[..consumed].to_vec(),
        Scanner {
            line: scanner.line,
            column: u32::try_from(scanner.column as usize + consumed).expect("column should fit in 32 bits"),
            start_idx: scanner.start_idx + consumed,
        },
        consumed,
    )
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct CharVal(u32);
impl TryFrom<u32> for CharVal {
    type Error = &'static str;

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        if value <= 0x0010_ffff { Ok(CharVal(value)) } else { Err("CharVal can only be used with values <= 0x10ffff") }
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
        vec![u16::try_from(value).expect("value should fit in 16 bits")]
    } else {
        vec![
            u16::try_from(((value - 0x10000) >> 10) + 0xD800).expect("first half should be <= 16 bits"),
            u16::try_from(((value - 0x10000) & 0x3ff) + 0xDC00).expect("second half should fit in 16 bits"),
        ]
    }
}

fn template_hex_digits_by_value(
    iter: &mut std::iter::Peekable<std::str::Chars>,
    scanner: &Scanner,
) -> (Option<Vec<u16>>, Vec<u16>, Scanner, usize) {
    let mut accumulator = 0;
    let mut consumed = 2;
    let mut raw_chars = vec!['u' as u16, '{' as u16];
    loop {
        let pot_digit = iter.next();
        if !pot_digit.is_some_and(|c| c.is_ascii_hexdigit()) {
            let tv = if consumed == 2 || pot_digit != Some('}') {
                None
            } else {
                match CharVal::try_from(accumulator) {
                    Err(_) => None,
                    Ok(val) => {
                        raw_chars.push('}' as u16);
                        consumed += 1;
                        Some(utf16_encode_code_point(val))
                    }
                }
            };
            return (
                tv,
                raw_chars,
                Scanner {
                    line: scanner.line,
                    column: u32::try_from(scanner.column as usize + consumed).expect("column should be less than 64k"),
                    start_idx: scanner.start_idx + consumed,
                },
                consumed,
            );
        }
        consumed += 1;
        let digit = pot_digit.unwrap(); // This is ok, because we've already validated as a hex digit char.
        let value = digit.to_digit(16).unwrap(); // This is also ok, because we've already validated as a hex digit char.

        // If the _current_ accumulator fits in a CharVal, add the new value to the accumulator. (This may produce an
        // invalid char; we're only checking to prevent overflow, which takes many additional digits.)
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

fn template_escape(scanner: Scanner, source: &str) -> (Option<Vec<u16>>, Vec<u16>, Scanner, usize) {
    let mut chars = source[scanner.start_idx..].chars().peekable();
    let single_char = |ch: char, val: u16, scanner: &Scanner| {
        (
            Some(vec![val]),
            utf16_encode_code_point(CharVal::from(ch)),
            Scanner { line: scanner.line, column: scanner.column + 1, start_idx: scanner.start_idx + ch.len_utf8() },
            1,
        )
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
        Some('0') if !chars.peek().is_some_and(char::is_ascii_digit) => single_char('0', 0, &scanner),
        Some(c) if c.is_ascii_digit() => (
            None,
            utf16_encode_code_point(CharVal::from(c)),
            Scanner { line: scanner.line, column: scanner.column + 1, start_idx: scanner.start_idx + c.len_utf8() },
            1,
        ),
        Some('x') => template_hex_digits(&mut chars, 'x' as u16, THDCount::try_from(2).unwrap(), &scanner),
        Some('u') => {
            let pot_brace_or_digit = chars.peek();
            if pot_brace_or_digit.is_some_and(|c| *c == '{') {
                // \u{digits} style
                chars.next();
                template_hex_digits_by_value(&mut chars, &scanner)
            } else {
                template_hex_digits(&mut chars, 'u' as u16, THDCount::try_from(4).unwrap(), &scanner)
            }
        }
        Some(c) if ['\n', '\u{2028}', '\u{2029}'].contains(&c) => (
            Some(vec![]),
            utf16_encode_code_point(CharVal::from(c)),
            Scanner { line: scanner.line + 1, column: 1, start_idx: scanner.start_idx + c.len_utf8() },
            1,
        ),
        Some('\r') => {
            let consumed = if chars.peek() == Some(&'\n') { 2 } else { 1 };
            (
                Some(vec![]),
                vec!['\n' as u16],
                Scanner { line: scanner.line + 1, column: 1, start_idx: scanner.start_idx + consumed },
                consumed,
            )
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
                return (tv.map(JSString::from), JSString::from(trv), current_scanner);
            }
            Some('$') if chars.peek() == Some(&'{') => {
                return (tv.map(JSString::from), JSString::from(trv), current_scanner);
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

#[derive(Copy, Clone)]
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
                TemplateStyle::NoSubOrHead => Token::NoSubstitutionTemplate,
                TemplateStyle::MiddleOrTail => Token::TemplateTail,
            };
            Some((make_token(TemplateData { tv, trv }), after_trailing_quote))
        }
        None => {
            let pot_template_head = match_char(&after_chars, source, '$').and_then(|r| match_char(&r, source, '{'));
            match pot_template_head {
                Some(after_template_head) => {
                    let make_token = match style {
                        TemplateStyle::NoSubOrHead => Token::TemplateHead,
                        TemplateStyle::MiddleOrTail => Token::TemplateMiddle,
                    };
                    Some((make_token(TemplateData { tv, trv }), after_template_head))
                }
                None => None,
            }
        }
    }
}

fn template(scanner: &Scanner, source: &str) -> Option<(Token, Scanner)> {
    template_token(scanner, source, TemplateStyle::NoSubOrHead)
}

fn private_identifier(scanner: &Scanner, source: &str) -> Option<(Token, Scanner)> {
    match_char(scanner, source, '#').and_then(|s| match identifier_internal(&s, source) {
        Err((errmsg, scan)) => Some((Token::Error(errmsg), scan)),
        Ok(Some((data, scan))) => {
            // Keep the '#' as part of the string_value
            let mut new_id = Vec::<u16>::with_capacity(data.string_value.len() + 1);
            new_id.push('#' as u16);
            new_id.extend(Vec::<u16>::from(data.string_value));
            Some((
                Token::PrivateIdentifier(IdentifierData { keyword_id: data.keyword_id, string_value: new_id.into() }),
                scan,
            ))
        }
        Ok(None) => None,
    })
}

fn debug_token(scanner: &Scanner, source: &str) -> Option<(Token, Scanner)> {
    if cfg!(test) {
        match_char(scanner, source, '@').and_then(|s| match_char(&s, source, '@')).and_then(|s| {
            let c = source[s.start_idx..].chars().next();
            if let Some(c) = c {
                if is_whitespace(c) {
                    None
                } else if c == '(' {
                    let closing_idx = source[s.start_idx + 1..].find(')');
                    if let Some(idx) = closing_idx {
                        let value = source[s.start_idx + 1..s.start_idx + 1 + idx].parse::<i64>();
                        if let Ok(num) = value {
                            Some((
                                Token::Debug(DebugKind::Number(num)),
                                Scanner {
                                    line: s.line,
                                    column: u32::try_from(
                                        s.column as usize + source[s.start_idx..=s.start_idx + 1 + idx].chars().count(),
                                    )
                                    .expect("column should be less than 64k"),
                                    start_idx: s.start_idx + 1 + idx + 1,
                                },
                            ))
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                } else {
                    Some((
                        Token::Debug(DebugKind::Char(c)),
                        Scanner { line: s.line, column: s.column + 1, start_idx: s.start_idx + c.len_utf8() },
                    ))
                }
            } else {
                None
            }
        })
    } else {
        None
    }
}

fn common_token(scanner: &Scanner, source: &str) -> Option<(Token, Scanner)> {
    debug_token(scanner, source).or_else(|| {
        private_identifier(scanner, source).or_else(|| {
            identifier_name(scanner, source).or_else(|| {
                numeric_literal(scanner, source).or_else(|| {
                    punctuator(scanner, source)
                        .or_else(|| string_literal(scanner, source).or_else(|| template(scanner, source)))
                })
            })
        })
    })
}

fn div_punctuator(scanner: &Scanner, source: &str, goal: ScanGoal) -> Option<(Token, Scanner)> {
    if goal == ScanGoal::InputElementDiv || goal == ScanGoal::InputElementTemplateTail {
        let mut iter = source[scanner.start_idx..].chars();
        match iter.next() {
            Some('/') => match iter.next() {
                Some('=') => Some((
                    Token::Punctuator(Punctuator::SlashEq),
                    Scanner { line: scanner.line, column: scanner.column + 2, start_idx: scanner.start_idx + 2 },
                )),
                _ => Some((
                    Token::Punctuator(Punctuator::Slash),
                    Scanner { line: scanner.line, column: scanner.column + 1, start_idx: scanner.start_idx + 1 },
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
                Token::Punctuator(Punctuator::RightBrace),
                Scanner { line: scanner.line, column: scanner.column + 1, start_idx: scanner.start_idx + 1 },
            ))
        } else {
            None
        }
    } else {
        None
    }
}

fn regular_expression_literal(scanner: &Scanner, source: &str, goal: ScanGoal) -> Option<(Token, Scanner)> {
    if goal == ScanGoal::InputElementRegExp || goal == ScanGoal::InputElementRegExpOrTemplateTail {
        static ESREGEX: LazyLock<Regex> = LazyLock::new(|| {
            let regular_expression_flags =
                r"(?:(?:[\p{ID_Continue}$\u200C\u200D]|(?:\\u(?:[0-9a-fA-F]{4}|(?:\{[0-9a-fA-F]*\}))))*)";
            let regular_expression_non_terminator = r"(?:[^\u000A\u2028\u2029\u000D])";
            let regular_expression_backslash_sequence = format!(r"(?:\\{regular_expression_non_terminator})");
            let regular_expression_class_char =
                format!(r"(?:[^\u000A\u2028\u2029\u000D\]\\]|{regular_expression_backslash_sequence})");
            let regular_expression_class_chars = format!("(?:{regular_expression_class_char}*)");
            let regular_expression_class = format!(r"(?:\[{regular_expression_class_chars}\])");
            let regular_expression_char = format!(
                r"(?:[^\u000A\u2028\u2029\u000D\[/\\]|{regular_expression_backslash_sequence}|{regular_expression_class})"
            );
            let regular_expression_first_char = format!(
                r"(?:[^\u000A\u2028\u2029\u000D*/\[\\]|{regular_expression_backslash_sequence}|{regular_expression_class})"
            );
            let regular_expression_chars = format!("(?:{regular_expression_char}*)");
            let regular_expression_body = format!("(?:{regular_expression_first_char}{regular_expression_chars})");
            let regular_expression_literal =
                format!("(?:^/(?P<body>{regular_expression_body})/(?P<flags>{regular_expression_flags}))");
            Regex::new(&regular_expression_literal).unwrap()
        });

        match ESREGEX.captures(&source[scanner.start_idx..]) {
            None => None,
            Some(captures) => {
                let body = String::from(captures.name("body").unwrap().as_str());
                let flag_cap = captures.name("flags").unwrap();
                let flag_end = flag_cap.end();
                let flags = String::from(flag_cap.as_str());
                let chars_in_match = source[scanner.start_idx..scanner.start_idx + flag_end].chars().count();
                let after_scanner = Scanner {
                    line: scanner.line,
                    column: u32::try_from(scanner.column as usize + chars_in_match).expect("column should fit"),
                    start_idx: scanner.start_idx + flag_end,
                };
                let token = Token::RegularExpression(RegularExpressionData { body, flags });
                Some((token, after_scanner))
            }
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

#[derive(Debug, PartialEq, Eq)]
pub struct RegularExpressionData {
    pub body: String,
    pub flags: String,
}

impl fmt::Display for RegularExpressionData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "/{}/{}", self.body, self.flags)
    }
}

impl RegularExpressionData {
    pub fn validate_regular_expression_literal(&self) -> Result<(), String> {
        // Static Semantics: IsValidRegularExpressionLiteral ( literal )
        //
        // The abstract operation IsValidRegularExpressionLiteral takes argument literal (a RegularExpressionLiteral
        // Parse Node). It determines if its argument is a valid regular expression literal. It performs the following
        // steps when called:
        //
        //  1. If FlagText of literal contains any code points other than g, i, m, s, u, or y, or if it contains the
        //     same code point more than once, return false.
        //  2. Let patternText be BodyText of literal.
        //  3. If FlagText of literal contains u, let u be true; else let u be false.
        //  4. If u is false, then
        //      a. Let stringValue be CodePointsToString(patternText).
        //      b. Set patternText to the sequence of code points resulting from interpreting each of the 16-bit
        //         elements of stringValue as a Unicode BMP code point. UTF-16 decoding is not applied to the elements.
        //  5. Let parseResult be ParsePattern(patternText, u).
        //  6. If parseResult is a Parse Node, return true; else return false.
        macro_rules! flag_check {
            ( $name:ident, $ch:literal ) => {
                if $name {
                    return Err(format!("Duplicate ‘{}’ flag found in regex flags ‘{}’", $ch, self.flags));
                } else {
                    $name = true;
                }
            };
        }

        let mut g_found = false;
        let mut i_found = false;
        let mut m_found = false;
        let mut s_found = false;
        let mut u_found = false;
        let mut y_found = false;
        for ch in self.flags.chars() {
            match ch {
                'g' => flag_check!(g_found, 'g'),
                'i' => flag_check!(i_found, 'i'),
                'm' => flag_check!(m_found, 'm'),
                's' => flag_check!(s_found, 's'),
                'u' => flag_check!(u_found, 'u'),
                'y' => flag_check!(y_found, 'y'),
                _ => {
                    return Err(format!("Unknown regex flag ‘{}’ in flags ‘{}’", ch, self.flags));
                }
            }
        }

        // todo!()
        // There's more to do here --- there's a whole pattern parsing thing to make sure you've made a reasonable regex.
        // Also some unicode vs straight u16 nonsense to handle.
        Ok(())
    }
}

pub fn scan_token(scanner: &Scanner, source: &str, goal: ScanGoal) -> (Token, Location, Scanner) {
    let skip_result = skip_skippables(scanner, source);
    match skip_result {
        Err(msg) => (Token::Error(msg), Location::from(scanner), *scanner),
        Ok(after_skippable) => {
            if after_skippable.start_idx >= source.len() {
                (Token::Eof, Location::from(&after_skippable), after_skippable)
            } else {
                common_token(&after_skippable, source)
                    .or_else(|| div_punctuator(&after_skippable, source, goal))
                    .or_else(|| right_brace_punctuator(&after_skippable, source, goal))
                    .or_else(|| regular_expression_literal(&after_skippable, source, goal))
                    .or_else(|| template_substitution_tail(&after_skippable, source, goal))
                    .map_or_else(
                        || {
                            (
                                Token::Error(String::from("Unrecognized Token")),
                                Location::from(&after_skippable),
                                after_skippable,
                            )
                        },
                        |(token, after)| (token, Location::from((&after_skippable, &after)), after),
                    )
            }
        }
    }
}

#[cfg(test)]
mod tests;
