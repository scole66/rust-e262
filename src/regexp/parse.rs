#![expect(dead_code)]
use super::*;
use combinations::Combination;
use std::fmt;

const PREVIOUSLY_SCANNED: &str = "previously scanned char should still exist";

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) enum UnicodeMode {
    Allowed,
    Denied,
}
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum NamedCaptureGroups {
    Allowed,
    Denied,
}
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) enum UnicodeSetsMode {
    Allowed,
    Denied,
}

pub(crate) struct JoinDisplay<'a, T> {
    items: &'a [T],
    sep: &'a str,
}

pub(crate) fn join_display<'a, T>(items: &'a [T], sep: &'a str) -> JoinDisplay<'a, T> {
    JoinDisplay { items, sep }
}

impl<T: fmt::Display> fmt::Display for JoinDisplay<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, item) in self.items.iter().enumerate() {
            if i > 0 {
                f.write_str(self.sep)?;
            }
            write!(f, "{item}")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct Scanner<'src> {
    all: &'src [u32],
    read_idx: usize,
    left_capturing_parens: usize,
}

impl<'src> Scanner<'src> {
    pub(crate) fn new(text: &'src [u32]) -> Self {
        Scanner { all: text, read_idx: 0, left_capturing_parens: 0 }
    }

    pub(crate) fn done(&self) -> bool {
        self.read_idx >= self.all.len()
    }

    pub(crate) fn peek(&self) -> Option<u32> {
        self.all.get(self.read_idx..).and_then(|s| s.iter().next().copied())
    }

    pub(crate) fn lookahead(&self, amt: usize) -> Option<u32> {
        self.all.get(self.read_idx..).and_then(|s| {
            let mut chars = s.iter().copied();
            for _ in 0..amt {
                chars.next();
            }
            chars.next()
        })
    }

    pub(crate) fn advance(&mut self) -> Option<()> {
        if self.read_idx < self.all.len() {
            self.read_idx += 1;
            Some(())
        } else {
            None
        }
    }

    pub(crate) fn consume(&mut self, target: char) -> Option<()> {
        let target = u32::from(target);
        self.all.get(self.read_idx..).and_then(|s| s.iter().copied().next()).and_then(|ch| {
            if ch == target {
                self.read_idx += 1;
                Some(())
            } else {
                None
            }
        })
    }

    pub(crate) fn consume_any(&mut self) -> Option<u32> {
        self.all.get(self.read_idx..).and_then(|s| s.iter().copied().next()).inspect(|_| {
            self.read_idx += 1;
        })
    }

    pub(crate) fn digit(&mut self, radix: u32) -> Option<u8> {
        self.all
            .get(self.read_idx..)
            .and_then(|s| s.iter().copied().next())
            .and_then(|ch| char::try_from(ch).ok())
            .and_then(|ch| {
                ch.to_digit(radix).map(|digit| {
                    self.read_idx += 1;
                    u8::try_from(digit).expect("one digit fits in a u8")
                })
            })
    }

    pub(crate) fn hex_digit(&mut self) -> Option<u8> {
        self.all
            .get(self.read_idx..)
            .and_then(|s| s.iter().copied().next())
            .and_then(|ch| char::try_from(ch).ok())
            .and_then(|ch| ch.to_digit(16))
            .map(|digit| {
                self.read_idx += 1;
                u8::try_from(digit).expect("one hex digit fits in a u8")
            })
    }

    pub(crate) fn advance_by_bytes(&mut self, amt: usize) {
        self.read_idx += amt;
    }

    pub(crate) fn matches_at(&self, ch: char, position: usize) -> Option<usize> {
        let target = u32::from(ch);
        self.lookahead(position).and_then(|newch| if newch == target { Some(1) } else { None })
    }

    fn is_ascii_letter(ch: u32) -> bool {
        // AsciiLetter :: one of
        //      a b c d e f g h i j k l m n o p q r s t u v w x y z A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
        char::try_from(ch).map(|ch| ch.is_ascii_alphabetic()).ok().unwrap_or(false)
    }

    fn is_decimal_digit(ch: u32) -> bool {
        // DecimalDigit :: one of
        //      0 1 2 3 4 5 6 7 8 9
        char::try_from(ch).map(|ch| ch.is_ascii_digit()).ok().unwrap_or(false)
    }

    fn is_unicode_property_name_character(ch: u32) -> bool {
        // UnicodePropertyNameCharacter ::
        //      AsciiLetter
        //      _
        Self::is_ascii_letter(ch) || ch == u32::from('_')
    }

    fn is_unicode_property_value_character(ch: u32) -> bool {
        // UnicodePropertyValueCharacter ::
        //      UnicodePropertyNameCharacter
        //      DecimalDigit
        Self::is_unicode_property_name_character(ch) || Self::is_decimal_digit(ch)
    }

    fn consume_filter(&mut self, f: impl FnOnce(u32) -> bool) -> Option<u32> {
        let ch = self.peek()?;
        if f(ch) {
            self.advance().expect(PREVIOUSLY_SCANNED);
            Some(ch)
        } else {
            None
        }
    }

    pub(crate) fn unicode_property_name_character(&mut self) -> Option<u32> {
        self.consume_filter(Self::is_unicode_property_name_character)
    }

    pub(crate) fn unicode_property_value_character(&mut self) -> Option<u32> {
        self.consume_filter(Self::is_unicode_property_value_character)
    }

    fn is_identifier_start_char(ch: u32) -> bool {
        // IdentifierStartChar ::
        //      UnicodeIDStart
        //      $
        //      _
        if let Some(ch) = char::from_u32(ch) { is_unicode_id_start(ch) || ch == '$' || ch == '_' } else { false }
    }

    pub(crate) fn identifier_start_char(&mut self) -> Option<u32> {
        self.consume_filter(Self::is_identifier_start_char)
    }

    fn is_identifier_part_char(ch: u32) -> bool {
        // IdentifierPartChar ::
        //      UnicodeIDContinue
        //      $
        if let Some(ch) = char::from_u32(ch) { is_unicode_id_continue(ch) || ch == '$' } else { false }
    }

    pub(crate) fn identifier_part_char(&mut self) -> Option<u32> {
        self.consume_filter(Self::is_identifier_part_char)
    }

    fn is_pattern_char(ch: u32) -> bool {
        ![
            u32::from('^'),
            u32::from('$'),
            u32::from('\\'),
            u32::from('.'),
            u32::from('*'),
            u32::from('+'),
            u32::from('?'),
            u32::from('('),
            u32::from(')'),
            u32::from('['),
            u32::from(']'),
            u32::from('{'),
            u32::from('}'),
            u32::from('|'),
        ]
        .contains(&ch)
    }
}

// ClassSetReservedPunctuator :: one of
//      & - ! # % , : ; < = > @ ` ~
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
struct ClassSetReservedPunctuator(u32);

impl ClassSetReservedPunctuator {
    pub(crate) fn parse(scanner: &Scanner) -> Option<(Self, usize)> {
        let mut new_scanner = scanner.clone();
        let ch = new_scanner.consume_filter(|ch| {
            [
                u32::from('&'),
                u32::from('-'),
                u32::from('!'),
                u32::from('#'),
                u32::from('%'),
                u32::from(','),
                u32::from(':'),
                u32::from(';'),
                u32::from('<'),
                u32::from('='),
                u32::from('>'),
                u32::from('@'),
                u32::from('`'),
                u32::from('~'),
            ]
            .contains(&ch)
        })?;
        Some((Self(ch), new_scanner.read_idx - scanner.read_idx))
    }
}

// ClassSetSyntaxCharacter :: one of
//      ( ) [ ] { } / - \ |
#[derive(Debug, Clone)]
struct ClassSetSyntaxCharacter(u32);

impl ClassSetSyntaxCharacter {
    pub(crate) fn parse(scanner: &Scanner) -> Option<(Self, usize)> {
        let ch = scanner.peek();
        if let Some(ch) = ch
            && [
                u32::from('('),
                u32::from(')'),
                u32::from('['),
                u32::from(']'),
                u32::from('{'),
                u32::from('}'),
                u32::from('/'),
                u32::from('-'),
                u32::from('\\'),
                u32::from('|'),
            ]
            .contains(&ch)
        {
            return Some((Self(ch), 1));
        }
        None
    }
}

// ClassSetReservedDoublePunctuator :: one of
//      && !! ## $$ %% ** ++ ,, .. :: ;; << == >> ?? @@ ^^ `` ~~
#[derive(Debug, Clone)]
struct ClassSetReservedDoublePunctuator {}

impl ClassSetReservedDoublePunctuator {
    pub(crate) fn parse(scanner: &Scanner) -> Option<(Self, usize)> {
        let och_left = scanner.peek();
        let och_right = scanner.lookahead(1);
        if let (Some(ch_left), Some(ch_right)) = (och_left, och_right)
            && [
                u32::from('&'),
                u32::from('!'),
                u32::from('#'),
                u32::from('$'),
                u32::from('%'),
                u32::from('*'),
                u32::from('+'),
                u32::from(','),
                u32::from('.'),
                u32::from(':'),
                u32::from(';'),
                u32::from('<'),
                u32::from('='),
                u32::from('>'),
                u32::from('?'),
                u32::from('@'),
                u32::from('^'),
                u32::from('`'),
                u32::from('~'),
            ]
            .contains(&ch_left)
            && ch_left == ch_right
        {
            return Some((Self {}, 2));
        }
        None
    }
}

// ClassSetCharacter ::
//      [lookahead ∉ ClassSetReservedDoublePunctuator] SourceCharacter but not ClassSetSyntaxCharacter
//      \ CharacterEscape[+UnicodeMode]
//      \ ClassSetReservedPunctuator
//      \b
#[derive(Debug, Clone, Eq, PartialEq)]
enum ClassSetCharacter {
    SourceCharacter(u32),
    CharacterEscape(CharacterEscape),
    ClassSetReservedPunctuator(ClassSetReservedPunctuator),
    LetterB,
}

impl ClassSetCharacter {
    pub(crate) fn parse(scanner: &Scanner) -> Option<(Self, usize)> {
        if ClassSetReservedDoublePunctuator::parse(scanner).is_some() {
            None
        } else if let Some((cssc, consumed)) = ClassSetSyntaxCharacter::parse(scanner) {
            let ClassSetSyntaxCharacter(cssc_match) = cssc;
            if cssc_match == u32::from('\\') {
                let mut newscan = scanner.clone();
                newscan.advance_by_bytes(consumed);
                if let Some((ce, consumed)) = CharacterEscape::parse(&newscan, UnicodeMode::Allowed) {
                    return Some((Self::CharacterEscape(ce), newscan.read_idx + consumed - scanner.read_idx));
                }
                if let Some((csrp, consumed)) = ClassSetReservedPunctuator::parse(&newscan) {
                    return Some((
                        Self::ClassSetReservedPunctuator(csrp),
                        newscan.read_idx + consumed - scanner.read_idx,
                    ));
                }
                if newscan.peek() == Some(u32::from('b')) {
                    newscan.advance().expect(PREVIOUSLY_SCANNED);
                    return Some((Self::LetterB, consumed + 1));
                }
                None
            } else {
                None
            }
        } else {
            // SourceCharacter but not ClassSetSyntaxCharacter
            let mut new_scanner = scanner.clone();
            let ch = new_scanner.consume_any()?;
            Some((Self::SourceCharacter(ch), new_scanner.read_idx - scanner.read_idx))
        }
    }

    fn character_value(&self) -> u32 {
        match self {
            ClassSetCharacter::SourceCharacter(val) => {
                // ClassSetCharacter :: SourceCharacter but not ClassSetSyntaxCharacter
                //      1. Let ch be the code point matched by SourceCharacter.
                //      2. Return the numeric value of ch.
                *val
            }
            ClassSetCharacter::CharacterEscape(character_escape) => character_escape.character_value(),
            ClassSetCharacter::ClassSetReservedPunctuator(punc) => {
                // ClassSetCharacter :: \ ClassSetReservedPunctuator
                //      1. Let ch be the code point matched by ClassSetReservedPunctuator.
                //      2. Return the numeric value of ch.
                punc.0
            }
            ClassSetCharacter::LetterB => {
                // ClassSetCharacter :: \b
                //      1. Return the numeric value of U+0008 (BACKSPACE).
                8
            }
        }
    }

    fn display_string(ch: u32) -> String {
        if let Some(ctrl) = match ch {
            0 => Some('0'),
            8 => Some('b'),
            9 => Some('t'),
            10 => Some('n'),
            11 => Some('v'),
            12 => Some('f'),
            13 => Some('r'),
            _ => None,
        } {
            return format!("\\{ctrl}");
        }

        let as_char = char::try_from(ch);
        if let Ok(ch) = as_char
            && ['&', '-', '!', '#', '%', ',', ':', ';', '<', '=', '>', '@', '`', '~'].contains(&ch)
        {
            return format!("\\{ch}");
        }
        if (1..=26).contains(&ch) {
            let repr = char::try_from(ch + 64).expect("should be fine");
            return format!("\\c{repr}");
        }
        if (0x7f..=0xa0).contains(&ch) {
            return format!("\\x{ch:x}");
        }

        if let Ok(ch) = as_char {
            return format!("{ch}");
        }

        format!("\\u{{{ch:x}}}")
    }
}

// CharacterEscape[UnicodeMode] ::
//      ControlEscape
//      c AsciiLetter
//      0 [lookahead ∉ DecimalDigit]
//      HexEscapeSequence
//      RegExpUnicodeEscapeSequence[?UnicodeMode]
//      IdentityEscape[?UnicodeMode]
#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) enum CharacterEscape {
    ControlEscape(ControlEscape),
    CAsciiLetter(AsciiLetter),
    Zero,
    HexEscapeSequence(HexEscapeSequence),
    RegExpUnicodeEscapeSequence(RegExpUnicodeEscapeSequence),
    IdentityEscape(IdentityEscape),
}

impl fmt::Display for CharacterEscape {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CharacterEscape::ControlEscape(control_escape) => control_escape.fmt(f),
            CharacterEscape::CAsciiLetter(ascii_letter) => write!(f, "c{ascii_letter}"),
            CharacterEscape::Zero => write!(f, "0"),
            CharacterEscape::HexEscapeSequence(hex_escape_sequence) => hex_escape_sequence.fmt(f),
            CharacterEscape::RegExpUnicodeEscapeSequence(reg_exp_unicode_escape_sequence) => {
                reg_exp_unicode_escape_sequence.fmt(f)
            }
            CharacterEscape::IdentityEscape(identity_escape) => identity_escape.fmt(f),
        }
    }
}

impl CharacterEscape {
    fn parse(scanner: &Scanner, mode: UnicodeMode) -> Option<(Self, usize)> {
        if let Some((ce, consumed)) = ControlEscape::parse(scanner) {
            return Some((Self::ControlEscape(ce), consumed));
        }
        let ch = scanner.peek();
        if ch == Some(u32::from('c')) {
            let mut new_scanner = scanner.clone();
            new_scanner.advance().expect(PREVIOUSLY_SCANNED);
            if let Some((al, consumed)) = AsciiLetter::parse(&new_scanner) {
                return Some((Self::CAsciiLetter(al), 1 + consumed));
            }
        } else if ch == Some(u32::from('0')) {
            let lookahead = scanner.lookahead(1);
            let lookahead_is_digit = if let Some(digit) = lookahead { Scanner::is_decimal_digit(digit) } else { false };
            if !lookahead_is_digit {
                let mut new_scanner = scanner.clone();
                new_scanner.advance().expect(PREVIOUSLY_SCANNED);
                return Some((Self::Zero, 1));
            }
        }
        if let Some((hes, consumed)) = HexEscapeSequence::parse(scanner) {
            return Some((Self::HexEscapeSequence(hes), consumed));
        }
        if let Some((reues, consumed)) = RegExpUnicodeEscapeSequence::parse(scanner, mode) {
            return Some((Self::RegExpUnicodeEscapeSequence(reues), consumed));
        }
        if let Some((ie, consumed)) = IdentityEscape::parse(scanner, mode) {
            return Some((Self::IdentityEscape(ie), consumed));
        }
        None
    }

    pub(crate) fn character_value(&self) -> u32 {
        match self {
            CharacterEscape::ControlEscape(ce) => match ce {
                // CharacterEscape :: ControlEscape
                //      1. Return the numeric value according to Table 64.
                //
                // Table 64: ControlEscape Code Point Values
                // +---------------+---------------+------------+----------------------+--------+
                // | ControlEscape | Numeric Value | Code Point | Unicode Name         | Symbol |
                // +---------------+---------------+------------+----------------------+--------+
                // | t             | 9             | U+0009     | CHARACTER TABULATION | <HT>   |
                // | n             | 10            | U+000A     | LINE FEED (LF)       | <LF>   |
                // | v             | 11            | U+000B     | LINE TABULATION      | <VT>   |
                // | f             | 12            | U+000C     | FORM FEED (FF)       | <FF>   |
                // | r             | 13            | U+000D     | CARRIAGE RETURN (CR) | <CR>   |
                // +---------------+---------------+------------+----------------------+--------+
                ControlEscape::Tee => 0x09, // Horizontal Tab
                ControlEscape::En => 0x0A,  // Line Feed
                ControlEscape::Vee => 0x0B, // Vertical Tab
                ControlEscape::Eff => 0x0C, // Form Feed
                ControlEscape::Ar => 0x0D,  // Carriage Return
            },
            CharacterEscape::CAsciiLetter(al) => {
                // CharacterEscape :: c AsciiLetter
                //      1. Let ch be the code point matched by AsciiLetter.
                //      2. Let i be the numeric value of ch.
                //      3. Return the remainder of dividing i by 32.
                al.0 % 32
            }
            CharacterEscape::Zero => {
                // CharacterEscape :: 0 [lookahead ∉ DecimalDigit]
                //      1. Return the numeric value of U+0000 (NULL).
                // NOTE | \0 represents the <NUL> character and cannot be followed by a decimal digit.
                0x00
            }
            CharacterEscape::HexEscapeSequence(hes) => {
                // CharacterEscape :: HexEscapeSequence
                //      1. Return the MV of HexEscapeSequence.
                hes.mv()
            }
            CharacterEscape::RegExpUnicodeEscapeSequence(reues) => reues.character_value(),
            CharacterEscape::IdentityEscape(escape) => {
                // CharacterEscape :: IdentityEscape
                //      1. Let ch be the code point matched by IdentityEscape.
                //      2. Return the numeric value of ch.
                escape.character_value()
            }
        }
    }
}

// ControlEscape :: one of
//      f n r t v
#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) enum ControlEscape {
    Eff,
    En,
    Ar,
    Tee,
    Vee,
}
impl fmt::Display for ControlEscape {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ControlEscape::Eff => write!(f, "f"),
            ControlEscape::En => write!(f, "n"),
            ControlEscape::Ar => write!(f, "r"),
            ControlEscape::Tee => write!(f, "t"),
            ControlEscape::Vee => write!(f, "v"),
        }
    }
}
impl ControlEscape {
    fn parse(scanner: &Scanner) -> Option<(Self, usize)> {
        let ch = scanner.peek();
        match ch {
            Some(x) if x == u32::from('f') => Some((Self::Eff, 1)),
            Some(x) if x == u32::from('n') => Some((Self::En, 1)),
            Some(x) if x == u32::from('r') => Some((Self::Ar, 1)),
            Some(x) if x == u32::from('t') => Some((Self::Tee, 1)),
            Some(x) if x == u32::from('v') => Some((Self::Vee, 1)),
            _ => None,
        }
    }
}

// AsciiLetter :: one of
//      a b c d e f g h i j k l m n o p q r s t u v w x y z A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct AsciiLetter(u32);
impl fmt::Display for AsciiLetter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", char::try_from(self.0).expect("ascii should be fine"))
    }
}
impl AsciiLetter {
    fn parse(scanner: &Scanner) -> Option<(Self, usize)> {
        scanner.peek().and_then(|ch| {
            if char::try_from(ch).ok().is_some_and(|as_char| as_char.is_ascii_alphabetic()) {
                Some((Self(ch), 1))
            } else {
                None
            }
        })
    }
}

// HexEscapeSequence ::
//      x HexDigit HexDigit
#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct HexEscapeSequence(u8);
impl fmt::Display for HexEscapeSequence {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "x{:x}", self.0)
    }
}
impl HexEscapeSequence {
    fn parse(scanner: &Scanner) -> Option<(Self, usize)> {
        let mut new_scanner = scanner.clone();
        new_scanner.consume('x')?;
        let d1 = new_scanner.hex_digit()?;
        let d2 = new_scanner.hex_digit()?;
        Some((Self((d1 << 4) | d2), new_scanner.read_idx - scanner.read_idx))
    }
    fn mv(&self) -> u32 {
        u32::from(self.0)
    }
}

// RegExpUnicodeEscapeSequence[UnicodeMode] ::
// [+UnicodeMode] u HexLeadSurrogate \u HexTrailSurrogate
// [+UnicodeMode] u HexLeadSurrogate
// [+UnicodeMode] u HexTrailSurrogate
// [+UnicodeMode] u HexNonSurrogate
// [~UnicodeMode] u Hex4Digits
// [+UnicodeMode] u{ CodePoint }
#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct RegExpUnicodeEscapeSequence(u32);
impl fmt::Display for RegExpUnicodeEscapeSequence {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0 > 0xffff { write!(f, "u{{{:x}}}", self.0) } else { write!(f, "u{:04x}", self.0) }
    }
}
impl RegExpUnicodeEscapeSequence {
    fn parse(scanner: &Scanner, mode: UnicodeMode) -> Option<(Self, usize)> {
        let mut new_scanner = scanner.clone();
        new_scanner.consume('u')?;
        if mode == UnicodeMode::Denied {
            // [~UnicodeMode] u Hex4Digits
            let d1 = u32::from(new_scanner.hex_digit()?);
            let d2 = u32::from(new_scanner.hex_digit()?);
            let d3 = u32::from(new_scanner.hex_digit()?);
            let d4 = u32::from(new_scanner.hex_digit()?);
            Some((Self((d1 << 12) | (d2 << 8) | (d3 << 4) | d4), new_scanner.read_idx - scanner.read_idx))
        } else {
            match new_scanner.consume('{') {
                Some(()) => {
                    // [+UnicodeMode] u{ CodePoint }
                    // RegExpUnicodeEscapeSequence :: u{ CodePoint }
                    //      1. Return the MV of CodePoint.
                    let mut value = None;
                    while new_scanner.peek()? != u32::from('}') {
                        let digit = u32::from(new_scanner.hex_digit()?);
                        value = Some(match value {
                            None => digit,
                            Some(previous) => {
                                let new_value = (previous << 4) | digit;
                                if new_value > 0x10_FFFF {
                                    return None;
                                }
                                new_value
                            }
                        });
                    }
                    if let Some(value) = value {
                        new_scanner.consume('}')?;
                        Some((Self(value), new_scanner.read_idx - scanner.read_idx))
                    } else {
                        None
                    }
                }
                None => {
                    // [+UnicodeMode] u HexLeadSurrogate \u HexTrailSurrogate
                    // [+UnicodeMode] u HexLeadSurrogate
                    // [+UnicodeMode] u HexTrailSurrogate
                    // [+UnicodeMode] u HexNonSurrogate

                    let d1 = u16::from(new_scanner.hex_digit()?);
                    let d2 = u16::from(new_scanner.hex_digit()?);
                    let d3 = u16::from(new_scanner.hex_digit()?);
                    let d4 = u16::from(new_scanner.hex_digit()?);
                    let word1 = (d1 << 12) | (d2 << 8) | (d3 << 4) | d4;
                    if (0xD800..=0xDBFF).contains(&word1) {
                        fn after_scanner(scanner: &Scanner) -> Option<(u16, usize)> {
                            let mut new_scanner = scanner.clone();
                            new_scanner.consume('\\')?;
                            new_scanner.consume('u')?;
                            let d1 = u16::from(new_scanner.hex_digit()?);
                            let d2 = u16::from(new_scanner.hex_digit()?);
                            let d3 = u16::from(new_scanner.hex_digit()?);
                            let d4 = u16::from(new_scanner.hex_digit()?);
                            let word = (d1 << 12) | (d2 << 8) | (d3 << 4) | d4;
                            if (0xDC00..=0xDFFF).contains(&word) {
                                Some((word, new_scanner.read_idx - scanner.read_idx))
                            } else {
                                None
                            }
                        }
                        let attempt = after_scanner(&new_scanner);
                        match attempt {
                            None => Some((Self(u32::from(word1)), new_scanner.read_idx - scanner.read_idx)),
                            Some((word2, size)) => Some((
                                // RegExpUnicodeEscapeSequence :: u HexLeadSurrogate \u HexTrailSurrogate
                                //      1. Let lead be the CharacterValue of HexLeadSurrogate.
                                //      2. Let trail be the CharacterValue of HexTrailSurrogate.
                                //      3. Let cp be UTF16SurrogatePairToCodePoint(lead, trail).
                                //      4. Return the numeric value of cp.
                                Self(utf16_surrogate_pair_to_code_point(word1, word2)),
                                size + new_scanner.read_idx - scanner.read_idx,
                            )),
                        }
                    } else {
                        Some((Self(u32::from(word1)), new_scanner.read_idx - scanner.read_idx))
                    }
                }
            }
        }
    }
    fn character_value(&self) -> u32 {
        self.0
    }
}

// IdentityEscape[UnicodeMode] ::
//      [+UnicodeMode] SyntaxCharacter
//      [+UnicodeMode] /
//      [~UnicodeMode] SourceCharacter but not UnicodeIDContinue
#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct IdentityEscape(u32);
impl fmt::Display for IdentityEscape {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let c = self.0;
        if let Ok(ch) = char::try_from(c)
            && !ch.is_control()
        {
            write!(f, "{ch}")
        } else {
            write!(f, "\\u{c:X}")
        }
    }
}
impl IdentityEscape {
    fn parse(scanner: &Scanner, mode: UnicodeMode) -> Option<(Self, usize)> {
        match mode {
            UnicodeMode::Allowed => {
                let peeked = scanner.peek()?;
                if [
                    u32::from('^'),
                    u32::from('$'),
                    u32::from('\\'),
                    u32::from('.'),
                    u32::from('*'),
                    u32::from('+'),
                    u32::from('?'),
                    u32::from('('),
                    u32::from(')'),
                    u32::from('['),
                    u32::from(']'),
                    u32::from('{'),
                    u32::from('}'),
                    u32::from('|'),
                    u32::from('/'),
                ]
                .contains(&peeked)
                {
                    Some((Self(peeked), 1))
                } else {
                    None
                }
            }
            UnicodeMode::Denied => {
                let mut new_scanner = scanner.clone();
                let ch = new_scanner.consume_any()?;
                if let Some(as_ch) = char::from_u32(ch)
                    && is_unicode_id_continue(as_ch)
                {
                    None
                } else {
                    Some((Self(ch), new_scanner.read_idx - scanner.read_idx))
                }
            }
        }
    }
    fn character_value(&self) -> u32 {
        // CharacterEscape :: IdentityEscape
        //      1. Let ch be the code point matched by IdentityEscape.
        //      2. Return the numeric value of ch.
        self.0
    }
}

// SyntaxCharacter :: one of
//      ^ $ \ . * + ? ( ) [ ] { } |

// UnicodeIDContinue ::
//      any Unicode code point with the Unicode property “ID_Continue”

// AtomEscape[UnicodeMode, NamedCaptureGroups] ::
//      DecimalEscape
//      CharacterClassEscape[?UnicodeMode]
//      CharacterEscape[?UnicodeMode]
//      [+NamedCaptureGroups] k GroupName[?UnicodeMode]
#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) enum AtomEscape {
    DecimalEscape(DecimalEscape),
    CharacterClassEscape(CharacterClassEscape),
    CharacterEscape(CharacterEscape),
    GroupName(Box<GroupName>),
}
impl fmt::Display for AtomEscape {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AtomEscape::DecimalEscape(decimal_escape) => decimal_escape.fmt(f),
            AtomEscape::CharacterClassEscape(character_class_escape) => character_class_escape.fmt(f),
            AtomEscape::CharacterEscape(character_escape) => character_escape.fmt(f),
            AtomEscape::GroupName(group_name) => write!(f, "k{group_name}"),
        }
    }
}
impl AtomEscape {
    fn parse(scanner: &Scanner, unicode: UnicodeMode, groups: NamedCaptureGroups) -> Option<(Self, usize)> {
        if let Some((de, amt)) = DecimalEscape::parse(scanner) {
            Some((Self::DecimalEscape(de), amt))
        } else if let Some((cce, amt)) = CharacterClassEscape::parse(scanner, unicode) {
            Some((Self::CharacterClassEscape(cce), amt))
        } else if let Some((ce, amt)) = CharacterEscape::parse(scanner, unicode) {
            Some((Self::CharacterEscape(ce), amt))
        } else if groups == NamedCaptureGroups::Allowed {
            let mut new_scanner = scanner.clone();
            new_scanner.consume('k')?;
            let (gn, amt) = GroupName::parse(&new_scanner, unicode)?;
            new_scanner.read_idx += amt;
            Some((Self::GroupName(Box::new(gn)), new_scanner.read_idx - scanner.read_idx))
        } else {
            None
        }
    }

    fn early_errors(
        &self,
        left_paren_count: usize,
        group_specifiers: &[&GroupSpecifier],
        usm: UnicodeSetsMode,
    ) -> Vec<Object> {
        match self {
            AtomEscape::DecimalEscape(decimal_escape) => {
                let mut errs = vec![];
                if decimal_escape.capturing_group_number() > left_paren_count {
                    errs.push(create_syntax_error_object("backreference too large", None));
                }
                errs
            }
            AtomEscape::CharacterClassEscape(node) => node.early_errors(usm),
            AtomEscape::CharacterEscape(_) => vec![],
            AtomEscape::GroupName(group_name) => {
                let mut errs = vec![];
                if group_specifiers_that_match(group_specifiers, group_name).is_empty() {
                    errs.push(create_syntax_error_object("unmatchable group name", None));
                }
                errs.append(&mut group_name.early_errors());
                errs
            }
        }
    }
}

fn group_specifiers_that_match(group_specifiers: &[&GroupSpecifier], this_group_name: &GroupName) -> Vec<usize> {
    // Static Semantics: GroupSpecifiersThatMatch ( thisGroupName )
    //
    // The abstract operation GroupSpecifiersThatMatch takes argument thisGroupName (a GroupName Parse Node) and returns
    // a List of GroupSpecifier Parse Nodes. It performs the following steps when called:
    //
    // 1. Let name be the CapturingGroupName of thisGroupName.
    // 2. Let pattern be the Pattern containing thisGroupName.
    // 3. Let result be a new empty List.
    // 4. For each GroupSpecifier gs that pattern contains, do
    //    a. If the CapturingGroupName of gs is name, then
    //       i. Append gs to result.
    // 5. Return result.
    let name = this_group_name.capturing_group_name();
    group_specifiers
        .iter()
        .enumerate()
        .filter_map(|(idx, gs)| if gs.capturing_group_name() == name { Some(idx) } else { None })
        .collect()
}
// DecimalEscape ::
//      NonZeroDigit DecimalDigits[~Sep]opt [lookahead ∉ DecimalDigit]
// DecimalDigits[Sep] ::
//      DecimalDigit
//      DecimalDigits[?Sep] DecimalDigit
//      [+Sep] DecimalDigits[+Sep] NumericLiteralSeparator DecimalDigit
// NonZeroDigit :: one of
//      1 2 3 4 5 6 7 8 9
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub(crate) struct DecimalEscape(usize);
impl fmt::Display for DecimalEscape {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl DecimalEscape {
    fn parse(scanner: &Scanner) -> Option<(Self, usize)> {
        const NUMBER_OK: &str = "number chars should be transformable to digits";
        let mut new_scanner = scanner.clone();
        let d1 = new_scanner.consume_filter(|c| (u32::from('1')..=u32::from('9')).contains(&c))?;
        let d1 = char::try_from(d1).expect("should be fine; we matched a value in 1..9");
        let mut value = usize::try_from(d1.to_digit(10).expect(NUMBER_OK)).expect(NUMBER_OK);
        while let Some(ch) = new_scanner.consume_filter(Scanner::is_decimal_digit) {
            let ch = char::try_from(ch).expect("should be fine; we matched a value in 1..9");
            value = value
                .checked_mul(10)?
                .checked_add(usize::try_from(ch.to_digit(10).expect(NUMBER_OK)).expect(NUMBER_OK))?;
        }
        Some((Self(value), new_scanner.read_idx - scanner.read_idx))
    }

    fn capturing_group_number(self) -> usize {
        self.0
    }
}

// CharacterClassEscape[UnicodeMode] ::
//      d
//      D
//      s
//      S
//      w
//      W
//      [+UnicodeMode] p{ UnicodePropertyValueExpression }
//      [+UnicodeMode] P{ UnicodePropertyValueExpression }
#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) enum CharacterClassEscape {
    Digit,
    NotDigit,
    Whitespace,
    NotWhitespace,
    Word,
    NotWord,
    Property(Box<UnicodePropertyValueExpression>),
    NotProperty(Box<UnicodePropertyValueExpression>),
}
impl fmt::Display for CharacterClassEscape {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CharacterClassEscape::Digit => write!(f, "d"),
            CharacterClassEscape::NotDigit => write!(f, "D"),
            CharacterClassEscape::Whitespace => write!(f, "s"),
            CharacterClassEscape::NotWhitespace => write!(f, "S"),
            CharacterClassEscape::Word => write!(f, "w"),
            CharacterClassEscape::NotWord => write!(f, "W"),
            CharacterClassEscape::Property(unicode_property_value_expression) => {
                write!(f, "p{{{unicode_property_value_expression}}}")
            }
            CharacterClassEscape::NotProperty(unicode_property_value_expression) => {
                write!(f, "P{{{unicode_property_value_expression}}}")
            }
        }
    }
}

impl CharacterClassEscape {
    fn parse(scanner: &Scanner, mode: UnicodeMode) -> Option<(Self, usize)> {
        fn property_value(
            scanner: &Scanner,
            f: fn(Box<UnicodePropertyValueExpression>) -> CharacterClassEscape,
            extra: usize,
        ) -> Option<(CharacterClassEscape, usize)> {
            let mut new_scanner = scanner.clone();
            new_scanner.consume('{')?;
            let (exp, amt) = UnicodePropertyValueExpression::parse(&new_scanner)?;
            new_scanner.read_idx += amt;
            new_scanner.consume('}')?;
            Some((f(Box::new(exp)), new_scanner.read_idx - scanner.read_idx + extra))
        }

        let mut new_scanner = scanner.clone();
        let ch = new_scanner.consume_any()?;
        let amt_used = new_scanner.read_idx - scanner.read_idx;
        let ch = char::try_from(ch).ok()?;
        match (ch, mode) {
            ('d', _) => Some((Self::Digit, amt_used)),
            ('D', _) => Some((Self::NotDigit, amt_used)),
            ('s', _) => Some((Self::Whitespace, amt_used)),
            ('S', _) => Some((Self::NotWhitespace, amt_used)),
            ('w', _) => Some((Self::Word, amt_used)),
            ('W', _) => Some((Self::NotWord, amt_used)),
            ('p', UnicodeMode::Allowed) => property_value(&new_scanner, Self::Property, amt_used),
            ('P', UnicodeMode::Allowed) => property_value(&new_scanner, Self::NotProperty, amt_used),
            _ => None,
        }
    }

    fn early_errors(&self, usm: UnicodeSetsMode) -> Vec<Object> {
        match self {
            CharacterClassEscape::Digit
            | CharacterClassEscape::NotDigit
            | CharacterClassEscape::Whitespace
            | CharacterClassEscape::NotWhitespace
            | CharacterClassEscape::Word
            | CharacterClassEscape::NotWord => vec![],
            CharacterClassEscape::Property(node) => node.early_errors(usm),
            CharacterClassEscape::NotProperty(node) => {
                // CharacterClassEscape :: P{ UnicodePropertyValueExpression }
                // It is a Syntax Error if MayContainStrings of the UnicodePropertyValueExpression is true.
                let mut errs = vec![];
                if node.may_contain_strings() {
                    errs.push(create_syntax_error_object("Cannot contain strings", None));
                }
                errs.append(&mut node.early_errors(usm));
                errs
            }
        }
    }

    fn may_contain_strings(&self) -> bool {
        // Static Semantics: MayContainStrings
        // The syntax-directed operation MayContainStrings takes no arguments and returns a Boolean.
        match self {
            CharacterClassEscape::Digit
            | CharacterClassEscape::NotDigit
            | CharacterClassEscape::Whitespace
            | CharacterClassEscape::NotWhitespace
            | CharacterClassEscape::Word
            | CharacterClassEscape::NotWord
            | CharacterClassEscape::NotProperty(_) => {
                // CharacterClassEscape ::
                //      d
                //      D
                //      s
                //      S
                //      w
                //      W
                //      P{ UnicodePropertyValueExpression }
                // 1. Return false.
                false
            }
            CharacterClassEscape::Property(node) => node.may_contain_strings(),
        }
    }
}

// UnicodePropertyValueExpression ::
//      UnicodePropertyName = UnicodePropertyValue
//      LoneUnicodePropertyNameOrValue
#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) enum UnicodePropertyValueExpression {
    NameValue { name: UnicodePropertyName, value: UnicodePropertyValue },
    Lone(LoneUnicodePropertyNameOrValue),
}

impl fmt::Display for UnicodePropertyValueExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnicodePropertyValueExpression::NameValue { name, value } => write!(f, "{name}={value}"),
            UnicodePropertyValueExpression::Lone(lone_unicode_property_name_or_value) => {
                lone_unicode_property_name_or_value.fmt(f)
            }
        }
    }
}

struct PotentialPropertyValue {
    name: &'static str,
    alias: &'static str,
}
struct PotentialPropertyCategory {
    property_name: &'static str,
    alias_name: &'static str,
    potential_values: &'static [PotentialPropertyValue],
}

impl UnicodePropertyValueExpression {
    fn parse(scanner: &Scanner) -> Option<(Self, usize)> {
        fn name_value(scanner: &Scanner) -> Option<(UnicodePropertyValueExpression, usize)> {
            let mut new_scanner = scanner.clone();
            let (name, amt_used) = UnicodePropertyName::parse(&new_scanner)?;
            new_scanner.read_idx += amt_used;
            new_scanner.consume('=')?;
            let (value, amt_used) = UnicodePropertyValue::parse(&new_scanner)?;
            new_scanner.read_idx += amt_used;
            Some((UnicodePropertyValueExpression::NameValue { name, value }, new_scanner.read_idx - scanner.read_idx))
        }
        if let Some(pair) = name_value(scanner) {
            Some(pair)
        } else if let Some((name, amt_used)) = LoneUnicodePropertyNameOrValue::parse(scanner) {
            Some((Self::Lone(name), amt_used))
        } else {
            None
        }
    }

    const PROPERTY_POSSIBILITIES: &[PotentialPropertyCategory] = &[
        PotentialPropertyCategory {
            property_name: "General_Category",
            alias_name: "gc",
            potential_values: &[
                PotentialPropertyValue { name: "C", alias: "Other" },
                PotentialPropertyValue { name: "Cc", alias: "Control" },
                PotentialPropertyValue { name: "Cf", alias: "Format" },
                PotentialPropertyValue { name: "Cn", alias: "Unassigned" },
                PotentialPropertyValue { name: "Co", alias: "Private_Use" },
                PotentialPropertyValue { name: "Cs", alias: "Surrogate" },
                PotentialPropertyValue { name: "L", alias: "Letter" },
                PotentialPropertyValue { name: "LC", alias: "Cased_Letter" },
                PotentialPropertyValue { name: "Ll", alias: "Lowercase_Letter" },
                PotentialPropertyValue { name: "Lm", alias: "Modifier_Letter" },
                PotentialPropertyValue { name: "Lo", alias: "Other_Letter" },
                PotentialPropertyValue { name: "Lt", alias: "Titlecase_Letter" },
                PotentialPropertyValue { name: "Lu", alias: "Uppercase_Letter" },
                PotentialPropertyValue { name: "M", alias: "Mark" },
                PotentialPropertyValue { name: "Mc", alias: "Spacing_Mark" },
                PotentialPropertyValue { name: "Me", alias: "Enclosing_Mark" },
                PotentialPropertyValue { name: "Mn", alias: "Nonspacing_Mark" },
                PotentialPropertyValue { name: "N", alias: "Number" },
                PotentialPropertyValue { name: "Nd", alias: "Decimal_Number" },
                PotentialPropertyValue { name: "Nl", alias: "Letter_Number" },
                PotentialPropertyValue { name: "No", alias: "Other_Number" },
                PotentialPropertyValue { name: "P", alias: "Punctuation" },
                PotentialPropertyValue { name: "Pc", alias: "Connector_Punctuation" },
                PotentialPropertyValue { name: "Pd", alias: "Dash_Punctuation" },
                PotentialPropertyValue { name: "Pe", alias: "Close_Punctuation" },
                PotentialPropertyValue { name: "Pf", alias: "Final_Punctuation" },
                PotentialPropertyValue { name: "Pi", alias: "Initial_Punctuation" },
                PotentialPropertyValue { name: "Po", alias: "Other_Punctuation" },
                PotentialPropertyValue { name: "Ps", alias: "Open_Punctuation" },
                PotentialPropertyValue { name: "S", alias: "Symbol" },
                PotentialPropertyValue { name: "Sc", alias: "Currency_Symbol" },
                PotentialPropertyValue { name: "Sk", alias: "Modifier_Symbol" },
                PotentialPropertyValue { name: "Sm", alias: "Math_Symbol" },
                PotentialPropertyValue { name: "So", alias: "Other_Symbol" },
                PotentialPropertyValue { name: "Z", alias: "Separator" },
                PotentialPropertyValue { name: "Zl", alias: "Line_Separator" },
                PotentialPropertyValue { name: "Zp", alias: "Paragraph_Separator" },
                PotentialPropertyValue { name: "Zs", alias: "Space_Separator" },
            ],
        },
        PotentialPropertyCategory {
            property_name: "Script",
            alias_name: "sc",
            potential_values: &[
                PotentialPropertyValue { name: "Adlm", alias: "Adlam" },
                PotentialPropertyValue { name: "Aghb", alias: "Caucasian_Albanian" },
                PotentialPropertyValue { name: "Ahom", alias: "Ahom" },
                PotentialPropertyValue { name: "Arab", alias: "Arabic" },
                PotentialPropertyValue { name: "Armi", alias: "Imperial_Aramaic" },
                PotentialPropertyValue { name: "Armn", alias: "Armenian" },
                PotentialPropertyValue { name: "Avst", alias: "Avestan" },
                PotentialPropertyValue { name: "Bali", alias: "Balinese" },
                PotentialPropertyValue { name: "Bamu", alias: "Bamum" },
                PotentialPropertyValue { name: "Bass", alias: "Bassa_Vah" },
                PotentialPropertyValue { name: "Batk", alias: "Batak" },
                PotentialPropertyValue { name: "Beng", alias: "Bengali" },
                PotentialPropertyValue { name: "Berf", alias: "Beria_Erfe" },
                PotentialPropertyValue { name: "Bhks", alias: "Bhaiksuki" },
                PotentialPropertyValue { name: "Bopo", alias: "Bopomofo" },
                PotentialPropertyValue { name: "Brah", alias: "Brahmi" },
                PotentialPropertyValue { name: "Brai", alias: "Braille" },
                PotentialPropertyValue { name: "Bugi", alias: "Buginese" },
                PotentialPropertyValue { name: "Buhd", alias: "Buhid" },
                PotentialPropertyValue { name: "Cakm", alias: "Chakma" },
                PotentialPropertyValue { name: "Cans", alias: "Canadian_Aboriginal" },
                PotentialPropertyValue { name: "Cari", alias: "Carian" },
                PotentialPropertyValue { name: "Cham", alias: "Cham" },
                PotentialPropertyValue { name: "Cher", alias: "Cherokee" },
                PotentialPropertyValue { name: "Chrs", alias: "Chorasmian" },
                PotentialPropertyValue { name: "Copt", alias: "Coptic" },
                PotentialPropertyValue { name: "Cpmn", alias: "Cypro_Minoan" },
                PotentialPropertyValue { name: "Cprt", alias: "Cypriot" },
                PotentialPropertyValue { name: "Cyrl", alias: "Cyrillic" },
                PotentialPropertyValue { name: "Deva", alias: "Devanagari" },
                PotentialPropertyValue { name: "Diak", alias: "Dives_Akuru" },
                PotentialPropertyValue { name: "Dogr", alias: "Dogra" },
                PotentialPropertyValue { name: "Dsrt", alias: "Deseret" },
                PotentialPropertyValue { name: "Dupl", alias: "Duployan" },
                PotentialPropertyValue { name: "Egyp", alias: "Egyptian_Hieroglyphs" },
                PotentialPropertyValue { name: "Elba", alias: "Elbasan" },
                PotentialPropertyValue { name: "Elym", alias: "Elymaic" },
                PotentialPropertyValue { name: "Ethi", alias: "Ethiopic" },
                PotentialPropertyValue { name: "Gara", alias: "Garay" },
                PotentialPropertyValue { name: "Geor", alias: "Georgian" },
                PotentialPropertyValue { name: "Glag", alias: "Glagolitic" },
                PotentialPropertyValue { name: "Gong", alias: "Gunjala_Gondi" },
                PotentialPropertyValue { name: "Gonm", alias: "Masaram_Gondi" },
                PotentialPropertyValue { name: "Goth", alias: "Gothic" },
                PotentialPropertyValue { name: "Gran", alias: "Grantha" },
                PotentialPropertyValue { name: "Grek", alias: "Greek" },
                PotentialPropertyValue { name: "Gujr", alias: "Gujarati" },
                PotentialPropertyValue { name: "Gukh", alias: "Gurung_Khema" },
                PotentialPropertyValue { name: "Guru", alias: "Gurmukhi" },
                PotentialPropertyValue { name: "Hang", alias: "Hangul" },
                PotentialPropertyValue { name: "Hani", alias: "Han" },
                PotentialPropertyValue { name: "Hano", alias: "Hanunoo" },
                PotentialPropertyValue { name: "Hatr", alias: "Hatran" },
                PotentialPropertyValue { name: "Hebr", alias: "Hebrew" },
                PotentialPropertyValue { name: "Hira", alias: "Hiragana" },
                PotentialPropertyValue { name: "Hluw", alias: "Anatolian_Hieroglyphs" },
                PotentialPropertyValue { name: "Hmng", alias: "Pahawh_Hmong" },
                PotentialPropertyValue { name: "Hmnp", alias: "Nyiakeng_Puachue_Hmong" },
                PotentialPropertyValue { name: "Hrkt", alias: "Katakana_Or_Hiragana" },
                PotentialPropertyValue { name: "Hung", alias: "Old_Hungarian" },
                PotentialPropertyValue { name: "Ital", alias: "Old_Italic" },
                PotentialPropertyValue { name: "Java", alias: "Javanese" },
                PotentialPropertyValue { name: "Kali", alias: "Kayah_Li" },
                PotentialPropertyValue { name: "Kana", alias: "Katakana" },
                PotentialPropertyValue { name: "Kawi", alias: "Kawi" },
                PotentialPropertyValue { name: "Khar", alias: "Kharoshthi" },
                PotentialPropertyValue { name: "Khmr", alias: "Khmer" },
                PotentialPropertyValue { name: "Khoj", alias: "Khojki" },
                PotentialPropertyValue { name: "Kits", alias: "Khitan_Small_Script" },
                PotentialPropertyValue { name: "Knda", alias: "Kannada" },
                PotentialPropertyValue { name: "Krai", alias: "Kirat_Rai" },
                PotentialPropertyValue { name: "Kthi", alias: "Kaithi" },
                PotentialPropertyValue { name: "Lana", alias: "Tai_Tham" },
                PotentialPropertyValue { name: "Laoo", alias: "Lao" },
                PotentialPropertyValue { name: "Latn", alias: "Latin" },
                PotentialPropertyValue { name: "Lepc", alias: "Lepcha" },
                PotentialPropertyValue { name: "Limb", alias: "Limbu" },
                PotentialPropertyValue { name: "Lina", alias: "Linear_A" },
                PotentialPropertyValue { name: "Linb", alias: "Linear_B" },
                PotentialPropertyValue { name: "Lisu", alias: "Lisu" },
                PotentialPropertyValue { name: "Lyci", alias: "Lycian" },
                PotentialPropertyValue { name: "Lydi", alias: "Lydian" },
                PotentialPropertyValue { name: "Mahj", alias: "Mahajani" },
                PotentialPropertyValue { name: "Maka", alias: "Makasar" },
                PotentialPropertyValue { name: "Mand", alias: "Mandaic" },
                PotentialPropertyValue { name: "Mani", alias: "Manichaean" },
                PotentialPropertyValue { name: "Marc", alias: "Marchen" },
                PotentialPropertyValue { name: "Medf", alias: "Medefaidrin" },
                PotentialPropertyValue { name: "Mend", alias: "Mende_Kikakui" },
                PotentialPropertyValue { name: "Merc", alias: "Meroitic_Cursive" },
                PotentialPropertyValue { name: "Mero", alias: "Meroitic_Hieroglyphs" },
                PotentialPropertyValue { name: "Mlym", alias: "Malayalam" },
                PotentialPropertyValue { name: "Modi", alias: "Modi" },
                PotentialPropertyValue { name: "Mong", alias: "Mongolian" },
                PotentialPropertyValue { name: "Mroo", alias: "Mro" },
                PotentialPropertyValue { name: "Mtei", alias: "Meetei_Mayek" },
                PotentialPropertyValue { name: "Mult", alias: "Multani" },
                PotentialPropertyValue { name: "Mymr", alias: "Myanmar" },
                PotentialPropertyValue { name: "Nagm", alias: "Nag_Mundari" },
                PotentialPropertyValue { name: "Nand", alias: "Nandinagari" },
                PotentialPropertyValue { name: "Narb", alias: "Old_North_Arabian" },
                PotentialPropertyValue { name: "Nbat", alias: "Nabataean" },
                PotentialPropertyValue { name: "Newa", alias: "Newa" },
                PotentialPropertyValue { name: "Nkoo", alias: "Nko" },
                PotentialPropertyValue { name: "Nshu", alias: "Nushu" },
                PotentialPropertyValue { name: "Ogam", alias: "Ogham" },
                PotentialPropertyValue { name: "Olck", alias: "Ol_Chiki" },
                PotentialPropertyValue { name: "Onao", alias: "Ol_Onal" },
                PotentialPropertyValue { name: "Orkh", alias: "Old_Turkic" },
                PotentialPropertyValue { name: "Orya", alias: "Oriya" },
                PotentialPropertyValue { name: "Osge", alias: "Osage" },
                PotentialPropertyValue { name: "Osma", alias: "Osmanya" },
                PotentialPropertyValue { name: "Ougr", alias: "Old_Uyghur" },
                PotentialPropertyValue { name: "Palm", alias: "Palmyrene" },
                PotentialPropertyValue { name: "Pauc", alias: "Pau_Cin_Hau" },
                PotentialPropertyValue { name: "Perm", alias: "Old_Permic" },
                PotentialPropertyValue { name: "Phag", alias: "Phags_Pa" },
                PotentialPropertyValue { name: "Phli", alias: "Inscriptional_Pahlavi" },
                PotentialPropertyValue { name: "Phlp", alias: "Psalter_Pahlavi" },
                PotentialPropertyValue { name: "Phnx", alias: "Phoenician" },
                PotentialPropertyValue { name: "Plrd", alias: "Miao" },
                PotentialPropertyValue { name: "Prti", alias: "Inscriptional_Parthian" },
                PotentialPropertyValue { name: "Rjng", alias: "Rejang" },
                PotentialPropertyValue { name: "Rohg", alias: "Hanifi_Rohingya" },
                PotentialPropertyValue { name: "Runr", alias: "Runic" },
                PotentialPropertyValue { name: "Samr", alias: "Samaritan" },
                PotentialPropertyValue { name: "Sarb", alias: "Old_South_Arabian" },
                PotentialPropertyValue { name: "Saur", alias: "Saurashtra" },
                PotentialPropertyValue { name: "Sgnw", alias: "SignWriting" },
                PotentialPropertyValue { name: "Shaw", alias: "Shavian" },
                PotentialPropertyValue { name: "Shrd", alias: "Sharada" },
                PotentialPropertyValue { name: "Sidd", alias: "Siddham" },
                PotentialPropertyValue { name: "Sidt", alias: "Sidetic" },
                PotentialPropertyValue { name: "Sind", alias: "Khudawadi" },
                PotentialPropertyValue { name: "Sinh", alias: "Sinhala" },
                PotentialPropertyValue { name: "Sogd", alias: "Sogdian" },
                PotentialPropertyValue { name: "Sogo", alias: "Old_Sogdian" },
                PotentialPropertyValue { name: "Sora", alias: "Sora_Sompeng" },
                PotentialPropertyValue { name: "Soyo", alias: "Soyombo" },
                PotentialPropertyValue { name: "Sund", alias: "Sundanese" },
                PotentialPropertyValue { name: "Sunu", alias: "Sunuwar" },
                PotentialPropertyValue { name: "Sylo", alias: "Syloti_Nagri" },
                PotentialPropertyValue { name: "Syrc", alias: "Syriac" },
                PotentialPropertyValue { name: "Tagb", alias: "Tagbanwa" },
                PotentialPropertyValue { name: "Takr", alias: "Takri" },
                PotentialPropertyValue { name: "Tale", alias: "Tai_Le" },
                PotentialPropertyValue { name: "Talu", alias: "New_Tai_Lue" },
                PotentialPropertyValue { name: "Taml", alias: "Tamil" },
                PotentialPropertyValue { name: "Tang", alias: "Tangut" },
                PotentialPropertyValue { name: "Tavt", alias: "Tai_Viet" },
                PotentialPropertyValue { name: "Tayo", alias: "Tai_Yo" },
                PotentialPropertyValue { name: "Telu", alias: "Telugu" },
                PotentialPropertyValue { name: "Tfng", alias: "Tifinagh" },
                PotentialPropertyValue { name: "Tglg", alias: "Tagalog" },
                PotentialPropertyValue { name: "Thaa", alias: "Thaana" },
                PotentialPropertyValue { name: "Thai", alias: "Thai" },
                PotentialPropertyValue { name: "Tibt", alias: "Tibetan" },
                PotentialPropertyValue { name: "Tirh", alias: "Tirhuta" },
                PotentialPropertyValue { name: "Tnsa", alias: "Tangsa" },
                PotentialPropertyValue { name: "Todr", alias: "Todhri" },
                PotentialPropertyValue { name: "Tols", alias: "Tolong_Siki" },
                PotentialPropertyValue { name: "Toto", alias: "Toto" },
                PotentialPropertyValue { name: "Tutg", alias: "Tulu_Tigalari" },
                PotentialPropertyValue { name: "Ugar", alias: "Ugaritic" },
                PotentialPropertyValue { name: "Vaii", alias: "Vai" },
                PotentialPropertyValue { name: "Vith", alias: "Vithkuqi" },
                PotentialPropertyValue { name: "Wara", alias: "Warang_Citi" },
                PotentialPropertyValue { name: "Wcho", alias: "Wancho" },
                PotentialPropertyValue { name: "Xpeo", alias: "Old_Persian" },
                PotentialPropertyValue { name: "Xsux", alias: "Cuneiform" },
                PotentialPropertyValue { name: "Yezi", alias: "Yezidi" },
                PotentialPropertyValue { name: "Yiii", alias: "Yi" },
                PotentialPropertyValue { name: "Zanb", alias: "Zanabazar_Square" },
                PotentialPropertyValue { name: "Zinh", alias: "Inherited" },
                PotentialPropertyValue { name: "Zyyy", alias: "Common" },
                PotentialPropertyValue { name: "Zzzz", alias: "Unknown" },
            ],
        },
        PotentialPropertyCategory { property_name: "Script_Extensions", alias_name: "scx", potential_values: &[] },
    ];

    fn property_name_ok(name: &str) -> bool {
        Self::PROPERTY_POSSIBILITIES.iter().any(|ppc| ppc.property_name == name || ppc.alias_name == name)
    }

    fn property_value_ok(name: &str, value: &str) -> bool {
        let item = Self::PROPERTY_POSSIBILITIES
            .iter()
            .find(|ppc| ppc.property_name == name || ppc.alias_name == name)
            .expect("we already know the name is ok");
        item.potential_values.iter().any(|ppv| ppv.name == value || ppv.alias == value)
    }

    const TABLE_65: &[&str] = &[
        "ASCII",
        "ASCII_Hex_Digit",
        "AHex",
        "Alphabetic",
        "Alpha",
        "Any",
        "Assigned",
        "Bidi_Control",
        "Bidi_C",
        "Bidi_Mirrored",
        "Bidi_M",
        "Case_Ignorable",
        "CI",
        "Cased",
        "Changes_When_Casefolded",
        "CWCF",
        "Changes_When_Casemapped",
        "CWCM",
        "Changes_When_Lowercased",
        "CWL",
        "Changes_When_NFKC_Casefolded",
        "CWKCF",
        "Changes_When_Titlecased",
        "CWT",
        "Changes_When_Uppercased",
        "CWU",
        "Dash",
        "Default_Ignorable_Code_Point",
        "DI",
        "Deprecated",
        "Dep",
        "Diacritic",
        "Dia",
        "Emoji",
        "Emoji_Component",
        "EComp",
        "Emoji_Modifier",
        "EMod",
        "Emoji_Modifier_Base",
        "EBase",
        "Emoji_Presentation",
        "EPres",
        "Extended_Pictographic",
        "ExtPict",
        "Extender",
        "Ext",
        "Grapheme_Base",
        "Gr_Base",
        "Grapheme_Extend",
        "Gr_Ext",
        "Hex_Digit",
        "Hex",
        "IDS_Binary_Operator",
        "IDSB",
        "IDS_Trinary_Operator",
        "IDST",
        "ID_Continue",
        "IDC",
        "ID_Start",
        "IDS",
        "Ideographic",
        "Ideo",
        "Join_Control",
        "Join_C",
        "Logical_Order_Exception",
        "LOE",
        "Lowercase",
        "Lower",
        "Math",
        "Noncharacter_Code_Point",
        "NChar",
        "Pattern_Syntax",
        "Pat_Syn",
        "Pattern_White_Space",
        "Pat_WS",
        "Quotation_Mark",
        "QMark",
        "Radical",
        "Regional_Indicator",
        "RI",
        "Sentence_Terminal",
        "STerm",
        "Soft_Dotted",
        "SD",
        "Terminal_Punctuation",
        "Term",
        "Unified_Ideograph",
        "UIdeo",
        "Uppercase",
        "Upper",
        "Variation_Selector",
        "VS",
        "White_Space",
        "space",
        "XID_Continue",
        "XIDC",
        "XID_Start",
        "XIDS",
    ];

    const BINARY_UNARY_PROPERTIES: &[&str] = &[
        "Basic_Emoji",
        "Emoji_Keycap_Sequence",
        "RGI_Emoji_Modifier_Sequence",
        "RGI_Emoji_Flag_Sequence",
        "RGI_Emoji_Tag_Sequence",
        "RGI_Emoji_ZWJ_Sequence",
        "RGI_Emoji",
    ];

    fn lone_name_ok(name: &str, usm: UnicodeSetsMode) -> bool {
        let item = Self::PROPERTY_POSSIBILITIES
            .iter()
            .find(|ppc| ppc.property_name == "General_Category")
            .expect("this one is in the table");
        if item.potential_values.iter().any(|ppv| ppv.name == name || ppv.alias == name) {
            return true;
        }

        if Self::TABLE_65.contains(&name) {
            return true;
        }

        if Self::BINARY_UNARY_PROPERTIES.contains(&name) {
            return usm == UnicodeSetsMode::Allowed;
        }

        false
    }

    fn early_errors(&self, usm: UnicodeSetsMode) -> Vec<Object> {
        // UnicodePropertyValueExpression :: UnicodePropertyName = UnicodePropertyValue
        // * It is a Syntax Error if the source text matched by UnicodePropertyName is not a Unicode property name or
        //   property alias listed in the “Property name and aliases” column of Table 64.
        // * It is a Syntax Error if the source text matched by UnicodePropertyValue is not a property value or property
        //   value alias for the Unicode property or property alias given by the source text matched by
        //   UnicodePropertyName listed in PropertyValueAliases.txt.
        //
        // UnicodePropertyValueExpression :: LoneUnicodePropertyNameOrValue
        // * It is a Syntax Error if the source text matched by LoneUnicodePropertyNameOrValue is not a Unicode property
        //   value or property value alias for the General_Category (gc) property listed in PropertyValueAliases.txt,
        //   nor a binary property or binary property alias listed in the “Property name and aliases” column of Table
        //   65, nor a binary property of strings listed in the “Property name” column of Table 66.
        // * It is a Syntax Error if the enclosing Pattern does not have a [UnicodeSetsMode] parameter and the source
        //   text matched by LoneUnicodePropertyNameOrValue is a binary property of strings listed in the “Property
        //   name” column of Table 66.
        match self {
            UnicodePropertyValueExpression::NameValue { name, value } => {
                let mut errs = vec![];
                if !Self::property_name_ok(&name.0) {
                    errs.push(create_syntax_error_object("Invalid Unicode Property Name", None));
                } else if !Self::property_value_ok(&name.0, &value.0) {
                    errs.push(create_syntax_error_object("Invalid Unicode Property Value", None));
                }
                errs
            }
            UnicodePropertyValueExpression::Lone(node) => {
                if Self::lone_name_ok(&node.0, usm) {
                    vec![]
                } else {
                    vec![create_syntax_error_object("Invalid Unicode Property", None)]
                }
            }
        }
    }

    fn may_contain_strings(&self) -> bool {
        // Static Semantics: MayContainStrings
        // The syntax-directed operation MayContainStrings takes no arguments and returns a Boolean.
        match self {
            UnicodePropertyValueExpression::NameValue { name: _, value: _ } => {
                // UnicodePropertyValueExpression ::
                //      UnicodePropertyName = UnicodePropertyValue
                //
                // 1. Return false.
                false
            }
            UnicodePropertyValueExpression::Lone(name) => {
                // UnicodePropertyValueExpression :: LoneUnicodePropertyNameOrValue
                // 1. If the source text matched by LoneUnicodePropertyNameOrValue is a binary property of strings
                //    listed in the “Property name” column of Table 66, return true.
                // 2. Return false.
                Self::BINARY_UNARY_PROPERTIES.contains(&name.0.as_str())
            }
        }
    }
}

// UnicodePropertyName ::
//      UnicodePropertyNameCharacters
// UnicodePropertyNameCharacters ::
//      UnicodePropertyNameCharacter UnicodePropertyNameCharactersopt
#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct UnicodePropertyName(String);
impl fmt::Display for UnicodePropertyName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl UnicodePropertyName {
    fn parse(scanner: &Scanner) -> Option<(Self, usize)> {
        // one or more UnicodePropertyNameCharacters
        let mut new_scanner = scanner.clone();
        let mut name = String::new();
        while let Some(ch) = new_scanner.unicode_property_name_character() {
            name.push(char::try_from(ch).expect("should be a valid, thanks to the prior call"));
        }
        if name.is_empty() { None } else { Some((Self(name), new_scanner.read_idx - scanner.read_idx)) }
    }
}

// UnicodePropertyValue ::
//      UnicodePropertyValueCharacters
// UnicodePropertyValueCharacters ::
//      UnicodePropertyValueCharacter UnicodePropertyValueCharactersopt
#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct UnicodePropertyValue(String);
impl fmt::Display for UnicodePropertyValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl UnicodePropertyValue {
    fn parse(scanner: &Scanner) -> Option<(Self, usize)> {
        // one or more UnicodePropertyValueCharacters
        let mut new_scanner = scanner.clone();
        let mut name = String::new();
        while let Some(ch) = new_scanner.unicode_property_value_character() {
            name.push(char::try_from(ch).expect("value should be fine, it was just checked"));
        }
        if name.is_empty() { None } else { Some((Self(name), new_scanner.read_idx - scanner.read_idx)) }
    }
}

// LoneUnicodePropertyNameOrValue ::
//      UnicodePropertyValueCharacters
#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct LoneUnicodePropertyNameOrValue(String);
impl fmt::Display for LoneUnicodePropertyNameOrValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl LoneUnicodePropertyNameOrValue {
    fn parse(scanner: &Scanner) -> Option<(Self, usize)> {
        // This is the same as UnicodePropertyValue, so just use that.
        let (UnicodePropertyValue(name_or_value), chars_used) = UnicodePropertyValue::parse(scanner)?;
        Some((Self(name_or_value), chars_used))
    }
}

// GroupSpecifier[UnicodeMode] ::
//      ? GroupName[?UnicodeMode]
#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct GroupSpecifier(GroupName);
impl fmt::Display for GroupSpecifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "?{}", self.0)
    }
}
impl GroupSpecifier {
    fn parse(scanner: &Scanner, mode: UnicodeMode) -> Option<(Self, usize)> {
        let mut new_scanner = scanner.clone();
        new_scanner.consume('?')?;
        let (name, amt) = GroupName::parse(&new_scanner, mode)?;
        new_scanner.read_idx += amt;
        Some((Self(name), new_scanner.read_idx - scanner.read_idx))
    }

    pub(crate) fn capturing_group_name(&self) -> JSString {
        self.0.capturing_group_name()
    }

    fn early_errors(&self) -> Vec<Object> {
        self.0.early_errors()
    }
}

// GroupName[UnicodeMode] ::
//      < RegExpIdentifierName[?UnicodeMode] >
#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct GroupName(RegExpIdentifierName);

impl fmt::Display for GroupName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<{}>", self.0)
    }
}

impl GroupName {
    fn parse(scanner: &Scanner, mode: UnicodeMode) -> Option<(Self, usize)> {
        let mut new_scanner = scanner.clone();
        new_scanner.consume('<')?;
        let (name, amt_used) = RegExpIdentifierName::parse(&new_scanner, mode)?;
        new_scanner.read_idx += amt_used;
        new_scanner.consume('>')?;
        Some((Self(name), new_scanner.read_idx - scanner.read_idx))
    }

    pub(crate) fn capturing_group_name(&self) -> JSString {
        // Static Semantics: CapturingGroupName
        //
        // The syntax-directed operation CapturingGroupName takes no arguments and returns a String. It is defined
        // piecewise over the following productions:
        //
        // GroupName :: < RegExpIdentifierName >
        // 1. Let idTextUnescaped be the RegExpIdentifierCodePoints of RegExpIdentifierName.
        // 2. Return CodePointsToString(idTextUnescaped).
        let id_text_unescaped = self.0.reg_exp_identifier_code_points();
        JSString::from(id_text_unescaped)
    }

    fn early_errors(&self) -> Vec<Object> {
        self.0.early_errors()
    }
}

// RegExpIdentifierName[UnicodeMode] ::
//      RegExpIdentifierStart[?UnicodeMode]
//      RegExpIdentifierName[?UnicodeMode] RegExpIdentifierPart[?UnicodeMode]
#[derive(Debug, Clone, Eq, PartialEq)]
struct RegExpIdentifierName(String);

impl fmt::Display for RegExpIdentifierName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl RegExpIdentifierName {
    fn parse(scanner: &Scanner, mode: UnicodeMode) -> Option<(Self, usize)> {
        // A "start" followed by zero or more "parts"
        let mut new_scanner = scanner.clone();
        let mut name = String::new();
        let (RegExpIdentifierStart(start), amt_used) = RegExpIdentifierStart::parse(&new_scanner, mode)?;
        name.push(char::from_u32(start).expect("char class should be fine"));
        new_scanner.read_idx += amt_used;
        while let Some((RegExpIdentifierPart(part), amt_used)) = RegExpIdentifierPart::parse(&new_scanner, mode) {
            name.push(char::from_u32(part).expect("char class should be fine"));
            new_scanner.read_idx += amt_used;
        }
        Some((Self(name), new_scanner.read_idx - scanner.read_idx))
    }

    pub(crate) fn reg_exp_identifier_code_points(&self) -> String {
        // Static Semantics: RegExpIdentifierCodePoints
        //
        // The syntax-directed operation RegExpIdentifierCodePoints takes no arguments and returns a List of code
        // points. It is defined piecewise over the following productions:
        //
        // RegExpIdentifierName :: RegExpIdentifierStart
        // 1. Let cp be the RegExpIdentifierCodePoint of RegExpIdentifierStart.
        // 2. Return « cp ».
        // RegExpIdentifierName :: RegExpIdentifierName RegExpIdentifierPart
        // 1. Let cps be the RegExpIdentifierCodePoints of the derived RegExpIdentifierName.
        // 2. Let cp be the RegExpIdentifierCodePoint of RegExpIdentifierPart.
        // 3. Return the list-concatenation of cps and « cp ».
        self.0.clone()
    }

    fn early_errors(&self) -> Vec<Object> {
        // RegExpIdentifierStart :: \ RegExpUnicodeEscapeSequence
        // * It is a Syntax Error if the CharacterValue of RegExpUnicodeEscapeSequence is not the numeric value of some
        //   code point matched by the IdentifierStartChar lexical grammar production.
        //
        // RegExpIdentifierStart :: UnicodeLeadSurrogate UnicodeTrailSurrogate
        // * It is a Syntax Error if the RegExpIdentifierCodePoint of RegExpIdentifierStart is not matched by the
        //   UnicodeIDStart lexical grammar production.
        //
        // RegExpIdentifierPart :: \ RegExpUnicodeEscapeSequence
        // * It is a Syntax Error if the CharacterValue of RegExpUnicodeEscapeSequence is not the numeric value of some
        //   code point matched by the IdentifierPartChar lexical grammar production.
        //
        // RegExpIdentifierPart :: UnicodeLeadSurrogate UnicodeTrailSurrogate
        // * It is a Syntax Error if the RegExpIdentifierCodePoint of RegExpIdentifierPart is not matched by the
        //   UnicodeIDContinue lexical grammar production.
        let mut errs = vec![];
        for (idx, ch) in self.0.chars().enumerate() {
            match (idx, ch) {
                (0, ch) if !Scanner::is_identifier_start_char(u32::from(ch)) => {
                    errs.push(create_syntax_error_object("Invalid character at regex id start", None));
                }
                (_, ch) if !Scanner::is_identifier_part_char(u32::from(ch)) => {
                    errs.push(create_syntax_error_object("Invalid character in regex id", None));
                }
                _ => {}
            }
        }
        errs
    }
}

// RegExpIdentifierStart[UnicodeMode] ::
//      IdentifierStartChar
//      \ RegExpUnicodeEscapeSequence[+UnicodeMode]
//      [~UnicodeMode] UnicodeLeadSurrogate UnicodeTrailSurrogate
struct RegExpIdentifierStart(u32);
impl RegExpIdentifierStart {
    fn parse(scanner: &Scanner, mode: UnicodeMode) -> Option<(Self, usize)> {
        let mut new_scanner = scanner.clone();
        let ch = new_scanner.consume_any()?;
        if Scanner::is_identifier_start_char(ch) {
            Some((Self(ch), new_scanner.read_idx - scanner.read_idx))
        } else if ch == u32::from('\\') {
            let (RegExpUnicodeEscapeSequence(ch), amt_read) =
                RegExpUnicodeEscapeSequence::parse(&new_scanner, UnicodeMode::Allowed)?;
            new_scanner.read_idx += amt_read;
            Some((Self(ch), new_scanner.read_idx - scanner.read_idx))
        } else if mode == UnicodeMode::Denied && (0x1_0000..=0x10_ffff).contains(&ch) {
            Some((Self(ch), new_scanner.read_idx - scanner.read_idx))
        } else {
            None
        }
    }
}

// RegExpIdentifierPart[UnicodeMode] ::
//      IdentifierPartChar
//      \ RegExpUnicodeEscapeSequence[+UnicodeMode]
//      [~UnicodeMode] UnicodeLeadSurrogate UnicodeTrailSurrogate
struct RegExpIdentifierPart(u32);
impl RegExpIdentifierPart {
    fn parse(scanner: &Scanner, mode: UnicodeMode) -> Option<(Self, usize)> {
        let mut new_scanner = scanner.clone();
        let ch = new_scanner.consume_any()?;
        if Scanner::is_identifier_part_char(ch) {
            Some((Self(ch), new_scanner.read_idx - scanner.read_idx))
        } else if ch == u32::from('\\') {
            let (RegExpUnicodeEscapeSequence(ch), amt_read) =
                RegExpUnicodeEscapeSequence::parse(&new_scanner, UnicodeMode::Allowed)?;
            new_scanner.read_idx += amt_read;
            Some((Self(ch), new_scanner.read_idx - scanner.read_idx))
        } else if mode == UnicodeMode::Denied && (0x1_0000..=0x10_ffff).contains(&ch) {
            Some((Self(ch), new_scanner.read_idx - scanner.read_idx))
        } else {
            None
        }
    }
}

// UnicodeLeadSurrogate ::
//      any Unicode code point in the inclusive interval from U+D800 to U+DBFF

// UnicodeTrailSurrogate ::
//      any Unicode code point in the inclusive interval from U+DC00 to U+DFFF

// Pattern[UnicodeMode, UnicodeSetsMode, NamedCaptureGroups] ::
//      Disjunction[?UnicodeMode, ?UnicodeSetsMode, ?NamedCaptureGroups]
#[derive(Debug, Clone)]
pub(crate) struct Pattern(pub(crate) Disjunction);

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl Pattern {
    fn parse(
        scanner: &Scanner,
        unicode: UnicodeMode,
        sets: UnicodeSetsMode,
        cgroups: NamedCaptureGroups,
    ) -> Option<Self> {
        let (disj, amt) = Disjunction::parse(scanner, unicode, sets, cgroups)?;
        if amt == scanner.all.len() { Some(Self(disj)) } else { None }
    }

    pub(crate) fn count_left_capturing_parens_within(&self) -> usize {
        self.0.count_left_capturing_parens_within()
    }

    pub(crate) fn early_errors(&self, usm: UnicodeSetsMode) -> Vec<Object> {
        let mut errs = vec![];
        let left_paren_count = self.count_left_capturing_parens_within();
        if left_paren_count >= 4_294_967_295 {
            errs.push(create_syntax_error_object("Too many captures", None));
        }
        let group_specifiers = self.all_group_specifiers();
        if group_specifiers.len() >= 2 {
            let combo_iter = Combination::new(&group_specifiers, 2);
            for pair in combo_iter {
                let (x, y) = {
                    match pair.as_slice() {
                        &[x, y] => (x, y),
                        _ => unreachable!(),
                    }
                };
                if x.capturing_group_name() == y.capturing_group_name() && self.might_both_participate(x, y) {
                    errs.push(create_syntax_error_object(
                        format!("Group name '{}' used multiple times", x.capturing_group_name()),
                        None,
                    ));
                }
            }
        }
        let mut sub_production_errors = self.0.early_errors(left_paren_count, &group_specifiers, usm);
        errs.append(&mut sub_production_errors);
        errs
    }

    pub(crate) fn all_group_specifiers(&self) -> Vec<&GroupSpecifier> {
        self.0.all_group_specifiers()
    }

    pub fn might_both_participate(&self, x: &GroupSpecifier, y: &GroupSpecifier) -> bool {
        // Static Semantics: MightBothParticipate ( x, y )
        //
        // The abstract operation MightBothParticipate takes arguments x (a Parse Node) and y (a Parse Node) and returns
        // a Boolean. It performs the following steps when called:
        //
        // 1. Assert: x and y have the same enclosing Pattern.
        // 2. If the enclosing Pattern contains a Disjunction :: Alternative | Disjunction Parse Node such that either x
        //    is contained within the Alternative and y is contained within the derived Disjunction, or x is contained
        //    within the derived Disjunction and y is contained within the Alternative, return false.
        // 3. Return true.
        self.0.might_both_participate(x, y)
    }

    pub(crate) fn group_name_associations(&self) -> Vec<Option<&GroupName>> {
        self.0.group_name_associations()
    }
}

pub(crate) fn parse_pattern(pattern: &[u32], u: UnicodeMode, v: UnicodeSetsMode) -> Result<Pattern, Vec<Object>> {
    let scanner = Scanner::new(pattern);

    if u == UnicodeMode::Allowed && v == UnicodeSetsMode::Allowed {
        return Err(vec![create_syntax_error_object("invalid regexp flags", None)]);
    }
    let pattern = Pattern::parse(&scanner, u, v, NamedCaptureGroups::Allowed)
        .ok_or_else(|| vec![create_syntax_error_object("invalid regexp", None)])?;

    let errs = pattern.early_errors(v);

    if errs.is_empty() { Ok(pattern) } else { Err(errs) }
}

// Disjunction[UnicodeMode, UnicodeSetsMode, NamedCaptureGroups] ::
//      Alternative[?UnicodeMode, ?UnicodeSetsMode, ?NamedCaptureGroups]
//      Alternative[?UnicodeMode, ?UnicodeSetsMode, ?NamedCaptureGroups] | Disjunction[?UnicodeMode, ?UnicodeSetsMode, ?NamedCaptureGroups]
#[derive(Debug, Clone)]
pub(crate) struct Disjunction(pub(crate) Vec<Alternative>);
impl fmt::Display for Disjunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", join_display(&self.0, "|"))
    }
}

impl Disjunction {
    fn parse(
        scanner: &Scanner,
        unicode: UnicodeMode,
        sets: UnicodeSetsMode,
        cgroups: NamedCaptureGroups,
    ) -> Option<(Self, usize)> {
        fn followup(
            scanner: &Scanner,
            unicode: UnicodeMode,
            sets: UnicodeSetsMode,
            cgroups: NamedCaptureGroups,
        ) -> Option<(Alternative, usize)> {
            let mut new_scanner = scanner.clone();
            new_scanner.consume('|')?;
            let (alt, amt) = Alternative::parse(&new_scanner, unicode, sets, cgroups);
            new_scanner.read_idx += amt;
            Some((alt, new_scanner.read_idx - scanner.read_idx))
        }
        let mut new_scanner = scanner.clone();
        let (first, amt) = Alternative::parse(&new_scanner, unicode, sets, cgroups);
        new_scanner.read_idx += amt;
        let mut result = vec![first];
        loop {
            if let Some((item, amt)) = followup(&new_scanner, unicode, sets, cgroups) {
                new_scanner.read_idx += amt;
                result.push(item);
            } else {
                break Some((Self(result), new_scanner.read_idx - scanner.read_idx));
            }
        }
    }

    pub(crate) fn count_left_capturing_parens_within(&self) -> usize {
        self.0.iter().map(Alternative::count_left_capturing_parens_within).sum()
    }

    pub(crate) fn might_both_participate(&self, x: &GroupSpecifier, y: &GroupSpecifier) -> bool {
        // Static Semantics: MightBothParticipate ( x, y )
        //
        // The abstract operation MightBothParticipate takes arguments x (a Parse Node) and y (a Parse Node) and returns
        // a Boolean. It performs the following steps when called:
        //
        // 1. Assert: x and y have the same enclosing Pattern.
        // 2. If the enclosing Pattern contains a Disjunction :: Alternative | Disjunction Parse Node such that either x
        //    is contained within the Alternative and y is contained within the derived Disjunction, or x is contained
        //    within the derived Disjunction and y is contained within the Alternative, return false.
        // 3. Return true.

        // Note: due to the design of the AST, a given input node will reside in either one or none of the alternatives.
        let alt_ids = self
            .0
            .iter()
            .enumerate()
            .filter_map(|(id, alt)| if alt.contains(x) || alt.contains(y) { Some(id) } else { None })
            .collect::<Vec<_>>();

        assert_eq!(alt_ids.len(), 2);
        if alt_ids[0] == alt_ids[1] { self.0[alt_ids[0]].might_both_participate(x, y) } else { false }
    }

    pub(crate) fn early_errors(
        &self,
        left_paren_count: usize,
        group_specifiers: &[&GroupSpecifier],
        usm: UnicodeSetsMode,
    ) -> Vec<Object> {
        self.0.iter().flat_map(|alt| alt.early_errors(left_paren_count, group_specifiers, usm)).collect()
    }

    pub(crate) fn contains(&self, gs: &GroupSpecifier) -> bool {
        self.0.iter().any(|alt| alt.contains(gs))
    }

    pub(crate) fn all_group_specifiers(&self) -> Vec<&GroupSpecifier> {
        self.0.iter().flat_map(|alt| alt.all_group_specifiers()).collect()
    }

    pub(crate) fn group_name_associations(&self) -> Vec<Option<&GroupName>> {
        self.0.iter().flat_map(Alternative::group_name_associations).collect()
    }
}

fn run_m2a(m1: &Matcher, m2: &Matcher, state: MatchState, continuation: MatcherContinuation) -> Option<MatchState> {
    let r = m1.as_ref()(state.clone(), continuation.clone());
    match r {
        Some(state) => Some(state),
        None => m2.as_ref()(state, continuation),
    }
}

fn match_two_alternatives(m1: Matcher, m2: Matcher) -> Matcher {
    // MatchTwoAlternatives ( m1, m2 )
    //
    // The abstract operation MatchTwoAlternatives takes arguments m1 (a Matcher) and m2 (a Matcher) and returns a
    // Matcher. It performs the following steps when called:
    //
    // 1. Return a new Matcher with parameters (x, c) that captures m1 and m2 and performs the following steps when called:
    //    a. Assert: x is a MatchState.
    //    b. Assert: c is a MatcherContinuation.
    //    c. Let r be m1(x, c).
    //    d. If r is not failure, return r.
    //    e. Return m2(x, c).
    Rc::new(move |state, continuation| run_m2a(&m1, &m2, state, continuation))
}

// Alternative[UnicodeMode, UnicodeSetsMode, NamedCaptureGroups] ::
//      [empty]
//      Alternative[?UnicodeMode, ?UnicodeSetsMode, ?NamedCaptureGroups] Term[?UnicodeMode, ?UnicodeSetsMode, ?NamedCaptureGroups]
#[derive(Debug, Clone)]
pub(crate) struct Alternative(pub(crate) Vec<Term>);
impl fmt::Display for Alternative {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", join_display(&self.0, ""))
    }
}
impl Alternative {
    fn parse(
        scanner: &Scanner,
        unicode: UnicodeMode,
        sets: UnicodeSetsMode,
        cgroups: NamedCaptureGroups,
    ) -> (Self, usize) {
        let mut new_scanner = scanner.clone();
        let mut results = vec![];
        while let Some((term, amt)) = Term::parse(&new_scanner, unicode, sets, cgroups) {
            new_scanner.read_idx += amt;
            results.push(term);
        }
        (Self(results), new_scanner.read_idx - scanner.read_idx)
    }

    pub(crate) fn early_errors(
        &self,
        left_paren_count: usize,
        group_specifiers: &[&GroupSpecifier],
        usm: UnicodeSetsMode,
    ) -> Vec<Object> {
        self.0.iter().flat_map(|term| term.early_errors(left_paren_count, group_specifiers, usm)).collect()
    }

    pub(crate) fn count_left_capturing_parens_within(&self) -> usize {
        self.0.iter().map(Term::count_left_capturing_parens_within).sum()
    }

    pub(crate) fn contains(&self, gs: &GroupSpecifier) -> bool {
        self.0.iter().any(|term| term.contains(gs))
    }

    pub(crate) fn might_both_participate(&self, x: &GroupSpecifier, y: &GroupSpecifier) -> bool {
        self.0.iter().any(|term| term.might_both_participate(x, y))
    }

    pub(crate) fn all_group_specifiers(&self) -> Vec<&GroupSpecifier> {
        self.0.iter().flat_map(|term| term.all_group_specifiers()).collect()
    }

    pub(crate) fn group_name_associations(&self) -> Vec<Option<&GroupName>> {
        self.0.iter().flat_map(Term::group_name_associations).collect()
    }
}

// Term[UnicodeMode, UnicodeSetsMode, NamedCaptureGroups] ::
//      Assertion[?UnicodeMode, ?UnicodeSetsMode, ?NamedCaptureGroups]
//      Atom[?UnicodeMode, ?UnicodeSetsMode, ?NamedCaptureGroups]
//      Atom[?UnicodeMode, ?UnicodeSetsMode, ?NamedCaptureGroups] Quantifier
#[derive(Debug, Clone)]
pub(crate) struct Term {
    pub(crate) node: TermNode,
    pub(crate) left_capturing_parens_before: usize,
}
impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.node {
            TermNode::Assertion(assertion) => assertion.fmt(f),
            TermNode::Atom(atom, Some(quantifier)) => write!(f, "{atom}{quantifier}"),
            TermNode::Atom(atom, None) => atom.fmt(f),
        }
    }
}
#[derive(Debug, Clone)]
pub(crate) enum TermNode {
    Assertion(Assertion),
    Atom(Atom, Option<Quantifier>),
}
impl Term {
    fn parse(
        scanner: &Scanner,
        unicode: UnicodeMode,
        sets: UnicodeSetsMode,
        cgroups: NamedCaptureGroups,
    ) -> Option<(Self, usize)> {
        let mut new_scanner = scanner.clone();
        let left_capturing_parens_before = new_scanner.left_capturing_parens;
        if let Some((assertion, amt)) = Assertion::parse(&new_scanner, unicode, sets, cgroups) {
            Some((Self { node: TermNode::Assertion(assertion), left_capturing_parens_before }, amt))
        } else if let Some((atom, amt)) = Atom::parse(&new_scanner, unicode, sets, cgroups) {
            new_scanner.read_idx += amt;
            if let Some((q, amt)) = Quantifier::parse(&new_scanner) {
                new_scanner.read_idx += amt;
                Some((
                    Self { node: TermNode::Atom(atom, Some(q)), left_capturing_parens_before },
                    new_scanner.read_idx - scanner.read_idx,
                ))
            } else {
                Some((
                    Self { node: TermNode::Atom(atom, None), left_capturing_parens_before },
                    new_scanner.read_idx - scanner.read_idx,
                ))
            }
        } else {
            None
        }
    }

    pub(crate) fn early_errors(
        &self,
        left_paren_count: usize,
        group_specifiers: &[&GroupSpecifier],
        usm: UnicodeSetsMode,
    ) -> Vec<Object> {
        match &self.node {
            TermNode::Assertion(assertion) => assertion.early_errors(left_paren_count, group_specifiers, usm),
            TermNode::Atom(atom, None) => atom.early_errors(left_paren_count, group_specifiers, usm),
            TermNode::Atom(atom, Some(quantifier)) => {
                let mut errs = atom.early_errors(left_paren_count, group_specifiers, usm);
                errs.append(&mut quantifier.early_errors());
                errs
            }
        }
    }

    pub(crate) fn count_left_capturing_parens_within(&self) -> usize {
        match &self.node {
            TermNode::Assertion(assertion) => assertion.count_left_capturing_parens_within(),
            TermNode::Atom(atom, _) => atom.count_left_capturing_parens_within(),
        }
    }
    pub(crate) fn might_both_participate(&self, x: &GroupSpecifier, y: &GroupSpecifier) -> bool {
        match &self.node {
            TermNode::Assertion(assertion) => assertion.might_both_participate(x, y),
            TermNode::Atom(atom, _) => atom.might_both_participate(x, y),
        }
    }

    pub(crate) fn contains(&self, gs: &GroupSpecifier) -> bool {
        match &self.node {
            TermNode::Assertion(assertion) => assertion.contains(gs),
            TermNode::Atom(atom, _) => atom.contains(gs),
        }
    }

    pub(crate) fn all_group_specifiers(&self) -> Vec<&GroupSpecifier> {
        match &self.node {
            TermNode::Assertion(assertion) => assertion.all_group_specifiers(),
            TermNode::Atom(atom, _) => atom.all_group_specifiers(),
        }
    }

    pub(crate) fn group_name_associations(&self) -> Vec<Option<&GroupName>> {
        match &self.node {
            TermNode::Assertion(assertion) => assertion.group_name_associations(),
            TermNode::Atom(atom, _) => atom.group_name_associations(),
        }
    }
}

// Assertion[UnicodeMode, UnicodeSetsMode, NamedCaptureGroups] ::
//      ^
//      $
//      \b
//      \B
//      (?= Disjunction[?UnicodeMode, ?UnicodeSetsMode, ?NamedCaptureGroups] )
//      (?! Disjunction[?UnicodeMode, ?UnicodeSetsMode, ?NamedCaptureGroups] )
//      (?<= Disjunction[?UnicodeMode, ?UnicodeSetsMode, ?NamedCaptureGroups] )
//      (?<! Disjunction[?UnicodeMode, ?UnicodeSetsMode, ?NamedCaptureGroups] )
#[derive(Debug, Clone)]
pub(crate) enum Assertion {
    Start,
    End,
    WordBoundary,
    NotWordBoundary,
    LookAhead(Box<Disjunction>),
    NegLookAhead(Box<Disjunction>),
    LookBehind(Box<Disjunction>),
    NegLookBehind(Box<Disjunction>),
}
impl fmt::Display for Assertion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Assertion::Start => write!(f, "^"),
            Assertion::End => write!(f, "$"),
            Assertion::WordBoundary => write!(f, "\\b"),
            Assertion::NotWordBoundary => write!(f, "\\B"),
            Assertion::LookAhead(disjunction) => write!(f, "(?={disjunction})"),
            Assertion::NegLookAhead(disjunction) => write!(f, "(?!{disjunction})"),
            Assertion::LookBehind(disjunction) => write!(f, "(?<={disjunction})"),
            Assertion::NegLookBehind(disjunction) => write!(f, "(?<!{disjunction})"),
        }
    }
}
impl Assertion {
    fn parse(
        scanner: &Scanner,
        unicode: UnicodeMode,
        sets: UnicodeSetsMode,
        cgroups: NamedCaptureGroups,
    ) -> Option<(Self, usize)> {
        fn word_boundary(scanner: &Scanner) -> Option<(Assertion, usize)> {
            let mut new_scanner = scanner.clone();
            new_scanner.consume('\\')?;
            let ch = new_scanner.consume_any()?;
            let assertion = if ch == u32::from('b') {
                Some(Assertion::WordBoundary)
            } else if ch == u32::from('B') {
                Some(Assertion::NotWordBoundary)
            } else {
                None
            };
            assertion.map(|assertion| (assertion, new_scanner.read_idx - scanner.read_idx))
        }
        fn lookaround(
            scanner: &Scanner,
            unicode: UnicodeMode,
            sets: UnicodeSetsMode,
            cgroups: NamedCaptureGroups,
        ) -> Option<(Assertion, usize)> {
            let mut new_scanner = scanner.clone();
            new_scanner.consume('(')?;
            new_scanner.consume('?')?;
            let is_lookbehind = new_scanner.consume('<').is_some();
            let negate = {
                let ch = new_scanner.consume_any()?;
                if ch == u32::from('!') {
                    true
                } else if ch == u32::from('=') {
                    false
                } else {
                    return None;
                }
            };
            let (disj, amt) = Disjunction::parse(&new_scanner, unicode, sets, cgroups)?;
            new_scanner.read_idx += amt;
            new_scanner.consume(')')?;
            Some((
                match (is_lookbehind, negate) {
                    (true, true) => Assertion::NegLookBehind,
                    (true, false) => Assertion::LookBehind,
                    (false, true) => Assertion::NegLookAhead,
                    (false, false) => Assertion::LookAhead,
                }(Box::new(disj)),
                new_scanner.read_idx - scanner.read_idx,
            ))
        }

        let mut new_scanner = scanner.clone();
        if let Some(()) = new_scanner.consume('^') {
            Some((Self::Start, new_scanner.read_idx - scanner.read_idx))
        } else if let Some(()) = new_scanner.consume('$') {
            Some((Self::End, new_scanner.read_idx - scanner.read_idx))
        } else if let Some((assertion, amt)) = word_boundary(&new_scanner) {
            new_scanner.read_idx += amt;
            Some((assertion, new_scanner.read_idx - scanner.read_idx))
        } else if let Some((assertion, amt)) = lookaround(&new_scanner, unicode, sets, cgroups) {
            new_scanner.read_idx += amt;
            Some((assertion, new_scanner.read_idx - scanner.read_idx))
        } else {
            None
        }
    }

    pub(crate) fn early_errors(
        &self,
        left_paren_count: usize,
        group_specifiers: &[&GroupSpecifier],
        usm: UnicodeSetsMode,
    ) -> Vec<Object> {
        match self {
            Assertion::Start | Assertion::End | Assertion::WordBoundary | Assertion::NotWordBoundary => vec![],
            Assertion::LookAhead(d)
            | Assertion::NegLookAhead(d)
            | Assertion::LookBehind(d)
            | Assertion::NegLookBehind(d) => d.early_errors(left_paren_count, group_specifiers, usm),
        }
    }

    pub(crate) fn count_left_capturing_parens_within(&self) -> usize {
        match self {
            Assertion::Start | Assertion::End | Assertion::WordBoundary | Assertion::NotWordBoundary => 0,
            Assertion::LookAhead(d)
            | Assertion::NegLookAhead(d)
            | Assertion::LookBehind(d)
            | Assertion::NegLookBehind(d) => d.count_left_capturing_parens_within(),
        }
    }

    pub(crate) fn might_both_participate(&self, x: &GroupSpecifier, y: &GroupSpecifier) -> bool {
        match self {
            Assertion::Start | Assertion::End | Assertion::WordBoundary | Assertion::NotWordBoundary => false,
            Assertion::LookAhead(disjunction)
            | Assertion::NegLookAhead(disjunction)
            | Assertion::LookBehind(disjunction)
            | Assertion::NegLookBehind(disjunction) => disjunction.might_both_participate(x, y),
        }
    }

    pub(crate) fn contains(&self, gs: &GroupSpecifier) -> bool {
        match self {
            Assertion::Start | Assertion::End | Assertion::WordBoundary | Assertion::NotWordBoundary => false,
            Assertion::LookAhead(disjunction)
            | Assertion::NegLookAhead(disjunction)
            | Assertion::LookBehind(disjunction)
            | Assertion::NegLookBehind(disjunction) => disjunction.contains(gs),
        }
    }

    pub(crate) fn all_group_specifiers(&self) -> Vec<&GroupSpecifier> {
        match self {
            Assertion::Start | Assertion::End | Assertion::WordBoundary | Assertion::NotWordBoundary => vec![],
            Assertion::LookAhead(disjunction)
            | Assertion::NegLookAhead(disjunction)
            | Assertion::LookBehind(disjunction)
            | Assertion::NegLookBehind(disjunction) => disjunction.all_group_specifiers(),
        }
    }

    pub(crate) fn group_name_associations(&self) -> Vec<Option<&GroupName>> {
        match self {
            Assertion::Start | Assertion::End | Assertion::WordBoundary | Assertion::NotWordBoundary => vec![],
            Assertion::LookAhead(disjunction)
            | Assertion::NegLookAhead(disjunction)
            | Assertion::LookBehind(disjunction)
            | Assertion::NegLookBehind(disjunction) => disjunction.group_name_associations(),
        }
    }
}

// Quantifier ::
//      QuantifierPrefix
//      QuantifierPrefix ?
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) enum Quantifier {
    Greedy(QuantifierPrefix),
    Restrained(QuantifierPrefix),
}
impl fmt::Display for Quantifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Quantifier::Greedy(quantifier_prefix) => quantifier_prefix.fmt(f),
            Quantifier::Restrained(quantifier_prefix) => write!(f, "{quantifier_prefix}?"),
        }
    }
}
impl Quantifier {
    fn parse(scanner: &Scanner) -> Option<(Self, usize)> {
        let mut new_scanner = scanner.clone();
        let (q, amt) = QuantifierPrefix::parse(&new_scanner)?;
        new_scanner.read_idx += amt;
        if let Some(()) = new_scanner.consume('?') {
            Some((Self::Restrained(q), new_scanner.read_idx - scanner.read_idx))
        } else {
            Some((Self::Greedy(q), new_scanner.read_idx - scanner.read_idx))
        }
    }

    pub(crate) fn early_errors(&self) -> Vec<Object> {
        match self {
            Quantifier::Greedy(quantifier_prefix) | Quantifier::Restrained(quantifier_prefix) => {
                quantifier_prefix.early_errors()
            }
        }
    }
}

// QuantifierPrefix ::
//      *
//      +
//      ?
//      { DecimalDigits[~Sep] }
//      { DecimalDigits[~Sep] ,}
//      { DecimalDigits[~Sep] , DecimalDigits[~Sep] }
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) enum QuantifierPrefix {
    ZeroOrMore,
    OneOrMore,
    ZeroOrOne,
    Exactly(u32),
    XOrMore(u32),
    Range(u32, u32),
}
impl fmt::Display for QuantifierPrefix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            QuantifierPrefix::ZeroOrMore => write!(f, "*"),
            QuantifierPrefix::OneOrMore => write!(f, "+"),
            QuantifierPrefix::ZeroOrOne => write!(f, "?"),
            QuantifierPrefix::Exactly(n) => write!(f, "{{{}}}", *n),
            QuantifierPrefix::XOrMore(n) => write!(f, "{{{},}}", *n),
            QuantifierPrefix::Range(low, high) => write!(f, "{{{low},{high}}}"),
        }
    }
}
impl QuantifierPrefix {
    fn parse(scanner: &Scanner) -> Option<(Self, usize)> {
        let mut new_scanner = scanner.clone();
        let ch = new_scanner.consume_any()?;
        if ch == u32::from('*') {
            Some((Self::ZeroOrMore, new_scanner.read_idx - scanner.read_idx))
        } else if ch == u32::from('+') {
            Some((Self::OneOrMore, new_scanner.read_idx - scanner.read_idx))
        } else if ch == u32::from('?') {
            Some((Self::ZeroOrOne, new_scanner.read_idx - scanner.read_idx))
        } else if ch == u32::from('{') {
            let mut val = u32::from(new_scanner.digit(10)?);
            while let Some(v) = new_scanner.digit(10) {
                val = val.checked_mul(10)?.checked_add(u32::from(v))?;
            }
            let ch = new_scanner.consume_any()?;
            if ch == u32::from('}') {
                Some((Self::Exactly(val), new_scanner.read_idx - scanner.read_idx))
            } else if ch == u32::from(',') {
                if let Some(()) = new_scanner.consume('}') {
                    Some((Self::XOrMore(val), new_scanner.read_idx - scanner.read_idx))
                } else {
                    let mut lim = u32::from(new_scanner.digit(10)?);
                    while let Some(v) = new_scanner.digit(10) {
                        lim = lim.checked_mul(10)?.checked_add(u32::from(v))?;
                    }
                    new_scanner.consume('}')?;
                    Some((Self::Range(val, lim), new_scanner.read_idx - scanner.read_idx))
                }
            } else {
                None
            }
        } else {
            None
        }
    }

    pub(crate) fn early_errors(&self) -> Vec<Object> {
        match self {
            QuantifierPrefix::Range(low, high) if low > high => {
                vec![create_syntax_error_object(
                    format!("Quantifier range: {{{low},{high}}}: {{low}} is larger than {{high}}"),
                    None,
                )]
            }
            QuantifierPrefix::Range(_, _)
            | QuantifierPrefix::ZeroOrMore
            | QuantifierPrefix::OneOrMore
            | QuantifierPrefix::ZeroOrOne
            | QuantifierPrefix::Exactly(_)
            | QuantifierPrefix::XOrMore(_) => vec![],
        }
    }
}

// Atom[UnicodeMode, UnicodeSetsMode, NamedCaptureGroups] ::
//      PatternCharacter
//      .
//      \ AtomEscape[?UnicodeMode, ?NamedCaptureGroups]
//      CharacterClass[?UnicodeMode, ?UnicodeSetsMode]
//      ( GroupSpecifier[?UnicodeMode]opt Disjunction[?UnicodeMode, ?UnicodeSetsMode, ?NamedCaptureGroups] )
//      (?: Disjunction[?UnicodeMode, ?UnicodeSetsMode, ?NamedCaptureGroups] )
#[derive(Debug, Clone)]
pub(crate) struct Atom {
    pub(crate) node: AtomNode,
    pub(crate) left_capturing_parens_before: usize,
}
#[derive(Debug, Clone)]
pub(crate) enum AtomNode {
    PatternCharacter(u32),
    Dot,
    AtomEscape(AtomEscape),
    CharacterClass(CharacterClass),
    GroupedDisjunction { group_specifier: Option<GroupSpecifier>, disjunction: Box<Disjunction> },
    UnGroupedDisjunction(Box<Disjunction>),
}
impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.node {
            AtomNode::PatternCharacter(c) => {
                if let Ok(ch) = char::try_from(*c)
                    && !ch.is_control()
                {
                    write!(f, "{ch}")
                } else {
                    write!(f, "\\u{c:X}")
                }
            }
            AtomNode::Dot => write!(f, "."),
            AtomNode::AtomEscape(atom_escape) => write!(f, "\\{atom_escape}"),
            AtomNode::CharacterClass(character_class) => character_class.fmt(f),
            AtomNode::GroupedDisjunction { group_specifier: Some(group_specifier), disjunction } => {
                write!(f, "({group_specifier}{disjunction})")
            }
            AtomNode::GroupedDisjunction { group_specifier: None, disjunction } => write!(f, "({disjunction})"),
            AtomNode::UnGroupedDisjunction(disjunction) => write!(f, "(?:{disjunction})"),
        }
    }
}
impl Atom {
    fn parse(
        scanner: &Scanner,
        unicode: UnicodeMode,
        sets: UnicodeSetsMode,
        cgroups: NamedCaptureGroups,
    ) -> Option<(Self, usize)> {
        fn slash_escape(
            scanner: &Scanner,
            unicode: UnicodeMode,
            cgroups: NamedCaptureGroups,
        ) -> Option<(AtomEscape, usize)> {
            let mut new_scanner = scanner.clone();
            new_scanner.consume('\\')?;
            let (escape, amt) = AtomEscape::parse(&new_scanner, unicode, cgroups)?;
            new_scanner.read_idx += amt;
            Some((escape, new_scanner.read_idx - scanner.read_idx))
        }
        fn grouped_disjunction(
            scanner: &Scanner,
            unicode: UnicodeMode,
            sets: UnicodeSetsMode,
            cgroups: NamedCaptureGroups,
        ) -> Option<(Option<GroupSpecifier>, Disjunction, usize)> {
            let mut new_scanner = scanner.clone();
            new_scanner.consume('(')?;
            let group_specifier = if let Some((gs, amt)) = GroupSpecifier::parse(&new_scanner, unicode) {
                new_scanner.read_idx += amt;
                Some(gs)
            } else {
                None
            };
            let (disj, amt) = Disjunction::parse(&new_scanner, unicode, sets, cgroups)?;
            new_scanner.read_idx += amt;
            new_scanner.consume(')')?;
            Some((group_specifier, disj, new_scanner.read_idx - scanner.read_idx))
        }
        fn unnamed_group(
            scanner: &Scanner,
            unicode: UnicodeMode,
            sets: UnicodeSetsMode,
            cgroups: NamedCaptureGroups,
        ) -> Option<(Disjunction, usize)> {
            let mut new_scanner = scanner.clone();
            new_scanner.consume('(')?;
            new_scanner.consume('?')?;
            new_scanner.consume(':')?;
            let (disj, amt) = Disjunction::parse(&new_scanner, unicode, sets, cgroups)?;
            new_scanner.read_idx += amt;
            new_scanner.consume(')')?;
            Some((disj, new_scanner.read_idx - scanner.read_idx))
        }

        let mut new_scanner = scanner.clone();
        let left_capturing_parens_before = scanner.left_capturing_parens;
        if let Some(ch) = new_scanner.consume_filter(Scanner::is_pattern_char) {
            Some((
                Self { node: AtomNode::PatternCharacter(ch), left_capturing_parens_before },
                new_scanner.read_idx - scanner.read_idx,
            ))
        } else if let Some(()) = new_scanner.consume('.') {
            Some((Self { node: AtomNode::Dot, left_capturing_parens_before }, new_scanner.read_idx - scanner.read_idx))
        } else if let Some((ae, amt)) = slash_escape(&new_scanner, unicode, cgroups) {
            Some((Self { node: AtomNode::AtomEscape(ae), left_capturing_parens_before }, amt))
        } else if let Some((class, amt)) = CharacterClass::parse(&new_scanner, unicode, sets) {
            new_scanner.read_idx += amt;
            Some((
                Self { node: AtomNode::CharacterClass(class), left_capturing_parens_before },
                new_scanner.read_idx - scanner.read_idx,
            ))
        } else if let Some((gs, disj, amt)) = grouped_disjunction(&new_scanner, unicode, sets, cgroups) {
            new_scanner.read_idx += amt;
            new_scanner.left_capturing_parens += 1;
            Some((
                Self {
                    node: AtomNode::GroupedDisjunction { group_specifier: gs, disjunction: Box::new(disj) },
                    left_capturing_parens_before,
                },
                new_scanner.read_idx - scanner.read_idx,
            ))
        } else {
            let (disj, amt) = unnamed_group(&new_scanner, unicode, sets, cgroups)?;
            new_scanner.read_idx += amt;
            Some((
                Self { node: AtomNode::UnGroupedDisjunction(Box::new(disj)), left_capturing_parens_before },
                new_scanner.read_idx - scanner.read_idx,
            ))
        }
    }

    pub(crate) fn early_errors(
        &self,
        left_paren_count: usize,
        group_specifiers: &[&GroupSpecifier],
        usm: UnicodeSetsMode,
    ) -> Vec<Object> {
        match &self.node {
            AtomNode::PatternCharacter(_) | AtomNode::Dot => vec![],
            AtomNode::AtomEscape(atom_escape) => atom_escape.early_errors(left_paren_count, group_specifiers, usm),
            AtomNode::CharacterClass(character_class) => character_class.early_errors(usm),
            AtomNode::GroupedDisjunction { group_specifier: Some(gs), disjunction } => {
                let mut errs = gs.early_errors();
                errs.append(&mut disjunction.early_errors(left_paren_count, group_specifiers, usm));
                errs
            }
            AtomNode::GroupedDisjunction { group_specifier: None, disjunction }
            | AtomNode::UnGroupedDisjunction(disjunction) => {
                disjunction.early_errors(left_paren_count, group_specifiers, usm)
            }
        }
    }

    pub(crate) fn count_left_capturing_parens_within(&self) -> usize {
        match &self.node {
            AtomNode::PatternCharacter(_) | AtomNode::Dot | AtomNode::AtomEscape(_) | AtomNode::CharacterClass(_) => 0,
            AtomNode::GroupedDisjunction { group_specifier: _, disjunction } => {
                1 + disjunction.count_left_capturing_parens_within()
            }
            AtomNode::UnGroupedDisjunction(d) => d.count_left_capturing_parens_within(),
        }
    }

    pub(crate) fn might_both_participate(&self, x: &GroupSpecifier, y: &GroupSpecifier) -> bool {
        match &self.node {
            AtomNode::PatternCharacter(_) | AtomNode::Dot | AtomNode::AtomEscape(_) | AtomNode::CharacterClass(_) => {
                false
            }
            AtomNode::GroupedDisjunction { group_specifier: _, disjunction }
            | AtomNode::UnGroupedDisjunction(disjunction) => disjunction.might_both_participate(x, y),
        }
    }

    pub(crate) fn contains(&self, gs: &GroupSpecifier) -> bool {
        match &self.node {
            AtomNode::PatternCharacter(_) | AtomNode::Dot | AtomNode::AtomEscape(_) | AtomNode::CharacterClass(_) => {
                false
            }
            AtomNode::GroupedDisjunction { group_specifier: Some(actual_gs), disjunction } => {
                std::ptr::eq(gs, actual_gs) || disjunction.contains(gs)
            }
            AtomNode::GroupedDisjunction { group_specifier: None, disjunction }
            | AtomNode::UnGroupedDisjunction(disjunction) => disjunction.contains(gs),
        }
    }

    pub(crate) fn all_group_specifiers(&self) -> Vec<&GroupSpecifier> {
        match &self.node {
            AtomNode::PatternCharacter(_) | AtomNode::Dot | AtomNode::AtomEscape(_) | AtomNode::CharacterClass(_) => {
                vec![]
            }
            AtomNode::GroupedDisjunction { group_specifier: Some(gs), disjunction } => {
                let mut result = vec![gs];
                let mut disjunction_gs = disjunction.all_group_specifiers();
                result.append(&mut disjunction_gs);
                result
            }
            AtomNode::GroupedDisjunction { group_specifier: None, disjunction }
            | AtomNode::UnGroupedDisjunction(disjunction) => disjunction.all_group_specifiers(),
        }
    }

    pub(crate) fn group_name_associations(&self) -> Vec<Option<&GroupName>> {
        match &self.node {
            AtomNode::PatternCharacter(_) | AtomNode::Dot | AtomNode::AtomEscape(_) | AtomNode::CharacterClass(_) => {
                vec![]
            }
            AtomNode::GroupedDisjunction { group_specifier, disjunction } => {
                let mut result = vec![group_specifier.as_ref().map(|gs| &gs.0)];
                let mut disjunction_names = disjunction.group_name_associations();
                result.append(&mut disjunction_names);
                result
            }
            AtomNode::UnGroupedDisjunction(disjunction) => disjunction.group_name_associations(),
        }
    }

    pub(crate) fn count_left_capturing_parens_before(&self) -> usize {
        self.left_capturing_parens_before
    }
}

// CharacterClass[UnicodeMode, UnicodeSetsMode] ::
//      [ [lookahead ≠ ^] ClassContents[?UnicodeMode, ?UnicodeSetsMode] ]
//      [^ ClassContents[?UnicodeMode, ?UnicodeSetsMode] ]
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum CharacterClass {
    Selection(ClassContents),
    Negation(ClassContents),
}
impl fmt::Display for CharacterClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CharacterClass::Selection(class_contents) => write!(f, "[{class_contents}]"),
            CharacterClass::Negation(class_contents) => write!(f, "[^{class_contents}]"),
        }
    }
}
impl CharacterClass {
    fn parse(scanner: &Scanner, unicode: UnicodeMode, sets: UnicodeSetsMode) -> Option<(Self, usize)> {
        let mut new_scanner = scanner.clone();
        new_scanner.consume('[')?;
        let cstr = if let Some(()) = new_scanner.consume('^') { Self::Negation } else { Self::Selection };
        let (contents, amt) = ClassContents::parse(&new_scanner, unicode, sets);
        new_scanner.read_idx += amt;
        new_scanner.consume(']')?;
        Some((cstr(contents), new_scanner.read_idx - scanner.read_idx))
    }

    pub(crate) fn early_errors(&self, usm: UnicodeSetsMode) -> Vec<Object> {
        match self {
            CharacterClass::Selection(contents) => contents.early_errors(usm),
            CharacterClass::Negation(contents) => {
                // CharacterClass :: [^ ClassContents ]
                // It is a Syntax Error if MayContainStrings of the ClassContents is true.
                let mut errs = vec![];
                if contents.may_contain_strings() {
                    errs.push(create_syntax_error_object("Negation on included strings", None));
                }
                errs.append(&mut contents.early_errors(usm));
                errs
            }
        }
    }
}

// ClassContents[UnicodeMode, UnicodeSetsMode] ::
//      [empty]
//      [~UnicodeSetsMode] NonemptyClassRanges[?UnicodeMode]
//      [+UnicodeSetsMode] ClassSetExpression
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ClassContents {
    None,
    NonemptyClassRanges(Box<NonemptyClassRanges>),
    ClassSetExpression(ClassSetExpression),
}
impl fmt::Display for ClassContents {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ClassContents::None => Ok(()),
            ClassContents::NonemptyClassRanges(node) => node.fmt(f),
            ClassContents::ClassSetExpression(node) => node.fmt(f),
        }
    }
}
impl ClassContents {
    fn parse(scanner: &Scanner, unicode: UnicodeMode, sets: UnicodeSetsMode) -> (Self, usize) {
        match sets {
            UnicodeSetsMode::Denied => {
                if let Some((ranges, amt)) = NonemptyClassRanges::parse(scanner, unicode) {
                    (Self::NonemptyClassRanges(Box::new(ranges)), amt)
                } else {
                    (Self::None, 0)
                }
            }
            UnicodeSetsMode::Allowed => {
                if let Some((expr, amt)) = ClassSetExpression::parse(scanner) {
                    (Self::ClassSetExpression(expr), amt)
                } else {
                    (Self::None, 0)
                }
            }
        }
    }

    pub(crate) fn early_errors(&self, usm: UnicodeSetsMode) -> Vec<Object> {
        match self {
            ClassContents::None => vec![],
            ClassContents::NonemptyClassRanges(node) => node.early_errors(usm),
            ClassContents::ClassSetExpression(node) => node.early_errors(usm),
        }
    }

    fn may_contain_strings(&self) -> bool {
        // Static Semantics: MayContainStrings
        // The syntax-directed operation MayContainStrings takes no arguments and returns a Boolean.
        match self {
            ClassContents::None | ClassContents::NonemptyClassRanges(_) => {
                // ClassContents ::
                //      [empty]
                //      NonemptyClassRanges
                //
                // 1. Return false.
                false
            }
            ClassContents::ClassSetExpression(class_set_expression) => class_set_expression.may_contain_strings(),
        }
    }
}

// NonemptyClassRanges[UnicodeMode] ::
//      ClassAtom[?UnicodeMode]
//      ClassAtom[?UnicodeMode] NonemptyClassRangesNoDash[?UnicodeMode]
//      ClassAtom[?UnicodeMode] - ClassAtom[?UnicodeMode] ClassContents[?UnicodeMode, ~UnicodeSetsMode]
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum NonemptyClassRanges {
    List(Vec<ClassAtom>),
    Range { head: Vec<ClassAtom>, tail: ClassAtom, content: ClassContents },
}
impl fmt::Display for NonemptyClassRanges {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NonemptyClassRanges::List(class_atoms) => write!(f, "{}", join_display(class_atoms, "")),
            NonemptyClassRanges::Range { head, tail, content } => {
                write!(f, "{}-{}{}", join_display(head, ""), tail, content)
            }
        }
    }
}
impl NonemptyClassRanges {
    fn parse(scanner: &Scanner, mode: UnicodeMode) -> Option<(Self, usize)> {
        fn tail_parse(scanner: &Scanner, mode: UnicodeMode) -> Option<(ClassAtom, ClassContents, usize)> {
            let mut new_scanner = scanner.clone();
            new_scanner.consume('-')?;
            let (atom, amt) = ClassAtom::parse(&new_scanner, mode)?;
            new_scanner.read_idx += amt;
            let (content, amt) = ClassContents::parse(&new_scanner, mode, UnicodeSetsMode::Denied);
            new_scanner.read_idx += amt;
            Some((atom, content, new_scanner.read_idx - scanner.read_idx))
        }

        let mut new_scanner = scanner.clone();
        let (first_atom, amt) = ClassAtom::parse(&new_scanner, mode)?;
        new_scanner.read_idx += amt;
        if let Some((nodash, amt)) = NonemptyClassRangesNoDash::parse(&new_scanner, mode) {
            new_scanner.read_idx += amt;
            match nodash {
                NonemptyClassRangesNoDash::List { leaders, tail } => {
                    let mut results = Vec::new();
                    results.push(first_atom);
                    results.extend(leaders.into_iter().map(ClassAtom::from));
                    results.push(tail);
                    Some((Self::List(results), new_scanner.read_idx - scanner.read_idx))
                }
                NonemptyClassRangesNoDash::Range { leaders, tail, content } => {
                    let mut results = Vec::new();
                    results.push(first_atom);
                    results.extend(leaders.into_iter().map(ClassAtom::from));
                    Some((Self::Range { head: results, tail, content }, new_scanner.read_idx - scanner.read_idx))
                }
            }
        } else if let Some((atom, content, amt)) = tail_parse(&new_scanner, mode) {
            new_scanner.read_idx += amt;
            Some((Self::Range { head: vec![first_atom], tail: atom, content }, new_scanner.read_idx - scanner.read_idx))
        } else {
            Some((Self::List(vec![first_atom]), new_scanner.read_idx - scanner.read_idx))
        }
    }

    pub(crate) fn early_errors(&self, usm: UnicodeSetsMode) -> Vec<Object> {
        match self {
            NonemptyClassRanges::List(atoms) => atoms.iter().flat_map(|ca| ca.early_errors(usm)).collect(),
            NonemptyClassRanges::Range { head, tail, content } => {
                let start = &head[head.len() - 1];
                let mut errs = vec![];
                if let (Some(start), Some(tail)) = (start.character_value(), tail.character_value()) {
                    if start > tail {
                        errs.push(create_syntax_error_object("character range start after end", None));
                    }
                } else {
                    errs.push(create_syntax_error_object(
                        "character classes may not by used as character range endpoints",
                        None,
                    ));
                }
                errs.append(&mut head.iter().flat_map(|ca| ca.early_errors(usm)).collect());
                errs.append(&mut tail.early_errors(usm));
                errs.append(&mut content.early_errors(usm));
                errs
            }
        }
    }
}

// NonemptyClassRangesNoDash[UnicodeMode] ::
//      ClassAtom[?UnicodeMode]
//      ClassAtomNoDash[?UnicodeMode] NonemptyClassRangesNoDash[?UnicodeMode]
//      ClassAtomNoDash[?UnicodeMode] - ClassAtom[?UnicodeMode] ClassContents[?UnicodeMode, ~UnicodeSetsMode]
#[derive(Debug, Clone, PartialEq, Eq)]
enum NonemptyClassRangesNoDash {
    List { leaders: Vec<ClassAtomNoDash>, tail: ClassAtom },
    Range { leaders: Vec<ClassAtomNoDash>, tail: ClassAtom, content: ClassContents },
}
impl NonemptyClassRangesNoDash {
    fn parse(scanner: &Scanner, mode: UnicodeMode) -> Option<(Self, usize)> {
        // Any number of ClassAtomNoDash, followed either by a ClassAtom or by the - ClassAtom ClassContents production
        // Note that a ClassAtomNoDash is also matched by a ClassAtom, so: be careful
        let mut new_scanner = scanner.clone();
        let mut atoms = Vec::new();
        while let Some((item, amt)) = ClassAtomNoDash::parse(&new_scanner, mode) {
            new_scanner.read_idx += amt;
            atoms.push(item);
        }
        match ClassAtom::parse(&new_scanner, mode) {
            None => {
                let tail = atoms.pop()?;
                Some((
                    Self::List { leaders: atoms, tail: ClassAtom::from(tail) },
                    new_scanner.read_idx - scanner.read_idx,
                ))
            }
            Some((item, amt)) => {
                new_scanner.read_idx += amt;
                if matches!(item, ClassAtom::Char(0x2d)) {
                    // This might be the end, or there might be more.
                    let mut second_scanner = new_scanner.clone();
                    if let Some((tail, amt)) = ClassAtom::parse(&second_scanner, mode) {
                        second_scanner.read_idx += amt;
                        let (content, amt) = ClassContents::parse(&second_scanner, mode, UnicodeSetsMode::Denied);
                        second_scanner.read_idx += amt;
                        return Some((
                            Self::Range { leaders: atoms, tail, content },
                            second_scanner.read_idx - scanner.read_idx,
                        ));
                    }
                }
                Some((Self::List { leaders: atoms, tail: item }, new_scanner.read_idx - scanner.read_idx))
            }
        }
    }
}

// ClassAtom[UnicodeMode] ::
//      -
//      ClassAtomNoDash[?UnicodeMode]
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ClassAtom {
    Char(u32),
    Class(CharacterClassEscape),
}
impl fmt::Display for ClassAtom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ClassAtom::Char(ch) => write!(f, "{ch}"),
            ClassAtom::Class(character_class_escape) => character_class_escape.fmt(f),
        }
    }
}
impl ClassAtom {
    fn parse(scanner: &Scanner, mode: UnicodeMode) -> Option<(Self, usize)> {
        let mut new_scanner = scanner.clone();
        if new_scanner.consume('-').is_some() {
            Some((Self::Char(0x2d), new_scanner.read_idx - scanner.read_idx))
        } else {
            let (atom, amt) = ClassAtomNoDash::parse(scanner, mode)?;
            Some((
                match atom {
                    ClassAtomNoDash::Char(val) => Self::Char(val),
                    ClassAtomNoDash::Class(class) => Self::Class(class),
                },
                amt,
            ))
        }
    }

    fn character_value(&self) -> Option<u32> {
        match self {
            ClassAtom::Char(ch) => Some(*ch),
            ClassAtom::Class(_) => None,
        }
    }

    fn early_errors(&self, usm: UnicodeSetsMode) -> Vec<Object> {
        match self {
            ClassAtom::Char(_) => vec![],
            ClassAtom::Class(character_class_escape) => character_class_escape.early_errors(usm),
        }
    }
}

impl From<ClassAtomNoDash> for ClassAtom {
    fn from(value: ClassAtomNoDash) -> Self {
        match value {
            ClassAtomNoDash::Char(ch) => Self::Char(ch),
            ClassAtomNoDash::Class(class) => Self::Class(class),
        }
    }
}

// ClassAtomNoDash[UnicodeMode] ::
//      SourceCharacter but not one of \ or ] or -
//      \ ClassEscape[?UnicodeMode]
#[derive(Debug, Clone, PartialEq, Eq)]
enum ClassAtomNoDash {
    Char(u32),
    Class(CharacterClassEscape),
}
impl ClassAtomNoDash {
    fn parse(scanner: &Scanner, mode: UnicodeMode) -> Option<(Self, usize)> {
        let mut new_scanner = scanner.clone();
        let ch = new_scanner.consume_any()?;
        if ch == u32::from(']') || ch == u32::from('-') {
            None
        } else if ch == u32::from('\\') {
            let (escape, amt) = ClassEscape::parse(&new_scanner, mode)?;
            Some((
                match escape {
                    ClassEscape::Char(val) => Self::Char(val),
                    ClassEscape::Class(class) => Self::Class(class),
                },
                new_scanner.read_idx + amt - scanner.read_idx,
            ))
        } else {
            Some((Self::Char(ch), new_scanner.read_idx - scanner.read_idx))
        }
    }
}

// ClassEscape[UnicodeMode] ::
//      b
//      [+UnicodeMode] -
//      CharacterClassEscape[?UnicodeMode]
//      CharacterEscape[?UnicodeMode]
#[derive(Debug, Clone, PartialEq, Eq)]
enum ClassEscape {
    Char(u32),
    Class(CharacterClassEscape),
} // nope. the char class escape can be multi
impl ClassEscape {
    fn parse(scanner: &Scanner, mode: UnicodeMode) -> Option<(Self, usize)> {
        let mut new_scanner = scanner.clone();
        if new_scanner.consume('b').is_some() {
            Some((Self::Char(8), new_scanner.read_idx - scanner.read_idx))
        } else if mode == UnicodeMode::Allowed && new_scanner.consume('-').is_some() {
            Some((Self::Char(0x2d), new_scanner.read_idx - scanner.read_idx))
        } else if let Some((cce, amt)) = CharacterClassEscape::parse(&new_scanner, mode) {
            Some((Self::Class(cce), amt))
        } else {
            let (ce, amt) = CharacterEscape::parse(&new_scanner, mode)?;
            Some((Self::Char(ce.character_value()), amt))
        }
    }
}

// ClassSetExpression ::
//      ClassUnion
//      ClassIntersection
//      ClassSubtraction
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ClassSetExpression {
    Union(ClassUnion),
    Intersection(ClassIntersection),
    Subtraction(ClassSubtraction),
}
impl fmt::Display for ClassSetExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ClassSetExpression::Union(node) => node.fmt(f),
            ClassSetExpression::Intersection(node) => node.fmt(f),
            ClassSetExpression::Subtraction(node) => node.fmt(f),
        }
    }
}
impl ClassSetExpression {
    fn parse(scanner: &Scanner) -> Option<(Self, usize)> {
        if let Some((union, amt)) = ClassUnion::parse(scanner) {
            Some((Self::Union(union), amt))
        } else if let Some((intersection, amt)) = ClassIntersection::parse(scanner) {
            Some((Self::Intersection(intersection), amt))
        } else {
            let (subtraction, amt) = ClassSubtraction::parse(scanner)?;
            Some((Self::Subtraction(subtraction), amt))
        }
    }

    fn early_errors(&self, usm: UnicodeSetsMode) -> Vec<Object> {
        match self {
            ClassSetExpression::Union(node) => node.early_errors(usm),
            ClassSetExpression::Intersection(node) => node.early_errors(usm),
            ClassSetExpression::Subtraction(node) => node.early_errors(usm),
        }
    }

    fn may_contain_strings(&self) -> bool {
        // Static Semantics: MayContainStrings
        // The syntax-directed operation MayContainStrings takes no arguments and returns a Boolean.
        match self {
            ClassSetExpression::Union(node) => node.may_contain_strings(),
            ClassSetExpression::Intersection(node) => node.may_contain_strings(),
            ClassSetExpression::Subtraction(node) => node.may_contain_strings(),
        }
    }
}

// ClassUnion ::
//      ClassSetRange ClassUnionopt
//      ClassSetOperand ClassUnionopt
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ClassUnion {
    Range { range: ClassSetRange, union: Option<Box<ClassUnion>> },
    Operand { operand: ClassSetOperand, union: Option<Box<ClassUnion>> },
}
impl fmt::Display for ClassUnion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ClassUnion::Range { range, union: None } => range.fmt(f),
            ClassUnion::Range { range, union: Some(union) } => write!(f, "{range}{union}"),
            ClassUnion::Operand { operand, union: None } => operand.fmt(f),
            ClassUnion::Operand { operand, union: Some(union) } => write!(f, "{operand}{union}"),
        }
    }
}
impl ClassUnion {
    fn parse(scanner: &Scanner) -> Option<(Self, usize)> {
        let mut new_scanner = scanner.clone();
        if let Some((range, amt)) = ClassSetRange::parse(&new_scanner) {
            new_scanner.read_idx += amt;
            let union = match ClassUnion::parse(&new_scanner) {
                None => None,
                Some((union, amt)) => {
                    new_scanner.read_idx += amt;
                    Some(Box::new(union))
                }
            };
            Some((ClassUnion::Range { range, union }, new_scanner.read_idx - scanner.read_idx))
        } else {
            let (operand, amt) = ClassSetOperand::parse(&new_scanner)?;
            new_scanner.read_idx += amt;
            let union = match ClassUnion::parse(&new_scanner) {
                None => None,
                Some((union, amt)) => {
                    new_scanner.read_idx += amt;
                    Some(Box::new(union))
                }
            };
            Some((ClassUnion::Operand { operand, union }, new_scanner.read_idx - scanner.read_idx))
        }
    }

    fn early_errors(&self, usm: UnicodeSetsMode) -> Vec<Object> {
        match self {
            ClassUnion::Range { range, union: Some(union) } => {
                let mut errs = range.early_errors();
                errs.append(&mut union.early_errors(usm));
                errs
            }
            ClassUnion::Range { range, union: None } => range.early_errors(),
            ClassUnion::Operand { operand, union: Some(union) } => {
                let mut errs = operand.early_errors(usm);
                errs.append(&mut union.early_errors(usm));
                errs
            }
            ClassUnion::Operand { operand, union: None } => operand.early_errors(usm),
        }
    }

    fn may_contain_strings(&self) -> bool {
        // Static Semantics: MayContainStrings
        // The syntax-directed operation MayContainStrings takes no arguments and returns a Boolean.
        match self {
            // ClassUnion :: ClassSetRange ClassUnion[opt]
            // 1. If the ClassUnion is present, return MayContainStrings of the ClassUnion.
            // 2. Return false.
            ClassUnion::Range { range: _, union: None } => false,
            ClassUnion::Range { range: _, union: Some(union) } => union.may_contain_strings(),
            // ClassUnion :: ClassSetOperand ClassUnionopt
            // 1. If MayContainStrings of the ClassSetOperand is true, return true.
            // 2. If ClassUnion is present, return MayContainStrings of the ClassUnion.
            // 3. Return false.
            ClassUnion::Operand { operand, union: None } => operand.may_contain_strings(),
            ClassUnion::Operand { operand, union: Some(union) } => {
                operand.may_contain_strings() || union.may_contain_strings()
            }
        }
    }
}

// ClassIntersection ::
//      ClassSetOperand && [lookahead ≠ &] ClassSetOperand
//      ClassIntersection && [lookahead ≠ &] ClassSetOperand
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ClassIntersection(Vec<ClassSetOperand>);
impl fmt::Display for ClassIntersection {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", join_display(&self.0, "&&"))
    }
}
impl ClassIntersection {
    fn parse(scanner: &Scanner) -> Option<(Self, usize)> {
        fn back_part(scanner: &Scanner) -> Option<(ClassSetOperand, usize)> {
            let mut new_scanner = scanner.clone();
            new_scanner.consume('&')?;
            new_scanner.consume('&')?;
            if new_scanner.peek().unwrap_or(32) == u32::from('&') {
                None
            } else {
                let (operand, amt) = ClassSetOperand::parse(&new_scanner)?;
                new_scanner.read_idx += amt;
                Some((operand, new_scanner.read_idx - scanner.read_idx))
            }
        }

        let mut new_scanner = scanner.clone();
        let mut result = Vec::new();
        let (operand, amt) = ClassSetOperand::parse(&new_scanner)?;
        new_scanner.read_idx += amt;
        result.push(operand);
        let (operand, amt) = back_part(&new_scanner)?;
        new_scanner.read_idx += amt;
        result.push(operand);

        while let Some((operand, amt)) = back_part(&new_scanner) {
            new_scanner.read_idx += amt;
            result.push(operand);
        }

        Some((Self(result), new_scanner.read_idx - scanner.read_idx))
    }

    fn early_errors(&self, usm: UnicodeSetsMode) -> Vec<Object> {
        self.0.iter().flat_map(|cso| cso.early_errors(usm)).collect()
    }

    fn may_contain_strings(&self) -> bool {
        // Static Semantics: MayContainStrings
        // The syntax-directed operation MayContainStrings takes no arguments and returns a Boolean.

        // ClassIntersection :: ClassSetOperand && ClassSetOperand
        // 1. If MayContainStrings of the first ClassSetOperand is false, return false.
        // 2. If MayContainStrings of the second ClassSetOperand is false, return false.
        // 3. Return true.
        // ClassIntersection :: ClassIntersection && ClassSetOperand
        // 1. If MayContainStrings of the ClassIntersection is false, return false.
        // 2. If MayContainStrings of the ClassSetOperand is false, return false.
        // 3. Return true.
        self.0.iter().all(ClassSetOperand::may_contain_strings)
    }
}

// ClassSubtraction ::
//      ClassSetOperand -- ClassSetOperand
//      ClassSubtraction -- ClassSetOperand
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ClassSubtraction(Vec<ClassSetOperand>);
impl fmt::Display for ClassSubtraction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", join_display(&self.0, "--"))
    }
}
impl ClassSubtraction {
    fn parse(scanner: &Scanner) -> Option<(Self, usize)> {
        fn back_part(scanner: &Scanner) -> Option<(ClassSetOperand, usize)> {
            let mut new_scanner = scanner.clone();
            new_scanner.consume('-')?;
            new_scanner.consume('-')?;
            let (operand, amt) = ClassSetOperand::parse(&new_scanner)?;
            new_scanner.read_idx += amt;
            Some((operand, new_scanner.read_idx - scanner.read_idx))
        }

        let mut new_scanner = scanner.clone();
        let mut result = Vec::new();
        let (operand, amt) = ClassSetOperand::parse(&new_scanner)?;
        new_scanner.read_idx += amt;
        result.push(operand);
        let (operand, amt) = back_part(&new_scanner)?;
        new_scanner.read_idx += amt;
        result.push(operand);

        while let Some((operand, amt)) = back_part(&new_scanner) {
            new_scanner.read_idx += amt;
            result.push(operand);
        }

        Some((Self(result), new_scanner.read_idx - scanner.read_idx))
    }

    fn early_errors(&self, usm: UnicodeSetsMode) -> Vec<Object> {
        self.0.iter().flat_map(|cso| cso.early_errors(usm)).collect()
    }

    fn may_contain_strings(&self) -> bool {
        // Static Semantics: MayContainStrings
        // The syntax-directed operation MayContainStrings takes no arguments and returns a Boolean.

        // ClassSubtraction :: ClassSetOperand -- ClassSetOperand
        // 1. Return MayContainStrings of the first ClassSetOperand.
        // ClassSubtraction :: ClassSubtraction -- ClassSetOperand
        // 1. Return MayContainStrings of the ClassSubtraction.

        self.0[0].may_contain_strings()
    }
}

// ClassSetRange ::
//      ClassSetCharacter - ClassSetCharacter
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) struct ClassSetRange {
    first: u32,
    last: u32,
}
impl fmt::Display for ClassSetRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}-{}", ClassSetCharacter::display_string(self.first), ClassSetCharacter::display_string(self.last))
    }
}
impl ClassSetRange {
    fn parse(scanner: &Scanner) -> Option<(Self, usize)> {
        let mut new_scanner = scanner.clone();
        let (first, amt) = ClassSetCharacter::parse(&new_scanner)?;
        new_scanner.read_idx += amt;
        new_scanner.consume('-')?;
        let (last, amt) = ClassSetCharacter::parse(&new_scanner)?;
        new_scanner.read_idx += amt;
        Some((
            Self { first: first.character_value(), last: last.character_value() },
            new_scanner.read_idx - scanner.read_idx,
        ))
    }

    fn early_errors(self) -> Vec<Object> {
        if self.first > self.last {
            vec![create_syntax_error_object("character range start is after end", None)]
        } else {
            vec![]
        }
    }
}

// ClassSetOperand ::
//      NestedClass
//      ClassStringDisjunction
//      ClassSetCharacter
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ClassSetOperand {
    NestedClass(NestedClass),
    ClassStringDisjunction(ClassStringDisjunction),
    ClassSetCharacter(u32),
}
impl fmt::Display for ClassSetOperand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ClassSetOperand::NestedClass(nested_class) => nested_class.fmt(f),
            ClassSetOperand::ClassStringDisjunction(class_string_disjunction) => class_string_disjunction.fmt(f),
            ClassSetOperand::ClassSetCharacter(ch) => write!(f, "{}", ClassSetCharacter::display_string(*ch)),
        }
    }
}
impl ClassSetOperand {
    fn parse(scanner: &Scanner) -> Option<(Self, usize)> {
        if let Some((nested, amt)) = NestedClass::parse(scanner) {
            Some((Self::NestedClass(nested), amt))
        } else if let Some((disj, amt)) = ClassStringDisjunction::parse(scanner) {
            Some((Self::ClassStringDisjunction(disj), amt))
        } else {
            let (ch, amt) = ClassSetCharacter::parse(scanner)?;
            Some((Self::ClassSetCharacter(ch.character_value()), amt))
        }
    }

    fn early_errors(&self, usm: UnicodeSetsMode) -> Vec<Object> {
        match self {
            ClassSetOperand::NestedClass(node) => node.early_errors(usm),
            ClassSetOperand::ClassStringDisjunction(_) | ClassSetOperand::ClassSetCharacter(_) => vec![],
        }
    }

    fn may_contain_strings(&self) -> bool {
        // Static Semantics: MayContainStrings
        // The syntax-directed operation MayContainStrings takes no arguments and returns a Boolean.
        match self {
            ClassSetOperand::NestedClass(node) => node.may_contain_strings(),
            ClassSetOperand::ClassStringDisjunction(node) => node.may_contain_strings(),
            ClassSetOperand::ClassSetCharacter(_) => {
                // ClassSetOperand ::
                //      ClassSetCharacter
                // 1. Return false.
                false
            }
        }
    }
}

// NestedClass ::
//      [ [lookahead ≠ ^] ClassContents[+UnicodeMode, +UnicodeSetsMode] ]
//      [^ ClassContents[+UnicodeMode, +UnicodeSetsMode] ]
//      \ CharacterClassEscape[+UnicodeMode]
// NOTE: The first two lines here are equivalent to CharacterClass.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum NestedClass {
    Class(Box<ClassContents>),
    NegatedClass(Box<ClassContents>),
    CharacterClassEscape(CharacterClassEscape),
}
impl fmt::Display for NestedClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NestedClass::Class(class_contents) => write!(f, "[{class_contents}]"),
            NestedClass::NegatedClass(class_contents) => write!(f, "[^{class_contents}]"),
            NestedClass::CharacterClassEscape(character_class_escape) => write!(f, "\\{character_class_escape}"),
        }
    }
}
impl NestedClass {
    fn parse(scanner: &Scanner) -> Option<(Self, usize)> {
        let mut new_scanner = scanner.clone();
        let ch = new_scanner.consume_any()?;
        if ch == u32::from('\\') {
            let (class, amt) = CharacterClassEscape::parse(&new_scanner, UnicodeMode::Allowed)?;
            new_scanner.read_idx += amt;
            Some((Self::CharacterClassEscape(class), new_scanner.read_idx - scanner.read_idx))
        } else if ch == u32::from('[') {
            match new_scanner.consume('^') {
                None => {
                    let (contents, amt) =
                        ClassContents::parse(&new_scanner, UnicodeMode::Allowed, UnicodeSetsMode::Allowed);
                    new_scanner.read_idx += amt;
                    new_scanner.consume(']')?;
                    Some((Self::Class(Box::new(contents)), new_scanner.read_idx - scanner.read_idx))
                }
                Some(()) => {
                    let (contents, amt) =
                        ClassContents::parse(&new_scanner, UnicodeMode::Allowed, UnicodeSetsMode::Allowed);
                    new_scanner.read_idx += amt;
                    new_scanner.consume(']')?;
                    Some((Self::NegatedClass(Box::new(contents)), new_scanner.read_idx - scanner.read_idx))
                }
            }
        } else {
            None
        }
    }

    fn early_errors(&self, usm: UnicodeSetsMode) -> Vec<Object> {
        match self {
            NestedClass::Class(node) => node.early_errors(usm),
            NestedClass::NegatedClass(node) => {
                // NestedClass :: [^ ClassContents ]
                // It is a Syntax Error if MayContainStrings of the ClassContents is true.
                let mut errs = vec![];
                if node.may_contain_strings() {
                    errs.push(create_syntax_error_object("Negated class can't contain strings", None));
                }
                errs.append(&mut node.early_errors(usm));
                errs
            }
            NestedClass::CharacterClassEscape(character_class_escape) => character_class_escape.early_errors(usm),
        }
    }

    fn may_contain_strings(&self) -> bool {
        // Static Semantics: MayContainStrings
        // The syntax-directed operation MayContainStrings takes no arguments and returns a Boolean.
        match self {
            NestedClass::Class(node) => node.may_contain_strings(),
            NestedClass::NegatedClass(_) => {
                // NestedClass ::
                //      [^ ClassContents ]
                //
                // 1. Return false.
                false
            }
            NestedClass::CharacterClassEscape(node) => node.may_contain_strings(),
        }
    }
}

// ClassStringDisjunction ::
//      \q{ ClassStringDisjunctionContents }
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ClassStringDisjunction(Vec<ClassString>);
impl fmt::Display for ClassStringDisjunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\\q{{{}}}", join_display(&self.0, "|"))
    }
}
impl ClassStringDisjunction {
    fn parse(scanner: &Scanner) -> Option<(Self, usize)> {
        let mut new_scanner = scanner.clone();
        new_scanner.consume('\\')?;
        new_scanner.consume('q')?;
        new_scanner.consume('{')?;
        let (content, amt) = ClassStringDisjunctionContents::parse(&new_scanner)?;
        new_scanner.read_idx += amt;
        new_scanner.consume('}')?;
        Some((Self(content.0), new_scanner.read_idx - scanner.read_idx))
    }

    fn may_contain_strings(&self) -> bool {
        // Static Semantics: MayContainStrings
        // The syntax-directed operation MayContainStrings takes no arguments and returns a Boolean.

        // ClassStringDisjunctionContents :: ClassString | ClassStringDisjunctionContents
        // 1. If MayContainStrings of the ClassString is true, return true.
        // 2. Return MayContainStrings of the ClassStringDisjunctionContents.
        self.0.iter().any(ClassString::may_contain_strings)
    }
}

// ClassStringDisjunctionContents ::
//      ClassString
//      ClassString | ClassStringDisjunctionContents
#[derive(Debug, Clone, PartialEq, Eq)]
struct ClassStringDisjunctionContents(Vec<ClassString>);
impl ClassStringDisjunctionContents {
    fn parse(scanner: &Scanner) -> Option<(Self, usize)> {
        let mut new_scanner = scanner.clone();
        let mut result = Vec::new();
        loop {
            let (string, amt) = ClassString::parse(&new_scanner);
            new_scanner.read_idx += amt;
            result.push(string);
            if new_scanner.consume('|').is_none() {
                break;
            }
        }
        if result.is_empty() { None } else { Some((Self(result), new_scanner.read_idx - scanner.read_idx)) }
    }
}

// ClassString ::
//      [empty]
//      NonEmptyClassString
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ClassString(Vec<u32>);
impl fmt::Display for ClassString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", join_display(&self.0, ""))
    }
}
impl ClassString {
    fn parse(scanner: &Scanner) -> (Self, usize) {
        if let Some((string, amt)) = NonEmptyClassString::parse(scanner) {
            (Self(string.0), amt)
        } else {
            (Self(Vec::new()), 0)
        }
    }

    fn may_contain_strings(&self) -> bool {
        // Static Semantics: MayContainStrings
        // The syntax-directed operation MayContainStrings takes no arguments and returns a Boolean.

        // ClassString :: [empty]
        //      1. Return true.
        // ClassString :: NonEmptyClassString
        //      1. Return MayContainStrings of the NonEmptyClassString.
        // NonEmptyClassString :: ClassSetCharacter NonEmptyClassStringopt
        //      1. If NonEmptyClassString is present, return true.
        //      2. Return false.
        self.0.len() != 1
    }
}

// NonEmptyClassString ::
//      ClassSetCharacter NonEmptyClassStringopt
#[derive(Debug, Clone, PartialEq, Eq)]
struct NonEmptyClassString(Vec<u32>);
impl NonEmptyClassString {
    fn parse(scanner: &Scanner) -> Option<(Self, usize)> {
        // This is one or more ClassSetCharacters
        let mut result = Vec::new();
        let mut new_scanner = scanner.clone();
        while let Some((ch, amt)) = ClassSetCharacter::parse(&new_scanner) {
            new_scanner.read_idx += amt;
            result.push(ch.character_value());
        }
        if result.is_empty() { None } else { Some((Self(result), new_scanner.read_idx - scanner.read_idx)) }
    }
}

#[cfg(test)]
mod tests;
