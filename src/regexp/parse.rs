#![expect(dead_code)]
use super::*;

const PREVIOUSLY_SCANNED: &str = "previously scanned char should still exist";

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum UnicodeMode {
    Allowed,
    Denied,
}
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum NamedCaptureGroups {
    Allowed,
    Denied,
}
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum UnicodeSetsMode {
    Allowed,
    Denied,
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct Scanner<'src> {
    all: &'src str,
    read_idx: usize,
}

impl<'src> Scanner<'src> {
    pub fn new(text: &'src str) -> Self {
        Scanner { all: text, read_idx: 0 }
    }

    pub fn done(&self) -> bool {
        self.read_idx >= self.all.len()
    }

    pub fn peek(&self) -> Option<char> {
        self.all.get(self.read_idx..).and_then(|s| s.chars().next())
    }

    pub fn lookahead(&self, amt: usize) -> Option<char> {
        self.all.get(self.read_idx..).and_then(|s| {
            let mut chars = s.chars();
            for _ in 0..amt {
                chars.next();
            }
            chars.next()
        })
    }

    pub fn advance(&mut self) -> Option<usize> {
        self.all.get(self.read_idx..).and_then(|s| s.chars().next()).map(|ch| {
            let ch_len = ch.len_utf8();
            self.read_idx += ch_len;
            ch_len
        })
    }

    pub fn consume(&mut self, target: char) -> Option<()> {
        self.all.get(self.read_idx..).and_then(|s| s.chars().next()).and_then(|ch| {
            if ch == target {
                let ch_len = ch.len_utf8();
                self.read_idx += ch_len;
                Some(())
            } else {
                None
            }
        })
    }

    pub fn consume_any(&mut self) -> Option<char> {
        self.all.get(self.read_idx..).and_then(|s| s.chars().next()).inspect(|ch| {
            let ch_len = ch.len_utf8();
            self.read_idx += ch_len;
        })
    }

    pub fn digit(&mut self, radix: u32) -> Option<u8> {
        self.all.get(self.read_idx..).and_then(|s| s.chars().next()).and_then(|ch| ch.to_digit(radix)).map(|digit| {
            self.read_idx += 1;
            u8::try_from(digit).expect("one digit fits in a u8")
        })
    }

    pub fn hex_digit(&mut self) -> Option<u8> {
        self.all.get(self.read_idx..).and_then(|s| s.chars().next()).and_then(|ch| ch.to_digit(16)).map(|digit| {
            self.read_idx += 1;
            u8::try_from(digit).expect("one hex digit fits in a u8")
        })
    }

    pub fn advance_by_bytes(&mut self, amt: usize) {
        self.read_idx += amt;
    }

    pub fn matches_at(&self, ch: char, position: usize) -> Option<usize> {
        self.lookahead(position).and_then(|newch| if newch == ch { Some(ch.len_utf8()) } else { None })
    }

    fn is_ascii_letter(ch: char) -> bool {
        // AsciiLetter :: one of
        //      a b c d e f g h i j k l m n o p q r s t u v w x y z A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
        ch.is_ascii_alphabetic()
    }

    fn is_decimal_digit(ch: char) -> bool {
        // DecimalDigit :: one of
        //      0 1 2 3 4 5 6 7 8 9
        ch.is_ascii_digit()
    }

    fn is_unicode_property_name_character(ch: char) -> bool {
        // UnicodePropertyNameCharacter ::
        //      AsciiLetter
        //      _
        Self::is_ascii_letter(ch) || ch == '_'
    }

    fn is_unicode_property_value_character(ch: char) -> bool {
        // UnicodePropertyValueCharacter ::
        //      UnicodePropertyNameCharacter
        //      DecimalDigit
        Self::is_unicode_property_name_character(ch) || Self::is_decimal_digit(ch)
    }

    fn consume_filter(&mut self, f: impl FnOnce(char) -> bool) -> Option<char> {
        let ch = self.peek()?;
        if f(ch) {
            self.advance().expect(PREVIOUSLY_SCANNED);
            Some(ch)
        } else {
            None
        }
    }

    pub fn unicode_property_name_character(&mut self) -> Option<char> {
        self.consume_filter(Self::is_unicode_property_name_character)
    }

    pub fn unicode_property_value_character(&mut self) -> Option<char> {
        self.consume_filter(Self::is_unicode_property_value_character)
    }

    fn is_identifier_start_char(ch: char) -> bool {
        // IdentifierStartChar ::
        //      UnicodeIDStart
        //      $
        //      _
        is_unicode_id_start(ch) || ch == '$' || ch == '_'
    }

    pub fn identifier_start_char(&mut self) -> Option<char> {
        self.consume_filter(Self::is_identifier_start_char)
    }

    fn is_identifier_part_char(ch: char) -> bool {
        // IdentifierPartChar ::
        //      UnicodeIDContinue
        //      $
        is_unicode_id_continue(ch) || ch == '$'
    }

    pub fn identifier_part_char(&mut self) -> Option<char> {
        self.consume_filter(Self::is_identifier_part_char)
    }

    fn is_pattern_char(ch: char) -> bool {
        !['^', '$', '\\', '.', '*', '+', '?', '(', ')', '[', ']', '{', '}', '|'].contains(&ch)
    }
}

// ClassSetReservedPunctuator :: one of
//      & - ! # % , : ; < = > @ ` ~
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
struct ClassSetReservedPunctuator(u32);

impl ClassSetReservedPunctuator {
    pub fn parse(scanner: &Scanner) -> Option<(Self, usize)> {
        let mut new_scanner = scanner.clone();
        let ch = new_scanner.consume_filter(|ch| {
            ['&', '-', '!', '#', '%', ',', ':', ';', '<', '=', '>', '@', '`', '~'].contains(&ch)
        })?;
        Some((Self(u32::from(ch)), new_scanner.read_idx - scanner.read_idx))
    }
}

// ClassSetSyntaxCharacter :: one of
//      ( ) [ ] { } / - \ |
#[derive(Debug, Clone)]
struct ClassSetSyntaxCharacter(char);

impl ClassSetSyntaxCharacter {
    pub fn parse(scanner: &Scanner) -> Option<(Self, usize)> {
        let ch = scanner.peek();
        if let Some(ch) = ch {
            if ['(', ')', '[', ']', '{', '}', '/', '-', '\\', '|'].contains(&ch) {
                return Some((Self(ch), 1));
            }
        }
        None
    }
}

// ClassSetReservedDoublePunctuator :: one of
//      && !! ## $$ %% ** ++ ,, .. :: ;; << == >> ?? @@ ^^ `` ~~
#[derive(Debug, Clone)]
struct ClassSetReservedDoublePunctuator {}

impl ClassSetReservedDoublePunctuator {
    pub fn parse(scanner: &Scanner) -> Option<(Self, usize)> {
        let och_left = scanner.peek();
        let och_right = scanner.lookahead(1);
        if let (Some(ch_left), Some(ch_right)) = (och_left, och_right) {
            if ['&', '!', '#', '$', '%', '*', '+', ',', '.', ':', ';', '<', '=', '>', '?', '@', '^', '`', '~']
                .contains(&ch_left)
                && ch_left == ch_right
            {
                return Some((Self {}, 2));
            }
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
    pub fn parse(scanner: &Scanner) -> Option<(Self, usize)> {
        if ClassSetReservedDoublePunctuator::parse(scanner).is_some() {
            None
        } else if let Some((cssc, consumed)) = ClassSetSyntaxCharacter::parse(scanner) {
            let ClassSetSyntaxCharacter(cssc_match) = cssc;
            if cssc_match == '\\' {
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
                if newscan.peek() == Some('b') {
                    let x = newscan.advance().expect(PREVIOUSLY_SCANNED);
                    return Some((Self::LetterB, consumed + x));
                }
                None
            } else {
                None
            }
        } else {
            // SourceCharacter but not ClassSetSyntaxCharacter
            let mut new_scanner = scanner.clone();
            let ch = new_scanner.consume_any()?;
            Some((Self::SourceCharacter(u32::from(ch)), new_scanner.read_idx - scanner.read_idx))
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
}

// CharacterEscape[UnicodeMode] ::
//      ControlEscape
//      c AsciiLetter
//      0 [lookahead ∉ DecimalDigit]
//      HexEscapeSequence
//      RegExpUnicodeEscapeSequence[?UnicodeMode]
//      IdentityEscape[?UnicodeMode]
#[derive(Debug, Clone, Eq, PartialEq)]
enum CharacterEscape {
    ControlEscape(ControlEscape),
    CAsciiLetter(AsciiLetter),
    Zero,
    HexEscapeSequence(HexEscapeSequence),
    RegExpUnicodeEscapeSequence(RegExpUnicodeEscapeSequence),
    IdentityEscape(IdentityEscape),
}

impl CharacterEscape {
    fn parse(scanner: &Scanner, mode: UnicodeMode) -> Option<(Self, usize)> {
        if let Some((ce, consumed)) = ControlEscape::parse(scanner) {
            return Some((Self::ControlEscape(ce), consumed));
        }
        let ch = scanner.peek();
        if ch == Some('c') {
            let mut new_scanner = scanner.clone();
            let start = new_scanner.advance().expect(PREVIOUSLY_SCANNED);
            if let Some((al, consumed)) = AsciiLetter::parse(&new_scanner) {
                return Some((Self::CAsciiLetter(al), start + consumed));
            }
        } else if ch == Some('0') {
            let lookahead = scanner.lookahead(1);
            let lookahead_is_digit = if let Some(digit) = lookahead { digit.is_ascii_digit() } else { false };
            if !lookahead_is_digit {
                let mut new_scanner = scanner.clone();
                let len = new_scanner.advance().expect(PREVIOUSLY_SCANNED);
                return Some((Self::Zero, len));
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

    pub fn character_value(&self) -> u32 {
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
                u32::from(al.0) % 32
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
enum ControlEscape {
    Eff,
    En,
    Ar,
    Tee,
    Vee,
}
impl ControlEscape {
    fn parse(scanner: &Scanner) -> Option<(Self, usize)> {
        let ch = scanner.peek();
        match ch {
            Some('f') => Some((Self::Eff, 1)),
            Some('n') => Some((Self::En, 1)),
            Some('r') => Some((Self::Ar, 1)),
            Some('t') => Some((Self::Tee, 1)),
            Some('v') => Some((Self::Vee, 1)),
            _ => None,
        }
    }
}

// AsciiLetter :: one of
//      a b c d e f g h i j k l m n o p q r s t u v w x y z A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
#[derive(Debug, Clone, Eq, PartialEq)]
struct AsciiLetter(char);
impl AsciiLetter {
    fn parse(scanner: &Scanner) -> Option<(Self, usize)> {
        let ch = scanner.peek();
        if let Some(ch) = ch {
            if ch.is_ascii_alphabetic() {
                return Some((Self(ch), 1));
            }
        }
        None
    }
}

// HexEscapeSequence ::
//      x HexDigit HexDigit
#[derive(Debug, Clone, Eq, PartialEq)]
struct HexEscapeSequence(u8);
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
struct RegExpUnicodeEscapeSequence(u32);
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
                    while new_scanner.peek()? != '}' {
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
struct IdentityEscape(u32);
impl IdentityEscape {
    fn parse(scanner: &Scanner, mode: UnicodeMode) -> Option<(Self, usize)> {
        match mode {
            UnicodeMode::Allowed => {
                let peeked = scanner.peek()?;
                if ['^', '$', '\\', '.', '*', '+', '?', '(', ')', '[', ']', '{', '}', '|', '/'].contains(&peeked) {
                    Some((Self(u32::from(peeked)), 1))
                } else {
                    None
                }
            }
            UnicodeMode::Denied => {
                let mut new_scanner = scanner.clone();
                let ch = new_scanner.consume_any()?;
                if is_unicode_id_continue(ch) {
                    None
                } else {
                    Some((Self(u32::from(ch)), new_scanner.read_idx - scanner.read_idx))
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
enum AtomEscape {
    DecimalEscape(DecimalEscape),
    CharacterClassEscape(CharacterClassEscape),
    CharacterEscape(CharacterEscape),
    GroupName(Box<GroupName>),
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
struct DecimalEscape(u32);

impl DecimalEscape {
    fn parse(scanner: &Scanner) -> Option<(Self, usize)> {
        const NUMBER_OK: &str = "number chars should be transformable to digits";
        let mut new_scanner = scanner.clone();
        let d1 = new_scanner.consume_filter(|c| ('1'..='9').contains(&c))?;
        let mut value = d1.to_digit(10).expect(NUMBER_OK);
        while let Some(ch) = new_scanner.consume_filter(Scanner::is_decimal_digit) {
            value = value.checked_mul(10)?.checked_add(ch.to_digit(10).expect(NUMBER_OK))?;
        }
        Some((Self(value), new_scanner.read_idx - scanner.read_idx))
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
enum CharacterClassEscape {
    Digit,
    NotDigit,
    Whitespace,
    NotWhitespace,
    Word,
    NotWord,
    Property(Box<UnicodePropertyValueExpression>),
    NotProperty(Box<UnicodePropertyValueExpression>),
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
}

// UnicodePropertyValueExpression ::
//      UnicodePropertyName = UnicodePropertyValue
//      LoneUnicodePropertyNameOrValue
#[derive(Debug, Clone, Eq, PartialEq)]
enum UnicodePropertyValueExpression {
    NameValue { name: UnicodePropertyName, value: UnicodePropertyValue },
    Lone(LoneUnicodePropertyNameOrValue),
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
}

// UnicodePropertyName ::
//      UnicodePropertyNameCharacters
// UnicodePropertyNameCharacters ::
//      UnicodePropertyNameCharacter UnicodePropertyNameCharactersopt
#[derive(Debug, Clone, Eq, PartialEq)]
struct UnicodePropertyName(String);

impl UnicodePropertyName {
    fn parse(scanner: &Scanner) -> Option<(Self, usize)> {
        // one or more UnicodePropertyNameCharacters
        let mut new_scanner = scanner.clone();
        let mut name = String::new();
        while let Some(ch) = new_scanner.unicode_property_name_character() {
            name.push(ch);
        }
        if name.is_empty() { None } else { Some((Self(name), new_scanner.read_idx - scanner.read_idx)) }
    }
}

// UnicodePropertyValue ::
//      UnicodePropertyValueCharacters
// UnicodePropertyValueCharacters ::
//      UnicodePropertyValueCharacter UnicodePropertyValueCharactersopt
#[derive(Debug, Clone, Eq, PartialEq)]
struct UnicodePropertyValue(String);

impl UnicodePropertyValue {
    fn parse(scanner: &Scanner) -> Option<(Self, usize)> {
        // one or more UnicodePropertyValueCharacters
        let mut new_scanner = scanner.clone();
        let mut name = String::new();
        while let Some(ch) = new_scanner.unicode_property_value_character() {
            name.push(ch);
        }
        if name.is_empty() { None } else { Some((Self(name), new_scanner.read_idx - scanner.read_idx)) }
    }
}

// LoneUnicodePropertyNameOrValue ::
//      UnicodePropertyValueCharacters
#[derive(Debug, Clone, Eq, PartialEq)]
struct LoneUnicodePropertyNameOrValue(String);

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
struct GroupSpecifier(GroupName);
impl GroupSpecifier {
    fn parse(scanner: &Scanner, mode: UnicodeMode) -> Option<(Self, usize)> {
        let mut new_scanner = scanner.clone();
        new_scanner.consume('?')?;
        let (name, amt) = GroupName::parse(&new_scanner, mode)?;
        new_scanner.read_idx += amt;
        Some((Self(name), new_scanner.read_idx - scanner.read_idx))
    }
}

// GroupName[UnicodeMode] ::
//      < RegExpIdentifierName[?UnicodeMode] >
#[derive(Debug, Clone, Eq, PartialEq)]
struct GroupName(RegExpIdentifierName);

impl GroupName {
    fn parse(scanner: &Scanner, mode: UnicodeMode) -> Option<(Self, usize)> {
        let mut new_scanner = scanner.clone();
        new_scanner.consume('<')?;
        let (name, amt_used) = RegExpIdentifierName::parse(&new_scanner, mode)?;
        new_scanner.read_idx += amt_used;
        new_scanner.consume('>')?;
        Some((Self(name), new_scanner.read_idx - scanner.read_idx))
    }
}

// RegExpIdentifierName[UnicodeMode] ::
//      RegExpIdentifierStart[?UnicodeMode]
//      RegExpIdentifierName[?UnicodeMode] RegExpIdentifierPart[?UnicodeMode]
#[derive(Debug, Clone, Eq, PartialEq)]
struct RegExpIdentifierName(String);

impl RegExpIdentifierName {
    fn parse(scanner: &Scanner, mode: UnicodeMode) -> Option<(Self, usize)> {
        // A "start" followed by zero or more "parts"
        let mut new_scanner = scanner.clone();
        let mut name = String::new();
        let (RegExpIdentifierStart(start), amt_used) = RegExpIdentifierStart::parse(&new_scanner, mode)?;
        name.push(start);
        new_scanner.read_idx += amt_used;
        while let Some((RegExpIdentifierPart(part), amt_used)) = RegExpIdentifierPart::parse(&new_scanner, mode) {
            name.push(part);
            new_scanner.read_idx += amt_used;
        }
        Some((Self(name), new_scanner.read_idx - scanner.read_idx))
    }
}

// RegExpIdentifierStart[UnicodeMode] ::
//      IdentifierStartChar
//      \ RegExpUnicodeEscapeSequence[+UnicodeMode]
//      [~UnicodeMode] UnicodeLeadSurrogate UnicodeTrailSurrogate
struct RegExpIdentifierStart(char);
impl RegExpIdentifierStart {
    fn parse(scanner: &Scanner, mode: UnicodeMode) -> Option<(Self, usize)> {
        let mut new_scanner = scanner.clone();
        let ch = new_scanner.consume_any()?;
        if Scanner::is_identifier_start_char(ch) {
            Some((Self(ch), new_scanner.read_idx - scanner.read_idx))
        } else if ch == '\\' {
            let (RegExpUnicodeEscapeSequence(ch), amt_read) =
                RegExpUnicodeEscapeSequence::parse(&new_scanner, UnicodeMode::Allowed)?;
            new_scanner.read_idx += amt_read;
            Some((Self(char::from_u32(ch)?), new_scanner.read_idx - scanner.read_idx))
        } else if mode == UnicodeMode::Denied && u32::from(ch) >= 0x1_0000 && u32::from(ch) <= 0x10_ffff {
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
struct RegExpIdentifierPart(char);
impl RegExpIdentifierPart {
    fn parse(scanner: &Scanner, mode: UnicodeMode) -> Option<(Self, usize)> {
        let mut new_scanner = scanner.clone();
        let ch = new_scanner.consume_any()?;
        if Scanner::is_identifier_part_char(ch) {
            Some((Self(ch), new_scanner.read_idx - scanner.read_idx))
        } else if ch == '\\' {
            let (RegExpUnicodeEscapeSequence(ch), amt_read) =
                RegExpUnicodeEscapeSequence::parse(&new_scanner, UnicodeMode::Allowed)?;
            new_scanner.read_idx += amt_read;
            Some((Self(char::from_u32(ch)?), new_scanner.read_idx - scanner.read_idx))
        } else if mode == UnicodeMode::Denied && u32::from(ch) >= 0x1_0000 && u32::from(ch) <= 0x10_ffff {
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Pattern(Disjunction);
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

    pub fn count_left_capturing_parens_within(&self) -> usize {
        self.0.count_left_capturing_parens_within()
    }
}

pub fn parse_pattern(pattern: &str, u: bool, v: bool) -> Completion<Pattern> {
    let scanner = Scanner::new(pattern);
    if u && v {
        return Err(create_syntax_error("invalid regexp flags", None));
    }
    let (unicode, sets) = if v {
        (UnicodeMode::Allowed, UnicodeSetsMode::Allowed)
    } else if u {
        (UnicodeMode::Allowed, UnicodeSetsMode::Denied)
    } else {
        (UnicodeMode::Denied, UnicodeSetsMode::Denied)
    };
    Pattern::parse(&scanner, unicode, sets, NamedCaptureGroups::Allowed)
        .ok_or_else(|| create_syntax_error("invalid regexp", None))
}

// Disjunction[UnicodeMode, UnicodeSetsMode, NamedCaptureGroups] ::
//      Alternative[?UnicodeMode, ?UnicodeSetsMode, ?NamedCaptureGroups]
//      Alternative[?UnicodeMode, ?UnicodeSetsMode, ?NamedCaptureGroups] | Disjunction[?UnicodeMode, ?UnicodeSetsMode, ?NamedCaptureGroups]
#[derive(Debug, Clone, PartialEq, Eq)]
struct Disjunction(Vec<Alternative>);
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
    pub fn count_left_capturing_parens_within(&self) -> usize {
        self.0.iter().map(Alternative::count_left_capturing_parens_within).sum()
    }
}

// Alternative[UnicodeMode, UnicodeSetsMode, NamedCaptureGroups] ::
//      [empty]
//      Alternative[?UnicodeMode, ?UnicodeSetsMode, ?NamedCaptureGroups] Term[?UnicodeMode, ?UnicodeSetsMode, ?NamedCaptureGroups]
#[derive(Debug, Clone, PartialEq, Eq)]
struct Alternative(Vec<Term>);
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
    pub fn count_left_capturing_parens_within(&self) -> usize {
        self.0.iter().map(Term::count_left_capturing_parens_within).sum()
    }
}

// Term[UnicodeMode, UnicodeSetsMode, NamedCaptureGroups] ::
//      Assertion[?UnicodeMode, ?UnicodeSetsMode, ?NamedCaptureGroups]
//      Atom[?UnicodeMode, ?UnicodeSetsMode, ?NamedCaptureGroups]
//      Atom[?UnicodeMode, ?UnicodeSetsMode, ?NamedCaptureGroups] Quantifier
#[derive(Debug, Clone, PartialEq, Eq)]
enum Term {
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
        if let Some((assertion, amt)) = Assertion::parse(&new_scanner, unicode, sets, cgroups) {
            Some((Self::Assertion(assertion), amt))
        } else if let Some((atom, amt)) = Atom::parse(&new_scanner, unicode, sets, cgroups) {
            new_scanner.read_idx += amt;
            if let Some((q, amt)) = Quantifier::parse(&new_scanner) {
                new_scanner.read_idx += amt;
                Some((Self::Atom(atom, Some(q)), new_scanner.read_idx - scanner.read_idx))
            } else {
                Some((Self::Atom(atom, None), new_scanner.read_idx - scanner.read_idx))
            }
        } else {
            None
        }
    }
    pub fn count_left_capturing_parens_within(&self) -> usize {
        match self {
            Term::Assertion(assertion) => assertion.count_left_capturing_parens_within(),
            Term::Atom(atom, _) => atom.count_left_capturing_parens_within(),
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
#[derive(Debug, Clone, PartialEq, Eq)]
enum Assertion {
    Start,
    End,
    WordBoundary,
    NotWordBoundary,
    LookAhead(Box<Disjunction>),
    NegLookAhead(Box<Disjunction>),
    LookBehind(Box<Disjunction>),
    NegLookBehind(Box<Disjunction>),
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
            let assertion = if ch == 'b' {
                Some(Assertion::WordBoundary)
            } else if ch == 'B' {
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
                if ch == '!' {
                    true
                } else if ch == '=' {
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
    pub fn count_left_capturing_parens_within(&self) -> usize {
        match self {
            Assertion::Start | Assertion::End | Assertion::WordBoundary | Assertion::NotWordBoundary => 0,
            Assertion::LookAhead(d)
            | Assertion::NegLookAhead(d)
            | Assertion::LookBehind(d)
            | Assertion::NegLookBehind(d) => d.count_left_capturing_parens_within(),
        }
    }
}

// Quantifier ::
//      QuantifierPrefix
//      QuantifierPrefix ?
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Quantifier {
    Greedy(QuantifierPrefix),
    Restrained(QuantifierPrefix),
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
}

// QuantifierPrefix ::
//      *
//      +
//      ?
//      { DecimalDigits[~Sep] }
//      { DecimalDigits[~Sep] ,}
//      { DecimalDigits[~Sep] , DecimalDigits[~Sep] }
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum QuantifierPrefix {
    ZeroOrMore,
    OneOrMore,
    ZeroOrOne,
    Exactly(u32),
    XOrMore(u32),
    Range(u32, u32),
}
impl QuantifierPrefix {
    fn parse(scanner: &Scanner) -> Option<(Self, usize)> {
        let mut new_scanner = scanner.clone();
        let ch = new_scanner.consume_any()?;
        if ch == '*' {
            Some((Self::ZeroOrMore, new_scanner.read_idx - scanner.read_idx))
        } else if ch == '+' {
            Some((Self::OneOrMore, new_scanner.read_idx - scanner.read_idx))
        } else if ch == '?' {
            Some((Self::ZeroOrOne, new_scanner.read_idx - scanner.read_idx))
        } else if ch == '{' {
            let mut val = u32::from(new_scanner.digit(10)?);
            while let Some(v) = new_scanner.digit(10) {
                val = val.checked_mul(10)?.checked_add(u32::from(v))?;
            }
            let ch = new_scanner.consume_any()?;
            if ch == '}' {
                Some((Self::Exactly(val), new_scanner.read_idx - scanner.read_idx))
            } else if ch == ',' {
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
}

// Atom[UnicodeMode, UnicodeSetsMode, NamedCaptureGroups] ::
//      PatternCharacter
//      .
//      \ AtomEscape[?UnicodeMode, ?NamedCaptureGroups]
//      CharacterClass[?UnicodeMode, ?UnicodeSetsMode]
//      ( GroupSpecifier[?UnicodeMode]opt Disjunction[?UnicodeMode, ?UnicodeSetsMode, ?NamedCaptureGroups] )
//      (?: Disjunction[?UnicodeMode, ?UnicodeSetsMode, ?NamedCaptureGroups] )
#[derive(Debug, Clone, PartialEq, Eq)]
enum Atom {
    PatternCharacter(char),
    Dot,
    AtomEscape(AtomEscape),
    CharacterClass(CharacterClass),
    GroupedDisjunction { group_specifier: Option<GroupSpecifier>, disjunction: Box<Disjunction> },
    UnGroupedDisjunction(Box<Disjunction>),
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
        if let Some(ch) = new_scanner.consume_filter(Scanner::is_pattern_char) {
            Some((Self::PatternCharacter(ch), new_scanner.read_idx - scanner.read_idx))
        } else if let Some(()) = new_scanner.consume('.') {
            Some((Self::Dot, new_scanner.read_idx - scanner.read_idx))
        } else if let Some((ae, amt)) = slash_escape(&new_scanner, unicode, cgroups) {
            Some((Self::AtomEscape(ae), amt))
        } else if let Some((class, amt)) = CharacterClass::parse(&new_scanner, unicode, sets) {
            new_scanner.read_idx += amt;
            Some((Self::CharacterClass(class), new_scanner.read_idx - scanner.read_idx))
        } else if let Some((gs, disj, amt)) = grouped_disjunction(&new_scanner, unicode, sets, cgroups) {
            new_scanner.read_idx += amt;
            Some((
                Self::GroupedDisjunction { group_specifier: gs, disjunction: Box::new(disj) },
                new_scanner.read_idx - scanner.read_idx,
            ))
        } else {
            let (disj, amt) = unnamed_group(&new_scanner, unicode, sets, cgroups)?;
            new_scanner.read_idx += amt;
            Some((Self::UnGroupedDisjunction(Box::new(disj)), new_scanner.read_idx - scanner.read_idx))
        }
    }

    pub fn count_left_capturing_parens_within(&self) -> usize {
        match self {
            Atom::PatternCharacter(_) | Atom::Dot | Atom::AtomEscape(_) | Atom::CharacterClass(_) => 0,
            Atom::GroupedDisjunction { group_specifier: _, disjunction } => {
                1 + disjunction.count_left_capturing_parens_within()
            }
            Atom::UnGroupedDisjunction(d) => d.count_left_capturing_parens_within(),
        }
    }
}

// CharacterClass[UnicodeMode, UnicodeSetsMode] ::
//      [ [lookahead ≠ ^] ClassContents[?UnicodeMode, ?UnicodeSetsMode] ]
//      [^ ClassContents[?UnicodeMode, ?UnicodeSetsMode] ]
#[derive(Debug, Clone, PartialEq, Eq)]
enum CharacterClass {
    Selection(ClassContents),
    Negation(ClassContents),
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
}

// ClassContents[UnicodeMode, UnicodeSetsMode] ::
//      [empty]
//      [~UnicodeSetsMode] NonemptyClassRanges[?UnicodeMode]
//      [+UnicodeSetsMode] ClassSetExpression
#[derive(Debug, Clone, PartialEq, Eq)]
enum ClassContents {
    None,
    NonemptyClassRanges(Box<NonemptyClassRanges>),
    ClassSetExpression(ClassSetExpression),
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
}

// NonemptyClassRanges[UnicodeMode] ::
//      ClassAtom[?UnicodeMode]
//      ClassAtom[?UnicodeMode] NonemptyClassRangesNoDash[?UnicodeMode]
//      ClassAtom[?UnicodeMode] - ClassAtom[?UnicodeMode] ClassContents[?UnicodeMode, ~UnicodeSetsMode]
#[derive(Debug, Clone, PartialEq, Eq)]
enum NonemptyClassRanges {
    List(Vec<ClassAtom>),
    Range { head: Vec<ClassAtom>, tail: ClassAtom, content: ClassContents },
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
enum ClassAtom {
    Char(u32),
    Class(CharacterClassEscape),
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
        if ch == ']' || ch == '-' {
            None
        } else if ch == '\\' {
            let (escape, amt) = ClassEscape::parse(&new_scanner, mode)?;
            Some((
                match escape {
                    ClassEscape::Char(val) => Self::Char(val),
                    ClassEscape::Class(class) => Self::Class(class),
                },
                new_scanner.read_idx + amt - scanner.read_idx,
            ))
        } else {
            Some((Self::Char(u32::from(ch)), new_scanner.read_idx - scanner.read_idx))
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
enum ClassSetExpression {
    Union(ClassUnion),
    Intersection(ClassIntersection),
    Subtraction(ClassSubtraction),
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
}

// ClassUnion ::
//      ClassSetRange ClassUnionopt
//      ClassSetOperand ClassUnionopt
#[derive(Debug, Clone, PartialEq, Eq)]
enum ClassUnion {
    Range { range: ClassSetRange, union: Option<Box<ClassUnion>> },
    Operand { operand: ClassSetOperand, union: Option<Box<ClassUnion>> },
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
}

// ClassIntersection ::
//      ClassSetOperand && [lookahead ≠ &] ClassSetOperand
//      ClassIntersection && [lookahead ≠ &] ClassSetOperand
#[derive(Debug, Clone, PartialEq, Eq)]
struct ClassIntersection(Vec<ClassSetOperand>);
impl ClassIntersection {
    fn parse(scanner: &Scanner) -> Option<(Self, usize)> {
        fn back_part(scanner: &Scanner) -> Option<(ClassSetOperand, usize)> {
            let mut new_scanner = scanner.clone();
            new_scanner.consume('&')?;
            new_scanner.consume('&')?;
            if new_scanner.peek().unwrap_or(' ') == '&' {
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
}

// ClassSubtraction ::
//      ClassSetOperand -- ClassSetOperand
//      ClassSubtraction -- ClassSetOperand
#[derive(Debug, Clone, PartialEq, Eq)]
struct ClassSubtraction(Vec<ClassSetOperand>);
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
}

// ClassSetRange ::
//      ClassSetCharacter - ClassSetCharacter
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct ClassSetRange {
    first: u32,
    last: u32,
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
}

// ClassSetOperand ::
//      NestedClass
//      ClassStringDisjunction
//      ClassSetCharacter
#[derive(Debug, Clone, PartialEq, Eq)]
enum ClassSetOperand {
    NestedClass(NestedClass),
    ClassStringDisjunction(ClassStringDisjunction),
    ClassSetCharacter(u32),
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
}

// NestedClass ::
//      [ [lookahead ≠ ^] ClassContents[+UnicodeMode, +UnicodeSetsMode] ]
//      [^ ClassContents[+UnicodeMode, +UnicodeSetsMode] ]
//      \ CharacterClassEscape[+UnicodeMode]
// NOTE: The first two lines here are equivalent to CharacterClass.
#[derive(Debug, Clone, PartialEq, Eq)]
enum NestedClass {
    Class(Box<ClassContents>),
    NegatedClass(Box<ClassContents>),
    CharacterClassEscape(CharacterClassEscape),
}
impl NestedClass {
    fn parse(scanner: &Scanner) -> Option<(Self, usize)> {
        let mut new_scanner = scanner.clone();
        let ch = new_scanner.consume_any()?;
        if ch == '\\' {
            let (class, amt) = CharacterClassEscape::parse(&new_scanner, UnicodeMode::Allowed)?;
            new_scanner.read_idx += amt;
            Some((Self::CharacterClassEscape(class), new_scanner.read_idx - scanner.read_idx))
        } else if ch == '[' {
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
}

// ClassStringDisjunction ::
//      \q{ ClassStringDisjunctionContents }
#[derive(Debug, Clone, PartialEq, Eq)]
struct ClassStringDisjunction(Vec<ClassString>);
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
struct ClassString(Vec<u32>);
impl ClassString {
    fn parse(scanner: &Scanner) -> (Self, usize) {
        if let Some((string, amt)) = NonEmptyClassString::parse(scanner) {
            (Self(string.0), amt)
        } else {
            (Self(Vec::new()), 0)
        }
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
