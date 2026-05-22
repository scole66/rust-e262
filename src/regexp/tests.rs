use super::*;
use test_case::test_case;

#[test_case(false, Case::Significant, 'a' as u32 => 'a' as u32; "case-sensitive ascii lowercase unchanged")]
#[test_case(false, Case::Significant, 'A' as u32 => 'A' as u32; "case-sensitive ascii uppercase unchanged")]
#[test_case(true, Case::Significant, 'a' as u32 => 'a' as u32; "unicode case-sensitive lowercase unchanged")]
#[test_case(true, Case::Significant, 0x017F => 0x017F; "unicode case-sensitive long s unchanged")]
#[test_case(false, Case::Unimportant, 'a' as u32 => 'A' as u32; "legacy ignore-case ascii lowercase uppercases")]
#[test_case(false, Case::Unimportant, 'A' as u32 => 'A' as u32; "legacy ignore-case ascii uppercase unchanged")]
#[test_case(false, Case::Unimportant, 'z' as u32 => 'Z' as u32; "legacy ignore-case ascii z uppercases")]
#[test_case(false, Case::Unimportant, '1' as u32 => '1' as u32; "legacy ignore-case non-letter unchanged")]
#[test_case(false, Case::Unimportant, 0x00DF => 0x00DF; "legacy ignore-case multi-code-point uppercase mapping ignored")]
#[test_case(false, Case::Unimportant, 0x017F => 0x017F; "legacy ignore-case non-ascii must not map to ascii")]
#[test_case(false, Case::Unimportant, 0x212A => 0x212A; "legacy ignore-case kelvin sign must not map to ascii")]
#[test_case(false, Case::Unimportant, 0x03C9 => 0x03A9; "legacy ignore-case greek small omega uppercases")]
#[test_case(false, Case::Unimportant, 0x2126 => 0x2126; "legacy ignore-case ohm sign uppercases to itself")]
#[test_case(false, Case::Unimportant, 0xD800 => 0xD800; "legacy ignore-case lone surrogate unchanged")]
#[test_case(true, Case::Unimportant, 'A' as u32 => 'a' as u32; "unicode ignore-case ascii uppercase casefolds")]
#[test_case(true, Case::Unimportant, 'a' as u32 => 'a' as u32; "unicode ignore-case ascii lowercase unchanged")]
#[test_case(true, Case::Unimportant, 0x017F => 's' as u32; "unicode ignore-case long s folds to ascii s")]
#[test_case(true, Case::Unimportant, 0x212A => 'k' as u32; "unicode ignore-case kelvin sign folds to ascii k")]
#[test_case(true, Case::Unimportant, 0x2126 => 0x03C9; "unicode ignore-case ohm sign folds to small omega")]
#[test_case(true, Case::Unimportant, 0x03A9 => 0x03C9; "unicode ignore-case capital omega folds to small omega")]
#[test_case(true, Case::Unimportant, 0x00DF => 0x00DF; "unicode ignore-case sharp s simple fold stays single code point")]
fn canonicalize(unicode_ish: bool, case: Case, ch: u32) -> u32 {
    super::canonicalize(unicode_ish, case, ch)
}
