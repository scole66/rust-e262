use super::*;
//use super::parse::*;
//use crate::tests::*;
//use test_case::test_case;

mod class_set_character {
    use super::*;
    use test_case::test_case;

    #[test_case("gg" => Some((ClassSetCharacter::SourceCharacter(103), ScannerMutation { new_idx: 1, new_paren: 0 })); "unadorned source")]
    #[test_case("\\b" => Some((ClassSetCharacter::LetterB, ScannerMutation { new_idx: 2, new_paren: 0 })); "letter b")]
    #[test_case("\\@abc" => Some((ClassSetCharacter::ClassSetReservedPunctuator(ClassSetReservedPunctuator(64)), ScannerMutation { new_idx: 2, new_paren: 0 })); "reserved punct")]
    fn parse(text: &str) -> Option<(ClassSetCharacter, ScannerMutation)> {
        let text = text.chars().map(u32::from).collect::<Vec<_>>();
        let scanner = Scanner::new(&text);

        ClassSetCharacter::parse(&scanner)
    }
}

//mod pattern {
//    use super::*;
//
//    #[test]
//    fn parse() {
//        let scanner = Scanner::new(r"blue[a-z0-9]{78,90}.+(?:not-captured)$");
//        let result = Pattern::parse(&scanner, UnicodeMode::Allowed, UnicodeSetsMode::Denied, NamedCaptureGroups::Denied);
//
//        println!("{result:#?}");
//        assert!(false);
//    }
//}

#[expect(clippy::unnecessary_wraps)]
fn ssu(s: &str, u: ScannerMutation) -> Option<(String, ScannerMutation)> {
    Some((s.to_string(), u))
}

mod term {
    use super::*;
    use test_case::test_case;

    #[test_case("a*" => ssu("a*", ScannerMutation { new_idx: 2, new_paren: 0 }); "with star quantifier")]
    #[test_case("a{4}" => ssu("a{4}", ScannerMutation { new_idx: 4, new_paren: 0 }); "with exact quantifier")]
    #[test_case("a{212,}" => ssu("a{212,}", ScannerMutation { new_idx: 7, new_paren: 0 }); "with or-more quantifier")]
    #[test_case("a{212,1863}" => ssu("a{212,1863}", ScannerMutation { new_idx: 11, new_paren: 0 }); "with range quantifier")]
    #[test_case("^" => ssu("^", ScannerMutation { new_idx: 1, new_paren: 0 }); "assertion")]
    #[test_case("abc" => ssu("a", ScannerMutation { new_idx: 1, new_paren: 0 }); "atom with no quantifier")]
    #[test_case("???" => None; "Not a valid Term")]
    fn parse(src: &str) -> Option<(String, ScannerMutation)> {
        let src = src.chars().map(u32::from).collect::<Vec<_>>();
        let scanner = Scanner::new(&src);
        Term::parse(&scanner, UnicodeMode::Allowed, UnicodeSetsMode::Allowed, NamedCaptureGroups::Allowed)
            .map(|(t, s)| (t.to_string(), s))
    }
}
