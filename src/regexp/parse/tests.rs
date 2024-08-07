use super::*;
//use super::parse::*;
//use crate::tests::*;
//use test_case::test_case;

mod class_set_character {
    use super::*;
    use test_case::test_case;

    #[test_case("gg" => Some((ClassSetCharacter::SourceCharacter(103), 1)); "unadorned source")]
    #[test_case("\\b" => Some((ClassSetCharacter::LetterB, 2)); "letter b")]
    #[test_case("\\@abc" => Some((ClassSetCharacter::ClassSetReservedPunctuator(ClassSetReservedPunctuator(64)), 2)); "reserved punct")]
    fn parse(text: &str) -> Option<(ClassSetCharacter, usize)> {
        let scanner = Scanner::new(text);

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

mod term {
    use super::*;
    use test_case::test_case;

    #[test_case("a*" => Some((Term::Atom(Atom::PatternCharacter('a'), Some(Quantifier::Greedy(QuantifierPrefix::ZeroOrMore))), 2)); "with star quantifier")]
    #[test_case("a{4}" => Some((Term::Atom(Atom::PatternCharacter('a'), Some(Quantifier::Greedy(QuantifierPrefix::Exactly(4)))), 4)); "with exact quantifier")]
    #[test_case("a{212,}" => Some((Term::Atom(Atom::PatternCharacter('a'), Some(Quantifier::Greedy(QuantifierPrefix::XOrMore(212)))), 7)); "with or-more quantifier")]
    #[test_case("a{212,1863}" => Some((Term::Atom(Atom::PatternCharacter('a'), Some(Quantifier::Greedy(QuantifierPrefix::Range(212, 1863)))), 11)); "with range quantifier")]
    #[test_case("^" => Some((Term::Assertion(Assertion::Start), 1)); "assertion")]
    #[test_case("abc" => Some((Term::Atom(Atom::PatternCharacter('a'), None), 1)); "atom with no quantifier")]
    #[test_case("???" => None; "Not a valid Term")]
    fn parse(src: &str) -> Option<(Term, usize)> {
        let scanner = Scanner::new(src);
        Term::parse(&scanner, UnicodeMode::Allowed, UnicodeSetsMode::Allowed, NamedCaptureGroups::Allowed)
    }
}
