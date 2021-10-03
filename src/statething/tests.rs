use super::*;

#[test]
fn state_has_true() {
    let s = State::parse("value");
    assert!(s.has("value"))
}

#[test]
fn state_has_false() {
    let s = State::parse("value");
    assert!(!s.has("missing"))
}
