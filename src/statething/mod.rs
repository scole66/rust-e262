#[derive(Debug)]
struct InnerState(String);

impl InnerState {
    pub fn parse(src: &str) -> Self {
        Self(String::from(src))
    }

    pub fn has(&self, needle: &str) -> bool {
        self.0 == needle
    }
}

#[derive(Debug)]
pub struct State(InnerState);

impl State {
    pub fn parse(src: &str) -> Self {
        Self(InnerState::parse(src))
    }

    pub fn has(&self, needle: &str) -> bool {
        self.0.has(needle)
    }
}

#[cfg(test)]
mod tests {
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
}
