#[derive(Debug)]
pub struct State(String);
impl State {
    pub fn parse(src: &str) -> Self {
        Self(String::from(src))
    }

    pub fn has(&self, needle: &str) -> bool {
        self.0 == needle
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
