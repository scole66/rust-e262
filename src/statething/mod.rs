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
struct State(InnerState);

impl State {
    pub fn parse(src: &str) -> Self {
        Self(InnerState::parse(src))
    }

    pub fn has(&self, needle: &str) -> bool {
        self.0.has(needle)
    }
}

#[cfg(test)]
mod tests;
