use crate::regexp::*;
use ahash::AHashMap;
use anyhow::anyhow;
use std::collections::{BTreeMap, BTreeSet};
use std::sync::{LazyLock, RwLock};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub(crate) enum Direction {
    Forward,
    Backward,
}

const LINE_TERMINATORS: [u32; 4] = [0xa, 0xd, 0x2028, 0x2029];

#[derive(Debug, Copy, Clone)]
pub(crate) struct CaptureRange {
    pub(crate) start_index: usize,
    pub(crate) end_index: usize,
}

#[derive(Debug, Clone)]
pub(crate) struct MatchState {
    pub(crate) input: Rc<[u32]>,
    pub(crate) end_index: usize,
    pub(crate) captures: Vec<Option<CaptureRange>>,
}

pub(crate) type MatcherContinuation = Rc<dyn Fn(MatchState) -> Step>;
pub(crate) type Matcher = Rc<dyn Fn(MatchState, MatcherContinuation) -> Step>;
pub(crate) type PatternMatcher = Rc<dyn Fn(&[u32], usize) -> Option<MatchState>>;

pub(crate) struct RegExpData {
    pub(crate) original_source: JSString,
    pub(crate) original_flags: JSString,
    pub(crate) reg_exp_record: RegExpRecord,
    pub(crate) reg_exp_matcher: Option<PatternMatcher>,
}

impl std::fmt::Debug for RegExpData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("RegExpData")
            .field("original_source", &self.original_source)
            .field("original_flags", &self.original_flags)
            .field("reg_exp_record", &self.reg_exp_record)
            .field("reg_exp_matcher", &self.reg_exp_matcher.as_ref().map(|_| "compiled matcher"))
            .finish()
    }
}

pub(crate) enum Step {
    Done(Option<MatchState>),
    CallMatcher {
        matcher: Matcher,
        state: MatchState,
        continuation: MatcherContinuation,
    },
    CallContinuation {
        state: MatchState,
        continuation: MatcherContinuation,
    },
    PushChoice {
        fallback: Box<Step>,
        then_do: Box<Step>,
    },
    RunMatchAnyAlternatives {
        alternatives: Rc<Vec<Matcher>>,
        index: usize,
        state: MatchState,
        continuation: MatcherContinuation,
    },
    RepeatMatcher {
        matcher: Matcher,
        min: usize,
        max: Option<usize>,
        greedy: bool,
        state: MatchState,
        continuation: MatcherContinuation,
        p_index: usize,
        p_count: usize,
    },
    // Runs an assertion submatcher to completion as an isolated submatch.
    // The result is then handled by the trampoline, not by recursive Rust calls.
    PositiveLookaround {
        matcher: Matcher,
        state: MatchState,
        continuation: MatcherContinuation,
    },

    NegativeLookaround {
        matcher: Matcher,
        state: MatchState,
        continuation: MatcherContinuation,
    },

    MatchStringAlternatives {
        strings: Rc<[Rc<[u32]>]>,
        index: usize,
        state: MatchState,
        continuation: MatcherContinuation,
        direction: Direction,
    },
}

fn run(mut step: Step) -> Option<MatchState> {
    let mut choices = vec![];
    loop {
        step = match step {
            Step::Done(Some(state)) => return Some(state),
            Step::Done(None) => choices.pop()?,
            Step::CallMatcher { matcher, state, continuation } => matcher(state, continuation),
            Step::CallContinuation { state, continuation } => continuation(state),
            Step::PushChoice { fallback, then_do } => {
                choices.push(*fallback);
                *then_do
            }
            Step::RunMatchAnyAlternatives { alternatives, index, state, continuation } => {
                run_match_any_alternatives(&alternatives, index, state, continuation)
            }
            Step::RepeatMatcher { matcher, min, max, greedy, state, continuation, p_index, p_count } => {
                repeat_matcher_step(matcher, min, max, greedy, state, continuation, p_index, p_count)
            }
            Step::PositiveLookaround { matcher, state, continuation } => {
                let assertion_start = state.clone();

                let result =
                    run(Step::CallMatcher { matcher, state: state.clone(), continuation: identity_continuation() });

                match result {
                    Some(assertion_result) => {
                        let continued_state = MatchState {
                            input: assertion_start.input,
                            end_index: assertion_start.end_index,
                            captures: assertion_result.captures,
                        };

                        Step::CallContinuation { continuation, state: continued_state }
                    }
                    None => Step::Done(None),
                }
            }
            Step::NegativeLookaround { matcher, state, continuation } => {
                let result =
                    run(Step::CallMatcher { matcher, state: state.clone(), continuation: identity_continuation() });

                match result {
                    Some(_) => Step::Done(None),
                    None => Step::CallContinuation { continuation, state },
                }
            }
            Step::MatchStringAlternatives { strings, index, state, continuation, direction } => {
                match_string_alternatives_step(strings, index, state, continuation, direction)
            }
        }
    }
}

fn match_string_alternatives_step(
    strings: Rc<[Rc<[u32]>]>,
    index: usize,
    state: MatchState,
    continuation: MatcherContinuation,
    direction: Direction,
) -> Step {
    let Some(string) = strings.get(index).cloned() else {
        return Step::Done(None);
    };

    let Some(next_state) = match_string_at_state(&state, string.as_ref(), direction) else {
        return Step::MatchStringAlternatives { strings, index: index + 1, state, continuation, direction };
    };

    // If this string matches locally, try the caller's continuation.
    // If the continuation fails, fall back to the next string alternative.
    Step::PushChoice {
        then_do: Box::new(Step::CallContinuation { continuation: continuation.clone(), state: next_state }),
        fallback: Box::new(Step::MatchStringAlternatives { strings, index: index + 1, state, continuation, direction }),
    }
}

fn match_string_at_state(state: &MatchState, string: &[u32], direction: Direction) -> Option<MatchState> {
    let input = state.input.as_ref();
    let end_index = state.end_index;
    let len = string.len();

    let next_index = match direction {
        Direction::Forward => {
            if end_index + len > input.len() {
                return None;
            }

            for i in 0..len {
                if input[end_index + i] != string[i] {
                    return None;
                }
            }

            end_index + len
        }

        Direction::Backward => {
            if end_index < len {
                return None;
            }

            let start = end_index - len;

            for i in 0..len {
                if input[start + i] != string[i] {
                    return None;
                }
            }

            start
        }
    };

    Some(MatchState { input: state.input.clone(), end_index: next_index, captures: state.captures.clone() })
}

fn identity_continuation() -> MatcherContinuation {
    Rc::new(|state| Step::Done(Some(state)))
}

impl Pattern {
    fn run_matcher(matcher: &Matcher, capture_count: usize, input: &[u32], index: usize) -> Option<MatchState> {
        let continuation: MatcherContinuation = identity_continuation();
        let captures = vec![None; capture_count];
        let state = MatchState { input: Rc::from(input), end_index: index, captures };
        let first_step = Step::CallMatcher { matcher: matcher.clone(), state, continuation };
        run(first_step)
    }

    pub(crate) fn compile_pattern(&self, rer: &RegExpRecord, group_specifiers: &[&GroupSpecifier]) -> PatternMatcher {
        let matcher = self.0.compile_subpattern(rer, Direction::Forward, group_specifiers);
        let capture_count = rer.capturing_groups_count;

        Rc::new(move |input, index| Self::run_matcher(&matcher, capture_count, input, index))
    }
}

impl Disjunction {
    pub(crate) fn compile_subpattern(
        &self,
        rer: &RegExpRecord,
        direction: Direction,
        group_specifiers: &[&GroupSpecifier],
    ) -> Matcher {
        // CompileSubpattern for Disjunction.
        //
        // The spec grammar represents a disjunction as a recursive chain:
        //
        //     Disjunction :: Alternative
        //     Disjunction :: Alternative | Disjunction
        //
        // and defines compilation recursively:
        //
        //     m1 = CompileSubpattern(Alternative)
        //     m2 = CompileSubpattern(Disjunction)
        //     return MatchTwoAlternatives(m1, m2)
        //
        // Our parser stores the alternatives as a flat vector instead of a recursive
        // chain. Compile each Alternative to a Matcher, then build one matcher that
        // tries them in source order with the same left-to-right alternative
        // semantics as repeated MatchTwoAlternatives.
        let alternatives = self.0.iter().map(|alt| alt.compile_subpattern(rer, direction, group_specifiers));

        match_any_alternatives(alternatives)
    }
}

fn match_any_alternatives(alternatives: impl IntoIterator<Item = Matcher>) -> Matcher {
    // 1. Return a new matcher with parameters (x, c) that captures the alternatives and performs the following steps
    //    when called:
    //    a. Let k = 0.
    //    b. while k < the number of alternatives
    //       i. let m be alternatives[k].
    //       ii. let r be m(x, c).
    //       iii. if r is not failure, return r.
    //       iv. let k = k + 1.
    //    c. return failure
    let alts = Rc::new(alternatives.into_iter().collect::<Vec<_>>());
    Rc::new(move |state, continuation| run_match_any_alternatives(&alts, 0, state, continuation))
}

fn run_match_any_alternatives(
    alternatives: &Rc<Vec<Matcher>>,
    index: usize,
    state: MatchState,
    continuation: MatcherContinuation,
) -> Step {
    let Some(matcher) = alternatives.get(index).cloned() else {
        return Step::Done(None);
    };

    if index + 1 >= alternatives.len() {
        return Step::CallMatcher { matcher, state, continuation };
    }

    Step::PushChoice {
        fallback: Box::new(Step::RunMatchAnyAlternatives {
            alternatives: Rc::clone(alternatives),
            index: index + 1,
            state: state.clone(),
            continuation: continuation.clone(),
        }),
        then_do: Box::new(Step::CallMatcher { matcher, state, continuation }),
    }
}

impl Alternative {
    pub(crate) fn compile_subpattern(
        &self,
        rer: &RegExpRecord,
        direction: Direction,
        group_specifiers: &[&GroupSpecifier],
    ) -> Matcher {
        // CompileSubpattern for Alternative.
        //
        // The spec grammar represents an Alternative recursively:
        //
        //     Alternative :: [empty]
        //     Alternative :: Alternative Term
        //
        // and defines compilation in terms of EmptyMatcher for the empty case,
        // then repeated MatchSequence composition as each Term is added.
        //
        // Our parser stores an Alternative as a flat sequence of Terms. Compile
        // each Term to a Matcher, then compose the sequence in the requested
        // direction. If there are no terms, match_all_terms returns EmptyMatcher,
        // preserving the spec's Alternative :: [empty] behavior.
        let terms = self.0.iter().map(|term| term.compile_subpattern(rer, direction, group_specifiers));

        match_all_terms(terms, direction)
    }
}

fn empty_matcher() -> Matcher {
    // EmptyMatcher returns a matcher that, when called with (state, continuation),
    // succeeds at the current position by calling continuation(state).
    //
    // Since matchers are trampolined, "calling the continuation" is represented as
    // a Step rather than a direct Rust function call.
    Rc::new(move |state, continuation| Step::CallContinuation { continuation, state })
}

fn match_all_terms(terms: impl IntoIterator<Item = Matcher>, direction: Direction) -> Matcher {
    // Compose a non-empty list of term matchers into one matcher.
    //
    // The spec builds a chain of matchers, where each term's continuation runs
    // the next term. The last term receives the caller's continuation.
    //
    // Forward matching composes terms left-to-right. Backward matching composes
    // the same source terms right-to-left.
    //
    // Because matching is trampolined, each composed matcher returns the next
    // call as a Step instead of invoking the next matcher directly.
    let mut trms = terms.into_iter().collect::<Vec<_>>();

    if matches!(direction, Direction::Backward) {
        trms.reverse();
    }

    let Some(first) = trms.first().cloned() else {
        return empty_matcher();
    };

    let mut m = first;

    for term in trms.iter().skip(1) {
        let inner_m = m.clone();
        let inner_term = term.clone();

        m = Rc::new(move |state, continuation| {
            run_match_term_inner(inner_m.clone(), inner_term.clone(), state, continuation)
        });
    }

    m
}

fn run_match_term_inner(m1: Matcher, m2: Matcher, state: MatchState, continuation: MatcherContinuation) -> Step {
    // After m1 succeeds, run m2 with the original caller's continuation.
    let d: MatcherContinuation = Rc::new(move |y: MatchState| Step::CallMatcher {
        matcher: m2.clone(),
        state: y,
        continuation: continuation.clone(),
    });

    Step::CallMatcher { matcher: m1, state, continuation: d }
}

impl Term {
    pub(crate) fn compile_subpattern(
        &self,
        rer: &RegExpRecord,
        direction: Direction,
        group_specifiers: &[&GroupSpecifier],
    ) -> Matcher {
        // CompileSubpattern for Term.
        //
        // A Term is either an Assertion, a bare Atom, or an Atom followed by a
        // Quantifier. Assertions are independent of direction. Bare atoms compile
        // directly to their atom matcher.
        //
        // For quantified atoms, the spec compiles the Atom to a matcher `m`,
        // compiles the Quantifier to `{ min, max, greedy }`, computes the capture
        // range that must be cleared on each repetition, and returns a matcher
        // that invokes RepeatMatcher.
        //
        // Since this engine is trampolined, the returned matcher does not call
        // RepeatMatcher directly. It returns a Step describing that call, so the
        // trampoline loop can execute the repetition without growing the Rust
        // call stack.
        match &self.node {
            TermNode::Assertion(assertion) => {
                // Term :: Assertion
                //
                // Assertions do not consume input, and their compiled matcher is
                // independent of the matching direction.
                assertion.compile_assertion(rer, group_specifiers)
            }

            TermNode::Atom(atom, None) => {
                // Term :: Atom
                atom.compile_atom(rer, direction, group_specifiers)
            }

            TermNode::Atom(atom, Some(quantifier)) => {
                // Term :: Atom Quantifier
                let matcher = atom.compile_atom(rer, direction, group_specifiers);
                let quantifier = quantifier.compile_quantifier();

                debug_assert!(quantifier.max.is_none_or(|max| quantifier.min <= max));

                let p_index = self.count_left_capturing_parens_before();
                let p_count = atom.count_left_capturing_parens_within();

                Rc::new(move |state, continuation| Step::RepeatMatcher {
                    matcher: matcher.clone(),
                    min: quantifier.min,
                    max: quantifier.max,
                    greedy: quantifier.greedy,
                    state,
                    continuation,
                    p_index,
                    p_count,
                })
            }
        }
    }

    fn count_left_capturing_parens_before(&self) -> usize {
        // CountLeftCapturingParensBefore returns the number of capturing groups
        // in the enclosing pattern that occur before this term, plus groups that
        // contain this term. The parser computes and stores that value while
        // building the regexp AST.
        self.left_capturing_parens_before
    }
}

#[expect(clippy::too_many_arguments)]
fn repeater_continuation(
    y: MatchState,
    matcher: Matcher,
    min: usize,
    max: Option<usize>,
    greedy: bool,
    previous_state: &MatchState,
    continuation: MatcherContinuation,
    p_index: usize,
    p_count: usize,
) -> Step {
    // This is the continuation `d` from RepeatMatcher.
    //
    // It is called after one repetition of `matcher` succeeds. If the repetition
    // was allowed to match zero times and made no progress, fail to avoid an
    // infinite loop for patterns such as /(?:)*/.
    //
    // Otherwise, decrement the remaining repetition bounds and schedule another
    // RepeatMatcher step. This is the trampolined form of the spec's recursive
    // call to RepeatMatcher.
    if min == 0 && y.end_index == previous_state.end_index {
        return Step::Done(None);
    }

    Step::RepeatMatcher {
        matcher,
        min: min.saturating_sub(1),
        max: max.map(|max| max - 1),
        greedy,
        state: y,
        continuation,
        p_index,
        p_count,
    }
}

#[expect(clippy::too_many_arguments)]
fn repeat_matcher_step(
    matcher: Matcher,
    min: usize,
    max: Option<usize>,
    greedy: bool,
    state: MatchState,
    continuation: MatcherContinuation,
    p_index: usize,
    p_count: usize,
) -> Step {
    // RepeatMatcher(m, min, max, greedy, state, continuation, p_index, p_count)
    //
    // This follows the spec's RepeatMatcher operation, but returns the next
    // trampoline Step instead of directly invoking matchers and continuations.
    //
    // Greedy and non-greedy behavior is represented with PushChoice:
    //
    //   greedy:     try another repetition first; on failure, try continuation
    //   non-greedy: try continuation first; on failure, try another repetition
    //
    // This preserves the spec's backtracking order while keeping the native Rust
    // stack shallow.
    if max == Some(0) {
        return Step::CallContinuation { continuation, state };
    }

    let mut captures = state.captures.clone();

    for capture in captures.iter_mut().take(p_index + p_count).skip(p_index) {
        *capture = None;
    }

    let repeated_state = MatchState { input: state.input.clone(), end_index: state.end_index, captures };

    let previous_state = state.clone();

    let d: MatcherContinuation = Rc::new({
        let matcher = matcher.clone();
        let continuation = continuation.clone();

        move |y: MatchState| {
            repeater_continuation(
                y,
                matcher.clone(),
                min,
                max,
                greedy,
                &previous_state,
                continuation.clone(),
                p_index,
                p_count,
            )
        }
    });

    if min != 0 {
        return Step::CallMatcher { matcher, state: repeated_state, continuation: d };
    }

    if greedy {
        Step::PushChoice {
            then_do: Box::new(Step::CallMatcher { matcher, state: repeated_state, continuation: d }),
            fallback: Box::new(Step::CallContinuation { continuation, state }),
        }
    } else {
        Step::PushChoice {
            then_do: Box::new(Step::CallContinuation { continuation, state }),
            fallback: Box::new(Step::CallMatcher { matcher, state: repeated_state, continuation: d }),
        }
    }
}

impl Assertion {
    fn compile_assertion(&self, rer: &RegExpRecord, group_specifiers: &[&GroupSpecifier]) -> Matcher {
        match self {
            Assertion::Start => {
                let rer = rer.clone();

                Rc::new(move |state, continuation| {
                    let input = state.input.as_ref();
                    let e = state.end_index;

                    // `^` matches the start of input, or the position after a
                    // line terminator when multiline mode is enabled. The sticky
                    // flag does not change this behavior.
                    if e == 0 || rer.multiline == Lines::Multi && LINE_TERMINATORS.contains(&input[e - 1]) {
                        Step::CallContinuation { continuation, state }
                    } else {
                        Step::Done(None)
                    }
                })
            }

            Assertion::End => {
                let rer = rer.clone();

                Rc::new(move |state, continuation| {
                    let input = state.input.as_ref();
                    let e = state.end_index;
                    let input_length = input.len();

                    // `$` matches the end of input, or the position before a
                    // line terminator when multiline mode is enabled.
                    if e == input_length
                        || rer.multiline == Lines::Multi && e < input_length && LINE_TERMINATORS.contains(&input[e])
                    {
                        Step::CallContinuation { continuation, state }
                    } else {
                        Step::Done(None)
                    }
                })
            }

            Assertion::WordBoundary => {
                let rer = rer.clone();

                Rc::new(move |state, continuation| {
                    let input = state.input.as_ref();
                    let e = state.end_index;

                    // A word boundary exists when exactly one side of the
                    // current position is a word character.
                    let before = e > 0 && is_word_char(&rer, input, e - 1);
                    let after = is_word_char(&rer, input, e);

                    if before == after { Step::Done(None) } else { Step::CallContinuation { continuation, state } }
                })
            }

            Assertion::NotWordBoundary => {
                let rer = rer.clone();

                Rc::new(move |state, continuation| {
                    let input = state.input.as_ref();
                    let e = state.end_index;

                    // A non-boundary exists when both sides of the current
                    // position have the same word-character status.
                    let before = e > 0 && is_word_char(&rer, input, e - 1);
                    let after = is_word_char(&rer, input, e);

                    if before == after { Step::CallContinuation { continuation, state } } else { Step::Done(None) }
                })
            }

            Assertion::LookAhead(disjunction) => {
                // Positive lookahead checks the subpattern from the current
                // position without consuming input. If the assertion body
                // succeeds, its captures are kept but the original end_index is
                // restored before continuing.
                let matcher = disjunction.compile_subpattern(rer, Direction::Forward, group_specifiers);

                Rc::new(move |state, continuation| Step::PositiveLookaround {
                    matcher: matcher.clone(),
                    state,
                    continuation,
                })
            }

            Assertion::NegLookAhead(disjunction) => {
                // Negative lookahead succeeds only when the forward subpattern
                // fails at the current position.
                let matcher = disjunction.compile_subpattern(rer, Direction::Forward, group_specifiers);

                Rc::new(move |state, continuation| Step::NegativeLookaround {
                    matcher: matcher.clone(),
                    state,
                    continuation,
                })
            }

            Assertion::LookBehind(disjunction) => {
                // Positive lookbehind checks the subpattern ending at the
                // current position. The subpattern is compiled backward, but a
                // successful assertion still consumes no input.
                let matcher = disjunction.compile_subpattern(rer, Direction::Backward, group_specifiers);

                Rc::new(move |state, continuation| Step::PositiveLookaround {
                    matcher: matcher.clone(),
                    state,
                    continuation,
                })
            }

            Assertion::NegLookBehind(disjunction) => {
                // Negative lookbehind succeeds only when the backward subpattern
                // fails ending at the current position.
                let matcher = disjunction.compile_subpattern(rer, Direction::Backward, group_specifiers);

                Rc::new(move |state, continuation| Step::NegativeLookaround {
                    matcher: matcher.clone(),
                    state,
                    continuation,
                })
            }
        }
    }
}

struct QData {
    min: usize,
    max: Option<usize>,
    greedy: bool,
}

impl Quantifier {
    fn compile_quantifier(&self) -> QData {
        // Runtime Semantics: CompileQuantifier
        //
        // The syntax-directed operation CompileQuantifier takes no arguments and returns a Record with fields [[Min]]
        // (a non-negative integer), [[Max]] (a non-negative integer or +∞), and [[Greedy]] (a Boolean). It is defined
        // piecewise over the following productions:
        match self {
            Quantifier::Greedy(quantifier_prefix) => {
                // Quantifier :: QuantifierPrefix
                // 1. Let qp be CompileQuantifierPrefix of QuantifierPrefix.
                // 2. Return the Record { [[Min]]: qp.[[Min]], [[Max]]: qp.[[Max]], [[Greedy]]: true }.
                let qp = quantifier_prefix.compile_quantifier_prefix();
                QData { min: qp.min, max: qp.max, greedy: true }
            }
            Quantifier::Restrained(quantifier_prefix) => {
                // Quantifier :: QuantifierPrefix ?
                // 1. Let qp be CompileQuantifierPrefix of QuantifierPrefix.
                // 2. Return the Record { [[Min]]: qp.[[Min]], [[Max]]: qp.[[Max]], [[Greedy]]: false }.
                let qp = quantifier_prefix.compile_quantifier_prefix();
                QData { min: qp.min, max: qp.max, greedy: false }
            }
        }
    }
}

struct MinMax {
    min: usize,
    max: Option<usize>,
}
impl QuantifierPrefix {
    fn compile_quantifier_prefix(&self) -> MinMax {
        // Runtime Semantics: CompileQuantifierPrefix
        //
        // The syntax-directed operation CompileQuantifierPrefix takes no arguments and returns a Record with fields
        // [[Min]] (a non-negative integer) and [[Max]] (a non-negative integer or +∞). It is defined piecewise over the
        // following productions:
        match self {
            QuantifierPrefix::ZeroOrMore => {
                // QuantifierPrefix :: *
                //      1. Return the Record { [[Min]]: 0, [[Max]]: +∞ }.
                MinMax { min: 0, max: None }
            }
            QuantifierPrefix::OneOrMore => {
                // QuantifierPrefix :: +
                //      1. Return the Record { [[Min]]: 1, [[Max]]: +∞ }.
                MinMax { min: 1, max: None }
            }
            QuantifierPrefix::ZeroOrOne => {
                // QuantifierPrefix :: ?
                //      1. Return the Record { [[Min]]: 0, [[Max]]: 1 }.
                MinMax { min: 0, max: Some(1) }
            }
            QuantifierPrefix::Exactly(digits) => {
                // QuantifierPrefix :: { DecimalDigits }
                //      1. Let i be the MV of DecimalDigits (see 12.9.3).
                //      2. Return the Record { [[Min]]: i, [[Max]]: i }.
                let i = usize::try_from(*digits).expect("u32 should fit in usize");
                MinMax { min: i, max: Some(i) }
            }
            QuantifierPrefix::XOrMore(val) => {
                // QuantifierPrefix :: { DecimalDigits ,}
                //      1. Let i be the MV of DecimalDigits.
                //      2. Return the Record { [[Min]]: i, [[Max]]: +∞ }.
                let i = usize::try_from(*val).expect("u32 should fit in usize");
                MinMax { min: i, max: None }
            }
            QuantifierPrefix::Range(low, high) => {
                // QuantifierPrefix :: { DecimalDigits , DecimalDigits }
                //      1. Let i be the MV of the first DecimalDigits.
                //      2. Let j be the MV of the second DecimalDigits.
                //      3. Return the Record { [[Min]]: i, [[Max]]: j }.
                let min = usize::try_from(*low).expect("u32 should fit in a usize");
                let max = usize::try_from(*high).expect("u32 should fit in a usize");
                MinMax { min, max: Some(max) }
            }
        }
    }
}

fn group_continuation(
    state: &MatchState,
    outer_state: &MatchState,
    continuation: MatcherContinuation,
    direction: Direction,
    paren_index: usize,
) -> Step {
    // Capturing group continuation.
    //
    // This runs after the group's body has matched. It records the range covered
    // by the group, using the original entry position from `outer_state` and the
    // current position from `state`.
    //
    // Forward groups capture [outer_end, inner_end). Backward groups capture
    // [inner_end, outer_end). After updating the capture slot, continue matching
    // from the group's current end position.
    let mut captures = state.captures.clone();

    let input = outer_state.input.clone();
    let xe = outer_state.end_index;
    let ye = state.end_index;

    let range = match direction {
        Direction::Forward => {
            debug_assert!(xe <= ye);
            CaptureRange { start_index: xe, end_index: ye }
        }
        Direction::Backward => {
            debug_assert!(ye <= xe);
            CaptureRange { start_index: ye, end_index: xe }
        }
    };

    captures[paren_index] = Some(range);

    let next_state = MatchState { input, end_index: ye, captures };
    Step::CallContinuation { continuation, state: next_state }
}

fn group_matcher(x: MatchState, c: MatcherContinuation, direction: Direction, m: Matcher, paren_index: usize) -> Step {
    // Group matcher.
    //
    // The group body matcher `m` runs at the current state `x`. If it succeeds,
    // its continuation records the group's capture range and then resumes the
    // caller's continuation `c`.
    //
    // Since execution is trampolined, this function returns a Step describing the
    // call to `m` instead of invoking `m` directly.
    let outer_state = x.clone();

    let d: MatcherContinuation =
        Rc::new(move |y| group_continuation(&y, &outer_state, c.clone(), direction, paren_index));

    Step::CallMatcher { matcher: m, state: x, continuation: d }
}

fn string_alternatives_matcher(strings: impl IntoIterator<Item = Vec<u32>>, direction: Direction) -> Matcher {
    let mut strings = strings.into_iter().map(|s| Rc::<[u32]>::from(s.into_boxed_slice())).collect::<Vec<_>>();

    // Spec tries multi-character strings in descending length.
    // Keep whatever ordering rules your caller already applied if it has already
    // sorted according to the spec.
    strings.sort_by_key(|s| std::cmp::Reverse(s.len()));

    let strings: Rc<[Rc<[u32]>]> = Rc::from(strings.into_boxed_slice());

    Rc::new(move |state, continuation| Step::MatchStringAlternatives {
        strings: strings.clone(),
        index: 0,
        state,
        continuation,
        direction,
    })
}

impl Atom {
    pub(crate) fn compile_atom(
        &self,
        rer: &RegExpRecord,
        direction: Direction,
        group_specifiers: &[&GroupSpecifier],
    ) -> Matcher {
        // Runtime Semantics: CompileAtom
        //
        // The syntax-directed operation CompileAtom takes arguments rer (a RegExp Record) and direction (forward or
        // backward) and returns a Matcher.
        //
        // It is defined piecewise over the following productions:
        match &self.node {
            AtomNode::PatternCharacter(ch) => {
                // Atom :: PatternCharacter
                // 1. Let ch be the character matched by PatternCharacter.
                // 2. Let A be a one-element CharSet containing the character ch.
                // 3. Return CharacterSetMatcher(rer, A, false, direction).
                let a = CharSet::from(*ch);
                character_set_matcher(rer, &a, false, direction)
            }
            AtomNode::Dot => {
                // Atom :: .
                // 1. Let A be AllCharacters(rer).
                // 2. If rer.[[DotAll]] is not true, then
                //    a. Remove from A all characters corresponding to a code point on the right-hand side of the
                //       LineTerminator production.
                // 3. Return CharacterSetMatcher(rer, A, false, direction).
                let mut a = all_characters(rer);
                if !rer.dot_all {
                    for ch in LINE_TERMINATORS {
                        a.remove(ch);
                    }
                }
                character_set_matcher(rer, &a, false, direction)
            }
            AtomNode::AtomEscape(atom_escape) => atom_escape.compile_atom(rer, direction, group_specifiers),
            AtomNode::CharacterClass(character_class) => {
                // Atom :: CharacterClass
                // 1. Let cc be CompileCharacterClass of CharacterClass with argument rer.
                let cc = character_class.compile_character_class(rer);
                // 2. Let cs be cc.[[CharSet]].
                let cs = cc.char_set;
                // 3. If rer.[[UnicodeSets]] is false, or if every CharSetElement of cs consists of a single character
                //    (including if cs is empty), return CharacterSetMatcher(rer, cs, cc.[[Invert]], direction).
                if rer.unicode_sets == UnicodeSetsMode::Denied || cs.all_single_or_empty() {
                    return character_set_matcher(rer, &cs, cc.invert, direction);
                }

                // UnicodeSets mode allows string-valued CharSetElements from \q{...}. The spec
                // describes these by building a list of matchers:
                //
                //   1. multi-character strings, tried in descending length
                //   2. all single characters as one CharacterSetMatcher
                //   3. EmptyMatcher, if the class contains the empty string
                //
                // Then it combines that list with MatchTwoAlternatives.
                //
                // Our matcher execution is trampolined, so we avoid building nested
                // MatchSequence chains for each string. Instead, compile the multi-character
                // elements into one dedicated fixed-string-alternatives matcher, then combine
                // that with the ordinary single-character matcher and optional empty matcher.
                debug_assert!(!cc.invert);

                let mut strings = cs
                    .iter()
                    .filter_map(|element| {
                        if let CharSetElement::Multiple(element) = element {
                            let chars = element.as_slice();

                            if chars.len() > 1 { Some(chars.to_vec()) } else { None }
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<_>>();

                strings.sort_by_key(|s| std::cmp::Reverse(s.len()));

                let mut matchers = Vec::new();

                if !strings.is_empty() {
                    matchers.push(string_alternatives_matcher(strings, direction));
                }

                let singles = cs
                    .iter()
                    .filter_map(|element| if let CharSetElement::Single(ch) = element { Some(ch) } else { None })
                    .collect::<CharSet>();

                matchers.push(character_set_matcher(rer, &singles, false, direction));

                if cs.contains_string(&[]) {
                    matchers.push(empty_matcher());
                }

                match_any_alternatives(matchers)
            }
            AtomNode::GroupedDisjunction { group_specifier: _, disjunction } => {
                // Atom :: ( GroupSpecifieropt Disjunction )
                // 1. Let m be CompileSubpattern of Disjunction with arguments regexpRecord and direction.
                // 2. Let parenIndex be CountLeftCapturingParensBefore(Atom).
                // 3. Return a new Matcher with parameters (x, c) that captures direction, m, and parenIndex and
                //    performs the following steps when called:
                //    a. Assert: x is a MatchState.
                //    b. Assert: c is a MatcherContinuation.
                //    c. Let d be a new MatcherContinuation with parameters (y) that captures x, c, direction, and
                //       parenIndex and performs the following steps when called:
                //       i. Assert: y is a MatchState.
                //       ii. Let cap be a copy of y.[[Captures]].
                //       iii. Let input be x.[[Input]].
                //       iv. Let xe be x.[[EndIndex]].
                //       v. Let ye be y.[[EndIndex]].
                //       vi. If direction is forward, then
                //           1. Assert: xe ≤ ye.
                //           2. Let r be the CaptureRange { [[StartIndex]]: xe, [[EndIndex]]: ye }.
                //       vii. Else,
                //            1. Assert: direction is backward.
                //            2. Assert: ye ≤ xe.
                //            3. Let r be the CaptureRange { [[StartIndex]]: ye, [[EndIndex]]: xe }.
                //       viii. Set cap[parenIndex + 1] to r.
                //       ix. Let z be the MatchState { [[Input]]: input, [[EndIndex]]: ye, [[Captures]]: cap }.
                //       x. Return c(z).
                //    d. Return m(x, d).
                let m = disjunction.compile_subpattern(rer, direction, group_specifiers);
                let paren_index = self.count_left_capturing_parens_before();
                Rc::new(move |x, c| group_matcher(x, c, direction, m.clone(), paren_index))
            }
            AtomNode::UnGroupedDisjunction((add, remove), disjunction) => {
                let empty_remove = RegularExpressionModifiers::default();
                let remove = remove.as_ref().unwrap_or(&empty_remove);
                let new_rer = rer.update_modifiers(add, remove);
                disjunction.compile_subpattern(&new_rer, direction, group_specifiers)
            }
        }
    }
}

struct CharClassResult {
    char_set: CharSet,
    invert: bool,
}

impl CharacterClass {
    fn compile_character_class(&self, rer: &RegExpRecord) -> CharClassResult {
        match self {
            CharacterClass::Selection(class_contents) => {
                let a = class_contents.compile_to_char_set(rer);
                CharClassResult { char_set: a, invert: false }
            }
            CharacterClass::Negation(class_contents) => {
                let a = class_contents.compile_to_char_set(rer);
                if rer.unicode_sets == UnicodeSetsMode::Allowed {
                    CharClassResult { char_set: a.character_complement(rer), invert: false }
                } else {
                    CharClassResult { char_set: a, invert: true }
                }
            }
        }
    }
}

impl ClassContents {
    fn compile_to_char_set(&self, rer: &RegExpRecord) -> CharSet {
        match self {
            ClassContents::None => CharSet::default(),
            ClassContents::NonemptyClassRanges(node) => node.compile_to_char_set(rer),
            ClassContents::ClassSetExpression(node) => node.compile_to_char_set(rer),
        }
    }
}

impl NonemptyClassRanges {
    fn compile_to_char_set(&self, rer: &RegExpRecord) -> CharSet {
        match self {
            NonemptyClassRanges::List(class_atoms) => {
                let mut ranges = CharSet::default();
                for atom in class_atoms {
                    let atom_set = atom.compile_to_char_set(rer);
                    ranges.union_with(&atom_set);
                }
                ranges
            }
            NonemptyClassRanges::Range { head, tail, content } => {
                let mut ranges = CharSet::default();
                for atom in &head[0..head.len() - 1] {
                    let atom_set = atom.compile_to_char_set(rer);
                    ranges.union_with(&atom_set);
                }
                let range_start = head[head.len() - 1].compile_to_char_set(rer);
                let range_end = tail.compile_to_char_set(rer);
                let range = character_range(&range_start, &range_end);
                ranges.union_with(&range);
                let final_content = content.compile_to_char_set(rer);
                ranges.union_with(&final_content);
                ranges
            }
        }
    }
}

impl ClassSetExpression {
    fn compile_to_char_set(&self, rer: &RegExpRecord) -> CharSet {
        match self {
            ClassSetExpression::Union(node) => node.compile_to_char_set(rer),
            ClassSetExpression::Intersection(node) => node.compile_to_char_set(rer),
            ClassSetExpression::Subtraction(node) => node.compile_to_char_set(rer),
        }
    }
}

impl ClassUnion {
    fn compile_to_char_set(&self, rer: &RegExpRecord) -> CharSet {
        match self {
            ClassUnion::Range { range, union: None } => range.compile_to_char_set(rer),
            ClassUnion::Range { range, union: Some(union) } => {
                let mut char_set = range.compile_to_char_set(rer);
                let other_set = union.compile_to_char_set(rer);
                char_set.union_with(&other_set);
                char_set
            }
            ClassUnion::Operand { operand, union: None } => operand.compile_to_char_set(rer),
            ClassUnion::Operand { operand, union: Some(union) } => {
                let mut char_set = operand.compile_to_char_set(rer);
                let other_set = union.compile_to_char_set(rer);
                char_set.union_with(&other_set);
                char_set
            }
        }
    }
}

impl ClassSetOperand {
    fn compile_to_char_set(&self, rer: &RegExpRecord) -> CharSet {
        match self {
            ClassSetOperand::NestedClass(node) => node.compile_to_char_set(rer),
            ClassSetOperand::ClassStringDisjunction(node) => {
                let cs = node.compile_to_char_set();
                maybe_simple_case_folding(rer, cs)
            }
            ClassSetOperand::ClassSetCharacter(ch) => {
                let cs = CharSet::from(*ch);
                maybe_simple_case_folding(rer, cs)
            }
        }
    }
}

impl ClassStringDisjunction {
    fn compile_to_char_set(&self) -> CharSet {
        self.0.iter().map(ClassString::compile_class_set_string).collect::<CharSet>()
    }
}

impl ClassString {
    fn compile_class_set_string(&self) -> Vec<u32> {
        self.0.clone()
    }
}

impl NestedClass {
    fn compile_to_char_set(&self, rer: &RegExpRecord) -> CharSet {
        match self {
            NestedClass::Class(node) => node.compile_to_char_set(rer),
            NestedClass::NegatedClass(node) => {
                let char_set = node.compile_to_char_set(rer);
                char_set.character_complement(rer)
            }
            NestedClass::CharacterClassEscape(node) => node.compile_to_char_set(rer),
        }
    }
}

impl ClassSetRange {
    fn compile_to_char_set(self, rer: &RegExpRecord) -> CharSet {
        let cs = CharSet::from((self.first, self.last));
        maybe_simple_case_folding(rer, cs)
    }
}

impl ClassIntersection {
    #[expect(unused_variables)]
    fn compile_to_char_set(&self, rer: &RegExpRecord) -> CharSet {
        todo!()
    }
}

impl ClassSubtraction {
    #[expect(unused_variables)]
    fn compile_to_char_set(&self, rer: &RegExpRecord) -> CharSet {
        todo!()
    }
}

impl ClassAtom {
    fn compile_to_char_set(&self, rer: &RegExpRecord) -> CharSet {
        // Runtime Semantics: CompileToCharSet
        // The syntax-directed operation CompileToCharSet takes argument rer (a RegExp Record) and returns a CharSet.
        match self {
            ClassAtom::Char(ch) => {
                // ClassAtom :: -
                //  1. Return the CharSet containing the single character - U+002D (HYPHEN-MINUS).
                //
                // ClassAtomNoDash :: SourceCharacter but not one of \ or ] or -
                //  1. Return the CharSet containing the character matched by SourceCharacter.
                //
                // ClassEscape ::
                //      b
                //      -
                //      CharacterEscape
                //  1. Let cv be the CharacterValue of this ClassEscape.
                //  2. Let c be the character whose character value is cv.
                //  3. Return the CharSet containing the single character c.
                CharSet::from(*ch)
            }
            ClassAtom::Class(character_class_escape) => character_class_escape.compile_to_char_set(rer),
        }
    }
}

impl CharacterClassEscape {
    pub(crate) fn compile_to_char_set(&self, rer: &RegExpRecord) -> CharSet {
        // Runtime Semantics: CompileToCharSet
        // The syntax-directed operation CompileToCharSet takes argument rer (a RegExp Record) and returns a CharSet.
        match self {
            CharacterClassEscape::Digit => {
                // CharacterClassEscape :: d
                // 1. Return the ten-element CharSet containing the characters 0, 1, 2, 3, 4, 5, 6, 7, 8, and 9.
                let mut set = CharSet::default();
                set.insert_range(u32::from('0'), u32::from('9'));
                set
            }
            CharacterClassEscape::NotDigit => {
                // CharacterClassEscape :: D
                // 1. Let S be the CharSet returned by CharacterClassEscape :: d .
                // 2. Return CharacterComplement(rer, S).
                let mut set = CharSet::default();
                set.insert_range(u32::from('0'), u32::from('9'));
                set.character_complement(rer)
            }
            CharacterClassEscape::Whitespace => {
                // CharacterClassEscape :: s
                // 1. Return the CharSet containing all characters corresponding to a code point on the right-hand side
                //    of the WhiteSpace or LineTerminator productions.
                CharSet::whitespace()
            }
            CharacterClassEscape::NotWhitespace => {
                // 1. Let charSet be the CharSet returned by CharacterClassEscape :: s .
                // 2. Return CharacterComplement(regexpRecord, charSet).
                CharSet::whitespace().character_complement(rer)
            }
            CharacterClassEscape::Word => {
                // CharacterClassEscape :: w
                // 1. Return MaybeSimpleCaseFolding(rer, WordCharacters(rer)).
                maybe_simple_case_folding(rer, word_characters(rer))
            }
            CharacterClassEscape::NotWord => {
                // CharacterClassEscape :: W
                // 1. Let charSet be the CharSet returned by CharacterClassEscape :: w .
                // 2. Return CharacterComplement(regexpRecord, charSet).
                maybe_simple_case_folding(rer, word_characters(rer)).character_complement(rer)
            }
            CharacterClassEscape::Property(ve) => ve.compile_to_char_set(rer),
            CharacterClassEscape::NotProperty(ve) => ve.compile_to_char_set(rer).character_complement(rer),
        }
    }
}

fn character_set_matcher(rer: &RegExpRecord, set: &CharSet, invert: bool, direction: Direction) -> Matcher {
    let set = set.canonicalized_for_matching(rer);
    let rer = rer.clone();

    Rc::new(move |state, continuation| run_character_set_matcher(state, continuation, &rer, &set, invert, direction))
}

fn run_character_set_matcher(
    state: MatchState,
    continuation: MatcherContinuation,
    rer: &RegExpRecord,
    char_set: &CharSet,
    invert: bool,
    direction: Direction,
) -> Step {
    let MatchState { input, end_index, captures } = state;

    // Character set matching consumes exactly one input character. Forward
    // matching reads at `end_index` and advances; backward matching reads the
    // character before `end_index` and retreats.
    let next_index = match direction {
        Direction::Forward => end_index + 1,

        Direction::Backward => {
            if end_index == 0 {
                return Step::Done(None);
            }

            end_index - 1
        }
    };

    // Forward matching can run off the end of the input. Backward matching was
    // already guarded against moving before the start.
    if next_index > input.len() {
        return Step::Done(None);
    }

    // This is the input position actually tested: `end_index` for forward
    // matching, and `end_index - 1` for backward matching.
    let matched_index = end_index.min(next_index);
    let ch = input[matched_index];

    // Character sets are canonicalized when their matcher is created. Canonicalize
    // the input character too, so ignore-case matching compares canonical values
    // on both sides.
    let canonical_ch = rer.canonicalize(ch);
    let found = char_set.contains(canonical_ch);

    // Inverted character classes succeed exactly when the character was not in
    // the set.
    if invert == found {
        return Step::Done(None);
    }

    // Character set matching does not modify captures; it only moves the current
    // input position. Since execution is trampolined, success schedules the
    // continuation instead of calling it directly.
    Step::CallContinuation { continuation, state: MatchState { input, end_index: next_index, captures } }
}

fn unicode_match_property(rer: &RegExpRecord, property: &str) -> anyhow::Result<&'static str> {
    if rer.unicode_sets == UnicodeSetsMode::Allowed
        && let Some(&item) = UnicodePropertyValueExpression::BINARY_UNARY_PROPERTIES.iter().find(|p| **p == property)
    {
        Ok(item)
    } else if let Some(item) =
        UnicodePropertyValueExpression::TABLE_65_CANON.iter().find(|c| c.name == property || c.alias == Some(property))
    {
        Ok(item.name)
    } else if let Some(item) = UnicodePropertyValueExpression::PROPERTY_POSSIBILITIES
        .iter()
        .find(|cat| cat.property_name == property || cat.alias_name == property)
    {
        Ok(item.property_name)
    } else {
        Err(anyhow!("Invalid Unicode property name"))
    }
}

fn unicode_match_property_value(property: &str, value: &str) -> anyhow::Result<&'static str> {
    let property = if property == "Script_Extensions" { "Script" } else { property };
    let ppv_list = UnicodePropertyValueExpression::PROPERTY_POSSIBILITIES
        .iter()
        .find(|x| x.property_name == property)
        .ok_or_else(|| anyhow!("Invalid Unicode property name"))?
        .potential_values;
    ppv_list
        .iter()
        .find(|&ppv| ppv.name == value || ppv.alias == value)
        .map(|pv| pv.alias)
        .ok_or_else(|| anyhow!("Invalid Unicode property value"))
}

type UnicodeRangeTable = &'static [(u32, u32)];
type UnicodePropertyValueTable = (&'static str, UnicodeRangeTable);
type UnicodePropertyTable = (&'static str, &'static [UnicodePropertyValueTable]);
impl UnicodePropertyValueExpression {
    const PROPERTY_TABLE: &[UnicodePropertyTable] = &[
        ("General_Category", gc::BY_NAME),
        ("Script", sc::BY_NAME),
        ("Script_Extensions", scx::BY_NAME),
        ("", property_bool::BY_NAME),
    ];

    fn get_property_charset(name: &str, value: &str) -> anyhow::Result<CharSet> {
        // Look up the generated Unicode range table for the normalized property name, then the normalized property
        // value within that table.

        // There are a handful of properties that are not present in the unicode tables but instead are simply defined
        // in Unicode Technical Report 18, Section 1.2.5: General Category Property, under the heading "Core
        // Properties".
        if name.is_empty() {
            if value == "Any" {
                // The special property `Any` matches `[\u{0}-\u{10FFFF}]`.
                return Ok(CharSet::from((0, 0x10_ffff)));
            }
            if value == "ASCII" {
                // The special property `ASCII` matches `[\u{0}-\u{7F}]`,
                return Ok(CharSet::from((0, 0x7f)));
            }
            if value == "Assigned" {
                // The special property `Assigned` matches `\P{Unassigned}`.
                let unassigned = Self::property_charset("General_Category", "Unassigned").expect("known good args");
                let all = CharSet::from((0, 0x10_ffff));
                let assigned = CharSet::difference(&all, &unassigned);
                return Ok(assigned);
            }
            if value == "Changes_When_NFKC_Casefolded" {
                // This is in the spec, but not in our generated datafiles.
                return Ok(CharSet::from(CHANGES_WHEN_NFKC_CASEFOLDED));
            }
            if value == "Emoji_Keycap_Sequence" {
                let mut cs = CharSet::default();
                for s in EMOJI_KEYCAP_SEQUENCE {
                    cs.insert_string(*s);
                }
                return Ok(cs);
            }
        } else if (name == "Script" || name == "Script_Extensions") && value == "Unknown" {
            let mut set = CharSet::from((0, 0x10_ffff));
            let table = Self::PROPERTY_TABLE
                .iter()
                .find(|&(table_name, _)| *table_name == name)
                .expect("Script & Script_Extensions tables should exist");
            let (name, table) = table;
            for script_name in table.iter().map(|&(x, _)| x) {
                let known = Self::property_charset(name, script_name).expect("known good args");
                set = CharSet::difference(&set, &known);
            }
            return Ok(set);
        }

        // Binary properties are stored under the empty property name because the property itself is the lookup key and
        // the implied value is `True`.
        let table = Self::PROPERTY_TABLE
            .iter()
            .find(|&(table_name, _)| *table_name == name)
            .ok_or_else(|| anyhow!("bad name"))?
            .1;

        let cs = table.iter().find(|&(value_name, _)| *value_name == value).ok_or_else(|| anyhow!("bad value"))?.1;

        Ok(CharSet::from(cs))
    }

    fn property_charset(name: &'static str, value: &'static str) -> anyhow::Result<CharSet> {
        static CACHE: LazyLock<RwLock<AHashMap<(&'static str, &'static str), CharSet>>> =
            LazyLock::new(|| RwLock::new(AHashMap::new()));

        let key = (name, value);

        // Most property escapes reuse a small set of Unicode tables. Cache the converted CharSet so repeated parses do
        // not rebuild the same ranges.
        if let Some(cs) =
            CACHE.read().expect("property charset cache lock should not be poisoned during read").get(&key)
        {
            return Ok(cs.clone());
        }

        // Build outside the write lock so other threads can keep reading cached properties while this table is
        // converted.
        let cs = Self::get_property_charset(name, value)?;

        let mut cache = CACHE.write().expect("property charset cache lock should not be poisoned during write");

        // Another thread may have inserted the same table while we were building
        // it; keep that value if it exists.
        let cs = cache.entry(key).or_insert_with(|| cs).clone();

        Ok(cs)
    }

    pub(crate) fn compile_to_char_set(&self, rer: &RegExpRecord) -> CharSet {
        match self {
            UnicodePropertyValueExpression::NameValue { name, value: val } => {
                // Explicit property escapes, such as `\p{Script=Greek}`, are normalized through the Unicode alias
                // tables before indexing the generated data.
                let property = unicode_match_property(rer, &name.0).expect("names should have been validated");
                let value = unicode_match_property_value(property, &val.0).expect("values should have been validated");

                Self::property_charset(property, value).expect("name/value pair should have generated Unicode data")
            }

            UnicodePropertyValueExpression::Lone(s) => {
                if let Ok(val) = unicode_match_property_value("General_Category", &s.0) {
                    // A lone value that names a General_Category value is treated as `General_Category=value`, for
                    // example `\p{Lu}`.
                    Self::property_charset("General_Category", val)
                        .expect("General_Category value should have generated Unicode data")
                } else {
                    // Otherwise, a lone name denotes a binary Unicode property whose implied value is `True`, for
                    // example `\p{Alphabetic}`.
                    let property = unicode_match_property(rer, &s.0).expect("property name should have been validated");

                    let cs = Self::property_charset("", property)
                        .expect("binary property should have generated Unicode data");

                    // UnicodeSets ignore-case mode folds property sets after lookup, matching the spec's
                    // MaybeSimpleCaseFolding step.
                    maybe_simple_case_folding(rer, cs)
                }
            }
        }
    }
}

fn all_characters(rer: &RegExpRecord) -> CharSet {
    if rer.unicode_sets == UnicodeSetsMode::Allowed && rer.case == Case::Unimportant {
        // Return the CharSet containing all Unicode code points c that do not have a Simple Case Folding mapping (that
        // is, scf(c)=c).
        static DOES_NOT_FOLD: LazyLock<CharSet> = LazyLock::new(|| {
            let mut set = CharSet::default();
            for pair in DOES_NOT_FOLD_RANGES {
                let &(start, end) = pair;
                set.insert_range(start, end);
            }
            set
        });
        DOES_NOT_FOLD.clone()
    } else if rer.has_either_unicode_flag() {
        let mut set = CharSet::default();
        set.insert_range(0, 0x10_FFFF);
        set
    } else {
        let mut set = CharSet::default();
        set.insert_range(0, 0xFFFF);
        set
    }
}

fn word_characters(rer: &RegExpRecord) -> CharSet {
    // WordCharacters ( rer )
    //
    // The abstract operation WordCharacters takes argument rer (a RegExp Record) and returns a CharSet. Returns a
    // CharSet containing the characters considered "word characters" for the purposes of \b, \B, \w, and \W. It
    // performs the following steps when called:
    //
    // 1. Let basicWordChars be the CharSet containing every character in the ASCII word characters.
    // 2. Let extraWordChars be the CharSet containing all characters c such that c is not in basicWordChars but
    //    Canonicalize(rer, c) is in basicWordChars.
    // 3. Assert: extraWordChars is empty unless HasEitherUnicodeFlag(rer) is true and rer.[[IgnoreCase]] is true.
    // 4. Return the union of basicWordChars and extraWordChars.
    static BASIC_WORD_CHARS: LazyLock<CharSet> = LazyLock::new(|| {
        let mut basic_word_chars = CharSet::default();
        basic_word_chars.insert_range(u32::from('A'), u32::from('Z'));
        basic_word_chars.insert_range(u32::from('a'), u32::from('z'));
        basic_word_chars.insert_range(u32::from('0'), u32::from('9'));
        basic_word_chars.insert(u32::from('_'));
        basic_word_chars
    });

    static UNICODE_IGNORE_CASE_WORD_CHARS: LazyLock<CharSet> = LazyLock::new(|| {
        let mut set = BASIC_WORD_CHARS.clone();

        // In Unicode-aware ignore-case matching, word-character tests use
        // Canonicalize before checking the ASCII word-character set. These
        // non-ASCII code points case-fold to ASCII letters.
        set.insert(0x017F); // LATIN SMALL LETTER LONG S -> "s"
        set.insert(0x212A); // KELVIN SIGN -> "k"

        set
    });

    if rer.has_either_unicode_flag() && rer.case == Case::Unimportant {
        // This is the Unicode-ish + Case::Unimportant case
        UNICODE_IGNORE_CASE_WORD_CHARS.clone()
    } else {
        BASIC_WORD_CHARS.clone()
    }
}

fn is_word_char(rer: &RegExpRecord, input: &[u32], e: usize) -> bool {
    // IsWordChar ( regexpRecord, input, e )
    // The abstract operation IsWordChar takes arguments regexpRecord (a RegExp Record), input (a List of characters),
    // and e (an integer) and returns a Boolean. It performs the following steps when called:
    //
    // 1. Let inputLength be the number of elements in input.
    let input_length = input.len();
    // 2. If e = -1 or e = inputLength, return false.
    if e == input_length {
        false
    } else {
        // 3. Let c be the character input[e].
        let c = input[e];
        // 4. If WordCharacters(regexpRecord) contains c, return true.
        word_characters(rer).contains_char(c)
        // 5. Return false.
    }
}

fn maybe_simple_case_folding(rer: &RegExpRecord, a: CharSet) -> CharSet {
    // Implements: 22.2.2.9.5 MaybeSimpleCaseFolding

    // Simple case folding only applies to UnicodeSets regexps in ignore-case
    // mode. In all other modes, the parsed set is used unchanged.
    if rer.unicode_sets != UnicodeSetsMode::Allowed || rer.case == Case::Significant {
        return a;
    }

    let mut folded = CharSet::default();

    // In UnicodeSets mode, a CharSet element can be a sequence, not just a
    // single code point. Fold each code point in the sequence independently,
    // preserving the element's sequence length.
    for element in a {
        let folded_element = element.iter().map(casefold_simple).collect::<Vec<_>>();

        // `insert_string` should normalize one-code-point sequences into the
        // ordinary character ranges, while preserving empty and multi-code-point
        // string elements for UnicodeSets matching.
        folded.insert_string(folded_element);
    }

    folded
}

fn character_range(set_a: &CharSet, set_b: &CharSet) -> CharSet {
    // CharacterRange ( A, B )
    //
    // The abstract operation CharacterRange takes arguments A (a CharSet) and B (a CharSet) and returns a CharSet. It
    // performs the following steps when called:
    //
    // 1. Assert: A and B each contain exactly one character.
    // 2. Let a be the one character in CharSet A.
    // 3. Let b be the one character in CharSet B.
    // 4. Let i be the character value of character a.
    // 5. Let j be the character value of character b.
    // 6. Assert: i ≤ j.
    // 7. Return the CharSet containing all characters with a character value in the inclusive interval from i to j.
    fn get_only_char(set: &CharSet) -> u32 {
        let mut iter = set.ranges();
        let (a, spare) = iter.next().expect("there should be at least one range");
        assert_eq!(a, spare, "the first item in the set should be one char");
        assert!(iter.next().is_none(), "there should be only one char in this set");
        assert!(set.strings.is_empty(), "there should be no UnicodeSets collections in this set");
        a
    }

    let a = get_only_char(set_a);
    let b = get_only_char(set_b);

    assert!(a <= b, "ranges should only go forward");

    let mut result = CharSet::default();
    result.insert_range(a, b);

    result
}

pub(crate) enum CharSetElement {
    Single(u32),
    Multiple(Vec<u32>),
}
impl CharSetElement {
    pub(crate) fn iter(&self) -> CharSetElementIter<'_> {
        match self {
            Self::Single(ch) => CharSetElementIter::Single(Some(*ch)),
            Self::Multiple(chars) => CharSetElementIter::Multiple(chars.iter().copied()),
        }
    }
}

pub(crate) enum CharSetElementIter<'a> {
    Single(Option<u32>),
    Multiple(std::iter::Copied<std::slice::Iter<'a, u32>>),
}

impl Iterator for CharSetElementIter<'_> {
    type Item = u32;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Single(ch) => ch.take(),
            Self::Multiple(iter) => iter.next(),
        }
    }
}

impl<'a> IntoIterator for &'a CharSetElement {
    type Item = u32;
    type IntoIter = CharSetElementIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

#[derive(Debug, Default, Clone)]
pub(crate) struct CharSet {
    // Inclusive ranges of single RegExp characters.
    ranges: BTreeMap<u32, u32>,

    // Multi-character set elements used by UnicodeSets (/v). A single
    // character may be represented either here as a one-element sequence or in
    // `ranges`; prefer `ranges` for single characters.
    strings: BTreeSet<Vec<u32>>,
}

impl From<char> for CharSet {
    fn from(value: char) -> Self {
        let item = u32::from(value);
        let mut set = CharSet::default();
        set.insert(item);
        set
    }
}

impl From<u32> for CharSet {
    fn from(value: u32) -> Self {
        let mut set = CharSet::default();
        set.insert(value);
        set
    }
}

impl FromIterator<Vec<u32>> for CharSet {
    fn from_iter<T: IntoIterator<Item = Vec<u32>>>(iter: T) -> Self {
        let mut cs = CharSet::default();
        for s in iter {
            cs.insert_string(s);
        }
        cs
    }
}

impl FromIterator<u32> for CharSet {
    fn from_iter<T: IntoIterator<Item = u32>>(iter: T) -> Self {
        let mut cs = CharSet::default();
        for ch in iter {
            cs.insert(ch);
        }
        cs
    }
}

impl From<(u32, u32)> for CharSet {
    fn from(value: (u32, u32)) -> Self {
        let mut set = CharSet::default();
        set.insert_range(value.0, value.1);
        set
    }
}

impl From<&[(u32, u32)]> for CharSet {
    fn from(value: &[(u32, u32)]) -> Self {
        let mut set = CharSet::default();
        for &(low, high) in value {
            set.insert_range(low, high);
        }
        set
    }
}

impl CharSet {
    fn contains(&self, x: u32) -> bool {
        self.ranges.range(..=x).next_back().is_some_and(|(&start, &end)| start <= x && x <= end)
    }

    pub(crate) fn contains_char(&self, ch: u32) -> bool {
        self.contains(ch)
    }

    pub(crate) fn contains_string(&self, s: &[u32]) -> bool {
        match s {
            [ch] => self.contains(*ch),
            _ => self.strings.contains(s),
        }
    }

    fn insert(&mut self, x: u32) {
        self.insert_range(x, x);
    }

    fn insert_range(&mut self, mut start: u32, mut end: u32) {
        if start > end {
            std::mem::swap(&mut start, &mut end);
        }

        // Merge with a previous overlapping/adjacent range if present
        if let Some((&s, &e)) = self.ranges.range(..=start).next_back()
            && e.saturating_add(1) >= start
        {
            start = start.min(s);
            end = end.max(e);
            self.ranges.remove(&s);
        }

        // Merge with following overlapping/adjacent ranges
        loop {
            let next = self.ranges.range(start..).next().map(|(&s, &e)| (s, e));
            match next {
                Some((s, e)) if s <= end.saturating_add(1) => {
                    end = end.max(e);
                    self.ranges.remove(&s);
                }
                _ => break,
            }
        }

        self.ranges.insert(start, end);
    }

    pub(crate) fn insert_string(&mut self, s: impl Into<Vec<u32>>) {
        let s = s.into();

        if let [ch] = s.as_slice() {
            self.insert(*ch);
        } else {
            self.strings.insert(s);
        }
    }

    fn remove(&mut self, x: u32) -> bool {
        let Some((&start, &end)) = self.ranges.range(..=x).next_back() else {
            return false;
        };

        if x > end {
            return false;
        }

        self.ranges.remove(&start);

        match (start == x, end == x) {
            // range was exactly [x, x]
            (true, true) => {}

            // removing first element: [x, end] -> [x+1, end]
            (true, false) => {
                self.ranges.insert(x + 1, end);
            }

            // removing last element: [start, x] -> [start, x-1]
            (false, true) => {
                self.ranges.insert(start, x - 1);
            }

            // removing from middle: [start, end] -> [start, x-1] and [x+1, end]
            (false, false) => {
                self.ranges.insert(start, x - 1);
                self.ranges.insert(x + 1, end);
            }
        }

        true
    }

    fn union_with(&mut self, other: &Self) {
        for (&start, &end) in &other.ranges {
            self.insert_range(start, end);
        }
        for s in &other.strings {
            self.insert_string(s.clone());
        }
    }

    /// Iterate over stored inclusive ranges as `(start, end)`.
    fn ranges(&self) -> impl Iterator<Item = (u32, u32)> + '_ {
        self.ranges.iter().map(|(&start, &end)| (start, end))
    }

    /// Returns all values in `a` that are not present in `s`.
    pub(crate) fn difference(a: &Self, s: &Self) -> Self {
        let mut out = Self::default();

        for (a_start, a_end) in a.ranges() {
            let mut cursor = a_start;

            // Any S-range that starts after a_end cannot overlap.
            // Any S-range that ends before a_start also cannot overlap.
            for (&s_start, &s_end) in s.ranges.range(..=a_end) {
                if s_end < a_start {
                    continue;
                }

                // If there's a gap before this S-range, keep it.
                if cursor < s_start {
                    out.ranges.insert(cursor, s_start - 1);
                }

                // Advance cursor past the subtracted region.
                if s_end == u32::MAX {
                    cursor = u32::MAX;
                    break;
                }

                cursor = cursor.max(s_end.saturating_add(1));

                // If we've consumed all of A's range, we're done with it.
                if cursor > a_end {
                    break;
                }
            }

            // Any tail after the last overlapping S-range remains.
            if cursor <= a_end {
                out.ranges.insert(cursor, a_end);
            }
        }

        out
    }

    fn character_complement(&self, rer: &RegExpRecord) -> Self {
        // CharacterComplement ( rer, S )
        //
        // The abstract operation CharacterComplement takes arguments rer (a RegExp Record) and S (a CharSet) and
        // returns a CharSet. It performs the following steps when called:
        // 1. Let A be AllCharacters(rer).
        // 2. Return the CharSet containing the CharSetElements of A which are not also CharSetElements of S.
        let all = all_characters(rer);
        Self::difference(&all, self)
    }

    fn all_single_or_empty(&self) -> bool {
        self.strings.iter().all(|v| v.len() <= 1)
    }

    fn whitespace() -> Self {
        // 1. Return the CharSet containing all characters corresponding to a code point on the right-hand side
        //    of the WhiteSpace or LineTerminator productions.
        let mut set = Self::default();
        set.insert(9); // <TAB>
        set.insert_range(11, 12); // <VT> & <FF>
        set.insert(0xfeff); // <ZWNBSP>
        set.insert(32); // <space>
        set.insert(0xa0); // <NO-BREAK SPACE>
        set.insert(0x1680); // <OGHAM SPACE MARK>
        set.insert_range(0x2000, 0x200a);
        set.insert(0x202f); // <NARROW NO-BREAK SPACE>
        set.insert(0x205f); // <MEDIUM MATHEMATICAL SPACE>
        set.insert(0x3000); // <IDEOGRAPHIC SPACE>
        for ch in LINE_TERMINATORS {
            set.insert(ch);
        }
        set
    }

    pub(crate) fn iter(&self) -> CharSetIter<'_> {
        CharSetIter { ranges: self.ranges.iter(), current: None, strings: self.strings.iter() }
    }

    pub(crate) fn canonicalized_for_matching(&self, rer: &RegExpRecord) -> Self {
        if rer.case == Case::Significant {
            return self.clone();
        }

        let mappings = if rer.has_either_unicode_flag() { SIMPLE_CASE_FOLD_RANGES } else { LEGACY_CANONICALIZE_RANGES };

        let mut result = CharSet::default();

        for (&first, &last) in &self.ranges {
            result.add_canonicalized_range(first, last, mappings);
        }

        // If your CharSet has UnicodeSets string elements, fold those
        // character-by-character too. Single-character strings may normalize
        // into ordinary character ranges through `insert_string`.
        for string in &self.strings {
            result.insert_string(string.iter().map(|&ch| rer.canonicalize(ch)).collect::<Vec<_>>());
        }

        result
    }

    fn add_canonicalized_range(&mut self, first: u32, last: u32, mappings: &'static [CanonicalizeRange]) {
        let mut cursor = first;

        for mapping in mappings {
            if mapping.last < cursor {
                continue;
            }

            if mapping.first > last {
                break;
            }

            let overlap_first = cursor.max(mapping.first);
            let overlap_last = last.min(mapping.last);

            if cursor < overlap_first {
                // No canonicalization mapping applies in this gap, so these code
                // points canonicalize to themselves.
                self.insert_range(cursor, overlap_first - 1);
            }

            // This source subrange maps linearly to another subrange.
            let mapped_first = mapping.map(overlap_first);
            let mapped_last = mapping.map(overlap_last);

            self.insert_range(mapped_first.min(mapped_last), mapped_first.max(mapped_last));

            cursor = overlap_last.saturating_add(1);

            if cursor > last {
                return;
            }
        }

        // Any remaining tail has no mapping and canonicalizes to itself.
        if cursor <= last {
            self.insert_range(cursor, last);
        }
    }
}

pub(crate) struct CharSetIter<'a> {
    ranges: std::collections::btree_map::Iter<'a, u32, u32>,
    current: Option<(u32, u32)>,
    strings: std::collections::btree_set::Iter<'a, Vec<u32>>,
}

impl Iterator for CharSetIter<'_> {
    type Item = CharSetElement;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some((next, end)) = self.current {
                if next <= end {
                    self.current = next.checked_add(1).map(|next| (next, end));
                    return Some(CharSetElement::Single(next));
                }

                self.current = None;
            }

            if let Some((&start, &end)) = self.ranges.next() {
                self.current = Some((start, end));
            } else {
                return self.strings.next().map(|v| CharSetElement::Multiple(v.clone()));
            }
        }
    }
}

impl<'a> IntoIterator for &'a CharSet {
    type Item = CharSetElement;
    type IntoIter = CharSetIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

pub(crate) struct CharSetIntoIter {
    ranges: std::collections::btree_map::IntoIter<u32, u32>,
    current: Option<(u32, u32)>,
    strings: std::collections::btree_set::IntoIter<Vec<u32>>,
}

impl IntoIterator for CharSet {
    type Item = CharSetElement;
    type IntoIter = CharSetIntoIter;

    fn into_iter(self) -> Self::IntoIter {
        CharSetIntoIter { ranges: self.ranges.into_iter(), current: None, strings: self.strings.into_iter() }
    }
}

impl Iterator for CharSetIntoIter {
    type Item = CharSetElement;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some((next, end)) = self.current {
                if next <= end {
                    self.current = next.checked_add(1).map(|next| (next, end));
                    return Some(CharSetElement::Single(next));
                }

                self.current = None;
            }

            if let Some((start, end)) = self.ranges.next() {
                self.current = Some((start, end));
            } else {
                return self.strings.next().map(|v| CharSetElement::Multiple(v.clone()));
            }
        }
    }
}

fn run_backreference_match(
    state: MatchState,
    continuation: MatcherContinuation,
    rer: &RegExpRecord,
    backreferences: &[usize],
    direction: Direction,
) -> Step {
    let input = state.input;
    let captures = state.captures;
    let end_index = state.end_index;

    // Find the first referenced capture that participated in the match. If none
    // did, the backreference matches the empty string and simply continues.
    let capture_range = backreferences.iter().find_map(|&n| captures[n - 1].as_ref());

    let Some(capture_range) = capture_range else {
        return Step::CallContinuation { continuation, state: MatchState { input, end_index, captures } };
    };

    let rs = capture_range.start_index;
    let re = capture_range.end_index;
    let len = re - rs;

    // A backreference matches the captured text at the current position. Forward
    // matching advances by the capture length; backward matching retreats by it.
    let next_index = if direction == Direction::Forward {
        end_index + len
    } else {
        if end_index < len {
            return Step::Done(None);
        }
        end_index - len
    };

    if next_index > input.len() {
        return Step::Done(None);
    }

    // Compare the captured text with the candidate text, after applying regexp
    // canonicalization for ignore-case matching.
    let candidate_start = end_index.min(next_index);
    let matched =
        (0..len).all(|idx| rer.canonicalize(input[rs + idx]) == rer.canonicalize(input[candidate_start + idx]));
    if !matched {
        return Step::Done(None);
    }
    Step::CallContinuation { continuation, state: MatchState { input, end_index: next_index, captures } }
}

fn backreference_matcher(rer: &RegExpRecord, backreferences: &[usize], direction: Direction) -> Matcher {
    let rer = rer.clone();
    let backrefs = Vec::from(backreferences);

    Rc::new(move |state, continuation| {
        run_backreference_match(state, continuation, &rer, backrefs.as_slice(), direction)
    })
}
impl AtomEscape {
    pub(crate) fn compile_atom(
        &self,
        rer: &RegExpRecord,
        direction: Direction,
        group_specifiers: &[&GroupSpecifier],
    ) -> Matcher {
        // Runtime Semantics: CompileAtom
        //
        // The syntax-directed operation CompileAtom takes arguments rer (a RegExp Record) and direction (forward or
        // backward) and returns a Matcher.
        match self {
            AtomEscape::DecimalEscape(decimal_escape) => {
                // AtomEscape :: DecimalEscape
                // 1. Let n be the CapturingGroupNumber of DecimalEscape.
                let n = decimal_escape.capturing_group_number();
                // 2. Assert: n ≤ regexpRecord.[[CapturingGroupsCount]].
                // 3. Return BackreferenceMatcher(regexpRecord, « n », direction).
                backreference_matcher(rer, &[n], direction)
                // Note 3
                // An escape sequence of the form \ followed by a non-zero decimal number n matches the result of the nth set of capturing parentheses (22.2.2.1). It is an error if the regular expression has fewer than n capturing parentheses. If the regular expression has n or more capturing parentheses but the nth one is undefined because it has not captured anything, then the backreference always succeeds.
            }
            AtomEscape::CharacterClassEscape(character_class_escape) => {
                // AtomEscape :: CharacterClassEscape
                // 1. Let cs be CompileToCharSet of CharacterClassEscape with argument rer.
                // 2. If rer.[[UnicodeSets]] is false, or if every CharSetElement of cs consists of a single character
                //    (including if cs is empty), return CharacterSetMatcher(rer, cs, false, direction).
                // 3. Let lm be an empty List of Matchers.
                // 4. For each CharSetElement s in cs containing more than 1 character, iterating in descending order of
                //    length, do
                //    a. Let cs2 be a one-element CharSet containing the last code point of s.
                //    b. Let m2 be CharacterSetMatcher(rer, cs2, false, direction).
                //    c. For each code point c1 in s, iterating backwards from its second-to-last code point, do
                //       i. Let cs1 be a one-element CharSet containing c1.
                //       ii. Let m1 be CharacterSetMatcher(rer, cs1, false, direction).
                //       iii. Set m2 to MatchSequence(m1, m2, direction).
                //    d. Append m2 to lm.
                // 5. Let singles be the CharSet containing every CharSetElement of cs that consists of a single
                //    character.
                // 6. Append CharacterSetMatcher(rer, singles, false, direction) to lm.
                // 7. If cs contains the empty sequence of characters, append EmptyMatcher() to lm.
                // 8. Let m2 be the last Matcher in lm.
                // 9. For each Matcher m1 of lm, iterating backwards from its second-to-last element, do
                //    a. Set m2 to MatchTwoAlternatives(m1, m2).
                // 10. Return m2.
                let cs = character_class_escape.compile_to_char_set(rer);
                if rer.unicode_sets == UnicodeSetsMode::Denied || cs.all_single_or_empty() {
                    return character_set_matcher(rer, &cs, false, direction);
                }
                todo!()
            }
            AtomEscape::CharacterEscape(character_escape) => {
                // AtomEscape :: CharacterEscape
                // 1. Let cv be the CharacterValue of CharacterEscape.
                // 2. Let ch be the character whose character value is cv.
                // 3. Let A be a one-element CharSet containing the character ch.
                // 4. Return CharacterSetMatcher(rer, A, false, direction).
                let cv = character_escape.character_value();
                let a = CharSet::from(cv);
                character_set_matcher(rer, &a, false, direction)
            }
            AtomEscape::GroupName(group_name) => {
                let matching_group_specifiers = group_specifiers_that_match(group_specifiers, group_name);
                let mut paren_indices = vec![];
                for group_specifier_idx in matching_group_specifiers {
                    let paren_index = group_specifiers[group_specifier_idx].count_left_capturing_parens_before();
                    paren_indices.push(paren_index + 1);
                }
                backreference_matcher(rer, &paren_indices, direction)
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct CanonicalizeRange {
    pub(crate) first: u32,
    pub(crate) last: u32,
    pub(crate) delta: i32,
}

impl CanonicalizeRange {
    #[expect(dead_code)]
    pub(crate) fn contains(self, ch: u32) -> bool {
        self.first <= ch && ch <= self.last
    }

    pub(crate) fn map(self, ch: u32) -> u32 {
        let abs = self.delta.unsigned_abs();
        if self.delta >= 0 { ch + abs } else { ch - abs }
    }
}

#[expect(dead_code)]
pub(crate) fn compress_mappings(mut mappings: Vec<(u32, u32)>) -> Vec<CanonicalizeRange> {
    mappings.sort_unstable();

    let mut ranges: Vec<CanonicalizeRange> = Vec::new();

    for (from, to) in mappings {
        if from == to {
            continue;
        }

        let delta = i64::from(to) - i64::from(from);
        let delta = i32::try_from(delta).expect("case mapping delta should fit in i32");

        match ranges.last_mut() {
            Some(last)
                if last.last + 1 == from
                    && last.delta == delta
                    && i64::from(last.last) + i64::from(last.delta) + 1 == i64::from(to) =>
            {
                last.last = from;
            }
            _ => ranges.push(CanonicalizeRange { first: from, last: from, delta }),
        }
    }

    ranges
}

#[expect(dead_code)]
pub(crate) fn legacy_canonicalize_for_table(ch: u32) -> u32 {
    let Some(ch_as_char) = char::from_u32(ch) else {
        return ch;
    };

    let upper = ch_as_char.to_uppercase().collect::<String>();
    let upper = JSString::from(upper);

    // Legacy RegExp canonicalization ignores mappings that expand to multiple
    // UTF-16 code units.
    let [cu] = upper.as_slice() else {
        return ch;
    };

    // Legacy non-Unicode ignore-case must not map non-ASCII characters into
    // ASCII. This is what keeps /[a-z]/i from matching ſ and K.
    if ch >= 128 && *cu < 128 {
        return ch;
    }

    u32::from(*cu)
}

// # Emoji_Keycap_Sequence
//
// 0023 FE0F 20E3; Emoji_Keycap_Sequence        ; keycap: \x{23}                                                 # E0.6   [1] (#️⃣)
// 002A FE0F 20E3; Emoji_Keycap_Sequence        ; keycap: *                                                      # E2.0   [1] (*️⃣)
// 0030 FE0F 20E3; Emoji_Keycap_Sequence        ; keycap: 0                                                      # E0.6   [1] (0️⃣)
// 0031 FE0F 20E3; Emoji_Keycap_Sequence        ; keycap: 1                                                      # E0.6   [1] (1️⃣)
// 0032 FE0F 20E3; Emoji_Keycap_Sequence        ; keycap: 2                                                      # E0.6   [1] (2️⃣)
// 0033 FE0F 20E3; Emoji_Keycap_Sequence        ; keycap: 3                                                      # E0.6   [1] (3️⃣)
// 0034 FE0F 20E3; Emoji_Keycap_Sequence        ; keycap: 4                                                      # E0.6   [1] (4️⃣)
// 0035 FE0F 20E3; Emoji_Keycap_Sequence        ; keycap: 5                                                      # E0.6   [1] (5️⃣)
// 0036 FE0F 20E3; Emoji_Keycap_Sequence        ; keycap: 6                                                      # E0.6   [1] (6️⃣)
// 0037 FE0F 20E3; Emoji_Keycap_Sequence        ; keycap: 7                                                      # E0.6   [1] (7️⃣)
// 0038 FE0F 20E3; Emoji_Keycap_Sequence        ; keycap: 8                                                      # E0.6   [1] (8️⃣)
// 0039 FE0F 20E3; Emoji_Keycap_Sequence        ; keycap: 9                                                      # E0.6   [1] (9️⃣)
const EMOJI_KEYCAP_SEQUENCE: &[&[u32; 3]; 12] = &[
    &[0x0023, 0xFE0F, 0x20E3],
    &[0x002A, 0xFE0F, 0x20E3],
    &[0x0030, 0xFE0F, 0x20E3],
    &[0x0031, 0xFE0F, 0x20E3],
    &[0x0032, 0xFE0F, 0x20E3],
    &[0x0033, 0xFE0F, 0x20E3],
    &[0x0034, 0xFE0F, 0x20E3],
    &[0x0035, 0xFE0F, 0x20E3],
    &[0x0036, 0xFE0F, 0x20E3],
    &[0x0037, 0xFE0F, 0x20E3],
    &[0x0038, 0xFE0F, 0x20E3],
    &[0x0039, 0xFE0F, 0x20E3],
];
