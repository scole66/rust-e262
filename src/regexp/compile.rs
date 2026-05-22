#![expect(dead_code)]
use crate::regexp::*;
use std::collections::{BTreeMap, BTreeSet};
use std::sync::LazyLock;

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
    pub(crate) input: Vec<u32>,
    pub(crate) end_index: usize,
    pub(crate) captures: Vec<Option<CaptureRange>>,
}

pub(crate) type MatcherContinuation = Rc<dyn Fn(MatchState) -> Option<MatchState>>;
pub(crate) type Matcher = Rc<dyn Fn(MatchState, MatcherContinuation) -> Option<MatchState>>;
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

impl Pattern {
    #[expect(clippy::unnecessary_wraps)]
    fn identity(y: MatchState) -> Option<MatchState> {
        Some(y)
    }

    fn run_matcher(matcher: &Matcher, capture_count: usize, input: &[u32], index: usize) -> Option<MatchState> {
        let continuation: MatcherContinuation = Rc::new(Self::identity);
        let captures = vec![None; capture_count];
        let state = MatchState { input: Vec::from(input), end_index: index, captures };
        matcher.as_ref()(state, continuation)
    }

    pub(crate) fn compile_pattern(&self, rer: &RegExpRecord) -> PatternMatcher {
        let matcher = self.0.compile_subpattern(rer, Direction::Forward);
        let capture_count = rer.capturing_groups_count;

        Rc::new(move |input, index| Self::run_matcher(&matcher, capture_count, input, index))
    }
}

impl Disjunction {
    pub(crate) fn compile_subpattern(&self, rer: &RegExpRecord, direction: Direction) -> Matcher {
        // The syntax-directed operation CompileSubpattern takes arguments rer (a RegExp Record) and direction (forward
        // or backward) and returns a Matcher.
        // Disjunction :: Alternative | Disjunction
        // 1. Let m1 be CompileSubpattern of Alternative with arguments rer and direction.
        // 2. Let m2 be CompileSubpattern of Disjunction with arguments rer and direction.
        // 3. Return MatchTwoAlternatives(m1, m2).
        //
        // Note: Since we transformed this chain into a vector, this needs a bit of modification.
        let mut iterator = self.0.iter().map(|alt| alt.compile_subpattern(rer, direction));
        match_any_alternatives(&mut iterator)
    }
}

fn match_any_alternatives(alternatives: &mut impl Iterator<Item = Matcher>) -> Matcher {
    // 1. Return a new matcher with parameters (x, c) that captures the alternatives and performs the following steps
    //    when called:
    //    a. Let k = 0.
    //    b. while k < the number of alternatives
    //       i. let m be alternatives[k].
    //       ii. let r be m(x, c).
    //       iii. if r is not failure, return r.
    //       iv. let k = k + 1.
    //    c. return failure
    let alts = alternatives.collect::<Vec<_>>();
    Rc::new(move |state, continuation| run_match_any_alternatives(&alts, &state, &continuation))
}

fn run_match_any_alternatives(
    alternatives: &[Matcher],
    state: &MatchState,
    continuation: &MatcherContinuation,
) -> Option<MatchState> {
    for m in alternatives {
        let r = m.as_ref()(state.clone(), continuation.clone());
        if r.is_some() {
            return r;
        }
    }
    None
}

impl Alternative {
    pub(crate) fn compile_subpattern(&self, rer: &RegExpRecord, direction: Direction) -> Matcher {
        // The syntax-directed operation CompileSubpattern takes arguments rer (a RegExp Record) and direction (forward
        // or backward) and returns a Matcher.
        //  Alternative :: [empty]
        //  1. Return EmptyMatcher().
        //  Alternative :: Alternative Term
        //  1. Let m1 be CompileSubpattern of Alternative with arguments rer and direction.
        //  2. Let m2 be CompileSubpattern of Term with arguments rer and direction.
        //  3. Return MatchSequence(m1, m2, direction).

        // Since we parsed as a sequence of Terms, this is equivalent to
        //  [empty] Term Term ... Term (where there may be any number >= 0 of Terms)
        let mut iterator =
            [empty_matcher()].into_iter().chain(self.0.iter().map(|term| term.compile_subpattern(rer, direction)));
        match_all_terms(&mut iterator, direction)
    }
}

fn run_empty(state: MatchState, continuation: &MatcherContinuation) -> Option<MatchState> {
    //  Performs the following steps when called:
    //    a. Assert: x is a MatchState.
    //    b. Assert: c is a MatcherContinuation.
    //    c. Return c(x).
    continuation.as_ref()(state)
}

fn empty_matcher() -> Matcher {
    // EmptyMatcher ( )
    // The abstract operation EmptyMatcher takes no arguments and returns a Matcher. It performs the following steps when called:
    //
    // 1. Return a new Matcher with parameters (x, c) that captures nothing and performs the following steps when called:
    //    a. Assert: x is a MatchState.
    //    b. Assert: c is a MatcherContinuation.
    //    c. Return c(x).
    //
    Rc::new(move |state, continuation| run_empty(state, &continuation))
}

fn match_all_terms(terms: &mut impl Iterator<Item = Matcher>, direction: Direction) -> Matcher {
    // Compose a non-empty list of term matchers into one matcher.
    //
    // Forward matching runs the terms from left to right. Backward matching runs
    // the same terms from right to left. Each step wraps the matcher built so
    // far with a continuation that runs the next term before the caller's
    // continuation.
    let trms = terms.collect::<Vec<_>>();

    match direction {
        Direction::Forward => match trms.as_slice() {
            [first, rest @ ..] => {
                let mut m = first.clone();

                for term in rest {
                    let inner_m = m.clone();
                    let inner_term = term.clone();

                    m = Rc::new(move |state, continuation| {
                        run_match_term_inner(&inner_m, inner_term.clone(), state, continuation)
                    });
                }

                m
            }
            [] => panic!("match_all_terms should be called with at least one matcher"),
        },

        Direction::Backward => match trms.as_slice() {
            [rest @ .., last] => {
                let mut m = last.clone();

                for term in rest.iter().rev() {
                    let inner_m = m.clone();
                    let inner_term = term.clone();

                    m = Rc::new(move |state, continuation| {
                        run_match_term_inner(&inner_m, inner_term.clone(), state, continuation)
                    });
                }

                m
            }
            [] => panic!("match_all_terms should be called with at least one matcher"),
        },
    }
}

fn run_match_term_inner(
    m1: &Matcher,
    m2: Matcher,
    state: MatchState,
    continuation: MatcherContinuation,
) -> Option<MatchState> {
    let d = Rc::new(move |y: MatchState| m2.as_ref()(y, continuation.clone()));
    m1.as_ref()(state, d)
}

// [empty] TermA TermB TermC
// m = match_sequence(e-a-b-matcher, c-matcher)
//   = |x, c| {
//        let continuation-c = |y| c-matcher(y, c)
//        e-a-b-matcher(x, continuation-c)
//     }
// e-a-b-matcher = match_sequence(e-a-matcher, b-matcher)
//   = |x, c| {
//        let continuation-b = |y| b-matcher(y, c)
//        e-a-matcher(x, continuation-b)
//     }
// e-a-matcher = match_sequene(e-matcher, a-matcher)
//   = |x, c| {
//       let continuation-a = |y| a-matcher(y, c)
//       e-matcher(x, continuation-a)
//     }

// fn match_sequence(m1: Matcher, m2: Matcher, direction: Direction) -> Matcher {
//     // MatchSequence ( m1, m2, direction )
//     //
//     // The abstract operation MatchSequence takes arguments m1 (a Matcher), m2 (a Matcher), and direction (forward or
//     // backward) and returns a Matcher. It performs the following steps when called:
//     //
//     // 1. If direction is forward, then
//     //    a. Return a new Matcher with parameters (x, c) that captures m1 and m2 and performs the following steps when
//     //       called:
//     //       i. Assert: x is a MatchState.
//     //       ii. Assert: c is a MatcherContinuation.
//     //       iii. Let d be a new MatcherContinuation with parameters (y) that captures c and m2 and performs the
//     //            following steps when called:
//     //            1. Assert: y is a MatchState.
//     //            2. Return m2(y, c).
//     //       iv. Return m1(x, d).
//     // 2. Assert: direction is backward.
//     // 3. Return a new Matcher with parameters (x, c) that captures m1 and m2 and performs the following steps when
//     //    called:
//     //    a. Assert: x is a MatchState.
//     //    b. Assert: c is a MatcherContinuation.
//     //    c. Let d be a new MatcherContinuation with parameters (y) that captures c and m1 and performs the following
//     //       steps when called:
//     //       i. Assert: y is a MatchState.
//     //       ii. Return m1(y, c).
//     //    d. Return m2(x, d).
//     match direction {
//         Direction::Forward => {
//             Rc::new(move |state, continuation| run_forward_match_sequence(&m1, m2.clone(), state, continuation))
//         }
//         Direction::Backward => {
//             Rc::new(move |state, continuation| run_backward_match_sequence(m1.clone(), &m2, state, continuation))
//         }
//     }
// }
//
// fn run_forward_match_sequence(
//     m1: &Matcher,
//     m2: Matcher,
//     state: MatchState,
//     continuation: MatcherContinuation,
// ) -> Option<MatchState> {
//     let d: MatcherContinuation = Rc::new(move |y: MatchState| m2.as_ref()(y, continuation.clone()));
//     m1.as_ref()(state, d)
// }
//
// fn run_backward_match_sequence(
//     m1: Matcher,
//     m2: &Matcher,
//     state: MatchState,
//     continuation: MatcherContinuation,
// ) -> Option<MatchState> {
//     let d: MatcherContinuation = Rc::new(move |y: MatchState| m1.as_ref()(y, continuation.clone()));
//     m2.as_ref()(state, d)
// }

impl Term {
    pub(crate) fn compile_subpattern(&self, rer: &RegExpRecord, direction: Direction) -> Matcher {
        // The syntax-directed operation CompileSubpattern takes arguments rer (a RegExp Record) and direction (forward
        // or backward) and returns a Matcher.
        match &self.node {
            TermNode::Assertion(assertion) => {
                // Term :: Assertion
                // 1. Return CompileAssertion of Assertion with argument regexpRecord.
                assertion.compile_assertion(rer)
                // Note 4
                // The resulting Matcher is independent of direction.
            }
            TermNode::Atom(atom, None) => {
                // Term :: Atom
                // 1. Return CompileAtom of Atom with arguments rer and direction.
                atom.compile_atom(rer, direction)
            }
            TermNode::Atom(atom, Some(quantifier)) => {
                // Term :: Atom Quantifier
                // 1. Let m be CompileAtom of Atom with arguments rer and direction.
                // 2. Let q be CompileQuantifier of Quantifier.
                // 3. Assert: q.[[Min]] ≤ q.[[Max]].
                // 4. Let parenIndex be CountLeftCapturingParensBefore(Term).
                // 5. Let parenCount be CountLeftCapturingParensWithin(Atom).
                // 6. Return a new Matcher with parameters (x, c) that captures m, q, parenIndex, and parenCount and performs the following steps when called:
                //    a. Assert: x is a MatchState.
                //    b. Assert: c is a MatcherContinuation.
                //    c. Return RepeatMatcher(m, q.[[Min]], q.[[Max]], q.[[Greedy]], x, c, parenIndex, parenCount).
                let m = atom.compile_atom(rer, direction);
                let q = quantifier.compile_quantifier();
                let paren_index = self.count_left_capturing_parens_before();
                let paren_count = atom.count_left_capturing_parens_within();
                Rc::new(move |state, continuation| {
                    repeat_matcher(&m, q.min, q.max, q.greedy, state, &continuation, paren_index, paren_count)
                })
            }
        }
    }

    fn count_left_capturing_parens_before(&self) -> usize {
        // Static Semantics: CountLeftCapturingParensBefore ( node )
        //
        // The abstract operation CountLeftCapturingParensBefore takes argument node (a Parse Node) and returns a
        // non-negative integer. It returns the number of left-capturing parentheses within the enclosing pattern that
        // occur to the left of node.
        //
        // It performs the following steps when called:
        //
        //  1. Assert: node is an instance of a production in the RegExp Pattern grammar.
        //  2. Let pattern be the Pattern containing node.
        //  3. Return the number of Atom :: ( GroupSpecifieropt Disjunction ) Parse Nodes contained within pattern that
        //     either occur before node or contain node.
        self.left_capturing_parens_before
    }
}

#[expect(clippy::too_many_arguments)]
fn repeater_continuation(
    y: MatchState,
    m: &Matcher,
    min: usize,
    max: Option<usize>,
    greedy: bool,
    state: &MatchState,
    continuation: &MatcherContinuation,
    paren_index: usize,
    paren_count: usize,
) -> Option<MatchState> {
    //    a. Assert: y is a MatchState.
    //    b. If min = 0 and y.[[EndIndex]] = matchState.[[EndIndex]], return failure.
    //    c. If min = 0, let min2 be 0; else let min2 be min - 1.
    //    d. If max = +∞, let max2 be +∞; else let max2 be max - 1.
    //    e. Return RepeatMatcher(m, min2, max2, greedy, y, continue, parenIndex, parenCount).
    if min == 0 && y.end_index == state.end_index {
        return None;
    }
    let min2 = if min == 0 { 0 } else { min - 1 };
    let max2 = max.map(|max| max - 1);
    repeat_matcher(m, min2, max2, greedy, y, continuation, paren_index, paren_count)
}

#[expect(clippy::too_many_arguments)]
fn repeat_matcher(
    m: &Matcher,
    min: usize,
    max: Option<usize>,
    greedy: bool,
    state: MatchState,
    continuation: &MatcherContinuation,
    p_index: usize,
    p_count: usize,
) -> Option<MatchState> {
    // RepeatMatcher ( m, min, max, greedy, matchState, continue, parenIndex, parenCount )
    //
    // The abstract operation RepeatMatcher takes arguments m (a Matcher), min (a non-negative integer), max (a
    // non-negative integer or +∞), greedy (a Boolean), matchState (a MatchState), continue (a MatcherContinuation),
    // parenIndex (a non-negative integer), and parenCount (a non-negative integer) and returns either a MatchState or
    // failure. It performs the following steps when called:
    //
    // 1. If max = 0, return continue(matchState).
    // 2. Let d be a new MatcherContinuation with parameters (y) that captures m, min, max, greedy, matchState,
    //    continue, parenIndex, and parenCount and performs the following steps when called:
    //    a. Assert: y is a MatchState.
    //    b. If min = 0 and y.[[EndIndex]] = matchState.[[EndIndex]], return failure.
    //    c. If min = 0, let min2 be 0; else let min2 be min - 1.
    //    d. If max = +∞, let max2 be +∞; else let max2 be max - 1.
    //    e. Return RepeatMatcher(m, min2, max2, greedy, y, continue, parenIndex, parenCount).
    // 3. Let cap be a copy of matchState.[[Captures]].
    // 4. For each integer k in the inclusive interval from parenIndex + 1 to parenIndex + parenCount, set cap[k] to
    //    undefined.
    // 5. Let input be matchState.[[Input]].
    // 6. Let e be matchState.[[EndIndex]].
    // 7. Let xr be the MatchState { [[Input]]: input, [[EndIndex]]: e, [[Captures]]: cap }.
    // 8. If min ≠ 0, return m(xr, d).
    // 9. If greedy is false, then
    //    a. Let z be continue(matchState).
    //    b. If z is not failure, return z.
    //    c. Return m(xr, d).
    // 10. Let z be m(xr, d).
    // 11. If z is not failure, return z.
    // 12. Return continue(matchState).
    if max.is_some_and(|max| max == 0) {
        return continuation(state);
    }
    let mut cap = state.captures.clone();
    for item in cap.iter_mut().take(p_index + p_count).skip(p_index) {
        *item = None;
    }
    let input = state.input.clone();
    let e = state.end_index;
    let xr = MatchState { input, end_index: e, captures: cap };
    let value = state.clone();
    let continue_copy = continuation.clone();
    let matcher_copy = m.clone();
    let d = Rc::new(move |y| {
        repeater_continuation(y, &matcher_copy, min, max, greedy, &value, &continue_copy, p_index, p_count)
    });
    if min != 0 {
        return m.as_ref()(xr, d);
    }
    if !greedy {
        let z = continuation(state);
        if z.is_some() {
            return z;
        }
        return m.as_ref()(xr, d);
    }
    let z = m.as_ref()(xr, d);
    if z.is_some() {
        return z;
    }
    continuation(state)
}

impl Assertion {
    fn compile_assertion(&self, rer: &RegExpRecord) -> Matcher {
        match self {
            Assertion::Start => {
                let rer = rer.clone();

                Rc::new(move |state, continuation| {
                    let input = state.input.as_slice();
                    let e = state.end_index;

                    // `^` matches the start of input, or the position after a
                    // line terminator when multiline mode is enabled. The sticky
                    // flag does not change this behavior.
                    if e == 0 || rer.multiline == Lines::Multi && LINE_TERMINATORS.contains(&input[e - 1]) {
                        continuation(state)
                    } else {
                        None
                    }
                })
            }

            Assertion::End => {
                let rer = rer.clone();

                Rc::new(move |state, continuation| {
                    let input = state.input.as_slice();
                    let e = state.end_index;
                    let input_length = input.len();

                    // `$` matches the end of input, or the position before a
                    // line terminator when multiline mode is enabled.
                    if e == input_length || rer.multiline == Lines::Multi && LINE_TERMINATORS.contains(&input[e]) {
                        continuation(state)
                    } else {
                        None
                    }
                })
            }

            Assertion::WordBoundary => {
                let rer = rer.clone();

                Rc::new(move |state, continuation| {
                    let input = state.input.as_slice();
                    let e = state.end_index;

                    // A word boundary exists when exactly one side of the
                    // current position is a word character.
                    let before = e > 0 && is_word_char(&rer, input, e - 1);
                    let after = is_word_char(&rer, input, e);

                    if before == after { None } else { continuation(state) }
                })
            }

            Assertion::NotWordBoundary => {
                let rer = rer.clone();

                Rc::new(move |state, continuation| {
                    let input = state.input.as_slice();
                    let e = state.end_index;

                    // A non-boundary exists when both sides of the current
                    // position have the same word-character status.
                    let before = e > 0 && is_word_char(&rer, input, e - 1);
                    let after = is_word_char(&rer, input, e);

                    if before == after { continuation(state) } else { None }
                })
            }

            Assertion::LookAhead(disjunction) => {
                // Lookahead checks the subpattern from the current position
                // without consuming input.
                let m = disjunction.compile_subpattern(rer, Direction::Forward);

                Rc::new(move |state, continuation| {
                    // Run the assertion body with an identity continuation so we
                    // can observe whether it matches and collect its captures.
                    let d: MatcherContinuation = Rc::new(Some);

                    let r = m.as_ref()(state.clone(), d)?;

                    // Positive lookahead keeps captures produced inside the
                    // assertion, but restores the original input position.
                    let z = MatchState { input: state.input, end_index: state.end_index, captures: r.captures };

                    continuation(z)
                })
            }

            Assertion::NegLookAhead(disjunction) => {
                // Negative lookahead succeeds only when the forward subpattern
                // fails at the current position.
                let m = disjunction.compile_subpattern(rer, Direction::Forward);

                Rc::new(move |state, continuation| {
                    let d: MatcherContinuation = Rc::new(Some);

                    match m.as_ref()(state.clone(), d) {
                        Some(_) => None,
                        None => continuation(state),
                    }
                })
            }

            Assertion::LookBehind(disjunction) => {
                // Lookbehind checks the subpattern ending at the current
                // position. The subpattern is compiled in backward direction,
                // but a successful assertion still consumes no input.
                let m = disjunction.compile_subpattern(rer, Direction::Backward);

                Rc::new(move |state, continuation| {
                    let d: MatcherContinuation = Rc::new(Some);

                    let r = m.as_ref()(state.clone(), d)?;

                    // Positive lookbehind keeps captures produced inside the
                    // assertion, but restores the original input position.
                    let z = MatchState { input: state.input, end_index: state.end_index, captures: r.captures };

                    continuation(z)
                })
            }

            Assertion::NegLookBehind(disjunction) => {
                // Negative lookbehind succeeds only when the backward subpattern
                // fails ending at the current position.
                let m = disjunction.compile_subpattern(rer, Direction::Backward);

                Rc::new(move |state, continuation| {
                    let d: MatcherContinuation = Rc::new(Some);

                    match m.as_ref()(state.clone(), d) {
                        Some(_) => None,
                        None => continuation(state),
                    }
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
    continuation: &MatcherContinuation,
    direction: Direction,
    paren_index: usize,
) -> Option<MatchState> {
    // A MatcherContinuation with parameters (y) that captures x, c, direction, and parenIndex and performs the
    // following steps when called:
    //
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
    let mut cap = state.captures.clone();
    let input = outer_state.input.clone();
    let xe = outer_state.end_index;
    let ye = state.end_index;
    let r = match direction {
        Direction::Forward => CaptureRange { start_index: xe, end_index: ye },
        Direction::Backward => CaptureRange { start_index: ye, end_index: xe },
    };
    cap[paren_index] = Some(r);
    let z = MatchState { input, end_index: ye, captures: cap };
    continuation.as_ref()(z)
}

fn group_matcher(
    x: MatchState,
    c: MatcherContinuation,
    direction: Direction,
    m: &Matcher,
    paren_index: usize,
) -> Option<MatchState> {
    // a Matcher with parameters (x, c) that captures direction, m, and parenIndex and
    //    performs the following steps when called:
    //
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
    let xc = x.clone();
    let d = Rc::new(move |y| group_continuation(&y, &xc, &c, direction, paren_index));
    m.as_ref()(x, d.clone())
}

impl Atom {
    pub(crate) fn compile_atom(&self, rer: &RegExpRecord, direction: Direction) -> Matcher {
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
                character_set_matcher(rer, a, false, direction)
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
                character_set_matcher(rer, a, false, direction)
            }
            AtomNode::AtomEscape(atom_escape) => atom_escape.compile_atom(rer, direction),
            AtomNode::CharacterClass(character_class) => {
                // Atom :: CharacterClass
                // 1. Let cc be CompileCharacterClass of CharacterClass with argument rer.
                let cc = character_class.compile_character_class(rer);
                // 2. Let cs be cc.[[CharSet]].
                let cs = cc.char_set;
                // 3. If rer.[[UnicodeSets]] is false, or if every CharSetElement of cs consists of a single character
                //    (including if cs is empty), return CharacterSetMatcher(rer, cs, cc.[[Invert]], direction).
                if rer.unicode_sets == UnicodeSetsMode::Denied || cs.all_single_or_empty() {
                    return character_set_matcher(rer, cs, cc.invert, direction);
                }
                // 4. Assert: cc.[[Invert]] is false.
                // 5. Let lm be an empty List of Matchers.
                // 6. For each CharSetElement s in cs containing more than 1 character, iterating in descending order of
                //    length, do
                //    a. Let cs2 be a one-element CharSet containing the last code point of s.
                //    b. Let m2 be CharacterSetMatcher(rer, cs2, false, direction).
                //    c. For each code point c1 in s, iterating backwards from its second-to-last code point, do
                //       i. Let cs1 be a one-element CharSet containing c1.
                //       ii. Let m1 be CharacterSetMatcher(rer, cs1, false, direction).
                //       iii. Set m2 to MatchSequence(m1, m2, direction).
                //    d. Append m2 to lm.
                // 7. Let singles be the CharSet containing every CharSetElement of cs that consists of a single
                //    character.
                // 8. Append CharacterSetMatcher(rer, singles, false, direction) to lm.
                // 9. If cs contains the empty sequence of characters, append EmptyMatcher() to lm.
                // 10. Let m2 be the last Matcher in lm.
                // 11. For each Matcher m1 of lm, iterating backwards from its second-to-last element, do
                //     a. Set m2 to MatchTwoAlternatives(m1, m2).
                // 12. Return m2.

                todo!()
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
                let m = disjunction.compile_subpattern(rer, direction);
                let paren_index = self.count_left_capturing_parens_before();
                Rc::new(move |x, c| group_matcher(x, c, direction, &m, paren_index))
            }
            AtomNode::UnGroupedDisjunction((add, remove), disjunction) => {
                let empty_remove = RegularExpressionModifiers::default();
                let remove = remove.as_ref().unwrap_or(&empty_remove);
                let new_rer = rer.update_modifiers(add, remove);
                disjunction.compile_subpattern(&new_rer, direction)
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
    #[expect(unused_variables)]
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
            CharacterClassEscape::Property(unicode_property_value_expression) => todo!(),
            CharacterClassEscape::NotProperty(unicode_property_value_expression) => todo!(),
        }
    }
}

fn character_set_matcher(rer: &RegExpRecord, set: CharSet, invert: bool, direction: Direction) -> Matcher {
    let rer = rer.clone();
    Rc::new(move |state, continuation| run_character_set_matcher(state, &continuation, &rer, &set, invert, direction))
}

fn run_character_set_matcher(
    state: MatchState,
    continuation: &MatcherContinuation,
    rer: &RegExpRecord,
    a: &CharSet,
    invert: bool,
    direction: Direction,
) -> Option<MatchState> {
    let MatchState { input, end_index: e, captures: cap } = state;
    let f = match direction {
        Direction::Forward => e + 1,
        Direction::Backward => {
            if e == 0 {
                return None;
            }
            e - 1
        }
    };
    let input_length = input.len();
    if f > input_length {
        return None;
    }
    let index = e.min(f);
    let ch = input[index];
    let cc = rer.canonicalize(ch);
    let found = a.contains(cc);
    if invert == found {
        return None;
    }
    let y = MatchState { input, end_index: f, captures: cap };
    continuation(y)
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
    }

    /// Iterate over stored inclusive ranges as `(start, end)`.
    fn ranges(&self) -> impl Iterator<Item = (u32, u32)> + '_ {
        self.ranges.iter().map(|(&start, &end)| (start, end))
    }

    /// Returns all values in `a` that are not present in `s`.
    fn difference(a: &Self, s: &Self) -> Self {
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
        set.insert(0xa0);
        set.insert(0x1680);
        set.insert_range(0x2000, 0x200a);
        set.insert(0x202f);
        set.insert(0x205f);
        set.insert(0x3000);
        set
    }

    pub(crate) fn iter(&self) -> CharSetIter<'_> {
        CharSetIter { ranges: self.ranges.iter(), current: None, strings: self.strings.iter() }
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
    continuation: &MatcherContinuation,
    rer: &RegExpRecord,
    backreferences: &[usize],
    direction: Direction,
) -> Option<MatchState> {
    // a Matcher with parameters (x, c) that captures regexpRecord, ns, and direction and performs the following steps when called:
    //    a. Assert: x is a MatchState.
    //    b. Assert: c is a MatcherContinuation.
    //    c. Let input be x.[[Input]].
    let input = state.input;
    //    d. Let cap be x.[[Captures]].
    let captures = state.captures;
    //    e. Let r be undefined.
    let mut r = None;
    //    f. For each integer n of ns, do
    for &n in backreferences {
        //       i. If cap[n] is not undefined, then
        if let Some(cap) = &captures[n - 1] {
            //          1. Assert: r is undefined.
            //          2. Set r to cap[n].
            r = Some(cap);
            break;
        }
    }
    //    h. Let endIndex be x.[[EndIndex]].
    let end_index = state.end_index;
    match r {
        None => {
            //    g. If r is undefined, return c(x).
            continuation(MatchState { input, end_index, captures })
        }
        Some(r) => {
            //    i. Let rs be r.[[StartIndex]].
            let rs = r.start_index;
            //    j. Let re be r.[[EndIndex]].
            let re = r.end_index;
            //    k. Let len be re - rs.
            let len = re - rs;
            //    l. If direction is forward, let f be endIndex + len.
            //    m. Else, let f be endIndex - len.
            let f = if direction == Direction::Forward {
                end_index + len
            } else {
                if end_index < len {
                    return None;
                }
                end_index - len
            };
            //    n. Let inputLength be the number of elements in input.
            let input_length = input.len();
            //    o. If f < 0 or f > inputLength, return failure.
            if f > input_length {
                return None;
            }
            //    p. Let g be min(endIndex, f).
            let g = end_index.min(f);
            //    q. If there exists an integer i in the interval from 0 (inclusive) to len (exclusive) such that
            //    Canonicalize(regexpRecord, input[rs + i]) is not Canonicalize(regexpRecord, input[g + i]), return
            //    failure.
            if (0..len).any(|idx| rer.canonicalize(input[rs + idx]) != rer.canonicalize(input[g + idx])) {
                return None;
            }
            //    r. Let y be the MatchState { [[Input]]: input, [[EndIndex]]: f, [[Captures]]: cap }.
            let y = MatchState { input, end_index: f, captures };
            //    s. Return c(y).
            continuation(y)
        }
    }
}

fn backreference_matcher(rer: &RegExpRecord, backreferences: &[usize], direction: Direction) -> Matcher {
    // BackreferenceMatcher ( regexpRecord, ns, direction )
    // The abstract operation BackreferenceMatcher takes arguments regexpRecord (a RegExp Record), ns (a List of positive integers), and direction (forward or backward) and returns a Matcher. It performs the following steps when called:
    //
    // 1. Return a new Matcher with parameters (x, c) that captures regexpRecord, ns, and direction and performs the following steps when called:
    //    a. Assert: x is a MatchState.
    //    b. Assert: c is a MatcherContinuation.
    //    c. Let input be x.[[Input]].
    //    d. Let cap be x.[[Captures]].
    //    e. Let r be undefined.
    //    f. For each integer n of ns, do
    //       i. If cap[n] is not undefined, then
    //          1. Assert: r is undefined.
    //          2. Set r to cap[n].
    //    g. If r is undefined, return c(x).
    //    h. Let endIndex be x.[[EndIndex]].
    //    i. Let rs be r.[[StartIndex]].
    //    j. Let re be r.[[EndIndex]].
    //    k. Let len be re - rs.
    //    l. If direction is forward, let f be endIndex + len.
    //    m. Else, let f be endIndex - len.
    //    n. Let inputLength be the number of elements in input.
    //    o. If f < 0 or f > inputLength, return failure.
    //    p. Let g be min(endIndex, f).
    //    q. If there exists an integer i in the interval from 0 (inclusive) to len (exclusive) such that Canonicalize(regexpRecord, input[rs + i]) is not Canonicalize(regexpRecord, input[g + i]), return failure.
    //    r. Let y be the MatchState { [[Input]]: input, [[EndIndex]]: f, [[Captures]]: cap }.
    //    s. Return c(y).
    let rer = rer.clone();
    let backrefs = Vec::from(backreferences);
    Rc::new(move |state, continuation| {
        run_backreference_match(state, &continuation, &rer, backrefs.as_slice(), direction)
    })
}

impl AtomEscape {
    #[expect(unused_variables)]
    pub(crate) fn compile_atom(&self, rer: &RegExpRecord, direction: Direction) -> Matcher {
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
                    return character_set_matcher(rer, cs, false, direction);
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
                character_set_matcher(rer, a, false, direction)
            }
            AtomEscape::GroupName(group_name) => todo!(),
        }
    }
}
