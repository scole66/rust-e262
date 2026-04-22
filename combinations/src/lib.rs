//! # Combination and Permutation Iterators
//!
//! These two iteration structures, [Combination] and [Permutation], provide combinatronic visitation of
//! values, via the algorithms presented in Knuth volume 4A, in particular, algorithms
//! * 7.2.1.2: Generating all permutations (algorithm L)
//! * 7.2.1.3: Generating all combinations (algorithm T)
//!
#![warn(missing_docs)]

/// An iterator that returns [Vec]s of items representing all combinations, in lexographic order.
///
/// When we speak of combinations, we name two things: a list of distinct values, and a grouping number. For
/// instance, the set of _a_, _b_, and _c_, can be combined in groups of 2 to form _ab_, _ac_, and _bc_.
///
/// # Examples
///
/// ```
/// use combinations::Combination;
///
/// let combo_iter = Combination::new(&["a", "b", "c"], 2);
/// let combos = combo_iter.collect::<Vec<_>>();
/// assert_eq!(combos, vec![vec!["a", "b"], vec!["a", "c"], vec!["b", "c"]]);
/// ```
///
/// Since the values in the resulting combination vectors are duplicated, the items must implement [Clone].
/// (The system does, however, use an internal vector of easily [Copy]-able integers, and only clones when
/// generating the result.) If your items are expensive to clone, you may be better off making combinations of
/// references.
///
/// ```
/// use combinations::Combination;
///
/// let (a, b, c) = (String::from("long String A"), String::from("String B"), String::from("C"));
/// let combo_iter = Combination::new(&[&a, &b, &c], 2);
/// let combos = combo_iter.collect::<Vec<_>>();
/// assert_eq!(combos, vec![vec![&a, &b], vec![&a, &c], vec![&b, &c]]);
/// ```
///
/// Lexographic order is maintained assuming that the initial set of items is in lexographic order itself. (So
/// you can adjust the initial order to rank items "higher" or "lower", if that's useful.) But play with it;
/// it probably doesn't actually behave the way you think it should.
pub struct Combination<T> {
    source: Vec<T>,
    c: Vec<usize>,
    j: usize,
    t: usize,
    done: bool,
}

impl<T> Combination<T> {
    /// Create a new combination-generating iterator.
    ///
    /// See the [Combination] structure for more discussion.
    ///
    /// # Example
    /// ```
    /// use combinations::Combination;
    ///
    /// let combo_iter = Combination::new(&[10, 20, 30, 40], 2);
    /// assert_eq!(combo_iter.collect::<Vec<_>>(), vec![
    ///     vec![10, 20],
    ///     vec![10, 30],
    ///     vec![20, 30],
    ///     vec![10, 40],
    ///     vec![20, 40],
    ///     vec![30, 40],
    /// ]);
    /// ```
    pub fn new(items: &[T], size: usize) -> Combination<T>
    where
        T: Clone,
    {
        let mut c = (0..size).collect::<Vec<_>>();
        c.push(items.len());
        c.push(0);
        Combination {
            source: items.to_vec(),
            c,
            j: size,
            t: size,
            done: false,
        }
    }
}

impl<T> Iterator for Combination<T>
where
    T: Clone,
{
    type Item = Vec<T>;

    // algorithm T from Knuth 7.2.1.3 "Generating all combinations"
    fn next(&mut self) -> Option<Self::Item> {
        // This structure uses a "child vector" that contains all the indexes into the source data. We do the
        // combinatorial work on that index vector, as copying an index is likely much cheaper than cloning
        // the actual items being permuted. Source items are only cloned when the return value is constructed.
        if self.done {
            None
        } else {
            let result = self.c[0..self.t]
                .iter()
                .map(|&idx| self.source[idx].clone())
                .collect::<Vec<_>>();

            let mut x;
            if self.j > 0 {
                x = self.j;
            } else {
                if self.c[0] + 1 < self.c[1] {
                    self.c[0] += 1;
                    return Some(result);
                }
                self.j = 2;
                loop {
                    self.c[self.j - 2] = self.j - 2;
                    x = self.c[self.j - 1] + 1;
                    if x != self.c[self.j] {
                        break;
                    }
                    self.j += 1;
                }
                if self.j > self.t {
                    self.done = true;
                    return Some(result);
                }
            }
            self.c[self.j - 1] = x;
            self.j -= 1;

            Some(result)
        }
    }
}

/// An iterator that returns [Vec]s of items representing all permutations, in lexographic order.
///
/// A permutation of a set of items is one particular ordering of those items. This iterator returns all such
/// orderings, one at a time.
///
/// # Examples
///
/// ```
/// use combinations::Permutation;
///
/// let permute_iter = Permutation::new(&["a", "b", "c"]);
/// let permutes = permute_iter.collect::<Vec<_>>();
/// assert_eq!(permutes, vec![
///     vec!["a", "b", "c"],
///     vec!["a", "c", "b"],
///     vec!["b", "a", "c"],
///     vec!["b", "c", "a"],
///     vec!["c", "a", "b"],
///     vec!["c", "b", "a"],
/// ]);
/// ```
/// Since the values in the resulting permutation vectors are duplicated, the items must implement [Clone].
/// (The system does, however, use an internal vector of easily [Copy]-able integers, and only clones when
/// generating the result.) If your items are expensive to clone, you may be better off making permutations of
/// references.
///
/// Note that the number of items returned by the permutation iterator is the factorial of the number of
/// items. This can be extremely large. Consider well whether you really want that list.
pub struct Permutation<T> {
    items: Vec<T>,
    a: Vec<usize>,
    n: usize,
    done: bool,
}

impl<T> Permutation<T>
where
    T: Clone,
{
    /// Create a new permutation-generating iterator.
    ///
    /// See the [Permutation] structure for more discussion.
    ///
    /// # Example
    /// ```
    /// use combinations::Permutation;
    ///
    /// let perm_iter = Permutation::new(&[10, 20, 30, 40]);
    /// assert_eq!(perm_iter.collect::<Vec<_>>(), vec![
    ///     vec![10, 20, 30, 40],
    ///     vec![10, 20, 40, 30],
    ///     vec![10, 30, 20, 40],
    ///     vec![10, 30, 40, 20],
    ///     vec![10, 40, 20, 30],
    ///     vec![10, 40, 30, 20],
    ///     vec![20, 10, 30, 40],
    ///     vec![20, 10, 40, 30],
    ///     vec![20, 30, 10, 40],
    ///     vec![20, 30, 40, 10],
    ///     vec![20, 40, 10, 30],
    ///     vec![20, 40, 30, 10],
    ///     vec![30, 10, 20, 40],
    ///     vec![30, 10, 40, 20],
    ///     vec![30, 20, 10, 40],
    ///     vec![30, 20, 40, 10],
    ///     vec![30, 40, 10, 20],
    ///     vec![30, 40, 20, 10],
    ///     vec![40, 10, 20, 30],
    ///     vec![40, 10, 30, 20],
    ///     vec![40, 20, 10, 30],
    ///     vec![40, 20, 30, 10],
    ///     vec![40, 30, 10, 20],
    ///     vec![40, 30, 20, 10],
    /// ]);
    /// ```
    pub fn new(items: &[T]) -> Self {
        let n = items.len();
        Permutation {
            items: items.to_vec(),
            n,
            a: (0..=n).collect::<Vec<_>>(),
            done: false,
        }
    }
}

impl<T> Iterator for Permutation<T>
where
    T: Clone,
{
    type Item = Vec<T>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            None
        } else {
            // Algorithm L from Knuth 7.2.1.2. Generating all permutations.
            let result = Some(
                self.a[1..=self.n]
                    .iter()
                    .map(|&idx| self.items[idx - 1].clone())
                    .collect::<Vec<_>>(),
            );

            let mut j = self.n - 1;
            while j > 0 && self.a[j + 1] <= self.a[j] {
                j -= 1;
            }
            if j == 0 {
                self.done = true;
            } else {
                let mut l = self.n;
                while self.a[j] >= self.a[l] {
                    l -= 1;
                }
                (self.a[j], self.a[l]) = (self.a[l], self.a[j]);
                let mut k = j + 1;
                let mut l = self.n;
                while k < l {
                    (self.a[k], self.a[l]) = (self.a[l], self.a[k]);
                    k += 1;
                    l -= 1;
                }
            }
            result
        }
    }
}
