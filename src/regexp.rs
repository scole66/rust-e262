use super::*;
use casefold::*;
use compile::*;
use itertools::Itertools;
use parse::*;

pub(crate) fn provision_regexp_intrinsic(realm: &Rc<RefCell<Realm>>) {
    let object_prototype = realm.borrow().intrinsics.object_prototype.clone();
    let function_prototype = realm.borrow().intrinsics.function_prototype.clone();

    // The RegExp prototype object:
    //
    // * is %RegExp.prototype%.
    // * is an ordinary object.
    // * is not a RegExp instance and does not have a [[RegExpMatcher]] internal slot or any of the other
    //   internal slots of RegExp instance objects.
    // * has a [[Prototype]] internal slot whose value is %Object.prototype%.
    let regexp_prototype = ordinary_object_create(Some(object_prototype));
    realm.borrow_mut().intrinsics.reg_exp_prototype = regexp_prototype.clone();

    // The RegExp Constructor
    //
    // * is %RegExp%.
    // * is the initial value of the "RegExp" property of the global object.
    // * creates and initializes a new RegExp object when called as a constructor.
    // * when called as a function rather than as a constructor, returns either a new RegExp object, or the
    //   argument itself if the only argument is a RegExp object.
    // * may be used as the value of an extends clause of a class definition. Subclass constructors that
    //   intend to inherit the specified RegExp behaviour must include a super call to the RegExp constructor
    //   to create and initialize subclass instances with the necessary internal slots.

    // Properties of the RegExp Constructor
    // The RegExp constructor:
    //
    // * has a [[Prototype]] internal slot whose value is %Function.prototype%.

    let regexp_constructor = create_builtin_function(
        Box::new(regexp_constructor_function),
        Some(ConstructorKind::Base),
        1_f64,
        PropertyKey::from("RegExp"),
        BUILTIN_FUNCTION_SLOTS,
        Some(realm.clone()),
        Some(function_prototype.clone()),
        None,
    );
    realm.borrow_mut().intrinsics.reg_exp = regexp_constructor.clone();

    // RegExp.prototype
    // The initial value of RegExp.prototype is the RegExp prototype object.
    //
    // This property has the attributes { [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: false
    // }.
    define_property_or_throw(
        &regexp_constructor,
        "prototype",
        PotentialPropertyDescriptor::new()
            .value(&regexp_prototype)
            .writable(false)
            .enumerable(false)
            .configurable(false),
    )
    .expect(GOODOBJ);

    // get RegExp [ %Symbol.species% ]
    // RegExp[%Symbol.species%] is an accessor property whose set accessor function is undefined. Its get
    // accessor function performs the following steps when called:
    //
    // 1. Return the this value.
    // The value of the "name" property of this function is "get [Symbol.species]".
    let species_sym = wks(WksId::Species);
    let species_fcn = create_builtin_function(
        Box::new(regexp_species),
        None,
        0.0,
        species_sym.clone().into(),
        BUILTIN_FUNCTION_SLOTS,
        Some(realm.clone()),
        Some(function_prototype.clone()),
        Some("get".into()),
    );
    let species_ppd = PotentialPropertyDescriptor::new().get(species_fcn).enumerable(false).configurable(true);
    define_property_or_throw(&regexp_constructor, species_sym, species_ppd).expect(GOODOBJ);

    // RegExp.prototype.constructor
    // The initial value of RegExp.prototype.constructor is %RegExp%.
    let prototype_ppd = PotentialPropertyDescriptor::new()
        .value(regexp_constructor.clone())
        .writable(true)
        .enumerable(false)
        .configurable(true);
    define_property_or_throw(&regexp_prototype, "constructor", prototype_ppd).expect(GOODOBJ);

    // Prototype function properties
    macro_rules! prototype_function {
        ( $steps:expr_2021, $name:expr_2021, $length:expr_2021 ) => {
            let key = PropertyKey::from($name);
            let function_object = create_builtin_function(
                Box::new($steps),
                None,
                $length,
                key.clone(),
                BUILTIN_FUNCTION_SLOTS,
                Some(realm.clone()),
                Some(function_prototype.clone()),
                None,
            );
            define_property_or_throw(
                &regexp_prototype,
                key,
                PotentialPropertyDescriptor::new()
                    .value(function_object)
                    .writable(true)
                    .enumerable(false)
                    .configurable(true),
            )
            .unwrap();
        };
    }

    macro_rules! prototype_symbol_function {
        ( $steps:expr_2021, $name:expr_2021, $length:expr_2021 ) => {
            let key = PropertyKey::from(wks($name));
            let function_object = create_builtin_function(
                Box::new($steps),
                None,
                $length,
                key.clone(),
                BUILTIN_FUNCTION_SLOTS,
                Some(realm.clone()),
                Some(function_prototype.clone()),
                None,
            );
            define_property_or_throw(
                &regexp_prototype,
                key,
                PotentialPropertyDescriptor::new()
                    .value(function_object)
                    .writable(true)
                    .enumerable(false)
                    .configurable(true),
            )
            .unwrap();
        };
    }

    macro_rules! prototype_getter {
        ( $steps:expr_2021, $name:expr_2021 ) => {
            let key = PropertyKey::from($name);

            let function_object = create_builtin_function(
                Box::new($steps),
                None,
                0.0,
                key.clone(),
                BUILTIN_FUNCTION_SLOTS,
                Some(realm.clone()),
                Some(function_prototype.clone()),
                Some(JSString::from("get")),
            );
            define_property_or_throw(
                &regexp_prototype,
                key,
                PotentialPropertyDescriptor::new()
                    .get(ECMAScriptValue::Object(function_object))
                    .enumerable(false)
                    .configurable(true),
            )
            .unwrap();
        };
    }

    prototype_function!(regexp_prototype_exec, "exec", 1.0);
    prototype_function!(regexp_prototype_test, "test", 1.0);
    prototype_function!(regexp_prototype_to_string, "toString", 0.0);
    prototype_symbol_function!(regexp_prototype_match, WksId::Match, 1.0);
    prototype_symbol_function!(regexp_prototype_match_all, WksId::MatchAll, 1.0);
    prototype_symbol_function!(regexp_prototype_replace, WksId::Replace, 2.0);
    prototype_symbol_function!(regexp_prototype_search, WksId::Search, 1.0);
    prototype_symbol_function!(regexp_prototyep_split, WksId::Split, 2.0);
    prototype_getter!(regexp_prototype_dotall, "dotAll");
    prototype_getter!(regexp_prototype_flags, "flags");
    prototype_getter!(regexp_prototype_global, "global");
    prototype_getter!(regexp_prototype_has_indices, "hasIndices");
    prototype_getter!(regexp_prototype_ignore_case, "ignoreCase");
    prototype_getter!(regexp_prototype_multiline, "multiline");
    prototype_getter!(regexp_prototype_source, "source");
    prototype_getter!(regexp_prototype_sticky, "sticky");
    prototype_getter!(regexp_prototype_unicode, "unicode");
    prototype_getter!(regexp_prototype_unicode_sets, "unicodeSets");
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) enum Case {
    Significant,
    Unimportant,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) enum Lines {
    Single,
    Multi,
}

#[derive(Debug, Clone)]
#[expect(unused)]
pub(crate) struct RegExpRecord {
    case: Case,
    multiline: Lines,
    dot_all: bool,
    unicode: UnicodeMode,
    unicode_sets: UnicodeSetsMode,
    capturing_groups_count: usize,
    has_group_names: bool,
    group_names: Vec<Option<JSString>>,
}

impl RegExpRecord {
    pub(crate) fn has_either_unicode_flag(&self) -> bool {
        // HasEitherUnicodeFlag ( rer )
        //
        // The abstract operation HasEitherUnicodeFlag takes argument rer (a RegExp Record) and returns a Boolean. It
        // performs the following steps when called:
        //
        // 1. If rer.[[Unicode]] is true or rer.[[UnicodeSets]] is true, return true.
        // 2. Return false.
        self.unicode == UnicodeMode::Allowed || self.unicode_sets == UnicodeSetsMode::Allowed
    }

    pub(crate) fn canonicalize(&self, ch: u32) -> u32 {
        // Canonicalize ( rer, ch )
        //
        // The abstract operation Canonicalize takes arguments rer (a RegExp Record) and ch (a character) and returns a
        // character. It performs the following steps when called:
        //
        // 1. If HasEitherUnicodeFlag(rer) is true and rer.[[IgnoreCase]] is true, then
        //    a. If the file CaseFolding.txt of the Unicode Character Database provides a simple or common case folding
        //       mapping for ch, return the result of applying that mapping to ch.
        //    b. Return ch.
        // 2. If rer.[[IgnoreCase]] is false, return ch.
        // 3. Assert: ch is a UTF-16 code unit.
        // 4. Let cp be the code point whose numeric value is the numeric value of ch.
        // 5. Let u be toUppercase(« cp »), according to the Unicode Default Case Conversion algorithm.
        // 6. Let uStr be CodePointsToString(u).
        // 7. If the length of uStr ≠ 1, return ch.
        // 8. Let cu be uStr's single code unit element.
        // 9. If the numeric value of ch ≥ 128 and the numeric value of cu < 128, return ch.
        // 10. Return cu.
        //
        // Note:
        //
        // In case-insignificant matches when HasEitherUnicodeFlag(rer) is true, all characters are implicitly
        // case-folded using the simple mapping provided by the Unicode Standard immediately before they are compared.
        // The simple mapping always maps to a single code point, so it does not map, for example, ß (U+00DF LATIN SMALL
        // LETTER SHARP S) to ss or SS. It may however map code points outside the Basic Latin block to code points
        // within it—for example, ſ (U+017F LATIN SMALL LETTER LONG S) case-folds to s (U+0073 LATIN SMALL LETTER S) and
        // K (U+212A KELVIN SIGN) case-folds to k (U+006B LATIN SMALL LETTER K). Strings containing those code points
        // are matched by regular expressions such as /[a-z]/ui.
        //
        // In case-insignificant matches when HasEitherUnicodeFlag(rer) is false, the mapping is based on Unicode
        // Default Case Conversion algorithm toUppercase rather than toCasefold, which results in some subtle
        // differences. For example, Ω (U+2126 OHM SIGN) is mapped by toUppercase to itself but by toCasefold to ω
        // (U+03C9 GREEK SMALL LETTER OMEGA) along with Ω (U+03A9 GREEK CAPITAL LETTER OMEGA), so "\u2126" is matched by
        // /[ω]/ui and /[\u03A9]/ui but not by /[ω]/i or /[\u03A9]/i. Also, no code point outside the Basic Latin block
        // is mapped to a code point within it, so strings such as "\u017F ſ" and "\u212A K" are not matched by
        // /[a-z]/i.

        if self.has_either_unicode_flag() && self.case == Case::Unimportant {
            return casefold_simple(ch);
        }
        if self.case == Case::Significant {
            return ch;
        }

        todo!()
    }
}

#[derive(Debug)]
pub(crate) struct RegExpObject {
    common: RefCell<CommonObjectData>,
    regexp_data: RefCell<RegExpData>,
}

impl<'a> From<&'a RegExpObject> for &'a dyn ObjectInterface {
    fn from(obj: &'a RegExpObject) -> Self {
        obj
    }
}

impl ObjectInterface for RegExpObject {
    fn common_object_data(&self) -> &RefCell<CommonObjectData> {
        &self.common
    }
    fn uses_ordinary_get_prototype_of(&self) -> bool {
        true
    }
    fn to_regexp_object(&self) -> Option<&RegExpObject> {
        Some(self)
    }
    fn is_regexp_object(&self) -> bool {
        true
    }

    // [[GetPrototypeOf]] ( )
    //
    // The [[GetPrototypeOf]] internal method of an ordinary object O takes no arguments. It performs the following
    // steps when called:
    //
    //  1. Return ! OrdinaryGetPrototypeOf(O).
    fn get_prototype_of(&self) -> Completion<Option<Object>> {
        Ok(ordinary_get_prototype_of(self))
    }

    // [[SetPrototypeOf]] ( V )
    //
    // The [[SetPrototypeOf]] internal method of an ordinary object O takes argument V (an Object or null). It performs
    // the following steps when called:
    //
    //  1. Return ! OrdinarySetPrototypeOf(O, V).
    fn set_prototype_of(&self, obj: Option<Object>) -> Completion<bool> {
        Ok(ordinary_set_prototype_of(self, obj))
    }

    // [[IsExtensible]] ( )
    //
    // The [[IsExtensible]] internal method of an ordinary object O takes no arguments. It performs the following steps
    // when called:
    //
    //  1. Return ! OrdinaryIsExtensible(O).
    fn is_extensible(&self) -> Completion<bool> {
        Ok(ordinary_is_extensible(self))
    }

    // [[PreventExtensions]] ( )
    //
    // The [[PreventExtensions]] internal method of an ordinary object O takes no arguments. It performs the following
    // steps when called:
    //
    //  1. Return ! OrdinaryPreventExtensions(O).
    fn prevent_extensions(&self) -> Completion<bool> {
        Ok(ordinary_prevent_extensions(self))
    }

    // [[GetOwnProperty]] ( P )
    //
    // The [[GetOwnProperty]] internal method of an ordinary object O takes argument P (a property key). It performs the
    // following steps when called:
    //
    //  1. Return ! OrdinaryGetOwnProperty(O, P).
    fn get_own_property(&self, key: &PropertyKey) -> Completion<Option<PropertyDescriptor>> {
        Ok(ordinary_get_own_property(self, key))
    }

    // [[DefineOwnProperty]] ( P, Desc )
    //
    // The [[DefineOwnProperty]] internal method of an ordinary object O takes arguments P (a property key) and Desc (a
    // Property Descriptor). It performs the following steps when called:
    //
    //  1. Return ? OrdinaryDefineOwnProperty(O, P, Desc).
    fn define_own_property(&self, key: PropertyKey, desc: PotentialPropertyDescriptor) -> Completion<bool> {
        ordinary_define_own_property(self, key, desc)
    }

    // [[HasProperty]] ( P )
    //
    // The [[HasProperty]] internal method of an ordinary object O takes argument P (a property key). It performs the
    // following steps when called:
    //
    //  1. Return ? OrdinaryHasProperty(O, P).
    fn has_property(&self, key: &PropertyKey) -> Completion<bool> {
        ordinary_has_property(self, key)
    }

    // [[Get]] ( P, Receiver )
    //
    // The [[Get]] internal method of an ordinary object O takes arguments P (a property key) and Receiver (an
    // ECMAScript language value). It performs the following steps when called:
    //
    //  1. Return ? OrdinaryGet(O, P, Receiver).
    fn get(&self, key: &PropertyKey, receiver: &ECMAScriptValue) -> Completion<ECMAScriptValue> {
        ordinary_get(self, key, receiver)
    }

    // [[Set]] ( P, V, Receiver )
    //
    // The [[Set]] internal method of an ordinary object O takes arguments P (a property key), V (an ECMAScript language
    // value), and Receiver (an ECMAScript language value). It performs the following steps when called:
    //
    //  1. Return ? OrdinarySet(O, P, V, Receiver).
    fn set(&self, key: PropertyKey, v: ECMAScriptValue, receiver: &ECMAScriptValue) -> Completion<bool> {
        ordinary_set(self, key, v, receiver)
    }

    // [[Delete]] ( P )
    //
    // The [[Delete]] internal method of an ordinary object O takes argument P (a property key). It performs the
    // following steps when called:
    //
    //  1. Return ? OrdinaryDelete(O, P).
    fn delete(&self, key: &PropertyKey) -> Completion<bool> {
        ordinary_delete(self, key)
    }

    // [[OwnPropertyKeys]] ( )
    //
    // The [[OwnPropertyKeys]] internal method of an ordinary object O takes no arguments. It performs the following
    // steps when called:
    //
    // 1. Return ! OrdinaryOwnPropertyKeys(O).
    fn own_property_keys(&self) -> Completion<Vec<PropertyKey>> {
        Ok(ordinary_own_property_keys(self))
    }

    fn kind(&self) -> ObjectTag {
        ObjectTag::RegExp
    }
}

impl RegExpObject {
    pub(crate) fn new(prototype: Option<Object>) -> Self {
        Self {
            common: RefCell::new(CommonObjectData::new(prototype, true, REGEXP_OBJECT_SLOTS)),
            regexp_data: RefCell::new(RegExpData {
                original_source: JSString::from(""),
                original_flags: JSString::from(""),
                reg_exp_record: RegExpRecord {
                    case: Case::Significant,
                    multiline: Lines::Single,
                    dot_all: false,
                    unicode: UnicodeMode::Denied,
                    unicode_sets: UnicodeSetsMode::Denied,
                    capturing_groups_count: 0,
                    has_group_names: false,
                    group_names: vec![],
                },
                reg_exp_matcher: None,
            }),
        }
    }
    pub(crate) fn object(prototype: Option<Object>) -> Object {
        Object { o: Rc::new(Self::new(prototype)) }
    }
}

impl Object {
    pub(crate) fn reg_exp_builtin_exec(&self, s: &JSString) -> Completion<Option<Object>> {
        // RegExpBuiltinExec ( R, S )
        //
        // The abstract operation RegExpBuiltinExec takes arguments R (an initialized RegExp instance) and S (a String)
        // and returns either a normal completion containing either an Array exotic object or null, or a throw
        // completion. It performs the following steps when called:
        //
        // 1. Let length be the length of S.
        // 2. Let lastIndex be ℝ(? ToLength(! Get(R, "lastIndex"))).
        // 3. Let flags be R.[[OriginalFlags]].
        // 4. If flags contains "g", let global be true; else let global be false.
        // 5. If flags contains "y", let sticky be true; else let sticky be false.
        // 6. If flags contains "d", let hasIndices be true; else let hasIndices be false.
        // 7. If global is false and sticky is false, set lastIndex to 0.
        // 8. Let matcher be R.[[RegExpMatcher]].
        // 9. If flags contains "u" or flags contains "v", let fullUnicode be true; else let fullUnicode be false.
        // 10. Let matchSucceeded be false.
        // 11. If fullUnicode is true, let input be StringToCodePoints(S); else let input be a List whose elements are
        //     the code units that are the elements of S.
        // 12. NOTE: Each element of input is considered to be a character.
        // 13. Repeat, while matchSucceeded is false,
        //     a. If lastIndex > length, then
        //        i. If global is true or sticky is true, then
        //           1. Perform ? Set(R, "lastIndex", +0𝔽, true).
        //        ii. Return null.
        //     b. Let inputIndex be the index into input of the character that was obtained from element lastIndex of S.
        //     c. Let r be matcher(input, inputIndex).
        //     d. If r is failure, then
        //        i. If sticky is true, then
        //           1. Perform ? Set(R, "lastIndex", +0𝔽, true).
        //           2. Return null.
        //        ii. Set lastIndex to AdvanceStringIndex(S, lastIndex, fullUnicode).
        //     e. Else,
        //        i. Assert: r is a MatchState.
        //        ii. Set matchSucceeded to true.
        // 14. Let e be r.[[EndIndex]].
        // 15. If fullUnicode is true, set e to GetStringIndex(S, e).
        // 16. If global is true or sticky is true, then
        //     a. Perform ? Set(R, "lastIndex", 𝔽(e), true).
        // 17. Let n be the number of elements in r.[[Captures]].
        // 18. Assert: n = R.[[RegExpRecord]].[[CapturingGroupsCount]].
        // 19. Assert: n < 2**32 - 1.
        // 20. Let A be ! ArrayCreate(n + 1).
        // 21. Assert: The mathematical value of A's "length" property is n + 1.
        // 22. Perform ! CreateDataPropertyOrThrow(A, "index", 𝔽(lastIndex)).
        // 23. Perform ! CreateDataPropertyOrThrow(A, "input", S).
        // 24. Let match be the Match Record { [[StartIndex]]: lastIndex, [[EndIndex]]: e }.
        // 25. Let indices be a new empty List.
        // 26. Let groupNames be a new empty List.
        // 27. Append match to indices.
        // 28. Let matchedSubstr be GetMatchString(S, match).
        // 29. Perform ! CreateDataPropertyOrThrow(A, "0", matchedSubstr).
        // 30. If R contains any GroupName, then
        //     a. Let groups be OrdinaryObjectCreate(null).
        //     b. Let hasGroups be true.
        // 31. Else,
        //     a. Let groups be undefined.
        //     b. Let hasGroups be false.
        // 32. Perform ! CreateDataPropertyOrThrow(A, "groups", groups).
        // 33. Let matchedGroupNames be a new empty List.
        // 34. For each integer i such that 1 ≤ i ≤ n, in ascending order, do
        //     a. Let captureI be ith element of r.[[Captures]].
        //     b. If captureI is undefined, then
        //        i. Let capturedValue be undefined.
        //        ii. Append undefined to indices.
        //     c. Else,
        //        i. Let captureStart be captureI.[[StartIndex]].
        //        ii. Let captureEnd be captureI.[[EndIndex]].
        //        iii. If fullUnicode is true, then
        //             1. Set captureStart to GetStringIndex(S, captureStart).
        //             2. Set captureEnd to GetStringIndex(S, captureEnd).
        //        iv. Let capture be the Match Record { [[StartIndex]]: captureStart, [[EndIndex]]: captureEnd }.
        //        v. Let capturedValue be GetMatchString(S, capture).
        //        vi. Append capture to indices.
        //     d. Perform ! CreateDataPropertyOrThrow(A, ! ToString(𝔽(i)), capturedValue).
        //     e. If the ith capture of R was defined with a GroupName, then
        //        i. Let s be the CapturingGroupName of that GroupName.
        //        ii. If matchedGroupNames contains s, then
        //            1. Assert: capturedValue is undefined.
        //            2. Append undefined to groupNames.
        //        iii. Else,
        //             1. If capturedValue is not undefined, append s to matchedGroupNames.
        //             2. NOTE: If there are multiple groups named s, groups may already have an s property at this
        //                point. However, because groups is an ordinary object whose properties are all writable data
        //                properties, the call to CreateDataPropertyOrThrow is nevertheless guaranteed to succeed.
        //             3. Perform ! CreateDataPropertyOrThrow(groups, s, capturedValue).
        //             4. Append s to groupNames.
        //     f. Else,
        //        i. Append undefined to groupNames.
        // 35. If hasIndices is true, then
        //     a. Let indicesArray be MakeMatchIndicesIndexPairArray(S, indices, groupNames, hasGroups).
        //     b. Perform ! CreateDataPropertyOrThrow(A, "indices", indicesArray).
        // 36. Return A.
        let length = s.len();
        let mut last_index =
            to_usize(self.get(&"lastIndex".into()).expect("spec says this shouldn't fail").to_length()?)
                .expect("toLength should return good integers");
        let r = self.o.to_regexp_object().expect("should only be called with regex objects");
        let re_data = r.regexp_data.borrow();
        let flags = &re_data.original_flags;
        let mut global = false;
        let mut sticky = false;
        let mut has_indices = false;
        let mut full_unicode = false;
        for ch in flags.as_slice() {
            match *ch {
                103 /* 'g' */ => { global = true; },
                121 /* 'y' */ => { sticky = true; },
                100 /* 'd' */ => { has_indices = true; },
                117 /* 'u' */ | 118 /* 'v' */ => { full_unicode = true; },
                _ => (),
            }
        }
        if !global && !sticky {
            last_index = 0;
        }
        let matcher = re_data.reg_exp_matcher.as_ref().expect("regular expression should be initialized");
        let (input, input_map) = if full_unicode {
            s.to_code_points_with_map()
        } else {
            (s.as_slice().iter().map(|ch| u32::from(*ch)).collect::<Vec<_>>(), (0..s.len()).collect::<Vec<_>>())
        };
        let map_len = input_map.len();
        let state = loop {
            if last_index > length {
                if global || sticky {
                    self.set("lastIndex", 0, true)?;
                }
                return Ok(None);
            }
            // b. Let inputIndex be the index into input of the character that was obtained from element lastIndex of S.
            let input_index = if last_index < map_len {
                input_map[last_index]
            } else {
                input_map[map_len - 1] + last_index - map_len + 1
            };
            let result = matcher.as_ref()(&input, input_index);
            match result {
                None => {
                    if sticky {
                        self.set("lastIndex", 0, true)?;
                        return Ok(None);
                    }
                    last_index = advance_string_index(s, last_index, full_unicode);
                }
                Some(match_state) => {
                    break match_state;
                }
            }
        };

        let end_index = { if full_unicode { s.get_index(state.end_index) } else { state.end_index } };
        if global || sticky {
            self.set("lastIndex", end_index, true)?;
        }

        let num_captures = state.captures.len();
        let array = array_create(to_f64(num_captures + 1).expect("numeric value is in range"), None)
            .expect("numeric value is in range");
        array.create_data_property_or_throw("index", last_index).expect(GOODOBJ);
        array.create_data_property_or_throw("input", s.clone()).expect(GOODOBJ);
        let complete_match = CaptureRange { start_index: last_index, end_index };
        let matched_substr = s.get_match_string(&complete_match);
        let mut indices = vec![Some(complete_match)];
        let mut group_names = vec![];
        array.create_data_property_or_throw("0", matched_substr).expect(GOODOBJ);
        let (groups, has_groups) = if re_data.reg_exp_record.has_group_names {
            (ECMAScriptValue::Object(ordinary_object_create(None)), true)
        } else {
            (ECMAScriptValue::Undefined, false)
        };
        array.create_data_property_or_throw("groups", groups.clone()).expect(GOODOBJ);
        let mut matched_group_names = vec![];
        for i in 1..=num_captures {
            let capture_i = state.captures[i].as_ref();
            let captured_value = if let Some(capture) = capture_i {
                let (capture_start, capture_end) = if full_unicode {
                    (s.get_index(capture.start_index), s.get_index(capture.end_index))
                } else {
                    (capture.start_index, capture.end_index)
                };
                let capture = CaptureRange { start_index: capture_start, end_index: capture_end };
                let captured_value = ECMAScriptValue::String(s.get_match_string(&capture));
                indices.push(Some(capture));
                captured_value
            } else {
                indices.push(None);
                ECMAScriptValue::Undefined
            };
            array.create_data_property_or_throw(i, captured_value.clone()).expect(GOODOBJ);
            if let Some(s) = &re_data.reg_exp_record.group_names[i - 1] {
                if matched_group_names.contains(s) {
                    group_names.push(None);
                } else {
                    if !captured_value.is_undefined() {
                        matched_group_names.push(s.clone());
                    }
                    groups
                        .object_ref()
                        .expect("groups should be an object if names are present")
                        .create_data_property_or_throw(s.clone(), captured_value)
                        .expect(GOODOBJ);
                    group_names.push(Some(s));
                }
            } else {
                group_names.push(None);
            }
        }
        if has_indices {
            let indices_array = make_match_indices_index_pair_array(s, &indices, &group_names, has_groups);
            array.create_data_property_or_throw("indices", indices_array).expect(GOODOBJ);
        }

        Ok(Some(array))
    }
}

fn make_match_indices_index_pair_array(
    s: &JSString,
    indices: &[Option<CaptureRange>],
    group_names: &[Option<&JSString>],
    has_groups: bool,
) -> Object {
    let n = indices.len();
    let array = array_create(to_f64(n).expect("length should be in range"), None).expect(GOODOBJ);
    let groups = if has_groups { Some(ordinary_object_create(None)) } else { None };
    array.create_data_property_or_throw("groups", groups.clone()).expect(GOODOBJ);
    for i in 0..n {
        let match_indices = indices[i].as_ref();
        let match_index_pair = match_indices.map(|range| s.get_match_index_pair(range));
        array.create_data_property_or_throw(i, match_index_pair.clone()).expect(GOODOBJ);
        if i > 0 {
            let gn = group_names[i - 1];
            if let Some(gn) = gn {
                groups
                    .as_ref()
                    .expect("groups should be an object if names are present")
                    .create_data_property_or_throw(gn, match_index_pair)
                    .expect(GOODOBJ);
            }
        }
    }
    array
}

fn advance_string_index(s: &JSString, index: usize, unicode: bool) -> usize {
    // AdvanceStringIndex ( S, index, unicode )
    //
    // The abstract operation AdvanceStringIndex takes arguments S (a String), index (a non-negative integer), and
    // unicode (a Boolean) and returns an integer. It performs the following steps when called:
    //
    // 1. Assert: index ≤ 2**53 - 1.
    // 2. If unicode is false, return index + 1.
    // 3. Let length be the length of S.
    // 4. If index + 1 ≥ length, return index + 1.
    // 5. Let cp be CodePointAt(S, index).
    // 6. Return index + cp.[[CodeUnitCount]].
    if unicode {
        let length = s.len();
        if index + 1 >= length {
            index + 1
        } else {
            let cp = code_point_at(s, index);
            index + cp.code_unit_count as usize
        }
    } else {
        index + 1
    }
}

impl JSString {
    fn get_index(&self, code_point_index: usize) -> usize {
        // GetStringIndex ( S, codePointIndex )
        //
        // The abstract operation GetStringIndex takes arguments S (a String) and codePointIndex (a non-negative
        // integer) and returns a non-negative integer. It interprets S as a sequence of UTF-16 encoded code points, as
        // described in 6.1.4, and returns the code unit index corresponding to code point index codePointIndex when
        // such an index exists. Otherwise, it returns the length of S. It performs the following steps when called:
        //
        // 1. If S is the empty String, return 0.
        // 2. Let len be the length of S.
        // 3. Let codeUnitCount be 0.
        // 4. Let codePointCount be 0.
        // 5. Repeat, while codeUnitCount < len,
        //    a. If codePointCount = codePointIndex, return codeUnitCount.
        //    b. Let cp be CodePointAt(S, codeUnitCount).
        //    c. Set codeUnitCount to codeUnitCount + cp.[[CodeUnitCount]].
        //    d. Set codePointCount to codePointCount + 1.
        // 6. Return len.
        if self.is_empty() {
            0
        } else {
            let len = self.len();
            let mut code_unit_count = 0;
            let mut code_point_count = 0;
            while code_unit_count < len {
                if code_point_count == code_point_index {
                    return code_unit_count;
                }
                let cp = code_point_at(self, code_unit_count);
                code_unit_count += usize::from(cp.code_unit_count);
                code_point_count += 1;
            }
            len
        }
    }

    fn get_match_string(&self, match_record: &CaptureRange) -> JSString {
        // GetMatchString ( S, match )
        //
        // The abstract operation GetMatchString takes arguments S (a String) and match (a Match Record) and returns a
        // String. It performs the following steps when called:
        //
        // 1. Assert: match.[[StartIndex]] ≤ match.[[EndIndex]] ≤ the length of S.
        // 2. Return the substring of S from match.[[StartIndex]] to match.[[EndIndex]].
        JSString::from(&self.as_slice()[match_record.start_index..match_record.end_index])
    }

    fn get_match_index_pair(&self, capture: &CaptureRange) -> Object {
        // GetMatchIndexPair ( S, match )
        //
        // The abstract operation GetMatchIndexPair takes arguments S (a String) and match (a Match Record) and returns
        // an Array. It performs the following steps when called:
        //
        // 1. Assert: match.[[StartIndex]] ≤ match.[[EndIndex]] ≤ the length of S.
        // 2. Return CreateArrayFromList(« 𝔽(match.[[StartIndex]]), 𝔽(match.[[EndIndex]]) »).
        assert!(capture.start_index <= capture.end_index);
        assert!(capture.end_index <= self.len());
        create_array_from_list(&[capture.start_index.into(), capture.end_index.into()])
    }
}

pub(crate) fn reg_exp_create(p: ECMAScriptValue, f: Option<JSString>) -> Completion<Object> {
    // RegExpCreate ( P, F )
    // The abstract operation RegExpCreate takes arguments P (an ECMAScript language value) and F (a String or
    // undefined) and returns either a normal completion containing an Object or a throw completion. It performs the
    // following steps when called:
    //
    //  1. Let obj be ! RegExpAlloc(%RegExp%).
    //  2. Return ? RegExpInitialize(obj, P, F).

    let obj = reg_exp_alloc(&intrinsic(IntrinsicId::RegExp)).expect(GOODCSTR);
    reg_exp_initialize(obj, p, ECMAScriptValue::from(f))
}

fn reg_exp_alloc(new_target: &Object) -> Completion<Object> {
    // RegExpAlloc ( newTarget )
    // The abstract operation RegExpAlloc takes argument newTarget (a constructor) and returns either a normal
    // completion containing an Object or a throw completion. It performs the following steps when called:
    //
    //  1. Let obj be ? OrdinaryCreateFromConstructor(newTarget, "%RegExp.prototype%", « [[OriginalSource]],
    //     [[OriginalFlags]], [[RegExpRecord]], [[RegExpMatcher]] »).
    //  2. Perform ! DefinePropertyOrThrow(obj, "lastIndex", PropertyDescriptor { [[Writable]]: true, [[Enumerable]]:
    //     false, [[Configurable]]: false }).
    //  3. Return obj.
    let obj = new_target.ordinary_create_from_constructor(IntrinsicId::RegExpPrototype, RegExpObject::object)?;
    define_property_or_throw(
        &obj,
        "lastIndex",
        PotentialPropertyDescriptor::new().writable(true).enumerable(false).configurable(false),
    )
    .expect(GOODOBJ);
    Ok(obj)
}

fn reg_exp_initialize(obj: Object, pattern: ECMAScriptValue, flags: ECMAScriptValue) -> Completion<Object> {
    // RegExpInitialize ( obj, pattern, flags )
    // The abstract operation RegExpInitialize takes arguments obj (an Object), pattern (an ECMAScript language value),
    // and flags (an ECMAScript language value) and returns either a normal completion containing an Object or a throw
    // completion. It performs the following steps when called:
    //
    //  1. If pattern is undefined, let P be the empty String.
    //  2. Else, let P be ? ToString(pattern).
    //  3. If flags is undefined, let F be the empty String.
    //  4. Else, let F be ? ToString(flags).
    //  5. If F contains any code unit other than "d", "g", "i", "m", "s", "u", "v", or "y", or if F contains any code
    //     unit more than once, throw a SyntaxError exception.
    //  6. If F contains "i", let i be true; else let i be false.
    //  7. If F contains "m", let m be true; else let m be false.
    //  8. If F contains "s", let s be true; else let s be false.
    //  9. If F contains "u", let u be true; else let u be false.
    //  10. If F contains "v", let v be true; else let v be false.
    //  11. If u is true or v is true, then
    //      a. Let patternText be StringToCodePoints(P).
    //  12. Else,
    //      a. Let patternText be the result of interpreting each of P's 16-bit elements as a Unicode BMP code point.
    //         UTF-16 decoding is not applied to the elements.
    //  13. Let parseResult be ParsePattern(patternText, u, v).
    //  14. If parseResult is a non-empty List of SyntaxError objects, throw a SyntaxError exception.
    //  15. Assert: parseResult is a Pattern Parse Node.
    //  16. Set obj.[[OriginalSource]] to P.
    //  17. Set obj.[[OriginalFlags]] to F.
    //  18. Let capturingGroupsCount be CountLeftCapturingParensWithin(parseResult).
    //  19. Let rer be the RegExp Record { [[IgnoreCase]]: i, [[Multiline]]: m, [[DotAll]]: s, [[Unicode]]: u,
    //      [[UnicodeSets]]: v, [[CapturingGroupsCount]]: capturingGroupsCount }.
    //  20. Set obj.[[RegExpRecord]] to rer.
    //  21. Set obj.[[RegExpMatcher]] to CompilePattern of parseResult with argument rer.
    //  22. Perform ? Set(obj, "lastIndex", +0𝔽, true).
    //  23. Return obj.
    let p = if pattern.is_undefined() { JSString::from("") } else { to_string(pattern)? };
    let f = if flags.is_undefined() { JSString::from("") } else { to_string(flags)? };
    let case = if f.contains('i' as u16) { Case::Unimportant } else { Case::Significant };
    let multiline = if f.contains('m' as u16) { Lines::Multi } else { Lines::Single };
    let dot_all = f.contains('s' as u16);
    let unicode = if f.contains('u' as u16) { UnicodeMode::Allowed } else { UnicodeMode::Denied };
    let unicode_sets = if f.contains('v' as u16) { UnicodeSetsMode::Allowed } else { UnicodeSetsMode::Denied };
    let flags_count = usize::from(case == Case::Unimportant)
        + usize::from(multiline == Lines::Multi)
        + usize::from(dot_all)
        + usize::from(unicode == UnicodeMode::Allowed)
        + usize::from(unicode_sets == UnicodeSetsMode::Allowed)
        + usize::from(f.contains('d' as u16))
        + usize::from(f.contains('g' as u16))
        + usize::from(f.contains('y' as u16));
    if f.len() != flags_count {
        return Err(create_syntax_error(format!("invalid flags for regular expression: {f}"), None));
    }
    let pattern_text = p.to_string();
    let parse_result = parse::parse_pattern(&pattern_text, unicode, unicode_sets).map_err(|errs| {
        create_syntax_error(
            errs.into_iter()
                .map(|err| to_string(ECMAScriptValue::Object(err)).expect("errs have representation"))
                .join("\n"),
            None,
        )
    })?;

    let regex_obj = obj.o.to_regexp_object().expect("this should be a regex obj");
    {
        let mut regex_data = regex_obj.regexp_data.borrow_mut();
        regex_data.original_source = p;
        regex_data.original_flags = f;
        regex_data.reg_exp_record = RegExpRecord {
            case,
            multiline,
            dot_all,
            unicode,
            unicode_sets,
            capturing_groups_count: parse_result.count_left_capturing_parens_within(),
            has_group_names: !parse_result.all_group_specifiers().is_empty(),
            group_names: parse_result
                .group_name_associations()
                .into_iter()
                .map(|gs| gs.map(GroupName::capturing_group_name))
                .collect(),
        };
        let matcher = parse_result.compile_pattern(&regex_data.reg_exp_record);
        regex_data.reg_exp_matcher = Some(matcher);
    }

    obj.set("lastIndex", 0, true)?;

    Ok(obj)
}

impl ECMAScriptValue {
    pub(crate) fn is_regexp(&self) -> Completion<bool> {
        // IsRegExp ( argument )
        //
        // The abstract operation IsRegExp takes argument argument (an ECMAScript language value) and returns either a
        // normal completion containing a Boolean or a throw completion. It performs the following steps when called:
        //
        // 1. If argument is not an Object, return false.
        // 2. Let matcher be ? Get(argument, %Symbol.match%).
        // 3. If matcher is not undefined, return ToBoolean(matcher).
        // 4. If argument has a [[RegExpMatcher]] internal slot, return true.
        // 5. Return false.
        match self.object_ref() {
            Some(obj) => {
                let key = PropertyKey::from(wks(WksId::Match));
                let matcher = obj.get(&key)?;
                match matcher {
                    ECMAScriptValue::Undefined => Ok(obj.o.is_regexp_object()),
                    _ => Ok(matcher.into_boolean()),
                }
            }
            None => Ok(false),
        }
    }

    pub(crate) fn to_regexp_object(&self) -> Option<&RegExpObject> {
        match self {
            ECMAScriptValue::Object(obj) => obj.o.to_regexp_object(),
            _ => None,
        }
    }
}

fn regexp_constructor_function(
    _this_value: &ECMAScriptValue,
    new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // RegExp ( pattern, flags )
    // This function performs the following steps when called:
    //
    // 1. Let patternIsRegExp be ? IsRegExp(pattern).
    // 2. If NewTarget is undefined, then
    //    a. Let newTarget be the active function object.
    //    b. If patternIsRegExp is true and flags is undefined, then
    //       i. Let patternConstructor be ? Get(pattern, "constructor").
    //       ii. If SameValue(newTarget, patternConstructor) is true, return pattern.
    // 3. Else,
    //    a. Let newTarget be NewTarget.
    // 4. If pattern is an Object and pattern has a [[RegExpMatcher]] internal slot, then
    //    a. Let P be pattern.[[OriginalSource]].
    //    b. If flags is undefined, let F be pattern.[[OriginalFlags]].
    //    c. Else, let F be flags.
    // 5. Else if patternIsRegExp is true, then
    //    a. Let P be ? Get(pattern, "source").
    //    b. If flags is undefined, then
    //       i. Let F be ? Get(pattern, "flags").
    //    c. Else,
    //       i. Let F be flags.
    // 6. Else,
    //    a. Let P be pattern.
    //    b. Let F be flags.
    // 7. Let O be ? RegExpAlloc(newTarget).
    // 8. Return ? RegExpInitialize(O, P, F).
    //
    // Note: If pattern is supplied using a StringLiteral, the usual escape sequence substitutions are performed before
    // the String is processed by this function. If pattern must contain an escape sequence to be recognized by this
    // function, any U+005C (REVERSE SOLIDUS) code points must be escaped within the StringLiteral to prevent them being
    // removed when the contents of the StringLiteral are formed.
    let mut args = FuncArgs::from(arguments);
    let pattern = args.next_arg();
    let flags = args.next_arg();

    let pattern_is_reg_exp = pattern.is_regexp()?;
    let ref_holder;

    let nt = match new_target {
        Some(object) => object,
        None => {
            ref_holder =
                active_function_object().expect("the current constructor should be the active function object");
            if pattern_is_reg_exp && flags.is_undefined() {
                let key = PropertyKey::from("constructor");
                let pattern_constructor = pattern.get(&key)?;
                if pattern_constructor.same_value(&ECMAScriptValue::Object(ref_holder.clone())) {
                    return Ok(pattern);
                }
            }
            &ref_holder
        }
    };

    let (p, f) = if let Some(pattern) = pattern.to_regexp_object() {
        let data = pattern.regexp_data.borrow();
        let p = ECMAScriptValue::String(data.original_source.clone());
        let f = if flags.is_undefined() { ECMAScriptValue::String(data.original_flags.clone()) } else { flags };
        (p, f)
    } else if pattern_is_reg_exp {
        let source_key = PropertyKey::from("source");
        let p = pattern.get(&source_key)?;
        let flags_key = PropertyKey::from("flags");
        let f = if flags.is_undefined() { pattern.get(&flags_key)? } else { flags };
        (p, f)
    } else {
        (pattern, flags)
    };

    let obj = reg_exp_alloc(nt)?;
    reg_exp_initialize(obj, p, f).map(ECMAScriptValue::Object)
}

#[expect(clippy::unnecessary_wraps)]
fn regexp_species(
    this_value: &ECMAScriptValue,
    _: Option<&Object>,
    _: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // get RegExp [ %Symbol.species% ]
    // RegExp[%Symbol.species%] is an accessor property whose set accessor function is undefined. Its get accessor
    // function performs the following steps when called:
    //
    // 1. Return the this value.
    //
    // Note: RegExp prototype methods normally use their this value's constructor to create a derived object. However, a
    // subclass constructor may over-ride that default behaviour by redefining its %Symbol.species% property.
    Ok(this_value.clone())
}

fn regexp_prototype_exec(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // RegExp.prototype.exec ( string )
    //
    // This method searches string for an occurrence of the regular expression pattern and returns an Array containing
    // the results of the match, or null if string did not match.
    //
    // It performs the following steps when called:
    //
    // 1. Let R be the this value.
    // 2. Perform ? RequireInternalSlot(R, [[RegExpMatcher]]).
    // 3. Let S be ? ToString(string).
    // 4. Return ? RegExpBuiltinExec(R, S).
    let mut args = FuncArgs::from(arguments);
    let string = args.next_arg();
    let _ = this_value.to_regexp_object().ok_or_else(|| create_type_error("RegExp.prototype.exec: bad receiver"))?;
    let r = to_object(this_value.clone()).expect("this should not have changed since prior line");
    let s = to_string(string)?;
    let result = r.reg_exp_builtin_exec(&s)?;
    Ok(ECMAScriptValue::to_obj_or_null(result))
}

fn regexp_prototype_test(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // RegExp.prototype.test ( S )
    // This method performs the following steps when called:
    //
    // 1. Let R be the this value.
    // 2. If R is not an Object, throw a TypeError exception.
    // 3. Let string be ? ToString(S).
    // 4. Let match be ? RegExpExec(R, string).
    // 5. If match is null, return false.
    // 6. Return true.
    let mut args = FuncArgs::from(arguments);
    let s = args.next_arg();

    let _ = this_value
        .object_ref()
        .ok_or_else(|| create_type_error("RegExp.prototype.test requires an object for 'this'"))?;

    let string = to_string(s)?;
    let matched = reg_exp_exec(this_value, string)?;
    Ok(matched.is_some().into())
}

fn regexp_prototype_to_string(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}

fn regexp_prototype_match(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}

fn regexp_prototype_match_all(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}

fn regexp_prototype_replace(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}

fn regexp_prototype_search(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}

fn regexp_prototyep_split(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}

fn regexp_prototype_dotall(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}

fn regexp_prototype_flags(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}

fn regexp_prototype_global(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}

fn regexp_prototype_has_indices(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}

fn regexp_prototype_ignore_case(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}

fn regexp_prototype_multiline(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}

fn regexp_prototype_source(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}

fn regexp_prototype_sticky(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}

fn regexp_prototype_unicode(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}

fn regexp_prototype_unicode_sets(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}

fn reg_exp_exec(r: &ECMAScriptValue, s: JSString) -> Completion<Option<Object>> {
    // RegExpExec ( R, S )
    //
    // The abstract operation RegExpExec takes arguments R (an Object) and S (a String) and returns either a normal
    // completion containing either an Object or null, or a throw completion. It performs the following steps when
    // called:
    //
    // 1. Let exec be ? Get(R, "exec").
    // 2. If IsCallable(exec) is true, then
    //    a. Let result be ? Call(exec, R, « S »).
    //    b. If result is not an Object and result is not null, throw a TypeError exception.
    //    c. Return result.
    // 3. Perform ? RequireInternalSlot(R, [[RegExpMatcher]]).
    // 4. Return ? RegExpBuiltinExec(R, S).
    //
    // Note: If a callable "exec" property is not found this algorithm falls back to attempting to use the built-in
    // RegExp matching algorithm. This provides compatible behaviour for code written for prior editions where most
    // built-in algorithms that use regular expressions did not perform a dynamic property lookup of "exec".
    let exec = r.get(&"exec".into())?;
    if exec.is_callable() {
        let result = call(&exec, r, &[s.into()])?;
        let result: Option<Object> = result
            .try_into()
            .map_err(|_| create_type_error("RegExp exec method returned something other than an Object or null"))?;
        Ok(result)
    } else {
        let _ = r.to_regexp_object().ok_or_else(|| create_type_error("RegExp exec called on incompatible receiver"))?;
        let reg = to_object(r.clone()).expect("things shouldn't have changed since prior line");
        reg.reg_exp_builtin_exec(&s)
    }
}

mod casefold;
mod compile;
mod parse;
