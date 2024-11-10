use super::*;

pub fn provision_regexp_intrinsic(realm: &Rc<RefCell<Realm>>) {
    let object_prototype = realm.borrow().intrinsics.object_prototype.clone();
    let function_prototype = realm.borrow().intrinsics.function_prototype.clone();

    let regexp_prototype = ordinary_object_create(Some(object_prototype));
    realm.borrow_mut().intrinsics.reg_exp_prototype = regexp_prototype.clone();

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
}

#[derive(Debug)]
#[allow(clippy::struct_excessive_bools)]
pub struct RegExpData {
    original_source: JSString,
    original_flags: JSString,
    ignore_case: bool,
    multiline: bool,
    unicode: bool,
    unicode_sets: bool,
    capturing_groups_count: usize,
    reg_exp_matcher: Option<parse::Pattern>,
}

#[derive(Debug)]
pub struct RegExpObject {
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
}

impl RegExpObject {
    pub fn new(prototype: Option<Object>) -> Self {
        Self {
            common: RefCell::new(CommonObjectData::new(prototype, true, REGEXP_OBJECT_SLOTS)),
            regexp_data: RefCell::new(RegExpData {
                original_source: JSString::from(""),
                original_flags: JSString::from(""),
                ignore_case: false,
                multiline: false,
                unicode: false,
                unicode_sets: false,
                capturing_groups_count: 0,
                reg_exp_matcher: None,
            }),
        }
    }
    pub fn object(prototype: Option<Object>) -> Object {
        Object { o: Rc::new(Self::new(prototype)) }
    }
}

pub fn reg_exp_create(p: ECMAScriptValue, f: Option<JSString>) -> Completion<Object> {
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
    let ignore_case = f.contains('i' as u16);
    let multiline = f.contains('m' as u16);
    let dot_all = f.contains('s' as u16);
    let unicode = f.contains('u' as u16);
    let unicode_sets = f.contains('v' as u16);
    let flags_count = usize::from(ignore_case)
        + usize::from(multiline)
        + usize::from(dot_all)
        + usize::from(unicode)
        + usize::from(unicode_sets)
        + usize::from(f.contains('d' as u16))
        + usize::from(f.contains('g' as u16))
        + usize::from(f.contains('y' as u16));
    if f.len() != flags_count {
        return Err(create_syntax_error(format!("invalid flags for regular expression: {f}"), None));
    }
    let pattern_text = p.to_string();
    let parse_result = parse::parse_pattern(&pattern_text, unicode, unicode_sets)?;

    let regex_obj = obj.o.to_regexp_object().expect("this should be a regex obj");
    {
        let mut regex_data = regex_obj.regexp_data.borrow_mut();
        regex_data.original_source = p;
        regex_data.original_flags = f;
        regex_data.ignore_case = ignore_case;
        regex_data.multiline = multiline;
        regex_data.unicode = unicode;
        regex_data.unicode_sets = unicode_sets;
        regex_data.capturing_groups_count = parse_result.count_left_capturing_parens_within();
        regex_data.reg_exp_matcher = Some(parse_result);
    }

    obj.set("lastIndex", 0, true)?;

    Ok(obj)
}

fn regexp_constructor_function(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}

fn regexp_species(
    this_value: &ECMAScriptValue,
    _: Option<&Object>,
    _: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // get RegExp [ %Symbol.species% ]
    // RegExp[%Symbol.species%] is an accessor property whose set accessor function is undefined. Its get
    // accessor function performs the following steps when called:
    //
    // 1. Return the this value.
    //
    // Note
    // RegExp prototype methods normally use their this value's constructor to create a derived object.
    // However, a subclass constructor may over-ride that default behaviour by redefining its %Symbol.species%
    // property.
    Ok(this_value.clone())
}

mod parse;
