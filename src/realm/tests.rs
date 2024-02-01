use super::*;
use crate::tests::*;
use ahash::RandomState;
use test_case::test_case;

const ALL_INTRINSIC_IDS: &[IntrinsicId] = &[
    IntrinsicId::Array,
    IntrinsicId::ArrayPrototype,
    IntrinsicId::ArrayPrototypeValues,
    IntrinsicId::ArrayIteratorPrototype,
    IntrinsicId::Boolean,
    IntrinsicId::BooleanPrototype,
    IntrinsicId::DecodeURI,
    IntrinsicId::DecodeURIComponent,
    IntrinsicId::EncodeURI,
    IntrinsicId::EncodeURIComponent,
    IntrinsicId::Error,
    IntrinsicId::ErrorPrototype,
    IntrinsicId::Eval,
    IntrinsicId::EvalError,
    IntrinsicId::EvalErrorPrototype,
    IntrinsicId::ForInIteratorPrototype,
    IntrinsicId::Function,
    IntrinsicId::FunctionPrototype,
    IntrinsicId::IsFinite,
    IntrinsicId::IsNaN,
    IntrinsicId::IteratorPrototype,
    IntrinsicId::GeneratorFunction,
    IntrinsicId::GeneratorFunctionPrototype,
    IntrinsicId::GeneratorFunctionPrototypePrototype,
    IntrinsicId::GeneratorFunctionPrototypePrototypeNext,
    IntrinsicId::Math,
    IntrinsicId::Object,
    IntrinsicId::ObjectPrototype,
    IntrinsicId::ObjectPrototypeToString,
    IntrinsicId::ParseFloat,
    IntrinsicId::ParseInt,
    IntrinsicId::Number,
    IntrinsicId::NumberPrototype,
    IntrinsicId::RangeError,
    IntrinsicId::RangeErrorPrototype,
    IntrinsicId::ReferenceError,
    IntrinsicId::ReferenceErrorPrototype,
    IntrinsicId::String,
    IntrinsicId::StringPrototype,
    IntrinsicId::Symbol,
    IntrinsicId::SymbolPrototype,
    IntrinsicId::SyntaxError,
    IntrinsicId::SyntaxErrorPrototype,
    IntrinsicId::ThrowTypeError,
    IntrinsicId::TypeError,
    IntrinsicId::TypeErrorPrototype,
    IntrinsicId::URIError,
    IntrinsicId::URIErrorPrototype,
];
#[test]
fn intrinsic_id_debug() {
    for id in ALL_INTRINSIC_IDS {
        assert_ne!(format!("{id:?}"), "");
    }
}
#[test]
fn intrinsic_id_eq() {
    for (right_idx, right_value) in ALL_INTRINSIC_IDS.iter().enumerate() {
        for (left_idx, left_value) in ALL_INTRINSIC_IDS.iter().enumerate() {
            assert_eq!(*left_value == *right_value, left_idx == right_idx);
        }
    }
}
#[test]
fn intrinsic_id_hash() {
    let factory = RandomState::new();
    for (right_idx, right_value) in ALL_INTRINSIC_IDS.iter().enumerate() {
        for (left_idx, left_value) in ALL_INTRINSIC_IDS.iter().enumerate() {
            assert_eq!(
                calculate_hash(&factory, &left_value) == calculate_hash(&factory, &right_value),
                left_idx == right_idx
            );
        }
    }
}
#[test]
#[allow(clippy::clone_on_copy)]
fn intrinsic_id_clone() {
    let id1 = IntrinsicId::Object;
    let id2 = id1.clone();
    assert_eq!(id1, id2);
}

mod intrinsics {
    use super::*;
    use test_case::test_case;

    #[test]
    fn debug() {
        setup_test_agent();
        assert_ne!(format!("{:?}", Intrinsics::new()), "")
    }

    #[test]
    fn get() {
        setup_test_agent();
        let realm_ptr = current_realm_record().unwrap();
        let realm = realm_ptr.borrow();
        let intrinsics = &realm.intrinsics;
        assert_eq!(intrinsics.get(IntrinsicId::Array), intrinsics.array);
        assert_eq!(intrinsics.get(IntrinsicId::ArrayPrototype), intrinsics.array_prototype);
        assert_eq!(intrinsics.get(IntrinsicId::ArrayPrototypeValues), intrinsics.array_prototype_values);
        assert_eq!(intrinsics.get(IntrinsicId::ArrayIteratorPrototype), intrinsics.array_iterator_prototype);
        assert_eq!(intrinsics.get(IntrinsicId::Boolean), intrinsics.boolean);
        assert_eq!(intrinsics.get(IntrinsicId::BooleanPrototype), intrinsics.boolean_prototype);
        assert_eq!(intrinsics.get(IntrinsicId::DecodeURI), intrinsics.decode_uri);
        assert_eq!(intrinsics.get(IntrinsicId::DecodeURIComponent), intrinsics.decode_uri_component);
        assert_eq!(intrinsics.get(IntrinsicId::EncodeURI), intrinsics.encode_uri);
        assert_eq!(intrinsics.get(IntrinsicId::EncodeURIComponent), intrinsics.encode_uri_component);
        assert_eq!(intrinsics.get(IntrinsicId::Error), intrinsics.error);
        assert_eq!(intrinsics.get(IntrinsicId::ErrorPrototype), intrinsics.error_prototype);
        assert_eq!(intrinsics.get(IntrinsicId::Eval), intrinsics.eval);
        assert_eq!(intrinsics.get(IntrinsicId::EvalError), intrinsics.eval_error);
        assert_eq!(intrinsics.get(IntrinsicId::EvalErrorPrototype), intrinsics.eval_error_prototype);
        assert_eq!(intrinsics.get(IntrinsicId::ForInIteratorPrototype), intrinsics.for_in_iterator_prototype);
        assert_eq!(intrinsics.get(IntrinsicId::Function), intrinsics.function);
        assert_eq!(intrinsics.get(IntrinsicId::FunctionPrototype), intrinsics.function_prototype);
        assert_eq!(intrinsics.get(IntrinsicId::IsFinite), intrinsics.is_finite);
        assert_eq!(intrinsics.get(IntrinsicId::IsNaN), intrinsics.is_nan);
        assert_eq!(intrinsics.get(IntrinsicId::IteratorPrototype), intrinsics.iterator_prototype);
        assert_eq!(intrinsics.get(IntrinsicId::GeneratorFunction), intrinsics.generator_function);
        assert_eq!(intrinsics.get(IntrinsicId::GeneratorFunctionPrototype), intrinsics.generator_function_prototype);
        assert_eq!(
            intrinsics.get(IntrinsicId::GeneratorFunctionPrototypePrototype),
            intrinsics.generator_function_prototype_prototype
        );
        assert_eq!(
            intrinsics.get(IntrinsicId::GeneratorFunctionPrototypePrototypeNext),
            intrinsics.generator_function_prototype_prototype_next
        );
        assert_eq!(intrinsics.get(IntrinsicId::Number), intrinsics.number);
        assert_eq!(intrinsics.get(IntrinsicId::NumberPrototype), intrinsics.number_prototype);
        assert_eq!(intrinsics.get(IntrinsicId::Object), intrinsics.object);
        assert_eq!(intrinsics.get(IntrinsicId::ObjectPrototype), intrinsics.object_prototype);
        assert_eq!(intrinsics.get(IntrinsicId::ObjectPrototypeToString), intrinsics.object_prototype_to_string);
        assert_eq!(intrinsics.get(IntrinsicId::ParseFloat), intrinsics.parse_float);
        assert_eq!(intrinsics.get(IntrinsicId::ParseInt), intrinsics.parse_int);
        assert_eq!(intrinsics.get(IntrinsicId::RangeError), intrinsics.range_error);
        assert_eq!(intrinsics.get(IntrinsicId::RangeErrorPrototype), intrinsics.range_error_prototype);
        assert_eq!(intrinsics.get(IntrinsicId::ReferenceError), intrinsics.reference_error);
        assert_eq!(intrinsics.get(IntrinsicId::ReferenceErrorPrototype), intrinsics.reference_error_prototype);
        assert_eq!(intrinsics.get(IntrinsicId::String), intrinsics.string);
        assert_eq!(intrinsics.get(IntrinsicId::StringPrototype), intrinsics.string_prototype);
        assert_eq!(intrinsics.get(IntrinsicId::Symbol), intrinsics.symbol);
        assert_eq!(intrinsics.get(IntrinsicId::SymbolPrototype), intrinsics.symbol_prototype);
        assert_eq!(intrinsics.get(IntrinsicId::SyntaxError), intrinsics.syntax_error);
        assert_eq!(intrinsics.get(IntrinsicId::SyntaxErrorPrototype), intrinsics.syntax_error_prototype);
        assert_eq!(intrinsics.get(IntrinsicId::ThrowTypeError), intrinsics.throw_type_error);
        assert_eq!(intrinsics.get(IntrinsicId::TypeError), intrinsics.type_error);
        assert_eq!(intrinsics.get(IntrinsicId::TypeErrorPrototype), intrinsics.type_error_prototype);
        assert_eq!(intrinsics.get(IntrinsicId::URIError), intrinsics.uri_error);
        assert_eq!(intrinsics.get(IntrinsicId::URIErrorPrototype), intrinsics.uri_error_prototype);
    }

    #[test_case(|| intrinsic(IntrinsicId::Array) => Some(IntrinsicId::Array); "id: Array")]
    #[test_case(|| intrinsic(IntrinsicId::ArrayPrototype) => Some(IntrinsicId::ArrayPrototype); "id: ArrayPrototype")]
    #[test_case(|| intrinsic(IntrinsicId::ArrayPrototypeValues) => Some(IntrinsicId::ArrayPrototypeValues); "id: ArrayPrototypeValues")]
    #[test_case(|| intrinsic(IntrinsicId::ArrayIteratorPrototype) => Some(IntrinsicId::ArrayIteratorPrototype); "id: ArrayIteratorPrototype")]
    #[test_case(|| intrinsic(IntrinsicId::Boolean) => Some(IntrinsicId::Boolean); "id: Boolean")]
    #[test_case(|| intrinsic(IntrinsicId::BooleanPrototype) => Some(IntrinsicId::BooleanPrototype); "id: BooleanPrototype")]
    #[test_case(|| intrinsic(IntrinsicId::DecodeURI) => Some(IntrinsicId::DecodeURI); "id: DecodeURI")]
    #[test_case(|| intrinsic(IntrinsicId::DecodeURIComponent) => Some(IntrinsicId::DecodeURIComponent); "id: DecodeURIComponent")]
    #[test_case(|| intrinsic(IntrinsicId::EncodeURI) => Some(IntrinsicId::EncodeURI); "id: EncodeURI")]
    #[test_case(|| intrinsic(IntrinsicId::EncodeURIComponent) => Some(IntrinsicId::EncodeURIComponent); "id: EncodeURIComponent")]
    #[test_case(|| intrinsic(IntrinsicId::Error) => Some(IntrinsicId::Error); "id: Error")]
    #[test_case(|| intrinsic(IntrinsicId::ErrorPrototype) => Some(IntrinsicId::ErrorPrototype); "id: ErrorPrototype")]
    #[test_case(|| intrinsic(IntrinsicId::Eval) => Some(IntrinsicId::Eval); "id: Eval")]
    #[test_case(|| intrinsic(IntrinsicId::EvalError) => Some(IntrinsicId::EvalError); "id: EvalError")]
    #[test_case(|| intrinsic(IntrinsicId::EvalErrorPrototype) => Some(IntrinsicId::EvalErrorPrototype); "id: EvalErrorPrototype")]
    #[test_case(|| intrinsic(IntrinsicId::ForInIteratorPrototype) => Some(IntrinsicId::ForInIteratorPrototype); "id: ForInIteratorPrototype")]
    #[test_case(|| intrinsic(IntrinsicId::Function) => Some(IntrinsicId::Function); "id: Function")]
    #[test_case(|| intrinsic(IntrinsicId::FunctionPrototype) => Some(IntrinsicId::FunctionPrototype); "id: FunctionPrototype")]
    #[test_case(|| intrinsic(IntrinsicId::IsFinite) => Some(IntrinsicId::IsFinite); "id: IsFinite")]
    #[test_case(|| intrinsic(IntrinsicId::IsNaN) => Some(IntrinsicId::IsNaN); "id: IsNaN")]
    #[test_case(|| intrinsic(IntrinsicId::IteratorPrototype) => Some(IntrinsicId::IteratorPrototype); "id: IteratorPrototype")]
    #[test_case(|| intrinsic(IntrinsicId::GeneratorFunction) => Some(IntrinsicId::GeneratorFunction); "id: GeneratorFunction")]
    #[test_case(|| intrinsic(IntrinsicId::GeneratorFunctionPrototype) => Some(IntrinsicId::GeneratorFunctionPrototype); "id: GeneratorFunctionPrototype")]
    #[test_case(|| intrinsic(IntrinsicId::GeneratorFunctionPrototypePrototype) => Some(IntrinsicId::GeneratorFunctionPrototypePrototype); "id: GeneratorFunctionPrototypePrototype")]
    #[test_case(|| intrinsic(IntrinsicId::GeneratorFunctionPrototypePrototypeNext) => Some(IntrinsicId::GeneratorFunctionPrototypePrototypeNext); "id: GeneratorFunctionPrototypePrototypeNext")]
    #[test_case(|| intrinsic(IntrinsicId::Number) => Some(IntrinsicId::Number); "id: Number")]
    #[test_case(|| intrinsic(IntrinsicId::NumberPrototype) => Some(IntrinsicId::NumberPrototype); "id: NumberPrototype")]
    #[test_case(|| intrinsic(IntrinsicId::Object) => Some(IntrinsicId::Object); "id: Object")]
    #[test_case(|| intrinsic(IntrinsicId::ObjectPrototype) => Some(IntrinsicId::ObjectPrototype); "id: ObjectPrototype")]
    #[test_case(|| intrinsic(IntrinsicId::ObjectPrototypeToString) => Some(IntrinsicId::ObjectPrototypeToString); "id: ObjectPrototypeToString")]
    #[test_case(|| intrinsic(IntrinsicId::ParseFloat) => Some(IntrinsicId::ParseFloat); "id: ParseFloat")]
    #[test_case(|| intrinsic(IntrinsicId::ParseInt) => Some(IntrinsicId::ParseInt); "id: ParseInt")]
    #[test_case(|| intrinsic(IntrinsicId::RangeError) => Some(IntrinsicId::RangeError); "id: RangeError")]
    #[test_case(|| intrinsic(IntrinsicId::RangeErrorPrototype) => Some(IntrinsicId::RangeErrorPrototype); "id: RangeErrorPrototype")]
    #[test_case(|| intrinsic(IntrinsicId::ReferenceError) => Some(IntrinsicId::ReferenceError); "id: ReferenceError")]
    #[test_case(|| intrinsic(IntrinsicId::ReferenceErrorPrototype) => Some(IntrinsicId::ReferenceErrorPrototype); "id: ReferenceErrorPrototype")]
    #[test_case(|| intrinsic(IntrinsicId::String) => Some(IntrinsicId::String); "id: String")]
    #[test_case(|| intrinsic(IntrinsicId::StringPrototype) => Some(IntrinsicId::StringPrototype); "id: StringPrototype")]
    #[test_case(|| intrinsic(IntrinsicId::Symbol) => Some(IntrinsicId::Symbol); "id: Symbol")]
    #[test_case(|| intrinsic(IntrinsicId::SymbolPrototype) => Some(IntrinsicId::SymbolPrototype); "id: SymbolPrototype")]
    #[test_case(|| intrinsic(IntrinsicId::SyntaxError) => Some(IntrinsicId::SyntaxError); "id: SyntaxError")]
    #[test_case(|| intrinsic(IntrinsicId::SyntaxErrorPrototype) => Some(IntrinsicId::SyntaxErrorPrototype); "id: SyntaxErrorPrototype")]
    #[test_case(|| intrinsic(IntrinsicId::ThrowTypeError) => Some(IntrinsicId::ThrowTypeError); "id: ThrowTypeError")]
    #[test_case(|| intrinsic(IntrinsicId::TypeError) => Some(IntrinsicId::TypeError); "id: TypeError")]
    #[test_case(|| intrinsic(IntrinsicId::TypeErrorPrototype) => Some(IntrinsicId::TypeErrorPrototype); "id: TypeErrorPrototype")]
    #[test_case(|| intrinsic(IntrinsicId::URIError) => Some(IntrinsicId::URIError); "id: URIError")]
    #[test_case(|| intrinsic(IntrinsicId::URIErrorPrototype) => Some(IntrinsicId::URIErrorPrototype); "id: URIErrorPrototype")]
    #[test_case(|| ordinary_object_create(None, &[]) => None; "not an intrinsic")]
    fn which(make_object: fn() -> Object) -> Option<IntrinsicId> {
        setup_test_agent();
        let obj = make_object();
        let realm_ref = current_realm_record().unwrap();
        let realm = realm_ref.borrow();
        realm.intrinsics.which(&obj)
    }
}

#[test]
fn realm_debug() {
    setup_test_agent();
    let realm_ptr = current_realm_record().unwrap();
    let realm = realm_ptr.borrow();
    assert_ne!(format!("{realm:?}"), "");
}

#[test]
fn throw_type_error_test() {
    setup_test_agent();
    let err = throw_type_error(ECMAScriptValue::Undefined, None, &[]).unwrap_err();
    let msg = unwind_type_error(err);
    assert_eq!(msg, "Generic TypeError");
}

#[test_case(super::decode_uri => panics "not yet implemented"; "decode_uri")]
#[test_case(super::decode_uri_component => panics "not yet implemented"; "decode_uri_component")]
#[test_case(super::encode_uri => panics "not yet implemented"; "encode_uri")]
#[test_case(super::encode_uri_component => panics "not yet implemented"; "encode_uri_component")]
#[test_case(super::parse_float => panics "not yet implemented"; "parse_float")]
#[test_case(super::parse_int => panics "not yet implemented"; "parse_int")]
fn todo(f: fn(ECMAScriptValue, Option<&Object>, &[ECMAScriptValue]) -> Completion<ECMAScriptValue>) {
    setup_test_agent();
    f(ECMAScriptValue::Undefined, None, &[]).unwrap();
}

#[test_case(
    || wks(WksId::Unscopables)
    => serr("TypeError: Symbol values cannot be converted to Number values");
    "to_number fails"
)]
#[test_case(|| 45 => sok("false"); "not nan")]
#[test_case(|| f64::NAN => sok("true"); "is actually not-a-number")]
fn is_nan<T>(make_val: impl FnOnce() -> T) -> Result<String, String>
where
    T: Into<ECMAScriptValue>,
{
    setup_test_agent();
    let val = make_val().into();
    super::is_nan(ECMAScriptValue::Undefined, None, &[val]).map_err(unwind_any_error).map(|v| v.test_result_string())
}

#[test_case(
    || wks(WksId::Unscopables)
    => serr("TypeError: Symbol values cannot be converted to Number values");
    "to_number fails"
)]
#[test_case(|| 45 => sok("true"); "ordinary number")]
#[test_case(|| f64::NAN => sok("false"); "nan")]
#[test_case(|| f64::INFINITY => sok("false"); "positive infinity")]
#[test_case(|| f64::NEG_INFINITY => sok("false"); "negative infinity")]
fn is_finite<T>(make_val: impl FnOnce() -> T) -> Result<String, String>
where
    T: Into<ECMAScriptValue>,
{
    setup_test_agent();
    let val = make_val().into();
    super::is_finite(ECMAScriptValue::Undefined, None, &[val]).map_err(unwind_any_error).map(|v| v.test_result_string())
}
