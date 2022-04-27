use super::*;
use crate::tests::{calculate_hash, test_agent, unwind_type_error};
use ahash::RandomState;

const ALL_INTRINSIC_IDS: &[IntrinsicId] = &[
    IntrinsicId::Array,
    IntrinsicId::ArrayPrototype,
    IntrinsicId::Boolean,
    IntrinsicId::BooleanPrototype,
    IntrinsicId::Error,
    IntrinsicId::ErrorPrototype,
    IntrinsicId::EvalError,
    IntrinsicId::EvalErrorPrototype,
    IntrinsicId::FunctionPrototype,
    IntrinsicId::Object,
    IntrinsicId::ObjectPrototype,
    IntrinsicId::Number,
    IntrinsicId::NumberPrototype,
    IntrinsicId::RangeError,
    IntrinsicId::RangeErrorPrototype,
    IntrinsicId::ReferenceError,
    IntrinsicId::ReferenceErrorPrototype,
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
        assert_ne!(format!("{:?}", id), "");
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
            assert_eq!(calculate_hash(&factory, &left_value) == calculate_hash(&factory, &right_value), left_idx == right_idx);
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

#[test]
fn intrinsics_debug() {
    let mut agent = test_agent();
    assert_ne!(format!("{:?}", Intrinsics::new(&mut agent)), "")
}

#[test]
fn intrinsics_get() {
    let agent = test_agent();
    let realm_ptr = agent.current_realm_record().unwrap();
    let realm = realm_ptr.borrow();
    let intrinsics = &realm.intrinsics;
    assert_eq!(intrinsics.get(IntrinsicId::Boolean), intrinsics.boolean);
    assert_eq!(intrinsics.get(IntrinsicId::BooleanPrototype), intrinsics.boolean_prototype);
    assert_eq!(intrinsics.get(IntrinsicId::Error), intrinsics.error);
    assert_eq!(intrinsics.get(IntrinsicId::ErrorPrototype), intrinsics.error_prototype);
    assert_eq!(intrinsics.get(IntrinsicId::EvalError), intrinsics.eval_error);
    assert_eq!(intrinsics.get(IntrinsicId::EvalErrorPrototype), intrinsics.eval_error_prototype);
    assert_eq!(intrinsics.get(IntrinsicId::FunctionPrototype), intrinsics.function_prototype);
    assert_eq!(intrinsics.get(IntrinsicId::Number), intrinsics.number);
    assert_eq!(intrinsics.get(IntrinsicId::NumberPrototype), intrinsics.number_prototype);
    assert_eq!(intrinsics.get(IntrinsicId::Object), intrinsics.object);
    assert_eq!(intrinsics.get(IntrinsicId::ObjectPrototype), intrinsics.object_prototype);
    assert_eq!(intrinsics.get(IntrinsicId::RangeError), intrinsics.range_error);
    assert_eq!(intrinsics.get(IntrinsicId::RangeErrorPrototype), intrinsics.range_error_prototype);
    assert_eq!(intrinsics.get(IntrinsicId::ReferenceError), intrinsics.reference_error);
    assert_eq!(intrinsics.get(IntrinsicId::ReferenceErrorPrototype), intrinsics.reference_error_prototype);
    assert_eq!(intrinsics.get(IntrinsicId::SyntaxError), intrinsics.syntax_error);
    assert_eq!(intrinsics.get(IntrinsicId::SyntaxErrorPrototype), intrinsics.syntax_error_prototype);
    assert_eq!(intrinsics.get(IntrinsicId::ThrowTypeError), intrinsics.throw_type_error);
    assert_eq!(intrinsics.get(IntrinsicId::TypeError), intrinsics.type_error);
    assert_eq!(intrinsics.get(IntrinsicId::TypeErrorPrototype), intrinsics.type_error_prototype);
    assert_eq!(intrinsics.get(IntrinsicId::URIError), intrinsics.uri_error);
    assert_eq!(intrinsics.get(IntrinsicId::URIErrorPrototype), intrinsics.uri_error_prototype);
}

#[test]
fn realm_debug() {
    let agent = test_agent();
    let realm_ptr = agent.current_realm_record().unwrap();
    let realm = realm_ptr.borrow();
    assert_ne!(format!("{:?}", realm), "");
}

#[test]
fn throw_type_error_test() {
    let mut agent = test_agent();
    let err = throw_type_error(&mut agent, ECMAScriptValue::Undefined, None, &[]).unwrap_err();
    let msg = unwind_type_error(&mut agent, err);
    assert_eq!(msg, "Generic TypeError");
}
