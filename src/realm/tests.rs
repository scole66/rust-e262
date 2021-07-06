use super::*;
use crate::tests::calculate_hash;
use ahash::RandomState;

const ALL_INTRINSIC_IDS: [IntrinsicId; 13] = [
    IntrinsicId::Boolean,
    IntrinsicId::BooleanPrototype,
    IntrinsicId::ErrorPrototype,
    IntrinsicId::FunctionPrototype,
    IntrinsicId::Object,
    IntrinsicId::ObjectPrototype,
    IntrinsicId::ReferenceError,
    IntrinsicId::ReferenceErrorPrototype,
    IntrinsicId::SyntaxError,
    IntrinsicId::SyntaxErrorPrototype,
    IntrinsicId::ThrowTypeError,
    IntrinsicId::TypeError,
    IntrinsicId::TypeErrorPrototype,
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
