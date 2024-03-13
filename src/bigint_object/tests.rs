use super::*;
use crate::tests::*;

mod bigint_object {
    use super::*;

    fn make() -> Object {
        BigIntObject::object(None, Rc::new(10.into()))
    }

    false_function!(is_proxy_object);
    none_function!(to_proxy_object);
}
