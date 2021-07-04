use super::cr::AltCompletion;
use super::object::Object;

pub fn is_array(_obj: &Object) -> AltCompletion<bool> {
    Ok(false)
}
