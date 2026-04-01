// JSON global object

use super::*;

pub(crate) fn provision_json_intrinsic(realm: &Rc<RefCell<Realm>>) {
    // The JSON object:
    //
    //   * is %JSON%.
    //   * is the initial value of the "JSON" property of the global object.
    //   * is an ordinary object.
    //   * contains two functions, parse and stringify, that are used to parse and construct JSON texts.
    //   * has a [[Prototype]] internal slot whose value is %Object.prototype%.
    //   * does not have a [[Construct]] internal method; it cannot be used as a constructor with the new operator.
    //   * does not have a [[Call]] internal method; it cannot be invoked as a function.
    //
    // The JSON Data Interchange Format is defined in ECMA-404. The JSON interchange format used in this specification
    // is exactly that described by ECMA-404. Conforming implementations of JSON.parse and JSON.stringify must support
    // the exact interchange format described in the ECMA-404 specification without any deletions or extensions to the
    // format.
    let object_prototype = realm.borrow().intrinsics.object_prototype.clone();
    let json_object = ordinary_object_create(Some(object_prototype));
    realm.borrow_mut().intrinsics.json = json_object;
}
