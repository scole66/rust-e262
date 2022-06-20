use super::agent::{Agent, WksId};
use super::comparison::is_extensible;
use super::cr::Completion;
use super::errors::{create_reference_error, create_type_error};
use super::function_object::ThisMode;
use super::object::{
    define_property_or_throw, get, has_own_property, has_property, set, DescriptorKind, Object,
    PotentialPropertyDescriptor,
};
use super::reference::{Base, Reference};
use super::strings::JSString;
use super::values::{to_boolean, ECMAScriptValue, PrivateName, PropertyKey};
use ahash::{AHashMap, AHashSet, RandomState};
use std::cell::RefCell;
use std::fmt::{self, Debug};
use std::rc::Rc;

// Environment Records
//
// Environment Record is a specification type used to define the association of Identifiers to specific variables and
// functions, based upon the lexical nesting structure of ECMAScript code. Usually an Environment Record is associated
// with some specific syntactic structure of ECMAScript code such as a FunctionDeclaration, a BlockStatement, or a Catch
// clause of a TryStatement. Each time such code is evaluated, a new Environment Record is created to record the
// identifier bindings that are created by that code.
//
// Every Environment Record has an [[OuterEnv]] field, which is either null or a reference to an outer Environment
// Record. This is used to model the logical nesting of Environment Record values. The outer reference of an (inner)
// Environment Record is a reference to the Environment Record that logically surrounds the inner Environment Record. An
// outer Environment Record may, of course, have its own outer Environment Record. An Environment Record may serve as
// the outer environment for multiple inner Environment Records. For example, if a FunctionDeclaration contains two
// nested FunctionDeclarations then the Environment Records of each of the nested functions will have as their outer
// Environment Record the Environment Record of the current evaluation of the surrounding function.
//
// Environment Records are purely specification mechanisms and need not correspond to any specific artefact of an
// ECMAScript implementation. It is impossible for an ECMAScript program to directly access or manipulate such values.
//
// The Environment Record Type Hierarchy
//
// Environment Records can be thought of as existing in a simple object-oriented hierarchy where Environment Record is
// an abstract class with three concrete subclasses: declarative Environment Record, object Environment Record, and
// global Environment Record. Function Environment Records and module Environment Records are subclasses of declarative
// Environment Record.
//
//  * Environment Record (abstract)
//
//      * A declarative Environment Record is used to define the effect of ECMAScript language syntactic elements such
//        as FunctionDeclarations, VariableDeclarations, and Catch clauses that directly associate identifier bindings
//        with ECMAScript language values.
//
//          * A function Environment Record corresponds to the invocation of an ECMAScript function object, and contains
//            bindings for the top-level declarations within that function. It may establish a new this binding. It also
//            captures the state necessary to support super method invocations.
//
//          * A module Environment Record contains the bindings for the top-level declarations of a Module. It also
//            contains the bindings that are explicitly imported by the Module. Its [[OuterEnv]] is a global Environment
//            Record.
//
//      * An object Environment Record is used to define the effect of ECMAScript elements such as WithStatement that
//        associate identifier bindings with the properties of some object.
//
//      * A global Environment Record is used for Script global declarations. It does not have an outer environment; its
//        [[OuterEnv]] is null. It may be prepopulated with identifier bindings and it includes an associated global
//        object whose properties provide some of the global environment's identifier bindings. As ECMAScript code is
//        executed, additional properties may be added to the global object and the initial properties may be modified.
//
//  The Environment Record abstract class includes the abstract specification methods defined in Table 17. These
//  abstract methods have distinct concrete algorithms for each of the concrete subclasses.
//

// Table 17: Abstract Methods of Environment Records
// +------------------------------+------------------------------------------------------------------------------------+
// | Method                       | Purpose                                                                            |
// +------------------------------+------------------------------------------------------------------------------------+
// | HasBinding(N)                | Determine if an Environment Record has a binding for the String value N. Return    |
// |                              | true if it does and false if it does not.                                          |
// +------------------------------+------------------------------------------------------------------------------------+
// | CreateMutableBinding(N, D)   | Create a new but uninitialized mutable binding in an Environment Record. The       |
// |                              | String value N is the text of the bound name. If the Boolean argument D is true    |
// |                              | the binding may be subsequently deleted.                                           |
// +------------------------------+------------------------------------------------------------------------------------+
// | CreateImmutableBinding(N, S) | Create a new but uninitialized immutable binding in an Environment Record. The     |
// |                              | String value N is the text of the bound name. If S is true then attempts to set it |
// |                              | after it has been initialized will always throw an exception, regardless of the    |
// |                              | strict mode setting of operations that reference that binding.                     |
// +------------------------------+------------------------------------------------------------------------------------+
// | InitializeBinding(N, V)      | Set the value of an already existing but uninitialized binding in an Environment   |
// |                              | Record. The String value N is the text of the bound name. V is the value for the   |
// |                              | binding and is a value of any ECMAScript language type.                            |
// +------------------------------+------------------------------------------------------------------------------------+
// | SetMutableBinding(N, V, S)   | Set the value of an already existing mutable binding in an Environment Record. The |
// |                              | String value N is the text of the bound name. V is the value for the binding and   |
// |                              | may be a value of any ECMAScript language type. S is a Boolean flag. If S is true  |
// |                              | and the binding cannot be set throw a TypeError exception.                         |
// +------------------------------+------------------------------------------------------------------------------------+
// | GetBindingValue(N, S)        | Returns the value of an already existing binding from an Environment Record. The   |
// |                              | String value N is the text of the bound name. S is used to identify references     |
// |                              | originating in strict mode code or that otherwise require strict mode reference    |
// |                              | semantics. If S is true and the binding does not exist throw a ReferenceError      |
// |                              | exception. If the binding exists but is uninitialized a ReferenceError is thrown,  |
// |                              | regardless of the value of S.                                                      |
// +------------------------------+------------------------------------------------------------------------------------+
// | DeleteBinding(N)             | Delete a binding from an Environment Record. The String value N is the text of the |
// |                              | bound name. If a binding for N exists, remove the binding and return true. If the  |
// |                              | binding exists but cannot be removed return false. If the binding does not exist   |
// |                              | return true.                                                                       |
// +------------------------------+------------------------------------------------------------------------------------+
// | HasThisBinding()             | Determine if an Environment Record establishes a this binding. Return true if it   |
// |                              | does and false if it does not.                                                     |
// +------------------------------+------------------------------------------------------------------------------------+
// | HasSuperBinding()            | Determine if an Environment Record establishes a super method binding. Return true |
// |                              | if it does and false if it does not.                                               |
// +------------------------------+------------------------------------------------------------------------------------+
// | WithBaseObject()             | If this Environment Record is associated with a with statement, return the with    |
// |                              | object. Otherwise, return undefined.                                               |
// +------------------------------+------------------------------------------------------------------------------------+

pub trait EnvironmentRecord: Debug {
    fn has_binding(&self, agent: &mut Agent, name: &JSString) -> Completion<bool>;
    fn create_mutable_binding(&self, agent: &mut Agent, name: JSString, deletable: bool) -> Completion<()>;
    fn create_immutable_binding(&self, agent: &mut Agent, name: JSString, strict: bool) -> Completion<()>;
    fn initialize_binding(&self, agent: &mut Agent, name: &JSString, value: ECMAScriptValue) -> Completion<()>;
    fn set_mutable_binding(
        &self,
        agent: &mut Agent,
        name: JSString,
        value: ECMAScriptValue,
        strict: bool,
    ) -> Completion<()>;
    fn get_binding_value(&self, agent: &mut Agent, name: &JSString, strict: bool) -> Completion<ECMAScriptValue>;
    fn delete_binding(&self, agent: &mut Agent, name: &JSString) -> Completion<bool>;
    fn has_this_binding(&self) -> bool;
    fn has_super_binding(&self) -> bool;
    fn with_base_object(&self) -> Option<Object>;
    fn get_outer_env(&self) -> Option<Rc<dyn EnvironmentRecord>>;
    fn get_this_binding(&self, _agent: &mut Agent) -> Completion<ECMAScriptValue> {
        unreachable!()
    }
}

// Declarative Environment Records
//
// Each declarative Environment Record is associated with an ECMAScript program scope containing variable, constant,
// let, class, module, import, and/or function declarations. A declarative Environment Record binds the set of
// identifiers defined by the declarations contained within its scope.
//
// The behaviour of the concrete specification methods for declarative Environment Records is defined by the following
// algorithms.

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
enum Removability {
    Deletable,
    Permanent,
}

impl From<bool> for Removability {
    fn from(source: bool) -> Self {
        match source {
            true => Removability::Deletable,
            false => Removability::Permanent,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
enum Strictness {
    Strict,
    Sloppy,
}

impl From<bool> for Strictness {
    fn from(source: bool) -> Self {
        match source {
            true => Strictness::Strict,
            false => Strictness::Sloppy,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
enum Mutability {
    Mutable(Removability),
    Immutable(Strictness),
}

#[derive(Debug)]
struct Binding {
    value: Option<ECMAScriptValue>,
    mutability: Mutability,
}

pub struct DeclarativeEnvironmentRecord {
    bindings: RefCell<AHashMap<JSString, Binding, RandomState>>,
    outer_env: Option<Rc<dyn EnvironmentRecord>>,
    name: String,
}

impl fmt::Debug for DeclarativeEnvironmentRecord {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if f.alternate() {
            f.debug_struct("DeclarativeEnvironmentRecord")
                .field("bindings", &self.bindings)
                .field("outer_env", &self.outer_env)
                .field("name", &self.name)
                .finish()
        } else {
            let name = &self.name;
            write!(f, "DeclarativeEnvironmentRecord({name})")
        }
    }
}

impl EnvironmentRecord for DeclarativeEnvironmentRecord {
    // HasBinding ( N )
    //
    // The HasBinding concrete method of a declarative Environment Record envRec takes argument N (a String). It
    // determines if the argument identifier is one of the identifiers bound by the record. It performs the following
    // steps when called:
    //
    //
    // 1. If envRec has a binding for the name that is the value of N, return true.
    // 2. Return false.
    fn has_binding(&self, _agent: &mut Agent, name: &JSString) -> Completion<bool> {
        Ok(self.bindings.borrow().contains_key(name))
    }

    // CreateMutableBinding ( N, D )
    //
    // The CreateMutableBinding concrete method of a declarative Environment Record envRec takes arguments N (a String)
    // and D (a Boolean). It creates a new mutable binding for the name N that is uninitialized. A binding must not
    // already exist in this Environment Record for N. If D has the value true, the new binding is marked as being
    // subject to deletion. It performs the following steps when called:
    //
    //  1. Assert: envRec does not already have a binding for N.
    //  2. Create a mutable binding in envRec for N and record that it is uninitialized. If D is true, record that the
    //     newly created binding may be deleted by a subsequent DeleteBinding call.
    //  3. Return NormalCompletion(empty).
    fn create_mutable_binding(&self, _agent: &mut Agent, name: JSString, deletable: bool) -> Completion<()> {
        let removable = Removability::from(deletable);
        self.bindings.borrow_mut().insert(name, Binding { value: None, mutability: Mutability::Mutable(removable) });
        Ok(())
    }

    // CreateImmutableBinding ( N, S )
    //
    // The CreateImmutableBinding concrete method of a declarative Environment Record envRec takes arguments N (a
    // String) and S (a Boolean). It creates a new immutable binding for the name N that is uninitialized. A binding
    // must not already exist in this Environment Record for N. If S has the value true, the new binding is marked as a
    // strict binding. It performs the following steps when called:
    //
    // 1. Assert: envRec does not already have a binding for N.
    // 2. Create an immutable binding in envRec for N and record that it is uninitialized. If S is true, record that the
    //    newly created binding is a strict binding.
    // 3. Return NormalCompletion(empty).
    fn create_immutable_binding(&self, _agent: &mut Agent, name: JSString, strict: bool) -> Completion<()> {
        let strictness = Strictness::from(strict);
        self.bindings.borrow_mut().insert(name, Binding { value: None, mutability: Mutability::Immutable(strictness) });
        Ok(())
    }

    // InitializeBinding ( N, V )
    //
    // The InitializeBinding concrete method of a declarative Environment Record envRec takes arguments N (a String) and
    // V (an ECMAScript language value). It is used to set the bound value of the current binding of the identifier
    // whose name is the value of the argument N to the value of argument V. An uninitialized binding for N must already
    // exist. It performs the following steps when called:
    //
    // 1. Assert: envRec must have an uninitialized binding for N.
    // 2. Set the bound value for N in envRec to V.
    // 3. Record that the binding for N in envRec has been initialized.
    // 4. Return NormalCompletion(empty).
    fn initialize_binding(&self, _agent: &mut Agent, name: &JSString, value: ECMAScriptValue) -> Completion<()> {
        self.bindings.borrow_mut().get_mut(name).unwrap().value = Some(value);
        Ok(())
    }

    // SetMutableBinding ( N, V, S )
    //
    // The SetMutableBinding concrete method of a declarative Environment Record envRec takes arguments N (a String), V
    // (an ECMAScript language value), and S (a Boolean). It attempts to change the bound value of the current binding
    // of the identifier whose name is the value of the argument N to the value of argument V. A binding for N normally
    // already exists, but in rare cases it may not. If the binding is an immutable binding, a TypeError is thrown if S
    // is true. It performs the following steps when called:
    //
    //  1. If envRec does not have a binding for N, then
    //      a. If S is true, throw a ReferenceError exception.
    //      b. Perform envRec.CreateMutableBinding(N, true).
    //      c. Perform envRec.InitializeBinding(N, V).
    //      d. Return NormalCompletion(empty).
    //  2. If the binding for N in envRec is a strict binding, set S to true.
    //  3. If the binding for N in envRec has not yet been initialized, throw a ReferenceError exception.
    //  4. Else if the binding for N in envRec is a mutable binding, change its bound value to V.
    //  5. Else,
    //      a. Assert: This is an attempt to change the value of an immutable binding.
    //      b. If S is true, throw a TypeError exception.
    //  6. Return NormalCompletion(empty).
    //
    // NOTE     An example of ECMAScript code that results in a missing binding at step 1 is:
    //              function f() { eval("var x; x = (delete x, 0);"); }
    fn set_mutable_binding(
        &self,
        agent: &mut Agent,
        name: JSString,
        value: ECMAScriptValue,
        strict: bool,
    ) -> Completion<()> {
        let mut bindings = self.bindings.borrow_mut();
        let maybe_item = bindings.get_mut(&name);
        match maybe_item {
            None => {
                if strict {
                    return Err(create_reference_error(agent, "Identifier not defined"));
                }
                bindings.insert(
                    name,
                    Binding { value: Some(value), mutability: Mutability::Mutable(Removability::Deletable) },
                );
                return Ok(());
            }
            Some(item) => match item.value {
                None => {
                    return Err(create_reference_error(agent, "Binding not initialized"));
                }
                Some(_) => match &item.mutability {
                    Mutability::Mutable(_) => {
                        item.value = Some(value);
                    }
                    Mutability::Immutable(s) => {
                        if *s == Strictness::Strict || strict {
                            return Err(create_type_error(agent, "Cannot change read-only value"));
                        }
                    }
                },
            },
        }
        Ok(())
    }

    // GetBindingValue ( N, S )
    //
    // The GetBindingValue concrete method of a declarative Environment Record envRec takes arguments N (a String) and S
    // (a Boolean). It returns the value of its bound identifier whose name is the value of the argument N. If the
    // binding exists but is uninitialized a ReferenceError is thrown, regardless of the value of S. It performs the
    // following steps when called:
    //
    //  1. Assert: envRec has a binding for N.
    //  2. If the binding for N in envRec is an uninitialized binding, throw a ReferenceError exception.
    //  3. Return the value currently bound to N in envRec.
    fn get_binding_value(&self, agent: &mut Agent, name: &JSString, _strict: bool) -> Completion<ECMAScriptValue> {
        let bindings = self.bindings.borrow();
        let maybe_value = &bindings.get(name).unwrap().value;
        match maybe_value {
            None => Err(create_reference_error(agent, "Binding not initialized")),
            Some(v) => Ok(v.clone()),
        }
    }

    // DeleteBinding ( N )
    //
    // The DeleteBinding concrete method of a declarative Environment Record envRec takes argument N (a String). It can
    // only delete bindings that have been explicitly designated as being subject to deletion. It performs the following
    // steps when called:
    //
    //  1. Assert: envRec has a binding for the name that is the value of N.
    //  2. If the binding for N in envRec cannot be deleted, return false.
    //  3. Remove the binding for N from envRec.
    //  4. Return true.
    fn delete_binding(&self, _agent: &mut Agent, name: &JSString) -> Completion<bool> {
        let mut bindings = self.bindings.borrow_mut();
        let item = bindings.get(name).unwrap();
        if item.mutability != Mutability::Mutable(Removability::Deletable) {
            Ok(false)
        } else {
            bindings.remove(name);
            Ok(true)
        }
    }

    // HasThisBinding ( )
    //
    // The HasThisBinding concrete method of a declarative Environment Record envRec takes no arguments. It performs the
    // following steps when called:
    //
    //  1. Return false.
    // NOTE     A regular declarative Environment Record (i.e., one that is neither a function Environment Record nor a
    //          module Environment Record) does not provide a this binding.
    fn has_this_binding(&self) -> bool {
        false
    }

    // HasSuperBinding ( )
    //
    // The HasSuperBinding concrete method of a declarative Environment Record envRec takes no arguments. It performs
    // the following steps when called:
    //
    //  1. Return false.
    // NOTE     A regular declarative Environment Record (i.e., one that is neither a function Environment Record nor a
    //          module Environment Record) does not provide a super binding.
    fn has_super_binding(&self) -> bool {
        false
    }

    // WithBaseObject ( )
    //
    // The WithBaseObject concrete method of a declarative Environment Record envRec takes no arguments. It performs the
    // following steps when called:
    //
    //  1. Return undefined.
    fn with_base_object(&self) -> Option<Object> {
        None
    }

    fn get_outer_env(&self) -> Option<Rc<dyn EnvironmentRecord>> {
        self.outer_env.as_ref().cloned()
    }
}

impl DeclarativeEnvironmentRecord {
    // NewDeclarativeEnvironment ( E )
    //
    // The abstract operation NewDeclarativeEnvironment takes argument E (an Environment Record). It performs the
    // following steps when called:
    //
    //  1. Let env be a new declarative Environment Record containing no bindings.
    //  2. Set env.[[OuterEnv]] to E.
    //  3. Return env.
    pub fn new(env: Option<Rc<dyn EnvironmentRecord>>, name: impl Into<String>) -> Self {
        DeclarativeEnvironmentRecord { bindings: Default::default(), outer_env: env, name: name.into() }
    }
}

// Object Environment Records
//
// Each object Environment Record is associated with an object called its binding object. An object Environment Record
// binds the set of string identifier names that directly correspond to the property names of its binding object.
// Property keys that are not strings in the form of an IdentifierName are not included in the set of bound identifiers.
// Both own and inherited properties are included in the set regardless of the setting of their [[Enumerable]]
// attribute. Because properties can be dynamically added and deleted from objects, the set of identifiers bound by an
// object Environment Record may potentially change as a side-effect of any operation that adds or deletes properties.
// Any bindings that are created as a result of such a side-effect are considered to be a mutable binding even if the
// Writable attribute of the corresponding property has the value false. Immutable bindings do not exist for object
// Environment Records.
//
// Object Environment Records created for with statements (14.11) can provide their binding object as an implicit this
// value for use in function calls. The capability is controlled by a Boolean [[IsWithEnvironment]] field.
//
// Object Environment Records have the additional state fields listed in Table 18.
//
// Table 18: Additional Fields of Object Environment Records
// +-----------------------+---------+----------------------------------------------------------------------------+
// | Field Name            | Value   | Meaning                                                                    |
// +-----------------------+---------+----------------------------------------------------------------------------+
// | [[BindingObject]]     | Object  | The binding object of this Environment Record.                             |
// +-----------------------+---------+----------------------------------------------------------------------------+
// | [[IsWithEnvironment]] | Boolean | Indicates whether this Environment Record is created for a with statement. |
// +-----------------------+---------+----------------------------------------------------------------------------+
//
// The behaviour of the concrete specification methods for object Environment Records is defined by the following
// algorithms.
pub struct ObjectEnvironmentRecord {
    binding_object: Object,
    is_with_environment: bool,
    outer_env: Option<Rc<dyn EnvironmentRecord>>,
    name: String,
}

impl fmt::Debug for ObjectEnvironmentRecord {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if f.alternate() {
            f.debug_struct("ObjectEnvironmentRecord")
                .field("binding_object", &self.binding_object)
                .field("is_with_environment", &self.is_with_environment)
                .field("outer_env", &self.outer_env)
                .field("name", &self.name)
                .finish()
        } else {
            let name = &self.name;
            write!(f, "ObjectEnvironmentRecord({name})")
        }
    }
}

impl EnvironmentRecord for ObjectEnvironmentRecord {
    // HasBinding ( N )
    //
    // The HasBinding concrete method of an object Environment Record envRec takes argument N (a String). It determines
    // if its associated binding object has a property whose name is the value of the argument N. It performs the
    // following steps when called:
    //
    //  1. Let bindingObject be envRec.[[BindingObject]].
    //  2. Let foundBinding be ? HasProperty(bindingObject, N).
    //  3. If foundBinding is false, return false.
    //  4. If envRec.[[IsWithEnvironment]] is false, return true.
    //  5. Let unscopables be ? Get(bindingObject, @@unscopables).
    //  6. If Type(unscopables) is Object, then
    //      a. Let blocked be ! ToBoolean(? Get(unscopables, N)).
    //      b. If blocked is true, return false.
    //  7. Return true.
    fn has_binding(&self, agent: &mut Agent, name: &JSString) -> Completion<bool> {
        let name_key = PropertyKey::from(name);
        let binding_object = &self.binding_object;
        let found_binding = has_property(agent, binding_object, &name_key)?;
        if !found_binding {
            Ok(false)
        } else if !self.is_with_environment {
            Ok(true)
        } else {
            let unscopables = get(agent, binding_object, &PropertyKey::from(agent.wks(WksId::Unscopables)))?;
            match &unscopables {
                ECMAScriptValue::Object(unscopables_obj) => {
                    let blocked = to_boolean(get(agent, unscopables_obj, &name_key)?);
                    Ok(!blocked)
                }
                _ => Ok(true),
            }
        }
    }

    // CreateMutableBinding ( N, D )
    //
    // The CreateMutableBinding concrete method of an object Environment Record envRec takes arguments N (a String) and
    // D (a Boolean). It creates in an Environment Record's associated binding object a property whose name is the
    // String value and initializes it to the value undefined. If D has the value true, the new property's
    // [[Configurable]] attribute is set to true; otherwise it is set to false. It performs the following steps when
    // called:
    //
    //  1. Let bindingObject be envRec.[[BindingObject]].
    //  2. Return ? DefinePropertyOrThrow(bindingObject, N, PropertyDescriptor { [[Value]]: undefined, [[Writable]]:
    //     true, [[Enumerable]]: true, [[Configurable]]: D }).
    //
    // NOTE     Normally envRec will not have a binding for N but if it does, the semantics of DefinePropertyOrThrow may
    //          result in an existing binding being replaced or shadowed or cause an abrupt completion to be returned.
    fn create_mutable_binding(&self, agent: &mut Agent, name: JSString, deletable: bool) -> Completion<()> {
        let binding_object = &self.binding_object;
        let desc = PotentialPropertyDescriptor::new()
            .value(ECMAScriptValue::Undefined)
            .writable(true)
            .enumerable(true)
            .configurable(deletable);
        define_property_or_throw(agent, binding_object, name, desc)
    }

    // CreateImmutableBinding ( N, S )
    //
    // The CreateImmutableBinding concrete method of an object Environment Record is never used within this
    // specification.
    fn create_immutable_binding(&self, _agent: &mut Agent, _name: JSString, _strict: bool) -> Completion<()> {
        unreachable!()
    }

    // InitializeBinding ( N, V )
    //
    // The InitializeBinding concrete method of an object Environment Record envRec takes arguments N (a String) and V
    // (an ECMAScript language value). It is used to set the bound value of the current binding of the identifier whose
    // name is the value of the argument N to the value of argument V. It performs the following steps when called:
    //
    //  1. Return ? envRec.SetMutableBinding(N, V, false).
    //
    // NOTE     In this specification, all uses of CreateMutableBinding for object Environment Records are immediately
    //          followed by a call to InitializeBinding for the same name. Hence, this specification does not explicitly
    //          track the initialization state of bindings in object Environment Records.
    fn initialize_binding(&self, agent: &mut Agent, name: &JSString, value: ECMAScriptValue) -> Completion<()> {
        self.set_mutable_binding(agent, name.clone(), value, false)
    }

    // SetMutableBinding ( N, V, S )
    //
    // The SetMutableBinding concrete method of an object Environment Record envRec takes arguments N (a String), V (an
    // ECMAScript language value), and S (a Boolean). It attempts to set the value of the Environment Record's
    // associated binding object's property whose name is the value of the argument N to the value of argument V. A
    // property named N normally already exists but if it does not or is not currently writable, error handling is
    // determined by S. It performs the following steps when called:
    //
    //  1. Let bindingObject be envRec.[[BindingObject]].
    //  2. Let stillExists be ? HasProperty(bindingObject, N).
    //  3. If stillExists is false and S is true, throw a ReferenceError exception.
    //  4. Return ? Set(bindingObject, N, V, S).
    fn set_mutable_binding(
        &self,
        agent: &mut Agent,
        name: JSString,
        value: ECMAScriptValue,
        strict: bool,
    ) -> Completion<()> {
        let name_key = PropertyKey::from(name);
        let binding_object = &self.binding_object;
        let still_exists = has_property(agent, binding_object, &name_key)?;
        if !still_exists && strict {
            Err(create_reference_error(agent, "Reference no longer exists"))
        } else {
            set(agent, binding_object, name_key, value, strict)?;
            Ok(())
        }
    }

    // GetBindingValue ( N, S )
    //
    // The GetBindingValue concrete method of an object Environment Record envRec takes arguments N (a String) and S (a
    // Boolean). It returns the value of its associated binding object's property whose name is the String value of the
    // argument identifier N. The property should already exist but if it does not the result depends upon S. It
    // performs the following steps when called:
    //
    //  1. Let bindingObject be envRec.[[BindingObject]].
    //  2. Let value be ? HasProperty(bindingObject, N).
    //  3. If value is false, then
    //      a. If S is false, return the value undefined; otherwise throw a ReferenceError exception.
    //  4. Return ? Get(bindingObject, N).
    fn get_binding_value(&self, agent: &mut Agent, name: &JSString, strict: bool) -> Completion<ECMAScriptValue> {
        let name_key = PropertyKey::from(name);
        let binding_object = &self.binding_object;
        let has_prop = has_property(agent, binding_object, &name_key)?;
        if !has_prop {
            if !strict {
                Ok(ECMAScriptValue::Undefined)
            } else {
                Err(create_reference_error(agent, "Unresolvable reference"))
            }
        } else {
            get(agent, binding_object, &name_key)
        }
    }

    // DeleteBinding ( N )
    //
    // The DeleteBinding concrete method of an object Environment Record envRec takes argument N (a String). It can only
    // delete bindings that correspond to properties of the environment object whose [[Configurable]] attribute have the
    // value true. It performs the following steps when called:
    //
    //  1. Let bindingObject be envRec.[[BindingObject]].
    //  2. Return ? bindingObject.[[Delete]](N).
    fn delete_binding(&self, agent: &mut Agent, name: &JSString) -> Completion<bool> {
        let name_key = PropertyKey::from(name);
        let binding_object = &self.binding_object;
        binding_object.o.delete(agent, &name_key)
    }

    // HasThisBinding ( )
    //
    // The HasThisBinding concrete method of an object Environment Record envRec takes no arguments. It performs the
    // following steps when called:
    //
    //  1. Return false.
    //
    // NOTE     Object Environment Records do not provide a this binding.
    fn has_this_binding(&self) -> bool {
        false
    }
    // HasSuperBinding ( )
    //
    // The HasSuperBinding concrete method of an object Environment Record envRec takes no arguments. It performs the
    // following steps when called:
    //
    //  1. Return false.
    //
    // NOTE     Object Environment Records do not provide a super binding.
    fn has_super_binding(&self) -> bool {
        false
    }

    // WithBaseObject ( )
    //
    // The WithBaseObject concrete method of an object Environment Record envRec takes no arguments. It performs the
    // following steps when called:
    //
    //  1. If envRec.[[IsWithEnvironment]] is true, return envRec.[[BindingObject]].
    //  2. Otherwise, return undefined.
    fn with_base_object(&self) -> Option<Object> {
        match self.is_with_environment {
            true => Some(self.binding_object.clone()),
            false => None,
        }
    }

    fn get_outer_env(&self) -> Option<Rc<dyn EnvironmentRecord>> {
        self.outer_env.as_ref().cloned()
    }
}

impl ObjectEnvironmentRecord {
    // NewObjectEnvironment ( O, W, E )
    //
    // The abstract operation NewObjectEnvironment takes arguments O (an Object), W (a Boolean), and E (an Environment
    // Record or null). It performs the following steps when called:
    //
    //  1. Let env be a new object Environment Record.
    //  2. Set env.[[BindingObject]] to O.
    //  3. Set env.[[IsWithEnvironment]] to W.
    //  4. Set env.[[OuterEnv]] to E.
    //  5. Return env.
    pub fn new(
        binding_object: Object,
        is_with_environment: bool,
        outer_env: Option<Rc<dyn EnvironmentRecord>>,
        name: impl Into<String>,
    ) -> Self {
        ObjectEnvironmentRecord { binding_object, is_with_environment, outer_env, name: name.into() }
    }
}

// Function Environment Records
//
// A function Environment Record is a declarative Environment Record that is used to represent the top-level scope of a
// function and, if the function is not an ArrowFunction, provides a this binding. If a function is not an ArrowFunction
// function and references super, its function Environment Record also contains the state that is used to perform super
// method invocations from within the function.
//
// Function Environment Records have the additional state fields listed in Table 19.
//
// Table 19: Additional Fields of Function Environment Records
// +-----------------------+-----------------------+-------------------------------------------------------------------+
// | Field Name            | Value                 | Meaning                                                           |
// +-----------------------+-----------------------+-------------------------------------------------------------------+
// | [[ThisValue]]         | Any                   | This is the this value used for this invocation of the function.  |
// +-----------------------+-----------------------+-------------------------------------------------------------------+
// | [[ThisBindingStatus]] | lexical | initialized | If the value is lexical, this is an ArrowFunction and does not    |
// |                       | | uninitialized       | have a local this value.                                          |
// +-----------------------+-----------------------+-------------------------------------------------------------------+
// | [[FunctionObject]]    | Object                | The function object whose invocation caused this Environment      |
// |                       |                       | Record to be created.                                             |
// +-----------------------+-----------------------+-------------------------------------------------------------------+
// | [[NewTarget]]         | Object | undefined    | If this Environment Record was created by the [[Construct]]       |
// |                       |                       | internal method, [[NewTarget]] is the value of the [[Construct]]  |
// |                       |                       | newTarget parameter. Otherwise, its value is undefined.           |
// +-----------------------+-----------------------+-------------------------------------------------------------------+

#[derive(PartialEq, Eq, Debug)]
pub enum BindingStatus {
    Lexical,
    Initialized,
    Uninitialized,
}

pub struct FunctionEnvironmentRecord {
    base: DeclarativeEnvironmentRecord,
    this_value: ECMAScriptValue,
    this_binding_status: BindingStatus,
    function_object: Object,
    new_target: Option<Object>,
    name: String,
}

impl fmt::Debug for FunctionEnvironmentRecord {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if f.alternate() {
            f.debug_struct("FunctionEnvironmentRecord")
                .field("base", &self.base)
                .field("this_value", &self.this_value)
                .field("this_binding_status", &self.this_binding_status)
                .field("function_object", &self.function_object)
                .field("new_target", &self.new_target)
                .field("name", &self.name)
                .finish()
        } else {
            let name = &self.name;
            write!(f, "FunctionEnvironmentRecord({name})")
        }
    }
}

impl EnvironmentRecord for FunctionEnvironmentRecord {
    // Function Environment Records support all of the declarative Environment Record methods listed in Table 17 and share
    // the same specifications for all of those methods except for HasThisBinding and HasSuperBinding.
    fn has_binding(&self, agent: &mut Agent, name: &JSString) -> Completion<bool> {
        self.base.has_binding(agent, name)
    }
    fn create_mutable_binding(&self, agent: &mut Agent, name: JSString, deletable: bool) -> Completion<()> {
        self.base.create_mutable_binding(agent, name, deletable)
    }
    fn create_immutable_binding(&self, agent: &mut Agent, name: JSString, strict: bool) -> Completion<()> {
        self.base.create_immutable_binding(agent, name, strict)
    }
    fn initialize_binding(&self, agent: &mut Agent, name: &JSString, value: ECMAScriptValue) -> Completion<()> {
        self.base.initialize_binding(agent, name, value)
    }
    fn set_mutable_binding(
        &self,
        agent: &mut Agent,
        name: JSString,
        value: ECMAScriptValue,
        strict: bool,
    ) -> Completion<()> {
        self.base.set_mutable_binding(agent, name, value, strict)
    }
    fn get_binding_value(&self, agent: &mut Agent, name: &JSString, strict: bool) -> Completion<ECMAScriptValue> {
        self.base.get_binding_value(agent, name, strict)
    }
    fn delete_binding(&self, agent: &mut Agent, name: &JSString) -> Completion<bool> {
        self.base.delete_binding(agent, name)
    }
    fn with_base_object(&self) -> Option<Object> {
        self.base.with_base_object()
    }

    // HasThisBinding ( )
    //
    // The HasThisBinding concrete method of a function Environment Record envRec takes no arguments. It performs the
    // following steps when called:
    //
    //  1. If envRec.[[ThisBindingStatus]] is lexical, return false; otherwise, return true.
    fn has_this_binding(&self) -> bool {
        self.this_binding_status != BindingStatus::Lexical
    }

    // HasSuperBinding ( )
    //
    // The HasSuperBinding concrete method of a function Environment Record envRec takes no arguments. It performs the
    // following steps when called:
    //
    //  1. If envRec.[[ThisBindingStatus]] is lexical, return false.
    //  2. If envRec.[[FunctionObject]].[[HomeObject]] has the value undefined, return false; otherwise, return true.
    fn has_super_binding(&self) -> bool {
        if self.this_binding_status == BindingStatus::Lexical {
            false
        } else {
            let fo = self.function_object.o.to_function_obj().unwrap();
            fo.function_data().borrow().home_object.is_some()
        }
    }

    fn get_outer_env(&self) -> Option<Rc<dyn EnvironmentRecord>> {
        self.base.outer_env.as_ref().cloned()
    }

    // GetThisBinding ( )
    //
    // The GetThisBinding concrete method of a function Environment Record envRec takes no arguments. It performs the
    // following steps when called:
    //
    //  1. Assert: envRec.[[ThisBindingStatus]] is not lexical.
    //  2. If envRec.[[ThisBindingStatus]] is uninitialized, throw a ReferenceError exception.
    //  3. Return envRec.[[ThisValue]].
    fn get_this_binding(&self, agent: &mut Agent) -> Completion<ECMAScriptValue> {
        if self.this_binding_status == BindingStatus::Uninitialized {
            Err(create_reference_error(agent, "This binding uninitialized"))
        } else {
            Ok(self.this_value.clone())
        }
    }
}

// In addition, function Environment Records support the methods listed in Table 20:
//
// Table 20: Additional Methods of Function Environment Records
//
// +------------------+------------------------------------------------------------------------------------------------+
// | Method           | Purpose                                                                                        |
// +------------------+------------------------------------------------------------------------------------------------+
// | BindThisValue(V) | Set the [[ThisValue]] and record that it has been initialized.                                 |
// +------------------+------------------------------------------------------------------------------------------------+
// | GetThisBinding() | Return the value of this Environment Record's this binding. Throws a ReferenceError if the     |
// |                  | this binding has not been initialized.                                                         |
// +------------------+------------------------------------------------------------------------------------------------+
// | GetSuperBase()   | Return the object that is the base for super property accesses bound in this Environment       |
// |                  | Record. The value undefined indicates that super property accesses will produce runtime        |
// |                  | errors.                                                                                        |
// +------------------+------------------------------------------------------------------------------------------------+
//
// The behaviour of the additional concrete specification methods for function Environment Records is defined by the
// following algorithms:

impl FunctionEnvironmentRecord {
    // BindThisValue ( V )
    //
    // The BindThisValue concrete method of a function Environment Record envRec takes argument V (an ECMAScript
    // language value). It performs the following steps when called:
    //
    //  1. Assert: envRec.[[ThisBindingStatus]] is not lexical.
    //  2. If envRec.[[ThisBindingStatus]] is initialized, throw a ReferenceError exception.
    //  3. Set envRec.[[ThisValue]] to V.
    //  4. Set envRec.[[ThisBindingStatus]] to initialized.
    //  5. Return V.
    pub fn bind_this_value(&mut self, agent: &mut Agent, val: ECMAScriptValue) -> Completion<ECMAScriptValue> {
        assert_ne!(self.this_binding_status, BindingStatus::Lexical);
        if self.this_binding_status == BindingStatus::Initialized {
            Err(create_reference_error(agent, "This value already bound"))
        } else {
            self.this_value = val.clone();
            self.this_binding_status = BindingStatus::Initialized;
            Ok(val)
        }
    }

    // GetSuperBase ( )
    //
    // The GetSuperBase concrete method of a function Environment Record envRec takes no arguments. It performs the
    // following steps when called:
    //
    //  1. Let home be envRec.[[FunctionObject]].[[HomeObject]].
    //  2. If home has the value undefined, return undefined.
    //  3. Assert: Type(home) is Object.
    //  4. Return ? home.[[GetPrototypeOf]]().
    pub fn get_super_base(&self, agent: &mut Agent) -> Completion<Option<Object>> {
        let fo = self.function_object.o.to_function_obj().unwrap();
        let home = &fo.function_data().borrow().home_object;
        match home {
            Some(obj) => obj.o.get_prototype_of(agent),
            None => Ok(None),
        }
    }

    // NewFunctionEnvironment ( F, newTarget )
    //
    // The abstract operation NewFunctionEnvironment takes arguments F and newTarget. It performs the following steps
    // when called:
    //
    //  1. Assert: F is an ECMAScript function.
    //  2. Assert: Type(newTarget) is Undefined or Object.
    //  3. Let env be a new function Environment Record containing no bindings.
    //  4. Set env.[[FunctionObject]] to F.
    //  5. If F.[[ThisMode]] is lexical, set env.[[ThisBindingStatus]] to lexical.
    //  6. Else, set env.[[ThisBindingStatus]] to uninitialized.
    //  7. Set env.[[NewTarget]] to newTarget.
    //  8. Set env.[[OuterEnv]] to F.[[Environment]].
    //  9. Return env.
    pub fn new(f: Object, new_target: Option<Object>, name: impl Into<String>) -> Self {
        let fo = f.o.to_function_obj().unwrap();
        let tbs = if fo.function_data().borrow().this_mode == ThisMode::Lexical {
            BindingStatus::Lexical
        } else {
            BindingStatus::Uninitialized
        };
        let outer = Some(fo.function_data().borrow().environment.clone());
        let name = name.into();

        FunctionEnvironmentRecord {
            base: DeclarativeEnvironmentRecord {
                bindings: Default::default(),
                outer_env: outer,
                name: format!("{name}-inner"),
            },
            this_value: ECMAScriptValue::Undefined,
            this_binding_status: tbs,
            function_object: f,
            new_target,
            name,
        }
    }
}

// Global Environment Records
//
// A global Environment Record is used to represent the outer most scope that is shared by all of the ECMAScript Script
// elements that are processed in a common realm. A global Environment Record provides the bindings for built-in globals
// (clause 19), properties of the global object, and for all top-level declarations (8.1.9, 8.1.11) that occur within a
// Script.
//
// A global Environment Record is logically a single record but it is specified as a composite encapsulating an object
// Environment Record and a declarative Environment Record. The object Environment Record has as its base object the
// global object of the associated Realm Record. This global object is the value returned by the global Environment
// Record's GetThisBinding concrete method. The object Environment Record component of a global Environment Record
// contains the bindings for all built-in globals (clause 19) and all bindings introduced by a FunctionDeclaration,
// GeneratorDeclaration, AsyncFunctionDeclaration, AsyncGeneratorDeclaration, or VariableStatement contained in global
// code. The bindings for all other ECMAScript declarations in global code are contained in the declarative Environment
// Record component of the global Environment Record.
//
// Properties may be created directly on a global object. Hence, the object Environment Record component of a global
// Environment Record may contain both bindings created explicitly by FunctionDeclaration, GeneratorDeclaration,
// AsyncFunctionDeclaration, AsyncGeneratorDeclaration, or VariableDeclaration declarations and bindings created
// implicitly as properties of the global object. In order to identify which bindings were explicitly created using
// declarations, a global Environment Record maintains a list of the names bound using its CreateGlobalVarBinding and
// CreateGlobalFunctionBinding concrete methods.
//
// Global Environment Records have the additional fields listed in Table 21 and the additional methods listed in Table
// 22.

// Table 21: Additional Fields of Global Environment Records
// +-----------------------+-------------+-----------------------------------------------------------------------------+
// | Field Name            | Value       | Meaning                                                                     |
// +-----------------------+-------------+-----------------------------------------------------------------------------+
// | [[ObjectRecord]]      | Object      | Binding object is the global object. It contains global built-in bindings   |
// |                       | Environment | as well as FunctionDeclaration, GeneratorDeclaration,                       |
// |                       | Record      | AsyncFunctionDeclaration, AsyncGeneratorDeclaration, and                    |
// |                       |             | VariableDeclaration bindings in global code for the associated realm.       |
// +-----------------------+-------------+-----------------------------------------------------------------------------+
// | [[GlobalThisValue]]   | Object      | The value returned by this in global scope. Hosts may provide any           |
// |                       |             | ECMAScript Object value.                                                    |
// +-----------------------+-------------+-----------------------------------------------------------------------------+
// | [[DeclarativeRecord]] | Declarative | Contains bindings for all declarations in global code for the associated    |
// |                       | Environment | realm code except for FunctionDeclaration, GeneratorDeclaration,            |
// |                       | Record      | AsyncFunctionDeclaration, AsyncGeneratorDeclaration, and                    |
// |                       |             | VariableDeclaration bindings.                                               |
// +-----------------------+-------------+-----------------------------------------------------------------------------+
// | [[VarNames]]          | List of     | The string names bound by FunctionDeclaration, GeneratorDeclaration,        |
// |                       | String      | AsyncFunctionDeclaration, AsyncGeneratorDeclaration, and                    |
// |                       |             | VariableDeclaration declarations in global code for the associated realm.   |
// +-----------------------+-------------+-----------------------------------------------------------------------------+
pub struct GlobalEnvironmentRecord {
    object_record: ObjectEnvironmentRecord,
    global_this_value: Object,
    declarative_record: DeclarativeEnvironmentRecord,
    var_names: RefCell<AHashSet<JSString>>,
    name: String,
}

impl fmt::Debug for GlobalEnvironmentRecord {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if f.alternate() {
            f.debug_struct("GlobalEnvironmentRecord")
                .field("object_record", &self.object_record)
                .field("global_this_value", &self.global_this_value)
                .field("declarative_record", &self.declarative_record)
                .field("var_names", &self.var_names)
                .field("name", &self.name)
                .finish()
        } else {
            let name = &self.name;
            write!(f, "GlobalEnvironmentRecord({name})")
        }
    }
}

// Table 22: Additional Methods of Global Environment Records
// +--------------------------------------+----------------------------------------------------------------------------+
// | Method                               | Purpose                                                                    |
// +--------------------------------------+----------------------------------------------------------------------------+
// | GetThisBinding()                     | Return the value of this Environment Record's this binding.                |
// +--------------------------------------+----------------------------------------------------------------------------+
// | HasVarDeclaration (N)                | Determines if the argument identifier has a binding in this Environment    |
// |                                      | Record that was created using a VariableDeclaration, FunctionDeclaration,  |
// |                                      | GeneratorDeclaration, AsyncFunctionDeclaration, or                         |
// |                                      | AsyncGeneratorDeclaration.                                                 |
// +--------------------------------------+----------------------------------------------------------------------------+
// | HasLexicalDeclaration (N)            | Determines if the argument identifier has a binding in this Environment    |
// |                                      | Record that was created using a lexical declaration such as a              |
// |                                      | LexicalDeclaration or a ClassDeclaration.                                  |
// +--------------------------------------+----------------------------------------------------------------------------+
// | HasRestrictedGlobalProperty (N)      | Determines if the argument is the name of a global object property that    |
// |                                      | may not be shadowed by a global lexical binding.                           |
// +--------------------------------------+----------------------------------------------------------------------------+
// | CanDeclareGlobalVar (N)              | Determines if a corresponding CreateGlobalVarBinding call would succeed if |
// |                                      | called for the same argument N.                                            |
// +--------------------------------------+----------------------------------------------------------------------------+
// | CanDeclareGlobalFunction (N)         | Determines if a corresponding CreateGlobalFunctionBinding call would       |
// |                                      | succeed if called for the same argument N.                                 |
// +--------------------------------------+----------------------------------------------------------------------------+
// | CreateGlobalVarBinding(N, D)         | Used to create and initialize to undefined a global var binding in the     |
// |                                      | [[ObjectRecord]] component of a global Environment Record. The binding     |
// |                                      | will be a mutable binding. The corresponding global object property will   |
// |                                      | have attribute values appropriate for a var. The String value N is the     |
// |                                      | bound name. If D is true the binding may be deleted. Logically equivalent  |
// |                                      | to CreateMutableBinding followed by a SetMutableBinding but it allows var  |
// |                                      | declarations to receive special treatment.                                 |
// +--------------------------------------+----------------------------------------------------------------------------+
// | CreateGlobalFunctionBinding(N, V, D) | Create and initialize a global function binding in the [[ObjectRecord]]    |
// |                                      | component of a global Environment Record. The binding will be a mutable    |
// |                                      | binding. The corresponding global object property will have attribute      |
// |                                      | values appropriate for a function. The String value N is the bound name. V |
// |                                      | is the initialization value. If the Boolean argument D is true the binding |
// |                                      | may be deleted. Logically equivalent to CreateMutableBinding followed by a |
// |                                      | SetMutableBinding but it allows function declarations to receive special   |
// |                                      | treatment.                                                                 |
// +--------------------------------------+----------------------------------------------------------------------------+

impl EnvironmentRecord for GlobalEnvironmentRecord {
    // HasBinding ( N )
    //
    // The HasBinding concrete method of a global Environment Record envRec takes argument N (a String). It determines
    // if the argument identifier is one of the identifiers bound by the record. It performs the following steps when
    // called:
    //
    //  1. Let DclRec be envRec.[[DeclarativeRecord]].
    //  2. If DclRec.HasBinding(N) is true, return true.
    //  3. Let ObjRec be envRec.[[ObjectRecord]].
    //  4. Return ? ObjRec.HasBinding(N).
    fn has_binding(&self, agent: &mut Agent, name: &JSString) -> Completion<bool> {
        Ok(self.declarative_record.has_binding(agent, name).unwrap() || self.object_record.has_binding(agent, name)?)
    }

    // CreateMutableBinding ( N, D )
    //
    // The CreateMutableBinding concrete method of a global Environment Record envRec takes arguments N (a String) and D
    // (a Boolean). It creates a new mutable binding for the name N that is uninitialized. The binding is created in the
    // associated DeclarativeRecord. A binding for N must not already exist in the DeclarativeRecord. If D has the value
    // true, the new binding is marked as being subject to deletion. It performs the following steps when called:
    //
    //  1. Let DclRec be envRec.[[DeclarativeRecord]].
    //  2. If DclRec.HasBinding(N) is true, throw a TypeError exception.
    //  3. Return DclRec.CreateMutableBinding(N, D).
    fn create_mutable_binding(&self, agent: &mut Agent, name: JSString, deletable: bool) -> Completion<()> {
        if self.declarative_record.has_binding(agent, &name).unwrap() {
            Err(create_type_error(agent, "Binding already exists"))
        } else {
            self.declarative_record.create_mutable_binding(agent, name, deletable)
        }
    }

    // CreateImmutableBinding ( N, S )
    //
    // The CreateImmutableBinding concrete method of a global Environment Record envRec takes arguments N (a String) and
    // S (a Boolean). It creates a new immutable binding for the name N that is uninitialized. A binding must not
    // already exist in this Environment Record for N. If S has the value true, the new binding is marked as a strict
    // binding. It performs the following steps when called:
    //
    //  1. Let DclRec be envRec.[[DeclarativeRecord]].
    //  2. If DclRec.HasBinding(N) is true, throw a TypeError exception.
    //  3. Return DclRec.CreateImmutableBinding(N, S).
    fn create_immutable_binding(&self, agent: &mut Agent, name: JSString, strict: bool) -> Completion<()> {
        if self.declarative_record.has_binding(agent, &name).unwrap() {
            Err(create_type_error(agent, "Binding already exists"))
        } else {
            self.declarative_record.create_immutable_binding(agent, name, strict)
        }
    }

    // InitializeBinding ( N, V )
    //
    // The InitializeBinding concrete method of a global Environment Record envRec takes arguments N (a String) and V
    // (an ECMAScript language value). It is used to set the bound value of the current binding of the identifier whose
    // name is the value of the argument N to the value of argument V. An uninitialized binding for N must already
    // exist. It performs the following steps when called:
    //
    //  1. Let DclRec be envRec.[[DeclarativeRecord]].
    //  2. If DclRec.HasBinding(N) is true, then
    //      a. Return DclRec.InitializeBinding(N, V).
    //  3. Assert: If the binding exists, it must be in the object Environment Record.
    //  4. Let ObjRec be envRec.[[ObjectRecord]].
    //  5. Return ? ObjRec.InitializeBinding(N, V).
    fn initialize_binding(&self, agent: &mut Agent, name: &JSString, value: ECMAScriptValue) -> Completion<()> {
        if self.declarative_record.has_binding(agent, name).unwrap() {
            self.declarative_record.initialize_binding(agent, name, value)
        } else {
            self.object_record.initialize_binding(agent, name, value)
        }
    }

    // SetMutableBinding ( N, V, S )
    //
    // The SetMutableBinding concrete method of a global Environment Record envRec takes arguments N (a String), V (an
    // ECMAScript language value), and S (a Boolean). It attempts to change the bound value of the current binding of
    // the identifier whose name is the value of the argument N to the value of argument V. If the binding is an
    // immutable binding, a TypeError is thrown if S is true. A property named N normally already exists but if it does
    // not or is not currently writable, error handling is determined by S. It performs the following steps when called:
    //
    //  1. Let DclRec be envRec.[[DeclarativeRecord]].
    //  2. If DclRec.HasBinding(N) is true, then
    //      a. Return DclRec.SetMutableBinding(N, V, S).
    //  3. Let ObjRec be envRec.[[ObjectRecord]].
    //  4. Return ? ObjRec.SetMutableBinding(N, V, S).
    fn set_mutable_binding(
        &self,
        agent: &mut Agent,
        name: JSString,
        value: ECMAScriptValue,
        strict: bool,
    ) -> Completion<()> {
        if self.declarative_record.has_binding(agent, &name).unwrap() {
            self.declarative_record.set_mutable_binding(agent, name, value, strict)
        } else {
            self.object_record.set_mutable_binding(agent, name, value, strict)
        }
    }

    // GetBindingValue ( N, S )
    //
    // The GetBindingValue concrete method of a global Environment Record envRec takes arguments N (a String) and S (a
    // Boolean). It returns the value of its bound identifier whose name is the value of the argument N. If the binding
    // is an uninitialized binding throw a ReferenceError exception. A property named N normally already exists but if
    // it does not or is not currently writable, error handling is determined by S. It performs the following steps when
    // called:
    //
    //  1. Let DclRec be envRec.[[DeclarativeRecord]].
    //  2. If DclRec.HasBinding(N) is true, then
    //      a. Return DclRec.GetBindingValue(N, S).
    //  3. Let ObjRec be envRec.[[ObjectRecord]].
    //  4. Return ? ObjRec.GetBindingValue(N, S).
    fn get_binding_value(&self, agent: &mut Agent, name: &JSString, strict: bool) -> Completion<ECMAScriptValue> {
        if self.declarative_record.has_binding(agent, name).unwrap() {
            self.declarative_record.get_binding_value(agent, name, strict)
        } else {
            self.object_record.get_binding_value(agent, name, strict)
        }
    }

    // DeleteBinding ( N )
    //
    // The DeleteBinding concrete method of a global Environment Record envRec takes argument N (a String). It can only
    // delete bindings that have been explicitly designated as being subject to deletion. It performs the following
    // steps when called:
    //
    //  1. Let DclRec be envRec.[[DeclarativeRecord]].
    //  2. If DclRec.HasBinding(N) is true, then
    //      a. Return DclRec.DeleteBinding(N).
    //  3. Let ObjRec be envRec.[[ObjectRecord]].
    //  4. Let globalObject be ObjRec.[[BindingObject]].
    //  5. Let existingProp be ? HasOwnProperty(globalObject, N).
    //  6. If existingProp is true, then
    //      a. Let status be ? ObjRec.DeleteBinding(N).
    //      b. If status is true, then
    //          i. Let varNames be envRec.[[VarNames]].
    //          ii. If N is an element of varNames, remove that element from the varNames.
    //      c. Return status.
    //  7. Return true.
    fn delete_binding(&self, agent: &mut Agent, name: &JSString) -> Completion<bool> {
        if self.declarative_record.has_binding(agent, name).unwrap() {
            self.declarative_record.delete_binding(agent, name)
        } else {
            let global_object = &self.object_record.binding_object;
            if has_own_property(agent, global_object, &name.clone().into())? {
                let status = self.object_record.delete_binding(agent, name)?;
                if status {
                    self.var_names.borrow_mut().remove(name);
                }
                Ok(status)
            } else {
                Ok(true)
            }
        }
    }

    // HasThisBinding ( )
    //
    // The HasThisBinding concrete method of a global Environment Record envRec takes no arguments. It performs the
    // following steps when called:
    //
    //  1. Return true.
    //
    // NOTE     Global Environment Records always provide a this binding.
    fn has_this_binding(&self) -> bool {
        true
    }

    // HasSuperBinding ( )
    //
    // The HasSuperBinding concrete method of a global Environment Record envRec takes no arguments. It performs the
    // following steps when called:
    //
    //  1. Return false.
    //
    // NOTE     Global Environment Records do not provide a super binding.
    fn has_super_binding(&self) -> bool {
        false
    }

    // WithBaseObject ( )
    //
    // The WithBaseObject concrete method of a global Environment Record envRec takes no arguments. It performs the
    // following steps when called:
    //
    //  1. Return undefined.
    fn with_base_object(&self) -> Option<Object> {
        None
    }

    fn get_outer_env(&self) -> Option<Rc<dyn EnvironmentRecord>> {
        None
    }

    // GetThisBinding ( )
    //
    // The GetThisBinding concrete method of a global Environment Record envRec takes no arguments. It performs the
    // following steps when called:
    //
    //  1. Return envRec.[[GlobalThisValue]].
    fn get_this_binding(&self, _: &mut Agent) -> Completion<ECMAScriptValue> {
        Ok(ECMAScriptValue::from(self.global_this_value.clone()))
    }
}

impl GlobalEnvironmentRecord {
    // HasVarDeclaration ( N )
    //
    // The HasVarDeclaration concrete method of a global Environment Record envRec takes argument N (a String). It
    // determines if the argument identifier has a binding in this record that was created using a VariableStatement or
    // a FunctionDeclaration. It performs the following steps when called:
    //
    //  1. Let varDeclaredNames be envRec.[[VarNames]].
    //  2. If varDeclaredNames contains N, return true.
    //  3. Return false.
    pub fn has_var_declaration(&self, name: &JSString) -> bool {
        self.var_names.borrow().contains(name)
    }

    // HasLexicalDeclaration ( N )
    //
    // The HasLexicalDeclaration concrete method of a global Environment Record envRec takes argument N (a String). It
    // determines if the argument identifier has a binding in this record that was created using a lexical declaration
    // such as a LexicalDeclaration or a ClassDeclaration. It performs the following steps when called:
    //
    //  1. Let DclRec be envRec.[[DeclarativeRecord]].
    //  2. Return DclRec.HasBinding(N).
    pub fn has_lexical_declaration(&self, agent: &mut Agent, name: &JSString) -> bool {
        self.declarative_record.has_binding(agent, name).unwrap()
    }

    // HasRestrictedGlobalProperty ( N )
    //
    // The HasRestrictedGlobalProperty concrete method of a global Environment Record envRec takes argument N (a
    // String). It determines if the argument identifier is the name of a property of the global object that must not be
    // shadowed by a global lexical binding. It performs the following steps when called:
    //
    //  1. Let ObjRec be envRec.[[ObjectRecord]].
    //  2. Let globalObject be ObjRec.[[BindingObject]].
    //  3. Let existingProp be ? globalObject.[[GetOwnProperty]](N).
    //  4. If existingProp is undefined, return false.
    //  5. If existingProp.[[Configurable]] is true, return false.
    //  6. Return true.
    //
    // NOTE     Properties may exist upon a global object that were directly created rather than being declared using a
    //          var or function declaration. A global lexical binding may not be created that has the same name as a
    //          non-configurable property of the global object. The global property "undefined" is an example of such a
    //          property.
    pub fn has_restricted_global_property(&self, agent: &mut Agent, name: &JSString) -> Completion<bool> {
        let global_object = &self.object_record.binding_object;
        let existing_prop = global_object.o.get_own_property(agent, &name.clone().into())?;
        match existing_prop {
            None => Ok(false),
            Some(prop) => Ok(!prop.configurable),
        }
    }

    // CanDeclareGlobalVar ( N )
    //
    // The CanDeclareGlobalVar concrete method of a global Environment Record envRec takes argument N (a String). It
    // determines if a corresponding CreateGlobalVarBinding call would succeed if called for the same argument N.
    // Redundant var declarations and var declarations for pre-existing global object properties are allowed. It
    // performs the following steps when called:
    //
    //  1. Let ObjRec be envRec.[[ObjectRecord]].
    //  2. Let globalObject be ObjRec.[[BindingObject]].
    //  3. Let hasProperty be ? HasOwnProperty(globalObject, N).
    //  4. If hasProperty is true, return true.
    //  5. Return ? IsExtensible(globalObject).
    pub fn can_declare_global_var(&self, agent: &mut Agent, name: &JSString) -> Completion<bool> {
        let global_object = &self.object_record.binding_object;
        Ok(has_own_property(agent, global_object, &name.clone().into())? || is_extensible(agent, global_object)?)
    }

    // CanDeclareGlobalFunction ( N )
    //
    // The CanDeclareGlobalFunction concrete method of a global Environment Record envRec takes argument N (a String).
    // It determines if a corresponding CreateGlobalFunctionBinding call would succeed if called for the same argument
    // N. It performs the following steps when called:
    //
    //  1. Let ObjRec be envRec.[[ObjectRecord]].
    //  2. Let globalObject be ObjRec.[[BindingObject]].
    //  3. Let existingProp be ? globalObject.[[GetOwnProperty]](N).
    //  4. If existingProp is undefined, return ? IsExtensible(globalObject).
    //  5. If existingProp.[[Configurable]] is true, return true.
    //  6. If IsDataDescriptor(existingProp) is true and existingProp has attribute values { [[Writable]]: true, [[Enumerable]]: true }, return true.
    //  7. Return false.
    pub fn can_declare_global_function(&self, agent: &mut Agent, name: &JSString) -> Completion<bool> {
        let global_object = &self.object_record.binding_object;
        let existing_prop = global_object.o.get_own_property(agent, &name.clone().into())?;
        match existing_prop {
            None => is_extensible(agent, global_object),
            Some(prop) => {
                Ok(prop.configurable
                    || (prop.is_data_descriptor() && prop.is_writable() == Some(true) && prop.enumerable))
            }
        }
    }

    // CreateGlobalVarBinding ( N, D )
    //
    // The CreateGlobalVarBinding concrete method of a global Environment Record envRec takes arguments N (a String) and
    // D (a Boolean). It creates and initializes a mutable binding in the associated object Environment Record and
    // records the bound name in the associated [[VarNames]] List. If a binding already exists, it is reused and assumed
    // to be initialized. It performs the following steps when called:
    //
    //  1. Let ObjRec be envRec.[[ObjectRecord]].
    //  2. Let globalObject be ObjRec.[[BindingObject]].
    //  3. Let hasProperty be ? HasOwnProperty(globalObject, N).
    //  4. Let extensible be ? IsExtensible(globalObject).
    //  5. If hasProperty is false and extensible is true, then
    //      a. Perform ? ObjRec.CreateMutableBinding(N, D).
    //      b. Perform ? ObjRec.InitializeBinding(N, undefined).
    //  6. Let varDeclaredNames be envRec.[[VarNames]].
    //  7. If varDeclaredNames does not contain N, then
    //      a. Append N to varDeclaredNames.
    //  8. Return NormalCompletion(empty).
    pub fn create_global_var_binding(&self, agent: &mut Agent, name: JSString, deletable: bool) -> Completion<()> {
        let global_object = &self.object_record.binding_object;
        let has_property = has_own_property(agent, global_object, &name.clone().into())?;
        let extensible = is_extensible(agent, global_object)?;
        if !has_property && extensible {
            self.object_record.create_mutable_binding(agent, name.clone(), deletable)?;
            self.object_record.initialize_binding(agent, &name, ECMAScriptValue::Undefined)?;
        }
        self.var_names.borrow_mut().insert(name);
        Ok(())
    }

    // CreateGlobalFunctionBinding ( N, V, D )
    //
    // The CreateGlobalFunctionBinding concrete method of a global Environment Record envRec takes arguments N (a
    // String), V (an ECMAScript language value), and D (a Boolean). It creates and initializes a mutable binding in the
    // associated object Environment Record and records the bound name in the associated [[VarNames]] List. If a binding
    // already exists, it is replaced. It performs the following steps when called:
    //
    //  1. Let ObjRec be envRec.[[ObjectRecord]].
    //  2. Let globalObject be ObjRec.[[BindingObject]].
    //  3. Let existingProp be ? globalObject.[[GetOwnProperty]](N).
    //  4. If existingProp is undefined or existingProp.[[Configurable]] is true, then
    //      a. Let desc be the PropertyDescriptor { [[Value]]: V, [[Writable]]: true, [[Enumerable]]: true,
    //         [[Configurable]]: D }.
    //  5. Else,
    //      a. Let desc be the PropertyDescriptor { [[Value]]: V }.
    //  6. Perform ? DefinePropertyOrThrow(globalObject, N, desc).
    //  7. Perform ? Set(globalObject, N, V, false).
    //  8. Let varDeclaredNames be envRec.[[VarNames]].
    //  9. If varDeclaredNames does not contain N, then
    //      a. Append N to varDeclaredNames.
    //  10. Return NormalCompletion(empty).
    //
    // NOTE     Global function declarations are always represented as own properties of the global object. If possible,
    //          an existing own property is reconfigured to have a standard set of attribute values. Step 7 is
    //          equivalent to what calling the InitializeBinding concrete method would do and if globalObject is a Proxy
    //          will produce the same sequence of Proxy trap calls.
    pub fn create_global_function_binding(
        &self,
        agent: &mut Agent,
        name: JSString,
        val: ECMAScriptValue,
        deletable: bool,
    ) -> Completion<()> {
        let global_object = &self.object_record.binding_object;
        let prop_key = PropertyKey::from(name.clone());
        let existing_prop = global_object.o.get_own_property(agent, &prop_key)?;
        let full_pd =
            |v, d| PotentialPropertyDescriptor::new().value(v).writable(true).enumerable(true).configurable(d);
        let desc = match existing_prop {
            None => full_pd(val.clone(), deletable),
            Some(prop) if prop.configurable => full_pd(val.clone(), deletable),
            _ => PotentialPropertyDescriptor::new().value(val.clone()),
        };
        define_property_or_throw(agent, global_object, prop_key.clone(), desc)?;
        set(agent, global_object, prop_key, val, false)?;
        self.var_names.borrow_mut().insert(name);
        Ok(())
    }

    // NewGlobalEnvironment ( G, thisValue )
    //
    // The abstract operation NewGlobalEnvironment takes arguments G and thisValue. It performs the following steps when
    // called:
    //
    //      1. Let objRec be NewObjectEnvironment(G, false, null).
    //      2. Let dclRec be a new declarative Environment Record containing no bindings.
    //      3. Let env be a new global Environment Record.
    //      4. Set env.[[ObjectRecord]] to objRec.
    //      5. Set env.[[GlobalThisValue]] to thisValue.
    //      6. Set env.[[DeclarativeRecord]] to dclRec.
    //      7. Set env.[[VarNames]] to a new empty List.
    //      8. Set env.[[OuterEnv]] to null.
    //      9. Return env.
    pub fn new(global: Object, this_value: Object, name: impl Into<String>) -> Self {
        let name = name.into();
        let obj_rec = ObjectEnvironmentRecord::new(global, false, None, format!("{name}-obj"));
        let dcl_rec = DeclarativeEnvironmentRecord::new(None, format!("{name}-dcl"));
        Self {
            object_record: obj_rec,
            global_this_value: this_value,
            declarative_record: dcl_rec,
            var_names: Default::default(),
            name,
        }
    }

    #[cfg(test)]
    pub fn var_decls(&self) -> Vec<JSString> {
        let var_names = self.var_names.borrow();
        var_names.iter().cloned().collect::<Vec<JSString>>()
    }
    #[cfg(test)]
    pub fn lex_decls(&self) -> Vec<JSString> {
        let bindings = self.declarative_record.bindings.borrow();
        bindings.keys().cloned().collect::<Vec<_>>()
    }
}

// GetIdentifierReference ( env, name, strict )
//
// The abstract operation GetIdentifierReference takes arguments env (an Environment Record or null), name (a String),
// and strict (a Boolean). It performs the following steps when called:
//
//  1. If env is the value null, then
//      a. Return the Reference Record { [[Base]]: unresolvable, [[ReferencedName]]: name, [[Strict]]: strict, [[ThisValue]]: empty }.
//  2. Let exists be ? env.HasBinding(name).
//  3. If exists is true, then
//      a. Return the Reference Record { [[Base]]: env, [[ReferencedName]]: name, [[Strict]]: strict, [[ThisValue]]: empty }.
//  4. Else,
//      a. Let outer be env.[[OuterEnv]].
//      b. Return ? GetIdentifierReference(outer, name, strict).
pub fn get_identifier_reference(
    agent: &mut Agent,
    environment: Option<Rc<dyn EnvironmentRecord>>,
    name: JSString,
    strict: bool,
) -> Completion<Reference> {
    match environment {
        None => Ok(Reference::new(Base::Unresolvable, name, strict, None)),
        Some(env) => {
            let exists = env.has_binding(agent, &name)?;
            if exists {
                Ok(Reference::new(Base::Environment(env.clone()), name, strict, None))
            } else {
                let outer = env.get_outer_env();
                get_identifier_reference(agent, outer, name, strict)
            }
        }
    }
}

// PrivateEnvironment Records
//
// A PrivateEnvironment Record is a specification mechanism used to track Private Names based upon the lexical nesting
// structure of ClassDeclarations and ClassExpressions in ECMAScript code. They are similar to, but distinct from,
// Environment Records. Each PrivateEnvironment Record is associated with a ClassDeclaration or ClassExpression. Each
// time such a class is evaluated, a new PrivateEnvironment Record is created to record the Private Names declared by
// that class.
//
// Each PrivateEnvironment Record has the fields defined in Table 27.
//
// Table 27: PrivateEnvironment Record Fields
//
// +-----------------------------+-----------------------+-------------------------------------------------------------+
// | Field Name                  | Value Type            | Meaning                                                     |
// +-----------------------------+-----------------------+-------------------------------------------------------------+
// | [[OuterPrivateEnvironment]] | PrivateEnvironment    | The PrivateEnvironment Record of the nearest containing     |
// |                             | Record or null        | class. null if the class with which this PrivateEnvironment |
// |                             |                       | Record is associated is not contained in any other class.   |
// +-----------------------------+-----------------------+-------------------------------------------------------------+
// | [[Names]]                   | List of Private Names | The Private Names declared by this class.                   |
// +-----------------------------+-----------------------+-------------------------------------------------------------+
#[derive(Debug)]
pub struct PrivateEnvironmentRecord {
    outer_private_environment: Option<Box<PrivateEnvironmentRecord>>,
    names: Vec<PrivateName>,
}

impl PrivateEnvironmentRecord {
    // NewPrivateEnvironment ( outerPrivEnv )
    //
    // The abstract operation NewPrivateEnvironment takes argument outerPrivEnv (a PrivateEnvironment Record or null).
    // It performs the following steps when called:
    //
    //  1. Let names be a new empty List.
    //  2. Return the PrivateEnvironment Record { [[OuterPrivateEnvironment]]: outerPrivEnv, [[Names]]: names }.
    pub fn new(outer_priv_env: Option<Box<PrivateEnvironmentRecord>>) -> Self {
        PrivateEnvironmentRecord { outer_private_environment: outer_priv_env, names: vec![] }
    }

    // ResolvePrivateIdentifier ( privEnv, identifier )
    //
    // The abstract operation ResolvePrivateIdentifier takes arguments privEnv (a PrivateEnvironment Record) and
    // identifier (a String). It performs the following steps when called:
    //
    //  1. Let names be privEnv.[[Names]].
    //  2. If names contains a Private Name whose [[Description]] is identifier, then
    //      a. Let name be that Private Name.
    //      b. Return name.
    //  3. Else,
    //      a. Let outerPrivEnv be privEnv.[[OuterPrivateEnvironment]].
    //      b. Assert: outerPrivEnv is not null.
    //      c. Return ResolvePrivateIdentifier(outerPrivEnv, identifier).
    pub fn resolve_private_identifier(&self, identifier: &JSString) -> PrivateName {
        match self.names.iter().find(|&item| item.description == *identifier) {
            Some(x) => x.clone(),
            None => self.outer_private_environment.as_ref().unwrap().resolve_private_identifier(identifier),
        }
    }
}

#[cfg(test)]
mod tests;
