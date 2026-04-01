use super::*;
use ahash::AHashSet;
use itertools::Itertools;
use num::{BigInt, ToPrimitive};
use regex::Regex;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;
use std::sync::LazyLock;

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub(crate) enum IntrinsicId {
    // If you add something here, _please_ update the ALL_INTRINSIC_IDS list in the unit tests!
    // And to the "which" function, below.
    Array,
    ArrayPrototype,
    ArrayPrototypeValues,
    ArrayIteratorPrototype,
    //AsyncFunctionPrototype,
    //AsyncGeneratorFunctionPrototype,
    //AsyncGeneratorFunctionPrototypePrototype,
    BigInt,
    BigIntPrototype,
    Boolean,
    BooleanPrototype,
    Date,
    DatePrototype,
    DecodeURI,
    DecodeURIComponent,
    EncodeURI,
    EncodeURIComponent,
    Error,
    ErrorPrototype,
    Eval,
    EvalError,
    EvalErrorPrototype,
    ForInIteratorPrototype,
    Function,
    FunctionPrototype,
    GeneratorFunction,
    GeneratorFunctionPrototype,
    GeneratorFunctionPrototypePrototype,
    GeneratorFunctionPrototypePrototypeNext,
    IsFinite,
    IsNaN,
    IteratorPrototype,
    Json,
    Map,
    MapPrototype,
    MapIteratorPrototype,
    Math,
    Number,
    NumberPrototype,
    Object,
    ObjectPrototype,
    ObjectPrototypeToString,
    ParseFloat,
    ParseInt,
    Proxy,
    RangeError,
    RangeErrorPrototype,
    ReferenceError,
    ReferenceErrorPrototype,
    Reflect,
    RegExp,
    RegExpPrototype,
    String,
    StringIteratorPrototype,
    StringPrototype,
    Symbol,
    SymbolPrototype,
    SyntaxError,
    SyntaxErrorPrototype,
    ThrowTypeError,
    TypeError,
    TypeErrorPrototype,
    URIError,
    URIErrorPrototype,
}

pub(crate) struct Intrinsics {
    //pub(crate) aggregate_error: Object, // aka "AggregateError", The AggregateError constructor
    pub(crate) array: Object,                  // aka "Array", The Array constructor
    pub(crate) array_prototype: Object,        // %Array.prototype%
    pub(crate) array_prototype_values: Object, // %Array.prototype.values%
    //pub(crate) array_buffer: Object,    // ArrayBuffer	The ArrayBuffer constructor (25.1.3)
    pub(crate) array_iterator_prototype: Object, // The prototype of Array iterator objects (23.1.5)
    //pub(crate) async_from_sync_iterator_prototype: Object, // The prototype of async-from-sync iterator objects (27.1.4)
    //pub(crate) async_function: Object,  // The constructor of async function objects (27.7.1)
    //pub(crate) async_function_prototype: Object,
    //pub(crate) async_generator_function: Object, // The constructor of async iterator objects (27.4.1)
    //pub(crate) async_generator_function_prototype: Object,
    //pub(crate) async_generator_function_prototype_prototype: Object,
    //pub(crate) async_iterator_prototype: Object, // An object that all standard built-in async iterator objects indirectly inherit from
    //pub(crate) atomics: Object,                  // Atomics	The Atomics object (25.4)
    pub(crate) big_int: Object,           // BigInt	The BigInt constructor (21.2.1)
    pub(crate) big_int_prototype: Object, //
    //pub(crate) big_int64_array: Object,          // BigInt64Array	The BigInt64Array constructor (23.2)
    //pub(crate) big_uint64_array: Object,         // BigUint64Array	The BigUint64Array constructor (23.2)
    pub(crate) boolean: Object,           // Boolean	The Boolean constructor (20.3.1)
    pub(crate) boolean_prototype: Object, // The Boolean object prototoype
    //pub(crate) data_view: Object,                // DataView	The DataView constructor (25.3.2)
    pub(crate) date: Object,                 // Date	The Date constructor (21.4.2)
    pub(crate) date_prototype: Object,       // Date.prototype
    pub(crate) decode_uri: Object,           // decodeURI	The decodeURI function (19.2.6.2)
    pub(crate) decode_uri_component: Object, // decodeURIComponent	The decodeURIComponent function (19.2.6.3)
    pub(crate) encode_uri: Object,           // encodeURI	The encodeURI function (19.2.6.4)
    pub(crate) encode_uri_component: Object, // encodeURIComponent	The encodeURIComponent function (19.2.6.5)
    pub(crate) error: Object,                // Error	The Error constructor (20.5.1)
    pub(crate) error_prototype: Object,      //
    pub(crate) eval: Object,                 // eval	The eval function (19.2.1)
    pub(crate) eval_error: Object,           // EvalError	The EvalError constructor (20.5.5.1)
    pub(crate) eval_error_prototype: Object, //
    //pub(crate) finalization_registry: Object,    // FinalizationRegistry	The FinalizationRegistry constructor (26.2.1)
    //pub(crate) float32_array: Object,            // Float32Array	The Float32Array constructor (23.2)
    //pub(crate) float64_array: Object,            // Float64Array	The Float64Array constructor (23.2)
    pub(crate) for_in_iterator_prototype: Object, // The prototype of For-In iterator objects (14.7.5.10)
    pub(crate) function: Object,                  // Function	The Function constructor (20.2.1)
    pub(crate) function_prototype: Object,        //
    pub(crate) generator_function: Object,        // The constructor of generator objects (27.3.1)
    pub(crate) generator_function_prototype: Object, //
    pub(crate) generator_function_prototype_prototype: Object, //
    pub(crate) generator_function_prototype_prototype_next: Object, //
    //pub(crate) int8_array: Object,               // Int8Array	The Int8Array constructor (23.2)
    //pub(crate) int16_array: Object,              // Int16Array	The Int16Array constructor (23.2)
    //pub(crate) int32_array: Object,              // Int32Array	The Int32Array constructor (23.2)
    pub(crate) is_finite: Object,                  // isFinite	The isFinite function (19.2.2)
    pub(crate) is_nan: Object,                     // isNaN	The isNaN function (19.2.3)
    pub(crate) iterator_prototype: Object, // An object that all standard built-in iterator objects indirectly inherit from
    pub(crate) json: Object,               // JSON	The JSON object (25.5)
    pub(crate) map: Object,                // Map	The Map constructor (24.1.1)
    pub(crate) map_prototype: Object,      // Map's prototype object
    pub(crate) map_iterator_prototype: Object, // The prototype of Map iterator objects (24.1.5)
    pub(crate) math: Object,               // Math	The Math object (21.3)
    pub(crate) number: Object,             // Number	The Number constructor (21.1.1)
    pub(crate) number_prototype: Object,   //
    pub(crate) object: Object,             // Object	The Object constructor (20.1.1)
    pub(crate) object_prototype: Object,   // The Object prototype object
    pub(crate) object_prototype_to_string: Object, // The initial value of %ObjectPrototype%.toString
    pub(crate) parse_float: Object,        // parseFloat	The parseFloat function (19.2.4)
    pub(crate) parse_int: Object,          // parseInt	The parseInt function (19.2.5)
    //pub(crate) promise: Object,            // Promise	The Promise constructor (27.2.3)
    pub(crate) proxy: Object,                     // Proxy	The Proxy constructor (28.2.1)
    pub(crate) range_error: Object,               // RangeError	The RangeError constructor (20.5.5.2)
    pub(crate) range_error_prototype: Object,     //
    pub(crate) reference_error: Object,           // ReferenceError	The ReferenceError constructor (20.5.5.3)
    pub(crate) reference_error_prototype: Object, //
    pub(crate) reflect: Object,                   // Reflect	The Reflect object (28.1)
    pub(crate) reg_exp: Object,                   // RegExp	The RegExp constructor (22.2.3)
    pub(crate) reg_exp_prototype: Object,
    //pub(crate) reg_exp_string_iterator_prototype: Object, // The prototype of RegExp String Iterator objects (22.2.7)
    //pub(crate) set: Object,                               // Set	The Set constructor (24.2.1)
    //pub(crate) set_iterator_prototype: Object,            // The prototype of Set iterator objects (24.2.5)
    //pub(crate) shared_array_buffer: Object, // SharedArrayBuffer	The SharedArrayBuffer constructor (25.2.2)
    pub(crate) string: Object,                    // String	The String constructor (22.1.1)
    pub(crate) string_prototype: Object,          // Initial value of %String.prototype%.
    pub(crate) string_iterator_prototype: Object, // The prototype of String iterator objects (22.1.5)
    pub(crate) symbol: Object,                    // Symbol	The Symbol constructor (20.4.1)
    pub(crate) symbol_prototype: Object,          //
    pub(crate) syntax_error: Object,              // SyntaxError	The SyntaxError constructor (20.5.5.4)
    pub(crate) syntax_error_prototype: Object,    //
    pub(crate) throw_type_error: Object, // A function object that unconditionally throws a new instance of %TypeError%
    //pub(crate) typed_array: Object,      // The super class of all typed Array constructors (23.2.1)
    pub(crate) type_error: Object, // TypeError	The TypeError constructor (20.5.5.5)
    pub(crate) type_error_prototype: Object, //
    //pub(crate) uint8_array: Object,      // Uint8Array	The Uint8Array constructor (23.2)
    //pub(crate) uint8_clampedarray: Object, // Uint8ClampedArray	The Uint8ClampedArray constructor (23.2)
    //pub(crate) uint16_array: Object,     // Uint16Array	The Uint16Array constructor (23.2)
    //pub(crate) uint32_array: Object,     // Uint32Array	The Uint32Array constructor (23.2)
    pub(crate) uri_error: Object, // URIError	The URIError constructor (20.5.5.6)
    pub(crate) uri_error_prototype: Object, //
                                  //pub(crate) weak_map: Object,         // WeakMap	The WeakMap constructor (24.3.1)
                                  //pub(crate) weak_ref: Object,         // WeakRef	The WeakRef constructor (26.1.1)
                                  //pub(crate) weak_set: Object,         // WeakSet	The WeakSet constructor (24.4.1)
}

impl Intrinsics {
    fn new() -> Self {
        // Since an intrinsics struct needs to be populated in a particular order and then fixed up, RAII doesn't work
        // so grand. Therefore, a "dead object" is placed in each field instead. This quickly gives us an initialized
        // struct, and also generates run-time panics if any "un-initialized" members are actually used. (Option<Object>
        // could have done a similar thing, except that then the check would be made for _every_ access of the field
        // itself, rather than when the data the field references is manipulated. In other words, this is actually far
        // less overhead.)
        let dead = DeadObject::object();
        Intrinsics {
            //aggregate_error: dead.clone(),
            array: dead.clone(),
            array_prototype: dead.clone(),
            array_prototype_values: dead.clone(),
            //array_buffer: dead.clone(),
            array_iterator_prototype: dead.clone(),
            //async_from_sync_iterator_prototype: dead.clone(),
            //async_function: dead.clone(),
            //async_function_prototype: dead.clone(),
            //async_generator_function: dead.clone(),
            //async_generator_function_prototype: dead.clone(),
            //async_generator_function_prototype_prototype: dead.clone(),
            //async_iterator_prototype: dead.clone(),
            //atomics: dead.clone(),
            big_int: dead.clone(),
            big_int_prototype: dead.clone(),
            //big_int64_array: dead.clone(),
            //big_uint64_array: dead.clone(),
            boolean: dead.clone(),
            boolean_prototype: dead.clone(),
            //data_view: dead.clone(),
            date: dead.clone(),
            date_prototype: dead.clone(),
            decode_uri: dead.clone(),
            decode_uri_component: dead.clone(),
            encode_uri: dead.clone(),
            encode_uri_component: dead.clone(),
            error: dead.clone(),
            error_prototype: dead.clone(),
            eval: dead.clone(),
            eval_error: dead.clone(),
            eval_error_prototype: dead.clone(),
            //finalization_registry: dead.clone(),
            //float32_array: dead.clone(),
            //float64_array: dead.clone(),
            for_in_iterator_prototype: dead.clone(),
            function: dead.clone(),
            function_prototype: dead.clone(),
            generator_function: dead.clone(),
            generator_function_prototype: dead.clone(),
            generator_function_prototype_prototype: dead.clone(),
            generator_function_prototype_prototype_next: dead.clone(),
            //int8_array: dead.clone(),
            //int16_array: dead.clone(),
            //int32_array: dead.clone(),
            is_finite: dead.clone(),
            is_nan: dead.clone(),
            iterator_prototype: dead.clone(),
            json: dead.clone(),
            map: dead.clone(),
            map_prototype: dead.clone(),
            map_iterator_prototype: dead.clone(),
            math: dead.clone(),
            number: dead.clone(),
            number_prototype: dead.clone(),
            object: dead.clone(),
            object_prototype: dead.clone(),
            object_prototype_to_string: dead.clone(),
            parse_float: dead.clone(),
            parse_int: dead.clone(),
            //promise: dead.clone(),
            proxy: dead.clone(),
            range_error: dead.clone(),
            range_error_prototype: dead.clone(),
            reference_error: dead.clone(),
            reference_error_prototype: dead.clone(),
            reflect: dead.clone(),
            reg_exp: dead.clone(),
            reg_exp_prototype: dead.clone(),
            //reg_exp_string_iterator_prototype: dead.clone(),
            //set: dead.clone(),
            //set_iterator_prototype: dead.clone(),
            //shared_array_buffer: dead.clone(),
            string: dead.clone(),
            string_prototype: dead.clone(),
            string_iterator_prototype: dead.clone(),
            symbol: dead.clone(),
            symbol_prototype: dead.clone(),
            syntax_error: dead.clone(),
            syntax_error_prototype: dead.clone(),
            throw_type_error: dead.clone(),
            //typed_array: dead.clone(),
            type_error: dead.clone(),
            type_error_prototype: dead.clone(),
            //uint8_array: dead.clone(),
            //uint8_clampedarray: dead.clone(),
            //uint16_array: dead.clone(),
            //uint32_array: dead.clone(),
            uri_error: dead.clone(),
            uri_error_prototype: dead.clone(),
            //weak_map: dead.clone(),
            //weak_ref: dead.clone(),
            //weak_set: dead,
        }
    }
    pub(crate) fn get(&self, id: IntrinsicId) -> Object {
        match id {
            IntrinsicId::Array => &self.array,
            IntrinsicId::ArrayPrototype => &self.array_prototype,
            IntrinsicId::ArrayPrototypeValues => &self.array_prototype_values,
            IntrinsicId::ArrayIteratorPrototype => &self.array_iterator_prototype,
            //IntrinsicId::AsyncFunctionPrototype => &self.async_function_prototype,
            //IntrinsicId::AsyncGeneratorFunctionPrototype => &self.async_generator_function_prototype,
            //IntrinsicId::AsyncGeneratorFunctionPrototypePrototype => &self.async_generator_function_prototype_prototype,
            IntrinsicId::BigInt => &self.big_int,
            IntrinsicId::BigIntPrototype => &self.big_int_prototype,
            IntrinsicId::Boolean => &self.boolean,
            IntrinsicId::BooleanPrototype => &self.boolean_prototype,
            IntrinsicId::Date => &self.date,
            IntrinsicId::DatePrototype => &self.date_prototype,
            IntrinsicId::DecodeURI => &self.decode_uri,
            IntrinsicId::DecodeURIComponent => &self.decode_uri_component,
            IntrinsicId::EncodeURI => &self.encode_uri,
            IntrinsicId::EncodeURIComponent => &self.encode_uri_component,
            IntrinsicId::Error => &self.error,
            IntrinsicId::ErrorPrototype => &self.error_prototype,
            IntrinsicId::Eval => &self.eval,
            IntrinsicId::EvalError => &self.eval_error,
            IntrinsicId::EvalErrorPrototype => &self.eval_error_prototype,
            IntrinsicId::ForInIteratorPrototype => &self.for_in_iterator_prototype,
            IntrinsicId::Function => &self.function,
            IntrinsicId::FunctionPrototype => &self.function_prototype,
            IntrinsicId::GeneratorFunction => &self.generator_function,
            IntrinsicId::GeneratorFunctionPrototype => &self.generator_function_prototype,
            IntrinsicId::GeneratorFunctionPrototypePrototype => &self.generator_function_prototype_prototype,
            IntrinsicId::GeneratorFunctionPrototypePrototypeNext => &self.generator_function_prototype_prototype_next,
            IntrinsicId::IsFinite => &self.is_finite,
            IntrinsicId::IsNaN => &self.is_nan,
            IntrinsicId::IteratorPrototype => &self.iterator_prototype,
            IntrinsicId::Json => &self.json,
            IntrinsicId::Map => &self.map,
            IntrinsicId::MapPrototype => &self.map_prototype,
            IntrinsicId::MapIteratorPrototype => &self.map_iterator_prototype,
            IntrinsicId::Math => &self.math,
            IntrinsicId::Number => &self.number,
            IntrinsicId::NumberPrototype => &self.number_prototype,
            IntrinsicId::Object => &self.object,
            IntrinsicId::ObjectPrototype => &self.object_prototype,
            IntrinsicId::ObjectPrototypeToString => &self.object_prototype_to_string,
            IntrinsicId::ParseFloat => &self.parse_float,
            IntrinsicId::ParseInt => &self.parse_int,
            IntrinsicId::Proxy => &self.proxy,
            IntrinsicId::RangeError => &self.range_error,
            IntrinsicId::RangeErrorPrototype => &self.range_error_prototype,
            IntrinsicId::ReferenceError => &self.reference_error,
            IntrinsicId::ReferenceErrorPrototype => &self.reference_error_prototype,
            IntrinsicId::Reflect => &self.reflect,
            IntrinsicId::RegExp => &self.reg_exp,
            IntrinsicId::RegExpPrototype => &self.reg_exp_prototype,
            IntrinsicId::String => &self.string,
            IntrinsicId::StringIteratorPrototype => &self.string_iterator_prototype,
            IntrinsicId::StringPrototype => &self.string_prototype,
            IntrinsicId::Symbol => &self.symbol,
            IntrinsicId::SymbolPrototype => &self.symbol_prototype,
            IntrinsicId::SyntaxError => &self.syntax_error,
            IntrinsicId::SyntaxErrorPrototype => &self.syntax_error_prototype,
            IntrinsicId::ThrowTypeError => &self.throw_type_error,
            IntrinsicId::TypeError => &self.type_error,
            IntrinsicId::TypeErrorPrototype => &self.type_error_prototype,
            IntrinsicId::URIError => &self.uri_error,
            IntrinsicId::URIErrorPrototype => &self.uri_error_prototype,
        }
        .clone()
    }
    pub(crate) fn which(&self, intrinsic: &Object) -> Option<IntrinsicId> {
        match intrinsic {
            o if o == &self.array => Some(IntrinsicId::Array),
            o if o == &self.array_prototype => Some(IntrinsicId::ArrayPrototype),
            o if o == &self.array_prototype_values => Some(IntrinsicId::ArrayPrototypeValues),
            o if o == &self.array_iterator_prototype => Some(IntrinsicId::ArrayIteratorPrototype),
            o if o == &self.big_int => Some(IntrinsicId::BigInt),
            o if o == &self.big_int_prototype => Some(IntrinsicId::BigIntPrototype),
            o if o == &self.boolean => Some(IntrinsicId::Boolean),
            o if o == &self.boolean_prototype => Some(IntrinsicId::BooleanPrototype),
            o if o == &self.date => Some(IntrinsicId::Date),
            o if o == &self.date_prototype => Some(IntrinsicId::DatePrototype),
            o if o == &self.decode_uri => Some(IntrinsicId::DecodeURI),
            o if o == &self.decode_uri_component => Some(IntrinsicId::DecodeURIComponent),
            o if o == &self.encode_uri => Some(IntrinsicId::EncodeURI),
            o if o == &self.encode_uri_component => Some(IntrinsicId::EncodeURIComponent),
            o if o == &self.error => Some(IntrinsicId::Error),
            o if o == &self.error_prototype => Some(IntrinsicId::ErrorPrototype),
            o if o == &self.eval => Some(IntrinsicId::Eval),
            o if o == &self.eval_error => Some(IntrinsicId::EvalError),
            o if o == &self.eval_error_prototype => Some(IntrinsicId::EvalErrorPrototype),
            o if o == &self.for_in_iterator_prototype => Some(IntrinsicId::ForInIteratorPrototype),
            o if o == &self.function => Some(IntrinsicId::Function),
            o if o == &self.function_prototype => Some(IntrinsicId::FunctionPrototype),
            o if o == &self.is_finite => Some(IntrinsicId::IsFinite),
            o if o == &self.is_nan => Some(IntrinsicId::IsNaN),
            o if o == &self.iterator_prototype => Some(IntrinsicId::IteratorPrototype),
            o if o == &self.generator_function => Some(IntrinsicId::GeneratorFunction),
            o if o == &self.generator_function_prototype => Some(IntrinsicId::GeneratorFunctionPrototype),
            o if o == &self.generator_function_prototype_prototype => {
                Some(IntrinsicId::GeneratorFunctionPrototypePrototype)
            }
            o if o == &self.generator_function_prototype_prototype_next => {
                Some(IntrinsicId::GeneratorFunctionPrototypePrototypeNext)
            }
            o if o == &self.map => Some(IntrinsicId::Map),
            o if o == &self.map_prototype => Some(IntrinsicId::MapPrototype),
            o if o == &self.map_iterator_prototype => Some(IntrinsicId::MapIteratorPrototype),
            o if o == &self.math => Some(IntrinsicId::Math),
            o if o == &self.number => Some(IntrinsicId::Number),
            o if o == &self.number_prototype => Some(IntrinsicId::NumberPrototype),
            o if o == &self.object => Some(IntrinsicId::Object),
            o if o == &self.object_prototype => Some(IntrinsicId::ObjectPrototype),
            o if o == &self.object_prototype_to_string => Some(IntrinsicId::ObjectPrototypeToString),
            o if o == &self.parse_float => Some(IntrinsicId::ParseFloat),
            o if o == &self.parse_int => Some(IntrinsicId::ParseInt),
            o if o == &self.proxy => Some(IntrinsicId::Proxy),
            o if o == &self.range_error => Some(IntrinsicId::RangeError),
            o if o == &self.range_error_prototype => Some(IntrinsicId::RangeErrorPrototype),
            o if o == &self.reference_error => Some(IntrinsicId::ReferenceError),
            o if o == &self.reference_error_prototype => Some(IntrinsicId::ReferenceErrorPrototype),
            o if o == &self.reflect => Some(IntrinsicId::Reflect),
            o if o == &self.string => Some(IntrinsicId::String),
            o if o == &self.string_iterator_prototype => Some(IntrinsicId::StringIteratorPrototype),
            o if o == &self.string_prototype => Some(IntrinsicId::StringPrototype),
            o if o == &self.symbol => Some(IntrinsicId::Symbol),
            o if o == &self.symbol_prototype => Some(IntrinsicId::SymbolPrototype),
            o if o == &self.syntax_error => Some(IntrinsicId::SyntaxError),
            o if o == &self.syntax_error_prototype => Some(IntrinsicId::SyntaxErrorPrototype),
            o if o == &self.throw_type_error => Some(IntrinsicId::ThrowTypeError),
            o if o == &self.type_error => Some(IntrinsicId::TypeError),
            o if o == &self.type_error_prototype => Some(IntrinsicId::TypeErrorPrototype),
            o if o == &self.uri_error => Some(IntrinsicId::URIError),
            o if o == &self.uri_error_prototype => Some(IntrinsicId::URIErrorPrototype),
            _ => None,
        }
    }
}

impl fmt::Debug for Intrinsics {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Intrinsics").finish_non_exhaustive()
    }
}

pub(crate) type RealmId = u64;

pub(crate) struct Realm {
    // NOTE NOTE NOTE NOTE: Realm is a circular structure. The function objects in the Intrinsics field each have their
    // own Realm pointer, which points back to this structure. They _should_ be weak pointers, because of that, except
    // that anything can duplicate an intrinsic, and doing so should mean that the pointer is no longer weak.
    //
    // In addition, any objects within the global_env, or reachable via the global_env may be functions which have a
    // realm pointer that points back to here, again making cycles.
    //
    // Until I figure out a solution for that, once a realm is created, it can never be dropped. (Currently, aside from
    // testing, only one realm is ever made, so it's not a huge issue.)
    pub(crate) intrinsics: Intrinsics,
    pub(crate) global_object: Option<Object>,
    pub(crate) global_env: Option<Rc<GlobalEnvironmentRecord>>,
    // TemplateMap: later, when needed
    id: RealmId,
}

impl fmt::Debug for Realm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct(format!("Realm({})", self.id).as_str())
            .field("intrinsics", &self.intrinsics)
            .field("global_object", &ConciseOptionalObject::from(&self.global_object))
            .field("global_env", &ConciseOptionalGlobalEnvironmentRecord(self.global_env.clone()))
            .finish()
    }
}

impl Realm {
    #[cfg(test)]
    pub(crate) fn id(&self) -> RealmId {
        self.id
    }
}

// CreateRealm ( )
//
// The abstract operation CreateRealm takes no arguments. It performs the following steps when called:
//
//  1. Let realmRec be a new Realm Record.
//  2. Perform CreateIntrinsics(realmRec).
//  3. Set realmRec.[[GlobalObject]] to undefined.
//  4. Set realmRec.[[GlobalEnv]] to undefined.
//  5. Set realmRec.[[TemplateMap]] to a new empty List.
//  6. Return realmRec.
pub(crate) fn create_realm(id: RealmId) -> Rc<RefCell<Realm>> {
    let r = Rc::new(RefCell::new(Realm { intrinsics: Intrinsics::new(), global_object: None, global_env: None, id }));
    create_intrinsics(&r);
    r
}

// CreateIntrinsics ( realmRec )
//
// The abstract operation CreateIntrinsics takes argument realmRec. It performs the following steps when called:
//
//  1. Let intrinsics be a new Record.
//  2. Set realmRec.[[Intrinsics]] to intrinsics.
//  3. Set fields of intrinsics with the values listed in Table 8. The field names are the names listed in column one of
//     the table. The value of each field is a new object value fully and recursively populated with property values as
//     defined by the specification of each object in clauses 19 through 28. All object property values are newly
//     created object values. All values that are built-in function objects are created by performing
//     CreateBuiltinFunction(steps, length, name, slots, realmRec, prototype) where steps is the definition of that
//     function provided by this specification, name is the initial value of the function's name property, length is the
//     initial value of the function's length property, slots is a list of the names, if any, of the function's
//     specified internal slots, and prototype is the specified value of the function's [[Prototype]] internal slot. The
//     creation of the intrinsics and their properties must be ordered to avoid any dependencies upon objects that have
//     not yet been created.
//  4. Perform AddRestrictedFunctionProperties(intrinsics.[[%Function.prototype%]], realmRec).
//  5. Return intrinsics.
pub(crate) fn create_intrinsics(realm_rec: &Rc<RefCell<Realm>>) {
    // %Object.prototype%
    let object_prototype = immutable_prototype_exotic_object_create(None);
    realm_rec.borrow_mut().intrinsics.object_prototype = object_prototype.clone();
    // %Function.prototype%
    let function_prototype = create_builtin_function(
        Box::new(do_nothing),
        None,
        0_f64,
        PropertyKey::from(""),
        BUILTIN_FUNCTION_SLOTS,
        Some(realm_rec.clone()),
        Some(object_prototype.clone()),
        None,
    );
    realm_rec.borrow_mut().intrinsics.function_prototype = function_prototype.clone();
    // %ThrowTypeError%
    realm_rec.borrow_mut().intrinsics.throw_type_error = create_throw_type_error_builtin(realm_rec.clone());

    provision_function_intrinsic(realm_rec);
    provision_boolean_intrinsic(realm_rec);
    provision_number_intrinsic(realm_rec);
    provision_big_int_intrinsic(realm_rec);
    provision_error_intrinsic(realm_rec);
    provision_eval_error_intrinsic(realm_rec);
    provision_range_error_intrinsic(realm_rec);
    provision_reference_error_intrinsic(realm_rec);
    provision_syntax_error_intrinsic(realm_rec);
    provision_type_error_intrinsic(realm_rec);
    provision_uri_error_intrinsic(realm_rec);
    provision_object_intrinsic(realm_rec);
    provision_array_intrinsic(realm_rec);
    provision_symbol_intrinsic(realm_rec);
    provision_string_intrinsic(realm_rec);
    provision_iterator_prototype(realm_rec);
    provision_generator_function_intrinsics(realm_rec);
    provision_array_iterator_intrinsic(realm_rec); // must be after %IteratorPrototype% and %FunctionPrototype%
    provision_for_in_iterator_prototype(realm_rec); // must be after %IteratorPrototype% and %FunctionPrototype%
    provision_string_iterator_prototype(realm_rec); // must be after %IteratorPrototype% and %FunctionPrototype%
    provision_proxy_intrinsic(realm_rec);
    provision_math_intrinsic(realm_rec);
    provision_reflect_intrinsic(realm_rec);
    provision_regexp_intrinsic(realm_rec);
    provision_date_intrinsic(realm_rec);
    provision_map_intrinsic(realm_rec);
    provision_json_intrinsic(realm_rec);

    macro_rules! intrinsic_function {
        ( $intrinsicid:ident, $name:expr_2021, $length:expr_2021 ) => {
            let function_object = create_builtin_function(
                Box::new($intrinsicid),
                None,
                f64::from($length),
                PropertyKey::from($name),
                BUILTIN_FUNCTION_SLOTS,
                Some(realm_rec.clone()),
                Some(function_prototype.clone()),
                None,
            );
            realm_rec.borrow_mut().intrinsics.$intrinsicid = function_object;
        };
    }
    intrinsic_function!(decode_uri, "decodeURI", 1);
    intrinsic_function!(decode_uri_component, "decodeURIComponent", 1);
    intrinsic_function!(encode_uri, "encodeURI", 1);
    intrinsic_function!(encode_uri_component, "encodeURIComponent", 1);
    intrinsic_function!(eval, "eval", 1);
    intrinsic_function!(is_finite, "isFinite", 1);
    intrinsic_function!(is_nan, "isNaN", 1);
    intrinsic_function!(parse_float, "parseFloat", 1);
    intrinsic_function!(parse_int, "parseInt", 2);

    add_restricted_function_properties(&function_prototype, realm_rec);
}

// From function objects...
// AddRestrictedFunctionProperties ( F, realm )
//
// The abstract operation AddRestrictedFunctionProperties takes arguments F (a function object) and realm (a Realm
// Record). It performs the following steps when called:
//
// 1. Assert: realm.[[Intrinsics]].[[%ThrowTypeError%]] exists and has been initialized.
// 2. Let thrower be realm.[[Intrinsics]].[[%ThrowTypeError%]].
// 3. Perform ! DefinePropertyOrThrow(F, "caller", PropertyDescriptor { [[Get]]: thrower, [[Set]]: thrower,
//    [[Enumerable]]: false, [[Configurable]]: true }).
// 4. Return ! DefinePropertyOrThrow(F, "arguments", PropertyDescriptor { [[Get]]: thrower, [[Set]]: thrower,
//    [[Enumerable]]: false, [[Configurable]]: true }).
pub(crate) fn add_restricted_function_properties(f: &Object, realm: &Rc<RefCell<Realm>>) {
    let thrower = ECMAScriptValue::Object(realm.borrow().intrinsics.get(IntrinsicId::ThrowTypeError));
    define_property_or_throw(
        f,
        "caller",
        PotentialPropertyDescriptor::new()
            .get(thrower.clone())
            .set(thrower.clone())
            .enumerable(false)
            .configurable(true),
    )
    .unwrap();
    define_property_or_throw(
        f,
        "arguments",
        PotentialPropertyDescriptor::new().get(thrower.clone()).set(thrower).enumerable(false).configurable(true),
    )
    .unwrap();
}

// %ThrowTypeError% ( )
//
// The %ThrowTypeError% intrinsic is an anonymous built-in function object that is defined once for each realm. When
// %ThrowTypeError% is called it performs the following steps:
//
//      1. Throw a TypeError exception.
//
// The value of the [[Extensible]] internal slot of a %ThrowTypeError% function is false.
//
// The "length" property of a %ThrowTypeError% function has the attributes { [[Writable]]: false, [[Enumerable]]: false,
// [[Configurable]]: false }.
//
// The "name" property of a %ThrowTypeError% function has the attributes { [[Writable]]: false, [[Enumerable]]: false,
// [[Configurable]]: false }.
pub(crate) fn throw_type_error(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    Err(create_type_error("Generic TypeError"))
}

fn create_throw_type_error_builtin(realm: Rc<RefCell<Realm>>) -> Object {
    let function_proto = realm.borrow().intrinsics.get(IntrinsicId::FunctionPrototype);
    let fcn = create_builtin_function(
        Box::new(throw_type_error),
        None,
        0_f64,
        PropertyKey::from(""),
        BUILTIN_FUNCTION_SLOTS,
        Some(realm),
        Some(function_proto),
        None,
    );
    fcn.o.prevent_extensions().unwrap();
    define_property_or_throw(
        &fcn,
        "length",
        PotentialPropertyDescriptor::new().writable(false).enumerable(false).configurable(false),
    )
    .unwrap();
    define_property_or_throw(
        &fcn,
        "name",
        PotentialPropertyDescriptor::new().writable(false).enumerable(false).configurable(false),
    )
    .unwrap();

    fcn
}

#[expect(clippy::unnecessary_wraps)]
pub(crate) fn do_nothing(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    Ok(ECMAScriptValue::Undefined)
}

fn parse_hex_octet(string: &JSString, position: usize) -> Result<u8, Vec<Object>> {
    // 19.2.6.7 ParseHexOctet ( string, position )
    //
    // The abstract operation ParseHexOctet takes arguments string (a String) and position (a non-negative integer) and
    // returns either a non-negative integer or a non-empty List of SyntaxError objects. It parses a sequence of two
    // hexadecimal characters at the specified position in string into an unsigned 8-bit integer. It performs the
    // following steps when called:
    //
    // 1. Let len be the length of string.
    // 2. Assert: position + 2 ≤ len.
    // 3. Let hexDigits be the substring of string from position to position + 2.
    // 4. Let parseResult be ParseText(hexDigits, HexDigits[~Sep]).
    // 5. If parseResult is not a Parse Node, return parseResult.
    // 6. Let n be the MV of parseResult.
    // 7. Assert: n is in the inclusive interval from 0 to 255.
    // 8. Return n.

    let len = string.len();
    assert!(position + 2 <= len);
    let hex_digits = &string.as_slice()[position..position + 2];
    let left_char = char::from_u32(u32::from(hex_digits[0]))
        .ok_or_else(|| vec![create_syntax_error_object("Invalid Hex Digit", None)])?;
    let left = HexChar::try_from(left_char).map_err(|e| vec![create_syntax_error_object(e, None)])?;
    let right_char = char::from_u32(u32::from(hex_digits[1]))
        .ok_or_else(|| vec![create_syntax_error_object("Invalid Hex Digit", None)])?;
    let right = HexChar::try_from(right_char).map_err(|e| vec![create_syntax_error_object(e, None)])?;
    let left_mv = mv_of_hex_digit(left);
    let right_mv = mv_of_hex_digit(right);
    let combined = left_mv << 4 | right_mv;
    Ok(u8::try_from(combined).expect("two hex digits should never be greater than 255"))
}

fn decode(string: &JSString, preserve_escape_set: &JSString) -> Completion<JSString> {
    // Decode ( string, preserveEscapeSet )
    //
    // The abstract operation Decode takes arguments string (a String) and preserveEscapeSet (a String) and returns
    // either a normal completion containing a String or a throw completion. It performs URI unescaping and decoding,
    // preserving any escape sequences that correspond to Basic Latin characters in preserveEscapeSet. It performs the
    // following steps when called:
    //
    // 1. Let len be the length of string.
    // 2. Let R be the empty String.
    // 3. Let k be 0.
    // 4. Repeat, while k < len,
    //    a. Let C be the code unit at index k within string.
    //    b. Let S be C.
    //    c. If C is the code unit 0x0025 (PERCENT SIGN), then
    //       i. If k + 3 > len, throw a URIError exception.
    //       ii. Let escape be the substring of string from k to k + 3.
    //       iii. Let B be ParseHexOctet(string, k + 1).
    //       iv. If B is not an integer, throw a URIError exception.
    //       v. Set k to k + 2.
    //       vi. Let n be the number of leading 1 bits in B.
    //       vii. If n = 0, then
    //            1. Let asciiChar be the code unit whose numeric value is B.
    //            2. If preserveEscapeSet contains asciiChar, set S to escape; else set S to asciiChar.
    //       viii. Else,
    //             1. If n = 1 or n > 4, throw a URIError exception.
    //             2. Let Octets be « B ».
    //             3. Let j be 1.
    //             4. Repeat, while j < n,
    //                a. Set k to k + 1.
    //                b. If k + 3 > len, throw a URIError exception.
    //                c. If the code unit at index k within string is not the code unit 0x0025 (PERCENT SIGN), throw a
    //                   URIError exception.
    //                d. Let continuationByte be ParseHexOctet(string, k + 1).
    //                e. If continuationByte is not an integer, throw a URIError exception.
    //                f. Append continuationByte to Octets.
    //                g. Set k to k + 2.
    //                h. Set j to j + 1.
    //             5. Assert: The length of Octets is n.
    //             6. If Octets does not contain a valid UTF-8 encoding of a Unicode code point, throw a URIError
    //                exception.
    //             7. Let V be the code point obtained by applying the UTF-8 transformation to Octets, that is, from a
    //                List of octets into a 21-bit value.
    //             8. Set S to UTF16EncodeCodePoint(V).
    //    d. Set R to the string-concatenation of R and S.
    //    e. Set k to k + 1.
    // 5. Return R.
    //
    // Note: RFC 3629 prohibits the decoding of invalid UTF-8 octet sequences. For example, the invalid sequence 0xC0
    // 0x80 must not decode into the code unit 0x0000. Implementations of the Decode algorithm are required to throw a
    // URIError when encountering such invalid sequences.
    let len = string.len();
    let mut result: Vec<u16> = vec![];
    let mut read_index = 0;
    while read_index < len {
        let char_from_source = string[read_index];
        let decoded = if char_from_source == 0x0025 {
            if read_index + 3 > len {
                return Err(create_uri_error(format!(
                    "escape terminated by end of string: {}",
                    JSString::from(&string.as_slice()[read_index..])
                )));
            }
            let complete_escape_index_start = read_index;
            let escape = &string.as_slice()[read_index..read_index + 3];
            let b = parse_hex_octet(string, read_index + 1)
                .map_err(|_| create_uri_error(format!("Invalid escape sequence: {}", JSString::from(escape))))?;
            read_index += 2;
            let leading_ones = b.leading_ones();
            match leading_ones {
                0 => {
                    let ascii_char = u16::from(b);
                    if preserve_escape_set.as_slice().contains(&ascii_char) {
                        Vec::from(escape)
                    } else {
                        vec![ascii_char]
                    }
                }
                2..=4 => {
                    let mut octets = vec![b];
                    let mut j = 1;
                    while j < leading_ones {
                        read_index += 1;
                        if read_index + 3 > len {
                            return Err(create_uri_error(format!(
                                "escape terminated by end of string: {}",
                                JSString::from(&string.as_slice()[read_index..])
                            )));
                        }
                        if string[read_index] != 0x0025 {
                            return Err(create_uri_error(format!(
                                "malformed multi byte escape sequence: {}",
                                JSString::from(&string.as_slice()[complete_escape_index_start..read_index + 3])
                            )));
                        }
                        let continuation_byte = parse_hex_octet(string, read_index + 1).map_err(|_| {
                            create_uri_error(format!(
                                "Invalid escape sequence: {}",
                                JSString::from(&string.as_slice()[read_index..read_index + 3])
                            ))
                        })?;
                        octets.push(continuation_byte);
                        read_index += 2;
                        j += 1;
                    }
                    assert_eq!(leading_ones as usize, octets.len());
                    let octets = str::from_utf8(octets.as_slice())
                        .map_err(|e| create_uri_error(format!("Bad utf-8 endcoding: {e}")))?;
                    let mut char_iter = octets.chars();
                    let v = char_iter.next().expect("there should be at least one char");
                    assert!(char_iter.next().is_none(), "there should only have been one char");
                    let mut buffer: [u16; 2] = [0, 0];
                    let s_buf = utf16_encode_code_point(v as u32, &mut buffer)
                        .expect("good code points should convert without error");
                    Vec::from(s_buf)
                }
                _ => {
                    return Err(create_uri_error(format!("Invalid escape sequence: {}", JSString::from(escape))));
                }
            }
        } else {
            vec![char_from_source]
        };
        result.extend_from_slice(decoded.as_slice());
        read_index += 1;
    }
    Ok(JSString::from(result))
}

fn decode_uri(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // decodeURI ( encodedURI )
    //
    // This function computes a new version of a URI in which each escape sequence and UTF-8 encoding of the sort that
    // might be introduced by the encodeURI function is replaced with the UTF-16 encoding of the code point that it
    // represents. Escape sequences that could not have been introduced by encodeURI are not replaced.
    //
    // It is the %decodeURI% intrinsic object.
    //
    // It performs the following steps when called:
    //
    // 1. Let uriString be ? ToString(encodedURI).
    // 2. Let preserveEscapeSet be ";/?:@&=+$,#".
    // 3. Return ? Decode(uriString, preserveEscapeSet).
    let mut args = FuncArgs::from(arguments);
    let encoded_uri = args.next_arg();

    let uri_string = to_string(encoded_uri)?;
    let preserve_escape_set = JSString::from(";/?:@&=+$,#");
    decode(&uri_string, &preserve_escape_set).map(ECMAScriptValue::from)
}

fn decode_uri_component(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // decodeURIComponent ( encodedURIComponent )
    // This function computes a new version of a URI in which each escape sequence and UTF-8 encoding of the sort that
    // might be introduced by the encodeURIComponent function is replaced with the UTF-16 encoding of the code point
    // that it represents.
    //
    // It is the %decodeURIComponent% intrinsic object.
    //
    // It performs the following steps when called:
    //
    // 1. Let componentString be ? ToString(encodedURIComponent).
    // 2. Let preserveEscapeSet be the empty String.
    // 3. Return ? Decode(componentString, preserveEscapeSet).
    let mut args = FuncArgs::from(arguments);
    let encoded_uri_component = args.next_arg();

    let component_string = to_string(encoded_uri_component)?;
    decode(&component_string, &JSString::from("")).map(ECMAScriptValue::from)
}

fn encode(string: &JSString, extra_unescaped: &JSString) -> Completion<JSString> {
    // Encode ( string, extraUnescaped )
    //
    // The abstract operation Encode takes arguments string (a String) and extraUnescaped (a String) and returns either
    // a normal completion containing a String or a throw completion. It performs URI encoding and escaping,
    // interpreting string as a sequence of UTF-16 encoded code points as described in 6.1.4. If a character is
    // identified as unreserved in RFC 2396 or appears in extraUnescaped, it is not escaped. It performs the following
    // steps when called:
    //
    // 1. Let len be the length of string.
    // 2. Let R be the empty String.
    // 3. Let alwaysUnescaped be the string-concatenation of the ASCII word characters and "-.!~*'()".
    // 4. Let unescapedSet be the string-concatenation of alwaysUnescaped and extraUnescaped.
    // 5. Let k be 0.
    // 6. Repeat, while k < len,
    //    a. Let C be the code unit at index k within string.
    //    b. If unescapedSet contains C, then
    //       i. Set k to k + 1.
    //       ii. Set R to the string-concatenation of R and C.
    //    c. Else,
    //       i. Let cp be CodePointAt(string, k).
    //       ii. If cp.[[IsUnpairedSurrogate]] is true, throw a URIError exception.
    //       iii. Set k to k + cp.[[CodeUnitCount]].
    //       iv. Let Octets be the List of octets resulting by applying the UTF-8 transformation to cp.[[CodePoint]].
    //       v. For each element octet of Octets, do
    //          1. Let hex be the String representation of octet, formatted as an uppercase hexadecimal number.
    //          2. Set R to the string-concatenation of R, "%", and StringPad(hex, 2, "0", start).
    // 7. Return R.
    //
    // Note: Because percent-encoding is used to represent individual octets, a single code point may be expressed as
    // multiple consecutive escape sequences (one for each of its 8-bit UTF-8 code units).
    const ALWAYS_UNESCAPED: &str = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_-.!~*'()";
    let len = string.len();
    let mut result = JSString::from("");
    let unescaped_set = ALWAYS_UNESCAPED
        .chars()
        .map(|c| u16::try_from(c).expect("ascii range"))
        .chain(extra_unescaped.as_slice().iter().copied())
        .collect::<AHashSet<_>>();
    let mut k = 0;
    while k < len {
        let c = string[k];
        if unescaped_set.contains(&c) {
            k += 1;
            result = result.concat(JSString::from(&[c][..]));
        } else {
            let cp = code_point_at(string, k);
            if cp.is_unpaired_surrogate {
                return Err(create_uri_error("unpaired surrogate can't be encoded"));
            }
            k += cp.code_unit_count as usize;
            let mut buf = [0; 4];
            let octets = (char::try_from(cp.code_point).expect("came from a code point")).encode_utf8(&mut buf);
            for byte in octets.bytes() {
                result = result.concat(JSString::from(format!("%{byte:0X}")));
            }
        }
    }
    Ok(result)
}

fn encode_uri(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // encodeURI ( uri )
    //
    // This function computes a new version of a UTF-16 encoded (6.1.4) URI in which each instance of certain code
    // points is replaced by one, two, three, or four escape sequences representing the UTF-8 encoding of the code
    // point.
    //
    // It is the %encodeURI% intrinsic object.
    //
    // It performs the following steps when called:
    //
    // 1. Let uriString be ? ToString(uri).
    // 2. Let extraUnescaped be ";/?:@&=+$,#".
    // 3. Return ? Encode(uriString, extraUnescaped).
    let mut args = FuncArgs::from(arguments);
    let uri = args.next_arg();

    let uri_string = to_string(uri)?;
    encode(&uri_string, &JSString::from(";/?:@&=+$,#")).map(ECMAScriptValue::from)
}

fn encode_uri_component(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // encodeURIComponent ( uriComponent )
    //
    // This function computes a new version of a UTF-16 encoded (6.1.4) URI in which each instance of certain code
    // points is replaced by one, two, three, or four escape sequences representing the UTF-8 encoding of the code
    // point.
    //
    // It is the %encodeURIComponent% intrinsic object.
    //
    // It performs the following steps when called:
    //
    // 1. Let componentString be ? ToString(uriComponent).
    // 2. Let extraUnescaped be the empty String.
    // 3. Return ? Encode(componentString, extraUnescaped).
    let mut args = FuncArgs::from(arguments);
    let uri_component = args.next_arg();

    let component_string = to_string(uri_component)?;
    encode(&component_string, &JSString::from("")).map(ECMAScriptValue::from)
}

fn eval(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let mut args = FuncArgs::from(arguments);
    let x = args.next_arg();
    perform_eval(x, EvalCallStatus::NotDirect)
}

fn is_finite(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // This function is the %isFinite% intrinsic object.
    //
    // It performs the following steps when called:
    //
    //  1. Let num be ? ToNumber(number).
    //  2. If num is not finite, return false.
    //  3. Otherwise, return true.
    let mut args = FuncArgs::from(arguments);
    let number = args.next_arg();
    let num = number.to_number()?;
    Ok(ECMAScriptValue::from(num.is_finite()))
}

fn is_nan(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // This function is the %isNaN% intrinsic object.
    //
    // It performs the following steps when called:
    //
    //  1. Let num be ? ToNumber(number).
    //  2. If num is NaN, return true.
    //  3. Otherwise, return false.
    let mut args = FuncArgs::from(arguments);
    let number = args.next_arg();
    let num = number.to_number()?;
    Ok(ECMAScriptValue::from(num.is_nan()))
}

fn parse_float(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // parseFloat ( string ) This function produces a Number value dictated by interpretation of the contents of the
    // string argument as a decimal literal.
    //
    // It is the %parseFloat% intrinsic object.
    //
    // It performs the following steps when called:
    //
    // 1. Let inputString be ? ToString(string).
    // 2. Let trimmedString be ! TrimString(inputString, start).
    // 3. Let trimmed be StringToCodePoints(trimmedString).
    // 4. Let trimmedPrefix be the longest prefix of trimmed that satisfies the syntax of a StrDecimalLiteral, which
    //    might be trimmed itself. If there is no such prefix, return NaN.
    // 5. Let parsedNumber be ParseText(trimmedPrefix, StrDecimalLiteral).
    // 6. Assert: parsedNumber is a Parse Node.
    // 7. Return the StringNumericValue of parsedNumber.
    //
    // Note: This function may interpret only a leading portion of string as a Number value; it ignores any code units
    // that cannot be interpreted as part of the notation of a decimal literal, and no indication is given that any such
    // code units were ignored.
    static MATCHER: LazyLock<Regex> = LazyLock::new(|| {
        Regex::new(&format!("^{}", *STR_DECIMAL_LITERAL)).expect("pregenerated regex strings should be free of errors")
    });

    let mut args = FuncArgs::from(arguments);
    let string = args.next_arg();

    let trimmed_string = trim_string(string, TrimHint::Start)?;
    let trimmed_string = String::from(trimmed_string);
    if let Some(captures) = MATCHER.captures(&trimmed_string) {
        let s = captures.name("decimal").expect("group should be in pattern");
        Ok(ECMAScriptValue::Number(s.as_str().parse::<f64>().expect("regex should guarantee good parse")))
    } else {
        Ok(ECMAScriptValue::Number(f64::NAN))
    }
}

fn parse_int(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // parseInt ( string, radix )
    // This function produces an integral Number dictated by interpretation of the contents of string
    // according to the specified radix. Leading white space in string is ignored. If radix coerces to 0 (such
    // as when it is undefined), it is assumed to be 10 except when the number representation begins with "0x"
    // or "0X", in which case it is assumed to be 16. If radix is 16, the number representation may optionally
    // begin with "0x" or "0X".
    //
    // It is the %parseInt% intrinsic object.
    //
    // It performs the following steps when called:
    //
    // 1. Let inputString be ? ToString(string).
    // 2. Let S be ! TrimString(inputString, start).
    // 3. Let sign be 1.
    // 4. If S is not empty and the first code unit of S is the code unit 0x002D (HYPHEN-MINUS), set sign to
    //    -1.
    // 5. If S is not empty and the first code unit of S is either the code unit 0x002B (PLUS SIGN) or the
    //    code unit 0x002D (HYPHEN-MINUS), set S to the substring of S from index 1.
    // 6. Let R be ℝ(? ToInt32(radix)).
    // 7. Let stripPrefix be true.
    // 8. If R ≠ 0, then
    //    a. If R < 2 or R > 36, return NaN.
    //    b. If R ≠ 16, set stripPrefix to false.
    // 9. Else,
    //    a. Set R to 10.
    // 10. If stripPrefix is true, then
    //     a. If the length of S is at least 2 and the first two code units of S are either "0x" or "0X", then
    //        i. Set S to the substring of S from index 2.
    //        ii. Set R to 16.
    // 11. If S contains a code unit that is not a radix-R digit, let end be the index within S of the first
    //     such code unit; otherwise, let end be the length of S.
    // 12. Let Z be the substring of S from 0 to end.
    // 13. If Z is empty, return NaN.
    // 14. Let mathInt be the integer value that is represented by Z in radix-R notation, using the letters A
    //     through Z and a through z for digits with values 10 through 35. (However, if R = 10 and Z contains
    //     more than 20 significant digits, every significant digit after the 20th may be replaced by a 0
    //     digit, at the option of the implementation; and if R is not one of 2, 4, 8, 10, 16, or 32, then
    //     mathInt may be an implementation-approximated integer representing the integer value denoted by Z
    //     in radix-R notation.)
    // 15. If mathInt = 0, then
    //     a. If sign = -1, return -0𝔽.
    //     b. Return +0𝔽.
    // 16. Return 𝔽(sign × mathInt).
    //
    // Note
    // This function may interpret only a leading portion of string as an integer value; it ignores any code
    // units that cannot be interpreted as part of the notation of an integer, and no indication is given that
    // any such code units were ignored.
    let mut args = FuncArgs::from(arguments);
    let string = args.next_arg();
    let radix = args.next_arg();

    let input_string = to_string(string)?;
    let s_jss = trim_string(input_string.into(), TrimHint::Start).expect("argument is string already");
    let mut s = s_jss.as_slice();
    let sign = if !s.is_empty() && s[0] == 0x2d { -1 } else { 1 };
    if !s.is_empty() && [0x2b, 0x2d].contains(&s[0]) {
        s = &s[1..];
    }
    let mut r = radix.to_int32()?;
    let mut strip_prefix = true;
    if r != 0 {
        if !(2..=36).contains(&r) {
            return Ok(ECMAScriptValue::Number(f64::NAN));
        }
        if r != 16 {
            strip_prefix = false;
        }
    } else {
        r = 10;
    }
    if strip_prefix && s.len() >= 2 && s[0] == 0x30 && [0x58, 0x78].contains(&s[1]) {
        s = &s[2..];
        r = 16;
    }
    let mut end = s.len();
    for (idx, &val) in s.iter().enumerate() {
        if !is_radix_digit(val, r) {
            end = idx;
            break;
        }
    }
    let z = &s[0..end];
    if z.is_empty() {
        Ok(ECMAScriptValue::Number(f64::NAN))
    } else {
        let math_int = JSString::from(z).to_bigint_radix(u32::try_from(r).expect("r should be between 2 and 36"));
        if math_int == BigInt::ZERO {
            if sign == -1 { Ok(ECMAScriptValue::Number(-0.0)) } else { Ok(ECMAScriptValue::Number(0.0)) }
        } else {
            Ok(ECMAScriptValue::Number(f64::from(sign) * math_int.to_f64().unwrap()))
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) enum EvalCallStatus {
    DirectWithStrictCaller,
    DirectWithNonStrictCaller,
    NotDirect,
}

pub(crate) fn perform_eval(x: ECMAScriptValue, call_state: EvalCallStatus) -> Completion<ECMAScriptValue> {
    match x {
        ECMAScriptValue::String(x) => {
            let eval_realm = current_realm_record().unwrap();
            let (in_function, in_method, in_derived_constructor, in_class_field_initializer) =
                if call_state == EvalCallStatus::NotDirect {
                    (false, false, false, false)
                } else {
                    let this_env_rec = get_this_environment();
                    match this_env_rec.get_function_object() {
                        Some(f) => {
                            let fo = f.o.to_function_obj().unwrap().function_data().borrow();
                            (
                                true,
                                this_env_rec.has_super_binding(),
                                fo.constructor_kind == ConstructorKind::Derived,
                                fo.class_field_initializer_name.is_some(),
                            )
                        }
                        _ => (false, false, false, false),
                    }
                };
            let source_text = String::from(x);
            let script = parse_text(
                &source_text,
                ParseGoal::Script,
                call_state == EvalCallStatus::DirectWithStrictCaller,
                call_state != EvalCallStatus::NotDirect,
            );
            let source_tree = SourceTree { ast: script.clone(), text: source_text.clone() };
            match script {
                ParsedText::Errors(errs) => {
                    Err(create_syntax_error(errs.iter().map(unwind_any_error_object).join("; "), None))
                }
                ParsedText::Script(script) => match &script.body {
                    Some(body) => {
                        if !in_function && body.contains(ParseNodeKind::NewTarget) {
                            Err(create_syntax_error("new.target cannot be used outside of functions", None))
                        } else if !in_method && body.contains(ParseNodeKind::SuperProperty) {
                            Err(create_syntax_error("super properties cannot be used outside of methods", None))
                        } else if !in_derived_constructor && body.contains(ParseNodeKind::SuperCall) {
                            Err(create_syntax_error(
                                "calls to super() not allowed outside of derived constructors",
                                None,
                            ))
                        } else if in_class_field_initializer && body.contains_arguments() {
                            Err(create_syntax_error(
                                "class field initializers may not use the 'arguments' identifier",
                                None,
                            ))
                        } else {
                            let strict_eval =
                                call_state == EvalCallStatus::DirectWithStrictCaller || body.contains_use_strict();
                            let (lex_env, var_env, private_env) = match call_state {
                                EvalCallStatus::DirectWithStrictCaller | EvalCallStatus::DirectWithNonStrictCaller => {
                                    let lex_env: Rc<dyn EnvironmentRecord> = Rc::new(
                                        DeclarativeEnvironmentRecord::new(current_lexical_environment(), "eval"),
                                    );
                                    let var_env = if strict_eval {
                                        lex_env.clone()
                                    } else {
                                        current_variable_environment().unwrap()
                                    };
                                    let private_env = current_private_environment();
                                    (lex_env, var_env, private_env)
                                }
                                EvalCallStatus::NotDirect => {
                                    let global_env =
                                        eval_realm.borrow().global_env.clone().map(|g| g as Rc<dyn EnvironmentRecord>);
                                    let lex_env: Rc<dyn EnvironmentRecord> =
                                        Rc::new(DeclarativeEnvironmentRecord::new(global_env.clone(), "eval"));
                                    let var_env = if strict_eval { lex_env.clone() } else { global_env.unwrap() };
                                    (lex_env, var_env, None)
                                }
                            };
                            let mut eval_context = ExecutionContext::new(None, eval_realm, current_script_or_module());
                            eval_context.variable_environment = Some(var_env.clone());
                            eval_context.lexical_environment = Some(lex_env.clone());
                            eval_context.private_environment.clone_from(&private_env);
                            push_execution_context(eval_context);
                            let result = match eval_declaration_instantiation(
                                body,
                                &var_env,
                                &lex_env,
                                private_env.as_ref(),
                                strict_eval,
                                &source_tree,
                            ) {
                                Ok(()) => {
                                    let mut chunk = Chunk::new("eval code");
                                    script.compile(&mut chunk, strict_eval, &source_tree).unwrap();
                                    for line in chunk.disassemble() {
                                        println!("{line}");
                                    }
                                    evaluate(Rc::new(chunk), &source_tree)
                                }
                                Err(e) => Err(e),
                            };
                            pop_execution_context();
                            result
                        }
                    }
                    None => Ok(ECMAScriptValue::Undefined),
                },
                _ => unreachable!(),
            }
        }
        _ => Ok(x),
    }
}

fn eval_declaration_instantiation(
    body: &Rc<ScriptBody>,
    var_env: &Rc<dyn EnvironmentRecord>,
    lex_env: &Rc<dyn EnvironmentRecord>,
    private_env: Option<&Rc<RefCell<PrivateEnvironmentRecord>>>,
    strict: bool,
    source_tree: &SourceTree,
) -> Completion<()> {
    let var_names = body.var_declared_names();
    let var_declarations = body.var_scoped_declarations();
    if !strict {
        if let Some(ger) = var_env.as_global_environment_record() {
            for name in &var_names {
                if ger.has_lexical_declaration(name) {
                    return Err(create_syntax_error(
                        format!("Cannot create lexical binding {name} as it would shadow a global"),
                        None,
                    ));
                }
            }
        }
        let mut this_env = lex_env.clone();
        while !Rc::ptr_eq(&this_env, var_env) {
            if this_env.as_object_environment_record().is_none() {
                for name in &var_names {
                    if this_env.has_binding(name).unwrap() {
                        return Err(create_syntax_error(
                            format!("Cannot create binding {name} as it would shadow an existing var name"),
                            None,
                        ));
                    }
                }
            }
            this_env = this_env.get_outer_env().unwrap();
        }
    }
    let mut private_identifiers = vec![];
    let mut pointer = private_env.cloned();
    while let Some(ref per) = pointer {
        let outer = {
            let penv = per.borrow();
            for binding in &penv.names {
                if !private_identifiers.contains(&binding.description) {
                    private_identifiers.push(binding.description.clone());
                }
            }
            penv.outer_private_environment.clone()
        };
        pointer = outer;
    }
    if !body.all_private_identifiers_valid(&private_identifiers) {
        return Err(create_syntax_error("Invalid private identifiers seen", None));
    }
    let mut functions_to_initialize = vec![];
    let mut declared_function_names = vec![];
    // step 10.
    for d in var_declarations.iter().rev() {
        if let Ok(fd) = FcnDef::try_from(d.clone()) {
            let fname = fd.bound_name();
            if !declared_function_names.contains(&fname) {
                if let Some(ger) = var_env.as_global_environment_record()
                    && !ger.can_declare_global_function(&fname)?
                {
                    return Err(create_type_error(format!("Cannot create global function {fname}")));
                }
                declared_function_names.push(fname);
                functions_to_initialize.insert(0, d);
            }
        }
    }
    // Step 11
    let mut declared_var_names = vec![];
    // Step 12
    for d in &var_declarations {
        if matches!(d, VarScopeDecl::ForBinding(_) | VarScopeDecl::VariableDeclaration(_)) {
            for vn in d.bound_names() {
                if !declared_function_names.contains(&vn) {
                    if let Some(ger) = var_env.as_global_environment_record()
                        && !ger.can_declare_global_var(&vn)?
                    {
                        return Err(create_type_error(format!("Cannot create global var {vn}")));
                    }
                    if !declared_var_names.contains(&vn) {
                        declared_var_names.push(vn);
                    }
                }
            }
        }
    }
    // Step 15
    let lex_declarations = body.lexically_scoped_declarations();
    // Step 16
    for d in &lex_declarations {
        for dn in d.bound_names() {
            if d.is_constant_declaration() {
                lex_env.create_immutable_binding(dn, true)?;
            } else {
                lex_env.create_mutable_binding(dn, false)?;
            }
        }
    }
    // Step 17
    for vsd in functions_to_initialize {
        let f = FcnDef::try_from(vsd.clone()).unwrap();
        let fname = f.bound_name();
        let fo = f.instantiate_function_object(lex_env.clone(), private_env.cloned(), strict, source_tree).unwrap();
        if let Some(ger) = var_env.as_global_environment_record() {
            ger.create_global_function_binding(fname, fo, true)?;
        } else if !var_env.has_binding(&fname).unwrap() {
            var_env.create_mutable_binding(fname.clone(), true).unwrap();
            var_env.initialize_binding(&fname, fo).unwrap();
        } else {
            var_env.set_mutable_binding(fname, fo, false).unwrap();
        }
    }
    // Step 18
    for vn in declared_var_names {
        if let Some(ger) = var_env.as_global_environment_record() {
            ger.create_global_var_binding(vn, true)?;
        } else if !var_env.has_binding(&vn).unwrap() {
            var_env.create_mutable_binding(vn.clone(), true).unwrap();
            var_env.initialize_binding(&vn, ECMAScriptValue::Undefined).unwrap();
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests;
