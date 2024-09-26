// The Date Constructor

use super::*;

pub fn provision_date_intrinsic(realm: &Rc<RefCell<Realm>>) {
    let object_prototype = realm.borrow().intrinsics.object_prototype.clone();
    let function_prototype = realm.borrow().intrinsics.function_prototype.clone();

    // The Date constructor:
    //
    //  * is %Date%.
    //  * is the initial value of the "Date" property of the global object.
    //  * creates and initializes a new Date when called as a constructor.
    //  * returns a String representing the current time (UTC) when called as a function rather than as a
    //    constructor.
    //  * is a function whose behaviour differs based upon the number and types of its arguments.
    //  * may be used as the value of an extends clause of a class definition. Subclass constructors that
    //    intend to inherit the specified Date behaviour must include a super call to the Date constructor to
    //    create and initialize the subclass instance with a [[DateValue]] internal slot.

    // Properties of the Date Constructor
    // The Date constructor:
    //
    //  * has a [[Prototype]] internal slot whose value is %Function.prototype%.
    //  * has a "length" property whose value is 7𝔽.
    let date_constructor = create_builtin_function(
        Box::new(date_constructor_function),
        Some(ConstructorKind::Base),
        7.0,
        PropertyKey::from("Date"),
        BUILTIN_FUNCTION_SLOTS,
        Some(realm.clone()),
        Some(function_prototype.clone()),
        None,
    );

    // Constructor Function Properties
    macro_rules! constructor_function {
        ( $steps:expr, $name:expr, $length:expr ) => {
            let key = PropertyKey::from($name);
            let function_object = create_builtin_function(
                Box::new($steps),
                None,
                f64::from($length),
                key.clone(),
                BUILTIN_FUNCTION_SLOTS,
                Some(realm.clone()),
                Some(function_prototype.clone()),
                None,
            );
            define_property_or_throw(
                &date_constructor,
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
    constructor_function!(date_now, "now", 0); // 21.4.3.1 Date.now ( )
    constructor_function!(date_parse, "parse", 1); // 21.4.3.2 Date.parse ( string )
    constructor_function!(date_utc, "UTC", 7); // 21.4.3.4 Date.UTC ( year [ , month [ , date [ , hours [ , minutes [ , seconds [ , ms ] ] ] ] ] ] )

    // Properties of the Date Prototype Object
    // The Date prototype object:
    //
    //  * is %Date.prototype%.
    //  * is itself an ordinary object.
    //  * is not a Date instance and does not have a [[DateValue]] internal slot.
    //  * has a [[Prototype]] internal slot whose value is %Object.prototype%.
    //  * Unless explicitly defined otherwise, the methods of the Date prototype object defined below are not
    //    generic and the this value passed to them must be an object that has a [[DateValue]] internal slot
    //    that has been initialized to a time value.
    let date_prototype = ordinary_object_create(Some(object_prototype.clone()));
    define_property_or_throw(
        &date_constructor,
        "prototype",
        PotentialPropertyDescriptor::new()
            .value(date_prototype.clone())
            .writable(false)
            .enumerable(false)
            .configurable(false),
    )
    .unwrap();

    // Prototype function properties
    macro_rules! prototype_function {
        ( $steps:expr, $name:expr, $length:expr ) => {
            let key = PropertyKey::from($name);
            let function_object = create_builtin_function(
                Box::new($steps),
                None,
                f64::from($length),
                key.clone(),
                BUILTIN_FUNCTION_SLOTS,
                Some(realm.clone()),
                Some(function_prototype.clone()),
                None,
            );
            define_property_or_throw(
                &date_prototype,
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

    prototype_function!(date_prototype_getdate, "getDate", 0); // 21.4.4.2 Date.prototype.getDate ( )
    prototype_function!(date_prototype_getday, "getDay", 0); // 21.4.4.3 Date.prototype.getDay ( )
    prototype_function!(date_prototype_getfullyear, "getFullYear", 0); // 21.4.4.4 Date.prototype.getFullYear ( )
    prototype_function!(date_prototype_gethours, "getHours", 0); // 21.4.4.5 Date.prototype.getHours ( )
    prototype_function!(date_prototype_getmilliseconds, "getMillisecond0", 1); // 21.4.4.6 Date.prototype.getMilliseconds ( )
    prototype_function!(date_prototype_getminutes, "getMinutes", 0); // 21.4.4.7 Date.prototype.getMinutes ( )
    prototype_function!(date_prototype_getmonth, "getMonth", 0); // 21.4.4.8 Date.prototype.getMonth ( )
    prototype_function!(date_prototype_getseconds, "getSeconds", 0); // 21.4.4.9 Date.prototype.getSeconds ( )
    prototype_function!(date_prototype_gettime, "getTime", 0); // 21.4.4.10 Date.prototype.getTime ( )
    prototype_function!(date_prototype_gettimezoneoffset, "getTimezoneOffset", 0); // 21.4.4.11 Date.prototype.getTimezoneOffset ( )
    prototype_function!(date_prototype_getutcdate, "getUTCDate", 0); // 21.4.4.12 Date.prototype.getUTCDate ( )
    prototype_function!(date_prototype_getutcday, "getUTCDay", 0); // 21.4.4.13 Date.prototype.getUTCDay ( )
    prototype_function!(date_prototype_getutcfullyear, "getUTCFullYear", 0); // 21.4.4.14 Date.prototype.getUTCFullYear ( )
    prototype_function!(date_prototype_getutchours, "getUTCHours", 0); // 21.4.4.15 Date.prototype.getUTCHours ( )
    prototype_function!(date_prototype_getutcmilliseconds, "getUTCMilliseconds", 0); // 21.4.4.16 Date.prototype.getUTCMilliseconds ( )
    prototype_function!(date_prototype_getutcminutes, "getUTCMinutes", 0); // 21.4.4.17 Date.prototype.getUTCMinutes ( )
    prototype_function!(date_prototype_getutcmonth, "getUTCMonth", 0); // 21.4.4.18 Date.prototype.getUTCMonth ( )
    prototype_function!(date_prototype_getutcseconds, "getUTCSeconds", 0); // 21.4.4.19 Date.prototype.getUTCSeconds ( )
    prototype_function!(date_prototype_setdate, "setDate", 1); // 21.4.4.20 Date.prototype.setDate ( date )
    prototype_function!(date_prototype_setfullyear, "setFullYear", 1); // 21.4.4.21 Date.prototype.setFullYear ( year [ , month [ , date ] ] )
    prototype_function!(date_prototype_sethours, "setHours", 1); // 21.4.4.22 Date.prototype.setHours ( hour [ , min [ , sec [ , ms ] ] ] )
    prototype_function!(date_prototype_setmilliseconds, "setMilliseconds", 1); // 21.4.4.23 Date.prototype.setMilliseconds ( ms )
    prototype_function!(date_prototype_setminutes, "setMinutes", 1); // 21.4.4.24 Date.prototype.setMinutes ( min [ , sec [ , ms ] ] )
    prototype_function!(date_prototype_setmonth, "setMonth", 1); // 21.4.4.25 Date.prototype.setMonth ( month [ , date ] )
    prototype_function!(date_prototype_setseconds, "setSeconds", 1); // 21.4.4.26 Date.prototype.setSeconds ( sec [ , ms ] )
    prototype_function!(date_prototype_settime, "setTime", 1); // 21.4.4.27 Date.prototype.setTime ( time )
    prototype_function!(date_prototype_setutcdate, "setUTCDate", 1); // 21.4.4.28 Date.prototype.setUTCDate ( date )
    prototype_function!(date_prototype_setutcfullyear, "setUTCFullYear", 1); // 21.4.4.29 Date.prototype.setUTCFullYear ( year [ , month [ , date ] ] )
    prototype_function!(date_prototype_setutchours, "setUTCHours", 1); // 21.4.4.30 Date.prototype.setUTCHours ( hour [ , min [ , sec [ , ms ] ] ] )
    prototype_function!(date_prototype_setutcmilliseconds, "setUTCMilliseconds", 1); // 21.4.4.31 Date.prototype.setUTCMilliseconds ( ms )
    prototype_function!(date_prototype_setutcminutes, "setUTCMinutes", 1); // 21.4.4.32 Date.prototype.setUTCMinutes ( min [ , sec [ , ms ] ] )
    prototype_function!(date_prototype_setutcmonth, "setUTCMonth", 1); // 21.4.4.33 Date.prototype.setUTCMonth ( month [ , date ] )
    prototype_function!(date_prototype_setutcseconds, "setUTCSeconds", 1); // 21.4.4.34 Date.prototype.setUTCSeconds ( sec [ , ms ] )
    prototype_function!(date_prototype_todatestring, "toDateString", 0); // 21.4.4.35 Date.prototype.toDateString ( )
    prototype_function!(date_prototype_toisostring, "toISOString", 0); // 21.4.4.36 Date.prototype.toISOString ( )
    prototype_function!(date_prototype_tojson, "toJSON", 1); // 21.4.4.37 Date.prototype.toJSON ( key )
    prototype_function!(date_prototype_tolocaledatestring, "toLocaleDateString", 1); // 21.4.4.38 Date.prototype.toLocaleDateString ( [ reserved1 [ , reserved2 ] ] )
    prototype_function!(date_prototype_tolocalestring, "toLocaleString", 1); // 21.4.4.39 Date.prototype.toLocaleString ( [ reserved1 [ , reserved2 ] ] )
    prototype_function!(date_prototype_tolocaletimestring, "toLocaleTimeString", 1); // 21.4.4.40 Date.prototype.toLocaleTimeString ( [ reserved1 [ , reserved2 ] ] )
    prototype_function!(date_prototype_tostring, "toString", 0); // 21.4.4.41 Date.prototype.toString ( )
    prototype_function!(date_prototype_totimestring, "toTimeString", 0); // 21.4.4.42 Date.prototype.toTimeString ( )
    prototype_function!(date_prototype_toutcstring, "toUTCString", 0); // 21.4.4.43 Date.prototype.toUTCString ( )
    prototype_function!(date_prototype_valueof, "valueOf", 0); // 21.4.4.44 Date.prototype.valueOf ( )
                                                               // 21.4.4.45 Date.prototype [ %Symbol.toPrimitive% ] ( hint )
    let toprimitive_key = PropertyKey::from(wks(WksId::ToPrimitive));
    let date_prototype_toprimitive_function = create_builtin_function(
        Box::new(date_prototype_toprimitive),
        None,
        f64::from(1),
        toprimitive_key.clone(),
        BUILTIN_FUNCTION_SLOTS,
        Some(realm.clone()),
        Some(function_prototype.clone()),
        None,
    );
    define_property_or_throw(
        &date_prototype,
        toprimitive_key,
        PotentialPropertyDescriptor::new()
            .value(date_prototype_toprimitive_function)
            .writable(true)
            .enumerable(false)
            .configurable(true),
    )
    .unwrap();

    define_property_or_throw(
        &date_prototype,
        "constructor",
        PotentialPropertyDescriptor::new()
            .value(date_constructor.clone())
            .writable(true)
            .enumerable(false)
            .configurable(true),
    )
    .unwrap();

    realm.borrow_mut().intrinsics.date = date_constructor;
    realm.borrow_mut().intrinsics.date_prototype = date_prototype;
}

macro_rules! todo_function {
    ( $name:ident ) => {
        fn $name(
            _this_value: &ECMAScriptValue,
            _new_target: Option<&Object>,
            _arguments: &[ECMAScriptValue],
        ) -> Completion<ECMAScriptValue> {
            todo!()
        }
    };
}

todo_function!(date_constructor_function);
todo_function!(date_now);
todo_function!(date_parse);
todo_function!(date_utc);
todo_function!(date_prototype_getdate);
todo_function!(date_prototype_getday);
todo_function!(date_prototype_getfullyear);
todo_function!(date_prototype_gethours);
todo_function!(date_prototype_getmilliseconds);
todo_function!(date_prototype_getminutes);
todo_function!(date_prototype_getmonth);
todo_function!(date_prototype_getseconds);
todo_function!(date_prototype_gettime);
todo_function!(date_prototype_gettimezoneoffset);
todo_function!(date_prototype_getutcdate);
todo_function!(date_prototype_getutcday);
todo_function!(date_prototype_getutcfullyear);
todo_function!(date_prototype_getutchours);
todo_function!(date_prototype_getutcmilliseconds);
todo_function!(date_prototype_getutcminutes);
todo_function!(date_prototype_getutcmonth);
todo_function!(date_prototype_getutcseconds);
todo_function!(date_prototype_setdate);
todo_function!(date_prototype_setfullyear);
todo_function!(date_prototype_sethours);
todo_function!(date_prototype_setmilliseconds);
todo_function!(date_prototype_setminutes);
todo_function!(date_prototype_setmonth);
todo_function!(date_prototype_setseconds);
todo_function!(date_prototype_settime);
todo_function!(date_prototype_setutcdate);
todo_function!(date_prototype_setutcfullyear);
todo_function!(date_prototype_setutchours);
todo_function!(date_prototype_setutcmilliseconds);
todo_function!(date_prototype_setutcminutes);
todo_function!(date_prototype_setutcmonth);
todo_function!(date_prototype_setutcseconds);
todo_function!(date_prototype_todatestring);
todo_function!(date_prototype_toisostring);
todo_function!(date_prototype_tojson);
todo_function!(date_prototype_tolocaledatestring);
todo_function!(date_prototype_tolocalestring);
todo_function!(date_prototype_tolocaletimestring);
todo_function!(date_prototype_tostring);
todo_function!(date_prototype_totimestring);
todo_function!(date_prototype_toutcstring);
todo_function!(date_prototype_valueof);
todo_function!(date_prototype_toprimitive);
