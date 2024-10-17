// The Date Constructor
#![expect(unused_variables)]
#![expect(dead_code)]

use lazy_static::lazy_static;
use regex::Regex;
use std::num::TryFromIntError;

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

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
struct TimeNumber {
    // This is a floating point number that contains an integer between the values of -8,640,000,000,000,000 and
    // 8,640,000,000,000,000, inclusive.
    val: f64,
}

impl TryFrom<f64> for TimeNumber {
    type Error = InternalRuntimeError;
    fn try_from(value: f64) -> Result<Self, Self::Error> {
        if (-8_640_000_000_000_000.0..=8_640_000_000_000_000.0).contains(&value) && value.fract() == 0.0 {
            Ok(Self { val: value })
        } else {
            Err(InternalRuntimeError::TimeValueOutOfRange)
        }
    }
}

impl TryFrom<i64> for TimeNumber {
    type Error = InternalRuntimeError;

    #[expect(clippy::cast_precision_loss)]
    fn try_from(value: i64) -> Result<Self, Self::Error> {
        // A time value supports a slightly smaller range of -8,640,000,000,000,000 to 8,640,000,000,000,000
        // milliseconds.
        if (-8_640_000_000_000_000..=8_640_000_000_000_000).contains(&value) {
            Ok(Self { val: value as f64 })
        } else {
            Err(InternalRuntimeError::TimeValueOutOfRange)
        }
    }
}
impl TryFrom<isize> for TimeNumber {
    type Error = InternalRuntimeError;

    #[expect(clippy::cast_precision_loss)]
    fn try_from(value: isize) -> Result<Self, Self::Error> {
        // A time value supports a slightly smaller range of -8,640,000,000,000,000 to 8,640,000,000,000,000
        // milliseconds.
        if (-8_640_000_000_000_000..=8_640_000_000_000_000).contains(&value) {
            Ok(Self { val: value as f64 })
        } else {
            Err(InternalRuntimeError::TimeValueOutOfRange)
        }
    }
}
impl TryFrom<i128> for TimeNumber {
    type Error = InternalRuntimeError;

    #[expect(clippy::cast_precision_loss)]
    fn try_from(value: i128) -> Result<Self, Self::Error> {
        // A time value supports a slightly smaller range of -8,640,000,000,000,000 to 8,640,000,000,000,000
        // milliseconds.
        if (-8_640_000_000_000_000..=8_640_000_000_000_000).contains(&value) {
            Ok(Self { val: value as f64 })
        } else {
            Err(InternalRuntimeError::TimeValueOutOfRange)
        }
    }
}
impl From<u8> for TimeNumber {
    fn from(value: u8) -> Self {
        Self { val: f64::from(value) }
    }
}
impl From<TimeNumber> for i128 {
    #[expect(clippy::cast_possible_truncation)]
    fn from(value: TimeNumber) -> Self {
        // an f64 has 52 bits of mantissa, and a date value is an integer, so this conversion is ok.
        value.val as i128
    }
}
impl From<TimeNumber> for isize {
    #[expect(clippy::cast_possible_truncation)]
    fn from(value: TimeNumber) -> Self {
        // an f64 has 52 bits of mantissa, and a date value is an integer, so this conversion is ok.
        value.val as isize
    }
}
impl From<TimeNumber> for f64 {
    fn from(value: TimeNumber) -> Self {
        value.val
    }
}
impl TryFrom<TimeNumber> for u16 {
    type Error = TryFromIntError;
    fn try_from(value: TimeNumber) -> Result<Self, Self::Error> {
        let val = i128::from(value);
        u16::try_from(val)
    }
}
impl TryFrom<TimeNumber> for u8 {
    type Error = TryFromIntError;
    fn try_from(value: TimeNumber) -> Result<Self, Self::Error> {
        let val = i128::from(value);
        u8::try_from(val)
    }
}

#[derive(Debug)]
pub struct DateObject {
    common: RefCell<CommonObjectData>,
    date_value: f64,
}

impl<'a> From<&'a DateObject> for &'a dyn ObjectInterface {
    fn from(obj: &'a DateObject) -> Self {
        obj
    }
}

impl ObjectInterface for DateObject {
    fn common_object_data(&self) -> &RefCell<CommonObjectData> {
        &self.common
    }
    fn uses_ordinary_get_prototype_of(&self) -> bool {
        true
    }
    fn kind(&self) -> ObjectTag {
        ObjectTag::Date
    }
    fn to_date_obj(&self) -> Option<&DateObject> {
        Some(self)
    }

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

impl DateObject {
    pub fn date_value(&self) -> f64 {
        self.date_value
    }

    pub fn now_utc() -> anyhow::Result<f64> {
        let timestamp = time::OffsetDateTime::now_utc();
        let nanos = timestamp.unix_timestamp_nanos();
        let milliseconds = nanos / 1_000_000;
        Ok(TimeNumber::try_from(milliseconds)?.val)
    }

    fn new(prototype: Option<Object>, value: TimeNumber) -> Self {
        Self { common: RefCell::new(CommonObjectData::new(prototype, true, DATE_OBJECT_SLOTS)), date_value: value.val }
    }

    fn object(prototype: Option<Object>, value: TimeNumber) -> Object {
        Object { o: Rc::new(Self::new(prototype, value)) }
    }
}

// Time-related Constants
// These constants are referenced by algorithms in the following sections.
//
// HoursPerDay = 24
const HOURS_PER_DAY: usize = 24;
const HOURS_PER_DAY_I128: i128 = HOURS_PER_DAY as i128;
const HOURS_PER_DAY_F64: f64 = 24.0;
// MinutesPerHour = 60
const MINUTES_PER_HOUR: usize = 60;
const MINUTES_PER_HOUR_I128: i128 = MINUTES_PER_HOUR as i128;
const MINUTES_PER_HOUR_F64: f64 = 60.0;
// SecondsPerMinute = 60
const SECONDS_PER_MINUTE: usize = 60;
const SECONDS_PER_MINUTE_I128: i128 = SECONDS_PER_MINUTE as i128;
const SECONDS_PER_MINUTE_F64: f64 = 60.0;
// msPerSecond = 1000𝔽
const MS_PER_SECOND: usize = 1000;
const MS_PER_SECOND_F64: f64 = 1000.0;
// msPerMinute = 60000𝔽 = msPerSecond × 𝔽(SecondsPerMinute)
const MS_PER_MINUTE: usize = MS_PER_SECOND * SECONDS_PER_MINUTE;
const MS_PER_MINUTE_F64: f64 = MS_PER_SECOND_F64 * SECONDS_PER_MINUTE_F64;
// msPerHour = 3600000𝔽 = msPerMinute × 𝔽(MinutesPerHour)
const MS_PER_HOUR: usize = MS_PER_MINUTE * MINUTES_PER_HOUR;
const MS_PER_HOUR_F64: f64 = MS_PER_MINUTE_F64 * MINUTES_PER_HOUR_F64;
// msPerDay = 86400000𝔽 = msPerHour × 𝔽(HoursPerDay)
const MS_PER_DAY: usize = MS_PER_HOUR * HOURS_PER_DAY;
const MS_PER_DAY_F64: f64 = MS_PER_HOUR_F64 * HOURS_PER_DAY_F64;
const MS_PER_DAY_ISIZE: isize = 86_400_000;

fn day(t: f64) -> isize {
    // Day ( t )
    // The abstract operation Day takes argument t (a finite time value) and returns an integral Number. It returns the
    // day number of the day in which t falls. It performs the following steps when called:
    //
    //  1. Return 𝔽(floor(ℝ(t / msPerDay))).
    isize::from(TimeNumber::try_from((t / MS_PER_DAY_F64).floor()).unwrap())
}

fn days_in_year(y: isize) -> u16 {
    // DaysInYear ( y )
    // The abstract operation DaysInYear takes argument y (an integral Number) and returns 365𝔽 or 366𝔽. It returns
    // the number of days in year y. Leap years have 366 days; all other years have 365. It performs the following steps
    // when called:
    //
    // 1. Let ry be ℝ(y).
    // 2. If (ry modulo 400) = 0, return 366𝔽.
    // 3. If (ry modulo 100) = 0, return 365𝔽.
    // 4. If (ry modulo 4) = 0, return 366𝔽.
    // 5. Return 365𝔽.
    if y % 400 == 0 {
        366
    } else if y % 100 == 0 {
        365
    } else if y % 4 == 0 {
        366
    } else {
        365
    }
}

fn day_from_year(y: isize) -> isize {
    // DayFromYear ( y )
    // The abstract operation DayFromYear takes argument y (an integral Number) and returns an integral Number. It
    // returns the day number of the first day of year y. It performs the following steps when called:
    //
    // 1. Let ry be ℝ(y).
    // 2. NOTE: In the following steps, numYears1, numYears4, numYears100, and numYears400 represent the number of years
    //    divisible by 1, 4, 100, and 400, respectively, that occur between the epoch and the start of year y. The
    //    number is negative if y is before the epoch.
    // 3. Let numYears1 be (ry - 1970).
    // 4. Let numYears4 be floor((ry - 1969) / 4).
    // 5. Let numYears100 be floor((ry - 1901) / 100).
    // 6. Let numYears400 be floor((ry - 1601) / 400).
    // 7. Return 𝔽(365 × numYears1 + numYears4 - numYears100 + numYears400).
    let num_years_1 = y - 1970;
    let num_years_4 = (y - 1969).div_euclid(4);
    let num_years_100 = (y - 1901).div_euclid(100);
    let num_years_400 = (y - 1601).div_euclid(400);
    365 * num_years_1 + num_years_4 - num_years_100 + num_years_400
}

fn time_from_year(y: isize) -> f64 {
    // TimeFromYear ( y )
    // The abstract operation TimeFromYear takes argument y (an integral Number) and returns a time value. It returns
    // the time value of the start of year y. It performs the following steps when called:
    //
    // 1. Return msPerDay × DayFromYear(y).
    TimeNumber::try_from(MS_PER_DAY_ISIZE * day_from_year(y)).unwrap().val
}

fn year_from_time(t: f64) -> isize {
    // YearFromTime ( t )
    // The abstract operation YearFromTime takes argument t (a finite time value) and returns an integral Number. It
    // returns the year in which t falls. It performs the following steps when called:
    //
    // 1. Return the largest integral Number y (closest to +∞) such that TimeFromYear(y) ≤ t.
    let mut guess = day(t) / 365 + 1970;
    loop {
        let guess_time = time_from_year(guess);
        let next_time = time_from_year(guess + 1);
        if guess_time <= t && t < next_time {
            return guess;
        } else if t >= next_time {
            guess += 1;
        } else {
            guess -= 1;
        }
    }
}

fn day_within_year(t: f64) -> u16 {
    // DayWithinYear ( t )
    // The abstract operation DayWithinYear takes argument t (a finite time value) and returns an integral Number in the
    // inclusive interval from +0𝔽 to 365𝔽. It performs the following steps when called:
    //
    // 1. Return Day(t) - DayFromYear(YearFromTime(t)).
    let result = day(t) - day_from_year(year_from_time(t));
    assert!(0 <= result);
    assert!(result <= 365);
    u16::try_from(result).unwrap()
}

fn in_leap_year(t: f64) -> u8 {
    // InLeapYear ( t )
    // The abstract operation InLeapYear takes argument t (a finite time value) and returns +0𝔽 or 1𝔽. It returns 1𝔽
    // if t is within a leap year and +0𝔽 otherwise. It performs the following steps when called:
    //
    // 1. If DaysInYear(YearFromTime(t)) is 366𝔽, return 1𝔽; else return +0𝔽.
    u8::from(days_in_year(year_from_time(t)) == 366)
}

fn month_from_time(t: f64) -> u8 {
    // MonthFromTime ( t )
    // The abstract operation MonthFromTime takes argument t (a finite time value) and returns an integral Number in the
    // inclusive interval from +0𝔽 to 11𝔽. It returns a Number identifying the month in which t falls. A month value
    // of +0𝔽 specifies January; 1𝔽 specifies February; 2𝔽 specifies March; 3𝔽 specifies April; 4𝔽 specifies May;
    // 5𝔽 specifies June; 6𝔽 specifies July; 7𝔽 specifies August; 8𝔽 specifies September; 9𝔽 specifies October;
    // 10𝔽 specifies November; and 11𝔽 specifies December. Note that MonthFromTime(+0𝔽) = +0𝔽, corresponding to
    // Thursday, 1 January 1970. It performs the following steps when called:
    //
    // 1. Let inLeapYear be InLeapYear(t).
    // 2. Let dayWithinYear be DayWithinYear(t).
    // 3. If dayWithinYear < 31𝔽, return +0𝔽.
    // 4. If dayWithinYear < 59𝔽 + inLeapYear, return 1𝔽.
    // 5. If dayWithinYear < 90𝔽 + inLeapYear, return 2𝔽.
    // 6. If dayWithinYear < 120𝔽 + inLeapYear, return 3𝔽.
    // 7. If dayWithinYear < 151𝔽 + inLeapYear, return 4𝔽.
    // 8. If dayWithinYear < 181𝔽 + inLeapYear, return 5𝔽.
    // 9. If dayWithinYear < 212𝔽 + inLeapYear, return 6𝔽.
    // 10. If dayWithinYear < 243𝔽 + inLeapYear, return 7𝔽.
    // 11. If dayWithinYear < 273𝔽 + inLeapYear, return 8𝔽.
    // 12. If dayWithinYear < 304𝔽 + inLeapYear, return 9𝔽.
    // 13. If dayWithinYear < 334𝔽 + inLeapYear, return 10𝔽.
    // 14. Assert: dayWithinYear < 365𝔽 + inLeapYear.
    // 15. Return 11𝔽.
    let in_leap_year = u16::from(in_leap_year(t));
    let day_within_year = day_within_year(t);
    for (idx, (limit, ly)) in [
        (31_u16, false),
        (59_u16, true),
        (90_u16, true),
        (120_u16, true),
        (151_u16, true),
        (181_u16, true),
        (212_u16, true),
        (243_u16, true),
        (273_u16, true),
        (304_u16, true),
        (334_u16, true),
    ]
    .into_iter()
    .enumerate()
    {
        if day_within_year < limit + if ly { in_leap_year } else { 0 } {
            return u8::try_from(idx).unwrap();
        }
    }
    11
}

fn date_from_time(t: f64) -> u8 {
    // DateFromTime ( t )
    // The abstract operation DateFromTime takes argument t (a finite time value) and returns an integral Number in the
    // inclusive interval from 1𝔽 to 31𝔽. It returns the day of the month in which t falls. It performs the following
    // steps when called:
    //
    // 1. Let inLeapYear be InLeapYear(t).
    // 2. Let dayWithinYear be DayWithinYear(t).
    // 3. Let month be MonthFromTime(t).
    // 4. If month is +0𝔽, return dayWithinYear + 1𝔽.
    // 5. If month is 1𝔽, return dayWithinYear - 30𝔽.
    // 6. If month is 2𝔽, return dayWithinYear - 58𝔽 - inLeapYear.
    // 7. If month is 3𝔽, return dayWithinYear - 89𝔽 - inLeapYear.
    // 8. If month is 4𝔽, return dayWithinYear - 119𝔽 - inLeapYear.
    // 9. If month is 5𝔽, return dayWithinYear - 150𝔽 - inLeapYear.
    // 10. If month is 6𝔽, return dayWithinYear - 180𝔽 - inLeapYear.
    // 11. If month is 7𝔽, return dayWithinYear - 211𝔽 - inLeapYear.
    // 12. If month is 8𝔽, return dayWithinYear - 242𝔽 - inLeapYear.
    // 13. If month is 9𝔽, return dayWithinYear - 272𝔽 - inLeapYear.
    // 14. If month is 10𝔽, return dayWithinYear - 303𝔽 - inLeapYear.
    // 15. Assert: month is 11𝔽.
    // 16. Return dayWithinYear - 333𝔽 - inLeapYear.
    const INFO: [(i32, bool); 12] = [
        (1, false),
        (-30, false),
        (-58, true),
        (-89, true),
        (-119, true),
        (-150, true),
        (-180, true),
        (-211, true),
        (-242, true),
        (-272, true),
        (-303, true),
        (-333, true),
    ];
    let in_leap_year = i32::from(in_leap_year(t));
    let day_within_year = i32::from(day_within_year(t));
    let month = usize::from(month_from_time(t));
    let (delta, consider_leaps) = INFO[month];
    u8::try_from(day_within_year + delta - if consider_leaps { in_leap_year } else { 0 }).unwrap()
}

fn week_day(t: f64) -> u8 {
    // WeekDay ( t )
    // The abstract operation WeekDay takes argument t (a finite time value) and returns an integral Number in the
    // inclusive interval from +0𝔽 to 6𝔽. It returns a Number identifying the day of the week in which t falls. A
    // weekday value of +0𝔽 specifies Sunday; 1𝔽 specifies Monday; 2𝔽 specifies Tuesday; 3𝔽 specifies Wednesday; 4𝔽
    // specifies Thursday; 5𝔽 specifies Friday; and 6𝔽 specifies Saturday. Note that WeekDay(+0𝔽) = 4𝔽,
    // corresponding to Thursday, 1 January 1970. It performs the following steps when called:
    //
    // 1. Return 𝔽(ℝ(Day(t) + 4𝔽) modulo 7).
    u8::try_from((day(t) + 4) % 7).unwrap()
}

fn hour_from_time(t: f64) -> u8 {
    // HourFromTime ( t )
    // The abstract operation HourFromTime takes argument t (a finite time value) and returns an integral Number in the
    // inclusive interval from +0𝔽 to 23𝔽. It returns the hour of the day in which t falls. It performs the following
    // steps when called:
    //
    // 1. Return 𝔽(floor(ℝ(t / msPerHour)) modulo HoursPerDay).
    u8::try_from(TimeNumber::try_from((t / MS_PER_HOUR_F64).floor().rem_euclid(HOURS_PER_DAY_F64)).unwrap()).unwrap()
}

fn min_from_time(t: f64) -> u8 {
    // MinFromTime ( t )
    // The abstract operation MinFromTime takes argument t (a finite time value) and returns an integral Number in the
    // inclusive interval from +0𝔽 to 59𝔽. It returns the minute of the hour in which t falls. It performs the
    // following steps when called:
    //
    // 1. Return 𝔽(floor(ℝ(t / msPerMinute)) modulo MinutesPerHour).
    u8::try_from(TimeNumber::try_from((t / MS_PER_MINUTE_F64).floor().rem_euclid(MINUTES_PER_HOUR_F64)).unwrap())
        .unwrap()
}

fn sec_from_time(t: f64) -> u8 {
    // SecFromTime ( t )
    // The abstract operation SecFromTime takes argument t (a finite time value) and returns an integral Number in the
    // inclusive interval from +0𝔽 to 59𝔽. It returns the second of the minute in which t falls. It performs the
    // following steps when called:
    //
    // 1. Return 𝔽(floor(ℝ(t / msPerSecond)) modulo SecondsPerMinute).
    u8::try_from(TimeNumber::try_from((t / MS_PER_SECOND_F64).floor().rem_euclid(SECONDS_PER_MINUTE_F64)).unwrap())
        .unwrap()
}

fn ms_from_time(t: f64) -> u16 {
    // msFromTime ( t )
    // The abstract operation msFromTime takes argument t (a finite time value) and returns an integral Number in the
    // inclusive interval from +0𝔽 to 999𝔽. It returns the millisecond of the second in which t falls. It performs the
    // following steps when called:
    //
    //  1. Return 𝔽(ℝ(t) modulo ℝ(msPerSecond)).
    u16::try_from(TimeNumber::try_from(t.rem_euclid(MS_PER_SECOND_F64)).unwrap()).expect("number should fit")
}

#[expect(clippy::too_many_arguments)]
fn get_utc_epoch_nanoseconds(
    year: isize,
    month: u8,
    day: u8,
    hour: u8,
    minute: u8,
    second: u8,
    millisecond: u16,
    microsecond: u16,
    nanosecond: u16,
) -> anyhow::Result<i128> {
    // GetUTCEpochNanoseconds ( year, month, day, hour, minute, second, millisecond, microsecond, nanosecond )
    // The abstract operation GetUTCEpochNanoseconds takes arguments year (an integer), month (an integer in the
    // inclusive interval from 1 to 12), day (an integer in the inclusive interval from 1 to 31), hour (an integer in
    // the inclusive interval from 0 to 23), minute (an integer in the inclusive interval from 0 to 59), second (an
    // integer in the inclusive interval from 0 to 59), millisecond (an integer in the inclusive interval from 0 to
    // 999), microsecond (an integer in the inclusive interval from 0 to 999), and nanosecond (an integer in the
    // inclusive interval from 0 to 999) and returns a BigInt. The returned value represents a number of nanoseconds
    // since the epoch that corresponds to the given ISO 8601 calendar date and wall-clock time in UTC. It performs the
    // following steps when called:
    //
    //  1. Let date be MakeDay(𝔽(year), 𝔽(month - 1), 𝔽(day)).
    //  2. Let time be MakeTime(𝔽(hour), 𝔽(minute), 𝔽(second), 𝔽(millisecond)).
    //  3. Let ms be MakeDate(date, time).
    //  4. Assert: ms is an integral Number.
    //  5. Return ℤ(ℝ(ms) × 10**6 + microsecond × 10**3 + nanosecond).
    let date = make_day(TimeNumber::try_from(year).unwrap().val, f64::from(month) - 1.0, f64::from(day))?;
    let time = make_time(f64::from(hour), f64::from(minute), f64::from(second), f64::from(millisecond));
    let ms = make_date(date, time);
    let millis = i128::from(TimeNumber::try_from(ms).unwrap());
    let nanos = millis * 1_000_000 + i128::from(microsecond) * 1_000 + i128::from(nanosecond);
    Ok(nanos)
}

#[expect(clippy::too_many_arguments)]
fn get_named_time_zone_epoch_nanoseconds(
    time_zone_identifier: &str,
    year: isize,
    month: u8,
    day: u8,
    hour: u8,
    minute: u8,
    second: u8,
    millisecond: u16,
    microsecond: u16,
    nanosecond: u16,
) -> anyhow::Result<Vec<i128>> {
    // GetNamedTimeZoneEpochNanoseconds ( timeZoneIdentifier, year, month, day, hour, minute, second, millisecond, microsecond, nanosecond )
    // The implementation-defined abstract operation GetNamedTimeZoneEpochNanoseconds takes arguments timeZoneIdentifier
    // (a String), year (an integer), month (an integer in the inclusive interval from 1 to 12), day (an integer in the
    // inclusive interval from 1 to 31), hour (an integer in the inclusive interval from 0 to 23), minute (an integer in
    // the inclusive interval from 0 to 59), second (an integer in the inclusive interval from 0 to 59), millisecond (an
    // integer in the inclusive interval from 0 to 999), microsecond (an integer in the inclusive interval from 0 to
    // 999), and nanosecond (an integer in the inclusive interval from 0 to 999) and returns a List of BigInts. Each
    // value in the returned List represents a number of nanoseconds since the epoch that corresponds to the given ISO
    // 8601 calendar date and wall-clock time in the named time zone identified by timeZoneIdentifier.
    //
    // When the input represents a local time occurring more than once because of a negative time zone transition (e.g.
    // when daylight saving time ends or the time zone offset is decreased due to a time zone rule change), the returned
    // List will have more than one element and will be sorted by ascending numerical value. When the input represents a
    // local time skipped because of a positive time zone transition (e.g. when daylight saving time begins or the time
    // zone offset is increased due to a time zone rule change), the returned List will be empty. Otherwise, the
    // returned List will have one element.
    //
    // The default implementation of GetNamedTimeZoneEpochNanoseconds, to be used for ECMAScript implementations that do
    // not include local political rules for any time zones, performs the following steps when called:
    //
    //  1. Assert: timeZoneIdentifier is "UTC".
    //  2. Let epochNanoseconds be GetUTCEpochNanoseconds(year, month, day, hour, minute, second, millisecond,
    //     microsecond, nanosecond).
    //  3. Return « epochNanoseconds ».
    //
    // Note
    //      It is required for time zone aware implementations (and recommended for all others) to use the time zone
    //      information of the IANA Time Zone Database https://www.iana.org/time-zones/.
    //
    //      1:30 AM on 5 November 2017 in America/New_York is repeated twice, so
    //      GetNamedTimeZoneEpochNanoseconds("America/New_York", 2017, 11, 5, 1, 30, 0, 0, 0, 0) would return a List of
    //      length 2 in which the first element represents 05:30 UTC (corresponding with 01:30 US Eastern Daylight Time
    //      at UTC offset -04:00) and the second element represents 06:30 UTC (corresponding with 01:30 US Eastern
    //      Standard Time at UTC offset -05:00).
    //
    //      2:30 AM on 12 March 2017 in America/New_York does not exist, so
    //      GetNamedTimeZoneEpochNanoseconds("America/New_York", 2017, 3, 12, 2, 30, 0, 0, 0, 0) would return an empty
    //      List.
    Ok(vec![get_utc_epoch_nanoseconds(year, month, day, hour, minute, second, millisecond, microsecond, nanosecond)?])
}

fn get_named_time_zone_offset_nanoseconds(
    time_zone_identifier: &str,
    _epoch_nanoseconds: i128,
) -> anyhow::Result<i128> {
    // GetNamedTimeZoneOffsetNanoseconds ( timeZoneIdentifier, epochNanoseconds )
    // The implementation-defined abstract operation GetNamedTimeZoneOffsetNanoseconds takes arguments
    // timeZoneIdentifier (a String) and epochNanoseconds (a BigInt) and returns an integer.
    //
    // The returned integer represents the offset from UTC of the named time zone identified by timeZoneIdentifier, at
    // the instant corresponding with epochNanoseconds relative to the epoch, both in nanoseconds.
    //
    // The default implementation of GetNamedTimeZoneOffsetNanoseconds, to be used for ECMAScript implementations that
    // do not include local political rules for any time zones, performs the following steps when called:
    //
    //  1. Assert: timeZoneIdentifier is "UTC".
    //  2. Return 0.
    //
    // Note: Time zone offset values may be positive or negative.
    if time_zone_identifier == "UTC" {
        Ok(0_i128)
    } else {
        Err(InternalRuntimeError::UnknownTimeZoneIdentifier)?
    }
}

fn system_time_zone_identifier() -> String {
    // SystemTimeZoneIdentifier ( )
    // The implementation-defined abstract operation SystemTimeZoneIdentifier takes no arguments and returns a String.
    // It returns a String representing the host environment's current time zone, which is either a String representing
    // a UTC offset for which IsTimeZoneOffsetString returns true, or a primary time zone identifier. It performs the
    // following steps when called:
    //
    // 1. If the implementation only supports the UTC time zone, return "UTC".
    // 2. Let systemTimeZoneString be the String representing the host environment's current time zone, either a primary
    //    time zone identifier or an offset time zone identifier.
    // 3. Return systemTimeZoneString.
    //
    // Note
    // To ensure the level of functionality that implementations commonly provide in the methods of the Date object, it
    // is recommended that SystemTimeZoneIdentifier return an IANA time zone name corresponding to the host
    // environment's time zone setting, if such a thing exists. GetNamedTimeZoneEpochNanoseconds and
    // GetNamedTimeZoneOffsetNanoseconds must reflect the local political rules for standard time and daylight saving
    // time in that time zone, if such rules exist.
    //
    // For example, if the host environment is a browser on a system where the user has chosen US Eastern Time as their
    // time zone, SystemTimeZoneIdentifier returns "America/New_York".
    String::from("UTC")
}

fn local_time(t: f64) -> anyhow::Result<f64> {
    // LocalTime ( t )
    // The abstract operation LocalTime takes argument t (a finite time value) and returns an integral Number. It
    // converts t from UTC to local time. The local political rules for standard time and daylight saving time in effect
    // at t should be used to determine the result in the way specified in this section. It performs the following steps
    // when called:
    //
    // 1. Let systemTimeZoneIdentifier be SystemTimeZoneIdentifier().
    // 2. If IsTimeZoneOffsetString(systemTimeZoneIdentifier) is true, then
    //    a. Let offsetNs be ParseTimeZoneOffsetString(systemTimeZoneIdentifier).
    // 3. Else,
    //    a. Let offsetNs be GetNamedTimeZoneOffsetNanoseconds(systemTimeZoneIdentifier, ℤ(ℝ(t) × 10**6)).
    // 4. Let offsetMs be truncate(offsetNs / 10**6).
    // 5. Return t + 𝔽(offsetMs).
    //
    // Note 1
    // If political rules for the local time t are not available within the implementation, the result is t because
    // SystemTimeZoneIdentifier returns "UTC" and GetNamedTimeZoneOffsetNanoseconds returns 0.
    //
    // Note 2
    // It is required for time zone aware implementations (and recommended for all others) to use the time zone
    // information of the IANA Time Zone Database https://www.iana.org/time-zones/.
    //
    // Note 3
    // Two different input time values tUTC are converted to the same local time tlocal at a negative time zone
    // transition when there are repeated times (e.g. the daylight saving time ends or the time zone adjustment is
    // decreased.).
    //
    // LocalTime(UTC(tlocal)) is not necessarily always equal to tlocal. Correspondingly, UTC(LocalTime(tUTC)) is not
    // necessarily always equal to tUTC.
    let system_time_zone_identifier = system_time_zone_identifier();
    let offset_ns = if let Some(ns) = parse_time_zone_offset_string(&system_time_zone_identifier) {
        ns
    } else {
        get_named_time_zone_offset_nanoseconds(
            &system_time_zone_identifier,
            i128::from(TimeNumber::try_from(t).unwrap()) * 1_000_000,
        )?
    };
    let offset_ms = TimeNumber::try_from(offset_ns / 1_000_000)?.val;
    Ok(t + offset_ms)
}

fn utc(t: f64) -> anyhow::Result<f64> {
    // UTC ( t )
    // The abstract operation UTC takes argument t (a Number) and returns a time value. It converts t from local time to
    // a UTC time value. The local political rules for standard time and daylight saving time in effect at t should be
    // used to determine the result in the way specified in this section. It performs the following steps when called:
    //
    //  1. If t is not finite, return NaN.
    //  2. Let systemTimeZoneIdentifier be SystemTimeZoneIdentifier().
    //  3. If IsTimeZoneOffsetString(systemTimeZoneIdentifier) is true, then
    //     a. Let offsetNs be ParseTimeZoneOffsetString(systemTimeZoneIdentifier).
    //  4. Else,
    //     a. Let possibleInstants be GetNamedTimeZoneEpochNanoseconds(systemTimeZoneIdentifier, ℝ(YearFromTime(t)),
    //        ℝ(MonthFromTime(t)) + 1, ℝ(DateFromTime(t)), ℝ(HourFromTime(t)), ℝ(MinFromTime(t)), ℝ(SecFromTime(t)),
    //        ℝ(msFromTime(t)), 0, 0).
    //     b. NOTE: The following steps ensure that when t represents local time repeating multiple times at a negative
    //        time zone transition (e.g. when the daylight saving time ends or the time zone offset is decreased due to
    //        a time zone rule change) or skipped local time at a positive time zone transition (e.g. when the daylight
    //        saving time starts or the time zone offset is increased due to a time zone rule change), t is interpreted
    //        using the time zone offset before the transition.
    //     c. If possibleInstants is not empty, then
    //        i. Let disambiguatedInstant be possibleInstants[0].
    //     d. Else,
    //        i. NOTE: t represents a local time skipped at a positive time zone transition (e.g. due to daylight saving
    //           time starting or a time zone rule change increasing the UTC offset).
    //        ii. Let possibleInstantsBefore be GetNamedTimeZoneEpochNanoseconds(systemTimeZoneIdentifier,
    //            ℝ(YearFromTime(tBefore)), ℝ(MonthFromTime(tBefore)) + 1, ℝ(DateFromTime(tBefore)),
    //            ℝ(HourFromTime(tBefore)), ℝ(MinFromTime(tBefore)), ℝ(SecFromTime(tBefore)), ℝ(msFromTime(tBefore)), 0,
    //            0), where tBefore is the largest integral Number < t for which possibleInstantsBefore is not empty
    //            (i.e., tBefore represents the last local time before the transition).
    //        iii. Let disambiguatedInstant be the last element of possibleInstantsBefore.
    //     e. Let offsetNs be GetNamedTimeZoneOffsetNanoseconds(systemTimeZoneIdentifier, disambiguatedInstant).
    //  5. Let offsetMs be truncate(offsetNs / 10**6).
    //  6. Return t - 𝔽(offsetMs).
    //
    // Input t is nominally a time value but may be any Number value. The algorithm must not limit t to the time value
    // range, so that inputs corresponding with a boundary of the time value range can be supported regardless of local
    // UTC offset. For example, the maximum time value is 8.64 × 10**15, corresponding with "+275760-09-13T00:00:00Z".
    // In an environment where the local time zone offset is ahead of UTC by 1 hour at that instant, it is represented
    // by the larger input of 8.64 × 10**15 + 3.6 × 10**6, corresponding with "+275760-09-13T01:00:00+01:00".
    //
    // If political rules for the local time t are not available within the implementation, the result is t because
    // SystemTimeZoneIdentifier returns "UTC" and GetNamedTimeZoneOffsetNanoseconds returns 0.
    //
    // Note 1
    // It is required for time zone aware implementations (and recommended for all others) to use the time zone
    // information of the IANA Time Zone Database https://www.iana.org/time-zones/.
    //
    // 1:30 AM on 5 November 2017 in America/New_York is repeated twice (fall backward), but it must be interpreted as
    // 1:30 AM UTC-04 instead of 1:30 AM UTC-05. In UTC(TimeClip(MakeDate(MakeDay(2017, 10, 5), MakeTime(1, 30, 0,
    // 0)))), the value of offsetMs is -4 × msPerHour.
    //
    // 2:30 AM on 12 March 2017 in America/New_York does not exist, but it must be interpreted as 2:30 AM UTC-05
    // (equivalent to 3:30 AM UTC-04). In UTC(TimeClip(MakeDate(MakeDay(2017, 2, 12), MakeTime(2, 30, 0, 0)))), the
    // value of offsetMs is -5 × msPerHour.
    //
    // Note 2
    // UTC(LocalTime(tUTC)) is not necessarily always equal to tUTC. Correspondingly, LocalTime(UTC(tlocal)) is not
    // necessarily always equal to tlocal.
    if t.is_finite() {
        let system_time_zone_identifier = system_time_zone_identifier();
        let offset_ns = match parse_time_zone_offset_string(&system_time_zone_identifier) {
            Some(offset_ns) => offset_ns,
            None => {
                let possible_instants = get_named_time_zone_epoch_nanoseconds(
                    &system_time_zone_identifier,
                    year_from_time(t),
                    month_from_time(t) + 1,
                    date_from_time(t),
                    hour_from_time(t),
                    min_from_time(t),
                    sec_from_time(t),
                    ms_from_time(t),
                    0,
                    0,
                )?;
                let disambiguated_instant =
                    if let Some(instant) = possible_instants.first() { *instant } else { todo!() };
                get_named_time_zone_offset_nanoseconds(&system_time_zone_identifier, disambiguated_instant)?
            }
        };
        let offset_ms = TimeNumber::try_from(offset_ns / 1_000_000)?.val;
        Ok(t - offset_ms)
    } else {
        Ok(f64::NAN)
    }
}

fn make_time(hour: f64, min: f64, sec: f64, ms: f64) -> f64 {
    // MakeTime ( hour, min, sec, ms )
    // The abstract operation MakeTime takes arguments hour (a Number), min (a Number), sec (a Number), and ms (a
    // Number) and returns a Number. It calculates a number of milliseconds. It performs the following steps when
    // called:
    //
    //  1. If hour is not finite, min is not finite, sec is not finite, or ms is not finite, return NaN.
    //  2. Let h be 𝔽(! ToIntegerOrInfinity(hour)).
    //  3. Let m be 𝔽(! ToIntegerOrInfinity(min)).
    //  4. Let s be 𝔽(! ToIntegerOrInfinity(sec)).
    //  5. Let milli be 𝔽(! ToIntegerOrInfinity(ms)).
    //  6. Return ((h × msPerHour + m × msPerMinute) + s × msPerSecond) + milli.
    //
    // Note
    // The arithmetic in MakeTime is floating-point arithmetic, which is not associative, so the operations must be
    // performed in the correct order.
    if !hour.is_finite() || !min.is_finite() || !sec.is_finite() || !ms.is_finite() {
        f64::NAN
    } else {
        let h = to_integer_or_infinity(hour);
        let m = to_integer_or_infinity(min);
        let s = to_integer_or_infinity(sec);
        let milli = to_integer_or_infinity(ms);
        h * MS_PER_HOUR_F64 + m * MS_PER_MINUTE_F64 + s * MS_PER_SECOND_F64 + milli
    }
}

fn make_day(year: f64, month: f64, date: f64) -> anyhow::Result<f64> {
    // MakeDay ( year, month, date )
    // The abstract operation MakeDay takes arguments year (a Number), month (a Number), and date (a Number) and returns
    // a Number. It calculates a number of days. It performs the following steps when called:
    //
    //  1. If year is not finite, month is not finite, or date is not finite, return NaN.
    //  2. Let y be 𝔽(! ToIntegerOrInfinity(year)).
    //  3. Let m be 𝔽(! ToIntegerOrInfinity(month)).
    //  4. Let dt be 𝔽(! ToIntegerOrInfinity(date)).
    //  5. Let ym be y + 𝔽(floor(ℝ(m) / 12)).
    //  6. If ym is not finite, return NaN.
    //  7. Let mn be 𝔽(ℝ(m) modulo 12).
    //  8. Find a finite time value t such that YearFromTime(t) is ym, MonthFromTime(t) is mn, and DateFromTime(t) is
    //     1𝔽; but if this is not possible (because some argument is out of range), return NaN.
    //  9. Return Day(t) + dt - 1𝔽.
    fn guess(year: f64, month: f64) -> f64 {
        const APPROXIMATE_YEAR_MS: f64 = 365.0 * MS_PER_DAY_F64;
        const APPROXIMATE_MONTH_MS: f64 = 30.0 * MS_PER_DAY_F64;
        APPROXIMATE_YEAR_MS * (year - 1970.0) + month * APPROXIMATE_MONTH_MS
    }
    fn compare(guess: f64, year: f64, month: f64) -> std::cmp::Ordering {
        let result_y = f64::from(TimeNumber::try_from(year_from_time(guess)).unwrap());
        let result_m = f64::from(TimeNumber::from(month_from_time(guess)));
        let result_d = date_from_time(guess);

        if result_y > year {
            std::cmp::Ordering::Greater
        } else if result_y < year {
            std::cmp::Ordering::Less
        } else if result_m > month {
            std::cmp::Ordering::Greater
        } else if result_m < month {
            std::cmp::Ordering::Less
        } else if result_d > 1 {
            std::cmp::Ordering::Greater
        } else {
            std::cmp::Ordering::Equal
        }
    }
    if !year.is_finite() || !month.is_finite() || !date.is_finite() {
        Ok(f64::NAN)
    } else {
        let y = to_integer_or_infinity(year);
        let m = to_integer_or_infinity(month);
        let dt = to_integer_or_infinity(date);
        let ym = y + (m / 12.0).floor();
        if ym.is_finite() {
            let mn = m.rem_euclid(12.0);
            let g = guess(ym, mn);
            let (mut upper, mut lower) = match compare(g, ym, mn) {
                std::cmp::Ordering::Less => {
                    let lower = g;
                    let mut yp = ym + 1.0;
                    loop {
                        let new_guess = guess(yp, mn);
                        match compare(new_guess, ym, mn) {
                            std::cmp::Ordering::Less => {
                                yp += 1.0;
                            }
                            std::cmp::Ordering::Greater => {
                                break (new_guess, lower);
                            }
                            std::cmp::Ordering::Equal => {
                                return Ok(f64::from(TimeNumber::try_from(day(new_guess))?) + dt - 1.0);
                            }
                        }
                    }
                }
                std::cmp::Ordering::Equal => {
                    return Ok(f64::from(TimeNumber::try_from(day(g))?) + dt - 1.0);
                }
                std::cmp::Ordering::Greater => {
                    let upper = g;
                    let mut yp = ym - 1.0;
                    loop {
                        let new_guess = guess(yp, mn);
                        match compare(new_guess, ym, mn) {
                            std::cmp::Ordering::Greater => {
                                yp -= 1.0;
                            }
                            std::cmp::Ordering::Less => {
                                break (upper, new_guess);
                            }
                            std::cmp::Ordering::Equal => {
                                return Ok(f64::from(TimeNumber::try_from(day(new_guess))?) + dt - 1.0);
                            }
                        }
                    }
                }
            };

            loop {
                let probe = (upper + lower) / 2.0;
                // pay attention when probe == -62167064625000
                match compare(probe, ym, mn) {
                    std::cmp::Ordering::Less => {
                        lower = probe;
                    }
                    std::cmp::Ordering::Equal => {
                        return Ok(f64::from(TimeNumber::try_from(day(probe))?) + dt - 1.0);
                    }
                    std::cmp::Ordering::Greater => {
                        upper = probe;
                    }
                }
                if upper - lower < MS_PER_DAY_F64 {
                    return Ok(f64::NAN);
                }
            }
        } else {
            Ok(f64::NAN)
        }
    }
}

fn make_date(day: f64, time: f64) -> f64 {
    // MakeDate ( day, time )
    // The abstract operation MakeDate takes arguments day (a Number) and time (a Number) and returns a Number. It
    // calculates a number of milliseconds. It performs the following steps when called:
    //
    //  1. If day is not finite or time is not finite, return NaN.
    //  2. Let tv be day × msPerDay + time.
    //  3. If tv is not finite, return NaN.
    //  4. Return tv.
    if day.is_finite() && time.is_finite() {
        let tv = day * MS_PER_DAY_F64 + time;
        if tv.is_finite() {
            tv
        } else {
            f64::NAN
        }
    } else {
        f64::NAN
    }
}

fn make_full_year(year: f64) -> f64 {
    // MakeFullYear ( year )
    // The abstract operation MakeFullYear takes argument year (a Number) and returns an integral Number or NaN. It
    // returns the full year associated with the integer part of year, interpreting any value in the inclusive interval
    // from 0 to 99 as a count of years since the start of 1900. For alignment with the proleptic Gregorian calendar,
    // "full year" is defined as the signed count of complete years since the start of year 0 (1 B.C.). It performs the
    // following steps when called:
    //
    //  1. If year is NaN, return NaN.
    //  2. Let truncated be ! ToIntegerOrInfinity(year).
    //  3. If truncated is in the inclusive interval from 0 to 99, return 1900𝔽 + 𝔽(truncated).
    //  4. Return 𝔽(truncated).
    if year.is_nan() {
        f64::NAN
    } else {
        let truncated = to_integer_or_infinity(year);
        if (0.0..=99.0).contains(&year) {
            1900.0 + truncated
        } else {
            truncated
        }
    }
}

fn time_clip(time: f64) -> f64 {
    // TimeClip ( time )
    // The abstract operation TimeClip takes argument time (a Number) and returns a Number. It calculates a number of
    // milliseconds. It performs the following steps when called:
    //
    // 1. If time is not finite, return NaN.
    // 2. If abs(ℝ(time)) > 8.64 × 10**15, return NaN.
    // 3. Return 𝔽(! ToIntegerOrInfinity(time)).
    if time.abs() <= 8.64e15 {
        to_integer_or_infinity(time)
    } else {
        f64::NAN
    }
}

fn parse_time_zone_offset_string(tzo: &str) -> Option<i128> {
    // IsTimeZoneOffsetString ( offsetString )
    // The abstract operation IsTimeZoneOffsetString takes argument offsetString (a String) and returns a Boolean. The
    // return value indicates whether offsetString conforms to the grammar given by UTCOffset. It performs the following
    // steps when called:
    //
    // 1. Let parseResult be ParseText(offsetString, UTCOffset).
    // 2. If parseResult is a List of errors, return false.
    // 3. Return true.

    // ParseTimeZoneOffsetString ( offsetString )
    // The abstract operation ParseTimeZoneOffsetString takes argument offsetString (a String) and returns an integer.
    // The return value is the UTC offset, as a number of nanoseconds, that corresponds to the String offsetString. It
    // performs the following steps when called:
    //
    // 1. Let parseResult be ParseText(offsetString, UTCOffset).
    // 2. Assert: parseResult is not a List of errors.
    // 3. Assert: parseResult contains a ASCIISign Parse Node.
    // 4. Let parsedSign be the source text matched by the ASCIISign Parse Node contained within parseResult.
    // 5. If parsedSign is the single code point U+002D (HYPHEN-MINUS), then
    //    a. Let sign be -1.
    // 6. Else,
    //    a. Let sign be 1.
    // 7. NOTE: Applications of StringToNumber below do not lose precision, since each of the parsed values is
    //    guaranteed to be a sufficiently short string of decimal digits.
    // 8. Assert: parseResult contains an Hour Parse Node.
    // 9. Let parsedHours be the source text matched by the Hour Parse Node contained within parseResult.
    // 10. Let hours be ℝ(StringToNumber(CodePointsToString(parsedHours))).
    // 11. If parseResult does not contain a MinuteSecond Parse Node, then
    //     a. Let minutes be 0.
    // 12. Else,
    //     a. Let parsedMinutes be the source text matched by the first MinuteSecond Parse Node contained within
    //        parseResult.
    //     b. Let minutes be ℝ(StringToNumber(CodePointsToString(parsedMinutes))).
    // 13. If parseResult does not contain two MinuteSecond Parse Nodes, then
    //     a. Let seconds be 0.
    // 14. Else,
    //     a. Let parsedSeconds be the source text matched by the second MinuteSecond Parse Node contained within
    //        parseResult.
    //     b. Let seconds be ℝ(StringToNumber(CodePointsToString(parsedSeconds))).
    // 15. If parseResult does not contain a TemporalDecimalFraction Parse Node, then
    //     a. Let nanoseconds be 0.
    // 16. Else,
    //     a. Let parsedFraction be the source text matched by the TemporalDecimalFraction Parse Node contained within
    //        parseResult.
    //     b. Let fraction be the string-concatenation of CodePointsToString(parsedFraction) and "000000000".
    //     c. Let nanosecondsString be the substring of fraction from 1 to 10.
    //     d. Let nanoseconds be ℝ(StringToNumber(nanosecondsString)).
    // 17. Return sign × (((hours × 60 + minutes) × 60 + seconds) × 10**9 + nanoseconds).

    const TEMPORAL_DECIMAL_SEPARATOR: &str = "(?:[.,])";
    const MINUTE_SECOND: &str = "(?:[0-5][0-9])";
    const ASCII_SIGN: &str = r"(?P<ascii_sign>\+|\-)";
    const HOUR: &str = "(?P<hour>(?:[0-1][0-9])|(?:2[0-3]))";
    const TIME_SEPARATOR: &str = "";
    const TIME_SEPARATOR_EXTENDED: &str = ":";

    lazy_static! {
        static ref TEMPORAL_DECIMAL_FRACTION1: String = format!("(?P<temporal_decimal_fraction1>{TEMPORAL_DECIMAL_SEPARATOR}[0-9]{{1,9}})");
        static ref TEMPORAL_DECIMAL_FRACTION2: String = format!("(?P<temporal_decimal_fraction2>{TEMPORAL_DECIMAL_SEPARATOR}[0-9]{{1,9}})");
        static ref HOUR_SUBCOMPONENTS: String =
            format!("(?:{TIME_SEPARATOR}(?P<minute1>{MINUTE_SECOND})(?:{TIME_SEPARATOR}(?P<second1>{MINUTE_SECOND}){}?)?)", *TEMPORAL_DECIMAL_FRACTION1);
        static ref HOUR_SUBCOMPONENTS_EXTENDED: String =
            format!("(?:{TIME_SEPARATOR_EXTENDED}(?P<minute2>{MINUTE_SECOND})(?:{TIME_SEPARATOR_EXTENDED}(?P<second2>{MINUTE_SECOND}){}?)?)",
            *TEMPORAL_DECIMAL_FRACTION2);

        static ref UTC_OFFSET: String = format!("(?:{ASCII_SIGN}{HOUR}(?:{}|{}))", *HOUR_SUBCOMPONENTS, *HOUR_SUBCOMPONENTS_EXTENDED);

        static ref MATCHER: Regex = Regex::new(&UTC_OFFSET).expect("regular expressions not based on user input should not fail");
    }
    MATCHER.captures(tzo).map(|captures| {
        let sign = if captures.name("ascii_sign").expect("sign should have been captured").as_str() == "+" {
            1_i128
        } else {
            -1_i128
        };
        let hours = captures
            .name("hour")
            .expect("hour should have been captured")
            .as_str()
            .parse::<i128>()
            .expect("hours should look like a nubmer");
        let minutes = captures
            .name("minute1")
            .or_else(|| captures.name("minute2"))
            .map_or(0, |txt| txt.as_str().parse::<i128>().expect("minutes should have been a number"));
        let seconds = captures
            .name("second1")
            .or_else(|| captures.name("second2"))
            .map_or(0, |txt| txt.as_str().parse::<i128>().expect("seconds should be a number"));
        let nanoseconds = captures
            .name("temporal_decimal_fraction1")
            .or_else(|| captures.name("temporal_decimal_fraction2"))
            .map_or(0, |matched| {
                let to_parse = format!("{}000000000", matched.as_str());
                to_parse.as_str()[1..10].parse::<i128>().expect("should be numbers")
            });
        sign * (((hours * 60 + minutes) * 60 + seconds) * 1_000_000_000 + nanoseconds)
    })
}

fn date_constructor_function(
    _this_value: &ECMAScriptValue,
    new_target: Option<&Object>,
    values: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Date ( ...values )
    // This function performs the following steps when called:
    //
    // 1. If NewTarget is undefined, then
    //    a. Let now be the time value (UTC) identifying the current time.
    //    b. Return ToDateString(now).
    // 2. Let numberOfArgs be the number of elements in values.
    // 3. If numberOfArgs = 0, then
    //    a. Let dv be the time value (UTC) identifying the current time.
    // 4. Else if numberOfArgs = 1, then
    //    a. Let value be values[0].
    //    b. If value is an Object and value has a [[DateValue]] internal slot, then
    //       i. Let tv be value.[[DateValue]].
    //    c. Else,
    //       i. Let v be ? ToPrimitive(value).
    //       ii. If v is a String, then
    //           1. Assert: The next step never returns an abrupt completion because v is a String.
    //           2. Let tv be the result of parsing v as a date, in exactly the same manner as for the parse method
    //              (21.4.3.2).
    //       iii. Else,
    //            1. Let tv be ? ToNumber(v).
    //    d. Let dv be TimeClip(tv).
    // 5. Else,
    //    a. Assert: numberOfArgs ≥ 2.
    //    b. Let y be ? ToNumber(values[0]).
    //    c. Let m be ? ToNumber(values[1]).
    //    d. If numberOfArgs > 2, let dt be ? ToNumber(values[2]); else let dt be 1𝔽.
    //    e. If numberOfArgs > 3, let h be ? ToNumber(values[3]); else let h be +0𝔽.
    //    f. If numberOfArgs > 4, let min be ? ToNumber(values[4]); else let min be +0𝔽.
    //    g. If numberOfArgs > 5, let s be ? ToNumber(values[5]); else let s be +0𝔽.
    //    h. If numberOfArgs > 6, let milli be ? ToNumber(values[6]); else let milli be +0𝔽.
    //    i. Let yr be MakeFullYear(y).
    //    j. Let finalDate be MakeDate(MakeDay(yr, m, dt), MakeTime(h, min, s, milli)).
    //    k. Let dv be TimeClip(UTC(finalDate)).
    // 6. Let O be ? OrdinaryCreateFromConstructor(NewTarget, "%Date.prototype%", « [[DateValue]] »).
    // 7. Set O.[[DateValue]] to dv.
    // 8. Return O.
    const ERROR: &str = "time values should be in range";
    match new_target {
        None => {
            let now = DateObject::now_utc().expect(ERROR);
            Ok(to_date_string(now).map(ECMAScriptValue::String).expect(ERROR))
        }
        Some(nt) => {
            let number_of_args = values.len();
            let dv = match number_of_args {
                0 => DateObject::now_utc().expect(ERROR),
                1 => {
                    let value = values[0].clone();
                    match value.to_date_object() {
                        Some(date_obj) => date_obj.date_value(),
                        None => {
                            let v = to_primitive(value, None)?;
                            match v {
                                ECMAScriptValue::String(s) => parse_date(&s),
                                _ => v.to_number()?,
                            }
                        }
                    }
                }
                _ => {
                    let y = values[0].to_number()?;
                    let m = values[1].to_number()?;
                    let dt = if number_of_args > 2 { values[2].to_number()? } else { 1.0 };
                    let h = if number_of_args > 3 { values[3].to_number()? } else { 0.0 };
                    let min = if number_of_args > 4 { values[4].to_number()? } else { 0.0 };
                    let s = if number_of_args > 5 { values[5].to_number()? } else { 0.0 };
                    let milli = if number_of_args > 6 { values[6].to_number()? } else { 0.0 };
                    let yr = make_full_year(y);
                    let final_date = make_date(make_day(yr, m, dt).expect(ERROR), make_time(h, min, s, milli));
                    time_clip(utc(final_date).expect(ERROR))
                }
            };
            let dv = TimeNumber::try_from(dv).expect(ERROR);
            Ok(ECMAScriptValue::Object(
                nt.ordinary_create_from_constructor(IntrinsicId::DatePrototype, |proto| DateObject::object(proto, dv))?,
            ))
        }
    }
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

todo_function!(date_now);

fn date_parse(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Date.parse ( string )
    // This function applies the ToString operator to its argument. If ToString results in an abrupt completion the
    // Completion Record is immediately returned. Otherwise, this function interprets the resulting String as a date and
    // time; it returns a Number, the UTC time value corresponding to the date and time. The String may be interpreted
    // as a local time, a UTC time, or a time in some other time zone, depending on the contents of the String. The
    // function first attempts to parse the String according to the format described in Date Time String Format
    // (21.4.1.32), including expanded years. If the String does not conform to that format the function may fall back
    // to any implementation-specific heuristics or implementation-specific date formats. Strings that are
    // unrecognizable or contain out-of-bounds format element values shall cause this function to return NaN.
    //
    // If the String conforms to the Date Time String Format, substitute values take the place of absent format
    // elements. When the MM or DD elements are absent, "01" is used. When the HH, mm, or ss elements are absent, "00"
    // is used. When the sss element is absent, "000" is used. When the UTC offset representation is absent, date-only
    // forms are interpreted as a UTC time and date-time forms are interpreted as a local time.
    //
    // If x is any Date whose milliseconds amount is zero within a particular implementation of ECMAScript, then all of
    // the following expressions should produce the same numeric value in that implementation, if all the properties
    // referenced have their initial values:
    //
    //     x.valueOf()
    //     Date.parse(x.toString())
    //     Date.parse(x.toUTCString())
    //     Date.parse(x.toISOString())
    //
    // However, the expression
    //
    //     Date.parse(x.toLocaleString())
    //
    // is not required to produce the same Number value as the preceding three expressions and, in general, the value
    // produced by this function is implementation-defined when given any String value that does not conform to the Date
    // Time String Format (21.4.1.32) and that could not be produced in that implementation by the toString or
    // toUTCString method.
    let mut args = FuncArgs::from(arguments);
    let date_str = to_string(args.next_arg())?;
    Ok(ECMAScriptValue::Number(parse_date(&date_str)))
}

// Date Time String Format
// ECMAScript defines a string interchange format for date-times based upon a simplification of the ISO 8601 calendar
// date extended format. The format is as follows: YYYY-MM-DDTHH:mm:ss.sssZ
//
// Where the elements are as follows:
//
// YYYY is the year in the proleptic Gregorian calendar as four decimal digits from 0000 to 9999, or as an expanded year
//      of "+" or "-" followed by six decimal digits.
// -	"-" (hyphen) appears literally twice in the string.
// MM	is the month of the year as two decimal digits from 01 (January) to 12 (December).
// DD	is the day of the month as two decimal digits from 01 to 31.
// T	"T" appears literally in the string, to indicate the beginning of the time element.
// HH	is the number of complete hours that have passed since midnight as two decimal digits from 00 to 24.
// :	":" (colon) appears literally twice in the string.
// mm	is the number of complete minutes since the start of the hour as two decimal digits from 00 to 59.
// ss	is the number of complete seconds since the start of the minute as two decimal digits from 00 to 59.
// .	"." (dot) appears literally in the string.
// sss	is the number of complete milliseconds since the start of the second as three decimal digits.
// Z	is the UTC offset representation specified as "Z" (for UTC with no offset) or as either "+" or "-" followed by a
//      time expression HH:mm (a subset of the time zone offset string format for indicating local time ahead of or
//      behind UTC, respectively)
//
// This format includes date-only forms:
//
// YYYY
// YYYY-MM
// YYYY-MM-DD
//
// It also includes “date-time” forms that consist of one of the above date-only forms immediately followed by one of
// the following time forms with an optional UTC offset representation appended:
//
// THH:mm
// THH:mm:ss
// THH:mm:ss.sss
//
// A string containing out-of-bounds or nonconforming elements is not a valid instance of this format.
//
// Note 1
// As every day both starts and ends with midnight, the two notations 00:00 and 24:00 are available to distinguish the
// two midnights that can be associated with one date. This means that the following two notations refer to exactly the
// same point in time: 1995-02-04T24:00 and 1995-02-05T00:00. This interpretation of the latter form as "end of a
// calendar day" is consistent with ISO 8601, even though that specification reserves it for describing time intervals
// and does not permit it within representations of single points in time.
//
// Note 2
// There exists no international standard that specifies abbreviations for civil time zones like CET, EST, etc. and
// sometimes the same abbreviation is even used for two very different time zones. For this reason, both ISO 8601 and
// this format specify numeric representations of time zone offsets.

// Expanded Years
// Covering the full time value range of approximately 273,790 years forward or backward from 1 January 1970 (21.4.1.1)
// requires representing years before 0 or after 9999. ISO 8601 permits expansion of the year representation, but only
// by mutual agreement of the partners in information interchange. In the simplified ECMAScript format, such an expanded
// year representation shall have 6 digits and is always prefixed with a + or - sign. The year 0 is considered positive
// and must be prefixed with a + sign. The representation of the year 0 as -000000 is invalid. Strings matching the Date
// Time String Format with expanded years representing instants in time outside the range of a time value are treated as
// unrecognizable by Date.parse and cause that function to return NaN without falling back to implementation-specific
// behaviour or heuristics.
//
// Note
// Examples of date-time values with expanded years:
//
// -271821-04-20T00:00:00Z	271822 B.C.
// -000001-01-01T00:00:00Z	2 B.C.
// +000000-01-01T00:00:00Z	1 B.C.
// +000001-01-01T00:00:00Z	1 A.D.
// +001970-01-01T00:00:00Z	1970 A.D.
// +002009-12-15T00:00:00Z	2009 A.D.
// +275760-09-13T00:00:00Z	275760 A.D.

fn parse_date(date_str: &JSString) -> f64 {
    const FDY: &str = "(?<four_digit_year>[0-9]{4})";
    const SDY: &str = "(?<six_digit_year>[-+][0-9]{6})";
    const MONTH: &str = "(?<month>[0-9][0-9])";
    const DAY: &str = "(?<day>[0-9][0-9])";
    const HOUR: &str = "(?<hour>[0-9][0-9])";
    const MINUTE: &str = "(?<minute>[0-9][0-9])";
    const SECOND: &str = "(?<second>[0-9][0-9])";
    const MILLIS: &str = "(?<millis>[0-9]{3})";
    const ZONE: &str = "(?<zone>Z|([-+][0-9][0-9]:[0-9][0-9]))";
    lazy_static! {
        static ref PATTERN: String =
            format!(r"({FDY}|{SDY})(-{MONTH}(-{DAY}(T{HOUR}:{MINUTE}(:{SECOND}(\.{MILLIS})?)?{ZONE}?)?)?)?");
        static ref MATCHER: Regex =
            Regex::new(&PATTERN).expect("regular expressions not based on user input should not fail");
    }
    let date_str = String::from(date_str);
    let date_match = MATCHER.captures(&date_str);
    match date_match {
        None => f64::NAN,
        Some(caps) => {
            let year = f64::from(if let Some(year) = caps.name("four_digit_year") {
                year.as_str().parse::<i32>().expect("match should parse as an integer")
            } else {
                caps.name("six_digit_year")
                    .expect("we should get either a 4-digit year or a six-digit year")
                    .as_str()
                    .parse::<i32>()
                    .expect("match should parse as an integer")
            });
            let month = f64::from(
                caps.name("month")
                    .map_or(1, |month| month.as_str().parse::<i32>().expect("month should parse as an integer")),
            ) - 1.0;
            let day = f64::from(
                caps.name("day").map_or(1, |day| day.as_str().parse::<i32>().expect("days should parse as an integer")),
            );
            let hour = f64::from(
                caps.name("hour")
                    .map_or(0, |hour| hour.as_str().parse::<i32>().expect("hours shouls parse as an integer")),
            );
            let minute = f64::from(
                caps.name("minute")
                    .map_or(0, |minute| minute.as_str().parse::<i32>().expect("minutes should parse as an int")),
            );
            let second = f64::from(
                caps.name("second")
                    .map_or(0, |second| second.as_str().parse::<i32>().expect("seconds should parse as an int")),
            );
            let millis = f64::from(
                caps.name("millis")
                    .map_or(0, |millis| millis.as_str().parse::<i32>().expect("millis should parse as an int")),
            );
            let zone = caps.name("zone").map_or("Z", |zone| zone.as_str());

            make_date(make_day(year, month, day).expect("should be ok"), make_time(hour, minute, second, millis))
        }
    }
}

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

fn date_prototype_gettimezoneoffset(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Date.prototype.getTimezoneOffset ( )
    // This method performs the following steps when called:
    //
    //  1. Let dateObject be the this value.
    //  2. Perform ? RequireInternalSlot(dateObject, [[DateValue]]).
    //  3. Let t be dateObject.[[DateValue]].
    //  4. If t is NaN, return NaN.
    //  5. Return (t - LocalTime(t)) / msPerMinute.
    this_value
        .to_date_object()
        .map(|obj| {
            let t = obj.date_value();
            let result = (t - local_time(t).unwrap()) / MS_PER_MINUTE_F64;
            ECMAScriptValue::Number(result)
        })
        .ok_or_else(|| create_type_error("Date.prototype.getTimezoneOffset requires a datelike object"))
}

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
fn date_prototype_tostring(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Date.prototype.toString ( )
    // This method performs the following steps when called:
    //
    //  1. Let dateObject be the this value.
    //  2. Perform ? RequireInternalSlot(dateObject, [[DateValue]]).
    //  3. Let tv be dateObject.[[DateValue]].
    //  4. Return ToDateString(tv).
    //
    // Note 1
    // For any Date d such that d.[[DateValue]] is evenly divisible by 1000, the result of Date.parse(d.toString()) =
    // d.valueOf(). See 21.4.3.2.
    //
    // Note 2
    // This method is not generic; it throws a TypeError exception if its this value is not a Date. Therefore, it cannot
    // be transferred to other kinds of objects for use as a method.
    this_value
        .to_date_object()
        .ok_or_else(|| create_type_error("Date.prototype.valueOf requires a datelike object"))
        .map(|date_object| {
            ECMAScriptValue::String(
                to_date_string(date_object.date_value())
                    .expect("a valid date value should always produce a good date string"),
            )
        })
}

fn time_string(tv: f64) -> JSString {
    // TimeString ( tv )
    // The abstract operation TimeString takes argument tv (a Number, but not NaN) and returns a String. It performs the
    // following steps when called:
    //
    // 1. Let hour be ToZeroPaddedDecimalString(ℝ(HourFromTime(tv)), 2).
    // 2. Let minute be ToZeroPaddedDecimalString(ℝ(MinFromTime(tv)), 2).
    // 3. Let second be ToZeroPaddedDecimalString(ℝ(SecFromTime(tv)), 2).
    // 4. Return the string-concatenation of hour, ":", minute, ":", second, the code unit 0x0020 (SPACE), and "GMT".
    let hour = to_zero_padded_decimal_string(usize::from(hour_from_time(tv)), 2);
    let minute = to_zero_padded_decimal_string(usize::from(min_from_time(tv)), 2);
    let second = to_zero_padded_decimal_string(usize::from(sec_from_time(tv)), 2);
    hour.concat(":").concat(minute).concat(":").concat(second).concat(" GMT")
}

fn date_string(tv: f64) -> JSString {
    // DateString ( tv )
    // The abstract operation DateString takes argument tv (a Number, but not NaN) and returns a String. It performs the
    // following steps when called:
    //
    //  1. Let weekday be the Name of the entry in Table 61 with the Number WeekDay(tv).
    //  2. Let month be the Name of the entry in Table 62 with the Number MonthFromTime(tv).
    //  3. Let day be ToZeroPaddedDecimalString(ℝ(DateFromTime(tv)), 2).
    //  4. Let yv be YearFromTime(tv).
    //  5. If yv is +0𝔽 or yv > +0𝔽, let yearSign be the empty String; otherwise, let yearSign be "-".
    //  6. Let paddedYear be ToZeroPaddedDecimalString(abs(ℝ(yv)), 4).
    //  7. Return the string-concatenation of weekday, the code unit 0x0020 (SPACE), month, the code unit 0x0020
    //     (SPACE), day, the code unit 0x0020 (SPACE), yearSign, and paddedYear.
    //
    // Table 61: Names of days of the week
    // Number	Name
    // +0𝔽	"Sun"
    // 1𝔽	"Mon"
    // 2𝔽	"Tue"
    // 3𝔽	"Wed"
    // 4𝔽	"Thu"
    // 5𝔽	"Fri"
    // 6𝔽	"Sat"
    //
    // Table 62: Names of months of the year
    // Number	Name
    // +0𝔽	"Jan"
    // 1𝔽	"Feb"
    // 2𝔽	"Mar"
    // 3𝔽	"Apr"
    // 4𝔽	"May"
    // 5𝔽	"Jun"
    // 6𝔽	"Jul"
    // 7𝔽	"Aug"
    // 8𝔽	"Sep"
    // 9𝔽	"Oct"
    // 10𝔽	"Nov"
    // 11𝔽	"Dec"
    const WEEKDAY_NAMES: [&str; 7] = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
    const MONTH_NAMES: [&str; 12] =
        ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];

    let weekday = WEEKDAY_NAMES[week_day(tv) as usize];
    let month = MONTH_NAMES[month_from_time(tv) as usize];
    let day = to_zero_padded_decimal_string(date_from_time(tv) as usize, 2);
    let yv = year_from_time(tv);
    let year_sign = if yv >= 0 { "" } else { "-" };
    let padded_year = to_zero_padded_decimal_string(yv.abs().try_into().unwrap(), 4);

    JSString::from(weekday)
        .concat(" ")
        .concat(month)
        .concat(" ")
        .concat(day)
        .concat(" ")
        .concat(year_sign)
        .concat(padded_year)
}

fn time_zone_string(tv: f64) -> anyhow::Result<JSString> {
    // TimeZoneString ( tv )
    // The abstract operation TimeZoneString takes argument tv (an integral Number) and returns a String. It performs
    // the following steps when called:
    //
    //  1. Let systemTimeZoneIdentifier be SystemTimeZoneIdentifier().
    //  2. If IsTimeZoneOffsetString(systemTimeZoneIdentifier) is true, then
    //     a. Let offsetNs be ParseTimeZoneOffsetString(systemTimeZoneIdentifier).
    //  3. Else,
    //     a. Let offsetNs be GetNamedTimeZoneOffsetNanoseconds(systemTimeZoneIdentifier, ℤ(ℝ(tv) × 10**6)).
    //  4. Let offset be 𝔽(truncate(offsetNs / 10**6)).
    //  5. If offset is +0𝔽 or offset > +0𝔽, then
    //     a. Let offsetSign be "+".
    //     b. Let absOffset be offset.
    //  6. Else,
    //     a. Let offsetSign be "-".
    //     b. Let absOffset be -offset.
    //  7. Let offsetMin be ToZeroPaddedDecimalString(ℝ(MinFromTime(absOffset)), 2).
    //  8. Let offsetHour be ToZeroPaddedDecimalString(ℝ(HourFromTime(absOffset)), 2).
    //  9. Let tzName be an implementation-defined string that is either the empty String or the string-concatenation of
    //     the code unit 0x0020 (SPACE), the code unit 0x0028 (LEFT PARENTHESIS), an implementation-defined timezone
    //     name, and the code unit 0x0029 (RIGHT PARENTHESIS).
    //  10. Return the string-concatenation of offsetSign, offsetHour, offsetMin, and tzName.
    let system_time_zone_identifier = system_time_zone_identifier();
    let offset_ns = match parse_time_zone_offset_string(&system_time_zone_identifier) {
        Some(value) => value,
        None => {
            let tv = i128::from(TimeNumber::try_from(tv)?);
            get_named_time_zone_offset_nanoseconds(&system_time_zone_identifier, tv * 1_000_000)?
        }
    };
    let offset = f64::from(TimeNumber::try_from(offset_ns / 1_000_000)?);
    let (offset_sign, abs_offset) = if offset.signum() > 0.0 { ("+", offset) } else { ("-", -offset) };
    let offset_min = to_zero_padded_decimal_string(usize::from(min_from_time(abs_offset)), 2);
    let offset_hour = to_zero_padded_decimal_string(usize::from(hour_from_time(abs_offset)), 2);
    let tz_name = " (UTC)";
    Ok(JSString::from(offset_sign).concat(offset_hour).concat(offset_min).concat(tz_name))
}

fn to_date_string(tv: f64) -> anyhow::Result<JSString> {
    // ToDateString ( tv )
    // The abstract operation ToDateString takes argument tv (an integral Number or NaN) and returns a String. It
    // performs the following steps when called:
    //
    //  1. If tv is NaN, return "Invalid Date".
    //  2. Let t be LocalTime(tv).
    //  3. Return the string-concatenation of DateString(t), the code unit 0x0020 (SPACE), TimeString(t), and
    //     TimeZoneString(tv).
    if tv.is_nan() {
        Ok(JSString::from("Invalid Date"))
    } else {
        let t = local_time(tv)?;
        let date = date_string(t);
        let time = time_string(t);
        let zone = time_zone_string(tv)?;
        Ok(date.concat(" ").concat(time).concat(zone))
    }
}

todo_function!(date_prototype_totimestring);
todo_function!(date_prototype_toutcstring);

fn date_prototype_valueof(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Date.prototype.valueOf ( )
    // This method performs the following steps when called:
    //
    //  1. Let dateObject be the this value.
    //  2. Perform ? RequireInternalSlot(dateObject, [[DateValue]]).
    //  3. Return dateObject.[[DateValue]].
    this_value
        .to_date_object()
        .map(|obj| ECMAScriptValue::Number(obj.date_value()))
        .ok_or_else(|| create_type_error("Date.prototype.valueOf requires a datelike object"))
}

fn date_prototype_toprimitive(
    this_value: &ECMAScriptValue,
    _: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Date.prototype [ %Symbol.toPrimitive% ] ( hint )
    // This method is called by ECMAScript language operators to convert a Date to a primitive value. The allowed values
    // for hint are "default", "number", and "string". Dates are unique among built-in ECMAScript object in that they
    // treat "default" as being equivalent to "string", All other built-in ECMAScript objects treat "default" as being
    // equivalent to "number".
    //
    // It performs the following steps when called:
    //
    // 1. Let O be the this value.
    // 2. If O is not an Object, throw a TypeError exception.
    // 3. If hint is either "string" or "default", then
    //    a. Let tryFirst be string.
    // 4. Else if hint is "number", then
    //    a. Let tryFirst be number.
    // 5. Else,
    //    a. Throw a TypeError exception.
    // 6. Return ? OrdinaryToPrimitive(O, tryFirst).
    let mut args = FuncArgs::from(arguments);
    let hint = args.next_arg();
    if let ECMAScriptValue::Object(o) = this_value {
        let try_first = if hint == ECMAScriptValue::from("default") || hint == ECMAScriptValue::from("string") {
            ConversionHint::String
        } else if hint == ECMAScriptValue::from("number") {
            ConversionHint::Number
        } else {
            return Err(create_type_error("Date.prototype[Symbol.toPrimitive]: invalid hint"));
        };
        ordinary_to_primitive(o, try_first)
    } else {
        Err(create_type_error("Date.prototype[Symbol.toPrimitive] called on incompatible receiver"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_case::test_case;

    #[test]
    fn day() {
        let historical = -(MS_PER_DAY_F64);
        assert_eq!(super::day(historical), -1);
    }
    #[test]
    fn day_within_year() {
        let historical = -(MS_PER_DAY_F64);
        assert_eq!(super::day_within_year(historical), 364);
    }

    #[test_case(0.0 => "Thu Jan 01 1970")]
    #[test_case(-1.0 => "Wed Dec 31 1969")]
    #[test_case(1_727_974_098_974.0 => "Thu Oct 03 2024")]
    fn date_string(tv: f64) -> String {
        super::date_string(tv).to_string()
    }

    #[test_case(0.0 => Some(0.0); "zero")]
    #[test_case(f64::INFINITY => None; "positive infinity")]
    #[test_case(f64::NEG_INFINITY => None; "negative infinity")]
    #[test_case(f64::NAN => None; "not a number")]
    #[test_case(8.0e15 => Some(8.0e15); "big but valid")]
    fn time_clip(t: f64) -> Option<f64> {
        let result = super::time_clip(t);
        if result.is_nan() {
            None
        } else {
            Some(result)
        }
    }

    #[test_case("0000" => -62_167_219_200_000.0; "zero")]
    #[test_case("0001" => -62_135_596_800_000.0; "one")]
    #[test_case("1969" => -31_536_000_000.0; "nineteen sixty-nine")]
    #[test_case("1970" => 0.0; "nineteen seventy")]
    #[test_case("1971" => 31_536_000_000.0; "nineteen seventy-one")]
    #[test_case("2000" => 946_684_800_000.0; "two thousand")]
    #[test_case("1929-03-22T10:53:31.021Z" => -1_286_888_788_979.0; "march 22, 1929")]
    fn parse_date(s: &str) -> f64 {
        super::parse_date(&JSString::from(s))
    }

    #[test_case(-62_167_219_200_000.0 => 1; "year zero")]
    fn date_from_time(t: f64) -> u8 {
        super::date_from_time(t)
    }

    #[test_case(-62_167_219_200_000.0 => 0; "year zero")]
    fn year_from_time(t: f64) -> isize {
        super::year_from_time(t)
    }
}
