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
#[test_case("1970-13-65" => with |i: f64| assert!(i.is_nan(), "Expected NaN, found {i}"); "out of range")]
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
