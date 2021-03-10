use crate::dtoa_r::dtoa;
use std::io;

pub fn number_to_string<T>(writer: &mut T, value: f64) -> io::Result<()>
where
    T: io::Write,
{
    if value.is_nan() {
        return write!(writer, "NaN");
    }
    if value == 0.0 {
        return write!(writer, "0");
    }
    if value < 0.0 {
        write!(writer, "-")?;
        return number_to_string(writer, -value);
    }
    if value.is_infinite() {
        return write!(writer, "Infinity");
    }
    let info = dtoa(value);

    let k = info.chars.find('\u{0}').unwrap() as i64;
    let n = info.decpt as i64;
    let mut iter = info.chars.chars();
    if k <= n && n <= 21 {
        for _ in 0..k {
            let ch = iter.next().unwrap();
            write!(writer, "{}", ch)?;
        }
        let zeros = n - k;
        for _ in 0..zeros {
            write!(writer, "0")?;
        }
        return Ok(());
    }
    if 0 < n && n <= 21 {
        for _ in 0..n {
            let ch = iter.next().unwrap();
            write!(writer, "{}", ch)?;
        }
        write!(writer, ".")?;
        for _ in 0..k - n {
            let ch = iter.next().unwrap();
            write!(writer, "{}", ch)?;
        }
        return Ok(());
    }
    if -6 < n && n <= 0 {
        write!(writer, "0.")?;
        for _ in n..0 {
            write!(writer, "0")?;
        }
        for _ in 0..k {
            let ch = iter.next().unwrap();
            write!(writer, "{}", ch)?;
        }
        return Ok(());
    }
    let ch = iter.next().unwrap();
    write!(writer, "{}", ch)?;
    if k > 1 {
        write!(writer, ".")?;
        for _ in 1..k {
            let ch = iter.next().unwrap();
            write!(writer, "{}", ch)?;
        }
    }
    write!(writer, "e")?;
    let v = n - 1;
    write!(writer, "{}{}", if v > 0 { '+' } else { '-' }, v.abs())?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nts_test_nan() {
        let mut s = Vec::new();
        number_to_string(&mut s, f64::NAN).unwrap();
        assert_eq!(s, "NaN".as_bytes());
    }
    #[test]
    fn nts_test_zero() {
        let mut s = Vec::new();
        number_to_string(&mut s, 0.0).unwrap();
        assert_eq!(s, "0".as_bytes());
    }
    #[test]
    fn nts_test_infinity() {
        let mut s = Vec::new();
        number_to_string(&mut s, f64::INFINITY).unwrap();
        assert_eq!(s, "Infinity".as_bytes());
    }
    #[test]
    fn nts_test_negatives() {
        let mut s = Vec::new();
        number_to_string(&mut s, -6.0).unwrap();
        assert_eq!(s, "-6".as_bytes());
    }
    #[test]
    fn nts_test_ends_with_zeroes() {
        let mut s = Vec::new();
        number_to_string(&mut s, 98000000.0).unwrap();
        assert_eq!(s, "98000000".as_bytes());
    }
    #[test]
    fn nts_test_leading_zeroes() {
        let mut s = Vec::new();
        number_to_string(&mut s, 0.00125).unwrap();
        assert_eq!(s, "0.00125".as_bytes());
    }
    #[test]
    fn nts_test_decimal_mid() {
        let mut s = Vec::new();
        number_to_string(&mut s, 104.5024).unwrap();
        assert_eq!(s, "104.5024".as_bytes());
    }
    #[test]
    fn nts_test_pos_exponent() {
        let mut s = Vec::new();
        number_to_string(&mut s, 6.02e23).unwrap();
        assert_eq!(s, "6.02e+23".as_bytes());
    }
    #[test]
    fn nts_test_neg_exponent() {
        let mut s = Vec::new();
        number_to_string(&mut s, 3.441e-10).unwrap();
        assert_eq!(s, "3.441e-10".as_bytes());
    }
    #[test]
    fn nts_test_1dig_exponent() {
        let mut s = Vec::new();
        number_to_string(&mut s, 3e-10).unwrap();
        assert_eq!(s, "3e-10".as_bytes());
    }
}
