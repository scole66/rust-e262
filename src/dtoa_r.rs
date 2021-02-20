// char *dtoa_rust(double, int *, int *, char *, unsigned int);

use libc::size_t;
use std::os::raw::{c_double, c_int, c_uchar};

extern "C" {
    pub fn dtoa_rust(value: c_double, decpt: *mut c_int, sign: *mut c_int, outbuf: *mut c_uchar, buflen: size_t);
}

//#[derive(Debug)]
pub struct DtoAResult {
    pub chars: String,
    pub decpt: i32,
    pub sign: i8,
}

pub fn dtoa(value: f64) -> DtoAResult {
    let mut decpt: c_int = 0;
    let mut sign: c_int = 0;
    let mut digits: [u8; 64] = [0; 64];

    unsafe {
        dtoa_rust(value as c_double, &mut decpt, &mut sign, digits.as_mut_ptr(), digits.len() as size_t);
    }

    DtoAResult { chars: String::from_utf8(digits.to_vec()).unwrap(), decpt: decpt as i32, sign: sign as i8 }
}
