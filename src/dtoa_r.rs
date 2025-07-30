// char *dtoa_rust(double, int *, int *, char *, unsigned int);

use libc::size_t;
use std::os::raw::{c_double, c_int, c_uchar};
use std::sync::{Arc, LazyLock, Mutex};

unsafe extern "C" {
    pub fn dtoa_rust(
        value: c_double,
        mode: c_int,
        ndigits: c_int,
        decpt: *mut c_int,
        sign: *mut c_int,
        outbuf: *mut c_uchar,
        buflen: size_t,
    );
}

static DTOALOCK: LazyLock<Arc<Mutex<u32>>> = LazyLock::new(|| Arc::new(Mutex::new(0)));

#[derive(Debug)]
pub struct DtoAResult {
    pub chars: String,
    pub decpt: i32,
    pub sign: i8,
}

pub fn dtoa(value: f64) -> DtoAResult {
    let mut decpt: c_int = 0;
    let mut sign: c_int = 0;
    let mut digits: [u8; 64] = [0; 64];

    {
        let lock = Arc::clone(&DTOALOCK);
        let _locked = lock.lock().unwrap();

        unsafe {
            dtoa_rust(
                value as c_double,
                0,
                0,
                &raw mut decpt,
                &raw mut sign,
                digits.as_mut_ptr(),
                digits.len() as size_t,
            );
        }
    }
    DtoAResult {
        chars: String::from_utf8_lossy(&digits).to_string(),
        decpt,
        sign: i8::try_from(sign).expect("sign should fit into an i8"),
    }
}

pub fn dtoa_precise(value: f64, num_digits: i32) -> DtoAResult {
    let mut decpt: c_int = 0;
    let mut sign: c_int = 0;
    let mut digits: [u8; 110] = [0; 110];

    {
        let lock = Arc::clone(&DTOALOCK);
        let _locked = lock.lock().unwrap();

        unsafe {
            dtoa_rust(
                value as c_double,
                2,
                num_digits,
                &raw mut decpt,
                &raw mut sign,
                digits.as_mut_ptr(),
                digits.len() as size_t,
            );
        }
    }
    DtoAResult {
        chars: String::from_utf8_lossy(&digits).to_string(),
        decpt,
        sign: i8::try_from(sign).expect("sign should fit into an i8"),
    }
}
pub fn dtoa_fixed(value: f64, num_digits: i32) -> DtoAResult {
    let mut decpt: c_int = 0;
    let mut sign: c_int = 0;
    let mut digits: [u8; 110] = [0; 110];

    {
        let lock = Arc::clone(&DTOALOCK);
        let _locked = lock.lock().unwrap();

        unsafe {
            dtoa_rust(
                value as c_double,
                3,
                num_digits,
                &raw mut decpt,
                &raw mut sign,
                digits.as_mut_ptr(),
                digits.len() as size_t,
            );
        }
    }
    DtoAResult {
        chars: String::from_utf8_lossy(&digits).to_string(),
        decpt,
        sign: i8::try_from(sign).expect("sign should fit into an i8"),
    }
}
