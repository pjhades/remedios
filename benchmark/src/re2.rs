#![allow(non_camel_case_types)]

use std::ffi::CString;
use std::os::raw::c_char;

#[link(name = "re2")]
extern {
    fn re2_partial_match(text: *const c_char, pattern: *const c_char) -> bool;
}

pub fn rematch(pattern: &str, text: &str) -> Result<bool, String> {
    if unsafe {
        re2_partial_match(
            CString::new(text).unwrap().into_raw(),
            CString::new(pattern).unwrap().into_raw())
    } {
        return Ok(true);
    }
    Ok(false)
}
