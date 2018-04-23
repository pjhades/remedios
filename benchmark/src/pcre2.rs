#![allow(non_camel_case_types)]

use rand::{Rng, thread_rng};
use std::ffi::CString;
use std::os::raw::{c_char, c_void};
use std::ptr;

type pcre2_code = c_void;
type pcre2_match_data = c_void;
type pcre2_compile_context = c_void;
type pcre2_general_context = c_void;
type pcre2_match_context = c_void;

const PCRE2_ERROR_NOMATCH: i32 = -1;

#[link(name = "pcre2-8")]
extern "C" {
    fn pcre2_compile_8(
        pattern: *mut c_char,
        len: usize,
        options: u32,
        errnum: *mut i32,
        erroff: *mut usize,
        context: *mut pcre2_compile_context
    ) -> *mut pcre2_code;

    fn pcre2_match_data_create_from_pattern_8(
        code: *const pcre2_code,
        ctx: *mut pcre2_general_context
    ) -> *mut pcre2_match_data;

    fn pcre2_match_8(
        code: *const pcre2_code,
        pattern: *mut c_char,
        len: usize,
        offset: usize,
        options: u32,
        data: *mut pcre2_match_data,
        ctx: *mut pcre2_match_context
    ) -> i32;

    fn pcre2_match_data_free(data: *mut pcre2_match_data) -> c_void;
    fn pcre2_code_free(code: *mut pcre2_code) -> c_void;
}

pub fn rematch(pattern: &str, text: &str) -> Result<bool, String> {
    let pat_len = pattern.len();
    let txt_len = text.len();
    let mut errnum: i32 = 0;
    let mut erroff: usize = 0;

    let code = unsafe {
        pcre2_compile_8(
            CString::new(pattern).unwrap().into_raw(),
            pat_len,
            0,
            &mut errnum as *mut i32,
            &mut erroff,
            ptr::null_mut())
    };
    if code.is_null() {
        return Err("compile error".to_string());
    }

    let data = unsafe {
        pcre2_match_data_create_from_pattern_8(code, ptr::null_mut())
    };

    let ret = unsafe {
        pcre2_match_8(
            code,
            CString::new(text).unwrap().into_raw(),
            txt_len,
            0,
            0,
            data,
            ptr::null_mut())
    };
    if ret < 0 {
        if ret == PCRE2_ERROR_NOMATCH {
            return Ok(false);
        }
        return Err("match error".to_string());
    }

    Ok(true)
}
