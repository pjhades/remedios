#![allow(non_camel_case_types)]

use std::ffi::CString;
use std::os::raw::{c_char, c_void};
use std::ptr;

type OnigEncodingType = c_void;
type OnigSyntaxType = c_void;
type OnigOptionType = u32;
type OnigRegion = c_void;
type OnigErrorInfo = c_void;

#[repr(C)]
struct regex_t {
    onig: *mut c_void,
    re_nsub: usize,
    comp_options: i32,
}

#[link(name = "onig")]
extern {
    static OnigEncodingASCII: OnigEncodingType;
    static OnigDefaultSyntax: *const OnigSyntaxType;

    fn onig_initialize(
        encodings: *const *const OnigEncodingType,
        n: i32
    ) -> i32;

    fn onig_new(
        re: *mut *mut regex_t,
        pattern: *const c_char,
        pattern_end: *const c_char,
        options: OnigOptionType,
        enc: *const OnigEncodingType,
        syntax: *const OnigSyntaxType,
        einfo: *mut OnigErrorInfo
    ) -> i32;

    fn onig_region_new() -> *mut OnigRegion;

    fn onig_search(
        re: *mut regex_t,
        s: *const c_char,
        end: *const c_char,
        start: *const c_char,
        range: *const c_char,
        region: *mut OnigRegion,
        options: OnigOptionType
    ) -> i32;

    fn onig_region_free(r: *mut OnigRegion, free_self: i32) -> c_void;
    fn onig_free(re: *mut regex_t) -> c_void;
    fn onig_end() -> i32;
}

pub fn rematch(pattern: &str, text: &str) -> Result<bool, String> {
    unsafe { onig_initialize([&OnigEncodingASCII as *const OnigEncodingType].as_ptr(), 1); }

    let pat_start: *const c_char = CString::new(pattern).unwrap().into_raw();
    let pat_end: *const c_char = unsafe {
        pat_start.clone().offset(pattern.len() as isize)
    };

    let mut re = regex_t {
        onig: ptr::null_mut(),
        re_nsub: 0,
        comp_options: 0,
    };
    let mut re_ptr: *mut regex_t = &mut re;
    let re_ptrptr: *mut *mut regex_t = &mut re_ptr;

    let r = unsafe {
        onig_new(
            re_ptrptr,
            pat_start,
            pat_end,
            0,
            &OnigEncodingASCII as *const OnigEncodingType,
            OnigDefaultSyntax,
            ptr::null_mut())
    };

    if r != 0 {
        return Err("compile error".to_string());
    }

    let text_start: *const c_char = CString::new(text).unwrap().into_raw();
    let text_end: *const c_char = unsafe {
        text_start.clone().offset(text.len() as isize)
    };

    let region = unsafe { onig_region_new() };
    let r = unsafe {
        onig_search(
            re_ptr,
            text_start,
            text_end,
            text_start,
            text_end,
            region,
            0)
    };

    unsafe {
        onig_region_free(region, 1);
        onig_free(re_ptr);
        onig_end();
    }

    if r >= 0 {
        return Ok(true);
    }
    else if r == -1 {
        return Ok(false);
    }

    return Err("match error".to_string());
}
