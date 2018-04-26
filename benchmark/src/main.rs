#![feature(test)]

extern crate benchmark;
//extern crate core;
extern crate rand;
extern crate regex;
extern crate remedios;
extern crate test;
extern crate time;

use benchmark::{pcre2, oniguruma, re2};
//use core::arch;
use rand::{Rng, thread_rng};
//use remedios::{MatchResult, rematch};
use regex::Regex;

const RE_RNA: &str = r"^((A|C|G|T)?)*(A|C|G|T)*$";
const K: usize = 1024;
const M: usize = 1024 * K;

type Matcher = fn(&str, &str) -> Result<bool, String>;
type TextGenerator = fn(usize) -> String;

fn rna_of_len(len: usize) -> String {
    let mut rng = thread_rng();
    let nucleobase = ['A', 'C', 'G', 'T'];
    let mut s = String::with_capacity(len);
    for _ in 0..len {
        s.push(nucleobase[rng.gen_range(0, nucleobase.len())]);
    }
    s
}

fn alphanumeric_of_len(len: usize) -> String {
    let mut rng = thread_rng();
    let alphabet = "abcdefghijklmnopqrstuvwxyz\
                    ABCDEFGHIJKLMNOPQRSTUVWXYZ\
                    0123456789".chars().collect::<Vec<char>>();
    let mut s = String::with_capacity(len);
    for _ in 0..len {
        s.push(alphabet[rng.gen_range(0, alphabet.len())]);
    }
    s
}

fn rust_regex_rematch(pattern: &str, text: &str) -> Result<bool, String> {
    Ok(Regex::new(pattern)
       .map_err(|e| e.to_string())?
       .is_match(text))
}

fn remedios_rematch(pattern: &str, text: &str) -> Result<bool, String> {
    match remedios::rematch(pattern, text).map_err(|e| e.to_string())? {
        remedios::MatchResult::NotMatch => Ok(false),
        remedios::MatchResult::Match(_) => Ok(true),
    }
}

//#[cfg(target_arch = "x86")]
//fn flush_cache_line(ptr: *mut u8) {
//    arch::x86::_mm_clflush(ptr);
//}
//
//#[cfg(target_arch = "x86_64")]
//fn flush_cache_line(ptr: *mut u8) {
//    arch::x86_64::_mm_clflush(ptr);
//}
//
//#[cfg(not(any(target_arch = "x86_64", target_arch = "x86")))]
//fn flush_cache_line(_ptr: *mut u8) {
//    // no-op on other architectures, the results may differ
//}

const MATCHERS: &[(&str, Matcher)] = &[
    ("pcre2",     pcre2::rematch),
    ("oniguruma", oniguruma::rematch),
    ("re2",       re2::rematch),
    ("rust",      rust_regex_rematch),
    ("remedios",  remedios_rematch),
];

fn b(description: &str, pattern: &str, text_generator: TextGenerator) {
    println!("{}", description);

    for (name, func) in MATCHERS.iter() {
        let mut len = 8;
        while len <= 16*M {
            let text = text_generator(len);

            let begin_at = time::precise_time_ns();
            let ret = func(pattern, &text);
            let ns = time::precise_time_ns() - begin_at;

            if let Err(_) = ret {
                break;
            }

            println!("  {:15} {:5} {:15} ns {:10.2} M/s",
                     name,
                     if len < K {
                         format!("{}", len)
                     }
                     else if len < M {
                         format!("{}K", len / K)
                     }
                     else {
                         format!("{}M", len / M)
                     },
                     ns,
                     (len * 1_000_000_000) as f64 / ns as f64 / M as f64);
            len *= 2;
        }
    }
}

fn main() {
    b("full match", r"^.*$", alphanumeric_of_len);
    b("rna", RE_RNA, rna_of_len);
    //let cases = [
    //    ("full match", r"^.*$", ascii_of_len),
    //];

    //for (caption, pattern, gen) in cases.iter() {
    //    run_all(caption, pattern, gen);
    //}

    //println!("{}", pcre2::rematch(RE_RNA, &rna).unwrap());
    //println!("{}", oniguruma::rematch(RE_RNA, &rna).unwrap());
    //println!("{}", re2::rematch(RE_RNA, &rna).unwrap());
    //println!("{}", Regex::new(RE_RNA).unwrap().is_match(&rna));
}

#[cfg(test)]
mod tests {
    use test::Bencher;
    use super::*;

    macro_rules! add_bench {
        ( $name:ident, $matcher:ident, $len:expr ) => {
            #[bench]
            fn $name(b: &mut Bencher) {
                let rna = rna_of_len($len);
                b.iter(|| { $matcher(RE_RNA, &rna).unwrap(); });
            }
        };
    }

    add_bench!(  rna8Kr, remedios_rematch,   8*K);
    add_bench!( rna16Kr, remedios_rematch,  16*K);
    //add_bench!( rna32Kr, remedios_rematch,  32*K);
    //add_bench!( rna64Kr, remedios_rematch,  64*K);
    //add_bench!(rna128Kr, remedios_rematch, 128*K);
    //add_bench!(rna256Kr, remedios_rematch, 256*K);
    //add_bench!(rna512Kr, remedios_rematch, 512*K);

    add_bench!(  rna8K, rust_regex_rematch,   8*K);
    add_bench!( rna16K, rust_regex_rematch,  16*K);
    //add_bench!( rna32K, rust_regex_rematch,  32*K);
    //add_bench!( rna64K, rust_regex_rematch,  64*K);
    //add_bench!(rna128K, rust_regex_rematch, 128*K);
    //add_bench!(rna256K, rust_regex_rematch, 256*K);
    //add_bench!(rna512K, rust_regex_rematch, 512*K);
}
