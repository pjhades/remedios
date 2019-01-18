#![feature(test)]

extern crate benchmark;
//extern crate core;
extern crate rand;
extern crate regex;
extern crate remedios;
extern crate test;
extern crate time;

use benchmark::{pcre2, oniguruma, re2};
use rand::{Rng, thread_rng};
use regex::Regex;

const RE_ANY: &str = r"^.+$";
const RE_RNA: &str = r"^((A|C|G|T)?)*(A|C|G|T)*$";
const K: usize = 1024;
const M: usize = 1024 * K;

type Matcher = fn(&str, &str) -> Result<bool, String>;
type TextGenerator = fn(usize) -> String;

fn gen_rna(len: usize) -> String {
    let mut rng = thread_rng();
    let nucleobase = ['A', 'C', 'G', 'T'];
    let mut s = String::with_capacity(len);
    for _ in 0..len {
        s.push(nucleobase[rng.gen_range(0, nucleobase.len())]);
    }
    s
}

fn gen_alnum(len: usize) -> String {
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
}

#[cfg(test)]
mod tests {
    use test::Bencher;
    use super::*;

    macro_rules! add_bench {
        ( $benchname:ident, $matcher:ident, $regex:ident, $textgen:ident, $len:expr ) => {
            #[bench]
            fn $benchname(b: &mut Bencher) {
                let text = $textgen($len);
                b.iter(|| { $matcher($regex, &text).unwrap(); });
            }
        };
    }

    add_bench!(rna_8k_remedios,  remedios_rematch, RE_RNA, gen_rna, 8*K);
    add_bench!(rna_16k_remedios, remedios_rematch, RE_RNA, gen_rna, 16*K);
    add_bench!(rna_8k_rust,  rust_regex_rematch, RE_RNA, gen_rna, 8*K);
    add_bench!(rna_16k_rust, rust_regex_rematch, RE_RNA, gen_rna, 16*K);
    add_bench!(alnum_8k_remedios,  remedios_rematch, RE_ANY, gen_alnum, 8*K);
    add_bench!(alnum_16k_remedios, remedios_rematch, RE_ANY, gen_alnum, 16*K);
    add_bench!(alnum_8k_rust,  rust_regex_rematch, RE_ANY, gen_alnum, 8*K);
    add_bench!(alnum_16k_rust, rust_regex_rematch, RE_ANY, gen_alnum, 16*K);
}
