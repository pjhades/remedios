#![feature(test)]

extern crate benchmark;

extern crate rand;
extern crate remedios;
extern crate test;

use rand::{Rng, thread_rng};
//use remedios::{MatchResult, rematch};
use benchmark::{pcre2, oniguruma};

const RE_RNA: &str = r"^((A|C|G|T)?)*(A|C|G|T)*$";

fn rna_of_len(len: usize) -> String {
    let mut rng = thread_rng();
    let nucleobase = ['A', 'C', 'G', 'T'];
    let mut rna = String::with_capacity(len);
    for _ in 0..len {
        rna.push(nucleobase[rng.gen_range(0, nucleobase.len())]);
    }
    rna
}

fn main() {
    let rna = rna_of_len(100000);
    //let m = rematch(RE_RNA, &rna).unwrap();
    //assert!(match m {
    //    MatchResult::Match(_) => true,
    //    _ => false,
    //});
    println!("{}", pcre2::rematch(RE_RNA, &rna).unwrap());
    println!("{}", oniguruma::rematch(RE_RNA, &rna).unwrap());
}

#[cfg(test)]
mod tests {
    use test::Bencher;
    use super::*;

    macro_rules! add_bench {
        ( rna, $func:ident, $len:expr ) => {
            #[bench]
            fn $func(b: &mut Bencher) {
                let rna = rna_of_len($len);
                b.iter(|| { rematch(RE_RNA, &rna).unwrap(); });
            }
        };
    }

    add_bench!(rna, rna10, 10);
    add_bench!(rna, rna20, 20);
    add_bench!(rna, rna30, 30);
    add_bench!(rna, rna40, 40);
    add_bench!(rna, rna50, 50);
    add_bench!(rna, rna60, 60);
    add_bench!(rna, rna70, 70);
    add_bench!(rna, rna80, 80);
    add_bench!(rna, rna90, 90);
    add_bench!(rna, rna100, 100);
}
