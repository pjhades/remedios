extern crate remedios;

use remedios::{MatchResult, Group, re_match};

fn main() {
    let s = "defghxyz";
    let m = re_match(r"(a(bc)|d(ef)(gh))xyz", s).unwrap();
    if let MatchResult::Match(ref groups) = m {
        for (i, g) in groups.iter().enumerate() {
            if let &Some(Group{ begin, end }) = g {
                println!("{}: {}", i, &s[begin..end]);
            }
        }
    }
}
