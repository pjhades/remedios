extern crate remedios;

use remedios::{MatchResult, rematch, NGROUPS};

fn main() {
    let s = "defghxyz";
    let m = rematch(r"(a(bc)|d(ef)(gh))xyz", s).unwrap();
    if let MatchResult::Match(ref groups) = m {
        for i in 0..NGROUPS {
            let ii = i as usize * 2;
            if let (Some(begin), Some(end)) = (groups[ii], groups[ii + 1]) {
                println!("group {}: {}", i, &s[begin..end]);
            }
        }
    }
}
