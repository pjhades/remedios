use ::{Group, Groups};
use compile::{Prog, Inst, Iaddr};

#[derive(Copy, Clone, Debug)]
struct Thread {
    pc: Iaddr,
    groups: Groups,
}

#[derive(Debug)]
pub struct Vm<'a> {
    // The compiled program for the regex.
    pub prog: &'a Prog,
    // Groups captured along the execution.
    pub groups: Groups,
    // `visited[pc]` is the latest string index seen by thread `pc`.
    // Since the string index only moves forward, we only need to keep
    // one `pc` for a certain string index, because the execution
    // will be the same.
    visited: Vec<usize>,
}

impl<'a> Vm<'a> {
    pub fn new(prog: &'a Prog) -> Self {
        Vm {
            prog,
            groups: Groups::default(),
            visited: vec![0; prog.insts.len()],
        }
    }

    fn catch_up(&mut self, mut th: Thread, si: usize, slen: usize, list: &mut Vec<Thread>) {
        // The recursion here needs more love. Should be done with a stack.
        if si + 1 == self.visited[th.pc as usize] {
            return;
        }
        self.visited[th.pc as usize] = si + 1;

        match self.prog.insts[th.pc as usize] {
            Inst::AssertHat => {
                if si != 0 {
                    return;
                }
                th.pc += 1;
                self.catch_up(th, si, slen, list);
            },
            Inst::AssertDollar => {
                if si != slen {
                    return;
                }
                th.pc += 1;
                self.catch_up(th, si, slen, list);
            },
            Inst::Jump(iaddr) => {
                th.pc = iaddr;
                self.catch_up(th, si, slen, list);
            },
            Inst::Split(iaddr1, iaddr2) => {
                th.pc = iaddr1;
                self.catch_up(th.clone(), si, slen, list);
                th.pc = iaddr2;
                self.catch_up(th, si, slen, list);
            },
            Inst::Save(groupidx) => {
                let g = groupidx / 2;
                if groupidx % 2 == 0 {
                    th.groups[g as usize] = Some(Group { begin: si, end: si });
                }
                else {
                    if let Some(ref mut group) = th.groups[g as usize] {
                        group.end = si;
                    }
                }
                th.pc += 1;
                self.catch_up(th, si, slen, list);
            },
            _ => {
                list.push(th);
            },
        }
    }

    pub fn run(&mut self, s: &Vec<char>) -> bool {
        let mut v1: Vec<Thread> = Vec::with_capacity(self.prog.insts.len());
        let mut v2: Vec<Thread> = Vec::with_capacity(self.prog.insts.len());
        let mut curr: *mut Vec<Thread> = &mut v1;
        let mut next: *mut Vec<Thread> = &mut v2;

        let slen = s.len();

        unsafe {
            let mut si = 0;
            self.catch_up(Thread { pc: 0, groups: Groups::default() }, si, slen, &mut *curr);

            while !(*curr).is_empty() {
                for th in (*curr).iter_mut() {
                    match &self.prog.insts[th.pc as usize] {
                        &Inst::Match => {
                            th.groups[0] = Some(Group { begin: 0, end: s.len() });
                            self.groups = th.groups.clone();
                            return true;
                        },
                        &Inst::Char(c) => {
                            if si < s.len() && s[si] == c {
                                th.pc += 1;
                                self.catch_up(*th, si + 1, slen, &mut *next);
                            }
                        },
                        _ => self.catch_up(*th, si, slen, &mut *next),
                    }
                }
                (*curr).clear();
                si += 1;

                let temp = curr;
                curr = next;
                next = temp;
            }
        }

        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use parse::Parser;
    use compile::Compiler;

    fn m(re: &str, s: &str) -> bool {
        let prog = Compiler::compile(&Parser::parse(re).unwrap()).unwrap();
        let mut vm = Vm::new(&prog);
        vm.run(&s.chars().collect())
    }

    macro_rules! assert_match {
        ( $re:expr, $s:expr ) => { assert!(m($re, $s)); }
    }

    macro_rules! assert_not_match {
        ( $re:expr, $s:expr ) => { assert!(!m($re, $s)); }
    }

    macro_rules! assert_match_groups {
        ( $re:expr, $s:expr, $( ($groupidx:expr, $begin:expr, $end:expr) ),+ ) => {
            let prog = Compiler::compile(&Parser::parse($re).unwrap()).unwrap();
            let mut vm = Vm::new(&prog);
            assert!(vm.run(&$s.chars().collect()));

            let mut expected = Groups::default();
            expected[0] = Some(Group { begin: 0, end: $s.len() });
            for (groupidx, begin, end) in vec![$(($groupidx, $begin, $end)),+] {
                expected[groupidx] = Some(Group { begin, end });
            }
            assert_eq!(vm.groups, expected);
        }
    }

    #[test]
    fn test_match() {
        // The good
        assert_match!(r"a", "a");
        assert_match!(r"a?", "a");
        assert_match!(r"a?", "");
        assert_match!(r"a*", "");
        assert_match!(r"a*", "a");
        assert_match!(r"a*", "b");
        assert_match!(r"a*", "aaaaa");
        assert_match!(r"a+", "a");
        assert_match!(r"a+", "aaaaa");
        assert_match!(r"a|b|c", "a");
        assert_match!(r"a|b|c", "b");
        assert_match!(r"a|b|c", "c");
        assert_match!(r"abcde", "abcde");
        assert_match!(r"(a*)*", "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa");
        assert_match!(r"((a*)*)*", "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa");
        assert_match!(r"a*a*a*a*", "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa");
        assert_match!(r"(a?)*a*", "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa");
        assert_match!(r"(a*)?a*", "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa");
        assert_match!(r"(a|b)*d+(ef)?", "d");
        assert_match!(r"(a|b)*d+(ef)?", "def");
        assert_match!(r"(a|b)*d+(ef)?", "ddef");
        assert_match!(r"(a|b)*d+(ef)?", "addef");
        assert_match!(r"(a|b)*d+(ef)?", "bddef");
        assert_match!(r"(a|b)*d+(ef)?", "aabbaddef");

        assert_match!(r"^abc", "abcdefg");
        assert_match!(r"a*$", "aaaaaaa");
        assert_match!(r"^a*$", "aaaaaaa");

        assert_match_groups!(r"(a)", "a", (1, 0, 1));
        assert_match_groups!(r"(a)(b)", "ab", (1, 0, 1), (2, 1, 2));
        assert_match_groups!(r"(a|b)+d", "abaabd", (1, 4, 5));
        assert_match_groups!(r"(a(b(c)))d", "abcd", (1, 0, 3), (2, 1, 3), (3, 2, 3));
        assert_match_groups!(r"(a(b)|c(d))e", "cde", (1, 0, 2), (3, 1, 2));

        // The bad
        assert_not_match!(r"a", "b");
        assert_not_match!(r"abc", "bca");
        assert_not_match!(r"a+", "");
        assert_not_match!(r"a|b|c", "x");
        assert_not_match!(r"(a|b)+", "x");
        assert_not_match!(r"^a*$", "aaaab");
        assert_not_match!(r"a*$", "aaaab");
    }
}
