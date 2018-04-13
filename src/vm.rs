use ::Groups;
use compile::{Prog, Inst, Ip, CharKind};
use std::mem;

#[derive(Copy, Clone, Debug)]
struct Thread {
    pc: Ip,
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
    found_match: bool,
    stack: Vec<Thread>,
}

impl<'a> Vm<'a> {
    pub fn new(prog: &'a Prog) -> Self {
        Vm {
            prog,
            groups: Groups::default(),
            visited: vec![0; prog.insts.len()],
            found_match: false,
            stack: vec![],
        }
    }

    fn epsilon(&mut self,
               start: Thread,
               si: usize,
               slen: usize,
               list: &mut Vec<Thread>)
            -> bool {
        self.stack.clear();
        self.stack.push(start);
        // Keep following epsilon transitions until:
        //   - we find a match, or
        //   - the stack is empty, which means we have tried all possibilities.
        loop {
            let mut th = match self.stack.pop() {
                None => break,
                Some(x) => x,
            };

            loop {
                if si + 1 == self.visited[th.pc as usize] {
                    break;
                }
                self.visited[th.pc as usize] = si + 1;

                match self.prog.insts[th.pc as usize] {
                    Inst::Match => {
                        self.groups = th.groups.clone();
                        self.found_match = true;
                        return true;
                    }
                    Inst::AssertHat => {
                        if si != 0 {
                            break;
                        }
                        th.pc += 1;
                    }
                    Inst::AssertDollar => {
                        if si != slen {
                            break;
                        }
                        th.pc += 1;
                    }
                    Inst::Jump(ip) => {
                        th.pc = ip;
                    }
                    Inst::Split(ip1, ip2) => {
                        let mut th2 = th.clone();
                        th2.pc = ip2;
                        self.stack.push(th2);
                        th.pc = ip1;
                    }
                    Inst::Save(groupidx) => {
                        th.groups[groupidx as usize] = Some(si);
                        th.pc += 1;
                    }
                    _ => {
                        list.push(th);
                        break;
                    }
                }
            }
        }

        false
    }

    pub fn run(&mut self, s: &Vec<char>) -> bool {
        let mut curr: Vec<Thread> = Vec::with_capacity(self.prog.insts.len());
        let mut next: Vec<Thread> = Vec::with_capacity(self.prog.insts.len());
        let slen = s.len();
        let mut si = 0;

        self.epsilon(Thread { pc: 0, groups: Groups::default() }, si, slen, &mut curr);

        while !curr.is_empty() {
            for th in curr.iter_mut() {
                match &self.prog.insts[th.pc as usize] {
                    &Inst::Char(CharKind::Char(c)) => {
                        if si < s.len() && s[si] == c {
                            th.pc += 1;
                            if self.epsilon(*th, si + 1, slen, &mut next) {
                                break;
                            }
                        }
                    }
                    &Inst::Char(CharKind::AnyChar) => {
                        if si < s.len() {
                            th.pc += 1;
                            if self.epsilon(*th, si + 1, slen, &mut next) {
                                break;
                            }
                        }
                    }
                    _ => {
                        if self.epsilon(*th, si, slen, &mut next) {
                            break;
                        }
                    }
                }
            }
            curr.clear();
            si += 1;
            mem::swap(&mut curr, &mut next);
        }

        self.found_match
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
            for (groupidx, begin, end) in vec![$(($groupidx, $begin, $end)),+] {
                expected[groupidx * 2] = Some(begin);
                expected[groupidx * 2 + 1] = Some(end);
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
        assert_match!(r".", "a");
        assert_match!(r".a.", "xay");
        assert_match!(r"........", "whatever");
        assert_match!(r"^abc", "abcdefg");
        assert_match!(r"a*$", "aaaaaaa");
        assert_match!(r"^a*$", "aaaaaaa");
        assert_match!(r"^...$", "aaa");
        assert_match!(r"aaa", "xxxxxaaaxxxxx");
        assert_match_groups!(r"a*", "aaa", (0, 0, 3));
        assert_match_groups!(r"a*?", "aaa", (0, 0, 0));
        assert_match_groups!(r"a+", "aaa", (0, 0, 3));
        assert_match_groups!(r"a+?", "aaa", (0, 0, 1));
        assert_match_groups!(r"a?", "aaa", (0, 0, 1));
        assert_match_groups!(r"a??", "aaa", (0, 0, 0));
        assert_match_groups!(r"aaa", "xaaaxaaa", (0, 1, 4));
        assert_match_groups!(r"^aaa", "aaaxaaa", (0, 0, 3));
        assert_match_groups!(r"aaa$", "aaaxaaa", (0, 4, 7));
        assert_match_groups!(r"<.+>", "<a>b>c>", (0, 0, 7));
        assert_match_groups!(r"<.+?>", "<a>b>c>", (0, 0, 3));
        assert_match_groups!(r"(a)", "a", (0, 0, 1), (1, 0, 1));
        assert_match_groups!(r"(a)(b)", "ab", (0, 0, 2), (1, 0, 1), (2, 1, 2));
        assert_match_groups!(r"(a|b)+d", "abaabd", (0, 0, 6), (1, 4, 5));
        assert_match_groups!(r"(a(b(c)))d", "abcd", (0, 0, 4), (1, 0, 3), (2, 1, 3), (3, 2, 3));
        assert_match_groups!(r"(a(b)|c(d))e", "cde", (0, 0, 3), (1, 0, 2), (3, 1, 2));
        assert_match_groups!(r"(((((((((a)))))))))", "a", (0, 0, 1), (1, 0, 1), (2, 0, 1),
                                                          (3, 0, 1), (4, 0, 1), (5, 0, 1),
                                                          (6, 0, 1), (7, 0, 1), (8, 0, 1),
                                                          (9, 0, 1));

        // The bad
        assert_not_match!(r"a", "b");
        assert_not_match!(r"abc", "bca");
        assert_not_match!(r"a+", "");
        assert_not_match!(r"a|b|c", "x");
        assert_not_match!(r"(a|b)+", "x");
        assert_not_match!(r"^a*$", "aaaab");
        assert_not_match!(r"aaa$", "aaaxxxxx");
        assert_not_match!(r"^aaa", "xxxxxaaa");
        assert_not_match!(r"aaa", "xxxxxaaxxxx");
    }
}
