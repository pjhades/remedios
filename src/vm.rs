use ::Groups;
use compile::{Prog, Inst, Ip, CharKind};
use std::mem;

struct Thread {
    pc: Ip,
    groups: Groups,
}

#[derive(Debug)]
enum Action {
    Goto(Ip),
    Restore(u8, Option<usize>),
}

#[derive(Debug)]
pub struct Vm<'a> {
    // The compiled program for the regex.
    pub prog: &'a Prog,
    // Groups captured along the execution.
    pub groups: Groups,
    // `visited[ip]` is the latest string index seen by a thread executing
    // at instruction pointer `ip`. Since the string index only moves forward,
    // we only need to keep one instruction pointer for a certain string index,
    // because the execution will be the same.
    visited: Vec<usize>,
    found_match: bool,
    stack: Vec<Action>,
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

    fn follow(&mut self, t: &mut Thread, sidx: usize, slen: usize, list: &mut Vec<Thread>) -> bool {
        loop {
            if sidx + 1 <= self.visited[t.pc as usize] {
                break;
            }
            self.visited[t.pc as usize] = sidx + 1;

            match self.prog.insts[t.pc as usize] {
                Inst::Match => {
                    self.groups = t.groups;
                    self.found_match = true;
                    return true;
                }
                Inst::AssertHat => {
                    if sidx != 0 {
                        break;
                    }
                    t.pc += 1;
                }
                Inst::AssertDollar => {
                    if sidx != slen {
                        break;
                    }
                    t.pc += 1;
                }
                Inst::Jump(ip) => {
                    t.pc = ip;
                }
                Inst::Split(ip1, ip2) => {
                    self.stack.push(Action::Goto(ip2));
                    t.pc = ip1;
                }
                Inst::Save(i) => {
                    self.stack.push(Action::Restore(i, t.groups[i as usize]));
                    t.groups[i as usize] = Some(sidx);
                    t.pc += 1;
                }
                _ => {
                    list.push(Thread { pc: t.pc, groups: t.groups.clone()});
                    break;
                }
            }
        }

        false
    }

    fn epsilon(&mut self, t: &mut Thread, sidx: usize, slen: usize, list: &mut Vec<Thread>) -> bool {
        self.stack.clear();
        self.stack.push(Action::Goto(t.pc));

        loop {
            match self.stack.pop() {
                None => break,
                Some(Action::Goto(ip)) => {
                    t.pc = ip;
                    if self.follow(t, sidx, slen, list) {
                        return true;
                    }
                }
                Some(Action::Restore(i, entry)) => t.groups[i as usize] = entry,
            }
        }

        false
    }

    pub fn run(&mut self, s: &Vec<char>) -> bool {
        let mut curr = Vec::with_capacity(self.prog.insts.len());
        let mut next = Vec::with_capacity(self.prog.insts.len());
        let slen = s.len();
        let mut sidx = 0;

        let mut t = Thread { pc: 0, groups: Groups::default() };
        self.epsilon(&mut t, sidx, slen, &mut curr);

        while !curr.is_empty() {
            for t in curr.iter_mut() {
                match &self.prog.insts[t.pc as usize] {
                    &Inst::Char(CharKind::Char(c)) => {
                        if sidx < s.len() && s[sidx] == c {
                            t.pc += 1;
                            if self.epsilon(t, sidx + 1, slen, &mut next) {
                                break;
                            }
                        }
                    }
                    &Inst::Char(CharKind::AnyChar) => {
                        if sidx < s.len() {
                            t.pc += 1;
                            if self.epsilon(t, sidx + 1, slen, &mut next) {
                                break;
                            }
                        }
                    }
                    &Inst::Charset(ref cs) => {
                        if sidx < s.len() && cs.has(s[sidx]) {
                            t.pc += 1;
                            if self.epsilon(t, sidx + 1, slen, &mut next) {
                                break;
                            }
                        }
                    }
                    _ => {
                        if self.epsilon(t, sidx, slen, &mut next) {
                            break;
                        }
                    }
                }
            }
            curr.clear();
            sidx += 1;
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
        ( $re:expr, $s:expr, $( ($i:expr, $begin:expr, $end:expr) ),+ ) => {
            let prog = Compiler::compile(&Parser::parse($re).unwrap()).unwrap();
            let mut vm = Vm::new(&prog);
            assert!(vm.run(&$s.chars().collect()));

            let mut expected = Groups::default();
            for (i, begin, end) in vec![$(($i, $begin, $end)),+] {
                expected[i * 2] = Some(begin);
                expected[i * 2 + 1] = Some(end);
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
        assert_match!(r"(a*)*", "aaaaaaa");
        assert_match!(r"((a*)*)*", "aaaaaaa");
        assert_match!(r"a*a*a*a*", "aaaaaaa");
        assert_match!(r"(a?)*a*", "aaaaaaa");
        assert_match!(r"(a*)?a*", "aaaaaaa");
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
        assert_match!(r"[abc]", "a");
        assert_match!(r"[^abc]", "x");
        assert_match!(r"[a-c][a-c][a-c]", "abc");
        assert_match!(r"^[-c]+$", "c--");
        assert_match!(r"[a^c]", "^");
        assert_match!(r"[-]", "-");
        assert_match!(r"^[a-zA-Z0-9. ]+$", "Of course I still love you.");
        assert_match!(r"^[a-zA-Z0-9. ]+$", "Just read the instructions.");
        assert_match!(r"^a{3}$", "aaa");
        assert_match!(r"^a{3,}$", "aaa");
        assert_match!(r"^a{3,}$", "aaaaaaa");
        assert_match!(r"^a{,3}$", "");
        assert_match!(r"^a{,3}$", "a");
        assert_match!(r"^a{,3}$", "aaa");
        assert_match!(r"^a{3,3}$", "aaa");
        assert_match!(r"^a{3,5}$", "aaa");
        assert_match!(r"^a{3,5}$", "aaaa");
        assert_match!(r"^a{3,5}$", "aaaaa");
        assert_match!(r"^a{,}$", "");
        assert_match!(r"^a{,}$", "a");
        assert_match!(r"^a{,}$", "a");
        assert_match!(r"^[0-9a-zA-Z]{3,5}$", "0xA");
        assert_match!(r"^[0-9a-zA-Z]{3,5}$", "0xab");
        assert_match!(r"^[0-9a-zA-Z]{3,5}$", "0xa1b");

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
        assert_match_groups!(r"(a)(?:b(c)d)", "abcd", (0, 0, 4), (1, 0, 1), (2, 2, 3));
        assert_match_groups!(r"((a|b)+)d", "abaabd", (0, 0, 6), (1, 0, 5), (2, 4, 5));
        assert_match_groups!(r"(a(b)|c(d))+x", "abcdx", (0, 0, 5), (1, 2, 4), (2, 1, 2), (3, 3, 4));
        assert_match_groups!(r"(a(b(c)))d", "abcd", (0, 0, 4), (1, 0, 3), (2, 1, 3), (3, 2, 3));
        assert_match_groups!(r"(a(b)|c(d))e", "cde", (0, 0, 3), (1, 0, 2), (3, 1, 2));
        assert_match_groups!(r"(((((((((a)))))))))", "a", (0, 0, 1), (1, 0, 1), (2, 0, 1),
                                                          (3, 0, 1), (4, 0, 1), (5, 0, 1),
                                                          (6, 0, 1), (7, 0, 1), (8, 0, 1),
                                                          (9, 0, 1));
        assert_match_groups!(r"a{3,}?", "aaaaaaa", (0, 0, 3));
        assert_match_groups!(r"a{3,5}?", "aaa", (0, 0, 3));
        assert_match_groups!(r"a{3,5}?", "aaaa", (0, 0, 3));
        assert_match_groups!(r"a{3,5}?", "aaaaa", (0, 0, 3));

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
        assert_not_match!(r"a{3}", "aa");
        assert_not_match!(r"a{3,5}", "a");
        assert_not_match!(r"a{3,5}", "aa");
        assert_not_match!(r"^a{3,5}$", "aaaaaa");
    }
}
