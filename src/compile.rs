use ::GROUP_MAX;
use error::Error;
use parse::{Ast, RepKind, Parsed};
use std::fmt;

// It should be usize to be theoretically correct,
// but I need to encode the "hole" in this addr,
// and also 31-bit integer should be large enough
// to handle the majority of compiled regex instructions,
// meanwhile maitaining simplicity.
pub type Ip = i32;
pub const HOLE: Ip = -1;

#[derive(PartialEq)]
pub enum CharKind {
    Char(char),
    AnyChar,
}

impl fmt::Debug for CharKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self {
            CharKind::Char(c) => write!(f, "char {}", c),
            CharKind::AnyChar => write!(f, "anychar"),
        }
    }
}

#[derive(PartialEq)]
pub enum Inst {
    Match,
    // Avoid increasing the size of enum with `Assert(usize)`.
    AssertHat,
    AssertDollar,
    Char(CharKind),
    // The first branch has higher priority.
    Split(Ip, Ip),
    Jump(Ip),
    Save(u8),
}

#[derive(PartialEq)]
pub struct Prog {
    pub insts: Vec<Inst>,
}

impl fmt::Debug for Prog {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let mut width = 0;
        let mut n = self.insts.len();
        while n > 0 {
            width += 1;
            n >>= 4;
        }
        for (i, inst) in self.insts.iter().enumerate() {
            write!(f, "{:0w$x} ", i, w=width)?;
            match inst {
                &Inst::Match => writeln!(f, "match")?,
                &Inst::Char(ref k) => writeln!(f, "{:?}", k)?,
                &Inst::Split(x, y) => writeln!(f, "split {:0w$x} {:0w$x}", x, y, w=width)?,
                &Inst::Jump(x) => writeln!(f, "jump {:0w$x}", x, w=width)?,
                &Inst::Save(groupidx) => writeln!(f, "save {}", groupidx)?,
                &Inst::AssertHat => writeln!(f, "assert hat")?,
                &Inst::AssertDollar => writeln!(f, "assert dollar")?,
            }
        }
        Ok(())
    }
}

pub struct Compiler {
    prog: Prog,
}

#[derive(Debug)]
struct Patch {
    entry: Ip,
    holes: Vec<Ip>,
}

impl Compiler {
    fn next_ip(&self) -> Ip {
        self.prog.insts.len() as Ip
    }

    fn emit(&mut self, inst: Inst) -> Ip {
        let ip = self.next_ip();
        self.prog.insts.push(inst);
        ip
    }

    fn fill(&mut self, hole: Ip, ip: Ip) {
        match self.prog.insts[hole as usize] {
            Inst::Split(ref mut x, _) if *x == HOLE => *x = ip,
            Inst::Split(_, ref mut y) if *y == HOLE => *y = ip,
            Inst::Jump(ref mut x) => *x = ip,
            _ => (),
        }
    }

    fn compile_char(&mut self, kind: CharKind) -> Result<Patch, Error> {
        let ip = self.emit(Inst::Char(kind));
        Ok(Patch { entry: ip, holes: vec![] })
    }

    fn compile_star(&mut self, ast: &Ast, greedy: bool) -> Result<Patch, Error> {
        let inst = if greedy {
            Inst::Split(self.next_ip() + 1, HOLE)
        }
        else {
            Inst::Split(HOLE, self.next_ip() + 1)
        };
        let split = self.emit(inst);
        let patch = self.compile_ast(ast)?;
        let jump = self.emit(Inst::Jump(split));
        for hole in patch.holes {
            self.fill(hole, jump);
        }
        Ok(Patch { entry: split, holes: vec![split] })
    }

    fn compile_plus(&mut self, ast: &Ast, greedy: bool) -> Result<Patch, Error> {
        let patch = self.compile_ast(ast)?;
        let split = if greedy {
            self.emit(Inst::Split(patch.entry, HOLE))
        }
        else {
            self.emit(Inst::Split(HOLE, patch.entry))
        };
        for hole in patch.holes {
            self.fill(hole, split);
        }
        Ok(Patch { entry: patch.entry, holes: vec![split] })
    }

    fn compile_question(&mut self, ast: &Ast, greedy: bool) -> Result<Patch, Error> {
        let inst = if greedy {
            Inst::Split(self.next_ip() + 1, HOLE)
        }
        else {
            Inst::Split(HOLE, self.next_ip() + 1)
        };
        let split = self.emit(inst);
        let mut patch = self.compile_ast(ast)?;
        patch.entry = split;
        patch.holes.push(split);
        Ok(patch)
    }

    // ast | ...
    //
    //       split l1, l2
    //  l1:  <instruction for ast>
    //       jump l3
    //  l2:  <instruction for ...>
    //  l3:
    fn compile_alter(&mut self, asts: &Vec<Ast>) -> Result<Patch, Error> {
        let mut entry = HOLE;
        let mut holes = vec![];
        let mut last_split = HOLE;
        let (except_last, last) = asts.split_at(asts.len() - 1);

        for ast in except_last {
            let inst = Inst::Split(self.next_ip() + 1, HOLE);
            let split = self.emit(inst);
            if entry == HOLE {
                entry = split;
            }
            if last_split != HOLE {
                self.fill(last_split, split);
            }
            last_split = split;
            let patch = self.compile_ast(ast)?;
            let jump = self.emit(Inst::Jump(HOLE));
            holes.push(jump);
            for hole in patch.holes {
                self.fill(hole, jump);
            }
        }
 
        let mut patch = self.compile_ast(&last[0])?;
        self.fill(last_split, patch.entry);
        holes.append(&mut patch.holes);

        Ok(Patch { entry, holes })
    }

    fn compile_concat(&mut self, asts: &Vec<Ast>) -> Result<Patch, Error> {
        let mut entry = HOLE;
        let mut last_patch = Patch { entry: HOLE, holes: vec![] };
        for ast in asts {
            let patch = self.compile_ast(ast)?;
            if entry == HOLE {
                entry = patch.entry;
            }
            for hole in last_patch.holes {
                self.fill(hole, patch.entry);
            }
            last_patch = patch;
        }
        last_patch.entry = entry;
        Ok(last_patch)
    }

    fn compile_group(&mut self, groupidx: u8, ast: &Ast) -> Result<Patch, Error> {
        if groupidx > GROUP_MAX {
            return self.compile_ast(ast);
        }
        let save_begin = self.emit(Inst::Save(groupidx * 2));
        let patch = self.compile_ast(ast)?;
        let save_end = self.emit(Inst::Save(groupidx * 2 + 1));
        for hole in patch.holes {
            self.fill(hole, save_end);
        }
        Ok(Patch { entry: save_begin, holes: vec![] })
    }

    fn compile_ast(&mut self, ast: &Ast) -> Result<Patch, Error> {
        match ast {
            &Ast::Char(c) => self.compile_char(CharKind::Char(c)),
            &Ast::AnyChar => self.compile_char(CharKind::AnyChar),
            &Ast::Rep(ref rep) => {
                match rep.kind {
                    RepKind::Star => self.compile_star(&rep.ast, rep.greedy),
                    RepKind::Plus => self.compile_plus(&rep.ast, rep.greedy),
                    RepKind::Question => self.compile_question(&rep.ast, rep.greedy),
                }
            }
            &Ast::Alter(ref r) => self.compile_alter(r),
            &Ast::Concat(ref r) => self.compile_concat(r),
            &Ast::Group(idx, ref r) => self.compile_group(idx, r),
            _ => Err(Error::new("placeholder".to_string())),
        }
    }

    pub fn compile(parsed: &Parsed) -> Result<Prog, Error> {
        let mut c = Compiler {
            prog: Prog { insts: vec![] },
        };

        if parsed.hat {
            c.emit(Inst::AssertHat);
        }
        else {
            // Add .*? before the compiled code if there's no hat assertion
            c.emit(Inst::Split(3, 1));
            c.emit(Inst::Char(CharKind::AnyChar));
            c.emit(Inst::Jump(0));
        }

        c.emit(Inst::Save(0));
        let patch = c.compile_ast(&parsed.ast)?;

        // If present, the dollar assertion should come before `match`.
        let ip = if parsed.dollar {
            let x = c.emit(Inst::AssertDollar);
            c.emit(Inst::Save(1));
            x
        }
        else {
            c.emit(Inst::Save(1))
        };

        for hole in patch.holes {
            c.fill(hole, ip);
        }

        c.emit(Inst::Match);
        Ok(c.prog)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use parse::Parser;

    macro_rules! i {
        ( match ) => { Inst::Match };
        ( char $c:expr ) => { Inst::Char(CharKind::Char($c)) };
        ( anychar )      => { Inst::Char(CharKind::AnyChar) };
        ( split $x:expr, $y:expr ) => { Inst::Split($x, $y) };
        ( jump $x:expr ) => { Inst::Jump($x) };
        ( save $x:expr ) => { Inst::Save($x) };
        ( hat ) => { Inst::AssertHat };
        ( dollar ) => { Inst::AssertDollar };
    }

    macro_rules! p {
        ( $($i:expr),+ ) => {
            Prog {
                insts: vec![
                    i!(split 3, 1),
                    i!(anychar),
                    i!(jump 0),
                    $($i),+
                ]
            }
        }
    }

    macro_rules! h {
        ( $($i:expr),+ ) => {
            Prog { insts: vec![$($i),+], }
        }
    }

    macro_rules! assert_compile {
        ( $parsed:expr, $expected:expr ) => {
            let prog = Compiler::compile(&$parsed).unwrap();
            assert_eq!(prog, $expected);
        }
    }

    #[test]
    fn test_compile() {
        assert_compile!(Parser::parse(r"a").unwrap(), p! {
            i!(save 0),
            i!(char 'a'),
            i!(save 1),
            i!(match)
        });

        assert_compile!(Parser::parse(r"^a").unwrap(), h! {
            i!(hat),
            i!(save 0),
            i!(char 'a'),
            i!(save 1),
            i!(match)
        });

        assert_compile!(Parser::parse(r"a$").unwrap(), p! {
            i!(save 0),
            i!(char 'a'),
            i!(dollar),
            i!(save 1),
            i!(match)
        });

        assert_compile!(Parser::parse(r"a*$").unwrap(), p! {
            i!(save 0),
            i!(split 5, 7),
            i!(char 'a'),
            i!(jump 4),
            i!(dollar),
            i!(save 1),
            i!(match)
        });

        assert_compile!(Parser::parse(r"^a$").unwrap(), h! {
            i!(hat),
            i!(save 0),
            i!(char 'a'),
            i!(dollar),
            i!(save 1),
            i!(match)
        });

        assert_compile!(Parser::parse(r"a*").unwrap(), p! {
            i!(save 0),
            i!(split 5, 7),
            i!(char 'a'),
            i!(jump 4),
            i!(save 1),
            i!(match)
        });

        assert_compile!(Parser::parse(r"a*?").unwrap(), p! {
            i!(save 0),
            i!(split 7, 5),
            i!(char 'a'),
            i!(jump 4),
            i!(save 1),
            i!(match)
        });

        assert_compile!(Parser::parse(r"a+").unwrap(), p! {
            i!(save 0),
            i!(char 'a'),
            i!(split 4, 6),
            i!(save 1),
            i!(match)
        });

        assert_compile!(Parser::parse(r"a+?").unwrap(), p! {
            i!(save 0),
            i!(char 'a'),
            i!(split 6, 4),
            i!(save 1),
            i!(match)
        });

        assert_compile!(Parser::parse(r"a?").unwrap(), p! {
            i!(save 0),
            i!(split 5, 6),
            i!(char 'a'),
            i!(save 1),
            i!(match)
        });

        assert_compile!(Parser::parse(r"a??").unwrap(), p! {
            i!(save 0),
            i!(split 6, 5),
            i!(char 'a'),
            i!(save 1),
            i!(match)
        });

        assert_compile!(Parser::parse(r"a|b|c").unwrap(), p! {
            i!(save 0),
            i!(split 5, 7),
            i!(char 'a'),
            i!(jump 11),
            i!(split 8, 10),
            i!(char 'b'),
            i!(jump 11),
            i!(char 'c'),
            i!(save 1),
            i!(match)
        });

        assert_compile!(Parser::parse(r"abc").unwrap(), p! {
            i!(save 0),
            i!(char 'a'),
            i!(char 'b'),
            i!(char 'c'),
            i!(save 1),
            i!(match)
        });

        assert_compile!(Parser::parse(r"a...c").unwrap(), p! {
            i!(save 0),
            i!(char 'a'),
            i!(anychar),
            i!(anychar),
            i!(anychar),
            i!(char 'c'),
            i!(save 1),
            i!(match)
        });

        assert_compile!(Parser::parse(r".+").unwrap(), p! {
            i!(save 0),
            i!(anychar),
            i!(split 4, 6),
            i!(save 1),
            i!(match)
        });

        assert_compile!(Parser::parse(r"(a)(b(c))").unwrap(), p! {
            i!(save 0),
            i!(save 2),
            i!(char 'a'),
            i!(save 3),
            i!(save 4),
            i!(char 'b'),
            i!(save 6),
            i!(char 'c'),
            i!(save 7),
            i!(save 5),
            i!(save 1),
            i!(match)
        });

        assert_compile!(Parser::parse(r"((((((((((a))))))))))").unwrap(), p! {
            i!(save 0),
            i!(save 2),
            i!(save 4),
            i!(save 6),
            i!(save 8),
            i!(save 10),
            i!(save 12),
            i!(save 14),
            i!(save 16),
            i!(save 18),
            i!(char 'a'),
            i!(save 19),
            i!(save 17),
            i!(save 15),
            i!(save 13),
            i!(save 11),
            i!(save 9),
            i!(save 7),
            i!(save 5),
            i!(save 3),
            i!(save 1),
            i!(match)
        });
    }
}
