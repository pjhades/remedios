use error::Error;
use parse::Ast;
use std::fmt;

// It should be usize to be theoretically correct,
// but I need to encode the "hole" in this addr,
// and also 31-bit integer should be large enough
// to handle the majority of compiled regex instructions,
// meanwhile maitaining simplicity.
type Iaddr = i32;
const HOLE: Iaddr = -1;

#[derive(PartialEq)]
enum Inst {
    Match,
    Char(char),
    Split(Iaddr, Iaddr),
    Jump(Iaddr),
    Save(u8),
}

#[derive(PartialEq)]
struct Prog {
    insts: Vec<Inst>,
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
                &Inst::Char(c) => writeln!(f, "char {}", c)?,
                &Inst::Split(x, y) => writeln!(f, "split {:0w$x} {:0w$x}", x, y, w=width)?,
                &Inst::Jump(x) => writeln!(f, "jump {:0w$x}", x, w=width)?,
                &Inst::Save(groupidx) => writeln!(f, "save {}", groupidx)?,
            }
        }
        Ok(())
    }
}

struct Compiler {
    prog: Prog,
}

#[derive(Debug)]
struct Patch {
    entry: Iaddr,
    holes: Vec<Iaddr>,
}

impl Compiler {
    fn next_iaddr(&self) -> Iaddr {
        self.prog.insts.len() as Iaddr
    }

    fn emit(&mut self, inst: Inst) -> Iaddr {
        let iaddr = self.next_iaddr();
        self.prog.insts.push(inst);
        iaddr
    }

    fn fill(&mut self, hole: Iaddr, addr: Iaddr) {
        match self.prog.insts[hole as usize] {
            Inst::Split(ref mut x, ref mut y) => {
                if *x == HOLE {
                    *x = addr;
                }
                if *y == HOLE {
                    *y = addr;
                }
            },
            Inst::Jump(ref mut x) => {
                if *x == HOLE {
                    *x = addr;
                }
            },
            _ => (),
        }
    }

    fn compile_char(&mut self, c: char) -> Result<Patch, Error> {
        let iaddr = self.emit(Inst::Char(c));
        Ok(Patch { entry: iaddr, holes: vec![] })
    }

    fn compile_star(&mut self, ast: &Ast) -> Result<Patch, Error> {
        let inst = Inst::Split(self.next_iaddr() + 1, HOLE);
        let split = self.emit(inst);
        let patch = self.compile_ast(ast)?;
        let jump = self.emit(Inst::Jump(split));
        for hole in patch.holes {
            self.fill(hole, jump);
        }
        Ok(Patch { entry: split, holes: vec![split] })
    }

    fn compile_question(&mut self, ast: &Ast) -> Result<Patch, Error> {
        let inst = Inst::Split(self.next_iaddr() + 1, HOLE);
        let split = self.emit(inst);
        let mut patch = self.compile_ast(ast)?;
        patch.entry = split;
        patch.holes.push(split);
        Ok(patch)
    }

    fn compile_plus(&mut self, ast: &Ast) -> Result<Patch, Error> {
        let patch = self.compile_ast(ast)?;
        let split = self.emit(Inst::Split(patch.entry, HOLE));
        for hole in patch.holes {
            self.fill(hole, split);
        }
        Ok(Patch { entry: patch.entry, holes: vec![split] })
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
            let inst = Inst::Split(self.next_iaddr() + 1, HOLE);
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
            &Ast::Char(c) => self.compile_char(c),
            &Ast::Star(ref r) => self.compile_star(r),
            &Ast::Plus(ref r) => self.compile_plus(r),
            &Ast::Question(ref r) => self.compile_question(r),
            &Ast::Alter(ref r) => self.compile_alter(r),
            &Ast::Concat(ref r) => self.compile_concat(r),
            &Ast::Group(idx, ref r) => self.compile_group(idx, r),
            _ => Err(Error::new("placeholder".to_string())),
        }
    }

    pub fn compile(ast: &Ast) -> Result<Prog, Error> {
        let mut c = Compiler {
            prog: Prog { insts: vec![] },
        };
        let patch = c.compile_ast(ast)?;
        let match_addr = c.emit(Inst::Match);
        for hole in patch.holes {
            c.fill(hole, match_addr);
        }
        Ok(c.prog)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! i {
        ( match ) => { Inst::Match };
        ( char $c:expr ) => { Inst::Char($c) };
        ( split $x:expr, $y:expr ) => { Inst::Split($x, $y) };
        ( jump $x:expr ) => { Inst::Jump($x) };
        ( save $x:expr ) => { Inst::Save($x) };
    }

    macro_rules! p {
        ( $($i:expr),+ ) => {
            Prog { insts: vec![$($i),+], }
        }
    }

    macro_rules! assert_compile {
        ( $ast:expr, $expected:expr ) => {
            let prog = Compiler::compile(&$ast).unwrap();
            assert_eq!(prog, $expected);
        }
    }

    #[test]
    fn test_compile() {
        assert_compile!(ast!('a'), p! {
            i!(char 'a'),
            i!(match)
        });

        assert_compile!(ast!([* 'a']), p! {
            i!(split 1, 3),
            i!(char 'a'),
            i!(jump 0),
            i!(match)
        });

        assert_compile!(ast!([+ 'a']), p! {
            i!(char 'a'),
            i!(split 0, 2),
            i!(match)
        });

        assert_compile!(ast!([? 'a']), p! {
            i!(split 1, 2),
            i!(char 'a'),
            i!(match)
        });

        assert_compile!(ast!([| 'a', 'b', 'c']), p! {
            i!(split 1, 3),
            i!(char 'a'),
            i!(jump 7),
            i!(split 4, 6),
            i!(char 'b'),
            i!(jump 7),
            i!(char 'c'),
            i!(match)
        });

        assert_compile!(ast!([& 'a', 'b', 'c']), p! {
            i!(char 'a'),
            i!(char 'b'),
            i!(char 'c'),
            i!(match)
        });

        assert_compile!(ast!([& (1 'a'), (2 [& 'b', (3 'c')])]), p! {
            i!(save 2),
            i!(char 'a'),
            i!(save 3),
            i!(save 4),
            i!(char 'b'),
            i!(save 6),
            i!(char 'c'),
            i!(save 7),
            i!(save 5),
            i!(match)
        });
    }
}
