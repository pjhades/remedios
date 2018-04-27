pub mod charset;
pub mod error;
pub mod parse;
pub mod compile;
pub mod vm;

use error::Error;
use vm::Vm;
use compile::Compiler;
use parse::Parser;
use std::fmt;

pub const NGROUPS: u8 = 10;

pub type Groups = [Option<usize>; NGROUPS as usize * 2];

#[derive(PartialEq)]
pub enum MatchResult {
    NotMatch,
    Match(Groups),
}

impl fmt::Debug for MatchResult {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &MatchResult::NotMatch => write!(f, "not match")?,
            &MatchResult::Match(ref groups) => {
                writeln!(f, "captures:")?;
                for (i, g) in groups.chunks(2).enumerate() {
                    match (g[0], g[1]) {
                        (Some(begin), Some(end)) => writeln!(f, "{}: {},{}", i, begin, end)?,
                        _ => (),
                    }
                }
            },
        }
        Ok(())
    }
}

pub fn re_match(re: &str, s: &str) -> Result<MatchResult, Error> {
    let prog = Compiler::compile(&Parser::parse(re)?)?;
    let mut vm = Vm::new(&prog);
    if vm.run(&s.chars().collect()) {
        return Ok(MatchResult::Match(vm.groups.clone()));
    }
    Ok(MatchResult::NotMatch)
}
