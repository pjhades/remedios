mod error;
mod parse;
mod compile;
mod vm;

use error::Error;
use vm::{Vm, Groups, Group};
use compile::Compiler;
use parse::Parser;
use std::fmt;

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
                for (groupidx, group) in groups.iter().enumerate() {
                    if let &Some(Group { begin, end }) = group {
                        writeln!(f, "{}: {}, {}", groupidx, begin, end)?;
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
