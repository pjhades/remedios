use error::SyntaxError;
use error::SyntaxErrorKind::*;
use std::boxed::Box;

#[derive(Debug, PartialEq)]
pub enum Ast {
    Char(char),
    Star(Box<Ast>),
    Plus(Box<Ast>),
    Question(Box<Ast>),
    Alter(Vec<Ast>),
    Concat(Vec<Ast>),
    Group(u8, Box<Ast>),
    Lparen(u8),
}

pub struct Parser {
    chars: Vec<char>,
    off: usize,
    stack: Vec<Ast>,
    groupidx: u8,
}

impl Parser {
    fn parse_repeat(&mut self, rep: char) -> Result<(), SyntaxError> {
        match self.stack.pop() {
            Some(e @ Ast::Char(..)) | Some(e @ Ast::Group(..)) => {
                let node = match rep {
                    '*' => Ast::Star(Box::new(e)),
                    '+' => Ast::Plus(Box::new(e)),
                    _ => Ast::Question(Box::new(e)),
                };
                self.stack.push(node);
            },
            None => return Err(SyntaxError::new(self.off, NothingToRepeat)),
            _ => return Err(SyntaxError::new(self.off, CannotRepeat)),
        }
        self.off += 1;
        Ok(())
    }

    fn push_alter(&mut self, mut terms: Vec<Ast>, alter: Option<Vec<Ast>>) -> Result<(), SyntaxError> {
        if terms.is_empty() {
            return Err(SyntaxError::new(self.off, MissingAlternation));
        }

        terms.reverse();
        let concat = match terms.len() {
            1 => terms.pop().unwrap(),
            _ => Ast::Concat(terms),
        };

        match alter {
            None => self.stack.push(Ast::Alter(vec![concat])),
            Some(mut v) => {
                v.push(concat);
                self.stack.push(Ast::Alter(v));
            },
        }

        Ok(())
    }

    fn parse_alter(&mut self) -> Result<(), SyntaxError> {
        let mut terms = vec![];
        loop {
            match self.stack.pop() {
                None => {
                    self.push_alter(terms, None)?;
                    break;
                },
                Some(Ast::Lparen(g)) => {
                    self.stack.push(Ast::Lparen(g));
                    self.push_alter(terms, None)?;
                    break;
                },
                Some(Ast::Alter(v)) => {
                    self.push_alter(terms, Some(v))?;
                    break;
                },
                Some(t) =>
                    terms.push(t),
            }
        }
        self.off += 1;
        Ok(())
    }

    fn parse_group(&mut self) -> Result<(), SyntaxError> {
        self.off += 1;
        self.parse_eof(true)?;
        let group = self.stack.pop().unwrap();
        match self.stack.pop() {
            Some(Ast::Lparen(g)) => self.stack.push(Ast::Group(g, Box::new(group))),
            _ => return Err(SyntaxError::new(self.off, UnmatchedParen)),
        }
        Ok(())
    }

    fn parse_push_esc(&mut self) -> Result<(), SyntaxError> {
        self.off += 1;
        let esc =  match self.chars[self.off] {
            'n' => '\n',
            't' => '\t',
            _ => return Err(SyntaxError::new(self.off, UnknownEscape)),
        };
        self.stack.push(Ast::Char(esc));
        self.off += 1;
        Ok(())
    }

    fn push_eof(&mut self, mut terms: Vec<Ast>, alter: Option<Vec<Ast>>) -> Result<(), SyntaxError> {
        if terms.is_empty() {
            match alter {
                Some(_) => return Err(SyntaxError::new(self.off, MissingAlternation)),
                None => return Err(SyntaxError::new(self.off, EmptyRegex)),
            }
        }

        terms.reverse();
        let concat = match terms.len() {
            1 => terms.pop().unwrap(),
            _ => Ast::Concat(terms),
        };

        match alter {
            None => self.stack.push(concat),
            Some(mut v) => {
                v.push(concat);
                self.stack.push(Ast::Alter(v));
            },
        }

        Ok(())
    }

    fn parse_eof(&mut self, subregex: bool) -> Result<(), SyntaxError> {
        let mut terms = vec![];
        loop {
            match self.stack.pop() {
                None => {
                    if subregex {
                        return Err(SyntaxError::new(self.off, UnmatchedParen));
                    }
                    self.push_eof(terms, None)?;
                    break;
                },
                Some(Ast::Alter(v)) => {
                    self.push_eof(terms, Some(v))?;
                    break;
                },
                Some(Ast::Lparen(g)) => {
                    if !subregex {
                        return Err(SyntaxError::new(self.off, UnmatchedParen));
                    }
                    self.stack.push(Ast::Lparen(g));
                    self.push_eof(terms, None)?;
                    break;
                },
                Some(t) => terms.push(t),
            }
        }
        Ok(())
    }

    fn parse_regex(&mut self) -> Result<Ast, SyntaxError> {
        while self.off < self.chars.len() {
            match self.chars[self.off] {
                '\\' => self.parse_push_esc()?,
                c @ '*' | c @ '+' | c @ '?' => self.parse_repeat(c)?,
                '|' => self.parse_alter()?,
                '(' => {
                    self.groupidx += 1;
                    self.stack.push(Ast::Lparen(self.groupidx));
                    self.off += 1;
                },
                ')' => self.parse_group()?,
                c @ _ => {
                    self.stack.push(Ast::Char(c));
                    self.off += 1;
                }
            }
        }
        self.parse_eof(false)?;
        Ok(self.stack.pop().unwrap())
    }

    pub fn parse(pat: &str) -> Result<Ast, SyntaxError> {
        Parser {
            chars: pat.chars().collect(),
            off: 0,
            stack: vec![],
            groupidx: 0,
        }.parse_regex()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_err {
        ( $r:expr, $expected:expr ) => {
            {
                let e = $r.unwrap_err();
                assert_eq!(e.off, $expected.off);
                assert_eq!(e.kind, $expected.kind);
            }
        };
    }

    macro_rules! ast {
        ( [ | $( $t:tt ),+ ] ) => { Ast::Alter(vec![$( ast!($t) ),+]) };
        ( [ & $( $t:tt ),+ ] ) => { Ast::Concat(vec![$( ast!($t) ),+]) };
        ( [ * $t:tt ] )        => { Ast::Star(Box::new(ast!($t))) };
        ( [ + $t:tt ] )        => { Ast::Plus(Box::new(ast!($t))) };
        ( ( $idx:tt $t:tt ) )  => { Ast::Group($idx, Box::new(ast!($t))) };
        ( [ ? $t:tt ] )        => { Ast::Question(Box::new(ast!($t))) };
        ( $c:expr )            => { Ast::Char($c) };
    }

    #[test]
    fn test_parse() {
        // The good
        assert_eq!(Parser::parse("a").unwrap(), ast!('a'));
        assert_eq!(Parser::parse("\\n").unwrap(), ast!('\n'));
        assert_eq!(Parser::parse("ab").unwrap(), ast!([& 'a', 'b']));
        assert_eq!(Parser::parse("a+").unwrap(), ast!([+ 'a']));
        assert_eq!(Parser::parse("a+b*").unwrap(), ast!([& [+ 'a'], [* 'b']]));
        assert_eq!(Parser::parse("ab*").unwrap(), ast!([& 'a', [* 'b']]));
        assert_eq!(Parser::parse("a?b").unwrap(), ast!([& [? 'a'], 'b']));
        assert_eq!(Parser::parse("a+|b").unwrap(), ast!([| [+ 'a'], 'b']));
        assert_eq!(Parser::parse("a+|b*").unwrap(), ast!([| [+ 'a'], [* 'b']]));

        assert_eq!(Parser::parse("(a)").unwrap(), ast!((1 'a')));
        assert_eq!(Parser::parse("(ab)").unwrap(), ast!((1 [& 'a', 'b'])));
        assert_eq!(Parser::parse("(ab)+").unwrap(), ast!([+ (1 [& 'a', 'b'])]));
        assert_eq!(Parser::parse("(a(bc)?)+").unwrap(), ast!([+ (1 [& 'a', [? (2 [& 'b', 'c'])]])]));
        assert_eq!(Parser::parse("(a+|b*|cd?)").unwrap(), ast!((1 [| [+ 'a'], [* 'b'], [& 'c', [? 'd']]])));
        assert_eq!(Parser::parse("(a)|(b(c))").unwrap(), ast!([| (1 'a'), (2 [& 'b', (3 'c')])]));
        assert_eq!(Parser::parse("(a)(b)").unwrap(), ast!([& (1 'a'), (2 'b')]));
        assert_eq!(Parser::parse("(a|b)(c|d)").unwrap(), ast!([& (1 [| 'a', 'b']), (2 [| 'c', 'd'])]));
        assert_eq!(Parser::parse("(a)|(b)").unwrap(), ast!([| (1 'a'), (2 'b')]));
        assert_eq!(Parser::parse("(((quoi?)))").unwrap(), ast!((1 (2 (3 [& 'q', 'u', 'o', [? 'i']])))));

        // The bad
        assert_err!(Parser::parse("+"), SyntaxError::new(0, NothingToRepeat));
        assert_err!(Parser::parse("*"), SyntaxError::new(0, NothingToRepeat));
        assert_err!(Parser::parse("?"), SyntaxError::new(0, NothingToRepeat));
        assert_err!(Parser::parse("a**"), SyntaxError::new(2, CannotRepeat));
        assert_err!(Parser::parse("a*?"), SyntaxError::new(2, CannotRepeat));
        assert_err!(Parser::parse("a|+"), SyntaxError::new(2, CannotRepeat));
        assert_err!(Parser::parse("a|b(+"), SyntaxError::new(4, CannotRepeat));
        assert_err!(Parser::parse("|"), SyntaxError::new(0, MissingAlternation));
        assert_err!(Parser::parse("a|"), SyntaxError::new(2, MissingAlternation));
        assert_err!(Parser::parse("("), SyntaxError::new(1, UnmatchedParen));
        assert_err!(Parser::parse(")"), SyntaxError::new(1, UnmatchedParen));
        assert_err!(Parser::parse("(a"), SyntaxError::new(2, UnmatchedParen));
        assert_err!(Parser::parse("a)"), SyntaxError::new(2, UnmatchedParen));
        assert_err!(Parser::parse("()"), SyntaxError::new(2, EmptyRegex));
        assert_err!(Parser::parse(""), SyntaxError::new(0, EmptyRegex));
    }
}
