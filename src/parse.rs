use error::SyntaxError;
use error::SyntaxErrorKind::*;
use std::boxed::Box;

#[derive(Debug, PartialEq)]
pub struct Parsed {
    pub ast: Ast,
    pub hat: bool,
    pub dollar: bool,
}

#[derive(Debug, PartialEq)]
pub enum RepKind {
    Star,
    Plus,
    Question,
}

#[derive(Debug, PartialEq)]
pub struct Rep {
    pub ast: Box<Ast>,
    pub kind: RepKind,
    pub greedy: bool,
}

#[derive(Debug, PartialEq)]
pub enum Ast {
    Char(char),
    AnyChar,
    Rep(Rep),
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
    fn currchar(&self) -> char {
        self.chars[self.off]
    }

    fn nextchar(&self) -> Option<char> {
        if self.off + 1 >= self.chars.len() {
            None
        }
        else {
            Some(self.chars[self.off + 1])
        }
    }

    fn takechar(&mut self) {
        self.off += 1
    }

    fn parse_repeat(&mut self, repchar: char, greedy: bool) -> Result<(), SyntaxError> {
        match self.stack.pop() {
            Some(e @ Ast::Char(..)) |
            Some(e @ Ast::AnyChar) |
            Some(e @ Ast::Group(..)) => {
                let kind = match repchar {
                    '*' => RepKind::Star,
                    '+' => RepKind::Plus,
                    '?' => RepKind::Question,
                    _ => unreachable!("unknown repetition {}", repchar),
                };
                let rep = Rep { ast: Box::new(e), kind, greedy };
                self.stack.push(Ast::Rep(rep));
            }
            None => return Err(SyntaxError::new(self.off, NothingToRepeat)),
            _ => return Err(SyntaxError::new(self.off, CannotRepeat)),
        }
        self.takechar();
        if !greedy {
            self.takechar();
        }
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
            }
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
                }
                Some(Ast::Lparen(g)) => {
                    self.stack.push(Ast::Lparen(g));
                    self.push_alter(terms, None)?;
                    break;
                }
                Some(Ast::Alter(v)) => {
                    self.push_alter(terms, Some(v))?;
                    break;
                }
                Some(t) => terms.push(t),
            }
        }
        self.takechar();
        Ok(())
    }

    fn parse_group(&mut self) -> Result<(), SyntaxError> {
        self.takechar();
        self.parse_eof(true)?;
        let group = self.stack.pop().unwrap();
        match self.stack.pop() {
            Some(Ast::Lparen(g)) => self.stack.push(Ast::Group(g, Box::new(group))),
            _ => return Err(SyntaxError::new(self.off, UnmatchedParen)),
        }
        Ok(())
    }

    fn parse_push_esc(&mut self) -> Result<(), SyntaxError> {
        self.takechar();
        let esc =  match self.currchar() {
            'n' => '\n',
            't' => '\t',
            '^' => '^',
            '$' => '$',
            _ => return Err(SyntaxError::new(self.off, UnknownEscape)),
        };
        self.stack.push(Ast::Char(esc));
        self.takechar();
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
            }
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
                }
                Some(Ast::Alter(v)) => {
                    self.push_eof(terms, Some(v))?;
                    break;
                }
                Some(Ast::Lparen(g)) => {
                    if !subregex {
                        return Err(SyntaxError::new(self.off, UnmatchedParen));
                    }
                    self.stack.push(Ast::Lparen(g));
                    self.push_eof(terms, None)?;
                    break;
                }
                Some(t) => terms.push(t),
            }
        }
        Ok(())
    }

    fn parse_regex(&mut self) -> Result<Parsed, SyntaxError> {
        let mut hat = false;
        let mut dollar = false;

        while self.off < self.chars.len() {
            let curr = self.currchar();
            match curr {
                '\\' => self.parse_push_esc()?,
                '*' | '+' | '?' => {
                    let greedy = match self.nextchar() {
                        Some(c) if c == '?' => false,
                        Some(_) => true,
                        None => true,
                    };
                    self.parse_repeat(curr, greedy)?;
                }
                '|' => self.parse_alter()?,
                '(' => {
                    self.groupidx += 1;
                    self.stack.push(Ast::Lparen(self.groupidx));
                    self.takechar();
                }
                ')' => self.parse_group()?,
                '^' => {
                    if self.off != 0 {
                        return Err(SyntaxError::new(self.off, HatAssertPosition));
                    }
                    hat = true;
                    self.takechar();
                }
                '$' => {
                    if self.off != self.chars.len() - 1 {
                        return Err(SyntaxError::new(self.off, DollarAssertPosition));
                    }
                    dollar = true;
                    self.takechar();
                }
                '.' => {
                    self.stack.push(Ast::AnyChar);
                    self.takechar();
                }
                _ => {
                    self.stack.push(Ast::Char(curr));
                    self.takechar();
                }
            }
        }
        self.parse_eof(false)?;
        Ok(Parsed {
            ast: self.stack.pop().unwrap(),
            hat, dollar,
        })
    }

    pub fn parse(pat: &str) -> Result<Parsed, SyntaxError> {
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
        ( $re:expr, $expected:expr ) => {
            {
                let e = Parser::parse($re).unwrap_err();
                assert_eq!(e.off, $expected.off);
                assert_eq!(e.kind, $expected.kind);
            }
        };
    }

    macro_rules! ast {
        ( (| $( $t:tt ),+) ) => { Ast::Alter(vec![$( ast!($t) ),+]) };
        ( (& $( $t:tt ),+) ) => { Ast::Concat(vec![$( ast!($t) ),+]) };

        // repetitions, all are delegated to the `rep` macro
        ( (* $t:tt) )  => { ast!((rep $t, RepKind::Star, true)) };
        ( (*? $t:tt) ) => { ast!((rep $t, RepKind::Star, false)) };
        ( (+ $t:tt) )  => { ast!((rep $t, RepKind::Plus, true)) };
        ( (+? $t:tt) ) => { ast!((rep $t, RepKind::Plus, false)) };
        ( (? $t:tt) )  => { ast!((rep $t, RepKind::Question, true)) };
        ( (?? $t:tt) ) => { ast!((rep $t, RepKind::Question, false)) };

        ( (rep $ast:tt, $kind:expr, $greedy:expr) ) => {
            Ast::Rep(Rep { ast: Box::new(ast!($ast)), kind: $kind, greedy: $greedy, })
        };

        ( ($idx:tt $t:tt) )  => { Ast::Group($idx, Box::new(ast!($t))) };
        ( $c:expr )          => { Ast::Char($c) };
        ( . )                => { Ast::AnyChar };
    }

    macro_rules! assert_parse {
        ( $re:expr, $ast:expr, $hat:expr, $dollar:expr ) => {
            let parsed = Parser::parse($re).unwrap();
            assert_eq!(parsed.ast, $ast);
            assert_eq!(parsed.hat, $hat);
            assert_eq!(parsed.dollar, $dollar);
        };
    }

    #[test]
    fn test_parse() {
        // The good
        assert_parse!(r"a", ast!('a'), false, false);
        assert_parse!(r"\n", ast!('\n'), false, false);
        assert_parse!(r"ab", ast!((& 'a', 'b')), false, false);
        assert_parse!(r"a+", ast!((+ 'a')), false, false);
        assert_parse!(r"a*?", ast!((*? 'a')), false, false);
        assert_parse!(r"a+?", ast!((+? 'a')), false, false);
        assert_parse!(r"a??", ast!((?? 'a')), false, false);
        assert_parse!(r"a+b*", ast!((& (+ 'a'), (* 'b'))), false, false);
        assert_parse!(r"ab*", ast!((& 'a', (* 'b'))), false, false);
        assert_parse!(r"a?b", ast!((& (? 'a'), 'b')), false, false);
        assert_parse!(r"a+|b", ast!((| (+ 'a'), 'b')), false, false);
        assert_parse!(r"a+|b*", ast!((| (+ 'a'), (* 'b'))), false, false);
        assert_parse!(r".", ast!(.), false, false);
        assert_parse!(r"a..b", ast!((& 'a', ., ., 'b')), false, false);

        assert_parse!(r"(a)", ast!((1 'a')), false, false);
        assert_parse!(r"(ab)", ast!((1 (& 'a', 'b'))), false, false);
        assert_parse!(r"(ab)+", ast!((+ (1 (& 'a', 'b')))), false, false);
        assert_parse!(r"(a(bc)?)+", ast!((+ (1 (& 'a', (? (2 (& 'b', 'c'))))))), false, false);
        assert_parse!(r"(a+|b*|cd?)", ast!((1 (| (+ 'a'), (* 'b'), (& 'c', (? 'd'))))), false, false);
        assert_parse!(r"(a)|(b(c))", ast!((| (1 'a'), (2 (& 'b', (3 'c'))))), false, false);
        assert_parse!(r"(a)(b)", ast!((& (1 'a'), (2 'b'))), false, false);
        assert_parse!(r"(a|b)(c|d)", ast!((& (1 (| 'a', 'b')), (2 (| 'c', 'd')))), false, false);
        assert_parse!(r"(a)|(b)", ast!((| (1 'a'), (2 'b'))), false, false);
        assert_parse!(r"(((quoi?)))", ast!((1 (2 (3 (& 'q', 'u', 'o', (? 'i')))))), false, false);

        assert_parse!(r"^abc", ast!((& 'a', 'b', 'c')), true, false);
        assert_parse!(r"abc$", ast!((& 'a', 'b', 'c')), false, true);
        assert_parse!(r"^abc$", ast!((& 'a', 'b', 'c')), true, true);
        assert_parse!(r"^\^a\^c$", ast!((& '^', 'a', '^', 'c')), true, true);
        assert_parse!(r"^a\$c\$$", ast!((& 'a', '$', 'c', '$')), true, true);

        // The bad
        assert_err!(r"+", SyntaxError::new(0, NothingToRepeat));
        assert_err!(r"*", SyntaxError::new(0, NothingToRepeat));
        assert_err!(r"?", SyntaxError::new(0, NothingToRepeat));
        assert_err!(r"a**", SyntaxError::new(2, CannotRepeat));
        assert_err!(r"a|+", SyntaxError::new(2, CannotRepeat));
        assert_err!(r"a|b(+", SyntaxError::new(4, CannotRepeat));
        assert_err!(r"|", SyntaxError::new(0, MissingAlternation));
        assert_err!(r"a|", SyntaxError::new(2, MissingAlternation));
        assert_err!(r"(", SyntaxError::new(1, UnmatchedParen));
        assert_err!(r")", SyntaxError::new(1, UnmatchedParen));
        assert_err!(r"(a", SyntaxError::new(2, UnmatchedParen));
        assert_err!(r"a)", SyntaxError::new(2, UnmatchedParen));
        assert_err!(r"()", SyntaxError::new(2, EmptyRegex));
        assert_err!(r"", SyntaxError::new(0, EmptyRegex));

        assert_err!(r"a^b", SyntaxError::new(1, HatAssertPosition));
        assert_err!(r"ab^", SyntaxError::new(2, HatAssertPosition));
        assert_err!(r"$ab", SyntaxError::new(0, DollarAssertPosition));
        assert_err!(r"a$b", SyntaxError::new(1, DollarAssertPosition));

        assert_err!(r"a\b", SyntaxError::new(2, UnknownEscape));
    }
}
