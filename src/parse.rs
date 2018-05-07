use charset::Charset;
use error::SyntaxError;
use error::SyntaxErrorKind::*;
use std::boxed::Box;

#[derive(Debug, PartialEq)]
pub struct Parsed {
    pub ast: Ast,
    pub hat: bool,
    pub dollar: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub enum RepKind {
    Star,
    Question,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Rep {
    pub ast: Box<Ast>,
    pub kind: RepKind,
    pub greedy: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Ast {
    Char(char),
    Charset(Charset),
    AnyChar,
    Rep(Rep),
    Alter(Vec<Ast>),
    Concat(Vec<Ast>),
    Group(u8, Box<Ast>),
    NonCapGroup(Box<Ast>),
    Lparen(u8),
    NonCapLparen,
}

pub struct Parser {
    chars: Vec<char>,
    off: usize,
    stack: Vec<Ast>,
    paren: u8,
}

impl Parser {
    fn currchar(&self) -> Option<char> {
        if self.off >= self.chars.len() {
            None
        }
        else {
            Some(self.chars[self.off])
        }
    }

    fn nextchar(&self, off: usize) -> Option<char> {
        if self.off + off >= self.chars.len() {
            None
        }
        else {
            Some(self.chars[self.off + off])
        }
    }

    fn takechar(&mut self, off: usize) {
        self.off += off
    }

    fn parse_int(&mut self) -> Option<usize> {
        match self.currchar() {
            None => return None,
            Some(c) if !c.is_ascii_digit() => return None,
            _ => (),
        }

        let mut num = 0;
        loop {
            match self.currchar() {
                None => break,
                Some(c) if !c.is_ascii_digit() => break,
                Some(c) => {
                    self.takechar(1);
                    num = num * 10 + c.to_digit(10).unwrap();
                },
            }
        }

        Some(num as usize)
    }

    fn parse_counted_rep(&mut self) -> Result<(usize, Option<usize>), SyntaxError> {
        self.takechar(1); // take '{'

        let mut comma = false;

        let lb = match self.parse_int() {
            None => 0,
            Some(num) => num,
        };

        if let Some(',') = self.currchar() {
            comma = true;
            self.takechar(1);
        }

        let hb = match self.parse_int() {
            None if !comma => Some(lb),
            Some(hb) if lb > hb => return Err(SyntaxError::new(self.off, InvalidCountedRepetition)),
            hb @ _ => hb,
        };

        if let Some('}') = self.currchar() {
            self.takechar(1);
        }
        else {
            return Err(SyntaxError::new(self.off, InvalidCountedRepetition));
        }

        Ok((lb, hb))
    }

    fn parse_rep(&mut self) -> Result<(), SyntaxError> {
        // Check if the repetition is valid.
        let term = match self.stack.pop() {
            Some(t @ Ast::Char(..)) |
            Some(t @ Ast::Charset(..)) |
            Some(t @ Ast::AnyChar) |
            Some(t @ Ast::Group(..)) => t,
            None => return Err(SyntaxError::new(self.off, NothingToRepeat)),
            _ => return Err(SyntaxError::new(self.off, CannotRepeat)),
        };

        let (min, mut max) = match self.currchar() {
            Some(c) if c != '{' => {
                self.takechar(1);
                match c {
                    '*' => (0, None),
                    '+' => (1, None),
                    _ => (0, Some(1)),
                }
            }
            _ => self.parse_counted_rep()?
        };

        // Do not allow 0-repetition to make our life easier.
        if let (0, Some(0)) = (min, max) {
            return Err(SyntaxError::new(self.off, NothingToRepeat));
        }

        let greedy = match self.currchar() {
            Some('?') => {
                self.takechar(1);
                false
            }
            _ => true,
        };

        // If there's a lower bound for the repetition, generate a sequence
        // as if we see a concatenation of that length
        let mut concat = if min > 0 {
            if let Some(hb) = max {
                max = Some(hb - min);
            }
            Some(vec![term.clone(); min])
        }
        else {
            None
        };

        match max {
            // If there's no upper bound, generate a star here.
            None => {
                let rep = Rep { ast: Box::new(term), kind: RepKind::Star, greedy };
                match concat {
                    None => self.stack.push(Ast::Rep(rep)),
                    Some(ref mut v) => v.push(Ast::Rep(rep)),
                }
            }
            // If the repetition is deterministic like `a{3}` do nothing.
            Some(0) => (),
            // For the upper bound, generate a series of nested questions.
            Some(hb) => {
                let mut rep = Rep { ast: Box::new(term.clone()), kind: RepKind::Question, greedy };
                for _ in 0..hb - 1 {
                    rep = Rep {
                        ast: Box::new(Ast::Concat(vec![term.clone(), Ast::Rep(rep)])),
                        kind: RepKind::Question,
                        greedy,
                    };
                }
                match concat {
                    None => self.stack.push(Ast::Rep(rep)),
                    Some(ref mut v) => v.push(Ast::Rep(rep)),
                }
            }
        }

        if let Some(v) = concat {
            self.stack.push(Ast::Concat(v));
        }
        Ok(())
    }

    fn push_alter(&mut self, mut terms: Vec<Ast>, alter: Option<Vec<Ast>>)
            -> Result<(), SyntaxError> {
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
                Some(paren @ Ast::Lparen(_)) |
                Some(paren @ Ast::NonCapLparen) => {
                    self.stack.push(paren);
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
        self.takechar(1);
        Ok(())
    }

    fn parse_group(&mut self) -> Result<(), SyntaxError> {
        self.takechar(1);
        self.parse_eof(true)?;
        let group = self.stack.pop().unwrap();
        match self.stack.pop() {
            Some(Ast::Lparen(g)) => self.stack.push(Ast::Group(g, Box::new(group))),
            Some(Ast::NonCapLparen) => self.stack.push(Ast::NonCapGroup(Box::new(group))),
            _ => return Err(SyntaxError::new(self.off, UnmatchedParen)),
        }
        Ok(())
    }

    fn parse_push_esc(&mut self) -> Result<(), SyntaxError> {
        self.takechar(1);
        let esc =  match self.currchar() {
            Some('n') => '\n',
            Some('t') => '\t',
            Some('^') => '^',
            Some('$') => '$',
            _ => return Err(SyntaxError::new(self.off, UnknownEscape)),
        };
        self.stack.push(Ast::Char(esc));
        self.takechar(1);
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
                Some(paren @ Ast::Lparen(_)) |
                Some(paren @ Ast::NonCapLparen) => {
                    if !subregex {
                        return Err(SyntaxError::new(self.off, UnmatchedParen));
                    }
                    self.stack.push(paren);
                    self.push_eof(terms, None)?;
                    break;
                }
                Some(t) => terms.push(t),
            }
        }
        Ok(())
    }

    fn parse_paren(&mut self) -> Result<(), SyntaxError> {
        if let Some('?') = self.nextchar(1) {
            match self.nextchar(2) {
                Some(':') => {
                    self.stack.push(Ast::NonCapLparen);
                    self.takechar(3); // take '(' '?' ':'
                }
                _ => return Err(SyntaxError::new(self.off + 2, UnknownGroupExt)),
            }
        }
        else {
            self.paren += 1;
            self.stack.push(Ast::Lparen(self.paren));
            self.takechar(1); // take '('
        }
        Ok(())
    }

    fn parse_charset(&mut self) -> Result<(), SyntaxError> {
        self.takechar(1); // take '['

        let mut complemented = false;
        let mut cs = Charset::new();

        if let Some('^') = self.currchar() {
            complemented = true;
            self.takechar(1);
        }

        let mut prevchar = match self.currchar() {
            Some(c @ '-') | Some(c @ ']') => {
                cs.add(c);
                self.takechar(1);
                Some(c)
            }
            _ => None,
        };
        let mut pending_range = false;
        let mut escape = false;

        loop {
            match self.currchar() {
                None => return Err(SyntaxError::new(self.off, UnterminatedCharset)),
                Some(']') if !escape => break,
                Some('-') if !escape => {
                    if let Some(']') = self.nextchar(1) {
                        cs.add('-');
                        self.takechar(1);
                        continue;
                    }
                    if let None = prevchar {
                        return Err(SyntaxError::new(self.off, InvalidCharacterRange));
                    }
                    pending_range = true;
                    self.takechar(1);
                }
                Some('\\') if !escape => {
                    escape = true;
                    self.takechar(1);
                }
                Some(c @ _) => {
                    if !cs.add(c) {
                        return Err(SyntaxError::new(self.off, NonAsciiNotSupported));
                    }
                    if pending_range {
                        let lb = prevchar.unwrap();
                        if lb as u8 > c as u8 {
                            return Err(SyntaxError::new(self.off, InvalidCharacterRange));
                        }
                        for byte in lb as u8 + 1 .. c as u8 {
                            cs.add(byte as char);
                        }
                        pending_range = false;
                        prevchar = None;
                    }
                    else {
                        prevchar = Some(c);
                    }
                    escape = false;
                    self.takechar(1);
                }
            }
        }

        if complemented {
            cs.complement();
        }

        self.takechar(1); // take ']'
        self.stack.push(Ast::Charset(cs));
        Ok(())
    }

    fn parse_regex(&mut self) -> Result<Parsed, SyntaxError> {
        let mut hat = false;
        let mut dollar = false;

        while self.off < self.chars.len() {
            let curr = self.currchar().unwrap();
            match curr {
                '\\' => self.parse_push_esc()?,
                '*' | '+' | '?' | '{' => self.parse_rep()?,
                '|' => self.parse_alter()?,
                '(' => self.parse_paren()?,
                ')' => self.parse_group()?,
                '[' => self.parse_charset()?,
                '^' => {
                    if self.off != 0 {
                        return Err(SyntaxError::new(self.off, HatAssertPosition));
                    }
                    hat = true;
                    self.takechar(1);
                }
                '$' => {
                    if self.off != self.chars.len() - 1 {
                        return Err(SyntaxError::new(self.off, DollarAssertPosition));
                    }
                    dollar = true;
                    self.takechar(1);
                }
                '.' => {
                    self.stack.push(Ast::AnyChar);
                    self.takechar(1);
                }
                _ => {
                    self.stack.push(Ast::Char(curr));
                    self.takechar(1);
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
            paren: 0,
        }.parse_regex()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_err {
        ( $re:expr, $expected:expr ) => {
            {
                let e = Parser::parse($re);
                assert!(e.is_err());
                let e = e.unwrap_err();
                assert_eq!(e.off, $expected.off);
                assert_eq!(e.kind, $expected.kind);
            }
        };
    }

    macro_rules! ast {
        ( (| $( $t:tt ),+) ) => { Ast::Alter(vec![$( ast!($t) ),+]) };
        ( (& $( $t:tt ),+) ) => { Ast::Concat(vec![$( ast!($t) ),+]) };

        // repetitions, all are delegated to the `rep` macro
        ( (* $t:tt) )    => { ast!((rep $t, RepKind::Star, true)) };
        ( (*? $t:tt) )   => { ast!((rep $t, RepKind::Star, false)) };
        ( (+ $t:tt) )    => { ast!((& $t, (* $t))) };
        ( (+? $t:tt) )   => { ast!((& $t, (*? $t))) };
        ( (? $t:tt) )    => { ast!((rep $t, RepKind::Question, true)) };
        ( (?? $t:tt) )   => { ast!((rep $t, RepKind::Question, false)) };

        ( (rep $ast:tt, $kind:expr, $greedy:expr) ) => {
            Ast::Rep(Rep { ast: Box::new(ast!($ast)), kind: $kind, greedy: $greedy, })
        };

        ( (cs $($c:expr),+ ) )  => { Ast::Charset(Charset::from_chars(&[$($c),+])) };
        ( (!cs $($c:expr),+ ) ) => { Ast::Charset(Charset::from_chars_complement(&[$($c),+])) };

        ( ($idx:tt $t:tt) )    => { Ast::Group($idx, Box::new(ast!($t))) };
        ( ( $t:tt ) )          => { Ast::NonCapGroup(Box::new(ast!($t))) };
        ( $c:expr )            => { Ast::Char($c) };
        ( . )                  => { Ast::AnyChar };
    }

    macro_rules! assert_parse {
        ( $re:expr, $ast:expr, $hat:expr, $dollar:expr ) => {
            let parsed = Parser::parse($re);
            assert!(parsed.is_ok());
            let parsed = parsed.unwrap();
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
        assert_parse!(r"(?:abc)", ast!(((& 'a', 'b', 'c'))), false, false);
        assert_parse!(r"(?:ab(c))", ast!(((& 'a', 'b', (1 'c')))), false, false);
        assert_parse!(r"[abc]", ast!((cs 'a', 'b', 'c')), false, false);
        assert_parse!(r"[^abc]", ast!((!cs 'a', 'b', 'c')), false, false);
        assert_parse!(r"[a-c]", ast!((cs 'a', 'b', 'c')), false, false);
        assert_parse!(r"[a-ca-e]", ast!((cs 'a', 'b', 'c', 'd', 'e')), false, false);
        assert_parse!(r"[a-cx-z]", ast!((cs 'a', 'b', 'c', 'x', 'y', 'z')), false, false);
        assert_parse!(r"[a-ckkkx-z]", ast!((cs 'a', 'b', 'c', 'k', 'x', 'y', 'z')), false, false);
        assert_parse!(r"[]a]", ast!((cs 'a', ']')), false, false);
        assert_parse!(r"[a\]b]", ast!((cs 'a', 'b', ']')), false, false);
        assert_parse!(r"[a^b]", ast!((cs 'a', 'b', '^')), false, false);
        assert_parse!(r"[ab-]", ast!((cs 'a', 'b', '-')), false, false);
        assert_parse!(r"[-ab]", ast!((cs 'a', 'b', '-')), false, false);
        assert_parse!(r"[a\-b]", ast!((cs 'a', 'b', '-')), false, false);
        assert_parse!(r"[-\--]", ast!((cs '-')), false, false);
        assert_parse!(r"[-\-.]", ast!((cs '-', '.')), false, false);
        assert_parse!(r"[^--]", ast!((!cs '-')), false, false);
        assert_parse!(r"[^]-]", ast!((!cs '-', ']')), false, false);
        assert_parse!(r"[-a\-c^0\-3-]", ast!((cs '-', 'a', 'c', '^', '0', '3')), false, false);
        assert_parse!(r"[^a\-c^0\-3-]", ast!((!cs '-', 'a', 'c', '^', '0', '3')), false, false);
        assert_parse!(r"[\[\]\\\-\a]", ast!((cs 'a', '\\', '[', ']', '-')), false, false);
        assert_parse!(r"a{,}", ast!((* 'a')), false, false);
        assert_parse!(r"a{,}?", ast!((*? 'a')), false, false);
        assert_parse!(r"a{3}", ast!((& 'a', 'a', 'a')), false, false);
        assert_parse!(r"a{3}?", ast!((& 'a', 'a', 'a')), false, false);
        assert_parse!(r"a{3,}", ast!((& 'a', 'a', 'a', (* 'a'))), false, false);
        assert_parse!(r"a{3,}?", ast!((& 'a', 'a', 'a', (*? 'a'))), false, false);
        assert_parse!(r"a{,3}", ast!((? (& 'a', (? (& 'a', (? 'a')))))), false, false);
        assert_parse!(r"a{,3}?", ast!((?? (& 'a', (?? (& 'a', (?? 'a')))))), false, false);
        assert_parse!(r"a{1,3}", ast!((& 'a', (? (& 'a', (? 'a'))))), false, false);
        assert_parse!(r"a{1,3}?", ast!((& 'a', (?? (& 'a', (?? 'a'))))), false, false);

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
        assert_err!(r"(?abc)", SyntaxError::new(2, UnknownGroupExt));
        assert_err!(r"(?:)", SyntaxError::new(4, EmptyRegex));
        assert_err!(r"[]", SyntaxError::new(2, UnterminatedCharset));
        assert_err!(r"[a-b-c]", SyntaxError::new(4, InvalidCharacterRange));
        assert_err!(r"[z-a]", SyntaxError::new(3, InvalidCharacterRange));
        assert_err!(r"[a\", SyntaxError::new(3, UnterminatedCharset));
        assert_err!(r"[虎]", SyntaxError::new(1, NonAsciiNotSupported));
        assert_err!(r"a{", SyntaxError::new(2, InvalidCountedRepetition));
        assert_err!(r"a{3b", SyntaxError::new(3, InvalidCountedRepetition));
        assert_err!(r"a{b", SyntaxError::new(2, InvalidCountedRepetition));
        assert_err!(r"a{3,b", SyntaxError::new(4, InvalidCountedRepetition));
        assert_err!(r"a{5,3}", SyntaxError::new(5, InvalidCountedRepetition));
        assert_err!(r"a{}", SyntaxError::new(3, NothingToRepeat));
        assert_err!(r"a{}?", SyntaxError::new(3, NothingToRepeat));
        assert_err!(r"a{0}", SyntaxError::new(4, NothingToRepeat));
        assert_err!(r"a{0}?", SyntaxError::new(4, NothingToRepeat));
        assert_err!(r"a{0,0}", SyntaxError::new(6, NothingToRepeat));
        assert_err!(r"a{0,0}?", SyntaxError::new(6, NothingToRepeat));
    }
}
