use std::fmt;

#[derive(Debug, PartialEq)]
pub enum SyntaxErrorKind {
    NothingToRepeat,
    CannotRepeat,
    MissingAlternation,
    UnmatchedParen,
    UnknownEscape,
    EmptyRegex,
    HatAssertPosition,
    DollarAssertPosition,
    UnknownGroupExt,
    NonAsciiNotSupported,
    UnterminatedCharset,
    InvalidCharacterRange,
    InvalidCountedRepetition,
}

#[derive(Debug)]
pub struct Error {
    msg: String
}

#[derive(Debug)]
pub struct SyntaxError {
    pub off: usize,
    pub kind: SyntaxErrorKind,
}

impl Error {
    pub fn new(msg: String) -> Self {
        Error { msg }
    }
}

impl SyntaxError {
    pub fn new(off: usize, kind: SyntaxErrorKind) -> Self {
        SyntaxError { off, kind }
    }
}

impl From<SyntaxError> for Error {
    fn from(e: SyntaxError) -> Self {
        Error::new(format!("syntax error at {}: {}", e.off, match e.kind {
            SyntaxErrorKind::NothingToRepeat => "nothing to repeat",
            SyntaxErrorKind::CannotRepeat => "cannot repeat",
            SyntaxErrorKind::MissingAlternation => "missing alternation",
            SyntaxErrorKind::UnmatchedParen => "unmatched parenthesis",
            SyntaxErrorKind::UnknownEscape => "unknown escape sequence",
            SyntaxErrorKind::EmptyRegex => "empty regex",
            SyntaxErrorKind::HatAssertPosition => "hat assertion can only occur at the beginning",
            SyntaxErrorKind::DollarAssertPosition => "dollar assertion can only occur at the end",
            SyntaxErrorKind::UnknownGroupExt => "unknown capturing group extension",
            SyntaxErrorKind::NonAsciiNotSupported => "non ascii characters are not supported",
            SyntaxErrorKind::UnterminatedCharset => "unterminated character set",
            SyntaxErrorKind::InvalidCharacterRange => "invalid character range",
            SyntaxErrorKind::InvalidCountedRepetition => "invalid counted repetition",
        }))
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.msg)
    }
}
