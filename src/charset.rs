use std::{cmp, fmt};

#[derive(Copy, Clone)]
pub struct Charset {
    set: [bool; 128],
}

impl fmt::Debug for Charset {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "[")?;
        for c in 0x00_u8 .. 0x80_u8 {
            if self.set[c as usize] {
                write!(f, "{}", c as char)?;
            }
        }
        write!(f, "]")
    }
}

impl cmp::PartialEq for Charset {
    fn eq(&self, other: &Charset) -> bool {
        for i in 0..self.set.len() {
            if self.set[i] != other.set[i] {
                return false;
            }
        }
        true
    }
}

impl Charset {
    pub fn new() -> Self {
        Charset { set: [false; 128] }
    }

    pub fn from_chars(chars: &[char]) -> Self {
        let mut cs = Charset { set: [false; 128] };
        for c in chars {
            cs.add(*c);
        }
        cs
    }

    pub fn from_chars_complement(chars: &[char]) -> Self {
        let mut cs = Charset { set: [false; 128] };
        for c in chars {
            cs.add(*c);
        }
        cs.complement();
        cs
    }

    pub fn add(&mut self, c: char) -> bool {
        if !c.is_ascii() || c.len_utf8() != 1 {
            return false;
        }
        self.set[c as usize] = true;
        true
    }

    pub fn has(&self, c: char) -> bool {
        if !c.is_ascii() || c.len_utf8() != 1 {
            return false;
        }
        self.set[c as usize]
    }

    pub fn complement(&mut self) {
        for x in self.set.iter_mut() {
            *x = !*x;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_charset_acsii() {
        let mut cs = Charset::new();
        for c in 0x00_u8 .. 0x80_u8 {
            assert!(cs.add(c as char));
            assert!(cs.has(c as char));
        }
        assert!(!cs.add('虎'));
        assert!(!cs.has('虎'));
        assert!(!cs.add(0xfe as char));
        assert!(!cs.has(0xfe as char));
    }
}
