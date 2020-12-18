//
// Copyright 2020-Present (c) Raja Lehtihet & Wael El Oraiby
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// 1. Redistributions of source code must retain the above copyright notice,
// this list of conditions and the following disclaimer.
//
// 2. Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
//
// 3. Neither the name of the copyright holder nor the names of its contributors
// may be used to endorse or promote products derived from this software without
// specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//use std::io::*;

pub struct ParseError {
    message : String,
    offset  : usize
}

pub enum ParseResult<T> {
    PROk(T),
    PRErr(ParseError)
}

use ParseResult::*;

impl<T : core::cmp::PartialEq> PartialEq for ParseResult<T> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (PROk(s), PROk(o)) => *s == *o,
            (PRErr (ParseError{ message: msg1, offset: offset1 }), PRErr (ParseError{ message: msg2, offset: offset2 })) => *msg1 == *msg2 && *offset1 == *offset2,
            _ => false
        }
    }
}

#[derive(Clone)]
pub enum Exp {
    Int(i64),
    Float(f64),
    String(String),
    Symbol(String),
    List(Vec<Exp>),
}

impl PartialEq<Exp> for Exp {
    fn eq(&self, other: &Exp) -> bool {
        match (self, other) {
            (Self::Int(i0),             Self::Int(i1))      => i0 == i1,
            (Self::Float(f0),           Self::Float(f1))    => f0 == f1,
            (Self::String(s0),          Self::String(s1))   => s0 == s1,
            (Self::Symbol(s0),          Self::Symbol(s1))   => s0 == s1,
            (Self::List(s), Self::List(o)) => {
                if s.len() != o.len() { return false }
                for i in 0..s.len() {
                    if !Self::eq(&s[i], &o[i]) { return false }
                }
                true
            },
            _ => false
        }
    }
}
impl Exp {
    fn peek(src: &[char], offset: usize) -> Option<char> {

        if src.len() <= offset {
            None
        } else {
            Some(src[offset])
        }
    }

    fn getchar(src: &[char], offset: &mut usize) -> Option<char> {
        match Self::peek(src, *offset) {
            None => None,
            Some(c) => { *offset += 1; Some(c) },
        }
    }

    fn is_digit(c: char) -> bool {
        match c {
            c if c >= '0' && c <= '9' => true,
            _ => false
        }
    }

    fn is_alpha(c: char) -> bool {
        match c {
            c if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') => true,
            _ => false
        }
    }


    fn is_op(c: char) -> bool {
        match c {
            '+' | '-' | '*' | '/' | '%' | '~' | '!' | '@' | '#' | '$' | '^' | '&' | '|' | '_' | '=' | '<' | '>' | '?' | '.' | ':' | '\\' | '\'' => true,
            _ => false
        }
    }

    fn is_ws(c: char) -> bool {
        match c {
            ' ' | '\n' | '\t' => true,
            _ => false
        }
    }

    fn is_separator(c: char) -> bool {
        match c {
            '(' | ')' | '{' | '}' | ',' | '\'' | '"' => true,
            x if Self::is_ws(x) => true,
            _ => false
        }
    }

    pub fn parse_number(src: &[char], offset: &mut usize) -> ParseResult<Exp> {
        let mut s = String::new();
        loop {
            match Self::peek(src, *offset) {
                Some(c) if c == '+' || c == '-' || c == '.' || c == 'e' || c == 'E' || Self::is_digit(c) => {
                    s.push(c);
                    Self::getchar(src, offset);
                },
                Some(c) if Self::is_separator(c) => break,
                None => break,
                _ => return PRErr (ParseError { message: String::from("Unexpected end of stream (sign)"), offset: *offset })
            }
        }

        match str::parse::<i64>(s.as_str()) {
            Ok(i) => return ParseResult::PROk(Exp::Int(i)),
            _ => ()
        }

        match str::parse::<f64>(s.as_str()) {
            Ok(f) => return ParseResult::PROk(Exp::Float(f)),
            _ => ()
        }

        PRErr (ParseError { message: String::from("invalid number format"), offset: *offset })
    }

    fn parse_string(src: &[char], offset: &mut usize) -> ParseResult<String> {
        let mut s = String::new();
        match Self::peek(src, *offset) {
            Some(c) if c == '"' => (),
            _ => return PRErr (ParseError{ message: String::from("Expected \""), offset: *offset })
        }

        Self::getchar(src, offset);
        // TODO: handle '\' case
        loop {
            match Self::getchar(src, offset) {
                None => return PRErr (ParseError{ message: String::from("Unexpected end of stream (string)"), offset: *offset }),
                Some(c) if c == '"' => break,
                Some(c) => s.push(c),
            }
        }

        return PROk(s)
    }

    fn parse_symbol(src: &[char], offset: &mut usize) -> ParseResult<String> {
        let mut s = String::new();
        match Self::peek(src, *offset) {
            Some(c) if Self::is_alpha(c) || Self::is_op(c) => (),
            _ => return PRErr (ParseError{ message: String::from("Expected alpha/operator"), offset: *offset })
        }

        loop {
            match Self::peek(src, *offset) {
                Some(c) if Self::is_alpha(c) || Self::is_op(c) || Self::is_digit(c) => s.push(c),
                _ => break,
            }
            Self::getchar(src, offset);
        }

        return PROk(s)
    }

    fn skip_ws(src: &[char], offset: &mut usize) {
        loop {
            match Self::peek(src, *offset) {
                Some(c) if Self::is_ws(c) => { Self::getchar(src, offset); },
                _ => break
            }
        }
    }

    fn parse_token(src: &[char], offset: &mut usize) -> ParseResult<Exp> {
        match Self::peek(src, *offset) {
            Some(c) if c == '"' => {
                let string_res = Self::parse_string(src, offset);
                match string_res {
                    PROk(r) => PROk(Exp::String(r)),
                    PRErr(err) => PRErr(err)
                }
            },
            Some(c) if Self::is_digit(c) || ((c == '+' || c == '-') && match Self::peek(src, *offset + 1) { Some(c) if Self::is_digit(c) => true, _ => false })  => {
                let num_res = Self::parse_number(src, offset);
                match num_res {
                    PROk(r) => PROk(r),
                    PRErr(err) => PRErr(err)
                }
            },
            Some(c) if Self::is_alpha(c) || Self::is_op(c) => {
                let symbol_res = Self::parse_symbol(src, offset);
                match symbol_res {
                    PROk(r) => PROk(Exp::Symbol(r)),
                    PRErr(err) => PRErr(err)
                }
            },
            Some(c) if c == '(' => Self::parse_list(src, offset),
            Some(_) => PRErr(ParseError { message: String::from("unexpected char (token)"), offset: *offset}),
            None => PRErr(ParseError { message: String::from("unexpected end of stream (token)"), offset: *offset}),
        }
    }

    fn parse_list(src: &[char], offset: &mut usize) -> ParseResult<Exp> {
        match Self::getchar(src, offset) {
            Some(c) if c == '(' => (),
            Some(_) => return PRErr(ParseError { message: String::from("unexpected character (list)"), offset: *offset}),
            None => return PRErr(ParseError { message: String::from("unexpected end of stream (list)"), offset: *offset}),
        }

        let mut cells = Vec::new();
        loop {
            Self::skip_ws(src, offset);
            match Self::peek(src, *offset) {
                Some(c) if c == ')' => {
                    Self::getchar(src, offset);
                    return PROk(Exp::List(cells))
                },
                Some(_) => {
                    match Self::parse_token(src, offset) {
                        PROk(c) => cells.push(c),
                        PRErr(err) => return PRErr(err),
                    }
                },
                None => return PRErr(ParseError { message: String::from("unexpected end of stream (list)"), offset: *offset})
            }
        }
    }

    pub fn from_sexp(src: &str) -> ParseResult<Exp> {
        let mut offset : usize = 0;
        let v = src.chars().collect::<Vec<char>>();
        let bs = src.as_bytes();
        let src2 = v.as_slice();
        Self::skip_ws(src2, &mut offset);
        Self::parse_token(src2, &mut offset)
    }

    pub fn to_string(&self) -> String {
        match self {
            Self::Int(i) => format!("{}", i),
            Self::Float(f) => format!("{}", f),
            Self::String(s) => format!("{}", s),
            Self::Symbol(s) => s.clone(),
            Self::List(l) => {
                let mut s = String::new();
                s.push('(');
                for i in 0..l.len() {
                    s += &(l[i].to_string());
                    if i != l.len() - 1 {
                        s.push(' ');
                    }
                }
                s.push(')');
                s
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_number(s: &str) -> ParseResult<Exp> {
        let v : Vec<char> = s.chars().collect();
        let mut offset = 0;
        Exp::parse_number(&v, &mut offset)
    }

    fn parse_string(s: &str) -> ParseResult<String> {
        let v : Vec<char> = s.chars().collect();
        let mut offset = 0;
        Exp::parse_string(&v, &mut offset)
    }

    fn parse_symbol(s: &str) -> ParseResult<String> {
        let v : Vec<char> = s.chars().collect();
        let mut offset = 0;
        Exp::parse_symbol(&v, &mut offset)
    }

    #[test]
    fn test_parse_int() {
        let s = "1234";
        let res = parse_number(s);
        assert!(res == PROk(Exp::Int(1234)));

        let s = "-001234";
        let res = parse_number(s);
        assert!(res == PROk(Exp::Int(-1234)));

        let s = "-1234";
        let res = parse_number(s);
        assert!(res == PROk(Exp::Int(-1234)));

        let s = "-1234 ";
        let res = parse_number(s);
        assert!(res == PROk(Exp::Int(-1234)));

        let s = "-1234+";
        let res = parse_number(s);
        assert!(res != PROk(Exp::Int(-1234)));

        let s = "-1234a";
        let res = parse_number(s);
        assert!(res != PROk(Exp::Int(-1234)));
    }

    #[test]
    fn test_parse_float() {
        let s = "1234.";
        let res = parse_number(s);
        assert!(res == PROk(Exp::Float(1234.)));

        let s = "1234.0";
        let res = parse_number(s);
        assert!(res == PROk(Exp::Float(1234.)));

        let s = "-001234.0";
        let res = parse_number(s);
        assert!(res == PROk(Exp::Float(-1234.)));

        let s = "-1234.0";
        let res = parse_number(s);
        assert!(res == PROk(Exp::Float(-1234.)));

        let s = "-1234.0 ";
        let res = parse_number(s);
        assert!(res == PROk(Exp::Float(-1234.)));

        let s = "-1234.0+";
        let res = parse_number(s);
        assert!(res != PROk(Exp::Float(-1234.)));

        let s = "-1234.0a";
        let res = parse_number(s);
        assert!(res != PROk(Exp::Float(-1234.)));

        let s = "-001234.0E10";
        let res = parse_number(s);
        assert!(res == PROk(Exp::Float(-1234.0E10)));

        let s = "-001234.0E-10";
        let res = parse_number(s);
        assert!(res == PROk(Exp::Float(-1234.0E-10)));
    }

    #[test]
    fn test_parse_string() {
        let s = "\"1234\"";
        let res = parse_string(s);
        assert!(res == PROk(String::from("1234")));

        let s = "\"1234";
        let res = parse_string(s);
        assert!(res != PROk(String::from("1234")));
    }

    #[test]
    fn test_parse_symbol() {
        let s = "#t";
        let res = parse_symbol(s);
        assert!(res == PROk(String::from("#t")));

        let s = "t123";
        let res = parse_symbol(s);
        assert!(res == PROk(String::from("t123")));

        let s = "t123(";
        let res = parse_symbol(s);
        assert!(res == PROk(String::from("t123")));

        let s = "t123+=";
        let res = parse_symbol(s);
        assert!(res == PROk(String::from("t123+=")));

        let s = "12t123";
        let res = parse_symbol(s);
        assert!(res != PROk(String::from("12t123")));
    }

    #[test]
    fn test_parse_list() {

        let cells : [Exp; 3] = [Exp::Symbol(String::from("abcd")), Exp::Int(123), Exp::Symbol(String::from("abc"))];

        let sexp = "(abcd 123 abc)";
        let res : ParseResult<Exp> = Exp::from_sexp(sexp);
        match res {
            PROk(r) => {
                let mut v = Vec::new();
                for c in cells.iter() {
                    v.push(c.clone());
                }
                let e = Exp::List(Vec::from(v));
                assert!(Exp::eq(&e, &r))
            },
            PRErr(err) => panic!("{}", err.message)
        }
    }

    #[test]
    fn test_parse_list_string() {
        let sexp = "(abcd 123 abc)";
        let res = Exp::from_sexp(sexp);
        match res {
            PROk(r) => {
                let s = r.to_string();
                assert!(s == "(abcd 123 abc)")
            },
            PRErr(err) => panic!("{}", err.message)
        }
    }
}
