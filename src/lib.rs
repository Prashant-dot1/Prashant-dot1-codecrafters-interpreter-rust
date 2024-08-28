use std::{borrow::Cow, fmt::Display};
use miette::{Error, Diagnostic, SourceSpan,LabeledSpan};
use thiserror::Error;

#[derive(Diagnostic, Debug, Error)]
#[error("Unexpected token {token} in input")]
#[diagnostic(code(my_lib::random_error))]
pub struct InternalTokenError {
    // The `Source` that miette will use.
    #[source_code]
    pub src: String,

    // This will underline/mark the specific code inside the larger
    // snippet context.
    #[label = "This input character"]
    err_span: SourceSpan,

    pub token : char
}

impl InternalTokenError {
    pub fn line(&self) -> usize {
        let till_the_error_line = &self.src[..=self.err_span.offset()];
        return till_the_error_line.lines().count();
    }
}

#[derive(Debug, PartialEq,Clone, Copy)]
pub struct Token<'de> {
    origin : &'de str,
    kind : TokenKind
}

#[derive(Clone, PartialEq , Copy, Debug)]
pub enum TokenKind {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Star,
    Semicolon,
    Bang,
    Equal,
    BangEqual,
    EqualEqual,
    LessEqual,
    GreaterEqual,
    Less,
    Greater,
    Slash,
    String,
    Identifier,
    Number(f64),
    And, 
    Class, 
    Else,
    False, 
    For,
    Fun,
    If,
    Nil,
    Or,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Eof  
}

impl<'de> Display for Token<'de> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let origin = self.origin;
        match self.kind {
            TokenKind::LeftParen => write!(f,"LEFT_PAREN {origin} null"),
            TokenKind::RightParen => write!(f, "RIGHT_PAREN {origin} null"),
            TokenKind::LeftBrace => write!(f, "LEFT_BRACE {origin} null"),
            TokenKind::RightBrace => write!(f, "RIGHT_BRACE {origin} null"),
            TokenKind::Comma => write!(f, "COMMA {origin} null"),
            TokenKind::Dot => write!(f, "DOT {origin} null"),
            TokenKind::Minus => write!(f, "MINUS {origin} null"),
            TokenKind::Plus => write!(f, "PLUS {origin} null"),
            TokenKind::Star => write!(f, "STAR {origin} null"),
            TokenKind::Semicolon => write!(f, "SEMICOLON {origin} null"),
            TokenKind::Bang => write!(f,"BANG {origin} null"),
            TokenKind::BangEqual => write!(f,"BANG_EQUAL {origin} null"),
            TokenKind::Equal => write!(f,"EQUAL {origin} null"),
            TokenKind::EqualEqual => write!(f,"EQUAL_EQUAL {origin} null"),
            TokenKind::LessEqual => write!(f,"LESS_EQUAL {origin} null"),
            TokenKind::GreaterEqual => write!(f,"GREATER_EQUAL {origin} null"),
            TokenKind::Less => write!(f,"LESS {origin} null"),
            TokenKind::Greater => write!(f,"GREATER {origin} null"),
            TokenKind::Slash => write!(f,"SLASH {origin} null"),
            TokenKind::String => write!(f, "STRING {origin} {}", Token::unescape(origin)),
            TokenKind::Identifier => write!(f, "IDENTIFIER {origin} null"),
            TokenKind::Number(n) => write!(f, "NUMBER {origin} {n}"),
            TokenKind::And => write!(f, "AND {origin} null"),
            TokenKind::Class => write!(f, "CLASS {origin} null"),
            TokenKind::Else => write!(f, "ELSE {origin} null"),
            TokenKind::False => write!(f, "FALSE {origin} null"),
            TokenKind::For => write!(f, "FOR {origin} null"),
            TokenKind::Fun => write!(f, "FUN {origin} null"),
            TokenKind::If => write!(f, "IF {origin} null"),
            TokenKind::Nil => write!(f, "NIL {origin} null"),
            TokenKind::Or => write!(f, "OR {origin} null"),
            TokenKind::Return => write!(f, "RETURN {origin} null"),
            TokenKind::Super => write!(f, "SUPER {origin} null"),
            TokenKind::This => write!(f, "THIS {origin} null"),
            TokenKind::True => write!(f, "TRUE {origin} null"),
            TokenKind::Var => write!(f, "VAR {origin} null"),
            TokenKind::While => write!(f, "WHILE {origin} null"),
            TokenKind::Eof => write!(f, "EOF {origin}null"),
        }
    }
}

impl Token<'_>{
    fn unescape<'de>(s : &'de str) -> Cow<'de , str> {
        todo!()
    }
}


pub struct Lexer<'de> {
    whole : &'de str,
    rest : &'de str,
    byte : usize
}

impl<'de> Lexer<'de> {
    pub fn new(input : &'de str) -> Self {
        Self {
            whole : input,
            rest : input,
            byte : 0
        }
    }
}

impl<'de> Iterator for Lexer<'de> {
    type Item = Result<Token<'de>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        
        loop {
            let mut chars = self.rest.chars();    
            let c = chars.next()?;
            let c_onwards = self.rest;
            let c_str = &self.rest[..c.len_utf8()];

            self.rest  = chars.as_str();
            self.byte += c.len_utf8();

            enum Started {
                String,
                Number,
                Identifier,
                IfEqualElse(TokenKind, TokenKind),
                Slash
            }

            let just = move |kind : TokenKind| {
                Some(Ok(Token{
                    kind,
                    origin : c_str
                }))
            };

            let started = match c {
                '(' =>  return just(TokenKind::LeftParen),
                ')' =>  return just(TokenKind::RightParen),
                '{' =>  return just(TokenKind::LeftBrace),
                '}' =>  return just(TokenKind::RightBrace),
                ',' =>  return just(TokenKind::Comma),
                '.' =>  return just(TokenKind::Dot),
                '-' =>  return just(TokenKind::Minus),
                '+' =>  return just(TokenKind::Plus),
                ';' =>  return just(TokenKind::Semicolon),
                '*' =>  return just(TokenKind::Star),
                '/' =>  Started::Slash,
                '<' =>  Started::IfEqualElse(TokenKind::LessEqual, TokenKind::Less),
                '>' =>  Started::IfEqualElse(TokenKind::GreaterEqual, TokenKind::Greater),
                '!' =>  Started::IfEqualElse(TokenKind::BangEqual, TokenKind::Bang),
                '=' => Started::IfEqualElse(TokenKind::EqualEqual, TokenKind::Equal),
                '"' =>  Started::String,
                '0'..='9' => Started::Number,
                'a'..='z' | 'A'..='Z' | '_' => Started::Identifier,
                c if c.is_whitespace() => continue,
                c => return Some(Err(InternalTokenError{
                    src: self.whole.to_string(),
                    err_span: SourceSpan::from(self.byte - c.len_utf8()..self.byte),
                    token: c,
                }.into()
            ))
            };

            match started {
                Started::String => todo!(),
                Started::Slash => {
                    if self.rest.starts_with('/') {
                        let lineend_pos = self.rest.find('\n').unwrap_or_else(|| self.rest.len());
                        self.byte += lineend_pos;
                        self.rest = &self.rest[lineend_pos..];
                        continue;
                    }
                    else {
                        return Some(Ok(Token{
                            origin : c_str,
                            kind : TokenKind::Slash
                        }));
                    }
                },
                Started::Number => {
                    // find the fist non digit , if everything seems ok we store the len of the str slice
                    let first_non_digit = c_onwards
                                                        .find(|c| !matches!(c , '.' | '_' | '0'..='9'))
                                                        .unwrap_or_else(|| c_onwards.len());
                    
                    let mut literal = &c_onwards[..first_non_digit];
                    let mut dotted = literal.splitn(3, '.');

                    match (dotted.next() , dotted.next() , dotted.next()) {
                        (Some(one), Some(two), Some(_)) => {
                            literal = &literal[..one.len() + 1 + two.len()];
                        },
                        (Some(one), Some(two) , None) if two.is_empty() => {
                            literal = &literal[..one.len()];
                        },
                        _ => {

                        } 
                    }

                    self.byte += literal.len() - c.len_utf8();
                    self.rest = &self.rest[literal.len() - c.len_utf8()..];

                    let n = match literal.parse() {
                        Ok(n) => n,
                        Err(e) => {
                            return Some(Err(miette::miette!(
                                    labels = vec![
                                    LabeledSpan::at(self.byte - literal.len()..self.byte, "this numeric litearal"),
                                ],
                                "{e:?}",
                            ).with_source_code(self.whole.to_string())));
                        }
                    };

                    return Some(Ok(Token{
                        origin : literal,
                        kind : TokenKind::Number(n)
                    }))
                },
                Started::Identifier => {
                    let first_non_identifier = c_onwards
                                                        .find(|c| !matches!(c , 'a'..='z' | 'A'..='Z' | '0'..='9' | '_'))
                                                        .unwrap_or_else(|| c_onwards.len());
                    
                    let mut literal = &c_onwards[..first_non_identifier];
                    // let mut dotted = literal.splitn(3, '.');

                    // if let (Some(one), Some(two), Some(three)) = (dotted.next() , dotted.next() , dotted.next()) {
                    //     literal = &literal[..one.len() + 1 + two.len()];
                    // }

                    self.byte += literal.len() - c.len_utf8();
                    self.rest = &self.rest[literal.len() - c.len_utf8()..];

                    // let n = match literal.parse() {
                    //     Ok(n) => n,
                    //     Err(e) => {
                    //         return Some(Err(miette::miette!(
                    //                 labels = vec![
                    //                 LabeledSpan::at(self.byte - literal.len()..self.byte, "this numeric litearal"),
                    //             ],
                    //             "{e:?}",
                    //         ).with_source_code(self.whole.to_string())));
                    //     }
                    // };
                    
                    let kind = match literal {
                        "and" => TokenKind::And, 
                        "class" => TokenKind::Class, 
                        "else" => TokenKind::Else,
                        "false," => TokenKind::False, 
                        "for" => TokenKind::For,
                        "fun" => TokenKind::Fun,
                        "if" => TokenKind::If,
                        "nil" => TokenKind::Nil,
                        "or" => TokenKind::Or,
                        "return" => TokenKind::Return,
                        "super" => TokenKind::Super,
                        "this" => TokenKind::This,
                        "true" => TokenKind::True,
                        "var" => TokenKind::Var,
                        "while" => TokenKind::While,
                        _ => TokenKind::Identifier
                    };

                    return Some(Ok(Token{
                        origin : literal,
                        kind : kind
                    }))
                },
                Started::IfEqualElse(yes ,no ) => {
                    self.rest = self.rest.trim_start();
                    let trimmed = c_onwards.len() - self.rest.len() - 1;
                    self.byte += trimmed;

                    if self.rest.starts_with("=") {

                        let span = &c_onwards[..c.len_utf8() + trimmed + 1];
                        self.rest = &self.rest[1..];
                        self.byte +=1;
                        return Some(Ok(Token{
                            origin : span,
                            kind : yes
                        }));
                    }
                    else{
                        return Some(Ok(Token {
                            origin : c_str,
                            kind : no
                        }));
                    }
                }
            }
        }

        todo!()
    }
}