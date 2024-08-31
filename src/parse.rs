use std::{borrow::Cow, fmt::Display};
use miette::{Context, Error, LabeledSpan};

use crate::{lex::{Token, TokenKind}, Lexer};

pub struct Parser<'de> {
    whole : &'de str,
    lexer : Lexer<'de>
}

pub struct Ast;

#[derive(Debug, Clone, PartialEq)]
pub enum Atom <'de>{
    String(Cow<'de,str>),
    Number(f64),
    Bool(bool),
    Nil,
    Identifier(&'de str),
    Super,
    This
}

impl<'de> Display for Atom<'de> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Atom::String(s) => write!(f , "\"{s}\""),
            Atom::Number(n) => {
                if *n == n.trunc() {
                    write!(f, "{n}.0")
                }
                else {
                    write!(f, "{n}")
                }
            },
            Atom::Nil => write!(f , "nil"),
            Atom::Super => write!(f , "super"),
            Atom::Bool(b) =>write!(f ,"{b:?}"),
            Atom::This => write!(f , "this"),
            Atom::Identifier(i) => write!(f , "{i}")
        }
    }
}
 
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Operator {
    Minus,
    Plus,
    Star,
    Bang,
    BangEqual,
    EqualEqual,
    LessEqual,
    GreaterEqual,
    Less,
    Greater,
    Slash,
    Return,
    And, 
    Class,
    For,
    Or,
    Print,
    Field,
    Var,
    While,
    Call,
    Group
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f , "{}" , match self {
            Operator::Minus => "-",
            Operator::Plus => "+",
            Operator::Star => "*",
            Operator::Bang => "!",
            Operator::BangEqual => "!=",
            Operator::EqualEqual => "==",
            Operator::LessEqual => "<=",
            Operator::GreaterEqual => ">=",
            Operator::Less => "<",
            Operator::Greater => ">",
            Operator::Slash => "/",
            Operator::Return => "return",
            Operator::And => "and",
            Operator::Class => "class",
            Operator::For => "for",
            Operator::Or => "or",
            Operator::Print => "print",
            Operator::Field => ".",
            Operator::Var => "var",
            Operator::While => "while",
            Operator::Call => "call",
            Operator::Group => "group"
        }
        )
    }
}

pub enum TokenTree<'de>{
    Atoms(Atom<'de>),
    Cons(Operator, Vec<TokenTree<'de>>),
    Fun { name : Atom<'de> , parameters : Vec<Token<'de>> , body : Box<TokenTree<'de>>},
    If {condition : Box<TokenTree<'de>> , yes : Box<TokenTree<'de>>, no : Option<Box<TokenTree<'de>>>},
    Call{callee : Box<TokenTree<'de>>, args : Vec<TokenTree<'de>>}
}

impl<'de> Display for TokenTree<'de> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenTree::Atoms(i) => write!(f, "{}", i),
            TokenTree::Cons(head, rest) => {
                write!(f, "({}", head)?;
                for s in rest {
                    write!(f, " {}", s)?
                }
                write!(f, ")")
            },
            TokenTree::Fun{name , parameters , body} => {
                write!(f, "(def {name}")?;
                for p in parameters {
                    write!(f, " {}", p)?
                }
                write!(f, "{body})")
            },
            TokenTree::Call { callee, args } => {
                write!(f, "({callee})")?;
                for p in args {
                    write!(f, " {}", p)?
                }
                write!(f, ")")
            },
            TokenTree::If { condition, yes, no } => {
                write!(f, "(if {condition} {yes}")?;
                if let Some(no) = no {
                    write!(f, "{no}")?
                }
                write!(f, ")")
            }
        }
    }
}

impl<'de> Parser<'de>{
    pub fn new(input : &'de str) -> Self{
        Self {
            whole : input,
            lexer : Lexer::new(input)
        }
    }
    
    pub fn parse_expression(mut self) -> Result<TokenTree<'de>, Error> {
        self.parse_expression_within(0)
    }

    pub fn parse(mut self) -> Result<TokenTree<'de>, Error> {
        self.parse_statement_within(0)
    }

    pub fn parse_block(&mut self) -> Result<TokenTree<'de>, Error> {

        self.lexer
                .expect(TokenKind::LeftBrace, "Missing {")?;

                let block = self.parse_statement_within(0)?;
                
                self.lexer
                .expect(TokenKind::RightBrace, "Missing }")?;

        Ok(block)
    }

    pub fn parse_fun_call_args(&mut self) -> Result<Vec<TokenTree<'de>>, Error> {
        let mut args = Vec::new();

                        // self.lexer
                        // .expect(TokenKind::LeftParen, "missing (")
                        // .wrap_err("in args list for a func call")?;

                        if matches!(self.lexer.peek() , Some(Ok(Token {
                            kind : TokenKind::RightParen,
                            ..
                        }))) {
                            // no argument list
                        }
                        else {
                            loop {
                                let arg = self
                                                .parse_expression_within(0)
                                                                    .wrap_err_with(|| format!("in argument #{} of function call", args.len() - 1))?;
        
                                args.push(arg);                                                        
        
                                let token  = self.lexer
                                                        .expect_where(|token| matches!(token.kind, TokenKind::RightParen | TokenKind::Comma), "continuing argument list")
                                                        .wrap_err_with(|| format!("in args list of function call"))?;
                                
                                if token.kind == TokenKind::RightParen {
                                    break;
                                }
        
                                
                            }
                        }

                    Ok(args)
    }

    fn parse_statement_within(&mut self, min_bp: u8) -> Result<TokenTree<'de>,Error> {
        // let looking_for_msg = move || {
        //     if let Some((op, argi)) = looking_for {
        //         format!("looking for argument #{argi} for {op:?}");
        //     }
        //     else {
        //         "looking for a statement".to_string();
        //     }
        // };
        // let x = self.lexer.next();
        let lhs = match self.lexer.next() {
            Some(Ok(token)) => token,
            None => return Ok(TokenTree::Atoms(Atom::Nil)),
            Some(Err(e)) => {
                let msg = "parsing lhs";
                return Err(e).wrap_err(msg);
           }
        };
        
        let mut lhs = match lhs {
            Token {
                kind : TokenKind::Identifier,
                origin,
                .. 
            } => TokenTree::Atoms(Atom::Identifier(origin)),
            Token {
                kind : TokenKind::Super,
                .. 
            } => TokenTree::Atoms(Atom::Super),
            Token {
                kind : TokenKind::This,
                .. 
            } => TokenTree::Atoms(Atom::This),
            Token {
                kind : TokenKind::LeftParen,
                .. 
            } => {
                let lhs = self
                                        .parse_expression_within(0)
                                        .wrap_err("in left paren expression")?;

                self.lexer.expect(TokenKind::RightParen, "Unexpected end of bracketed expression")
                .wrap_err("afte brackted expression")?;

                TokenTree::Cons(Operator::Group, vec![lhs])

            },
            Token {
                kind : TokenKind::Print | TokenKind::Return,
                .. 
            } => {

                let op = match lhs.kind {
                    TokenKind::Print => Operator::Print,
                    TokenKind::Return => Operator::Return,
                    _ => unreachable!("by the outer match pattern")
                };
                let ((), r_bp) = prefix_binding_power(op);
                
                let rhs = self
                                                    .parse_expression_within(r_bp)
                                                    .wrap_err("parse RHS")?;
                return Ok(TokenTree::Cons(op, vec![rhs]))
            },
            Token {
                kind : TokenKind::Class,
                .. 
            } => {
                
                let token = self
                                        .lexer
                                        .expect(TokenKind::Identifier, "expected indentifier")
                                        .wrap_err("in class name")?;

                
                let identifier = TokenTree::Atoms(Atom::Identifier(token.origin));


                if lhs.kind == TokenKind::Var {
                    self.lexer
                    .expect(TokenKind::Equal, "Malformed variable assignment, missing = ")
                    .wrap_err("in variable assignment")?;  
                }            

                let block = self.parse_block()
                                                        .wrap_err("in class definition")?;


                return Ok(TokenTree::Cons(Operator::Class, vec![identifier, block]))
            },
            Token {
                kind :TokenKind::Var,
                .. 
            } => {
                
                let token = self
                                        .lexer
                                        .expect(TokenKind::Identifier, "expected indentifier")
                                        .wrap_err("in first argument of Variable")?;

                
                let identifier = TokenTree::Atoms(Atom::Identifier(token.origin));


                if lhs.kind == TokenKind::Var {
                    self.lexer
                    .expect(TokenKind::Equal, "Malformed variable assignment, missing = ")
                    .wrap_err("in variable assignment")?;  
                }            

                
                let second = self
                                            .parse_expression_within(0)
                                            .wrap_err("in second variable expression")
                                            ?;


                return Ok(TokenTree::Cons(Operator::Var, vec![identifier, second]))
            },
            Token {
                kind : TokenKind::For,
                .. 
            } => {
                self.lexer
                .expect(TokenKind::LeftParen, "Missing (")
                .wrap_err("in for loop condition")?;

                let init = self.parse_expression_within(0)
                                                        .wrap_err("in init condition of for loop")?;

                self.lexer
                .expect(TokenKind::Semicolon, "Missing ;")
                .wrap_err("in for loop condition")?;

                let cond = self.parse_expression_within(0)
                                                        .wrap_err("in loop condition of for loop")?;

                self.lexer
                .expect(TokenKind::Semicolon, "Missing ;")
                .wrap_err("in for loop condition")?;

                let incr = self.parse_expression_within(0)
                                                        .wrap_err("in incr condition of for loop")?;

                self.lexer
                .expect(TokenKind::RightParen, "Missing )")
                .wrap_err("in for loop condition")?;

                let block = self.parse_block()
                .wrap_err("in loop condition of for loop")?;


                return Ok(TokenTree::Cons(Operator::For, vec![init, cond, incr, block]))   
            },
            Token {
                kind : TokenKind::While,
                .. 
            } => {
                self.lexer
                .expect(TokenKind::LeftParen, "Missing (")
                .wrap_err("in for loop condition")?;

                let cond = self.parse_expression_within(0)
                                                        .wrap_err("in loop condition of while loop")?;

                self.lexer
                .expect(TokenKind::RightParen, "Missing )")
                .wrap_err("in while loop condition")?;

                let block = self.parse_block()
                                                        .wrap_err("in loop condition of while loop")?;


                return Ok(TokenTree::Cons(Operator::While, vec![ cond, block]))   
            }
            Token {
                kind : TokenKind::Fun,
                .. 
            } => {

                let token = self
                .lexer
                .expect(TokenKind::Identifier, "expected identifier")
                .wrap_err_with(|| format!("in name of the function"))?;
                
                let name = token.origin;
                let ident = TokenTree::Atoms(Atom::Identifier(token.origin));
                let mut parameters = Vec::new();

                self.lexer
                .expect(TokenKind::LeftParen, "missing (")
                .wrap_err("in function parameter list")?;
                
                if matches!(self.lexer.peek() , Some(Ok(Token {
                    kind : TokenKind::RightParen,
                    ..
                }))) {
                    // the function has no arguments
                }
                else {
                    loop {
                        let param = self.lexer
                                                    .expect(TokenKind::Identifier, "unexpected token")
                                                            .wrap_err_with(|| format!("in parameter #{} of function name {}", parameters.len() - 1 , name))?;

                        parameters.push(param);                                                        

                        let token  = self.lexer
                                                .expect_where(|token| matches!(token.kind, TokenKind::RightBrace | TokenKind::Comma), "continuing parameter list")
                                                .wrap_err_with(|| format!("in params list of function {}", name))?;
                        
                        if token.kind == TokenKind::RightParen {
                            break;
                        }

                        
                    }
                }

                let block = self.parse_block()
                                        .wrap_err_with(|| format!("in params list of function {}", name))?;
                

                return Ok(TokenTree::Fun {
                    name : Atom::Identifier(token.origin) , 
                    parameters ,
                    body : Box::new(block)
                })
            },
            Token {
                kind : TokenKind::If,
                .. 
            } => {
                
                self.lexer
                .expect(TokenKind::LeftParen, "Missing (")
                .wrap_err("in if condition")?;

                let cond = self.parse_expression_within(0)
                                                        .wrap_err("in if condition")?;

                self.lexer
                .expect(TokenKind::RightParen, "Missing )")
                .wrap_err("in if condition")?;       

                let block = self.parse_block().wrap_err("in if condition")?;  


                let mut else_case = None;
                if matches!(self.lexer.peek() , Some(Ok(Token {
                    kind : TokenKind::Else,
                    ..
                }))) {
                    self.lexer.next();

                    else_case = Some(self.parse_block().wrap_err("in else condition")?);
                }                                 
                
                return Ok(TokenTree::If{
                    condition : Box::new(cond),
                    yes : Box::new(block),
                    no : else_case.map(Box::new)})
            },

            token => return Err(miette::miette!(
                labels = vec![
                    LabeledSpan::at(token.offset..token.offset + token.origin.len() , "here is the problem")
                ],
                help = format!("unexpected {token:?}"),
                "Expected start of expression"
                ).with_source_code(self.whole.to_string())
            )
        };


        loop {
            let op = self.lexer.peek();
            if op.map_or(false, |op| op.is_err()) {
                return Err(
                    self.lexer
                    .next()
                    .expect("Checked some above")
                    .expect_err("Checked Err above"))
                    .wrap_err("in place of expected operator");   
            }
            

            let op = match op.map(|res| res.as_ref().expect("handled err above")) {
                None => break,
                Some(Token{
                    kind : TokenKind::LeftParen,
                    ..
                }) => Operator::Call,
                Some(Token{
                    kind : TokenKind::Dot ,
                    ..
                }) => Operator::Field,

                Some(token) => {
                    return Err(miette::miette!(
                        labels = vec![
                            LabeledSpan::at(token.offset..token.offset + token.origin.len(), "here")
                        ],
                        help = format!("unexpected {token:?}"),
                        "Expected an operator"
                    ).with_source_code(self.whole.to_string())
                    )
                }
                // t => panic!("bad token {:?}", t)
            };

            if let Some((l_bp, ())) = postfix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }
                
                self.lexer.next();


                lhs = if op == Operator::Call {
                    let x = self.parse_fun_call_args().wrap_err("in func call args list")?;
                    TokenTree::Call { callee : Box::new(lhs), args : x}
                } else {
                    TokenTree::Cons(op, vec![lhs])
                };
                continue;
            }
            if let Some((l_bp, r_bp)) = infix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }
                self.lexer.next();

                    let rhs = self.parse_expression_within(r_bp).wrap_err("in the rhs")?;
                    lhs = TokenTree::Cons(op, vec![lhs, rhs]);                    
                continue;
            }
            break;
        }
        Ok(lhs)

    }
    
    fn parse_expression_within(&mut self , min_bp: u8) -> Result<TokenTree<'de>,Error> {

        // let looking_for_msg = move || {
        //     if let Some((op, argi)) = looking_for {
        //         format!("looking for argument #{argi} for {op:?}");
        //     }
        //     else {
        //         "looking for a statement".to_string();
        //     }
        // };
        // let x = self.lexer.next();
        let lhs = match self.lexer.next() {
            Some(Ok(token)) => token,
            None => return Ok(TokenTree::Atoms(Atom::Nil)),
            Some(Err(e)) => {
                let msg = "parsing lhs";
                return Err(e).wrap_err(msg);
           }
        };
        
        let mut lhs = match lhs {
            Token {
                origin,
                kind: TokenKind::String,
                ..
            } => TokenTree::Atoms(Atom::String(Token::unescape(origin))),
            Token {
                kind: TokenKind::Number(n),
                ..
            } => TokenTree::Atoms(Atom::Number(n)),
            Token {
                kind: TokenKind::False,
                ..
            } => TokenTree::Atoms(Atom::Bool(false)),
            Token {
                kind:TokenKind::True,
                ..
            } => TokenTree::Atoms(Atom::Bool(true)),
            Token {
                kind : TokenKind::Identifier,
                origin,
                ..
            } => TokenTree::Atoms(Atom::Identifier(origin)),
            Token {
                kind : TokenKind::Nil,
                ..
            } => TokenTree::Atoms(Atom::Nil),
            Token {
                kind : TokenKind::Super,
                .. 
            } => TokenTree::Atoms(Atom::Super),
            Token {
                kind : TokenKind::This,
                .. 
            } => TokenTree::Atoms(Atom::This),
            Token {
                kind : TokenKind::LeftParen,
                .. 
            } => {

                let lhs = self.parse_expression_within(0).wrap_err("group")?;


                self.lexer
                .expect(TokenKind::RightParen, "Unexpected end to bracketed expression")
                .wrap_err("after brackted expression")?;

                TokenTree::Cons(Operator::Group, vec![lhs])

            },
            Token {
                kind : TokenKind::Bang | TokenKind::Minus,
                .. 
            } => {

                let op = match lhs.kind {
                    TokenKind::Bang => Operator::Bang,
                    TokenKind::Minus => Operator::Minus,
                    _ => unreachable!("by the outer match pattern")
                };
                let ((), r_bp) = prefix_binding_power(op);
                
                let rhs = self
                                                    .parse_expression_within(r_bp)
                                                    .wrap_err("parse RHS")?;
                TokenTree::Cons(op, vec![rhs])
            },
            _ => {
                panic!("we will see")
            }

        };




        loop {
            let op = self.lexer.peek();
            if op.map_or(false, |op| op.is_err()) {
                return Err(
                    self.lexer
                    .next()
                    .expect("Checked some above")
                    .expect_err("Checked Err above"))
                    .wrap_err("in place of exptected operator");   
            }
            

            let op = match op.map(|res| res.as_ref().expect("handled err above")) {
                None => break,
                Some(Token { kind : TokenKind::RightParen | TokenKind::Comma | TokenKind::Semicolon | TokenKind::RightBrace, .. }) => break,
                Some(Token{
                    kind : TokenKind::LeftParen , .. }) => Operator::Call,
                Some(Token{
                    kind : TokenKind::BangEqual , .. }) => Operator::BangEqual,
                Some(Token{
                        kind : TokenKind::Dot , .. }) => Operator::Field,
                    Some(Token{
                        kind : TokenKind::Minus , .. }) => Operator::Minus,
                    Some(Token{
                        kind : TokenKind::Plus , .. }) => Operator::Plus,
                    Some(Token{
                    kind : TokenKind::Star , .. }) => Operator::Star,
                    Some(Token{
                    kind : TokenKind::EqualEqual , .. }) => Operator::EqualEqual,
                    Some(Token{
                    kind : TokenKind::LessEqual , .. }) => Operator::LessEqual,
                    Some(Token{
                    kind : TokenKind::GreaterEqual , .. }) => Operator::GreaterEqual,
                    Some(Token{
                    kind : TokenKind::Less , .. }) => Operator::Less,
                    Some(Token{
                    kind : TokenKind::Greater , .. }) => Operator::Greater,
                    Some(Token{
                    kind : TokenKind::Slash , .. }) => Operator::Slash,
                    Some(Token{
                    kind : TokenKind::And , .. }) => Operator::And,
                    Some(Token{
                    kind : TokenKind::Or , .. }) => Operator::Or,

                    Some(token) => return Err(miette::miette!(
                        labels = vec![
                            LabeledSpan::at(token.offset..token.offset + token.origin.len() , "here is the problem")
                        ],
                        help = format!("unexpected {token:?}"),
                        "Expected an infix operator"
                        ).with_source_code(self.whole.to_string())
                    ),
                // t => panic!("bad token {:?}", t)
            };

            if let Some((l_bp, ())) = postfix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }
                self.lexer.next();
                lhs = match op {
                    Operator::Call => TokenTree::Call{callee : Box::new(lhs), args : self.parse_fun_call_args().wrap_err("in func call args")?},
                    // op, self.parse_fun_call_args().wrap_err("in func call args"))?,
                    _ => TokenTree::Cons(op, vec![lhs])
                };

                continue;
            }
            if let Some((l_bp, r_bp)) = infix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }
                self.lexer.next();
                lhs = match op {
                    _ => {
                        let rhs = self.parse_expression_within(r_bp)
                                    .wrap_err("on the rhs")?;
                        TokenTree::Cons(op, vec![lhs,rhs])
                    }
                };
                continue;
            }
            break;
        }
        Ok(lhs)
    }

}

fn prefix_binding_power(op: Operator) -> ((), u8) {
    match op {
        Operator::Print | Operator::Return => ((), 1),
        Operator::Bang | Operator::Minus  => ((),9),
        _ => panic!("bad op: {:?}", op),
    }
}

fn postfix_binding_power(op: Operator) -> Option<(u8, ())> {
    let res = match op {
        Operator::Call => (11, ()),
        _ => return None,
    };
    Some(res)
}
fn infix_binding_power(op: Operator) -> Option<(u8, u8)> {
    let res = match op {
        // '=' => (2, 1),
        // '?' => (4, 3),
        Operator::Plus | Operator::Minus => (5, 6),
        Operator::Star | Operator::Slash => (7, 8),
        Operator::Field => (14, 13),
        _ => return None,
    };
    Some(res)
}