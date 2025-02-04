use super::{
    ast::{Parseable, Visitable, Visitor},
    ParseError,
};
use crate::lexer::lexing_utils::{Keyword, Token, TokenStream, TokenType};
use anyhow::Result;
use std::fmt::Display;

#[derive(Default, Debug, Clone)]
pub struct Expr {
    pub assignment: Assignment,
}

#[derive(Debug, Clone)]
pub enum Assignment {
    Assignment(String, Box<Assignment>),
    Or(LogicOr),
}

#[derive(Default, Debug, Clone)]
pub struct LogicOr {
    pub lhs: LogicAnd,
    pub rhs: Vec<LogicAnd>,
}

#[derive(Debug, Clone, Default)]
pub struct LogicAnd {
    pub lhs: Equality,
    pub rhs: Vec<Equality>,
}

#[derive(Default, Debug, Clone)]
pub struct Equality {
    pub rhs: Comparison,
    pub lhs: Option<(Token, Box<Equality>)>,
}

#[derive(Default, Debug, Clone)]
pub struct Comparison {
    pub rhs: Term,
    pub lhs: Option<(Token, Box<Comparison>)>,
}

#[derive(Default, Debug, Clone)]
pub struct Term {
    pub rhs: Factor,
    pub lhs: Option<(Token, Box<Term>)>,
}

#[derive(Default, Debug, Clone)]
pub struct Factor {
    pub rhs: Unary,
    pub lhs: Option<(Token, Box<Factor>)>,
}

#[derive(Debug, Clone)]
pub enum Unary {
    Unary(Token, Box<Unary>),
    Primary(Box<Primary>),
}

#[derive(Debug, Clone)]
pub enum Primary {
    Token(Token),
    Call(FuncCall),
    Grouping(Expr),
}

#[derive(Default, Debug, Clone)]
pub struct FuncCall {
    pub args: Vec<Expr>,
    pub ident: String,
    pub addl: Vec<Vec<Expr>>,
}

impl Default for Assignment {
    fn default() -> Self {
        Self::Or(LogicOr::default())
    }
}

impl Default for Unary {
    fn default() -> Self {
        Self::Primary(Box::default())
    }
}

impl Default for Primary {
    fn default() -> Self {
        Self::Token(Token::default())
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.assignment)
    }
}

impl Display for Assignment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Assignment(ident, to) => write!(f, "{} = {}", ident, to),
            Self::Or(o) => write!(f, "{}", o),
        }
    }
}

impl Display for LogicOr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.lhs)?;
        for l in &self.rhs {
            write!(f, " or {}", l)?;
        }
        Ok(())
    }
}

impl Display for LogicAnd {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.lhs)?;
        for l in &self.rhs {
            write!(f, " and {}", l)?;
        }
        Ok(())
    }
}

impl Display for Equality {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some((op, eq)) = &self.lhs {
            write!(f, "({1:?} {0} {2})", eq, op.kind, self.rhs)?;
        } else {
            write!(f, "{}", self.rhs)?;
        }
        Ok(())
    }
}

impl Display for Comparison {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some((op, eq)) = &self.lhs {
            write!(f, "({1:?} {0} {2})", eq, op.kind, self.rhs)?;
        } else {
            write!(f, "{}", self.rhs)?;
        }
        Ok(())
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some((op, eq)) = &self.lhs {
            write!(f, "({1:?} {0} {2})", eq, op.kind, self.rhs)?;
        } else {
            write!(f, "{}", self.rhs)?;
        }
        Ok(())
    }
}

impl Display for Factor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some((op, eq)) = &self.lhs {
            write!(f, "({1:?} {0} {2})", eq, op.kind, self.rhs)?;
        } else {
            write!(f, "{}", self.rhs)?;
        }
        Ok(())
    }
}

impl Display for Unary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unary(op, unary) => write!(f, "({:?} {})", op.kind, unary)?,
            Self::Primary(primary) => write!(f, "{}", primary)?,
        }
        Ok(())
    }
}

impl Display for Primary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Token(token) => write!(f, "{:?}", token.kind)?,
            Self::Grouping(expr) => write!(f, "(group {})", expr)?,
            Self::Call(func) => write!(f, "call: {}", func)?,
        }
        Ok(())
    }
}

impl Display for FuncCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "args: ")?;
        for arg in &self.args {
            write!(f, "{} ", arg)?;
        }
        write!(f, ", ident: {}", self.ident)?;
        Ok(())
    }
}

impl<V: Visitor> Visitable<V> for Expr {
    fn accept(&self, visitor: &mut V) -> V::Output {
        visitor.visit_expr(self)
    }
}

impl<V: Visitor> Visitable<V> for Assignment {
    fn accept(&self, visitor: &mut V) -> V::Output {
        visitor.visit_assignment(self)
    }
}

impl<V: Visitor> Visitable<V> for LogicOr {
    fn accept(&self, visitor: &mut V) -> V::Output {
        visitor.visit_logic_or(self)
    }
}

impl<V: Visitor> Visitable<V> for LogicAnd {
    fn accept(&self, visitor: &mut V) -> V::Output {
        visitor.visit_logic_and(self)
    }
}

impl<V: Visitor> Visitable<V> for Equality {
    fn accept(&self, visitor: &mut V) -> V::Output {
        visitor.visit_equality(self)
    }
}

impl<V: Visitor> Visitable<V> for Comparison {
    fn accept(&self, visitor: &mut V) -> V::Output {
        visitor.visit_comparison(self)
    }
}

impl<V: Visitor> Visitable<V> for Term {
    fn accept(&self, visitor: &mut V) -> V::Output {
        visitor.visit_term(self)
    }
}

impl<V: Visitor> Visitable<V> for Factor {
    fn accept(&self, visitor: &mut V) -> V::Output {
        visitor.visit_factor(self)
    }
}

impl<V: Visitor> Visitable<V> for Unary {
    fn accept(&self, visitor: &mut V) -> V::Output {
        visitor.visit_unary(self)
    }
}

impl<V: Visitor> Visitable<V> for Primary {
    fn accept(&self, visitor: &mut V) -> V::Output {
        visitor.visit_primary(self)
    }
}

impl<T: Iterator<Item = Token> + Clone> Parseable<T> for Expr {
    fn try_parse(stream: &mut TokenStream<T>) -> Result<Self> {
        Assignment::try_parse(stream).map(|assignment| Self { assignment })
    }
}

impl<T: Iterator<Item = Token> + Clone> Parseable<T> for Assignment {
    fn try_parse(stream: &mut TokenStream<T>) -> Result<Self> {
        let mut fork = stream.fork();

        match LogicOr::try_parse(&mut fork) {
            Ok(eq) => match fork.peek() {
                Some(token) if token.kind == TokenType::Eq => {}
                _ => {
                    stream.join(fork);
                    return Ok(Self::Or(eq));
                }
            },
            Err(e) => match e.downcast_ref() {
                Some(ParseError::InvalidToken(ref token)) if token.kind == TokenType::Eq => {}
                _ => {
                    return Err(e);
                }
            },
        }

        let ident = match stream.peek() {
            Some(token) => match token.kind {
                TokenType::Ident(ident) => {
                    stream.next().unwrap();
                    ident
                }
                _ => return Err(ParseError::InvalidToken(token.clone()).into()),
            },
            None => return Err(ParseError::UnexpectedNone.into()),
        };

        match stream.peek() {
            Some(token) if token.kind == TokenType::Eq => _ = stream.next(),
            Some(token) => return Err(ParseError::InvalidToken(token.clone()).into()),
            None => return Err(ParseError::UnexpectedNone.into()),
        }

        let expr = Assignment::try_parse(stream)?;
        Ok(Self::Assignment(ident, Box::new(expr)))
    }
}

impl<T: Iterator<Item = Token> + Clone> Parseable<T> for LogicOr {
    fn try_parse(stream: &mut TokenStream<T>) -> Result<Self> {
        let lhs = LogicAnd::try_parse(stream)?;
        let mut rhs = Vec::new();
        while let Some(next) = stream.peek() {
            if next.kind == TokenType::Or {
                stream.next();
                rhs.push(LogicAnd::try_parse(stream)?);
            } else {
                break;
            }
        }
        Ok(Self { rhs, lhs })
    }
}

impl<T: Iterator<Item = Token> + Clone> Parseable<T> for LogicAnd {
    fn try_parse(stream: &mut TokenStream<T>) -> Result<Self> {
        let lhs = Equality::try_parse(stream)?;
        let mut rhs = Vec::new();
        while let Some(next) = stream.peek() {
            if next.kind == TokenType::And {
                stream.next();
                rhs.push(Equality::try_parse(stream)?);
            } else {
                break;
            }
        }
        Ok(Self { rhs, lhs })
    }
}

impl<T: Iterator<Item = Token> + Clone> Parseable<T> for Equality {
    fn try_parse(stream: &mut TokenStream<T>) -> Result<Self> {
        let rhs = Comparison::try_parse(stream)?;
        let mut lhs = None;
        let mut expr = Self { rhs, lhs };
        while let Some(next) = stream.peek() {
            match next.kind {
                TokenType::NEq | TokenType::EqEq => {
                    //TODO check if correct
                    let op = stream.next().unwrap();
                    //TODO maybe handle error by using None instead (and returning error alongside the parsed eq)
                    lhs = Some((op, Box::new(expr)));
                    expr = Self {
                        rhs: Comparison::try_parse(stream)?,
                        lhs,
                    };
                }
                _ => break,
            }
        }
        Ok(expr)
    }
}

impl<T: Iterator<Item = Token> + Clone> Parseable<T> for Comparison {
    fn try_parse(stream: &mut TokenStream<T>) -> Result<Self> {
        let rhs = Term::try_parse(stream)?;
        let mut lhs = None;
        let mut expr = Self { rhs, lhs };
        while let Some(next) = stream.peek() {
            match next.kind {
                TokenType::Gt | TokenType::GtEq | TokenType::Lt | TokenType::LtEq => {
                    //TODO check if correct
                    let op = stream.next().unwrap();
                    //TODO maybe handle error by using None instead (and returning error alongside the parsed eq)
                    lhs = Some((op, Box::new(expr)));
                    expr = Self {
                        rhs: Term::try_parse(stream)?,
                        lhs,
                    };
                }
                _ => break,
            }
        }
        Ok(expr)
    }
}

impl<T: Iterator<Item = Token> + Clone> Parseable<T> for Term {
    fn try_parse(stream: &mut TokenStream<T>) -> Result<Self> {
        let rhs = Factor::try_parse(stream)?;
        let mut lhs = None;
        let mut expr = Self { rhs, lhs };
        while let Some(next) = stream.peek() {
            match next.kind {
                TokenType::Plus | TokenType::Minus => {
                    //TODO check if correct
                    let op = stream.next().unwrap();
                    //TODO maybe handle error by using None instead (and returning error alongside the parsed eq)
                    lhs = Some((op, Box::new(expr)));
                    expr = Self {
                        rhs: Factor::try_parse(stream)?,
                        lhs,
                    };
                }
                _ => break,
            }
        }
        Ok(expr)
    }
}

impl<T: Iterator<Item = Token> + Clone> Parseable<T> for Factor {
    fn try_parse(stream: &mut TokenStream<T>) -> Result<Self> {
        let rhs = Unary::try_parse(stream)?;
        let mut lhs = None;
        let mut expr = Self { rhs, lhs };
        while let Some(next) = stream.peek() {
            match next.kind {
                TokenType::Star | TokenType::Slash => {
                    //TODO check if correct
                    let op = stream.next().unwrap();
                    //TODO maybe handle error by using None instead (and returning error alongside the parsed eq)
                    lhs = Some((op, Box::new(expr)));
                    expr = Self {
                        rhs: Unary::try_parse(stream)?,
                        lhs,
                    };
                }
                _ => break,
            }
        }
        Ok(expr)
    }
}

impl<T: Iterator<Item = Token> + Clone> Parseable<T> for Unary {
    fn try_parse(stream: &mut TokenStream<T>) -> Result<Self> {
        match stream.peek() {
            Some(token) => match token.kind {
                TokenType::Bang | TokenType::Minus => {
                    let op = stream.next().unwrap();
                    let next = Self::try_parse(stream)?;
                    Ok(Self::Unary(op, Box::new(next)))
                }
                _ => {
                    let primary = Primary::try_parse(stream)?;
                    Ok(Self::Primary(Box::new(primary)))
                }
            },
            None => Err(ParseError::UnexpectedNone.into()),
        }
    }
}

impl<T: Iterator<Item = Token> + Clone> Parseable<T> for Primary {
    fn try_parse(stream: &mut TokenStream<T>) -> Result<Self> {
        match stream.peek() {
            Some(token) => match token.kind {
                TokenType::Ident(_) => {
                    if let Some(n1) = stream.peek2() {
                        //TODO verify + rewrite this mess
                        if n1.kind == TokenType::OpenParen {
                            let ident = if let Some(i) = stream.next() {
                                match i.kind {
                                    TokenType::Ident(s) => s,
                                    _ => return Err(ParseError::InvalidToken(i.clone()).into()),
                                }
                            } else {
                                return Err(ParseError::UnexpectedNone.into());
                            };
                            _ = stream.next();
                            let mut args = Vec::new();
                            while let Some(next) = stream.peek() {
                                match next.kind {
                                    TokenType::CloseParen => {
                                        stream.next();
                                        break;
                                    }
                                    TokenType::Comma => _ = stream.next(),
                                    _ => match Expr::try_parse(stream) {
                                        Ok(e) => args.push(e),
                                        Err(e) => match e.downcast_ref() {
                                            Some(ParseError::InvalidToken(t))
                                                if t.kind == TokenType::CloseParen =>
                                            {
                                                _ = stream.next();
                                                break;
                                            }

                                            Some(ParseError::InvalidToken(t))
                                                if t.kind == TokenType::Comma =>
                                            {
                                                _ = stream.next()
                                            }

                                            _ => return Err(e),
                                        },
                                    },
                                }
                            }
                            if let Some(t) = stream.peek() {
                                if t.kind == TokenType::OpenParen {
                                    //TODO somehow recursively build fun call chain: fun1(args)(args2)(args3), where fun1 returns fun2, and so on.
                                    stream.next();
                                    let mut addl = vec![Vec::new()];
                                    while let Some(t) = stream.peek() {
                                        match t.kind {
                                            TokenType::Semi => {
                                                stream.next();
                                                break;
                                            }
                                            TokenType::Comma => _ = stream.next(),
                                            TokenType::CloseParen => _ = stream.next(),
                                            TokenType::OpenParen => addl.push(Vec::new()),
                                            _ => match Expr::try_parse(stream) {
                                                Ok(ex) => addl.last_mut().unwrap().push(ex),
                                                Err(e) => match e.downcast_ref() {
                                                    Some(ParseError::InvalidToken(t))
                                                        if t.kind == TokenType::CloseParen
                                                            || t.kind == TokenType::Comma =>
                                                    {
                                                        _ = stream.next()
                                                    }

                                                    _ => return Err(e),
                                                },
                                            },
                                        }
                                    }
                                    return Ok(Self::Call(FuncCall { args, ident, addl }));
                                }
                            }
                            Ok(Self::Call(FuncCall {
                                args,
                                ident,
                                addl: Vec::new(),
                            }))
                        } else {
                            Ok(Self::Token(stream.next().unwrap()))
                        }
                    } else {
                        Ok(Self::Token(stream.next().unwrap()))
                    }
                }

                TokenType::Literal(_)
                | TokenType::Keyword(Keyword::False | Keyword::Nil | Keyword::True) => {
                    Ok(Self::Token(stream.next().unwrap()))
                }
                TokenType::OpenParen => {
                    stream.next();
                    let expr = Expr::try_parse(stream)?;
                    if let Some(next) = stream.peek() {
                        if next.kind != TokenType::CloseParen {
                            return Err(ParseError::InvalidToken(next.clone()).into());
                        }
                    } else {
                        return Err(ParseError::UnexpectedNone.into());
                    }
                    stream.next();
                    Ok(Self::Grouping(expr))
                }
                _ => Err(ParseError::InvalidToken(token.clone()).into()),
            },
            None => Err(ParseError::UnexpectedNone.into()),
        }
    }
}

fn func_call_stackM<T: Iterator<Item = Token> + Clone>(
    stream: &mut TokenStream<T>,
) -> Result<Primary> {
    Ok(Primary::default())
}

/*
#[cfg(test)]
mod tests {
    use crate::{core::types::Number, lexer::lexing_utils::LiteralKind};

    use super::*;
    #[test]
    fn parse_eq() {
        let tokens = [Token::new(
            TokenType::Literal(LiteralKind::Number(Number::new(1), "1".into())),
            1,
        )]
        .into_iter()
        .peekable();
        let mut stream = TokenStream::new(tokens).peekable();
        let eq = Equality::try_parse(&mut stream);
        assert!(eq.is_ok());
        assert_eq!(
            eq.unwrap(),
            Equality {
                rhs: Comparison {
                    rhs: Term {
                        rhs: Factor {
                            rhs: Unary::Primary(Primary::Token(Token::new(
                                TokenType::Literal(LiteralKind::Number(Number::new(1), "1".into())),
                                1
                            ))),
                            lhs: None
                        },
                        lhs: None
                    },
                    lhs: None
                },
                lhs: None
            }
        );
    }
}
*/
