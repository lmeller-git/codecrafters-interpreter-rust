use super::{
    ast::{Parseable, Visitable, Visitor},
    ParseError,
};
use crate::lexer::lexing_utils::{Keyword, Token, TokenStream, TokenType};
use anyhow::Result;
use std::{fmt::Display, iter::Peekable};

#[derive(Default, Debug)]
pub struct Expr {
    pub assignment: Assignment,
}

#[derive(Debug)]
pub enum Assignment {
    Assignment(String, Box<Assignment>),
    Equality(Equality),
}

#[derive(Default, Debug)]
pub struct Equality {
    pub rhs: Comparison,
    pub lhs: Option<(Token, Box<Equality>)>,
}

#[derive(Default, Debug)]
pub struct Comparison {
    pub rhs: Term,
    pub lhs: Option<(Token, Box<Comparison>)>,
}

#[derive(Default, Debug)]
pub struct Term {
    pub rhs: Factor,
    pub lhs: Option<(Token, Box<Term>)>,
}

#[derive(Default, Debug)]
pub struct Factor {
    pub rhs: Unary,
    pub lhs: Option<(Token, Box<Factor>)>,
}

#[derive(Debug)]
pub enum Unary {
    Unary(Token, Box<Unary>),
    Primary(Box<Primary>),
}

#[derive(Debug)]
pub enum Primary {
    Token(Token),
    Grouping(Expr),
}

impl Default for Assignment {
    fn default() -> Self {
        Self::Equality(Equality::default())
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
            Self::Equality(eq) => write!(f, "{}", eq),
        }
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
        }
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

        match Equality::try_parse(&mut fork) {
            Ok(eq) => match fork.peek() {
                Some(token) if token.kind == TokenType::Eq => {}
                _ => {
                    stream.join(fork);
                    return Ok(Self::Equality(eq));
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
                TokenType::Literal(_)
                | TokenType::Ident(_)
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
