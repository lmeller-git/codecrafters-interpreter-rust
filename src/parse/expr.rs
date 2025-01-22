use super::{
    ast::{Parseable, Visitable, Visitor},
    ParseError,
};
use crate::lexer::lexing_utils::{Token, TokenStream, TokenType};
use anyhow::Result;
use std::{fmt::Display, iter::Peekable};

#[derive(Default, Debug)]
pub struct Expr {
    eq: Equality,
}

#[derive(Default, Debug)]
pub struct Equality {
    rhs: Comparison,
    lhs: Option<(Token, Box<Equality>)>,
}

#[derive(Default, Debug)]
pub struct Comparison {
    rhs: Term,
    lhs: Option<(Token, Box<Comparison>)>,
}

#[derive(Default, Debug)]
pub struct Term {
    rhs: Factor,
    lhs: Option<(Token, Box<Term>)>,
}

#[derive(Default, Debug)]
pub struct Factor {
    rhs: Unary,
    lhs: Option<(Token, Box<Factor>)>,
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

impl Default for Unary {
    fn default() -> Self {
        Self::Primary(Box::new(Primary::default()))
    }
}

impl Default for Primary {
    fn default() -> Self {
        Self::Token(Token::default())
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.eq)
    }
}

impl Display for Equality {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some((op, eq)) = &self.lhs {
            write!(f, "{1} {0} {2}", eq, op, self.rhs)?;
        } else {
            write!(f, "{}", self.rhs)?;
        }
        Ok(())
    }
}

impl Display for Comparison {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some((op, eq)) = &self.lhs {
            write!(f, "{1} {0} {2}", eq, op, self.rhs)?;
        } else {
            write!(f, "{}", self.rhs)?;
        }
        Ok(())
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some((op, eq)) = &self.lhs {
            write!(f, "{1} {0} {2}", eq, op, self.rhs)?;
        } else {
            write!(f, "{}", self.rhs)?;
        }
        Ok(())
    }
}

impl Display for Factor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some((op, eq)) = &self.lhs {
            write!(f, "{1} {0} {2}", eq, op, self.rhs)?;
        } else {
            write!(f, "{}", self.rhs)?;
        }
        Ok(())
    }
}

impl Display for Unary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unary(op, unary) => write!(f, "{} {}", op, unary)?,
            Self::Primary(primary) => write!(f, "{}", primary)?,
        }
        Ok(())
    }
}

impl Display for Primary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Token(token) => write!(f, "{:?}", token.kind)?,
            Self::Grouping(expr) => write!(f, "({})", expr)?,
        }
        Ok(())
    }
}

impl<V: Visitor> Visitable<V> for Expr {
    fn accept(&mut self, visitor: &mut V) {
        visitor.visit_expr(self);
    }
}

impl<V: Visitor> Visitable<V> for Equality {
    fn accept(&mut self, visitor: &mut V) {
        visitor.visit_equality(self);
    }
}

impl<V: Visitor> Visitable<V> for Comparison {
    fn accept(&mut self, visitor: &mut V) {
        visitor.visit_comparison(self);
    }
}

impl<V: Visitor> Visitable<V> for Term {
    fn accept(&mut self, visitor: &mut V) {
        visitor.visit_term(self);
    }
}

impl<V: Visitor> Visitable<V> for Factor {
    fn accept(&mut self, visitor: &mut V) {
        visitor.visit_factor(self);
    }
}

impl<V: Visitor> Visitable<V> for Unary {
    fn accept(&mut self, visitor: &mut V) {
        visitor.visit_unary(self);
    }
}

impl<V: Visitor> Visitable<V> for Primary {
    fn accept(&mut self, visitor: &mut V) {
        visitor.visit_primary(self);
    }
}

impl<T: Iterator<Item = Token>> Parseable<T> for Expr {
    fn try_parse(stream: &mut Peekable<TokenStream<T>>) -> Result<Self> {
        Equality::try_parse(stream).map(|eq| Self { eq })
    }
}

impl<T: Iterator<Item = Token>> Parseable<T> for Equality {
    fn try_parse(stream: &mut Peekable<TokenStream<T>>) -> Result<Self> {
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

impl<T: Iterator<Item = Token>> Parseable<T> for Comparison {
    fn try_parse(stream: &mut Peekable<TokenStream<T>>) -> Result<Self> {
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

impl<T: Iterator<Item = Token>> Parseable<T> for Term {
    fn try_parse(stream: &mut Peekable<TokenStream<T>>) -> Result<Self> {
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

impl<T: Iterator<Item = Token>> Parseable<T> for Factor {
    fn try_parse(stream: &mut Peekable<TokenStream<T>>) -> Result<Self> {
        let rhs = Unary::try_parse(stream)?;
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

impl<T: Iterator<Item = Token>> Parseable<T> for Unary {
    fn try_parse(stream: &mut Peekable<TokenStream<T>>) -> Result<Self> {
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

impl<T: Iterator<Item = Token>> Parseable<T> for Primary {
    fn try_parse(stream: &mut Peekable<TokenStream<T>>) -> Result<Self> {
        match stream.peek() {
            Some(token) => match token.kind {
                TokenType::Literal(_) | TokenType::Keyword(_) => {
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
