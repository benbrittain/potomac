use std::sync::Arc;

use chumsky::{extra, prelude::*, Parser};
use miette::SourceSpan;

use crate::{ast::Span, error::*};

type ToyParseError<'a> = Rich<'a, char, Span, &'a str>;

#[derive(Clone, Debug, PartialEq)]
pub enum Token<'src> {
    Bool(bool),
    Num(f64),
    Str(&'src str),
    Op(&'src str),
    Ctrl(char),
    Ident(&'src str),
    Def,
    Var,
    Return,
    Print,
}

impl std::fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub fn lex<'src>(src: &'src str) -> Result<Vec<(Token<'src>, Span)>, ToyFailure> {
    let (output, errs) = lexer().parse(src).into_output_errors();
    if let (Some(o), true) = (output, errs.is_empty()) {
        Ok(o)
    } else {
        Err(failure_from_errs(errs, src))
    }
}

fn lexer<'src>(
) -> impl Parser<'src, &'src str, Vec<(Token<'src>, Span)>, extra::Err<ToyParseError<'src>>> {
    let num = text::int(10)
        .then(just('.').then(text::digits(10)).or_not())
        .to_slice()
        .from_str()
        .unwrapped()
        .map(Token::Num);

    let comment = just("#")
        .then(any().and_is(just('\n').not()).repeated())
        .padded();

    // A parser for strings
    let str = just('"')
        .ignore_then(none_of('"').repeated().to_slice())
        .then_ignore(just('"'))
        .map(Token::Str);

    // A parser for operators
    let op = one_of("+*-/!=")
        .repeated()
        .at_least(1)
        .to_slice()
        .map(Token::Op);

    // A parser for control characters (delimiters, semicolons, etc.)
    let ctrl = one_of("()[]{};,<>").map(Token::Ctrl);

    let ident = text::ascii::ident().map(|ident: &str| match ident {
        "def" => Token::Def,
        "var" => Token::Var,
        "return" => Token::Return,
        "print" => Token::Print,
        _ => Token::Ident(ident),
    });

    // A single token can be one of the above
    let token = num.or(str).or(op).or(ctrl).or(ident);

    token
        .map_with(|tok, e| (tok, e.span()))
        .padded_by(comment.repeated())
        .padded()
        // If we encounter an error, skip and attempt to lex the next character as a token instead
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
}

pub fn failure_from_errs(errs: Vec<ToyParseError>, input: &str) -> ToyFailure {
    let src = Arc::new(String::from(input));
    ToyFailure {
        input: src.clone(),
        diagnostics: errs
            .into_iter()
            .map(|e| {
                let reason = e.reason().to_string();
                ToyDiagnostic {
                    input: src.clone(),
                    span: SourceSpan::new(
                        e.span().start().into(),
                        e.span().end() - e.span().start(),
                    ),
                    label: Some("invalid token"),
                    help: Some(reason),
                    kind: ToyErrorKind::Lex,
                }
            })
            .collect(),
    }
}
