use std::{num::ParseIntError, str::FromStr, sync::Arc};

use miette::{Diagnostic, Severity, SourceSpan};
use thiserror::Error;
use winnow::{
    ascii::{
        self, alpha1, alphanumeric1, digit0, digit1, hex_digit1, multispace0, multispace1, newline,
        oct_digit1, Caseless,
    },
    combinator::{
        alt, cut_err, delimited, eof, fail, not, opt, peek, preceded, repeat, repeat_till,
        separated, terminated,
    },
    error::{
        AddContext, ContextError, ErrorKind, FromExternalError, FromRecoverableError, ParserError,
        StrContext, StrContextValue,
    },
    stream::{AsChar, Location, Recoverable, Stream},
    token::{any, none_of, one_of, take, take_while},
    Located, Parser, RecoverableParser,
};

use crate::ast::*;

type Input<'a> = Recoverable<Located<&'a str>, ToyParseError>;
type PResult<T> = winnow::PResult<T, ToyParseError>;

fn ident<'s>(input: &mut Input<'s>) -> PResult<&'s str> {
    delimited(
        multispace0,
        take_while(1.., ('_', AsChar::is_alphanum)),
        multispace0,
    )
    .parse_next(input)
}

fn parens(i: &mut Input<'_>) -> PResult<ToyExpr> {
    delimited("(", binop, ")")
        .map(|e| ToyExpr::Paren(Box::new(e)))
        .parse_next(i)
}

fn factor(input: &mut Input<'_>) -> PResult<ToyExpr> {
    delimited(
        multispace0,
        alt((
            digit1.try_map(FromStr::from_str).map(ToyExpr::Value),
            parens,
            ident.map(String::from).map(ToyExpr::Ident),
        )),
        multispace0,
    )
    .parse_next(input)
}

fn term(input: &mut Input<'_>) -> PResult<ToyExpr> {
    let init = factor.parse_next(input)?;
    repeat(0.., (one_of(['*', '/']), factor))
        .fold(
            move || init.clone(),
            |acc, (op, val): (char, ToyExpr)| {
                if op == '*' {
                    ToyExpr::BinOp(ToyOp::Mul, Box::new(acc), Box::new(val))
                } else {
                    ToyExpr::BinOp(ToyOp::Div, Box::new(acc), Box::new(val))
                }
            },
        )
        .parse_next(input)
}

pub fn binop(input: &mut Input<'_>) -> PResult<ToyExpr> {
    let init = term.parse_next(input)?;
    repeat(0.., (one_of(['+', '-']), term))
        .fold(
            move || init.clone(),
            |acc, (op, val): (char, ToyExpr)| {
                if op == '+' {
                    ToyExpr::BinOp(ToyOp::Add, Box::new(acc), Box::new(val))
                } else {
                    ToyExpr::BinOp(ToyOp::Sub, Box::new(acc), Box::new(val))
                }
            },
        )
        .parse_next(input)
}

pub fn expr(input: &mut Input<'_>) -> PResult<ToyExpr> {
    alt((
        terminated(("var", ident, "=", multispace0, binop), ";")
            .map(|(_, ident, _, _, b)| ToyExpr::VarDecl(ident.to_string(), Box::new(b))),
        terminated(("return", binop), ";").map(|(_, b)| ToyExpr::Return(Box::new(b))),
    ))
    .parse_next(input)
}

pub fn func(input: &mut Input<'_>) -> PResult<ToyFunc> {
    let proto = preceded(("def", multispace0), ident).parse_next(input)?;
    let args: Vec<&str> = delimited('(', separated(0.., ident, ","), ')').parse_next(input)?;
    multispace0.parse_next(input)?;
    let block: Vec<ToyExpr> = delimited(
        "{",
        repeat(0.., delimited(multispace0, expr, multispace0)),
        (multispace0, "}"),
    )
    .parse_next(input)?;
    Ok(ToyFunc {
        proto: proto.to_string(),
        args: args.into_iter().map(|s| s.to_string()).collect(),
        block,
    })
}

pub fn module(input: &mut Input<'_>) -> PResult<ToyModule> {
    Ok(ToyModule { funcs: vec![] })
}

#[derive(Debug, Diagnostic, Clone, Eq, PartialEq, Error)]
#[error("Failed to parse Toy.")]
pub struct ToyParseFailure {
    /// Original input that this failure came from.
    #[source_code]
    pub input: Arc<String>,

    /// Sub-diagnostics for this failure.
    #[related]
    pub diagnostics: Vec<ToyDiagnostic>,
}

/// An individual diagnostic message for a Toy parsing issue.
///
/// While generally signifying errors, they can also be treated as warnings.
#[derive(Debug, Diagnostic, Clone, Eq, PartialEq, Error)]
#[error("{kind}")]
pub struct ToyDiagnostic {
    /// Shared source for the diagnostic.
    #[source_code]
    pub input: Arc<String>,

    /// Offset in chars of the error.
    #[label("{}", label.unwrap_or("here"))]
    pub span: SourceSpan,

    /// Label text for this span. Defaults to `"here"`.
    pub label: Option<&'static str>,

    /// Suggestion for fixing the parser error.
    #[help]
    pub help: Option<&'static str>,

    /// Severity level for the Diagnostic.
    #[diagnostic(severity)]
    pub severity: miette::Severity,

    /// Specific error kind for this parser error.
    pub kind: ToyErrorKind,
}

/// A type reprenting additional information specific to the type of error being returned.
#[derive(Debug, Diagnostic, Clone, Eq, PartialEq, Error)]
pub enum ToyErrorKind {
    /// An error occurred while parsing an integer.
    #[error(transparent)]
    #[diagnostic(code(toy::parse_int))]
    ParseIntError(ParseIntError),

    /// Generic parsing error. The given context string denotes the component
    /// that failed to parse.
    #[error("Expected {0}.")]
    #[diagnostic(code(toy::parse_component))]
    Context(&'static str),

    /// Generic unspecified error. If this is returned, the call site should
    /// be annotated with context, if possible.
    #[error("An unspecified parse error occurred.")]
    #[diagnostic(code(toy::other))]
    Other,
}

impl<'a> FromExternalError<Input<'a>, ParseIntError> for ToyParseError {
    fn from_external_error(_: &Input<'a>, _kind: ErrorKind, e: ParseIntError) -> Self {
        ToyParseError {
            span: None,
            label: None,
            help: None,
            context: None,
            kind: Some(ToyErrorKind::ParseIntError(e)),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct ToyParseError {
    pub(crate) context: Option<&'static str>,
    pub(crate) span: Option<SourceSpan>,
    pub(crate) label: Option<&'static str>,
    pub(crate) help: Option<&'static str>,
    pub(crate) kind: Option<ToyErrorKind>,
}

impl<I: Stream> ParserError<I> for ToyParseError {
    fn from_error_kind(_input: &I, _kind: ErrorKind) -> Self {
        Self {
            span: None,
            label: None,
            help: None,
            context: None,
            kind: None,
        }
    }

    fn append(
        self,
        _input: &I,
        _token_start: &<I as Stream>::Checkpoint,
        _kind: ErrorKind,
    ) -> Self {
        self
    }
}

impl<I: Stream + Location> FromRecoverableError<I, Self> for ToyParseError {
    #[inline]
    fn from_recoverable_error(
        token_start: &<I as Stream>::Checkpoint,
        _err_start: &<I as Stream>::Checkpoint,
        input: &I,
        mut e: Self,
    ) -> Self {
        e.span = e.span.or_else(|| {
            Some((input.offset_from(token_start).saturating_sub(1)..input.location()).into())
        });
        e
    }
}

pub(crate) fn try_parse<'a, P, T, E>(mut parser: P, input: &'a str) -> Result<T, ToyParseFailure>
where
    P: Parser<Input<'a>, T, ToyParseError>,
    P: RecoverableParser<Located<&'a str>, T, ToyParseError, E>,
{
    let (_, maybe_val, errs) = parser.recoverable_parse(Located::new(input));
    if let (Some(v), true) = (maybe_val, errs.is_empty()) {
        Ok(v)
    } else {
        Err(failure_from_errs(errs, input))
    }
}

pub(crate) fn failure_from_errs(errs: Vec<ToyParseError>, input: &str) -> ToyParseFailure {
    let src = Arc::new(String::from(input));
    ToyParseFailure {
        input: src.clone(),
        diagnostics: errs
            .into_iter()
            .map(|e| ToyDiagnostic {
                input: src.clone(),
                span: e.span.unwrap_or_else(|| (0usize..0usize).into()),
                label: e.label,
                help: e.help,
                severity: Severity::Error,
                kind: if let Some(ctx) = e.context {
                    ToyErrorKind::Context(ctx)
                } else {
                    ToyErrorKind::Other
                },
            })
            .collect(),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    /// Helper for making a string recoverable
    fn r(s: &str) -> Input<'_> {
        Recoverable::new(Located::new(s))
    }

    #[test]
    fn test_binop_expr() {
        assert_eq!(term.parse(r("3")).unwrap(), ToyExpr::Value(3),);

        assert_eq!(
            term.parse(r("3 * 2")).unwrap(),
            ToyExpr::BinOp(
                ToyOp::Mul,
                Box::new(ToyExpr::Value(3)),
                Box::new(ToyExpr::Value(2)),
            ),
        );

        assert_eq!(
            binop.parse(r("3 + 2 - 1")).unwrap(),
            ToyExpr::BinOp(
                ToyOp::Sub,
                Box::new(ToyExpr::BinOp(
                    ToyOp::Add,
                    Box::new(ToyExpr::Value(3)),
                    Box::new(ToyExpr::Value(2))
                )),
                Box::new(ToyExpr::Value(1)),
            ),
        );

        assert_eq!(
            binop.parse(r("3 + 2")).unwrap(),
            ToyExpr::BinOp(
                ToyOp::Add,
                Box::new(ToyExpr::Value(3)),
                Box::new(ToyExpr::Value(2)),
            ),
        );

        assert_eq!(
            binop.parse(r("3 * (2 - 1)")).unwrap(),
            ToyExpr::BinOp(
                ToyOp::Mul,
                Box::new(ToyExpr::Value(3)),
                Box::new(ToyExpr::Paren(Box::new(ToyExpr::BinOp(
                    ToyOp::Sub,
                    Box::new(ToyExpr::Value(2)),
                    Box::new(ToyExpr::Value(1))
                ))))
            ),
        );
    }

    #[test]
    fn test_return_expr() {
        assert_eq!(
            expr.parse(r("return 1;")).unwrap(),
            ToyExpr::Return(Box::new(ToyExpr::Value(1)))
        );

        assert_eq!(
            expr.parse(r("return a;")).unwrap(),
            ToyExpr::Return(Box::new(ToyExpr::Ident("a".to_string())))
        );
    }

    #[test]
    fn test_func() {
        assert_eq!(
            func.parse(r("def multiply_transpose(a,b) { return a; }"))
                .unwrap(),
            ToyFunc {
                proto: "multiply_transpose".to_string(),
                args: vec!["a".to_string(), "b".to_string()],
                block: vec![ToyExpr::Return(Box::new(ToyExpr::Ident("a".to_string())))],
            }
        )
    }
}
