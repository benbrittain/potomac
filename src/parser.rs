use std::{collections::HashMap, sync::Arc};

use chumsky::{extra, prelude::*, Parser};
use miette::SourceSpan;

use crate::{ast::*, error::*, lexer::Token};

type ToyParseError<'a, 'b> = Rich<'a, Token<'b>, Span, &'b str>;
type ParserInput<'tokens, 'src> =
    chumsky::input::SpannedInput<Token<'src>, Span, &'tokens [(Token<'src>, Span)]>;

pub fn parse<'src>(
    src: &'src str,
    tokens: Vec<(Token<'src>, Span)>,
) -> Result<(HashMap<&'src str, ToyFunc<'src>>, Span), ToyFailure> {
    let (ast, errs) = module_parser()
        .map_with(|ast, e| (ast, e.span()))
        .parse(tokens.as_slice().spanned((src.len()..src.len()).into()))
        .into_output_errors();
    if let (Some(o), true) = (ast, errs.is_empty()) {
        Ok(o)
    } else {
        Err(failure_from_errs(errs, src))
    }
}

fn expr_parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    Spanned<ToyExpr<'src>>,
    extra::Err<ToyParseError<'tokens, 'src>>,
> {
    recursive(|expr| {
        let val = select! {
            Token::Num(n) => ToyExpr::Value(ToyValue::Num(n)),
            Token::Str(n) => ToyExpr::Value(ToyValue::Str(n)),
            Token::Bool(n) => ToyExpr::Value(ToyValue::Bool(n)),
        }
        .labelled("value");

        let ident = select! { Token::Ident(ident) => ident }.labelled("identifier");

        let items = expr
            .clone()
            .separated_by(just(Token::Ctrl(',')))
            .allow_trailing()
            .collect::<Vec<_>>();

        let tensor = items
            .clone()
            .map(ToyExpr::Tensor)
            .delimited_by(just(Token::Ctrl('[')), just(Token::Ctrl(']')));

        let atom = val.or(ident.map(ToyExpr::Ident)).or(tensor);

        let op = just(Token::Op("*"))
            .to(ToyOp::Mul)
            .or(just(Token::Op("/")).to(ToyOp::Div));

        let product =
            atom.clone()
                .foldl_with(op.then(atom.clone()).repeated(), |a, (op, b), _e| {
                    ToyExpr::BinOp {
                        op: op,
                        left: Box::new(a),
                        right: Box::new(b),
                    }
                });

        // Sum ops (add and subtract) have equal precedence
        let op = just(Token::Op("+"))
            .to(ToyOp::Add)
            .or(just(Token::Op("-")).to(ToyOp::Sub));
        let sum = product
            .clone()
            .foldl_with(op.then(product).repeated(), |a, (op, b), _e| {
                ToyExpr::BinOp {
                    op: op,
                    left: Box::new(a),
                    right: Box::new(b),
                }
            });

        sum.map_with(|e, s| (e, s.span()))
    })
}

fn stmt_parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    Spanned<ToyStmt<'src>>,
    extra::Err<ToyParseError<'tokens, 'src>>,
> {
    let ident = select! { Token::Ident(ident) => ident };
    let num = select! { Token::Num(ident) => ident };

    let items = num
        .clone()
        .separated_by(just(Token::Ctrl(',')))
        .allow_trailing()
        .at_least(1)
        .collect::<Vec<_>>();

    let var_decl = just(Token::Var)
        .ignore_then(ident.labelled("variable name"))
        .then(
            items
                .delimited_by(just(Token::Ctrl('<')), just(Token::Ctrl('>')))
                .or_not(),
        )
        .then_ignore(just(Token::Op("=")))
        .then(expr_parser())
        .then_ignore(just(Token::Ctrl(';')))
        .map_with(|((ident, shape), expr), e| (ToyStmt::VarDecl(ident, shape, expr), e.span()))
        .labelled("variable decl");

    let print_decl = just(Token::Print)
        .ignore_then(expr_parser().delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))))
        .then_ignore(just(Token::Ctrl(';')))
        .map_with(|expr, e| (ToyStmt::Print(expr), e.span()));

    var_decl.or(print_decl)
}

fn module_parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    ToyModule<'src>,
    extra::Err<ToyParseError<'tokens, 'src>>,
> {
    let ident = select! { Token::Ident(ident) => ident };

    // Argument lists are just identifiers separated by commas, surrounded by parentheses
    let args = ident
        .separated_by(just(Token::Ctrl(',')))
        .allow_trailing()
        .at_least(0)
        .collect()
        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
        .labelled("function args");

    let func = just(Token::Def)
        .ignore_then(
            ident
                .map_with(|name, e| (name, e.span()))
                .labelled("function name"),
        )
        .then(args)
        .then(
            stmt_parser()
                .labelled("statement")
                .repeated()
                .at_least(0)
                .collect()
                .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}'))),
        )
        .map(|((name, args), stmts)| (name, ToyFunc { name, args, stmts }))
        .labelled("function");

    func.repeated()
        .collect::<Vec<_>>()
        .validate(|fs, _, emitter| {
            let mut funcs = ToyModule::new();
            for ((name, name_span), f) in fs {
                if funcs.insert(name, f).is_some() {
                    emitter.emit(Rich::custom(
                        name_span,
                        format!("Function '{}' already exists", name),
                    ));
                }
            }
            funcs
        })
        .then_ignore(end())
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
                    label: None,
                    help: Some(reason),
                    kind: ToyErrorKind::Parse,
                }
            })
            .collect(),
    }
}
