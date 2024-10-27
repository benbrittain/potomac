// TODO: Switch to using miette::SourceSpan?
pub type Span = chumsky::prelude::SimpleSpan<usize>;
// TODO: Consider merging Spanned into AST nodes directly.
pub type Spanned<T> = (T, Span);

#[derive(PartialEq, Clone, Debug)]
pub enum ToyOp {
    Add,
    Sub,
    Div,
    Mul,
}

#[derive(PartialEq, Clone, Debug)]
pub enum ToyValue<'src> {
    Bool(bool),
    Num(f64),
    Str(&'src str),
}

#[derive(PartialEq, Clone, Debug)]
pub enum ToyExpr<'src> {
    BinOp {
        op: ToyOp,
        left: Box<ToyExpr<'src>>,
        right: Box<ToyExpr<'src>>,
    },
    Ident(&'src str),
    Value(ToyValue<'src>),
    Tensor(Vec<Spanned<ToyExpr<'src>>>),
    Call(&'src str, Vec<Spanned<ToyExpr<'src>>>),
    Paren(Box<ToyExpr<'src>>),
}

#[derive(PartialEq, Clone, Debug)]
pub enum ToyStmt<'src> {
    VarDecl(&'src str, Option<Vec<f64>>, Spanned<ToyExpr<'src>>),
    Print(Spanned<ToyExpr<'src>>),
}

#[derive(PartialEq, Debug, Clone)]
pub struct ToyFunc<'src> {
    pub name: Spanned<&'src str>,
    pub args: Vec<&'src str>,
    pub stmts: Vec<Spanned<ToyStmt<'src>>>,
}

pub type ToyModule<'src> = std::collections::HashMap<&'src str, ToyFunc<'src>>;
