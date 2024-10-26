use miette::SourceSpan;

//use crate::parser::{try_parse, ToyParseFailure};

#[derive(PartialEq, Clone, Debug)]
pub enum ToyOp {
    Add,
    Sub,
    Div,
    Mul,
}

#[derive(PartialEq, Clone, Debug)]
pub enum ToyExpr {
    BinOp {
        op: ToyOp,
        left: Box<ToyExpr>,
        right: Box<ToyExpr>,
    },
    Value(i64),
    VarDecl(String, Box<ToyExpr>),
    Paren(Box<ToyExpr>),
    Return(Box<ToyExpr>),
    Ident(String),
}

#[derive(PartialEq, Debug)]
pub struct ToyFunc {
    pub proto: String,
    pub args: Vec<String>,
    pub block: Vec<ToyExpr>,
    pub span: SourceSpan,
}

#[derive(PartialEq, Debug)]
pub struct ToyModule {
    pub funcs: Vec<ToyFunc>,
}

//impl std::str::FromStr for ToyModule {
//    type Err = ToyParseFailure;
//
//    fn from_str(s: &str) -> Result<Self, Self::Err> {
//        try_parse(crate::parser::module, s)
//    }
//}
