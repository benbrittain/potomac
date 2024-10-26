mod ast;
mod lexer;
mod error;
//mod parser;
mod sys;

use miette::Result;

fn main() -> Result<()> {
    let src = r###"
    return 0 4@3^3;
    "###;
    let tokens = crate::lexer::lex(src)?;
    //let ast = crate::parser::parse(tokens)?;
    dbg!(tokens);
    //let ast = parser::try_parse(
    //    parser::func,
    //    r###"def main() {
    //  ar a = 3;
    //}"###,
    //)
    //.into_diagnostic()?;
    //let ast = parser::try_parse(
    //    parser::factor, "012#",
    //)
    //.into_diagnostic()?;
    Ok(())
}
