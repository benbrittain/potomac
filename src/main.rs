mod ast;
mod parser;
mod sys;

use miette::{IntoDiagnostic, Result};

fn main() -> Result<()> {
    dbg!(parser::try_parse(
        parser::func,
        r###"def main() {
      var a = 3;
    }"###
    )
    .into_diagnostic()?);
    Ok(())
}
