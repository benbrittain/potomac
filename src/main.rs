mod ast;
mod codegen;
mod error;
mod lexer;
mod mlir;
mod parser;

use miette::Result;

fn main() -> Result<()> {
    let src = r###"
def main1() {
    print(3);
  var a = a;
  var b = a;
}

def main() {
  # Define a variable `a` with shape <2, 3>, initialized with the literal value.
  # The shape is inferred from the supplied literal.
  var a = [[1, 2, 3], [4, 5, 6]];

  # b is identical to a, the literal tensor is implicitly reshaped: defining new
  # variables is the way to reshape tensors (element count must match).
  var b<2, 3> = [1, 2, 3, 4, 5, 6];

  print(1 + 3 * 4);

  # transpose() and print() are the only builtin, the following will transpose
  # a and b and perform an element-wise multiplication before printing the result.
  # print(transpose()); #(a) * transpose(b));
}
    "###;
    let tokens = crate::lexer::lex(src)?;
    let ast = crate::parser::parse(src, tokens)?;
    crate::codegen::generate(src, ast.0)
}
