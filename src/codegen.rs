use miette::Result;

pub fn generate(src: &str, ast: crate::ast::ToyModule) -> Result<()> {
    let context = crate::mlir::context::Context::new();
    dbg!(context.num_loaded_dialects());
    dbg!(context.get_or_load_dialect("toy"));
    Ok(())
}
