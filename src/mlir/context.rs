use crate::mlir::sys;

pub struct Context {
    ctx: sys::MlirContext,
}

impl Context {
    pub fn new() -> Self {
        let ctx = unsafe {
            let ctx = sys::mlirContextCreate();
            assert!(sys::mlirContextEqual(ctx, ctx));
            ctx
        };
        Context {
            ctx
        }
    //        mlirContextDestroy(ctx);
    }

    pub fn get_or_load_dialect(&self, name: &str) -> () {

    }

    pub fn num_loaded_dialects(&self) -> isize {
        unsafe {
            sys::mlirContextGetNumLoadedDialects(self.ctx)
        }
    }
}
