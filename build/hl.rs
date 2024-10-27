use std::collections::HashMap;

use anyhow::Result;
use heck::ToSnakeCase;
use quote::{format_ident, quote};
use syn::{
    visit::{self, Visit},
    ItemStruct, PatType, Type,
};

#[derive(Default)]
struct Visitor {
    mlir_types: Vec<String>,
    mlir_funcs: HashMap<String, syn::ForeignItemFn>,
}

impl<'ast> Visit<'ast> for Visitor {
    fn visit_foreign_item_fn(&mut self, f: &'ast syn::ForeignItemFn) {
        let ident = f.sig.ident.to_string();
        if ident.starts_with("mlir") {
            self.mlir_funcs.insert(ident, f.clone());
        }
        visit::visit_foreign_item_fn(self, f);
    }
    fn visit_item_struct(&mut self, st: &'ast ItemStruct) {
        let ident = st.ident.to_string();
        if ident.starts_with("Mlir") {
            self.mlir_types.push(ident);
        }
        visit::visit_item_struct(self, st);
    }
}

impl Visitor {
    fn generate(&self) -> String {
        let mut tokens = String::new();

        for ty in &self.mlir_types {
            //if ["MlirExecutionEngine", "MlirPassManager", "MlirSymbolTable"].contains(&ty.as_str()) {
            //    continue;
            //}
            let hl_ty = ty.strip_prefix("Mlir").unwrap();
            let hl_ty_ident = format_ident!("{}", hl_ty);
            let ll_ty_ident = format_ident!("{}", ty);

            let mut destructor: Option<_> = None;
            let mut class_funcs: Vec<_> = vec![];
            for (ffi_name, func) in self.mlir_funcs.iter() {
                let ffi_name_ident = format_ident!("{ffi_name}");
                let name = ffi_name
                    .strip_prefix("mlir")
                    .expect("functions should only start with mlir");

                if let Some(c_fn) = name.strip_prefix(hl_ty) {
                    match c_fn {
                        "Destroy" => {
                            destructor = Some(quote! {
                                impl Drop for #hl_ty_ident {
                                    fn drop(&mut self) {
                                        unsafe { sys::#ffi_name_ident(self.raw) };
                                    }
                                }
                            });
                        }
                        c_fn => {
                            let fn_ident = format_ident!("{}", c_fn.to_snake_case());
                            let mut fn_args = vec![];
                            let mut fn_params = vec![];
                            for fn_arg in func.sig.inputs.iter() {
                                match fn_arg {
                                    syn::FnArg::Typed(PatType { pat, ty, .. }) => {
                                        // too lazy
                                        let ty_str = quote! { #ty }.to_string();
                                        let mut pat_str = quote! { #pat }.to_string().to_snake_case();
                                        // rename type to ty to make rust happy
                                        let pat_str: String = pat_str.replace("type", "ty");
                                        let pat = format_ident!("{}", pat_str);
                                        if let Some(new_ty) = ty_str.strip_prefix("Mlir") {
                                            let new_ty = format_ident!("{new_ty}");
                                            fn_args.push(quote! {
                                                #pat : #new_ty
                                            });
                                            fn_params.push(quote! { #pat.to_raw() });
                                        } else {
                                            match ty_str.as_str() {
                                                ":: std :: os :: raw :: c_int" => {
                                                    fn_args.push(quote! {
                                                        #pat : i32
                                                    });
                                                    fn_params.push(quote! { #pat });
                                                }
                                                "* const :: std :: os :: raw :: c_void"
                                                | "* const :: std :: os :: raw :: c_char"
                                                | "* mut :: std :: os :: raw :: c_void"
                                                //| "std :: option :: Option < unsafe extern \"C\" fn (arg1 : * mut :: std :: os :: raw :: c_void) , >"
                                                //| ":: std :: option :: Option < unsafe extern \"C\" fn (arg1 : * mut :: std :: os :: raw :: c_void) , >"
                                                //| "isize"
                                                //| "i64"
                                                //| "* const MlirValue"
                                                //| "* const MlirRegion"
                                                //| "* const MlirBlock"
                                                //| "* const MlirType"
                                                //| "* mut MlirOperationState"
                                                | "i32"
                                                | "usize"
                                                | "bool" => {
                                                    fn_args.push(quote! {
                                                        #pat : #ty
                                                    });
                                                    fn_params.push(quote! { #pat });
                                                }
                                                _ => {
                                                    //fn_args.push(quote! {
                                                    //    #pat : todo!()
                                                    //});
                                                    fn_params.push(quote! { todo!(#ty_str)});
                                                }, //(), //todo!("{:?}", t),
                                            }
                                        }
                                    }
                                    _ => todo!(),
                                }
                            }

                            match c_fn {
                                "Create" => class_funcs.push(quote! {
                                    fn new(#(#fn_args,)*) -> Self {
                                        let raw = unsafe { sys::#ffi_name_ident(#(#fn_params,)*) };
                                        #hl_ty_ident{ raw }
                                    }
                                }),
                                c_fn => class_funcs.push(quote! {
                                    fn #fn_ident(#(#fn_args,)*) {
                                        todo!()
                                        // enforce that it's a void return
                                        //let _: () = unsafe { sys::#ffi_name_ident(#(#fn_params,)*) };
                                    }
                                }),
                            }
                        }
                    }
                }
            }

            let hl_struct = quote! {
                pub struct #hl_ty_ident {
                    raw: sys::#ll_ty_ident
                }

                impl #hl_ty_ident {
                    #(#class_funcs)*

                    pub fn to_raw(self) -> sys::#ll_ty_ident {
                        self.raw
                    }
                }
                #destructor
            };
            tokens.push_str(&hl_struct.to_string());
        }

        tokens
    }
}

pub fn gen(ll: &str) -> Result<String> {
    let ll_syntax_tree = syn::parse_file(ll)?;
    let mut vis = Visitor::default();
    vis.visit_file(&ll_syntax_tree);
    let generated = vis.generate();
    if let Ok(hl_syntax_tree) = syn::parse_file(&generated) {
        Ok(prettyplease::unparse(&hl_syntax_tree))
    } else {
        // better to generate something invalid than nothing for debugging purposes
        Ok(generated)
    }
}
