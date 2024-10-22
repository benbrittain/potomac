use std::{collections::HashMap, env, fs, path::PathBuf, process::Command};

use anyhow::{anyhow, Error};
use regex::Regex;

fn get_mlir_properties(llvm_build_dir: &str) -> Result<HashMap<String, Vec<String>>, Error> {
    let cmake_contents =
        fs::read_to_string(format!("{llvm_build_dir}/lib/cmake/mlir/MLIRConfig.cmake"))?;

    let mut lookup: HashMap<String, Vec<String>> = HashMap::new();
    let re = Regex::new(r"set\((\w*) .([\S]*).\)")?;
    for (_, [var, val]) in re.captures_iter(&cmake_contents).map(|c| c.extract()) {
        lookup.insert(
            var.to_string(),
            val.split(";").map(|s| s.to_string()).collect(),
        );
    }

    let re = Regex::new(r"set_property\(GLOBAL PROPERTY (\w*) .([\S]*).\)")?;
    for (_, [var, val]) in re.captures_iter(&cmake_contents).map(|c| c.extract()) {
        lookup.insert(
            var.to_string(),
            val.split(";").map(|s| s.to_string()).collect(),
        );
    }

    Ok(lookup)
}

fn strip_name(name: &str) -> Result<&str, Error> {
    if let Some(name2) = name.strip_prefix("lib") {
        name2
            .strip_suffix(".a")
            .ok_or(anyhow!("Invalid lib: {name}"))
    } else {
        Err(anyhow!("Invalid lib: {name}"))
    }
}

fn llvm_config(arg: &str) -> Result<String, Error> {
    let llvm_build_dir = env::var("LLVM_BUILD")
        .map_err(|_| anyhow!("LLVM_BUILD env variable must be set and pointing to a built llvm"))?;
    let out = Command::new("sh")
        .arg("-c")
        .arg(format!(
            "{llvm_build_dir}/bin/llvm-config --link-static --{arg}"
        ))
        .output()?
        .stdout;
    Ok(std::str::from_utf8(&out)?.to_string().trim().to_string())
}

fn main() -> Result<(), Error> {
    let llvm_build_dir = env::var("LLVM_BUILD")
        .map_err(|_| anyhow!("LLVM_BUILD env variable must be set and pointing to a built llvm"))?;

    let properties = dbg!(get_mlir_properties(&llvm_build_dir)?);
    let mlir_include_dir = &properties.get("MLIR_INCLUDE_DIR").unwrap()[0];

    println!("cargo:rustc-link-search={llvm_build_dir}/lib");
    for entry in std::fs::read_dir(format!("{llvm_build_dir}/lib"))? {
        let entry = entry?.path();
        let name = entry.file_name().unwrap().to_str().unwrap();
        if name.starts_with("libMLIR")
            && name.ends_with(".a")
            && !name.contains("Main")
            && name != "libMLIRSupportIndentedOstream.a"
        {
            println!("cargo:rustc-link-lib=static={}", strip_name(name)?);
        }
    }

    for lib in llvm_config("libnames")?.split(" ") {
        println!("cargo:rustc-link-lib={}", strip_name(lib)?);
    }

    println!("cargo:rustc-link-lib=rt");
    println!("cargo:rustc-link-lib=dl");
    println!("cargo:rustc-link-lib=m");
    println!("cargo:rustc-link-lib=z");
    println!("cargo:rustc-link-lib=zstd");
    println!("cargo:rustc-link-lib=stdc++");

    let bindings = bindgen::Builder::default().clang_arg(format!("-I{mlir_include_dir}"));
    let bindings = properties
        .get("MLIR_INCLUDE_DIRS")
        .unwrap()
        .into_iter()
        .fold(bindings, |bindings, dir| {
            bindings.clang_arg(format!("-I{dir}"))
        });

    let bindings = bindings
        .header("wrapper.h")
        .parse_callbacks(Box::new(bindgen::CargoCallbacks::new()))
        .generate()?;

    let out_path = PathBuf::from(env::var("OUT_DIR")?);
    bindings.write_to_file(out_path.join("bindings.rs"))?;
    Ok(())
}
