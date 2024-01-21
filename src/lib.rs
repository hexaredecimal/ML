use crate::error::{CompilerError, Result};
use crate::ir::raw::RawFunction;
use gen::Jit;
use nom::error::convert_error;
use std::collections::HashSet;
use std::mem;

pub mod error;
mod gen;
mod ir;
mod parser;
pub mod config;

pub fn compile_and_run(config: config::Config) -> Result<String> {
    
    let file_name = config.file.clone(); 

    if file_name.ends_with(".sml") == false {
        println!("Invalid input file. expected a file with `.sml` extension but found `{}`", file_name.clone()); 
        std::process::exit(1); 
    }

    let splits: Vec<_> = file_name.split(".sml").collect(); 

    let out_file = splits.first().unwrap();
    let out_file = out_file.to_string(); 

    let program = std::fs::read_to_string(config.file).unwrap(); 
    let functions = parse(program.as_str())?;

    let mut uniq = HashSet::new();
    if !functions.iter().all(|x| uniq.insert(x.name.clone())) {
        panic!("Duplicate function name");
    }

    let mut ctx =
        ir::sem::SemContext::from_funs(functions.iter().map(|f| (f.name.clone(), f.ty.clone())));
    let mut typed_functions = Vec::new();
    for func in functions.into_iter() {
        typed_functions.push(ir::sem::SemFunction::analyze(func, &mut ctx)?);
    }

    let mut jit = Jit::new();
    let code_ptr = jit.compile(&typed_functions)?;

    Ok(code_ptr)
}

fn parse(text: &str) -> Result<Vec<RawFunction>> {
    let (_, text) = match parser::comments_ommited(&text) {
        Ok(res) => res,
        Err(nom::Err::Incomplete(n)) => {
            return Err(CompilerError::Syntax(format!(
                "Undertermined syntax. More input needed to be sure: {:?}",
                n
            )));
        }
        Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
            return Err(CompilerError::Syntax(convert_error(text, e)));
        }
    };

    let (_, functions) = match parser::program(&text) {
        Ok(res) => res,
        Err(nom::Err::Incomplete(n)) => {
            return Err(CompilerError::Syntax(format!(
                "Undertermined syntax. More input needed to be sure: {:?}",
                n
            )));
        }
        Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
            return Err(CompilerError::Syntax(convert_error(text.as_str(), e)));
        }
    };
    Ok(functions)
}
