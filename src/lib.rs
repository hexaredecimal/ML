use crate::error::{CompilerError, Result};
use crate::ir::raw::{RawFunction, RecordType, TopLevel};
use gen::Jit;
use ir::raw::EnumType;
use nom::error::convert_error;
use std::collections::{HashMap, HashSet};
use std::mem;

pub mod config;
pub mod error;
mod gen;
mod ir;
mod parser;

pub fn compile_and_run(config: config::Config) -> Result<String> {
    let program = std::fs::read_to_string(config.file).unwrap();
    let toplevels = parse(program.as_str())?;

    let mut functions: Vec<RawFunction> = vec![];
    let mut records: Vec<RecordType> = vec![];
    let mut enums: Vec<EnumType> = vec![]; 
    for top in toplevels {
        match top {
            TopLevel::RawFunction {
                name,
                root,
                args,
                ty,
            } => functions.push(RawFunction {
                name,
                root,
                args,
                ty,
            }),
            TopLevel::RecordType { name, fields } => records.push(RecordType { name, fields }),
            TopLevel::EnumType { name, fields } => enums.push(EnumType { name, fields }),
        }
    }

    let mut uniq = HashSet::new();
    if !functions.iter().all(|x| uniq.insert(x.name.clone())) {
        panic!("Duplicate function name");
    }


    let mut ts: HashMap<String, RecordType> = HashMap::new();
    let mut es: HashMap<String, EnumType> = HashMap::new();
    
    for record in records.clone() {
        ts.insert(record.name.clone(), record.clone());
    }

    for en in enums.clone() {
        es.insert(en.name.clone(), en.clone()); 
    }
    let mut ctx =
        ir::sem::SemContext::from_funs(functions.iter().map(|f| (f.name.clone(), f.ty.clone())));

    ctx.records = ts.clone();
    ctx.enums = es.clone(); 

    let mut typed_functions = Vec::new();
    for func in functions.into_iter() {
        typed_functions.push(ir::sem::SemFunction::analyze(func, &mut ctx)?);
    }


    let mut jit = Jit::new();
    jit.records = ts.clone();
    jit.enums = es.clone(); 

    let ens = jit.process_enumns(enums)?; 
    let rcs = jit.process_records(records, true)?;
    let code_ptr = jit.compile(&typed_functions)?;

    Ok(format!("{}\n{}\n{}", ens, rcs, code_ptr))
}

fn parse(text: &str) -> Result<Vec<TopLevel>> {
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
