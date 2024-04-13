use crate::error::{CompilerError, Result};
use crate::ir::raw::{RawFunction, RecordType, TopLevel};
use gen::Jit;
use ir::raw::{EnumType, Import, Alias};
use nom::error::convert_error;
use std::collections::{HashMap, HashSet};
use std::fs;
use rust_embed::RustEmbed;


pub mod config;
pub mod error;
pub mod manager;
mod gen;
mod ir;
mod parser;

#[derive(RustEmbed)]
#[folder = "lib"]
struct Asset;
type CompilationUnit = (Vec<RawFunction>, Vec<RecordType>, Vec<EnumType>, Vec<Alias>);
pub fn compile_file(config: &config::Config, input: String, cache: &mut Vec<String>) -> Result<CompilationUnit> {
    let program = match Asset::get(&input) {
        Some(data) =>  {
            let dt = data.clone(); 
            let dt = dt.data.clone();
            String::from_utf8(dt.to_vec()).unwrap()
        }, 
        None => {
            match std::fs::read_to_string(&input) {
                Ok(x) => x, 
                Err(e) => {
                    return Err(CompilerError::BackendError(format!("failed to import file with path: {}, with reason: {e}", input)));
                }
            }
        }
    };
    
    let toplevels = parse(&program)?;

    let mut functions: Vec<RawFunction> = vec![];
    let mut records: Vec<RecordType> = vec![];
    let mut enums: Vec<EnumType> = vec![]; 
    let mut imports: Vec<Import> = vec![]; 
    let mut aliases: Vec<Alias> = vec![]; 

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
            TopLevel::Import { path } => imports.push(Import { path }),
            TopLevel::Alias { name, value } => aliases.push(Alias { name, value })
        }
    }

    if !imports.is_empty() {
        for import in imports {
            let path = import.path.clone(); 
            let path = path.join("/");
            let mut path = format!("{path}.sml");

            for dir_path in &config.import_paths  {
                let _path = format!("{dir_path}{path}");
                if !fs::metadata(&_path).is_ok() {
                    continue;
                } else {
                    path = _path;
                    break;
                }
            }

            if !cache.contains(&path) {
                cache.push(path.clone());
                let (f, r, e, a) = compile_file(config, path, cache)?;
                functions = [functions, f].concat(); 
                records = [records, r].concat(); 
                enums = [enums, e].concat(); 
                aliases = [aliases, a].concat();
            } else {
                // TODO: log a message if verbosity is allowed
                // println!("{path} has been skipped");
            }

        }
    }

    Ok((functions, records, enums, aliases))
}


pub fn compile_and_run(config: &config::Config) -> Result<String> {
    let program = std::fs::read_to_string(&config.file).unwrap();
    let toplevels = parse(program.as_str())?;

    let mut functions: Vec<RawFunction> = vec![];
    let mut records: Vec<RecordType> = vec![];
    let mut enums: Vec<EnumType> = vec![]; 
    let mut imports: Vec<Import> = vec![]; 
    let mut aliases: Vec<Alias> = vec![]; 

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
            TopLevel::Import { path } => imports.push(Import { path }),
            TopLevel::Alias { name, value } => aliases.push(Alias { name, value })
        }
    }

    let mut cache: Vec<String> = Vec::new(); 


    'outer: for import in imports {
        let path = import.path.join("/");
        let mut path = format!("{path}.sml"); 

        for dir_path in &config.import_paths  {
            let _path = format!("{dir_path}{path}");
            if !fs::metadata(&_path).is_ok() {
                continue;
            } else {
                path = _path;
                break;
            }
        }


        if cache.contains(&path) {
            continue;
        }

        cache.push(path.clone());
        let (f, r, e, a) = compile_file(config, path, &mut cache)?;
        // println!("symbols imported from {:?}\nfuncs: {:?}\nrecords: {:?}\n,imports: {:?}", path, f, r, e);
    
        for fx in &f {
            let import_func_name = &fx.name;
            let mut is_found = false; 
            for func in &functions {
                let func_name = &func.name;
                if *import_func_name == *func_name {
                    is_found = true; 
                }
            }

            if !is_found {
                functions.push(fx.clone()); 
            }
        }


        for rec in &r {
            let import_rec_name = &rec.name;
            let mut is_found = false; 
            for record in &records {
                let rec_name = &record.name;
                if *import_rec_name == *rec_name {
                    is_found = true; 
                }
            }

            if !is_found {
                records.push(rec.clone()); 
            }
        }


        for enm in &e {
            let import_enm_name = &enm.name;
            let mut is_found = false; 
            for enmm in &enums {
                let enum_name = &enmm.name;
                if *import_enm_name == *enum_name {
                    is_found = true; 
                }
            }

            if !is_found {
                enums.push(enm.clone()); 
            }
        }

        for ali in &a {
            let import_ali_name = &ali.name;
            let mut is_found = false; 
            for alias in aliases.clone() {
                let ali_name = &alias.name;
                if *import_ali_name == *ali_name {
                    is_found = true; 
                }
            }

            if !is_found {
                aliases.push(ali.clone()); 
            }
        }
    }

    let mut uniq = HashSet::new();
    if !functions.iter().all(|x| uniq.insert(x.name.clone())) {
        panic!("Duplicate function name");
    }

    let mut ts: HashMap<String, RecordType> = HashMap::new();
    let mut es: HashMap<String, EnumType> = HashMap::new();
    let mut ali: HashMap<String, Alias> = HashMap::new();
    
    for record in &records {
        ts.insert(record.name.clone(), record.clone());
    }

    for en in &enums {
        es.insert(en.name.clone(), en.clone()); 
    }

    for alias in &aliases {
        ali.insert(alias.name.clone(), alias.clone()); 
    }

    let mut ctx = ir::sem::SemContext::from_funs(functions.iter().map(|f| (f.name.clone(), f.ty.clone())));

    ctx.records = ts.clone();
    ctx.enums = es.clone(); 
    ctx.aliases = ali.clone(); 

    let mut typed_functions = Vec::new();
    for func in functions.into_iter() {
        typed_functions.push(ir::sem::SemFunction::analyze(func, &mut ctx)?);
    }

    let mut jit = Jit::new();
    jit.records = ts.clone();
    jit.enums = es.clone(); 
    jit.aliases = ali.clone(); 

    let ens = jit.process_enumns(enums, &mut ctx)?; 
    let rcs = jit.process_records(records, true, &mut ctx)?;
    let code_ptr = jit.compile(&typed_functions, &mut ctx)?;

    Ok(format!("{}\n{}\n{}", ens, rcs, code_ptr))
}

fn parse(text: &str) -> Result<Vec<TopLevel>> {
    let (_, text) = match parser::comments_ommited(text) {
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
