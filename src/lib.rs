use crate::error::{CompilerError, Result};
use crate::ir::raw::{RawFunction, RecordType, TopLevel};
use gen::Jit;
use ir::raw::{EnumType, Import, Alias};
use nom::error::convert_error;
use std::collections::{HashMap, HashSet};

pub mod config;
pub mod error;
mod gen;
mod ir;
mod parser;


pub fn compile_file(input: String) -> Result<(Vec<RawFunction>, Vec<RecordType>, Vec<EnumType>)> {
    let program = match std::fs::read_to_string(input.to_string()) {
        Ok(x) => x, 
        Err(e) => {
            return Err(CompilerError::BackendError(format!("failed to import file with path: {}, with reason: {e}", input)));
        }
    };
    
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

    if imports.len() > 0 {
        for import in imports {
            let path = import.path.clone(); 
            let path = path.join("/");
            let path = format!("./{path}.sml"); 
            let (f, r, e) = compile_file(path)?;

            functions = [functions, f].concat(); 
            records = [records, r].concat(); 
            enums = [enums, e].concat(); 
        }
    }

    Ok((functions, records, enums))
}


pub fn compile_and_run(config: config::Config) -> Result<String> {
    let program = std::fs::read_to_string(config.file).unwrap();
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

    for import in imports {
        let path = import.path; 
        let path = path.join("/");
        let path = format!("./{path}.sml"); 
        let (f, r, e) = compile_file(path.clone())?;
        // println!("symbols imported from {:?}\nfuncs: {:?}\nrecords: {:?}\n,imports: {:?}", path, f, r, e);
    
        for fx in f.clone().into_iter() {
            let import_func_name = fx.name.clone();
            let mut is_found = false; 
            for func in functions.clone() {
                let func_name = func.name.clone();
                if import_func_name == func_name {
                    is_found = true; 
                }
            }

            if !is_found {
                functions.push(fx); 
            }
        }


        for rec in r.clone().into_iter() {
            let import_rec_name = rec.name.clone();
            let mut is_found = false; 
            for record in records.clone() {
                let rec_name = record.name.clone();
                if import_rec_name == rec_name {
                    is_found = true; 
                }
            }

            if !is_found {
                records.push(rec); 
            }
        }


        for enm in e.clone().into_iter() {
            let import_enm_name = enm.name.clone();
            let mut is_found = false; 
            for enmm in enums.clone() {
                let enum_name = enmm.name.clone();
                if import_enm_name == enum_name {
                    is_found = true; 
                }
            }

            if !is_found {
                enums.push(enm); 
            }
        }
        // functions = [f, functions].concat(); 
        // records = [records, r].concat(); 
        // enums = [enums, e].concat(); 
    }


    let mut uniq = HashSet::new();

/*    fn count_dups(name: String, fxs: Vec<RawFunction>) -> i32 {
        let mut count = 0; 
        for fx in fxs.into_iter() {
            let nm = fx.name.clone(); 
            if nm == name {
                count += 1;
            }
        }
        count
    }

    fn remove_dups(name: String, fxs: &mut Vec<RawFunction>) {
        let mut all_ones = true;
        for (i, fx) in fxs.clone().into_iter().enumerate() {
            let dups = count_dups(name.clone(), fxs.clone());
            if dups > 1 {
                all_ones = false; 
                fxs.remove(i);
            }
        }

        if !all_ones {
            remove_dups(name, fxs);
        }
    }*/
    

    if !functions.iter().all(|x| uniq.insert(x.name.clone())) {
        panic!("Duplicate function name");
    }


    let mut ts: HashMap<String, RecordType> = HashMap::new();
    let mut es: HashMap<String, EnumType> = HashMap::new();
    let mut ali: HashMap<String, Alias> = HashMap::new();
    
    for record in records.clone() {
        ts.insert(record.name.clone(), record.clone());
    }

    for en in enums.clone() {
        es.insert(en.name.clone(), en.clone()); 
    }

    for alias in aliases.clone() {
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
