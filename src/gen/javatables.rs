
use crate::error::{CompilerError, Result};
use crate::ir::raw::{RecordType, EnumType, EnumField, Alias};
use crate::ir::sem::*;
use crate::ir::{self, Type};
use std::collections::HashMap;

use super::utils::Utils;
use super::javacodegen::JavaBackend;


#[derive(Clone)]
#[allow(unused_tuple_struct_fields)]
pub struct JavaTables {
    pub records: HashMap<String, RecordType>,
    pub enums: HashMap<String, EnumType>,
    pub aliases: HashMap<String, Alias>,
}

impl JavaTables {
    pub fn new() -> Self {
        Self {
            records: HashMap::new(),
            enums: HashMap::new(),
            aliases: HashMap::new()
        }
    }

    pub fn process_enumns(&mut self, records: Vec<EnumType>, ctx: &mut SemContext) -> Result<String> {
        let mut recs: String = String::new();

        for record in records.clone() {
            let name = record.name.clone();
            let args = record.fields.clone();
            let args: Vec<String> = args
                .into_iter()
                .map(|f| {
                    match f {
                        EnumField::Rec(rec) => {
                            let recc = self.process_records(vec![rec], false, ctx).unwrap();
                            let recc = recc.trim_end(); 
                            let recc = format!("\t{recc} implements {name} {}", "{}");
                            recc
                        }, 
                        EnumField::Id(_name) => format!("\trecord {} () implements {} {}", _name, name, "{}"), 
                    }
                })
                .collect();

            let args = args.join("\n");
            recs.push_str(format!("sealed interface {} {}\n{}\n{}", name, "{", args, "}").as_str())
        }

        Ok(recs)
    }


    pub fn process_records(&mut self, records: Vec<RecordType>, close: bool, ctx: &mut SemContext) -> Result<String> {
        let mut recs: String = String::new();

        for record in records.clone() {
            let name = record.name.clone();
            let args = record.fields.clone();
            let args: Vec<String> = args
                .into_iter()
                .map(|f| {
                    let (name, ty) = f;
                    let ty = self.clone().real_type(&ty, ctx).unwrap();
                    format!("{} {}", ty, name)
                })
                .collect();

            let args = args.join(", ");
            recs.push_str(format!("record {} ({}) {}", name, args, if close {"{}\n"} else {" "}).as_str())
        }

        Ok(recs)
    }

    pub fn compile(&mut self, funcs: &[SemFunction], ctx: &mut SemContext) -> Result<String> {
        let mut s = String::new();
        for fun in funcs {
            let a = self.translate_function(fun, ctx)?;
            s.push_str(a.as_str());
            s.push('\n');
        }
        Ok(s.clone())
    }

    fn translate_function(&mut self, func: &SemFunction, ctx: &mut SemContext) -> Result<String> {
        let name: String = func.name().to_string();

        let len = func.args().len();

        let args: Vec<Result<String>> = func
            .args()
            .iter()
            .enumerate()
            .map(|(i, f)| {
                let (name, ty) = f;
                match ty {
                    ir::Type::VarArgs(inner) => {
                        if i < len - 1 {
                            let e: Result<CompilerError> =
                                Result::Err(CompilerError::BackendError(
                                    "VarArgs cannot be used in the middle of paramaters"
                                        .to_string(),
                                ));
                            println!("{:?}", e);
                            std::process::exit(1);
                        }

                        let ty_list = match *inner.clone() {
                            Type::YourType(x) => {
                                if x.is_empty() {
                                    "Object... ".to_string()
                                } else {
                                    format!("{x}... ")
                                }
                            }
                            other => {
                                let ty = self.clone().real_type(&other, ctx).unwrap();
                                format!("{ty}... ")
                            }
                        };

                        Ok(format!("{ty_list} var_args"))
                    }
                    _ => {
                        let ty = self.clone().real_type(ty, ctx).unwrap();
                        Ok(format!("{} {}", ty, name))
                    }
                }
            })
            .collect();

        let args: Vec<String> = args.into_iter().map(|f| f.unwrap()).collect();
        let args = args.join(", ");

        let ret_type = if let ir::Type::Function(ret, _args) = func.ty() {
            self.clone().real_type(ret, ctx)?
        } else {
            return Err(CompilerError::BackendError(format!(
                "{:?} is not a function type",
                func.ty()
            )));
        };

        // declare function block level variables
        let mut variables: HashMap<String, Type> = HashMap::new();
        for (name, _ty) in func.args().iter() {
            let name = if name.is_empty() {
                "var_args".to_string()
            } else {
                name.clone()
            };
            variables.insert(name, _ty.clone());
        }

        let mut trans = JavaBackend::new(self.clone());

        let root = func.root().clone();
        let return_value = trans.translate_expr(func.root(), &mut variables, ctx)?;

        let mut s = if name == "main" {
            format!("{} {}({}) ", "void", name, args,)
        } else {
            format!("{} {}({}) ", ret_type, name, args,)
        };

        s.push_str(" {\n");

        s.push_str("// Variables \n");
        s.push_str(trans.vars.as_str());
        s.push('\n');
        // Patch all collected lamda blocks

        s.push_str("// Block patching area\n");
        s.push_str(trans.blocks.as_str());
        s.push('\n');

        if name == "main" {
            let return_value = match ret_type.as_str() {
                "Integer" => {
                    match root.expr() {
                        SemExpression::Block(_) => {
                            let sq = return_value.clone();
                            let sq: Vec<_> = sq.split('\n').collect();
                            let len = sq.len();

                            let mut p = String::new(); 
                            let rest = sq.get(0..len - 1).unwrap();
                            for st in rest {
                                p.push_str(st);
                                p.push('\n');
                            }

                            let last = sq.last().unwrap();
                            format!("{p}System.exit({last})") 
                        }
                        _ => format!("System.exit({})", return_value)
                    }
                }
                _ => return_value
            }; 

            let last_expr = format!("try {}\n\t{};\n{}\tcatch(Exception e) {}", "{", return_value, "}", "{ Intrinsic.panic(e.getMessage()); }"); 
            match func.root().expr() {
                SemExpression::Unit => (), 
                _ => s.push_str(format!("{};\n", last_expr).as_str()), 
            }
            s.push_str("\nreturn;\n");
        } else {
            match root.expr() {
                SemExpression::Block(_) => {

                    let sq = return_value.clone();
                    let sq: Vec<_> = sq.split('\n').collect();
                    let len = sq.len();

                    let rest = sq.get(0..len - 1).unwrap();
                    for st in rest {
                        s.push_str(st);
                        s.push('\n');
                    }

                    let last = sq.last().unwrap();
                    s.push_str("return ");
                    s.push_str(last);
                    s.push(';');
                }
                _ => {
                    s.push_str("return ");
                    s.push_str(return_value.as_str());
                    s.push(';');
                }
            }
        }
        s.push_str("\n}\n");
        Ok(s)
    }


    pub fn real_type(self, ty: &ir::Type, _ctx: &mut SemContext) -> Result<String> {
        match ty {
            ir::Type::Lambda(_ret, args) => {
                let len = args.len(); 
                let ret = self.clone().real_type(_ret, _ctx)?; 
                let args: Vec<_> = args.iter().map(|f| self.clone().real_type(f, _ctx).unwrap()).collect();
                let args = if len == 0 {
                    "".to_string()
                } else {
                    format!(", {}", args.join(", "))
                };

                Ok(format!("Lambda_{len}<{ret}{}>", args))
            }
            ir::Type::Any => Ok("Object".to_string()),
            ir::Type::Unit => Ok("Void".to_string()),
            ir::Type::String => Ok("String".to_string()),
            ir::Type::Usize => Ok("Integer".to_string()),
            ir::Type::Int => Ok("Integer".to_string()),
            ir::Type::Char => Ok("Character".to_string()),
            ir::Type::Bool => Ok("Boolean".to_string()),
            ir::Type::Float => Ok("Float".to_string()),
            ir::Type::Double => Ok("Double".to_string()),
            ir::Type::Lifter(inner) => Ok(inner.clone()),
            ir::Type::Long => Ok("Long".to_string()),
            ir::Type::Short => Ok("Short".to_string()),
            ir::Type::Byte => Ok("Byte".to_string()),
            /*
            ir::Type::Int8 => Ok("i8".to_string()),
            ir::Type::Int16 => Ok("i16".to_string()),
            ir::Type::Int32 => Ok("i32".to_string()),
            ir::Type::Int64 => Ok("i64".to_string()),
            ir::Type::Int128 => Ok("i128".to_string()), */
            ir::Type::Array(_num, _inner) => {
                let inner = *_inner.clone();
                let t = self.real_type(&inner, _ctx)?;
                let e:String = _num.to_string();
                Ok(format!("{}[{}]", t, if *_num == 0 { "".to_string() } else { e }))
            }
            ir::Type::List(_inner) => {
                let inner = *_inner.clone();
                let t = self.real_type(&inner, _ctx)?;
                Ok(format!("{}[]", t))
            }
            ir::Type::EnumType(name, _arg) => {
                if Utils::enum_type_exists(&self.enums, name).is_ok() {
                    let name = name.clone(); 
                    let arg = _arg.clone(); 
                    let enm = self.enums.get(&name).unwrap();

                    if name != enm.name {
                        return Err(CompilerError::BackendError(format!(
                            "Invalid enum type {:?}",
                            name.clone()
                        )));
                    };

                    let mut found = false; 
                    for _arg in enm.fields.clone() {
                        match _arg {
                            EnumField::Rec(rc) => {
                                let nm = rc.name.clone();
                                if nm == arg {
                                    found = true; 
                                }
                           }, 
                            EnumField::Id(s) => {
                                if s == arg {
                                    found = true; 
                                }
                            },
                        }
                    }

                    if !found {
                        return Err(CompilerError::BackendError(format!(
                            "Invalid enum field lookup for field `{}`, in enum type {}",
                            _arg, 
                            name.clone()
                        )));
                    }
                    
                    Ok(format!("{}.{}", name, _arg))
                } else {
                    Err(CompilerError::BackendError(format!("{} is not an enum", name))) 
                }
            }
            ir::Type::YourType(t) => {
                let jit = self.clone();
                if Utils::record_type_exists(&self.records, t).is_ok() || Utils::enum_type_exists(&self.enums, t).is_ok()  {
                    Ok(t.clone())
                } else if jit.aliases.contains_key(t) {
                    let alias = jit.aliases.get(t).unwrap(); 
                    self.real_type(&alias.value, _ctx)
                } else {
                    Err(CompilerError::BackendError(format!("Invalid struct/enum/alias type {t}")))
                }
            }
            _ => Err(CompilerError::BackendError(format!(
                "Unsupported type {:?}",
                ty
            ))),
        }
    }
}
