use crate::error::{CompilerError, Result};
use crate::ir::raw::{RecordType,EnumType, EnumField};
use crate::ir::sem::*;
use crate::ir::{self, Type};
use std::collections::HashMap;

use self::function::FunctionTranslator;

mod function;

#[derive(Clone)]
#[allow(unused_tuple_struct_fields)]
pub struct Jit {
    pub records: HashMap<String, RecordType>,
    pub enums: HashMap<String, EnumType>,
}

impl Jit {
    pub fn new() -> Self {
        Self {
            records: HashMap::new(),
            enums: HashMap::new(),
        }
    }


    fn record_contains_field(self, name: String, rec: Vec<(String, Type)>) -> (bool, usize) {
        for (i, (n, _)) in rec.into_iter().enumerate() {
            if n == name {
                return (true, i)
            }
        }
        (false, 1 as usize)
    }



    pub fn process_enumns(&mut self, records: Vec<EnumType>) -> Result<String> {
        let mut recs: String = String::new();

        for record in records.clone() {
            let name = record.name.clone();
            let args = record.fields.clone();
            let args: Vec<String> = args
                .into_iter()
                .map(|f| {
                    match f {
                        EnumField::Rec(rec) => {
                            let recc = self.process_records(vec![rec], false).unwrap();
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


    pub fn process_records(&mut self, records: Vec<RecordType>, close: bool) -> Result<String> {
        let mut recs: String = String::new();

        for record in records.clone() {
            let name = record.name.clone();
            let args = record.fields.clone();
            let args: Vec<String> = args
                .into_iter()
                .map(|f| {
                    let (name, ty) = f;
                    let ty = self.clone().real_type(&ty).unwrap();
                    format!("{} {}", ty, name)
                })
                .collect();

            let args = args.join(", ");
            recs.push_str(format!("record {} ({}) {}", name, args, if close {"{}\n"} else {" "}).as_str())
        }

        Ok(recs)
    }

    pub fn compile(&mut self, funcs: &[SemFunction]) -> Result<String> {
        let mut s = String::new();
        for fun in funcs {
            let a = self.translate_function(fun)?;
            s.push_str(a.as_str());
            s.push_str("\n");
        }
        Ok(s.clone())
    }

    fn translate_function(&mut self, func: &SemFunction) -> Result<String> {
        let name: String = func.name().to_string();

        let len = func.args().len();

        let args: Vec<Result<String>> = func
            .args()
            .into_iter()
            .enumerate()
            .map(|(i, f)| {
                let (name, ty) = f;
                let ty = match ty {
                    ir::Type::VarArgs => {
                        if i < len - 1 {
                            let e: Result<CompilerError> =
                                Result::Err(CompilerError::BackendError(
                                    "VarArgs cannot be used in the middle of paramaters"
                                        .to_string(),
                                ));
                            println!("{:?}", e);
                            std::process::exit(1);
                        }
                        Ok(format!("Object ...var_args"))
                    }
                    _ => {
                        let ty = self.clone().real_type(ty).unwrap();
                        Ok(format!("{} {}", ty, name))
                    }
                };
                ty
            })
            .collect();

        let args: Vec<String> = args.into_iter().map(|f| f.unwrap()).collect();
        let args = args.join(", ");

        let ret_type = if let ir::Type::Function(ret, _args) = func.ty() {
            self.clone().real_type(ret)?
        } else {
            return Err(CompilerError::BackendError(format!(
                "{:?} is not a function type",
                func.ty()
            )));
        };

        // declare function block level variables
        let mut variables: HashMap<String, Type> = HashMap::new();
        for (name, _ty) in func.args().iter() {
            let name = if name.is_empty() || name == "" {
                "var_args".to_string()
            } else {
                name.clone()
            };
            variables.insert(name, _ty.clone());
        }

        let mut trans = FunctionTranslator::new(self.clone());

        let root = func.root().clone();
        let return_value = trans.translate_expr(func.root(), &mut variables)?;

        let mut s = if name == "main" {
            format!("{} {}({}) ", "void", name, args,)
        } else {
            format!("{} {}({}) ", ret_type, name, args,)
        };

        s.push_str(" {\n");

        s.push_str("// Variables \n");
        s.push_str(trans.vars.as_str());
        s.push_str("\n");
        // Patch all collected lamda blocks

        s.push_str("// Block patching area\n");
        s.push_str(trans.blocks.as_str());
        s.push_str("\n");

        if name == "main" {
            let return_value = match ret_type.as_str() {
                "Integer" => {
                    match root.expr() {
                        SemExpression::Block(_) => {
                            let sq = return_value.clone();
                            let sq: Vec<_> = sq.split("\n").collect();
                            let len = sq.len();

                            let mut p = String::new(); 
                            let rest = sq.get(0..len - 1).unwrap();
                            for st in rest {
                                p.push_str(*st);
                                p.push_str("\n");
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
                    let sq: Vec<_> = sq.split("\n").collect();
                    let len = sq.len();

                    let rest = sq.get(0..len - 1).unwrap();
                    for st in rest {
                        s.push_str(*st);
                        s.push_str("\n");
                    }

                    let last = sq.last().unwrap();
                    s.push_str("return ");
                    s.push_str(last);
                    s.push_str(";");
                }
                _ => {
                    s.push_str("return ");
                    s.push_str(return_value.as_str());
                    s.push_str(";");
                }
            }
        }
        s.push_str("\n}\n");
        Ok(s)
    }

    pub fn is_sys_type(self, ty: &ir::Type) -> bool {
        match ty {
            ir::Type::Any => true,
            ir::Type::Unit => true,
            ir::Type::String => true,
            ir::Type::Usize => true,
            ir::Type::Int => true,
            ir::Type::Char => true,
            ir::Type::Bool => true,
            ir::Type::Float => true,
            ir::Type::Double => true,
            /*
            ir::Type::Int8 => true,
            ir::Type::Int16 => true,
            ir::Type::Int32 => true,
            ir::Type::Int64 => true,
            ir::Type::Int128 => true, */
            ir::Type::Array(_num, _inner) => true,
            ir::Type::List(_inner) => true,
            _ => false,
        }
    }

    pub fn extract_record_type(self, name: String, ty: Vec<EnumField>) -> Option<EnumField> {
        for t in ty.clone() {
            match t.clone() {
                EnumField::Rec(rec) => {
                    if rec.name == name {
                        return Some(t.clone());
                    }
                }

                EnumField::Id(s) => {
                    if name == s {
                        return Some(t.clone()); 
                    }
                }
            }
        }
        None
    }

    pub fn enum_type_exists(self, ty: String) -> Result<bool> {
        if !self.enums.contains_key(&ty) {
            Err(CompilerError::BackendError(format!(
                "Invalid enum type {}",
                ty
            )))
        } else {
            Ok(true)
        }
    }

    pub fn record_type_exists(self, ty: String) -> Result<bool> {
        if !self.records.contains_key(&ty) {
            Err(CompilerError::BackendError(format!(
                "Invalid struct type {}",
                ty
            )))
        } else {
            Ok(true)
        }
    }

    pub fn real_type(self, ty: &ir::Type) -> Result<String> {
        match ty {
            ir::Type::Lambda(_ret, args) => {
                let len = args.len(); 
                let ret = self.clone().real_type(_ret)?; 
                let args: Vec<_> = args.into_iter().map(|f| self.clone().real_type(f).unwrap()).collect(); 
                Ok(format!("Lambda_{len}<{ret}, {}>", args.join(", ")))
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
            /*
            ir::Type::Int8 => Ok("i8".to_string()),
            ir::Type::Int16 => Ok("i16".to_string()),
            ir::Type::Int32 => Ok("i32".to_string()),
            ir::Type::Int64 => Ok("i64".to_string()),
            ir::Type::Int128 => Ok("i128".to_string()), */
            ir::Type::Array(_num, _inner) => {
                let inner = *_inner.clone();
                let t = self.real_type(&inner)?;
                let e:String = String::from(_num.to_string());
                Ok(format!("{}[{}]", t, if *_num == 0 { "".to_string() } else { e }))
            }
            ir::Type::List(_inner) => {
                let inner = *_inner.clone();
                let t = self.real_type(&inner)?;
                Ok(format!("{}[]", t))
            }
            ir::Type::EnumType(name, _arg) => {
                let jit = self.clone();
                if jit.enum_type_exists(name.clone()).is_ok() {
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

                    if found == false {
                        return Err(CompilerError::BackendError(format!(
                            "Invalid enum field lookup for field `{}`, in enum type {}",
                            _arg, 
                            name.clone()
                        )));
                    }
                    
                    Ok(format!("{}.{}", name, _arg))
                } else {
                    return Err(CompilerError::BackendError(format!("{} is not an enum", name))); 
                }
            }
            ir::Type::UserType(t) => {
                let jit = self.clone();
                if self.record_type_exists(t.clone()).is_ok() {
                    Ok(t.clone())
                } else if jit.enum_type_exists(t.clone()).is_ok() {
                    Ok(t.clone())
                } else {
                    Err(CompilerError::BackendError(format!(
                        "Invalid struct/enum/alias type {:?}",
                        t.clone()
                    )))
                }
            }
            _ => Err(CompilerError::BackendError(format!(
                "Unsupported type {:?}",
                ty
            ))),
        }
    }
}
