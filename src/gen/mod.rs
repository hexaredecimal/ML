use crate::error::{CompilerError, Result};
use crate::ir;
use crate::ir::sem::*;
use std::collections::HashMap; 

use self::function::FunctionTranslator;

mod function;


#[derive(Clone)]
#[allow(unused_tuple_struct_fields)]
pub struct Jit {
    current_func: String, 
    args: String,
}

impl Jit {
    pub fn new() -> Self {
        Self {
            current_func: String::new(),
            args: String::new(),
        }
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

        let args: Vec<Result<String>> = func.args().into_iter().enumerate().map(|(i, f)| {
            let (name, ty) = f;
            let ty = match ty {
                ir::Type::VarArgs => {
                    if i < len -1 {
                        let e: Result<CompilerError> = Result::Err(CompilerError::BackendError("VarArgs cannot be used in the middle of paramaters".to_string()));
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
        }).collect(); 


        let args: Vec<String> = args.into_iter().map(|f| f.unwrap()).collect(); 
        let args = args.join(", "); 

        let mut ret_type = if let ir::Type::Function(ret, _args) = func.ty() {
            self.clone().real_type(ret)?
        } else {
            return Err(CompilerError::BackendError(format!(
                "{:?} is not a function type",
                func.ty()
            )));
        };

        // declare function block level variables
        let mut variables: HashMap<String, String> = HashMap::new();
        for (name, _ty) in func.args().iter(){
            variables.insert(name.clone(), name.clone()); 
        }

        let mut trans = FunctionTranslator::new(self.clone());

        let root = func.root().clone(); 
        let return_value = trans.translate_expr(func.root(), &mut variables)?;

        if name == "main" {
            ret_type = "void".to_string(); 
        }

        let mut s = format!("{} {}({}) ", ret_type, name, args,); 
        s.push_str(" {\n"); 
        

        s.push_str("// Variables \n"); 
        s.push_str(trans.vars.as_str());
        s.push_str("\n"); 
        // Patch all collected lamda blocks 
        
        s.push_str("// Block patching area\n"); 
        s.push_str(trans.blocks.as_str());
        s.push_str("\n"); 

        if name == "main" {
            s.push_str(return_value.as_str()); 
            s.push_str(";\nreturn;\n"); 

        } else {
            match root.expr() {
                SemExpression::Block(_) => {
                    let sq = return_value.clone(); 
                    let sq: Vec<_> = sq.split("\n").collect();
                    let len = sq.len(); 

                    let rest = sq.get(0 .. len - 1).unwrap(); 
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


    pub fn real_type(self, ty: &ir::Type)
     -> Result<String> {
            match ty {
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
                Ok(format!("[{}; {}]", t, _num))
            },
            ir::Type::List(_inner) => {
                let inner = *_inner.clone(); 
                let t = self.real_type(&inner)?;
                Ok(format!("&[{}]", t))
            },
            _ => Err(CompilerError::BackendError(format!(
                "Unsupported type {:?}",
                ty
            ))),
        }
    }
}

