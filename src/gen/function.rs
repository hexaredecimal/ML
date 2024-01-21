use crate::ir::{self, Type};
use crate::ir::sem::*;
use crate::error::CompilerError; 
use crate::Jit; 

use std::collections::hash_map::HashMap; 


pub(crate) struct FunctionTranslator  {
    jit: Jit, 
    block_count: i32,
    pub blocks: String,
    pub vars: String
}

impl FunctionTranslator {
    pub fn new(
        jit: Jit, 
    ) -> Self {
        Self {
            jit, 
            blocks: String::new(),
            vars: String::new(),
            block_count: 0, 
        }
    }

    pub fn translate_expr(&mut self, node: &SemNode, scope: &mut HashMap<String, String>) -> Result<String, CompilerError> {
        let real_ty = self.jit.clone().real_type(node.ty())?;

        match node.expr() {
            SemExpression::Embed(x) => {
                let e = self.translate_expr(x, scope)?;
                let e: Vec<_> = e.split("\n").collect();
                let e: Vec<_> = e.into_iter().map(|f| {
                    f.to_string()
                }).collect();
                let e = e.join("\n"); 
                let len = e.len(); 
                let e = e.get(1 .. len -1).unwrap_or(""); 
                Ok(e.to_string())
            }
            SemExpression::String(x) => Ok(format!("\"{x}\"")),
            SemExpression::Unit => Ok(format!("Void.Unit")), 
            SemExpression::Null(t) => {
                //let t = real_type(t, self.module).unwrap();
                todo!()
            }
            SemExpression::Cast(e, t) => {
                let ty = self.jit.clone().real_type(t)?;
                let e = self.translate_expr(e, scope)?; 

                Ok(format!("({}) as {}", e, ty))
            }
            SemExpression::Abs(x) => {
                todo!()
            }
            SemExpression::NullCheck(a) => {
                let a = a.clone(); 
                let e = self.translate_expr(&a, scope)?; 
                Ok(format!("{} == nul", e))
            }
            SemExpression::Block(values) => {
                let len = values.len(); 
                if len == 0 {
                    println!("Invalid block with 0 expressions"); 
                    panic!(); 
                }

                let rest = values.get(0 .. len - 1).unwrap();
                
                let mut block = String::from("{\n"); 

                for val in rest.into_iter() {
                    let v = val.clone(); 
                    let e = self.translate_expr(&v, scope)?; 
                    block.push_str("\t"); 
                    block.push_str(e.as_str()); 
                    match val.expr() {
                        SemExpression::Embed(_) => block.push_str("\n"),
                        SemExpression::Val(_, _) =>(),
                        SemExpression::Lets(_, _) => block.push_str(";\n"),  
                        _ => block.push_str(";\n"), 
                    }; 
                }

                let last = values.last().unwrap();
                let tmp = last.clone(); 
                let e = self.translate_expr(last, scope)?;

                match tmp.expr() {
                    SemExpression::Embed(_) => {
                        block.push_str(e.as_str()); 
                        block.push_str(";\n}"); 
                    }
                    _ => {
                        block.push_str("\treturn "); 
                        block.push_str(e.as_str()); 
                        block.push_str(";\n}"); 
                    }
                }
                let h = format!("() -> {}", block);
                let var = format!("Block<{real_ty}> block_{} = {};\n", self.block_count, h);

                let s = format!("{}\n\tblock_{}.run()", var.as_str(), self.block_count);
                self.block_count += 1; 
                Ok(s)
            }
            SemExpression::Match(cond, cases) => {
                let c = self.translate_expr(cond, scope)?;

                let len = cases.len(); 
                let rest = cases.get(0 .. len - 1).unwrap();
                let rest = rest.to_vec(); 

                let mut rest: Vec<_> = rest.into_iter().map(|f| {
                    let (co, bo) = f;
                    let co = self.translate_expr(&co, scope).unwrap();
                    let bo = self.translate_expr(&bo, scope).unwrap();
                    format!("\tcase {} -> {}", co, bo)
                }).collect();

                let (_, bo) = cases.last().unwrap();
                let e = self.translate_expr(&bo, scope)?;
                rest.push(format!("\tdefault -> {};", e)); 
                let rest = rest.join(";\n"); 

                let mut ret = format!("switch ({}) ", c);
                ret.push_str("{\n");
                ret.push_str(rest.as_str()); 
                ret.push_str("\n}\n"); 
                
                Ok(ret)
            }
            SemExpression::FunCall(func_name, args) => {
                self.translate_funcall(func_name, args, scope)
            }
            SemExpression::Conditional(cond, then, alt) => {
                self.translate_conditional(cond, then, alt, scope)
            }
            SemExpression::BinaryOp(op, left, right) => {
                let left_val = self.translate_expr(left, scope)?;
                let right_val = self.translate_expr(right, scope)?;


                match (op, left.ty(), right.ty()) {
                    (ir::BinaryOp::Equal, 
                        ir::Type::Int | ir::Type::Char | ir::Type::Int8 | ir::Type::Int16 | ir::Type::Int32 | ir::Type::Int64 | ir::Type::Int128, 
                        ir::Type::Int | ir::Type::Char | ir::Type::Int8 | ir::Type::Int16 | ir::Type::Int32 | ir::Type::Int64 | ir::Type::Int128, 
                    ) => {
                        Ok(format!("{} == {}", left_val, right_val))
                    }
                    (ir::BinaryOp::Equal, 
                        ir::Type::Bool, 
                        ir::Type::Bool
                    ) => {
                        Ok(format!("{} == {}", left_val, right_val))
                    }
                    (ir::BinaryOp::Equal,
                        ir::Type::Float | ir::Type::Double,
                        ir::Type::Float | ir::Type::Double,
                    ) => {
                        Ok(format!("{} == {}", left_val, right_val))
                    }
                    (ir::BinaryOp::NotEqual, 
                        ir::Type::Int | ir::Type::Char | ir::Type::Int8 | ir::Type::Int16 | ir::Type::Int32 | ir::Type::Int64 | ir::Type::Int128, 
                        ir::Type::Int | ir::Type::Char | ir::Type::Int8 | ir::Type::Int16 | ir::Type::Int32 | ir::Type::Int64 | ir::Type::Int128, 
                    ) => {
                        Ok(format!("{} != {}", left_val, right_val))
                    }
                    (ir::BinaryOp::NotEqual,
                        ir::Type::Bool, 
                        ir::Type::Bool
                    ) => {
                        Ok(format!("{} != {}", left_val, right_val))
                    }
                    (ir::BinaryOp::NotEqual, 
                        ir::Type::Float | ir::Type::Double,
                        ir::Type::Float | ir::Type::Double,
                    ) => {
                        Ok(format!("{} != {}", left_val, right_val))
                    }
                    (ir::BinaryOp::Multiply,
                        ir::Type::Int | ir::Type::Char | ir::Type::Int8 | ir::Type::Int16 | ir::Type::Int32 | ir::Type::Int64 | ir::Type::Int128, 
                        ir::Type::Int | ir::Type::Char | ir::Type::Int8 | ir::Type::Int16 | ir::Type::Int32 | ir::Type::Int64 | ir::Type::Int128, 
                    ) => {
                        Ok(format!("{} * {}", left_val, right_val))
                    }
                    (ir::BinaryOp::Multiply, 
                        ir::Type::Float | ir::Type::Double,
                        ir::Type::Float | ir::Type::Double,
                    ) => {
                        Ok(format!("{} * {}", left_val, right_val))
                    }
                    (ir::BinaryOp::Divide, 
                        ir::Type::Int | ir::Type::Char | ir::Type::Int8 | ir::Type::Int16 | ir::Type::Int32 | ir::Type::Int64 | ir::Type::Int128, 
                        ir::Type::Int | ir::Type::Char | ir::Type::Int8 | ir::Type::Int16 | ir::Type::Int32 | ir::Type::Int64 | ir::Type::Int128, 
                    ) => {
                        Ok(format!("{} / {}", left_val, right_val))
                    }
                    (ir::BinaryOp::Divide, 
                        ir::Type::Float | ir::Type::Double,
                        ir::Type::Float | ir::Type::Double,
                    ) => {
                        Ok(format!("{} / {}", left_val, right_val))
                    }
                    (ir::BinaryOp::Add, 
                        ir::Type::Int | ir::Type::Char | ir::Type::Int8 | ir::Type::Int16 | ir::Type::Int32 | ir::Type::Int64 | ir::Type::Int128 | ir::Type::String, 
                        ir::Type::Int | ir::Type::Char | ir::Type::Int8 | ir::Type::Int16 | ir::Type::Int32 | ir::Type::Int64 | ir::Type::Int128 | ir::Type::String, 
                    ) => {
                        Ok(format!("{} + {}", left_val, right_val))
                    }
                    (ir::BinaryOp::Add, 
                        ir::Type::Float | ir::Type::Double,
                        ir::Type::Float | ir::Type::Double,
                    ) => {
                        Ok(format!("{} + {}", left_val, right_val))
                    }
                    (ir::BinaryOp::Sub, 
                        ir::Type::Int | ir::Type::Char | ir::Type::Int8 | ir::Type::Int16 | ir::Type::Int32 | ir::Type::Int64 | ir::Type::Int128, 
                        ir::Type::Int | ir::Type::Char | ir::Type::Int8 | ir::Type::Int16 | ir::Type::Int32 | ir::Type::Int64 | ir::Type::Int128, 
                    ) => {
                        Ok(format!("{} - {}", left_val, right_val))
                    }
                    (ir::BinaryOp::Sub, 
                        ir::Type::Float | ir::Type::Double,
                        ir::Type::Float | ir::Type::Double,
                    ) => {
                        Ok(format!("{} - {}", left_val, right_val))
                    }
                    (ir::BinaryOp::LessThan, 
                        ir::Type::Int | ir::Type::Char | ir::Type::Int8 | ir::Type::Int16 | ir::Type::Int32 | ir::Type::Int64 | ir::Type::Int128, 
                        ir::Type::Int | ir::Type::Char | ir::Type::Int8 | ir::Type::Int16 | ir::Type::Int32 | ir::Type::Int64 | ir::Type::Int128, 
                    ) => {
                        Ok(format!("{} < {}", left_val, right_val))
                    }
                    (ir::BinaryOp::LessThan,
                        ir::Type::Float | ir::Type::Double,
                        ir::Type::Float | ir::Type::Double,
                    ) => {
                        Ok(format!("{} < {}", left_val, right_val))
                    }
                    (ir::BinaryOp::LessThanOrEqual, 
                        ir::Type::Int | ir::Type::Char | ir::Type::Int8 | ir::Type::Int16 | ir::Type::Int32 | ir::Type::Int64 | ir::Type::Int128, 
                        ir::Type::Int | ir::Type::Char | ir::Type::Int8 | ir::Type::Int16 | ir::Type::Int32 | ir::Type::Int64 | ir::Type::Int128, 
                    ) => {
                        Ok(format!("{} <= {}", left_val, right_val))
                    }
                    (ir::BinaryOp::LessThanOrEqual, 
                        ir::Type::Float | ir::Type::Double,
                        ir::Type::Float | ir::Type::Double,
                    ) => {
                        Ok(format!("{} <= {}", left_val, right_val))
                    }
                    (ir::BinaryOp::GreaterThan, 
                        ir::Type::Int | ir::Type::Char | ir::Type::Int8 | ir::Type::Int16 | ir::Type::Int32 | ir::Type::Int64 | ir::Type::Int128, 
                        ir::Type::Int | ir::Type::Char | ir::Type::Int8 | ir::Type::Int16 | ir::Type::Int32 | ir::Type::Int64 | ir::Type::Int128, 
                    ) => {
                        Ok(format!("{} > {}", left_val, right_val))
                    }
                    (ir::BinaryOp::GreaterThan, 
                        ir::Type::Float | ir::Type::Double,
                        ir::Type::Float | ir::Type::Double,
                    ) => {
                        Ok(format!("{} > {}", left_val, right_val))
                    }
                    (ir::BinaryOp::GreaterThanOrEqual, 
                        ir::Type::Int | ir::Type::Char | ir::Type::Int8 | ir::Type::Int16 | ir::Type::Int32 | ir::Type::Int64 | ir::Type::Int128, 
                        ir::Type::Int | ir::Type::Char | ir::Type::Int8 | ir::Type::Int16 | ir::Type::Int32 | ir::Type::Int64 | ir::Type::Int128, 
                    ) => {
                        Ok(format!("{} >= {}", left_val, right_val))
                    }
                    (ir::BinaryOp::GreaterThanOrEqual, 
                        ir::Type::Float | ir::Type::Double,
                        ir::Type::Float | ir::Type::Double,
                    ) => {
                        Ok(format!("{} >= {}", left_val, right_val))
                    }
                    (ir::BinaryOp::And, 
                        ir::Type::Bool, 
                        ir::Type::Bool
                    ) => {
                        Ok(format!("{} && {}", left_val, right_val))
                    }
                    (ir::BinaryOp::Or,
                        ir::Type::Bool, 
                        ir::Type::Bool
                    ) => {
                        Ok(format!("{} || {}", left_val, right_val))
                    }

                    (ir::BinaryOp::ArrayDeref, 
                        ir::Type::List(_),
                        ir::Type::Usize 
                    ) => {
                        // TODO: add bounds checking (don't bounds check on constants)
                        let elem = match left.ty() {
                            ir::Type::List(elem_ty) => elem_ty,
                            _ => {
                                return Err(CompilerError::BackendError(format!(
                                    "Error in Deref: {:?} is not of Array type",
                                    node.ty()
                                )))
                            }
                        };

                        Ok(format!("{}[{}]", left_val, right_val))
                    }

                    (ir::BinaryOp::ArrayDeref, 
                        ir::Type::Array(_, _),
                        ir::Type::Usize
                    ) => {
                        // TODO: add bounds checking (don't bounds check on constants)
                        let elem = match left.ty() {
                            ir::Type::Array(_num, elem_ty) => (_num, elem_ty),
                            _ => {
                                return Err(CompilerError::BackendError(format!(
                                    "Error in Deref: {:?} is not of Array type",
                                    node.ty()
                                )))
                            }
                        };

                        Ok(format!("{}[{}]", left_val, right_val))
                    }
                    _ => Err(CompilerError::BackendError(format!(
                        "Invalid binary operation {:?} over types {:?} and {:?}",
                        op,
                        left.ty(),
                        right.ty(),
                    ))),
                }
            }
            SemExpression::Array(elems) => {
                let (elem_count, elem_ty) = match node.ty() {
                    ir::Type::Array(elem_count, elem_ty) => (elem_count, elem_ty),
                    _ => {
                        return Err(CompilerError::BackendError(format!(
                            "Error in Array literal: {:?} is not of Array type",
                            node.ty()
                        )))
                    }
                };

                let arr: Vec<String> = elems.into_iter().map(|f| {
                    self.translate_expr(f, scope).unwrap()
                }).collect();

                let arr = arr.join(", "); 
                Ok(format!("{}{}{}", "{", arr, "}")) 
            }
            SemExpression::UnaryOp(op, n) => {
                let operand = self.translate_expr(n, scope)?;
                match (op, n.ty()) {
                    (ir::UnaryOp::Minus, ir::Type::Int) => Ok(format!("-{}", operand)), 
                    (ir::UnaryOp::Minus, ir::Type::Float) => Ok(format!("-{}", operand)), 
                    (ir::UnaryOp::Not, ir::Type::Bool) => Ok(format!("!{}", operand)), 
                    _ => Err(CompilerError::BackendError(format!(
                        "Invalid unary operation {:?} over type {:?}",
                        op,
                        n.ty()
                    ))),
                }
            }
            SemExpression::Let(id, init) => self.translate_val(id, init, scope, real_ty), 
            SemExpression::Lets(ids, expr) => {
                let mut s = String::new();
                let mut sc = scope.clone(); 

                s.push_str("{\n"); 
                for lt in ids {
                    let SemExpression::Let(name, exprl) = lt.clone() else {
                        panic!("expected let expression")
                    }; 
                    let a = self.translate_val(name.as_str(), &exprl, &mut sc, real_ty.clone())?; 
                    s.push_str("\t\t"); 
                    s.push_str(a.as_str()); 
                }
                let e = self.translate_expr(expr, &mut sc).unwrap(); 
                s.push_str("\t\t");
                s.push_str(e.as_str());
                s.push_str("\n\t}"); 
                Ok(s)
            }
            SemExpression::Val(id, init)=> self.translate_val(id, init, scope, real_ty),
            SemExpression::Id(id) => {

                let variable = scope.get(id).ok_or_else(|| {
                    CompilerError::BackendError(format!("No variable {} available", id))
                })?;
                
                Ok(id.clone())

            }
            SemExpression::Char(val) => {
                Ok(format!("'{}'", *val as char))
            },
            SemExpression::Bool(val) => {
                Ok(format!("{}", *val))
            }
            SemExpression::Integer(val, t) => {
                let t = *t.clone(); 
                let real_ty = self.jit.clone().real_type(&t).unwrap();

                let ty = match t.clone() {
                    Type::Usize => Ok(format!("{} as {}", *val, real_ty)), 
                    Type::Float => Ok(format!("{} as {}", *val, real_ty)), 
                    Type::Double => Ok(format!("{} as {}", *val, real_ty)),  
                    Type::Int => Ok(format!("{}", *val)), // 64 bit is the default integer
                    Type::Bool => Ok(format!("{} as {}", *val, real_ty)),
                    Type::Char => Ok(format!("{} as {}", *val, real_ty)),
                    Type::Int8 => Ok(format!("{} as {}", *val, real_ty)),
                    Type::Int16 => Ok(format!("{} as {}", *val, real_ty)),
                    Type::Int32 => Ok(format!("{} as {}", *val, real_ty)),
                    Type::Int64 => Ok(format!("{} as {}", *val, real_ty)),
                    Type::Int128 => Ok(format!("{} as {}", *val, real_ty)),
                    _ => unreachable!()
                }; 
                Ok(format!("{}", ty?))
            },

            SemExpression::Decimal(val, t) => {
                let ty = match *t.clone() {
                    Type::Float => Ok(format!("{}", *val)), 
                    Type::Double => Ok(format!("{}", *val)),  
                    _ => unreachable!()
                }; 

                ty
            },
        }
    }

    fn translate_conditional(
        &mut self,
        cond: &Box<SemNode>,
        then: &Box<SemNode>,
        alt: &Box<SemNode>,
        scope: &mut HashMap<String, String>
    ) -> Result<String, CompilerError> {
        let cond = self.translate_expr(cond, scope)?; 
        let then = self.translate_expr(then, scope)?; 
        let alt = self.translate_expr(alt, scope)?;

        let st = format!("(({}) : {} ? {})", cond, then, alt);  
        Ok(st)
    }

    fn translate_val(
        &mut self, 
        id: &str, 
        init: &Box<SemNode>,
        scope: &mut HashMap<String, String>,
        val_ty: String
    ) -> Result<String, CompilerError> {
        
        let node = init.clone(); 
        let val = self.translate_expr(&node, scope)?;
        scope.insert(id.to_string(), val.clone());

        match init.expr() {
            SemExpression::Block(_) => {
                let splits: Vec<_> = val.split("\n").collect(); 
                let len = splits.len(); 
                let rest = splits.get(0 .. len - 1).unwrap(); 
                let rest = rest.to_vec(); 
                let rest = rest.join("\n");

                let last = splits.last().unwrap(); 
                let last = last.trim(); 
                Ok(format!("{} {} {} = {};\n",rest, val_ty, id, last))
            }
            _ => Ok(format!("{} {} = {};\n", val_ty, id, val.clone()))
        }
        //self.vars.push_str(format!("{} {};", val_ty, id.clone()).as_str()); 
    }


    #[allow(unused)]
    fn translate_let(
        &mut self,
        id: &str,
        init: &Box<SemNode>,
        expr: &Box<SemNode>,
        scope: &mut HashMap<String, String>
    ) -> Result<String, CompilerError> {
        let val = self.translate_expr(init, scope)?;


        let shadowed_var = scope.remove(id.into());
        scope.insert(id.into(), val.clone());
        let res = self.translate_expr(expr, scope)?;

        scope.remove(id.into());
        if let Some(sv) = shadowed_var {
            scope.insert(id.into(), sv);
        }
        Ok(format!("let {} = {};\n\t{}\n", id, val.clone(), res))
    }

    fn translate_funcall(
        &mut self,
        func_name: &str,
        args: &[SemNode],
        scope: &mut HashMap<String, String>
    ) -> Result<String, CompilerError> {
        let mut args_values = args
            .iter()
            .map(|arg| self.translate_expr(arg, scope).unwrap())
            .collect::<Vec<_>>();

        Ok(format!("{}({})", func_name, args_values.join(", ")))
    }
}
