use crate::error::CompilerError;
use crate::ir::raw::EnumField;
use crate::ir::sem::*;
use crate::ir::{self, Type};
use crate::Jit;

use std::collections::hash_map::HashMap;

pub(crate) struct FunctionTranslator {
    jit: Jit,
    block_count: i32,
    pub blocks: String,
    pub vars: String,
}

impl FunctionTranslator {
    pub fn new(jit: Jit) -> Self {
        Self {
            jit,
            blocks: String::new(),
            vars: String::new(),
            block_count: 0,
        }
    }

    fn extract_block(
        &mut self,
        node: &SemNode,
        scope: &mut HashMap<String, Type>,
        real_ty: String,
    ) -> Result<(String, String), CompilerError>  {
        match node.expr() {
            SemExpression::Block(values) => {
                let len = values.len();
                if len == 0 {
                    println!("Invalid block with 0 expressions");
                    panic!();
                }

                let rest = values.get(0..len - 1).unwrap();

                let mut block = String::from("{\n");

                for val in rest.into_iter() {
                    let v = val.clone();
                    let e = self.translate_expr(&v, scope)?;
                    block.push_str("\t");
                    match val.expr().clone() {
                        SemExpression::Conditional(_, _, _) => {
                            let splits: Vec<_> = e.split("\n").collect();
                            let len = splits.len(); 
                            let first = splits.get(0 .. len -1).unwrap(); 
                            let first = first.join("\n");

                            let last = splits.last().unwrap(); 
                            block.push_str(format!("{}\n", first).as_str()); 
                            block.push_str(format!("var tmp_{} = {};\n", self.block_count, last).as_str());
                        }, 
                        _ => {
                            block.push_str(e.as_str())
                        }
                    }

                    match val.expr() {
                        SemExpression::Embed(_) => block.push_str("\n"),
                        SemExpression::Val(_, _) => (),
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

                    SemExpression::Lets(_, _) => {
                        let lines: Vec<_> = e.split("\n").collect();
                        let len = lines.len();
                        let first = lines.get(0..len - 1).unwrap();
                        let first = first.join("\n");
                        block.push_str("\t");
                        block.push_str(first.as_str());

                        let last = lines.last().unwrap();
                        block.push_str(format!("\treturn {};\n{}", last, "}").as_str());
                    }
                    _ => {
                        block.push_str("\treturn ");
                        block.push_str(e.as_str());
                        block.push_str(";\n}");
                    }
                }
                let h = format!("() -> {}", block);
                let var = format!("Block<{real_ty}> block_{} = {};\n", self.block_count, h);

                let s = format!("block_{}.run()", self.block_count);
                self.block_count += 1;
                Ok((var, s))
            }
            _ => panic!("Not a block"),
        }
    }

    pub fn indent_lines(&self, lines: String, depth: i32) -> String {
        let rest: Vec<_> = lines.split("\n").collect();
        let tag = "\t".repeat(depth as usize); 
        let rest: Vec<_> = rest.into_iter().map(|f| format!("{tag}{f}")).collect(); 
        rest.join("\n")
    }

    pub fn translate_expr(
        &mut self,
        node: &SemNode,
        scope: &mut HashMap<String, Type>,
    ) -> Result<String, CompilerError> {
        let real_ty = self.jit.clone().real_type(node.ty())?;

        match node.expr() {
            SemExpression::LambdaCall(e, args) => {
                let mut vals: Vec<_> = Vec::new();
                let _val = self.translate_expr(e, scope)?;
                for arg in args.clone().into_iter() {
                    let expr = self.translate_expr(&arg, scope)?;
                    vals.push(expr); 
                }

                println!("({})", vals.join(", "));
                todo!()
            }
            SemExpression::Lambda(args, _, bd) => {
                let mut sc = scope.clone();  
                for arg in args.clone().into_iter() {
                    let (name, ty) = arg.clone();
                    sc.insert(name, ty);
                }

                let mut args_list: Vec<String> = vec![]; 
                for arg in args.into_iter() {
                    let (name, ty) = arg.clone(); 
                    let real_ty = self.jit.clone().real_type(&ty)?;
                    args_list.push(format!("{real_ty} {name}")); 
                }; 

                let args_list = args_list.join(", ");
                let body = bd.expr();  
                let expr_ty = self.jit.clone().real_type(bd.ty())?;
                let expr = match body {
                    SemExpression::Block(_) => {
                        let (rest, last) = self.extract_block(bd, &mut sc, expr_ty)?;
                        let rest = self.indent_lines(rest, 1);
                        let expr = format!("{rest}return {last};");
                        let expr = self.indent_lines(expr, 1);
                        format!("{}\n{}\n{}", "{", expr, "}")
                    }
                    _ => self.translate_expr(bd, &mut sc)?
                };

                let ret = format!("({args_list}) -> {expr}");
                Ok(ret)
            }
            SemExpression::Destructure(names, e) => {
                let jit = self.jit.clone(); 
                let records = jit.records.clone(); 
                let _enums = jit.clone().enums.clone(); 
                let expr = self.translate_expr(e, scope)?;

                let mut str = String::new(); 
                let tmp_name = format!("destruct_tmp_{}", self.block_count + names.len() as i32); 
                str.push_str(format!("var {tmp_name} = {};\n", expr).as_str());

                match e.ty().clone() {
                    Type::UserType(x) => {
                        if records.contains_key(&x) {
                            let rec = records.get(&x).unwrap();
                            for name in names.into_iter() {
                                let (cond, i) = jit.clone().record_contains_field(name.clone(), rec.fields.clone()); 
                                if cond == false {
                                    return Err(CompilerError::BackendError(format!(
                                        "Cannot destructure field with name `{}` from type `{}`",name, e.ty(),
                                    )));
                                }

                                let (_name, ty) = rec.fields.get(i).unwrap(); 
                                scope.insert(name.to_string(), ty.clone());
                                let line = format!("\tvar {name} = {tmp_name}.{name}();\n"); 
                                str.push_str(line.as_str()); 
                            }
                            Ok(str)
                        } else {
                            return Err(CompilerError::BackendError(format!(
                                "Cannot destructure expression of type {}", e.ty(),
                            )));
                        }
                    }
                    Type::EnumType(parent, child) => {
                        jit.clone().enum_type_exists(parent.clone()).unwrap(); 
                        let args = self.jit.enums.get(&parent).unwrap(); 
                        let found = self.jit.clone().extract_record_type(child, args.clone().fields).unwrap();
                        
                        match found {
                            EnumField::Rec(r) => {
                                let rec = r.clone(); 
                                for name in names.into_iter() {
                                    let (cond, i) = jit.clone().record_contains_field(name.clone(), rec.fields.clone()); 
                                    if cond == false {
                                        return Err(CompilerError::BackendError(format!(
                                            "Cannot destructure field with name `{}` from type `{}`",name, e.ty(),
                                        )));
                                    }

                                    let (_name, ty) = rec.fields.get(i).unwrap(); 
                                    scope.insert(name.to_string(), ty.clone());
                                    let line = format!("\tvar {name} = {tmp_name}.{name}();\n"); 
                                    str.push_str(line.as_str()); 
                                }
                                return Ok(str)
                            }
                            EnumField::Id(_) => {
                                return Err(CompilerError::BackendError(format!(
                                    "Cannot destructure expression of type {} because it has no fields", e.ty(),
                                )));
                            }
                        }; 
                    }
                    _ => {
                        return Err(CompilerError::BackendError(format!(
                            "Cannot destructure expression of type {}", e.ty(),
                        )));
                    }
                }
            }
            SemExpression::EnumLiteral(parent, expr) => {
                let name = parent.clone();
                let jit = self.jit.clone(); 
                jit.enum_type_exists(name.clone()).unwrap(); 
                let args = self.jit.enums.get(&name).unwrap(); 
                let e = match expr.expr() {
                    SemExpression::FunCall(name, args_) => {
                        let _n = name.clone(); 
                        let found = self.jit.clone().extract_record_type(_n.to_string(), args.clone().fields).unwrap();
                        let found = match found {
                            EnumField::Rec(r) => r, 
                            _ => unreachable!()
                        }; 

                        let fields = found.fields; 
                        let len = fields.len(); 

                        if len != args_.len() {
                            return Err(CompilerError::BackendError(format!(
                                "Invalid number of initializer values when initializing enum value of `{}` in type `{}`, expected {} but found {}",
                                name,
                                parent.to_string(), 
                                args_.len(),
                                len
                            )));
                        }

                        let mut args: Vec<_> = vec![]; 
                        for (i, arg) in args_.into_iter().enumerate() {
                            let arg_ty = arg.ty().clone(); 
                            let expected_ty = fields.get(i).unwrap();
                            let (_, expected_ty) = expected_ty;
                            expected_ty.assert_eq(&arg_ty)?; 
                            // arg_ty.assert_eq(expected_ty).unwrap(); 
                            let t = self.translate_expr(arg, scope)?; 
                            args.push(t); 
                        }

                        let args = args.join(",");
                        let args = format!("{}({})", name, args); 
                        args
                    }, 
                    SemExpression::Id(_name) => {
                        format!("{}()", _name)
                    }, 
                    _ => unreachable!(), 
                }; 

                Ok(format!("new {}.{}", name, e))
            }
            SemExpression::RecordLiteral(name, args) => {
                let jit = self.jit.clone();
                jit.clone().record_type_exists(name.clone())?;

                let record = jit.records.get(name).unwrap();
                let record = record.clone();

                if record.fields.len() != args.len() {
                    return Err(CompilerError::BackendError(format!(
                        "Invalid number of initializers passed to struct {}, expected {} but found {}",
                        name,
                        record.fields.len(),
                        args.len()
                    )));
                }

                for (i, (n, e)) in args.clone().into_iter().enumerate() {
                    let rec = record.clone();
                    let c = rec.fields.get(i).unwrap();
                    let (_n, _e) = c;
                    let _n = _n.clone();

                    if n != _n {
                        return Err(CompilerError::BackendError(format!(
                            "Attempt to initialze an invalid field `{}` in struct literal of {}",
                            n, name,
                        )));
                    }

                    e.ty().assert_eq(_e)?;
                }

                let args: Vec<String> = args
                    .into_iter()
                    .map(|f| {
                        let (_, e) = f;
                        let e = self.translate_expr(e, scope).unwrap();
                        e
                    })
                    .collect();
                let args = args.join(",");
                Ok(format!("new {}({})", name, args))
            }
            SemExpression::Embed(x) => {
                let e = self.translate_expr(x, scope)?;
                let e: Vec<_> = e.split("\n").collect();
                let e: Vec<_> = e.into_iter().map(|f| f.to_string()).collect();
                let e = e.join("\n");
                let len = e.len();
                let e = e.get(1..len - 1).unwrap_or("");
                Ok(e.to_string())
            }
            SemExpression::String(x) => Ok(format!("\"{x}\"")),
            SemExpression::Unit => Ok(format!("Void.Unit")),
            SemExpression::Null(_) => Ok(format!("null")),
            SemExpression::Cast(e, t) => {
                let is_sys_type = self.jit.clone().is_sys_type(&*t.clone());
                let ty = t.clone();
                let ty = *ty.clone(); 
                let ty = self.jit.clone().real_type(&ty)?;
                let e = self.translate_expr(e, scope)?;

                let cast = if is_sys_type {
                    let ay = t.clone();
                    let ay = *ay.clone();
                    match ay {
                        ir::Type::String => format!("(String) {}", e),
                        _ => format!("SysConv.to_{}({})", ty, e)
                    }
                } else {
                    format!("(({}) {})", ty, e)
                };

                Ok(cast)
            }

            SemExpression::Abs(_x) => {
                let e = self.translate_expr(_x, scope)?;
                Ok(format!("Math.abs({})", e))
            }

            SemExpression::SimpleNullCheck(a) => {
                let a = a.clone();
                let e = self.translate_expr(&a, scope)?;
                Ok(format!("({} == null) ", e))
            }

            SemExpression::NullCheck(a, t) => {
                let a = a.clone();
                let ty = self.jit.clone().real_type(t)?;
                let e = self.translate_expr(&a, scope)?;
                Ok(format!("({} instanceof {ty})", e.clone()))
            }

            SemExpression::Block(_values) => {
                let (block, ret) = self.extract_block(node, scope, real_ty)?;
                let block = self.indent_lines(block, 1);
                Ok(format!("{}\n{}", block, ret))
            }
            SemExpression::Match(cond, cases) => {
                let c = self.translate_expr(cond, scope)?;

                let len = cases.len();
                let rest = cases.get(0..len - 1).unwrap();
                let rest = rest.to_vec();

                let mut rest: Vec<_> = rest
                    .into_iter()
                    .map(|f| {
                        let mut sc = scope.clone(); 
                        let (co, bo) = f;
                        let co = match co.expr() {
                            SemExpression::EnumLiteral(parent, child) => {
                                match child.expr() {
                                    SemExpression::FunCall(name, args) => {
                                        let mut str_args: Vec<String> = Vec::new();
                                        for arg in args {
                                            let ty = self.jit.clone().real_type(arg.ty()).unwrap();
                                            match arg.expr() {
                                                SemExpression::Id(n) => {
                                                    sc.insert(n.to_string(), arg.ty().clone());
                                                    str_args.push(format!("{} {}", ty, n));
                                                }
                                                _ => unreachable!()
                                            }
                                        }

                                        let str = str_args.join(", "); 
                                        let str = format!("{parent}.{name}({str})"); 
                                        str
                                    }
                                    SemExpression::Id(n) => {
                                        format!("{parent}.{n}()") 
                                    }
                                    _ => unreachable!()
                                }

                            }
                            _ => self.translate_expr(&co, &mut sc).unwrap()
                        }; 
                        let bo = self.translate_expr(&bo, &mut sc).unwrap();
                        format!("\tcase {} -> {}", co, bo)
                    })
                    .collect();

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
                        ir::Type::String | ir::Type::UserType(_) | ir::Type::EnumType(_, _), 
                        ir::Type::String | ir::Type::UserType(_) | ir::Type::EnumType(_, _), 
                    ) => {
                        println!("l: {left_val}, r: {right_val}"); 
                        Ok(format!("{}.equals({}) == true", left_val, right_val))
                    }
                    (
                        ir::BinaryOp::Equal,
                        ir::Type::Int
                        | ir::Type::Char
                        | ir::Type::Int8
                        | ir::Type::Int16
                        | ir::Type::Int32
                        | ir::Type::Int64
                        | ir::Type::Int128,
                        ir::Type::Int
                        | ir::Type::Char
                        | ir::Type::Int8
                        | ir::Type::Int16
                        | ir::Type::Int32
                        | ir::Type::Int64
                        | ir::Type::Int128
                    ) => {
                        println!("l: {left_val}, r: {right_val}"); 
                        Ok(format!("{} == {}", left_val, right_val))
                    }
                    (ir::BinaryOp::Equal, ir::Type::Bool, ir::Type::Bool) => {
                        Ok(format!("{} == {}", left_val, right_val))
                    }
                    (
                        ir::BinaryOp::Equal,
                        ir::Type::Float | ir::Type::Double,
                        ir::Type::Float | ir::Type::Double,
                    ) => Ok(format!("{} == {}", left_val, right_val)),
                    (
                        ir::BinaryOp::NotEqual,
                        ir::Type::Int
                        | ir::Type::Char
                        | ir::Type::Int8
                        | ir::Type::Int16
                        | ir::Type::Int32
                        | ir::Type::Int64
                        | ir::Type::Int128,
                        ir::Type::Int
                        | ir::Type::Char
                        | ir::Type::Int8
                        | ir::Type::Int16
                        | ir::Type::Int32
                        | ir::Type::Int64
                        | ir::Type::Int128,
                    ) => Ok(format!("{} != {}", left_val, right_val)),
                    (ir::BinaryOp::NotEqual, 
                        ir::Type::String | ir::Type::UserType(_) | ir::Type::EnumType(_, _), 
                        ir::Type::String | ir::Type::UserType(_) | ir::Type::EnumType(_, _), 
                    ) => {
                        Ok(format!("{}.equals({}) == false", left_val, right_val))
                    }
                    (ir::BinaryOp::NotEqual, ir::Type::Bool, ir::Type::Bool) => {
                        Ok(format!("{} != {}", left_val, right_val))
                    }
                    (
                        ir::BinaryOp::NotEqual,
                        ir::Type::Float | ir::Type::Double,
                        ir::Type::Float | ir::Type::Double,
                    ) => Ok(format!("{} != {}", left_val, right_val)),

                    (
                        ir::BinaryOp::Mod,
                        ir::Type::Int
                        | ir::Type::Char
                        | ir::Type::Int8
                        | ir::Type::Int16
                        | ir::Type::Int32
                        | ir::Type::Int64
                        | ir::Type::Int128,
                        ir::Type::Int
                        | ir::Type::Char
                        | ir::Type::Int8
                        | ir::Type::Int16
                        | ir::Type::Int32
                        | ir::Type::Int64
                        | ir::Type::Int128,
                    ) => Ok(format!("{} % {}", left_val, right_val)),
                    (
                        ir::BinaryOp::Mod,
                        ir::Type::Float | ir::Type::Double,
                        ir::Type::Float | ir::Type::Double,
                    ) => Ok(format!("{} % {}", left_val, right_val)),

                    (
                        ir::BinaryOp::Multiply,
                        ir::Type::Int
                        | ir::Type::Char
                        | ir::Type::Int8
                        | ir::Type::Int16
                        | ir::Type::Int32
                        | ir::Type::Int64
                        | ir::Type::Int128,
                        ir::Type::Int
                        | ir::Type::Char
                        | ir::Type::Int8
                        | ir::Type::Int16
                        | ir::Type::Int32
                        | ir::Type::Int64
                        | ir::Type::Int128,
                    ) => Ok(format!("{} * {}", left_val, right_val)),
                    (
                        ir::BinaryOp::Multiply,
                        ir::Type::Float | ir::Type::Double,
                        ir::Type::Float | ir::Type::Double,
                    ) => Ok(format!("{} * {}", left_val, right_val)),
                    (
                        ir::BinaryOp::Divide,
                        ir::Type::Int
                        | ir::Type::Char
                        | ir::Type::Int8
                        | ir::Type::Int16
                        | ir::Type::Int32
                        | ir::Type::Int64
                        | ir::Type::Int128,
                        ir::Type::Int
                        | ir::Type::Char
                        | ir::Type::Int8
                        | ir::Type::Int16
                        | ir::Type::Int32
                        | ir::Type::Int64
                        | ir::Type::Int128,
                    ) => Ok(format!("{} / {}", left_val, right_val)),
                    (
                        ir::BinaryOp::Divide,
                        ir::Type::Float | ir::Type::Double,
                        ir::Type::Float | ir::Type::Double,
                    ) => Ok(format!("{} / {}", left_val, right_val)),
                    (
                        ir::BinaryOp::Add,
                        ir::Type::Int
                        | ir::Type::Char
                        | ir::Type::Int8
                        | ir::Type::Int16
                        | ir::Type::Int32
                        | ir::Type::Int64
                        | ir::Type::Int128
                        | ir::Type::String
                        | ir::Type::Any,
                        ir::Type::Int
                        | ir::Type::Char
                        | ir::Type::Int8
                        | ir::Type::Int16
                        | ir::Type::Int32
                        | ir::Type::Int64
                        | ir::Type::Int128
                        | ir::Type::String
                        | ir::Type::Any,
                    ) => Ok(format!("{} + {}", left_val, right_val)),
                    (
                        ir::BinaryOp::Add,
                        ir::Type::String | ir::Type::UserType(_) | ir::Type::EnumType(_, _),
                        ir::Type::String | ir::Type::UserType(_) | ir::Type::EnumType(_, _),
                    ) => Ok(format!("{} + {}", left_val, right_val)),
                    (
                        ir::BinaryOp::Add,
                        ir::Type::Float | ir::Type::Double | ir::Type::String,
                        ir::Type::Float | ir::Type::Double | ir::Type::String,
                    ) => Ok(format!("{} + {}", left_val, right_val)),
                    (
                        ir::BinaryOp::Sub,
                        ir::Type::Int
                        | ir::Type::Char
                        | ir::Type::Int8
                        | ir::Type::Int16
                        | ir::Type::Int32
                        | ir::Type::Int64
                        | ir::Type::Int128,
                        ir::Type::Int
                        | ir::Type::Char
                        | ir::Type::Int8
                        | ir::Type::Int16
                        | ir::Type::Int32
                        | ir::Type::Int64
                        | ir::Type::Int128,
                    ) => Ok(format!("{} - {}", left_val, right_val)),
                    (
                        ir::BinaryOp::Sub,
                        ir::Type::Float | ir::Type::Double,
                        ir::Type::Float | ir::Type::Double,
                    ) => Ok(format!("{} - {}", left_val, right_val)),
                    (
                        ir::BinaryOp::LessThan,
                        ir::Type::Int
                        | ir::Type::Char
                        | ir::Type::Int8
                        | ir::Type::Int16
                        | ir::Type::Int32
                        | ir::Type::Int64
                        | ir::Type::Int128,
                        ir::Type::Int
                        | ir::Type::Char
                        | ir::Type::Int8
                        | ir::Type::Int16
                        | ir::Type::Int32
                        | ir::Type::Int64
                        | ir::Type::Int128,
                    ) => Ok(format!("{} < {}", left_val, right_val)),
                    (
                        ir::BinaryOp::LessThan,
                        ir::Type::Float | ir::Type::Double,
                        ir::Type::Float | ir::Type::Double,
                    ) => Ok(format!("{} < {}", left_val, right_val)),
                    (
                        ir::BinaryOp::LessThanOrEqual,
                        ir::Type::Int
                        | ir::Type::Char
                        | ir::Type::Int8
                        | ir::Type::Int16
                        | ir::Type::Int32
                        | ir::Type::Int64
                        | ir::Type::Int128,
                        ir::Type::Int
                        | ir::Type::Char
                        | ir::Type::Int8
                        | ir::Type::Int16
                        | ir::Type::Int32
                        | ir::Type::Int64
                        | ir::Type::Int128,
                    ) => Ok(format!("{} <= {}", left_val, right_val)),
                    (
                        ir::BinaryOp::LessThanOrEqual,
                        ir::Type::Float | ir::Type::Double,
                        ir::Type::Float | ir::Type::Double,
                    ) => Ok(format!("{} <= {}", left_val, right_val)),
                    (
                        ir::BinaryOp::GreaterThan,
                        ir::Type::Int
                        | ir::Type::Char
                        | ir::Type::Int8
                        | ir::Type::Int16
                        | ir::Type::Int32
                        | ir::Type::Int64
                        | ir::Type::Int128,
                        ir::Type::Int
                        | ir::Type::Char
                        | ir::Type::Int8
                        | ir::Type::Int16
                        | ir::Type::Int32
                        | ir::Type::Int64
                        | ir::Type::Int128,
                    ) => Ok(format!("{} > {}", left_val, right_val)),
                    (
                        ir::BinaryOp::GreaterThan,
                        ir::Type::Float | ir::Type::Double,
                        ir::Type::Float | ir::Type::Double,
                    ) => Ok(format!("{} > {}", left_val, right_val)),
                    (
                        ir::BinaryOp::GreaterThanOrEqual,
                        ir::Type::Int
                        | ir::Type::Char
                        | ir::Type::Int8
                        | ir::Type::Int16
                        | ir::Type::Int32
                        | ir::Type::Int64
                        | ir::Type::Int128,
                        ir::Type::Int
                        | ir::Type::Char
                        | ir::Type::Int8
                        | ir::Type::Int16
                        | ir::Type::Int32
                        | ir::Type::Int64
                        | ir::Type::Int128,
                    ) => Ok(format!("{} >= {}", left_val, right_val)),
                    (
                        ir::BinaryOp::GreaterThanOrEqual,
                        ir::Type::Float | ir::Type::Double,
                        ir::Type::Float | ir::Type::Double,
                    ) => Ok(format!("{} >= {}", left_val, right_val)),
                    (ir::BinaryOp::And, ir::Type::Bool, ir::Type::Bool) => {
                        Ok(format!("{} && {}", left_val, right_val))
                    }
                    (ir::BinaryOp::Or, ir::Type::Bool, ir::Type::Bool) => {
                        Ok(format!("{} || {}", left_val, right_val))
                    }

                    (ir::BinaryOp::ArrayDeref, ir::Type::List(_), ir::Type::Int) => {
                        // TODO: add bounds checking (don't bounds check on constants)
                        match left.ty() {
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

                    (ir::BinaryOp::ArrayDeref, ir::Type::Array(_, _), ir::Type::Int) => {
                        // TODO: add bounds checking (don't bounds check on constants)
                        match left.ty() {
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
                let (_elem_count, elem_ty) = match node.ty() {
                    ir::Type::Array(elem_count, elem_ty) => (elem_count, elem_ty),
                    _ => {
                        return Err(CompilerError::BackendError(format!(
                            "Error in Array literal: {:?} is not of Array type",
                            node.ty()
                        )))
                    }
                };

                let ty = self.jit.clone().real_type(elem_ty)?;
                let arr: Vec<String> = elems
                    .into_iter()
                    .map(|f| self.translate_expr(f, scope).unwrap())
                    .collect();

                let arr = arr.join(", ");
                Ok(format!("new {}[] {}{}{}", ty, "{", arr, "}"))
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

                match expr.expr() {
                    SemExpression::Block(_) => {
                        let real_ty = self.jit.clone().real_type(expr.ty())?; 
                        let (block, ret) = self.extract_block(expr, &mut sc, real_ty)?;

                        s.push_str(format!("\t\t{}", block).as_str());
                        s.push_str("\t\treturn ");
                        s.push_str(ret.as_str());
                        s.push_str(";\n\t}");
                    }
                    _ => {
                        let e = self.translate_expr(expr, &mut sc)?; 
                        s.push_str("\t\treturn ");
                        s.push_str(e.as_str());
                        s.push_str(";\n\t}");
                    }
                }; 

                // let e = self.translate_expr(expr, &mut sc).unwrap();

                let h = format!("() -> {}", s);
                let var = format!("Block<{real_ty}> block_{} = {};\n", self.block_count, h);

                let s = format!("{}\n\tblock_{}.run()", var.as_str(), self.block_count);
                self.block_count += 1;
                Ok(s)
            }
            SemExpression::Val(id, init) => self.translate_val(id, init, scope, real_ty),
            SemExpression::Id(id) => {
                scope.get(id).ok_or_else(|| {
                    CompilerError::BackendError(format!("No variable {} available", id))
                })?;
                Ok(id.clone())
            }
            SemExpression::Char(val) => Ok(format!("'{}'", *val as char)),
            SemExpression::Bool(val) => Ok(format!("{}", *val)),
            SemExpression::Integer(val, t) => {
                let t = *t.clone();
                let real_ty = self.jit.clone().real_type(&t).unwrap();

                let ty = match t.clone() {
                    Type::Usize => Ok(format!("({}){}", real_ty, *val)),
                    Type::Float => Ok(format!("({}){}", real_ty, *val)),
                    Type::Double => Ok(format!("({}){}", real_ty, *val)),
                    Type::Int => Ok(format!("{}", *val)), // 64 bit is the default integer
                    Type::Bool => Ok(format!("({}){}", real_ty, *val)),
                    Type::Char => Ok(format!("({}){}", real_ty, *val)),
                    Type::Int8 => Ok(format!("({}){}", real_ty, *val)),
                    Type::Int16 => Ok(format!("({}){}", real_ty, *val)),
                    Type::Int32 => Ok(format!("({}){}", real_ty, *val)),
                    Type::Int64 => Ok(format!("({}){}", real_ty, *val)),
                    Type::Int128 => Ok(format!("({}){}", real_ty, *val)),
                    _ => unreachable!(),
                };
                Ok(format!("{}", ty?))
            }

            SemExpression::Decimal(val, t) => {
                let ty = match *t.clone() {
                    Type::Float => Ok(format!("{}", *val)),
                    Type::Double => Ok(format!("{}", *val)),
                    _ => unreachable!(),
                };

                ty
            }
        }
    }

    fn translate_conditional(
        &mut self,
        cond: &Box<SemNode>,
        then: &Box<SemNode>,
        alt: &Box<SemNode>,
        scope: &mut HashMap<String, Type>,
    ) -> Result<String, CompilerError> {

        let cond_ty = "Boolean"; 
        let then_ty = then.ty(); 
        let alt_ty = alt.ty(); 

        let jit = self.jit.clone(); 

        let then_ty = jit.clone().real_type(then_ty)?; 
        let alt_ty = jit.real_type(alt_ty)?; 

        let mut ret_str = String::new(); 
        let cond = match cond.expr() {
            SemExpression::Block(_) => {
                let (block, ret) = self.extract_block(cond, scope, cond_ty.to_string())?;
                ret_str.push_str(block.as_str());
                ret_str.push_str("\n");
                ret
            },
            _ => self.translate_expr(cond, scope)?
        }; 

        let then = match then.expr() {
            SemExpression::Block(_) => {
                let (block, ret) = self.extract_block(then, scope, then_ty)?;
                ret_str.push_str(block.as_str()); 
                ret_str.push_str("\n");
                ret
            },
            _ => self.translate_expr(then, scope)?
        };

        let alt = match alt.expr() {
            SemExpression::Block(_) => {
                let (block, ret) = self.extract_block(alt, scope, alt_ty)?;
                ret_str.push_str(block.as_str()); 
                ret_str.push_str("\n");
                ret
            },
            _ => self.translate_expr(alt, scope)?
        }; 

        
        let st = format!("(({}) ? {} : {})", cond, then, alt);
        let st = format!("{ret_str}\n{st}");
        Ok(st)
    }

    fn translate_val(
        &mut self,
        id: &str,
        init: &Box<SemNode>,
        scope: &mut HashMap<String, Type>,
        val_ty: String,
    ) -> Result<String, CompilerError> {
        let ty = init.ty().clone();
        let node = init.clone();
        let val = self.translate_expr(&node, scope)?;
        scope.insert(id.to_string(), ty);

        match init.expr() {
            SemExpression::Block(_) | SemExpression::Lets(_, _) => {
                let splits: Vec<_> = val.split("\n").collect();
                let len = splits.len();
                let rest = splits.get(0..len - 1).unwrap();
                let rest = rest.to_vec();
                let rest = rest.join("\n");

                let last = splits.last().unwrap();
                let last = last.trim();
                Ok(format!("{}\t{} {} = {};\n", rest, "var", id, last))
            }
            SemExpression::Lambda(_, _, _) =>  Ok(format!("{} {} = {};\n", val_ty, id, val.clone())),
            SemExpression::Null(_) => Ok(format!("{} {} = {};\n", val_ty, id, val.clone())),
            _ => Ok(format!("{} {} = {};\n", "var", id, val.clone())),
        }
        //self.vars.push_str(format!("{} {};", val_ty, id.clone()).as_str());
    }

    #[allow(unused)]
    fn translate_let(
        &mut self,
        id: &str,
        init: &Box<SemNode>,
        expr: &Box<SemNode>,
        scope: &mut HashMap<String, Type>,
    ) -> Result<String, CompilerError> {
        let ty = init.ty().clone();
        let val = self.translate_expr(init, scope)?;
        let shadowed_var = scope.remove(id.into());
        scope.insert(id.into(), ty);
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
        scope: &mut HashMap<String, Type>,
    ) -> Result<String, CompilerError> {
        let args_values = args
            .iter()
            .map(|arg| self.translate_expr(arg, scope).unwrap())
            .collect::<Vec<_>>();

        Ok(format!("{}({})", func_name, args_values.join(", ")))
    }
}
