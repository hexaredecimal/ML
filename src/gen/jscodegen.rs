use crate::error::CompilerError;
use crate::ir::raw::EnumField;
use crate::ir::sem::*;
use crate::ir::{self, Type};
use crate::JSTables;
use super::utils::Utils;

use std::collections::hash_map::HashMap;

pub(crate) struct JSBackend {
    jit: JSTables,
    block_count: i32,
    pub blocks: String,
    pub vars: String,
}

impl JSBackend {
    pub fn new(jit: JSTables) -> Self {
        Self {
            jit,
            blocks: String::new(),
            vars: String::new(),
            block_count: 0,
        }
    }


    fn extract_match(&mut self, node: &SemNode, scope: &mut HashMap<String, Type>, ctx: &mut SemContext) -> Result<(String, String), CompilerError> {
        match node.expr() {
            SemExpression::Match(cond, cases) => {
                let c = self.translate_expr(cond, scope, ctx)?;

                let len = cases.len();
                let last = cases.last().unwrap();
                let rest = cases.get(0..len - 1).unwrap();
                let rest = rest.to_vec();
                
                let mut cases: Vec<String> = vec![]; 
                let mut lamdas: Vec<String> = vec![]; 
                let mut call_chain: Vec<String> = vec![];
                for case in rest {
                    let (_case, body) = case;
                    match _case.expr() {
                        SemExpression::EnumLiteral(parent, child) => {
                            match child.expr() {
                                SemExpression::FunCall(name, args) => {
                                    let mut sc = scope.clone();
                                    let mut _args = vec![]; 
                                    for arg in args {
                                        match arg.expr() {
                                            SemExpression::Id(var_name) => {
                                                    sc.insert(var_name.clone(), arg.ty().clone());
                                                    _args.push(var_name.clone());
                                                }
                                                _ => unreachable!()
                                            }
                                        }
                                        let _args = _args.join(", ");
                                        let cmp = format!("({c}).constructor === {parent}.{name}");
                                        cases.push(cmp);
                                        let fx = format!("case_{parent}_{name}");
                                        let call = format!("{fx}({_args})");
                                        call_chain.push(call);

                                        let bd = match body.expr() {
                                            SemExpression::Block(_) | SemExpression::Lets(_,_) => {
                                                let (block, ret) = self.extract_block(&body, &mut sc, ctx)?;
                                                format!("{block}\nretrun {ret}")
                                            }
                                            _ => format!("return {}", self.translate_expr(&body, &mut sc, ctx)?)
                                        }; 

                                        let fx = format!("const {fx} = ({_args}) {{\n{bd}\n}}");
                                        lamdas.push(fx);
                                }

                                _ => todo!()
                            }

                        }
                        _ => unreachable!()
                    }
                }

                let mut cond = String::new(); 
                for pack in cases.iter().zip(call_chain) {
                    let (lhs, rhs) = pack; 
                    cond.push_str(&format!("{lhs} ? {rhs} :"));
                }

                let fx = "case_else_".to_string();
                let call = format!("{fx}()");

                cond.push_str(&call);
                let (_, body) = last;
                let bd = match body.expr() {
                    SemExpression::Block(_) | SemExpression::Lets(_,_) => {
                        let (block, ret) = self.extract_block(body, scope, ctx)?;
                        format!("{block}\nretrun {ret}")
                    }
                    _ => format!("return {}", self.translate_expr(body, scope, ctx)?)
                }; 

                let fx = format!("const {fx} = () {{\n{bd}\n}}");
                lamdas.push(fx);
                let lamdas = lamdas.join("\n"); 
                Ok((lamdas, cond))
            }
            _ => unreachable!()
        }
    }

    fn extract_block(
        &mut self,
        node: &SemNode,
        scope: &mut HashMap<String, Type>,
        ctx: &mut SemContext
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

                for val in rest.iter() {
                    let v = val.clone();
                    let e = self.translate_expr(&v, scope, ctx)?;
                    block.push('\t');
                    match &val.expr() {
                        SemExpression::Conditional(_, _, _) => {
                            let splits: Vec<_> = e.split('\n').collect();
                            let len = splits.len(); 
                            let first = splits.get(0 .. len -1).unwrap(); 
                            let first = first.join("\n");

                            let last = splits.last().unwrap(); 
                            block.push_str(&format!("{}\n", first)); 
                            block.push_str(&format!("let tmp_{} = {}\n", self.block_count, last));
                        }, 
                        _ => {
                            block.push_str(e.as_str())
                        }
                    }

                    match &val.expr() {
                        SemExpression::Embed(_) => block.push('\n'),
                        SemExpression::Val(_, _) => (),
                        SemExpression::Lets(_, _) => block.push('\n'),
                        _ => block.push('\n'),
                    };
                }

                let last = values.last().unwrap();
                let e = self.translate_expr(last, scope, ctx)?;

                match &last.expr() {
                    SemExpression::Embed(_) => {
                        block.push_str(&e);
                        block.push_str(";\n}");
                    }

                    SemExpression::Lets(_, _) => {
                        let lines: Vec<_> = e.split('\n').collect();
                        let len = lines.len();
                        let first = lines.get(0..len - 1).unwrap();
                        let first = first.join("\n");
                        block.push('\t');
                        block.push_str(&first);

                        let last = lines.last().unwrap();
                        block.push_str(&format!("\treturn {last}\n{}", "}"));
                    }
                    _ => {
                        block.push_str("\treturn ");
                        block.push_str(&e);
                        block.push_str("\n}");
                    }
                }
                let header_format = format!("() => {}", block);
                let finished_block = format!("let block_{} = {header_format}\n", self.block_count);

                let block_invoker = format!("block_{}()", self.block_count);
                self.block_count += 1;
                Ok((finished_block, block_invoker))
            }
            _ => panic!("Not a block"),
        }
    }

    pub fn indent_lines(&self, lines: String, depth: i32) -> String {
        let rest: Vec<_> = lines.split('\n').collect();
        let tag = "\t".repeat(depth as usize); 
        let rest: Vec<_> = rest.into_iter().map(|f| format!("{tag}{f}")).collect(); 
        rest.join("\n")
    }

    pub fn translate_expr(
        &mut self,
        node: &SemNode,
        scope: &mut HashMap<String, Type>,
        ctx: &mut SemContext
    ) -> Result<String, CompilerError> {

        match node.expr() {
            SemExpression::FieldAccess(left, right) => {
                let left = self.translate_expr(left, scope, ctx)?;
                let right = match right.expr() {
                    SemExpression::Id(name) => {
                        name.to_string()
                    }
                    _ => {
                        self.translate_expr(right, scope, ctx)?
                    }
                };
                Ok(format!("{left}.{right}"))
            }
            SemExpression::LambdaCall(id, args) => {
                let mut vals: Vec<_> = Vec::new();
                for arg in args.clone().into_iter() {
                    let expr = self.translate_expr(&arg, scope, ctx)?;
                    vals.push(expr); 
                }

                Ok(format!("{id}.apply({})", vals.join(", ")))
            }
            SemExpression::Lambda(args, _, bd) => {
                let mut sc = scope.clone();  
                for arg in args.clone().into_iter() {
                    let (name, ty) = arg.clone();
                    sc.insert(name, ty);
                }

                let mut args_list: Vec<String> = vec![]; 
                for arg in args.iter() {
                    let (name, _) = arg.clone(); 
                    args_list.push(name.to_string()); 
                }; 

                let args_list = args_list.join(", ");
                let body = bd.expr();  
                let expr = match body {
                    SemExpression::Block(_) => {
                        let (rest, last) = self.extract_block(bd, &mut sc, ctx)?;
                        let rest = self.indent_lines(rest, 1);
                        let expr = format!("{rest}return {last};");
                        let expr = self.indent_lines(expr, 1);
                        format!("{}\n{expr}\n{}", "{", "}")
                    }
                    _ => self.translate_expr(bd, &mut sc, ctx)?
                };

                let ret = format!("({args_list}) => {expr}");
                Ok(ret)
            }
            SemExpression::Destructure(names, e) => {
                let jit = self.jit.clone(); 
                let records = &jit.records; 
                let _enums = &jit.enums; 
                let expr = self.translate_expr(e, scope, ctx)?;

                let mut str = String::new(); 
                let tmp_name = format!("destruct_tmp_{}", self.block_count + names.len() as i32); 
                str.push_str(&format!("let {tmp_name} = {expr};\n"));

                match &e.ty() {
                    Type::YourType(x) => {
                        if records.contains_key(x) {
                            let rec = records.get(x).unwrap();
                            for name in names.iter() {
                                let (cond, i) = Utils::record_contains_field(name, &rec.fields); 
                                if !cond {
                                    return Err(CompilerError::BackendError(format!(
                                        "Cannot destructure field with name `{name}` from type `{}`", e.ty(),
                                    )));
                                }

                                let (_name, ty) = rec.fields.get(i).unwrap(); 
                                scope.insert(name.to_string(), ty.clone());
                                let line = format!("\tlet {name} = {tmp_name}.{name}();\n"); 
                                str.push_str(&line); 
                            }
                            Ok(str)
                        } else {
                            Err(CompilerError::BackendError(format!(
                                "Cannot destructure expression of type {}", e.ty(),
                            )))
                        }
                    }
                    Type::EnumType(parent, child) => {
                        Utils::enum_type_exists(&self.jit.enums, parent).unwrap(); 
                        let args = self.jit.enums.get(parent).unwrap(); 
                        let found = Utils::extract_record_type(child, &args.fields).unwrap();
                        
                        match found {
                            EnumField::Rec(r) => {
                                let rec = r.clone(); 
                                for name in names {
                                    let (cond, i) = Utils::record_contains_field(name, &rec.fields); 
                                    if !cond {
                                        return Err(CompilerError::BackendError(format!(
                                            "Cannot destructure field with name `{}` from type `{}`",name, e.ty(),
                                        )));
                                    }

                                    let (_name, ty) = rec.fields.get(i).unwrap(); 
                                    scope.insert(name.to_string(), ty.clone());
                                    let line = format!("\tlet {name} = {tmp_name}.{name}();\n"); 
                                    str.push_str(&line);
                                }
                                Ok(str)
                            }
                            EnumField::Id(_) => {
                                Err(CompilerError::BackendError(format!(
                                    "Cannot destructure expression of type {} because it has no fields", e.ty(),
                                )))
                            }
                        }
                    }
                    _ => {
                        Err(CompilerError::BackendError(format!(
                            "Cannot destructure expression of type {}", e.ty(),
                        )))
                    }
                }
            }
            SemExpression::EnumLiteral(parent, expr) => {
                let jit = self.jit.clone(); 
                Utils::enum_type_exists(&jit.enums, parent).unwrap(); 
                let args = self.jit.enums.get(parent).unwrap(); 
                let e = match expr.expr() {
                    SemExpression::FunCall(name, args_) => {
                        let _n = name.clone(); 
                        let found = Utils::extract_record_type(name, &args.fields).unwrap();
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
                                parent, 
                                args_.len(),
                                len
                            )));
                        }

                        let mut args: Vec<_> = vec![]; 
                        for (i, arg) in args_.iter().enumerate() {
                            let expected_ty = fields.get(i).unwrap();
                            let (_, expected_ty) = expected_ty;
                            expected_ty.assert_eq(arg.ty(), ctx)?; 
                            let t = self.translate_expr(arg, scope, ctx)?; 
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

                Ok(format!("new {parent}.{e}"))
            }
            SemExpression::RecordLiteral(name, args) => {
                let jit = self.jit.clone();
                Utils::record_type_exists(&jit.records, name)?;

                let record = jit.records.get(name).unwrap();

                if record.fields.len() != args.len() {
                    return Err(CompilerError::BackendError(format!(
                        "Invalid number of initializers passed to struct {}, expected {} but found {}",
                        name,
                        record.fields.len(),
                        args.len()
                    )));
                }

                for (i, (arg_name, arg_expr)) in args.iter().enumerate() {
                    let (indexed_name, indexed_expr) = &record.fields.get(i).unwrap();
                    if *arg_name != *indexed_name {
                        return Err(CompilerError::BackendError(format!(
                            "Attempt to initialze an invalid field `{}` in struct literal of {}",
                            arg_name, name,
                        )));
                    }
                    arg_expr.ty().assert_eq(indexed_expr, ctx)?;
                }

                let args: Vec<String> = args
                    .iter()
                    .map(|f| {
                        let (_, e) = f;
                        
                        self.translate_expr(e, scope, ctx).unwrap()
                    })
                    .collect();
                let args = args.join(",");
                Ok(format!("new {}({})", name, args))
            }
            SemExpression::Embed(x) => {
                let e = self.translate_expr(x, scope, ctx)?;
                let e: Vec<_> = e.split('\n').collect();
                let e: Vec<_> = e.into_iter().map(|f| f.to_string()).collect();
                let e = e.join("\n");
                let len = e.len();
                let e = e.get(1..len - 1).unwrap_or("");
                Ok(e.to_string())
            }
            SemExpression::String(x) => Ok(format!("\"{x}\"")),
            SemExpression::Unit => Ok("Void.Unit".to_string()),
            SemExpression::Null(_) => Ok("null".to_string()),
            SemExpression::Cast(expr, ty) => {
                let is_sys_type = Utils::is_sys_type(ty);
                let real_ty = self.jit.to_js_type(*ty.clone());
                let e = self.translate_expr(expr, scope, ctx)?;
                let is_expr_sys_type = Utils::is_sys_type(expr.ty());

                let cast = if is_sys_type {
                    let ty = *ty.clone();
                    match (ty, is_expr_sys_type) {
                        (ir::Type::String, false) => {
                            format!("(String) {}", e)
                        }
                        (ir::Type::String, true) => {
                            format!("\"\" + {}", e)
                        }
                        _ => format!("SysConv.to_{}({})", real_ty, e)
                    }
                } else {
                    format!("(({}) {})", real_ty, e)
                };

                Ok(cast)
            }

            SemExpression::Abs(_x) => {
                let e = self.translate_expr(_x, scope, ctx)?;
                Ok(format!("Math.abs({})", e))
            }

            SemExpression::SimpleNullCheck(a) => {
                let a = a.clone();
                let e = self.translate_expr(&a, scope, ctx)?;
                Ok(format!("({} == null) ", e))
            }

            SemExpression::NullCheck(a, t) => {
                let a = a.clone();
                let ty = self.jit.to_js_type(*t.clone());
                let e = self.translate_expr(&a, scope, ctx)?;
                Ok(format!("({}.constructor === {ty})", e.clone()))
            }

            SemExpression::Block(_values) => {
                let (block, ret) = self.extract_block(node, scope, ctx)?;
                let block = self.indent_lines(block, 1);
                Ok(format!("{}\n{}", block, ret))
            }
            SemExpression::Match(cond, cases) => {
                let c = self.translate_expr(cond, scope, ctx)?;

                let len = cases.len();
                let last = cases.last().unwrap();
                let rest = cases.get(0..len - 1).unwrap();
                let rest = rest.to_vec();
                
                let mut cases: Vec<String> = vec![]; 
                let mut lamdas: Vec<String> = vec![]; 
                let mut call_chain: Vec<String> = vec![];
                for case in rest {
                    let (_case, body) = case;
                    match _case.expr() {
                        SemExpression::EnumLiteral(parent, child) => {
                            match child.expr() {
                                SemExpression::FunCall(name, args) => {
                                    let mut sc = scope.clone();
                                    let mut _args = vec![]; 
                                    for arg in args {
                                        match arg.expr() {
                                            SemExpression::Id(var_name) => {
                                                    sc.insert(var_name.clone(), arg.ty().clone());
                                                    _args.push(var_name.clone());
                                                }
                                                _ => unreachable!()
                                            }
                                        }
                                        let _args = _args.join(", ");
                                        let cmp = format!("({c}).constructor === {parent}.{name}");
                                        cases.push(cmp);
                                        let fx = format!("case_{parent}_{name}");
                                        let call = format!("{fx}({_args})");
                                        call_chain.push(call);

                                        let bd = match body.expr() {
                                            SemExpression::Block(_) | SemExpression::Lets(_,_) => {
                                                let (block, ret) = self.extract_block(&body, &mut sc, ctx)?;
                                                format!("{block}\nretrun {ret}")
                                            }
                                            _ => format!("return {}", self.translate_expr(&body, &mut sc, ctx)?)
                                        }; 

                                        let fx = format!("const {fx} = ({_args}) {{\n{bd}\n}}");
                                        lamdas.push(fx);
                                }

                                _ => todo!()
                            }

                        }
                        _ => unreachable!()
                    }
                }

                let mut cond = String::new(); 
                for pack in cases.iter().zip(call_chain) {
                    let (lhs, rhs) = pack; 
                    cond.push_str(&format!("{lhs} ? {rhs} :"));
                }

                let fx = "case_else_".to_string();
                let call = format!("{fx}()");

                cond.push_str(&call);
                let (_, body) = last;
                let bd = match body.expr() {
                    SemExpression::Block(_) | SemExpression::Lets(_,_) => {
                        let (block, ret) = self.extract_block(body, scope, ctx)?;
                        format!("{block}\nretrun {ret}")
                    }
                    _ => format!("return {}", self.translate_expr(body, scope, ctx)?)
                }; 

                let fx = format!("const {fx} = () {{\n{bd}\n}}");
                lamdas.push(fx);
                let lamdas = lamdas.join("\n"); 
                Ok(format!("{lamdas}\n{cond}\n"))
            }

            SemExpression::FunCall(func_name, args) => {
                self.translate_funcall(func_name, args, scope, ctx)
            }
            SemExpression::Conditional(cond, then, alt) => {
                self.translate_conditional(cond, then, alt, scope, ctx)
            }
            SemExpression::BinaryOp(op, left, right) => {
                let left_val = self.translate_expr(left, scope, ctx)?;
                let right_val = self.translate_expr(right, scope, ctx)?;

                match (op, left.ty(), right.ty()) {
                    (ir::BinaryOp::Equal, 
                        ir::Type::String | ir::Type::YourType(_) | ir::Type::EnumType(_, _), 
                        ir::Type::String | ir::Type::YourType(_) | ir::Type::EnumType(_, _), 
                    ) => {
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
                        ir::Type::String | ir::Type::YourType(_) | ir::Type::EnumType(_, _), 
                        ir::Type::String | ir::Type::YourType(_) | ir::Type::EnumType(_, _), 
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
                        ir::Type::String | ir::Type::YourType(_) | ir::Type::EnumType(_, _),
                        ir::Type::String | ir::Type::YourType(_) | ir::Type::EnumType(_, _),
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
                let (_elem_count, _elem_ty) = match node.ty() {
                    ir::Type::Array(elem_count, elem_ty) => (elem_count, elem_ty),
                    _ => {
                        return Err(CompilerError::BackendError(format!(
                            "Error in Array literal: {:?} is not of Array type",
                            node.ty()
                        )))
                    }
                };

                let arr: Vec<String> = elems
                    .iter()
                    .map(|f| self.translate_expr(f, scope, ctx).unwrap())
                    .collect();

                let arr = arr.join(", ");
                Ok(format!("[{arr}]"))
            }
            SemExpression::UnaryOp(op, n) => {
                let operand = self.translate_expr(n, scope, ctx)?;
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
            SemExpression::Let(id, init) => self.translate_val(id, init, scope, ctx),
            SemExpression::Lets(ids, expr) => {
                let mut s = String::new();
                let mut sc = scope.clone();

                s.push_str("{\n");
                for lt in ids {
                    let SemExpression::Let(name, exprl) = lt.clone() else {
                        panic!("expected let expression")
                    };
                    let a = self.translate_val(name.as_str(), &exprl, &mut sc,ctx)?;
                    s.push_str("\t\t");
                    s.push_str(&a);
                }

                match expr.expr() {
                    SemExpression::Block(_) => {
                        let (block, ret) = self.extract_block(expr, &mut sc, ctx)?;

                        s.push_str(&format!("\t\t{}", block));
                        s.push_str("\t\treturn ");
                        s.push_str(&ret);
                        s.push_str(";\n\t}");
                    }
                    _ => {
                        let e = self.translate_expr(expr, &mut sc, ctx)?; 
                        s.push_str("\t\treturn ");
                        s.push_str(&e);
                        s.push_str(";\n\t}");
                    }
                }; 

                // let e = self.translate_expr(expr, &mut sc).unwrap();

                let h = format!("() => {}", s);
                let var = format!("let block_{} = {};\n", self.block_count, h);
                let s = format!("{var}\n\tblock_{}()", self.block_count);
                self.block_count += 1;
                Ok(s)
            }
            SemExpression::Val(id, init) => self.translate_val(id, init, scope, ctx),
            SemExpression::Id(id) => {
                scope.get(id).ok_or_else(|| {
                    CompilerError::BackendError(format!("No variable {} available", id))
                })?;
                Ok(id.clone())
            }
            SemExpression::Char(val) => Ok(format!("'{}'", *val as char)),
            SemExpression::Bool(val) => Ok(format!("{}", *val)),
            SemExpression::Integer(val, _t) => {
                Ok(format!("{val}"))
            }

            SemExpression::Decimal(val, t) => {
                match *t.clone() {
                    Type::Float => Ok(format!("{}", *val)),
                    Type::Double => Ok(format!("{}", *val)),
                    _ => unreachable!(),
                }
            }
        }
    }

    fn translate_conditional(
        &mut self,
        cond: &SemNode,
        then: &SemNode,
        alt: &SemNode,
        scope: &mut HashMap<String, Type>,
        ctx: &mut SemContext
    ) -> Result<String, CompilerError> {

        let _cond_ty = "Boolean"; 
        let _then_ty = then.ty(); 
        let _alt_ty = alt.ty(); 

        let _jit = self.jit.clone(); 


        let mut ret_str = String::new(); 
        let cond = match cond.expr() {
            SemExpression::Block(_) => {
                let (block, ret) = self.extract_block(cond, scope, ctx)?;
                ret_str.push_str(&block);
                ret_str.push('\n');
                ret
            },
            _ => self.translate_expr(cond, scope, ctx)?
        }; 

        let then = match then.expr() {
            SemExpression::Block(_) => {
                let (block, ret) = self.extract_block(then, scope, ctx)?;
                ret_str.push_str(&block); 
                ret_str.push('\n');
                ret
            },
            _ => self.translate_expr(then, scope, ctx)?
        };

        let alt = match alt.expr() {
            SemExpression::Block(_) => {
                let (block, ret) = self.extract_block(alt, scope, ctx)?;
                ret_str.push_str(&block); 
                ret_str.push('\n');
                ret
            },
            _ => self.translate_expr(alt, scope, ctx)?
        }; 

        
        let st = format!("(({}) ? {} : {})", cond, then, alt);
        let st = format!("{ret_str}\n{st}");
        Ok(st)
    }

    fn translate_val(
        &mut self,
        id: &str,
        init: &SemNode,
        scope: &mut HashMap<String, Type>,
        ctx: &mut SemContext
    ) -> Result<String, CompilerError> {
        let ty = init.ty().clone();
        let node = init.clone();
        let val = self.translate_expr(&node, scope, ctx)?;
        scope.insert(id.to_string(), ty);

        match init.expr() {
            SemExpression::Lambda(args, _, _) => {
                let mut aa = vec![]; 
                for arg in args {
                    let (_, t) = arg;
                    aa.push(t.clone());
                }

                // let ty = Type::Lambda(ret.clone(), aa);
                Ok(format!("let {} = {}\n", id, val.clone()))
            }
            SemExpression::Block(_) | SemExpression::Lets(_, _) => {
                let splits: Vec<_> = val.split('\n').collect();
                let len = splits.len();
                let rest = splits.get(0..len - 1).unwrap();
                let rest = rest.to_vec();
                let rest = rest.join("\n");

                let last = splits.last().unwrap();
                let last = last.trim();
                Ok(format!("{}\t{} {} = {}\n", rest, "let", id, last))
            }
            SemExpression::Null(_) => Ok(format!("let {} = {}\n", id, val.clone())),
            _ => Ok(format!("let {id} = {val}\n"))
        }
        //self.vars.push_str(format!("{} {};", val_ty, id.clone()).as_str());
    }

    #[allow(unused)]
    fn translate_let(
        &mut self,
        id: &str,
        init: &SemNode,
        expr: &SemNode,
        scope: &mut HashMap<String, Type>,
        ctx: &mut SemContext
    ) -> Result<String, CompilerError> {
        let ty = init.ty().clone();
        let val = self.translate_expr(init, scope, ctx)?;
        let shadowed_var = scope.remove(id);
        scope.insert(id.into(), ty);
        let res = self.translate_expr(expr, scope, ctx)?;

        scope.remove(id);
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
        ctx: &mut SemContext
    ) -> Result<String, CompilerError> {
        let args_values = args
            .iter()
            .map(|arg| self.translate_expr(arg, scope, ctx).unwrap())
            .collect::<Vec<_>>();

        Ok(format!("{}({})", func_name, args_values.join(", ")))
    }
}
