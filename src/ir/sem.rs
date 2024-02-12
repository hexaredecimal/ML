use super::raw::*;
use super::*;
use crate::error::{CompilerError, Result};
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub enum SemExpression {
    Unit,
    Integer(i64, Box<Type>),
    Bool(bool),
    Decimal(f64, Box<Type>),
    Char(u8),
    String(String),
    Null(Box<Type>),
    Array(Vec<SemNode>),
    LambdaCall(Box<SemNode>, Vec<SemNode>),
    FunCall(String, Vec<SemNode>),
    Id(String), // TODO: semantic checking step
    BinaryOp(BinaryOp, Box<SemNode>, Box<SemNode>),
    UnaryOp(UnaryOp, Box<SemNode>),
    Conditional(Box<SemNode>, Box<SemNode>, Box<SemNode>),
    Let(String, Box<SemNode>),
    Lets(Vec<SemExpression>, Box<SemNode>),
    Val(String, Box<SemNode>),
    Block(Vec<SemNode>),
    SimpleNullCheck(Box<SemNode>),
    NullCheck(Box<SemNode>, Box<Type>),
    Match(Box<SemNode>, Vec<(SemNode, SemNode)>),
    Abs(Box<SemNode>),
    Embed(Box<SemNode>),
    RecordLiteral(String, Vec<(String, SemNode)>),
    EnumLiteral(String, Box<SemNode>),
    Cast(Box<SemNode>, Box<Type>),
    Lambda(Vec<(String, Type)>, Box<Type>, Box<SemNode>),
    Destructure(Vec<String>, Box<SemNode>),
}


#[derive(PartialEq, Debug)]
pub struct SemFunction {
    name: String,
    root: SemNode,
    args: Vec<(String, Type)>,
    ty: Type,
}

impl SemFunction {
    pub fn analyze(fun: RawFunction, ctx: &mut SemContext) -> Result<Self> {
        let name = fun.name.to_string();
        let (ret, arg_types) = fun.ty.into_function();
        let args = fun.args;

        ctx.append_new_vars(args.iter().cloned());
        let root = SemNode::analyze(fun.root, ctx)?;
        (&*root.ty()).assert_eq(&*ret)?;
        let ty = Type::Function(ret.clone(), arg_types);

        if fun.name == "main".to_string() && *ret.clone() != Type::Unit {
            return Err(CompilerError::BackendError("entry point main must return a unit type".to_string()));
        }

        Ok(Self {
            root,
            name,
            ty,
            args,
        })
    }

    pub fn ty(&self) -> &Type {
        &self.ty
    }
    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn root(&self) -> &SemNode {
        &self.root
    }
    pub fn args(&self) -> &Vec<(String, Type)> {
        &self.args
    }
}

#[derive(Debug, Clone)]
pub struct SemContext {
    funs: HashMap<String, Type>,
    vars: HashMap<String, Type>,
    pub records: HashMap<String, RecordType>, 
    pub enums: HashMap<String, EnumType>,
 
}

impl SemContext {
    pub fn from_funs(funs: impl IntoIterator<Item = (String, Type)>) -> Self {
        let funs = funs.into_iter().collect::<HashMap<_, _>>();
        let vars = HashMap::new();
        let records: HashMap<String, RecordType> = HashMap::new();
        let enums: HashMap<String, EnumType> = HashMap::new();

        Self { funs, vars, records, enums }
    }
    pub fn funs(&self) -> &HashMap<String, Type> {
        &self.funs
    }
    pub fn vars(&self) -> &HashMap<String, Type> {
        &self.vars
    }

    pub fn append_new_var(&mut self, id: String, ty: Type) {
        self.vars.insert(id, ty); 
    }

    pub fn append_new_vars(&mut self, it: impl IntoIterator<Item = (String, Type)>) {
        for (id, ty) in it {
            self.vars.insert(id, ty);
        }
    }

    #[allow(unused)]
    pub fn extend_var(&self, id: String, ty: Type) -> Self {
        let funs = self.funs.clone();
        let mut vars = self.vars.clone();
        vars.insert(id, ty);

        let records = self.records.clone(); 
        let enums = self.enums.clone(); 
        Self { funs, vars, records, enums }
    }

    #[allow(unused)]
    pub fn extend_vars(&self, it: impl IntoIterator<Item = (String, Type)>) -> Self {
        let funs = self.funs.clone();
        let mut vars = self.vars.clone();
        for (id, ty) in it {
            vars.insert(id, ty);
        }
        let records = self.records.clone(); 
        let enums = self.enums.clone(); 

        Self { funs, vars, records, enums }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct SemNode {
    ty: Type,
    expr: SemExpression,
}

impl SemNode {
    fn extract_enum_field(name: String, fields: Vec<EnumField>) -> Result<EnumField> {
        for field in fields.into_iter() {
            match field.clone() {
                EnumField::Rec(record) => {
                    if record.name == name  {
                        return Ok(field); 
                    }
                }
                EnumField::Id(n) => {
                    if n == name {
                        return Ok(field); 
                    }
                }
            }
        }
        return Err(CompilerError::BackendError(
            format!("Invalid enum field lookup for field `{}`", name)
        ));
    }

    pub fn analyze(node: RawNode, ctx: &mut SemContext) -> Result<Self> {
        let expr = match node.into_expr() {
            RawExpression::Lambda(args, ret, body) => {
                todo!()
            }
            RawExpression::Destructure(dest, e) => {
                let expr = SemNode::analyze(*e, ctx)?; 
                // TODO: recognize types for destructured fields, for now just use type any 
                for var in dest.clone().into_iter() {
                    ctx.vars.insert(var, Type::Any);
                }
                SemExpression::Destructure(dest.clone(), Box::new(expr))
            }
            RawExpression::RecordLiteral(parent, fields) => {
                let records = ctx.records.clone();
                let record_name = parent.clone(); 

                if records.contains_key(&record_name) == false {
                    return Err(CompilerError::BackendError(
                        format!("Invalid struct expression, `{}` is not a record type", parent)
                    ));
                }

                let record = records.get(&record_name).unwrap(); 

                if fields.len() != record.fields.len() {
                    return Err(CompilerError::BackendError(
                        format!(
                            "Invalid number of fields passed to struct literal for record type `{parent}`, expected {} but found {}",
                            record.fields.len(), 
                            fields.len()
                        )
                    ));
                }

                let mut args: Vec<(String, SemNode)> = Vec::new();
                for (passed, expected) in fields.into_iter().zip(record.fields.clone()) {
                    let (passed_name, passed_expr) = passed; 
                    let (expected_name, expected_type)  = expected; 

                    if passed_name != expected_name {
                        return Err(CompilerError::BackendError(
                            format!(
                                "Invalid field initialization. field `{passed_name}` doesn't exist in struct `{parent}`. Expected initialization of `{}` with value of type {}", 
                                expected_name, 
                                expected_type
                            )
                        )); 
                    }

                    let e = SemNode::analyze(passed_expr, ctx)?;
                    expected_type.assert_eq(e.ty())?;
                    args.push((passed_name, e)); 
                }

                SemExpression::RecordLiteral(parent.clone(), args)
            }
            RawExpression::EnumLiteral(parent, child) => {
                let enums = ctx.enums.clone(); 

                let enum_name = parent.clone(); 
                if enums.contains_key(&enum_name) == false {
                    return Err(CompilerError::BackendError(
                        format!("Invalid enum expression, `{}` is not an enum type", parent)
                    ));
                }

                match child.expr() {
                    RawExpression::FunCall(name, args) => {
                        let mut arguments: Vec<SemNode> = Vec::new();  
                        for arg in args.into_iter() {
                            let e = SemNode::analyze(arg.clone(), ctx)?; 
                            arguments.push(e); 
                        }
                        let inner  = SemExpression::FunCall(name.clone(), arguments);
                        let ty = Type::EnumType(parent.clone(), name.clone()); 
                        let inner = SemNode {expr: inner, ty: ty.clone()}; 
                        SemExpression::EnumLiteral(parent.clone(), Box::new(inner))
                    }
                    RawExpression::Id(name) => {
                        let ty = Type::EnumType(parent.clone(), name.clone()); 
                        let inner = SemExpression::Id(name.clone()); 
                        let inner = SemNode {expr: inner, ty: ty.clone()}; 
                        SemExpression::EnumLiteral(parent.clone(), Box::new(inner)) 
                    }
                    _ => unreachable!()
                }
            }
            RawExpression::SimpleNullCheck(e) => todo!("Implement simple null check"),
            RawExpression::LambdaCall(_, _) => todo!("Implement invoking a lamda expresssion"),
            RawExpression::Embed(x) => {
                let e = SemNode::analyze(*x, ctx)?; 
                SemExpression::Embed(Box::new(e))
            }
            RawExpression::String(x) => SemExpression::String(x.clone()), 
            RawExpression::Unit => SemExpression::Unit, 
            RawExpression::Let(_, _) => unreachable!(),
            RawExpression::Null(t) => SemExpression::Null(t.clone()), 
            RawExpression::Cast(e, t) => {
                let e = SemNode::analyze(*e, ctx)?; 
                SemExpression::Cast(Box::new(e), t)
            }
            RawExpression::Abs(x) => SemExpression::Abs(Box::new(SemNode::analyze(*x, ctx)?)), 
            RawExpression::Match(cond, cases) => {
                let e = SemNode::analyze(*cond, ctx)?; 

                let len = cases.len(); 

                let mut cs: Vec<_> = Vec::new(); 
                for (i, (case, body)) in cases.into_iter().enumerate() {
                    let (c, b) = (case, body); 

                    if i == len -1 { // Last case
                        let is_else = match c.expr() {
                            RawExpression::Id(i) => i == "_", 
                            _ => false
                        }; 

                        if is_else == false {
                            return Err(CompilerError::BackendError("expected last case to be the default case in match expression".to_string()));
                        } else {
                            let d = SemNode::analyze(b, ctx).unwrap();
                            cs.push((d.clone(), d));

                        }
                    } else {

                        let mut ct = ctx.clone(); 
                        let c = match c.expr() {
                            RawExpression::EnumLiteral(parent, child) => {
                                let enums = ctx.enums.clone(); 

                                if enums.contains_key(parent) == false {
                                    return Err(CompilerError::BackendError(
                                        format!("Invalid enum expression, `{}` is not an enum type", parent)
                                    ));
                                }

                                let enumer = enums.get(parent).unwrap(); 
                                match child.expr() {
                                    RawExpression::FunCall(name, args) => {
                                        let field = SemNode::extract_enum_field(name.clone(), enumer.fields.clone())?;
                                        match field {
                                            EnumField::Rec(record) => {
                                                if name.clone() != record.name {
                                                    return Err(CompilerError::BackendError(
                                                        format!("enum `{parent} has no field with name {name}`")
                                                    ));
                                                }

                                                if args.len() != record.fields.len() {
                                                    return Err(CompilerError::BackendError(
                                                        format!("invalid number of arguments passed to enum field `{name}` from enum `{parent}`")
                                                    ));
                                                }

                                                let mut _args: Vec<SemNode> = Vec::new(); 
                                                for (node, f) in args.into_iter().zip(record.fields) {
                                                    let RawExpression::Id(n) = node.expr() else {
                                                        return Err(CompilerError::BackendError(
                                                            format!("Attempt to destructure an invalid expression in enum field `{name}` of enum type `{parent}`")
                                                        ));
                                                    }; 

                                                    let (r, ty) = f; 
                                                    if r != n.clone() {
                                                        return Err(CompilerError::BackendError(
                                                            format!("Attempt to destructure an invalid or non existing field property `{n}` in enum field `{name}` of enum type `{parent}`. ")
                                                        ));
                                                    }
                                                    ct.vars.insert(n.clone(), ty.clone()); 
                                                    _args.push(SemNode{expr: SemExpression::Id(r), ty});
                                                }

                                                let ty = Type::EnumType(parent.clone(), name.clone()); 
                                                let call = SemExpression::FunCall(name.clone(), _args);
                                                let call = SemNode {expr: call, ty: ty.clone()};
                                                let enum_val = SemExpression::EnumLiteral(parent.clone(), Box::new(call));
                                                let enum_val = SemNode {expr: enum_val, ty}; 
                                                enum_val
                                            }
                                            EnumField::Id(_name) => {
                                                unreachable!()
                                            }
                                        }
                                    }
                                    RawExpression::Id(_name) => {
                                        let ty = Type::EnumType(parent.clone(), _name.clone()); 
                                        let id = SemExpression::Id(_name.clone());
                                        let id = SemNode {expr: id, ty: ty.clone()};
                                        let enum_val = SemExpression::EnumLiteral(parent.clone(), Box::new(id));
                                        SemNode {expr: enum_val, ty} 
                                    }, 
                                    _ => unreachable!()
                                }
                            }
                            _ => SemNode::analyze(c, ctx).unwrap()
                        }; 

                        let d = SemNode::analyze(b, &mut ct).unwrap();
                        cs.push((c, d));
                    }
                }

                SemExpression::Match(Box::new(e), cs)
            }
            RawExpression::Char(x) => SemExpression::Char(x), 
            RawExpression::NullCheck(e, t) => {
                let e = SemNode::analyze(*e, ctx)?;
                SemExpression::NullCheck(Box::new(e), t)
            }, 
            RawExpression::Block(values) => {
                let mut vals  = vec![]; 
                for val in values {
                    let node = SemNode::analyze(val, ctx)?;
                    vals.push(node);
                } 
                SemExpression::Block(vals)
            }
            RawExpression::Integer(val, t) => SemExpression::Integer(val, t.clone()),
            RawExpression::Bool(val) => SemExpression::Bool(val),
            RawExpression::Decimal(val, t) => SemExpression::Decimal(val, t.clone()),
            RawExpression::Id(val) => SemExpression::Id(val),
            RawExpression::Val(id, val) => {
                let val_ = Box::new(Self::analyze(*val.clone(), ctx)?);
                ctx.append_new_var(id.clone(), val_.ty().clone()); 
                SemExpression::Val(id, val_) 
            }
            RawExpression::Lets(lhss, expr) => {
                let mut es: Vec<SemExpression> = Vec::new(); 
                for lhs in lhss {
                    match lhs {
                        RawExpression::Let(name, val) => {
                            let val = Box::new(Self::analyze(*val, ctx)?);
                            ctx.append_new_var(name.clone(), val.ty().clone());
                            es.push(SemExpression::Let(name, val));
                        }
                        _ => unreachable!()
                    } 
                }
                let expr = Box::new(Self::analyze(*expr, ctx)?);
                SemExpression::Lets(es.clone(), expr)
            }
            RawExpression::Array(val) => {
                let mut out = Vec::new();
                for n in val {
                    out.push(Self::analyze(n, ctx)?);
                }
                SemExpression::Array(out)
            }
            RawExpression::BinaryOp(op, a, b) => SemExpression::BinaryOp(
                op,
                Box::new(Self::analyze(*a, ctx)?),
                Box::new(Self::analyze(*b, ctx)?),
            ),
            RawExpression::UnaryOp(op, a) => {
                SemExpression::UnaryOp(op, Box::new(Self::analyze(*a, ctx)?))
            }
            RawExpression::Conditional(cond, then, alt) => SemExpression::Conditional(
                Box::new(Self::analyze(*cond, ctx)?),
                Box::new(Self::analyze(*then, ctx)?),
                Box::new(Self::analyze(*alt, ctx)?),
            ),
            RawExpression::FunCall(id, args) => {
                let mut out_args = Vec::new();
                for a in args {
                    out_args.push(Self::analyze(a, ctx)?);
                }
                SemExpression::FunCall(id, out_args)
            }
        };

        let ty = match &expr {
            SemExpression::Lambda(args, ret, body) => *ret.clone(), 
            SemExpression::Destructure(p, e) => Type::Any,
            SemExpression::SimpleNullCheck(e) => Type::Bool,
            SemExpression::LambdaCall(e, args) => todo!(), 
            SemExpression::RecordLiteral(parent, child) => Type::UserType(parent.clone()),
            SemExpression::EnumLiteral(parent, child) => {
                let ty = match child.expr() {
                    SemExpression::FunCall(n,_) => n, 
                    SemExpression::Id(n) => n, 
                    _ => unreachable!()
                }; 
                Type::EnumType(parent.clone(), ty.clone())
            },
            SemExpression::String(_) => Type::String,
            SemExpression::Embed(_) => Type::Unit, 
            SemExpression::Unit => Type::Unit, 
            SemExpression::Null(t) => *t.clone(), 
            SemExpression::Cast(_, t) => *t.clone(), 
            SemExpression::Abs(x) => x.ty().clone(), 
            SemExpression::Match(_cond, cases) => {
                if cases.len() == 0 {
                    println!("Invalid match expression with no cases");
                    panic!();
                }

                let case = cases.first().unwrap();
                let (_, body) = case; 
                body.ty().clone()
            }
            SemExpression::Char(_) => Type::Char, 
            SemExpression::NullCheck(a, _) => Type::Bool, 
            SemExpression::Block(v) => {
                if v.len() == 0 {
                    return Err(CompilerError::InvalidBlock);
                }

                let node = (*v).last().unwrap(); 
                node.ty.clone()
            }
            SemExpression::Integer(_, t) => *t.clone(),
            SemExpression::Bool(_) => Type::Bool,
            SemExpression::Decimal(_, t) => *t.clone(),
            SemExpression::Id(name) => match ctx.vars().get(name) {
                Some(ty) => ty.clone(),
                None => {
                    return Err(CompilerError::UnknownVariable(name.to_string()));
                },
            },
            SemExpression::Let(_id, expr) => expr.ty().clone(),
            SemExpression::Lets(_id, expr) => expr.ty().clone(),
            SemExpression::Val(_id, _val) => _val.ty().clone(),
            SemExpression::FunCall(id, args) => {
                let ftype = match ctx.funs().get(id) {
                    Some(ty) => ty.clone(),
                    None => return Err(CompilerError::UnknownFunction(id.to_string())),
                };

                let (ret, argdefs) = ftype.as_function();

                let mut is_varargs = false; 
                if argdefs.len() > 0 {
                    let (_, t) = argdefs.last().unwrap();
                    match t {
                        Type::VarArgs => {
                            is_varargs = true;
                        }
                        _ => ()
                    }
                }

                let ret = if is_varargs {
                    (*ret).clone()
                } else {
                    if args.len() != argdefs.len() {
                        return Err(CompilerError::WrongNumberOfArguments(
                            id.to_string(),
                            argdefs.len(),
                            args.len(),
                        ));
                    };

                    for ((_arg_name, arg_type), arg_node) in argdefs.iter().zip(args.iter()) {
                        arg_type.assert_eq(arg_node.ty())?;
                    }
                    (*ret).clone()
                };
                ret
            }
            SemExpression::Array(vals) => {
                let inner_type = if !vals.is_empty() {
                    if vals.len() > 1 {
                        let first = vals[0].ty().clone();
                        let seconds = vals[1].ty().clone();
                        match (first.clone(), seconds.clone()) {
                            (Type::EnumType(parent1, _), Type::EnumType(parent2, _)) => {
                                let t = if parent1 == parent2 {
                                    Type::UserType(parent2)
                                } else {
                                    first.clone()
                                };
                                t
                            }
                            _ => first
                        }
                    } else{
                        vals[0].ty().clone()
                    }
                } else {
                    Type::Int
                };
                for val in vals.iter() {
                    val.ty().assert_eq(&inner_type)?;
                }
                Type::Array(vals.len() as u64, Box::new(inner_type))
            }
            SemExpression::UnaryOp(op, a) => {
                op.check_ty(a.ty())?;
                match op {
                    UnaryOp::Minus => Type::Int,
                    UnaryOp::Not => Type::Bool,
                }
            }
            SemExpression::BinaryOp(op, a, b) => {
                op.check_ty(a.ty(), b.ty())?;
                match op {
                    BinaryOp::Multiply | BinaryOp::Divide | BinaryOp::Add | BinaryOp::Sub => {
                        a.ty().clone()
                    }
                    BinaryOp::LessThan
                    | BinaryOp::LessThanOrEqual
                    | BinaryOp::GreaterThan
                    | BinaryOp::GreaterThanOrEqual
                    | BinaryOp::Equal
                    | BinaryOp::NotEqual
                    | BinaryOp::And
                    | BinaryOp::Or => Type::Bool,
                    BinaryOp::ArrayDeref => {
                        if let Type::Array(_, elem_type) = a.ty() {
                            if let SemExpression::Integer(_, _) = b.expr() {
                                (**elem_type).clone()
                            } else {
                                unreachable!()
                            }
                        } else if let Type::List(elem_type) = a.ty() {
                            if let SemExpression::Integer(_, _) = b.expr() {
                                (**elem_type).clone()
                            } else {
                                unreachable!()
                            }
                        } else {
                            return Err(CompilerError::CannotIndex(a.ty().clone()));
                        }
                    }
                }
            }
            SemExpression::Conditional(cond, then, alt) => {
                cond.ty().assert_eq(&Type::Bool)?;
                then.ty().assert_eq(alt.ty())?;
                then.ty().clone()
            }
        };

        Ok(Self { expr, ty })
    }
    pub fn ty(&self) -> &Type {
        &self.ty
    }
    pub fn expr(&self) -> &SemExpression {
        &self.expr
    }
}
