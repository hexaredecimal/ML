use super::raw::*;
use super::*;
use crate::error::{CompilerError, Result};
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub enum SemExpression {
    Integer(i64, Box<Type>),
    Bool(bool),
    Decimal(f64, Box<Type>),
    Char(u8),
    Array(Vec<SemNode>),
    Id(String), // TODO: semantic checking step
    Null(Box<Type>), 
    FunCall(String, Vec<SemNode>),
    BinaryOp(BinaryOp, Box<SemNode>, Box<SemNode>),
    UnaryOp(UnaryOp, Box<SemNode>),
    Conditional(Box<SemNode>, Box<SemNode>, Box<SemNode>),
    Let(String, Box<SemNode>),
    Lets(Vec<SemExpression>, Box<SemNode>),
    Val(String, Box<SemNode>),
    Block(Vec<SemNode>), 
    NullCheck(Box<SemNode>),
    Match(Box<SemNode>, Vec<(SemNode, SemNode)>),
    Abs(Box<SemNode>),
    Cast(Box<SemNode>, Box<Type>),
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
        let ty = Type::Function(ret, arg_types);

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

#[derive(Debug)]
pub struct SemContext {
    funs: HashMap<String, Type>,
    vars: HashMap<String, Type>,
}

impl SemContext {
    pub fn from_funs(funs: impl IntoIterator<Item = (String, Type)>) -> Self {
        let funs = funs.into_iter().collect::<HashMap<_, _>>();
        let vars = HashMap::new();
        Self { funs, vars }
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

    pub fn extend_var(&self, id: String, ty: Type) -> Self {
        let funs = self.funs.clone();
        let mut vars = self.vars.clone();
        vars.insert(id, ty);
        Self { funs, vars }
    }

    pub fn extend_vars(&self, it: impl IntoIterator<Item = (String, Type)>) -> Self {
        let funs = self.funs.clone();
        let mut vars = self.vars.clone();
        for (id, ty) in it {
            vars.insert(id, ty);
        }
        Self { funs, vars }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct SemNode {
    ty: Type,
    expr: SemExpression,
}

impl SemNode {
    pub fn analyze(node: RawNode, ctx: &mut SemContext) -> Result<Self> {
        let expr = match node.into_expr() {
            RawExpression::Let(_, _) => unreachable!(),
            RawExpression::Null(t) => SemExpression::Null(t.clone()), 
            RawExpression::Cast(e, t) => {
                let e = SemNode::analyze(*e, ctx)?; 
                SemExpression::Cast(Box::new(e), t)
            }
            RawExpression::Abs(x) => SemExpression::Abs(Box::new(SemNode::analyze(*x, ctx)?)), 
            RawExpression::Match(cond, cases) => {
                let e = SemNode::analyze(*cond, ctx)?;
                fn make_comp(cond: SemNode, other: SemNode) -> Result<SemNode> {
                    Ok(SemNode { 
                        ty: Type::Bool, 
                        expr: SemExpression::BinaryOp(BinaryOp::Equal, Box::new(cond), Box::new(other)) 
                    })
                }

                fn make_cond(cond: SemNode, other: SemNode, r: Option<SemNode>) -> Result<SemNode> {
                    let r = r.unwrap_or(other.clone()); 
                    Ok(SemNode { 
                        ty: Type::Bool, 
                        expr: SemExpression::Conditional(Box::new(cond), Box::new(other), Box::new(r)) 
                    })
                }

                fn construct_ast(cond: SemNode, cases_: Vec<(RawNode, RawNode)>, ctx: &mut SemContext) -> Result<SemNode> {
                    let len = cases_.len(); 
                    for i in 0 .. len {
                        let left = cases_.get(i);  
                        let right = cases_.get(i + 1);

                        if left.is_some() && right.is_none() {
                            let (_, l_bdy) = left.unwrap().clone(); 
                            return SemNode::analyze(l_bdy, ctx); 
                            //return Ok(left.unwrap().clone().1); 
                        } else if left.is_some() && right.is_some() {
                            let (l_cond, l_body) = left.unwrap(); 
                            let l = SemNode::analyze(l_cond.clone(), ctx)?; 
                            let l_b = SemNode::analyze(l_body.clone(), ctx)?; 
                            let cmp = make_comp(cond.clone(), l).unwrap(); 
                            //right.unwrap();

                            let right = right.clone().unwrap(); 
                            let (r_cond, r_body) = right;  
                            let last = cases_.last().unwrap();
                            if right.clone() == last.clone() {
                                let is_else = match r_cond.expr().clone() {
                                    RawExpression::Id(x) => x == "_", 
                                    _ => false, 
                                };

                                if !is_else {
                                    println!("Expected last condition to be default case in match"); 
                                    panic!(); 
                                }

                                let r_b = SemNode::analyze(r_body.clone(), ctx)?; 
                                return Ok(make_cond(cmp, l_b, Some(r_b)).unwrap()); 
                            }

                            let r = SemNode::analyze(r_cond.clone(), ctx)?; 
                            //let r_b = SemNode::analyze(r_body.clone(), ctx)?; 
                            let cmp2 = make_comp(cond.clone(), r).unwrap(); 
                            let rv = cases_.get(i + 1 .. len).unwrap();
                            let rv = rv.to_vec(); 
                            //println!("rv: {:?}", rv);
                            return Ok(make_cond(cmp, l_b, Some(construct_ast(cmp2, rv, ctx).unwrap())).unwrap());
                        }
                    }

                    return Ok(cond.clone());
                }

                let ast = construct_ast(e, cases.clone(), ctx)?;
                ast.expr
            }
            RawExpression::Char(x) => SemExpression::Char(x), 
            RawExpression::NullCheck(e) => {
                let e = SemNode::analyze(*e, ctx)?;
                SemExpression::NullCheck(Box::new(e))
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
            SemExpression::Null(t) => *t.clone(), 
            SemExpression::Cast(_, t) => *t.clone(), 
            SemExpression::Abs(x) => x.ty().clone(), 
            SemExpression::Match(cond, cases) => {
                if cases.len() == 0 {
                    println!("Invalid match expression with no cases");
                    panic!();
                }

                let case = cases.first().unwrap();
                let (_, body) = case; 
                body.ty().clone()
            }
            SemExpression::Char(_) => Type::Char, 
            SemExpression::NullCheck(a) => Type::Bool, 
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
                if args.len() != argdefs.len() {
                    return Err(CompilerError::WrongNumberOfArguments(
                        id.to_string(),
                        argdefs.len(),
                        args.len(),
                    ));
                }

                for ((_arg_name, arg_type), arg_node) in argdefs.iter().zip(args.iter()) {
                    arg_type.assert_eq(arg_node.ty())?;
                }
                (*ret).clone()
            }
            SemExpression::Array(vals) => {
                let inner_type = if !vals.is_empty() {
                    vals[0].ty().clone()
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
