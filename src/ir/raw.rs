use std::fmt::Debug;

use super::*;

#[derive(Debug, Clone)]
pub enum TopLevel {
    RawFunction {
        name: String,
        root: RawNode,
        args: Vec<(String, Type)>,
        ty: Type,
    },

    RecordType {
        name: String,
        fields: Vec<(String, Type)>,
    },

    EnumType {
        name: String, 
        fields: Vec<EnumField>
    },

    Import {
        path: Vec<String>
    },

    Alias {
        name: String, 
        value: Type
    }
}

#[derive(Debug, Clone)]
pub struct Alias {
    pub name: String,
    pub value: Type
}

#[derive(Debug, Clone)]
pub struct Import {
    pub path: Vec<String>
}

#[derive(Debug, Clone)]
pub enum EnumField {
    Rec(RecordType),
    Id(String)
}

#[derive(Clone, Debug)]
pub struct EnumType {
    pub name: String, 
    pub fields: Vec<EnumField>
}


#[derive(Clone, Debug)]
pub struct RecordType {
    pub name: String,
    pub fields: Vec<(String, Type)>,
}

#[derive(Clone, Debug)]
pub struct RawFunction {
    pub name: String,
    pub root: RawNode,
    pub args: Vec<(String, Type)>,
    pub ty: Type,
}

#[derive(Debug, PartialEq, Clone)]
pub enum RawExpression {
    Unit,
    Integer(i64, Box<Type>),
    Bool(bool),
    Decimal(f64, Box<Type>),
    Char(u8),
    String(String),
    Null(Box<Type>),
    Array(Vec<RawNode>),
    LambdaCall(Box<RawNode>, Vec<RawNode>),
    FunCall(String, Vec<RawNode>),
    Id(String), // TODO: semantic checking step
    BinaryOp(BinaryOp, Box<RawNode>, Box<RawNode>),
    UnaryOp(UnaryOp, Box<RawNode>),
    Conditional(Box<RawNode>, Box<RawNode>, Box<RawNode>),
    Let(String, Box<RawNode>),
    Lets(Vec<RawExpression>, Box<RawNode>),
    Val(String, Box<RawNode>),
    Block(Vec<RawNode>),
    SimpleNullCheck(Box<RawNode>),
    NullCheck(Box<RawNode>, Box<Type>),
    Match(Box<RawNode>, Vec<(RawNode, RawNode)>),
    Abs(Box<RawNode>),
    Embed(Box<RawNode>),
    RecordLiteral(String, Vec<(String, RawNode)>),
    EnumLiteral(String, Box<RawNode>),
    Cast(Box<RawNode>, Box<Type>),
    Lambda(Vec<(String, Type)>, Box<Type>, Box<RawNode>),
    Destructure(Vec<String>, Box<RawNode>),
}

pub enum TempExpr {
    Id(String), 
    Ids(Vec<String>)
}

pub trait Node: PartialEq + Sized {
    fn expr(&self) -> &RawExpression;
    fn into_expr(self) -> RawExpression;
}

#[derive(PartialEq, Debug, Clone)]
pub struct RawNode {
    expr: RawExpression,
}

impl RawNode {
    pub fn new(expr: RawExpression) -> Self {
        Self { expr }
    }
}

impl Node for RawNode {
    fn expr(&self) -> &RawExpression {
        &self.expr
    }
    fn into_expr(self) -> RawExpression {
        self.expr
    }
}
