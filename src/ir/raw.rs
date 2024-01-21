use super::*;

#[derive(Debug, Clone)]
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
    FunCall(String, Vec<RawNode>),
    Id(String), // TODO: semantic checking step
    BinaryOp(BinaryOp, Box<RawNode>, Box<RawNode>),
    UnaryOp(UnaryOp, Box<RawNode>),
    Conditional(Box<RawNode>, Box<RawNode>, Box<RawNode>),
    Let(String, Box<RawNode>), 
    Lets(Vec<RawExpression>, Box<RawNode>),
    Val(String, Box<RawNode>), 
    Block(Vec<RawNode>),
    NullCheck(Box<RawNode>),
    Match(Box<RawNode>, Vec<(RawNode, RawNode)>),
    Abs(Box<RawNode>),
    Embed(Box<RawNode>), 
    Cast(Box<RawNode>, Box<Type>),
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
