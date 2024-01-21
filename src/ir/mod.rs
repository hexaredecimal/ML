use crate::error::{CompilerError, Result};
pub mod raw;
pub mod sem;

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOp {
    Minus,
    Not,
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinaryOp {
    Equal,
    Multiply,
    Divide,
    Add,
    Sub,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    NotEqual,
    And,
    Or,
    ArrayDeref,
}

impl std::fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                UnaryOp::Minus => "-",
                UnaryOp::Not => "!",
            }
        )
    }
}

impl UnaryOp {
    pub fn check_ty(&self, operand: &Type) -> Result<()> {
        let allowed_types = match self {
            UnaryOp::Minus => vec![Type::Int, Type::Float],
            UnaryOp::Not => vec![Type::Bool],
        };
        if !allowed_types.contains(operand) {
            return Err(CompilerError::WrongUnaryOperatorType(
                operand.clone(),
                self.clone(),
            ));
        }
        Ok(())
    }
}

impl BinaryOp {
    pub fn check_ty(&self, left_operand: &Type, right_operand: &Type) -> Result<()> {
        if let BinaryOp::ArrayDeref = self {
            if let Type::Array(_, _) = left_operand {
                if right_operand != &Type::Usize {
                    return Err(CompilerError::WrongBinaryOperatorType(
                        right_operand.clone(),
                        self.clone(),
                    ));
                }
            }else if let Type::List(_) = left_operand {
                if right_operand != &Type::Int {
                    return Err(CompilerError::WrongBinaryOperatorType(
                        right_operand.clone(),
                        self.clone(),
                    ));
                }
            }
            else {
                return Err(CompilerError::WrongBinaryOperatorType(
                    left_operand.clone(),
                    self.clone(),
                ));
            }
        } else {
            let allowed_types = match self {
                BinaryOp::LessThan
                | BinaryOp::GreaterThan
                | BinaryOp::Multiply
                | BinaryOp::Divide
                | BinaryOp::Add
                | BinaryOp::Sub
                | BinaryOp::LessThanOrEqual
                | BinaryOp::GreaterThanOrEqual => {
                    vec![Type::Float, Type::Int, Type::Double, Type::Char, Type::Int8, Type::Int16, Type::Int32, Type::Int64,Type::Int128, Type::String]
                }
                BinaryOp::Equal | BinaryOp::NotEqual => {
                    vec![Type::Float, Type::Int, Type::Bool, Type::Double, Type::Char, Type::Int8, Type::Int16, Type::Int32, Type::Int64,Type::Int128, Type::String]
                },
                BinaryOp::Or | BinaryOp::And => vec![Type::Bool],
                BinaryOp::ArrayDeref => unreachable!(),
            };

            left_operand.assert_eq(right_operand)?;
            let operand = left_operand; // same types
            if !allowed_types.contains(operand) {
                return Err(CompilerError::WrongBinaryOperatorType(
                    operand.clone(),
                    self.clone(),
                ));
            }
        }
        Ok(())
    }
}

impl std::fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BinaryOp::Multiply => "*",
                BinaryOp::Divide => "/",
                BinaryOp::Add => "+",
                BinaryOp::Sub => "-",
                BinaryOp::LessThan => "<",
                BinaryOp::LessThanOrEqual => "<=",
                BinaryOp::GreaterThan => ">",
                BinaryOp::GreaterThanOrEqual => ">=",
                BinaryOp::Equal => "==",
                BinaryOp::NotEqual => "!=",
                BinaryOp::And => "&&",
                BinaryOp::Or => "||",
                BinaryOp::ArrayDeref => "[]",
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    VarArgs, 
    Unit,
    String,
    Usize,    // System Dependent
    Int,    // System Dependent
    Int32,
    Int64,
    Int128,
    Int16,
    Int8,
    Bool,
    Float,
    Double,
    Char,
    Array(u64, Box<Type>),
    List(Box<Type>),
    Function(Box<Type>, Vec<(String, Type)>),
}

impl Type {
    pub fn as_function(&self) -> (&Type, &Vec<(String, Type)>) {
        match self {
            Type::Function(ret, args) => (&ret, &args),
            _ => panic!("Not a Function type"),
        }
    }
    pub fn into_function(self) -> (Box<Type>, Vec<(String, Type)>) {
        match self {
            Type::Function(ret, args) => (ret, args),
            _ => panic!("Not a Function type"),
        }
    }
    pub fn assert_eq(&self, other: &Self) -> Result<()> {
        if self != other {
            match (self, other) {
                (Type::List(i), Type::Array(_, t)) => {
                    let e = i.assert_eq(t);
                    if e.is_err() {
                        return Err(CompilerError::TypeConflict(self.clone(), other.clone()))
                    }
                    return Ok(()); 
                } 
                _ => {
                    match other {
                        Type::Float 
                        | Type::Int 
                        | Type::Double 
                        | Type::Char 
                        | Type::Int8 
                        | Type::Int16 
                        | Type::Int32 
                        | Type::Int64 
                        | Type::Int128
                        | Type::Bool => return Ok(()), 
                        _ => return Err(CompilerError::TypeConflict(self.clone(), other.clone()))
                    }
                }
            }; 
        }
        Ok(())
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Type::VarArgs => "...".to_string(), 
                Type::Unit => "()".to_string(), 
                Type::Usize => "usize".to_string(), 
                Type::String => "String".to_string(), 
                Type::Char => "Char".to_string(),
                Type::List(t) => format!("List[{}]", t),
                Type::Double => "Double".to_string(),
                Type::Int => "Int".to_string(),
                Type::Int8 => "Int8".to_string(),
                Type::Int16 => "Int16".to_string(),
                Type::Int32 => "Int32".to_string(),
                Type::Int64 => "Int64".to_string(),
                Type::Int128 => "Int128".to_string(),
                Type::Bool => "Bool".to_string(),
                Type::Float => "Float".to_string(),
                Type::Array(len, ty) => format!("[{};{}]", ty, len),
                Type::Function(ret, args) => format!(
                    "fun ({}): {}",
                    args.iter()
                        .map(|arg| {
                            let (name, ty) = arg;
                            format!("{}: {}", name, ty)
                        })
                        .collect::<Vec<String>>()
                        .join(","),
                    ret,
                ),
            }
        )
    }
}
