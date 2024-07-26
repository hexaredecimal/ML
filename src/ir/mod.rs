use crate::error::{CompilerError, Result};
pub mod raw;
pub mod sem;

use sem::SemContext; 
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
    Mod,
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
    pub fn check_ty(&self, left_operand: &Type, right_operand: &Type, ctx: &mut SemContext) -> Result<()> {
        if let BinaryOp::ArrayDeref = self {
            if let Type::Array(_, _) = left_operand {
                if right_operand != &Type::Int {
                    return Err(CompilerError::WrongBinaryOperatorType(
                        right_operand.clone(),
                        self.clone(),
                    ));
                }
            } else if let Type::List(_) = left_operand {
                if right_operand != &Type::Int {
                    return Err(CompilerError::WrongBinaryOperatorType(
                        right_operand.clone(),
                        self.clone(),
                    ));
                }
            } else {
                return Err(CompilerError::WrongBinaryOperatorType(
                    left_operand.clone(),
                    self.clone(),
                ));
            }
        } else {
            let allowed_types = match self {
                BinaryOp::LessThan
                | BinaryOp::Mod
                | BinaryOp::GreaterThan
                | BinaryOp::Multiply
                | BinaryOp::Divide
                | BinaryOp::Add
                | BinaryOp::Sub
                | BinaryOp::LessThanOrEqual
                | BinaryOp::GreaterThanOrEqual => {
                    let mut ts = vec![
                        Type::Float,
                        Type::Long,
                        Type::Short, 
                        Type::Byte,
                        Type::Int,
                        Type::Double,
                        Type::Char,
                        Type::Int8,
                        Type::Int16,
                        Type::Int32,
                        Type::Int64,
                        Type::Int128,
                        Type::String,
                        Type::Any,
                    ];


                    if self == &BinaryOp::Add { match (left_operand, right_operand) {
                        (Type::String, Type::YourType(_)) => ts.push(right_operand.clone()),
                        (Type::String, Type::EnumType(_, _)) => ts.push(right_operand.clone()), 
                        (Type::YourType(_), Type::String) => ts.push(left_operand.clone()),
                        (Type::EnumType(_, _), Type::String) => ts.push(left_operand.clone()),
                        _ => (),
                    } };
                    ts
                }
                BinaryOp::Equal | BinaryOp::NotEqual => {
                    vec![
                        Type::Float,
                        Type::Long,
                        Type::Short, 
                        Type::Byte,
                        Type::Int,
                        Type::Bool,
                        Type::Double,
                        Type::Char,
                        Type::Int8,
                        Type::Int16,
                        Type::Int32,
                        Type::Int64,
                        Type::Int128,
                        Type::String,
                    ]
                }
                BinaryOp::Or | BinaryOp::And => vec![Type::Bool],
                BinaryOp::ArrayDeref => unreachable!(),
            };

            left_operand.assert_eq(right_operand, ctx)?;
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
                BinaryOp::Mod => "%", 
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
    VarArgs(Box<Type>),
    Unit,
    String,
    Any,
    Usize, // System Dependent
    Int,   // System Dependent
    Int32,
    Int64,
    Int128,
    Int16,
    Int8,
    Bool,
    Float,
    Double,
    Char,
    Long,
    Short,
    Byte,
    Array(u64, Box<Type>),
    List(Box<Type>),
    Lambda(Box<Type>, Vec<Type>),
    Function(Box<Type>, Vec<(String, Type)>),
    EnumType(String, String),
    YourType(String), // Tobe looked up in the type table
    Lifter(String),
}

impl Type {
    pub fn as_function(&self) -> (&Type, &Vec<(String, Type)>) {
        match self {
            Type::Function(ret, args) => (ret, args),
            _ => panic!("Not a Function type"),
        }
    }
    pub fn into_function(self) -> (Box<Type>, Vec<(String, Type)>) {
        match self {
            Type::Function(ret, args) => (ret, args),
            _ => panic!("Not a Function type"),
        }
    }
    pub fn assert_eq(&self, other: &Self, ctx: &mut SemContext) -> Result<()> {
        if self != other {
            match (self, other) {
                (Type::Lambda(ret, _), other) => return ret.assert_eq(other, ctx),
                (other, Type::Lambda(ret, _)) => return ret.assert_eq(other, ctx),
                (Type::VarArgs(bx), t) => {
                    match *bx.clone() {
                        Type::YourType(x) => {
                            if x.is_empty() {
                                return Ok(());
                            } 
                            return bx.assert_eq(t, ctx);
                        }
                        ty => {
                            return ty.assert_eq(t, ctx);
                        }
                    }
                }

                (Type::YourType(tname), Type::Unit) => {
                    if ctx.aliases.contains_key(tname) {
                        let alias = ctx.aliases.get(tname).unwrap();
                        let ty = alias.value.clone(); 
                        // println!("{tname} -> {ty}"); 
                        if ty == Type::Unit {
                            return Ok(()); 
                        }
                    } 
                    return Err(CompilerError::TypeConflict(self.clone(), other.clone())); 
                }

                (
                    Type::YourType(_),
                    Type::Float
                    | Type::Int
                    | Type::Double
                    | Type::Long
                    | Type::Short 
                    | Type::Byte
                    | Type::Char
                    | Type::Int8
                    | Type::Int16
                    | Type::Int32
                    | Type::Int64
                    | Type::Int128
                    | Type::Bool
                    | Type::String
                ) => {
                    return Err(CompilerError::TypeConflict(self.clone(), other.clone()));
                }

                (Type::List(list_ty), Type::Array(_, ty)) => {
                    list_ty.assert_eq(ty, ctx).unwrap(); 
                    return Ok(());
                }
                (Type::Any, _) => return Ok(()),
                (Type::Array(_, t), Type::List(i)) => {
                    let e = t.assert_eq(i, ctx);
                    if e.is_err() {
                        return Err(CompilerError::TypeConflict(self.clone(), other.clone()));
                    }
                    return Ok(());

                }
                (Type::YourType(t1), Type::YourType(t2)) => {
                    let (t1, t2) = (t1.clone(), t2.clone());
                    if t2 != t1 {
                        return Err(CompilerError::UserTypeConflict(t1, t2));
                    }
                    return Ok(());
                }
                (Type::YourType(user_name), Type::EnumType(parent, _child)) => {
                    if user_name != parent {
                        return Err(CompilerError::TypeConflict(self.clone(), other.clone()));
                    }
                    return Ok(());
                }
                (Type::EnumType(parent, _), Type::YourType(_parent)) => {
                    if parent == _parent {
                        return Ok(());
                    } else {
                        return Err(CompilerError::TypeConflict(self.clone(), other.clone()));
                    }
                }
                (Type::EnumType(parent, _c), Type::EnumType(parent_, _cc)) => {
                    if parent == parent_ && _c == _cc {
                        return Ok(());
                    } else {
                        return Err(CompilerError::TypeConflict(self.clone(), other.clone()));
                    }
                }
                (Type::String, Type::EnumType(_, _)) => return Ok(()), 
                (Type::String, Type::YourType(_)) => return Ok(()),
                (
                    Type::Float
                    | Type::Int
                    | Type::Long
                    | Type::Short 
                    | Type::Byte
                    | Type::Double
                    | Type::Char
                    | Type::Int8
                    | Type::Int16
                    | Type::Int32
                    | Type::Int64
                    | Type::Int128
                    | Type::Unit
                    | Type::Bool,
                    Type::YourType(tname)
                ) => {
                    if ctx.aliases.contains_key(tname) {
                        let alias = ctx.aliases.get(tname).unwrap();
                        let ty = alias.value.clone(); 
                        // println!("{tname} -> {ty}"); 
                        return self.assert_eq(&ty, ctx);
                    } 
                    return Err(CompilerError::TypeConflict(self.clone(), other.clone())); 
                }
                (Type::String, Type::Any) => return Ok(()), 
                (Type::String, 
                    Type::Float
                    | Type::Int
                    | Type::Long
                    | Type::Short 
                    | Type::Byte
                    | Type::Double
                    | Type::Char
                    | Type::Int8
                    | Type::Int16
                    | Type::Int32
                    | Type::Int64
                    | Type::Int128
                    | Type::String
                    | Type::Bool
                ) => return Ok(()),
                (Type::String, _) => {
                    return Err(CompilerError::TypeConflict(self.clone(), other.clone())); 
                }
                _ => match other {
                    Type::Float
                    | Type::Int
                    | Type::Long
                    | Type::Short 
                    | Type::Byte
                    | Type::Double
                    | Type::Char
                    | Type::Int8
                    | Type::Int16
                    | Type::Int32
                    | Type::Int64
                    | Type::Int128
                    | Type::Any
                    | Type::String
                    | Type::Bool => {
                        return Ok(())
                    },
                    _ => {
                        return Err(CompilerError::TypeConflict(self.clone(), other.clone()));
                    }
                },
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
                Type::Lambda(_ret, args) => {
                    let args: Vec<_> = args.iter().map(|f| format!("{f}")).collect(); 
                    args.join(",")
                }
                Type::EnumType(name, p) => format!("{}.{}", name, p), 
                Type::YourType(s) => s.clone(),
                Type::VarArgs(ty) => {
                    match *ty.clone() {
                        Type::YourType(x) => {
                            if x.is_empty() {
                                "...".to_string()
                            } else {
                                format!("{x}... ")
                            }
                        }
                        other => format!("{other}... ")
                    }
                }
                Type::Unit => "()".to_string(),
                Type::Usize => "usize".to_string(),
                Type::String => "String".to_string(),
                Type::Any => "Any".to_string(),
                Type::Char => "Char".to_string(),
                Type::List(t) => format!("List[{}]", t),
                Type::Double => "Double".to_string(),
                Type::Int => "Int".to_string(),
                Type::Int8 => "Int8".to_string(),
                Type::Int16 => "Int16".to_string(),
                Type::Int32 => "Int32".to_string(),
                Type::Int64 => "Int64".to_string(),
                Type::Int128 => "Int128".to_string(),
                Type::Long => "Long".to_string(),
                Type::Short => "Short".to_string(),
                Type::Byte => "Byte".to_string(),
                Type::Bool => "Bool".to_string(),
                Type::Float => "Float".to_string(),
                Type::Array(len, ty) => format!("[{};{}]", ty, len),
                Type::Lifter(ty) => ty.to_owned(),
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
