use crate::ir::{self, Type};
use crate::ir::sem::*;
use crate::error::CompilerError; 
use crate::Jit; 

use std::collections::hash_map::HashMap; 
use inkwell::types::{BasicTypeEnum, self, BasicType};
use inkwell::values::{AnyValue, BasicValueEnum, BasicValue, FloatValue}; 
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::{OptimizationLevel, IntPredicate, FloatPredicate};

// simple bump allocator
// TODO: more sophisticated memory management
struct Allocator {
    cur: usize,
}

impl Allocator {
    pub fn new() -> Self {
        Self { cur: 0 }
    }
    pub fn alloc(&mut self, size: usize) -> usize {
        let offset = self.cur;
        self.cur += size;
        offset
    }
}

pub(crate) struct FunctionTranslator<'ctx> {
    jit: &'ctx mut Jit<'ctx>, 
    variables: HashMap<String, BasicValueEnum<'ctx>>,
}

impl<'ctx> FunctionTranslator<'ctx> {
    pub fn new(
        jit: &mut Jit, 
        variables: HashMap<String, BasicValueEnum>,
    ) -> Self {
        let allocator = Allocator::new();
        Self {
            jit, 
            variables,
        }
    }

    pub fn translate_expr(&mut self, node: &SemNode) -> Result<BasicValueEnum, CompilerError> {
        let real_ty = self.jit.real_type(node.ty());

        match node.expr() {
            SemExpression::Null(t) => {
                //let t = real_type(t, self.module).unwrap();
                todo!()
            }
            SemExpression::Cast(e, t) => {
                todo!()
            }
            SemExpression::Abs(x) => {
                todo!()
            }
            SemExpression::NullCheck(a) => {
                let a = a.clone(); 
                let e = self.translate_expr(&a)?; 
                Ok(e.is_null())
            }
            SemExpression::Block(values) => {
                let len = values.len(); 
                if len == 0 {
                    println!("Invalid block with 0 expressions"); 
                    panic!(); 
                }

                let rest = values.get(0 .. len - 1).unwrap();
                
                for val in rest.into_iter() {
                    self.translate_expr(val)?; 
                }

                let last = values.last().unwrap(); 
                self.translate_expr(last)
            }
            SemExpression::Match(cond, cases) => unreachable!(), // Match is converted to a Conditional 
            SemExpression::FunCall(func_name, args) => {
                self.translate_funcall(func_name, args, real_ty)
            }
            SemExpression::Conditional(cond, then, alt) => {
                self.translate_conditional(cond, then, alt, real_ty)
            }
            SemExpression::BinaryOp(op, left, right) => {
                let left_val = self.translate_expr(left)?;
                let right_val = self.translate_expr(right)?;


                match (op, left.ty(), right.ty()) {
                    (ir::BinaryOp::Equal, 
                        ir::Type::Int | ir::Type::Char | ir::Type::Int8 | ir::Type::Int16 | ir::Type::Int32 | ir::Type::Int64 | ir::Type::Int128, 
                        ir::Type::Int | ir::Type::Char | ir::Type::Int8 | ir::Type::Int16 | ir::Type::Int32 | ir::Type::Int64 | ir::Type::Int128, 
                    ) => {
                        Ok(self.jit.builder.build_int_compare(IntPredicate::EQ, left_val, right_val, "cmp? with a name"))
                    }
                    (ir::BinaryOp::Equal, 
                        ir::Type::Bool, 
                        ir::Type::Bool
                    ) => {
                        Ok(self.jit.builder.build_int_compare(IntPredicate::EQ, left_val, right_val, "cmp? with a name"))
                    }
                    (ir::BinaryOp::Equal,
                        ir::Type::Float | ir::Type::Double,
                        ir::Type::Float | ir::Type::Double,
                    ) => {
                        Ok(self.jit.builder.build_float_compare(FloatPredicate::EQ, left_val, right_val, "cmp? with a name"))
                    }
                    (ir::BinaryOp::NotEqual, 
                        ir::Type::Int | ir::Type::Char | ir::Type::Int8 | ir::Type::Int16 | ir::Type::Int32 | ir::Type::Int64 | ir::Type::Int128, 
                        ir::Type::Int | ir::Type::Char | ir::Type::Int8 | ir::Type::Int16 | ir::Type::Int32 | ir::Type::Int64 | ir::Type::Int128, 
                    ) => {
                        Ok(self.jit.builder.build_int_compare(IntPredicate::NEQ, left_val, right_val, "cmp? with a name"))
                    }
                    (ir::BinaryOp::NotEqual,
                        ir::Type::Bool, 
                        ir::Type::Bool
                    ) => {
                        Ok(self.jit.builder.build_int_compare(IntPredicate::NEQ, left_val, right_val, "cmp? with a name"))
                    }
                    (ir::BinaryOp::NotEqual, 
                        ir::Type::Float | ir::Type::Double,
                        ir::Type::Float | ir::Type::Double,
                    ) => {
                        Ok(self.jit.builder.build_float_compare(FloatPredicate::NEQ, left_val, right_val, "cmp? with a name"))
                    }
                    (ir::BinaryOp::Multiply,
                        ir::Type::Int | ir::Type::Char | ir::Type::Int8 | ir::Type::Int16 | ir::Type::Int32 | ir::Type::Int64 | ir::Type::Int128, 
                        ir::Type::Int | ir::Type::Char | ir::Type::Int8 | ir::Type::Int16 | ir::Type::Int32 | ir::Type::Int64 | ir::Type::Int128, 
                    ) => {
                        Ok(self.jit.builder.build_int_mul(left_val, right_val, "Multiply?"))
                    }
                    (ir::BinaryOp::Multiply, 
                        ir::Type::Float | ir::Type::Double,
                        ir::Type::Float | ir::Type::Double,
                    ) => {
                        Ok(self.jit.builder.build_float_mul(left_val, right_val, "Multiply?"))
                    }
                    (ir::BinaryOp::Divide, 
                        ir::Type::Int | ir::Type::Char | ir::Type::Int8 | ir::Type::Int16 | ir::Type::Int32 | ir::Type::Int64 | ir::Type::Int128, 
                        ir::Type::Int | ir::Type::Char | ir::Type::Int8 | ir::Type::Int16 | ir::Type::Int32 | ir::Type::Int64 | ir::Type::Int128, 
                    ) => {
                        Ok(self.jit.builder.build_int_signed_div(left_val, right_val, "Multiply?"))
                    }
                    (ir::BinaryOp::Divide, 
                        ir::Type::Float | ir::Type::Double,
                        ir::Type::Float | ir::Type::Double,
                    ) => {
                        Ok(self.jit.builder.build_float_div(left_val, right_val, "Multiply?"))
                    }
                    (ir::BinaryOp::Add, 
                        ir::Type::Int | ir::Type::Char | ir::Type::Int8 | ir::Type::Int16 | ir::Type::Int32 | ir::Type::Int64 | ir::Type::Int128, 
                        ir::Type::Int | ir::Type::Char | ir::Type::Int8 | ir::Type::Int16 | ir::Type::Int32 | ir::Type::Int64 | ir::Type::Int128, 
                    ) => {
                        Ok(self.jit.builder.build_int_add(left_val, right_val, "Multiply?"))
                    }
                    (ir::BinaryOp::Add, 
                        ir::Type::Float | ir::Type::Double,
                        ir::Type::Float | ir::Type::Double,
                    ) => {
                        Ok(self.jit.builder.build_float_add(left_val, right_val, "Multiply?"))
                    }
                    (ir::BinaryOp::Sub, 
                        ir::Type::Int | ir::Type::Char | ir::Type::Int8 | ir::Type::Int16 | ir::Type::Int32 | ir::Type::Int64 | ir::Type::Int128, 
                        ir::Type::Int | ir::Type::Char | ir::Type::Int8 | ir::Type::Int16 | ir::Type::Int32 | ir::Type::Int64 | ir::Type::Int128, 
                    ) => {
                        Ok(self.jit.builder.build_int_sub(left_val, right_val, "Multiply?"))
                    }
                    (ir::BinaryOp::Sub, 
                        ir::Type::Float | ir::Type::Double,
                        ir::Type::Float | ir::Type::Double,
                    ) => {
                        Ok(self.jit.builder.build_float_sub(left_val, right_val, "Multiply?"))
                    }
                    (ir::BinaryOp::LessThan, 
                        ir::Type::Int | ir::Type::Char | ir::Type::Int8 | ir::Type::Int16 | ir::Type::Int32 | ir::Type::Int64 | ir::Type::Int128, 
                        ir::Type::Int | ir::Type::Char | ir::Type::Int8 | ir::Type::Int16 | ir::Type::Int32 | ir::Type::Int64 | ir::Type::Int128, 
                    ) => {
                        Ok(self.jit.builder.build_int_compare(IntPredicate::SLT, left_val, right_val, "cmp? with a name"))
                    }
                    (ir::BinaryOp::LessThan,
                        ir::Type::Float | ir::Type::Double,
                        ir::Type::Float | ir::Type::Double,
                    ) => {
                        Ok(self.jit.builder.build_float_compare(FloatPredicate::SLT, left_val, right_val, "cmp? with a name"))
                    }
                    (ir::BinaryOp::LessThanOrEqual, 
                        ir::Type::Int | ir::Type::Char | ir::Type::Int8 | ir::Type::Int16 | ir::Type::Int32 | ir::Type::Int64 | ir::Type::Int128, 
                        ir::Type::Int | ir::Type::Char | ir::Type::Int8 | ir::Type::Int16 | ir::Type::Int32 | ir::Type::Int64 | ir::Type::Int128, 
                    ) => {
                        Ok(self.jit.builder.build_int_compare(IntPredicate::SLE, left_val, right_val, "cmp? with a name"))
                    }
                    (ir::BinaryOp::LessThanOrEqual, 
                        ir::Type::Float | ir::Type::Double,
                        ir::Type::Float | ir::Type::Double,
                    ) => {
                        Ok(self.jit.builder.build_float_compare(FloatPredicate::SLE, left_val, right_val, "cmp? with a name"))
                    }
                    (ir::BinaryOp::GreaterThan, 
                        ir::Type::Int | ir::Type::Char | ir::Type::Int8 | ir::Type::Int16 | ir::Type::Int32 | ir::Type::Int64 | ir::Type::Int128, 
                        ir::Type::Int | ir::Type::Char | ir::Type::Int8 | ir::Type::Int16 | ir::Type::Int32 | ir::Type::Int64 | ir::Type::Int128, 
                    ) => {
                        Ok(self.jit.builder.build_int_compare(IntPredicate::SGT, left_val, right_val, "cmp? with a name"))
                    }
                    (ir::BinaryOp::GreaterThan, 
                        ir::Type::Float | ir::Type::Double,
                        ir::Type::Float | ir::Type::Double,
                    ) => {
                        Ok(self.jit.builder.build_float_compare(FloatPredicate::SGT, left_val, right_val, "cmp? with a name"))
                    }
                    (ir::BinaryOp::GreaterThanOrEqual, 
                        ir::Type::Int | ir::Type::Char | ir::Type::Int8 | ir::Type::Int16 | ir::Type::Int32 | ir::Type::Int64 | ir::Type::Int128, 
                        ir::Type::Int | ir::Type::Char | ir::Type::Int8 | ir::Type::Int16 | ir::Type::Int32 | ir::Type::Int64 | ir::Type::Int128, 
                    ) => {
                        Ok(self.jit.builder.build_int_compare(IntPredicate::SGE, left_val, right_val, "cmp? with a name"))
                    }
                    (ir::BinaryOp::GreaterThanOrEqual, 
                        ir::Type::Float | ir::Type::Double,
                        ir::Type::Float | ir::Type::Double,
                    ) => {
                        Ok(self.jit.builder.build_float_compare(FloatPredicate::SGE, left_val, right_val, "cmp? with a name"))
                    }
                    (ir::BinaryOp::And, 
                        ir::Type::Bool, 
                        ir::Type::Bool
                    ) => {
                        Ok(self.jit.builder.build_and(left_val, right_val, "bool and?"))
                    }
                    (ir::BinaryOp::Or,
                        ir::Type::Bool, 
                        ir::Type::Bool
                    ) => {
                        Ok(self.jit.builder.build_or(
                                left_val.into_int_value(), right_val.into_int_value(),"Bool or?").unwrap())
                    }

                    (ir::BinaryOp::ArrayDeref, 
                        ir::Type::List(_),
                        ir::Type::Int | ir::Type::Char | ir::Type::Int8 | ir::Type::Int16 | ir::Type::Int32 | ir::Type::Int64 | ir::Type::Int128, 
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

                        todo!()
                        /*
                        let elem_ty = *elem.clone();
                        let elem_ty = &elem_ty; 
                        //let count = elem.0; 
                        
                        
                        let real_elem_ty = self.jit.real_type(elem_ty)?; 
                        let gv_val = left_val;

                        let ref_address = self.jit.builder.build_int_add(left_val, BasicValueEnum::IntValue(real_elem_ty.bytes() as i64), "indexing array");
                        let offset_pointer_value = self.jit.builder.build_int_add(gv_val, ref_address, "array offset");

                        let elem = self.jit.builder.build_load(offset_pointer_value.into(), "Load"); 

                        Ok(elem) */
                    }

                    (ir::BinaryOp::ArrayDeref, 
                        ir::Type::Array(_, _),
                        ir::Type::Int
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

                        unreachable!()
                        /*
                        let elem_ty = elem.1; 
                        //let count = elem.0; 
                        
                        let real_elem_ty = self.jit.real_type(elem_ty)?;

                        let gv_val = left_val;

                        let ref_address = self.jit.builder.build_int_add(left_val, BasicValueEnum::IntValue(real_elem_ty.bytes() as i64), "indexing array");
                        let offset_pointer_value = self.jit.builder.build_int_add(gv_val, ref_address, "array offset");

                        let elem = self.jit.builder.build_load(offset_pointer_value.into(), "Load"); 
                        Ok(elem) */
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

                todo!()
                /*
                let real_elem_ty = self.jit.real_type(elem_ty)?;

                let l = real_elem_ty.as_basic_type_enum().size_of();
                let bytess = self.jit.context.i64_type().const_int(*elem_count as u64, false); 
                let bytes = self.jit.builder.build_int_mul(l, bytess, "resolving offset").unwrap(); 
                let arr = self.jit.builder.build_array_alloca(real_elem_ty, bytes.unwrap(), "alloc arr");

                for (i, elem) in elems.iter().enumerate() {
                    let offset = 64 as i32 * i as i32;
                    let elem_val = self.translate_expr(elem)?;

                    let addr = self.jit.builder.build_int_add(arr, BasicValueEnum::IntValue(offset), "Adding "); 
                    self.jit.builder.build_store(addr, elem_val); 
                }

                Ok(arr) */
            }
            SemExpression::UnaryOp(op, n) => {
                let operand = self.translate_expr(n)?;
                match (op, n.ty()) {
                    (ir::UnaryOp::Minus, ir::Type::Int) => Ok(
                        self.jit.builder.build_int_neg(operand.into(), "Neg Int").unwrap().as_basic_value_enum()),
                    (ir::UnaryOp::Minus, ir::Type::Float) => Ok(self.jit.builder.build_float_neg(operand.into(), "Neg Float").unwrap().as_basic_value_enum()),
                    (ir::UnaryOp::Not, ir::Type::Bool) => Ok(self.jit.builder.build_not(operand.into(), "Not").unwrap().as_basic_value_enum()),
                    _ => Err(CompilerError::BackendError(format!(
                        "Invalid unary operation {:?} over type {:?}",
                        op,
                        n.ty()
                    ))),
                }
            }
            SemExpression::Let(id, init) => self.translate_val(id, init), 
            SemExpression::Lets(ids, expr) => {
                for lt in ids {
                    let SemExpression::Let(name, exprl) = lt.clone() else {
                        panic!("expected let expression")
                    }; 
                    self.translate_val(name.as_str(), &exprl)?; 
                }
                self.translate_expr(expr)
            }
            SemExpression::Val(id, init)=> self.translate_val(id, init),
            SemExpression::Id(id) => {

                let variable = self.variables.get(id).ok_or_else(|| {
                    CompilerError::BackendError(format!("No variable {} available", id))
                })?;
                
                Ok(variable.clone())

            }
            SemExpression::Char(val) => {
                Ok(self.jit.context.i8_type().const_int(*val as u64, false).as_basic_value_enum())
            },
            SemExpression::Bool(val) => {
                Ok(self.jit.context.bool_type().const_int(*val as u64, false).as_basic_value_enum())
            }
            SemExpression::Integer(val, t) => {
                let t = *t.clone(); 
                let real_ty = self.jit.real_type(&t).unwrap();

                let ty = match t.clone() {
                    Type::Float => Ok(self.jit.context.f32_type().const_float(*val as f64).as_basic_value_enum()), 
                    Type::Double => Ok(self.jit.context.f64_type().const_float(*val as f64).as_basic_value_enum()),  
                    Type::Int => Ok(self.jit.context.i64_type().const_int(*val as u64, false).as_basic_value_enum()), 
                    Type::Bool => Ok(self.jit.context.bool_type().const_int(*val as u64, false).as_basic_value_enum()),
                    Type::Char => Ok(self.jit.context.i8_type().const_int(*val as u64, false).as_basic_value_enum()),
                    Type::Int8 => Ok(self.jit.context.i8_type().const_int(*val as u64, false).as_basic_value_enum()),
                    Type::Int16 => Ok(self.jit.context.i16_type().const_int(*val as u64, false).as_basic_value_enum()),
                    Type::Int32 => Ok(self.jit.context.i32_type().const_int(*val as u64, false).as_basic_value_enum()),
                    Type::Int64 => Ok(self.jit.context.i64_type().const_int(*val as u64, false).as_basic_value_enum()),
                    Type::Int128 => Ok(self.jit.context.i128_type().const_int(*val as u64, false).as_basic_value_enum()),
                    _ => unreachable!()
                }; 
                ty
            },

            SemExpression::Decimal(val, t) => {
                let ty = match *t.clone() {
                    Type::Float => Ok(self.jit.context.f32_type().const_float(*val as f64).as_basic_value_enum()), 
                    Type::Double => Ok(self.jit.context.f64_type().const_float(*val as f64).as_basic_value_enum()),  
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
    ) -> Result<BasicValueEnum, CompilerError> {
        /*
        let cond_val = self.translate_expr(cond)?;

        let then_block = self.builder.create_block();
        let else_block = self.builder.create_block();
        let merge_block = self.builder.create_block();

        self.builder.append_block_param(merge_block, out_ty);

        self.builder.ins().brz(cond_val, else_block, &[]);
        self.builder.ins().jump(then_block, &[]);

        self.builder.switch_to_block(then_block);
        self.builder.seal_block(then_block);
        let then_val = self.translate_expr(then)?;

        self.builder.ins().jump(merge_block, &[then_val]);

        self.builder.switch_to_block(else_block);
        self.builder.seal_block(else_block);
        let else_val = self.translate_expr(alt)?;

        self.builder.ins().jump(merge_block, &[else_val]);

        self.builder.switch_to_block(merge_block);

        self.builder.seal_block(merge_block);
        let phi = self.builder.block_params(merge_block)[0];

        Ok(phi)*/
        todo!()
    }

    fn translate_val(
        &mut self, 
        id: &str, 
        init: &Box<SemNode>
    ) -> Result<BasicValueEnum, CompilerError> {
        let val = self.translate_expr(init)?;
        self.variables.insert(id.into(), val);
        Ok(val)
    }


    fn translate_let(
        &mut self,
        id: &str,
        init: &Box<SemNode>,
        expr: &Box<SemNode>,
    ) -> Result<BasicValueEnum, CompilerError> {
        let val = self.translate_expr(init)?;


        let shadowed_var = self.variables.remove(id.into());
        self.variables.insert(id.into(), val);

        let res = self.translate_expr(expr)?;

        self.variables.remove(id.into());
        if let Some(sv) = shadowed_var {
            self.variables.insert(id.into(), sv);
        }

        Ok(res)
    }

    fn translate_funcall(
        &mut self,
        func_name: &str,
        args: &[SemNode],
        return_ty: BasicTypeEnum,
    ) -> Result<BasicValueEnum, CompilerError> {
        let mut args_values = args
            .iter()
            .map(|arg| self.translate_expr(arg).unwrap())
            .collect::<Vec<_>>();

        let funcs = self.jit.module.get_functions(); 
        todo!()
    }
}
