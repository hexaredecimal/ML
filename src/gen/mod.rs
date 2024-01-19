use crate::error::{CompilerError, Result};
use crate::ir;
use crate::ir::sem::*;
use std::collections::HashMap; 

use inkwell::types::{*, self}; 
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::{OptimizationLevel, AddressSpace};

use std::error::Error;

use self::function::FunctionTranslator;



mod function;

pub struct Jit<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,
}

impl <'ctx> Jit <'ctx> {
    pub fn new() -> Self {

        let context = Context::create();
        let module = context.create_module("sum");
        let execution_engine = module.create_jit_execution_engine(OptimizationLevel::None).unwrap();
        Self {
            context: &context,
            module,
            builder: context.create_builder(),
            execution_engine,
        }
    }

    pub fn compile(&mut self, funcs: &[SemFunction]) -> Result<*const u8> {
        // translate AST to cranelift
        for fun in funcs {
            self.translate_function(fun)?;
            let name = fun.name();
        }

        todo!()
    }

    fn translate_function(&mut self, func: &SemFunction) -> Result<()> {
        // setup signature

        let args: Vec<BasicMetadataTypeEnum> = func.args()
            .into_iter()
            .map(|f| { 
                let (_, ty) = f.clone(); 
                self.real_type(&ty).unwrap().into()
            }).collect(); 

        let ret_type = if let ir::Type::Function(ret, _args) = func.ty() {
            self.real_type(ret)?
        } else {
            return Err(CompilerError::BackendError(format!(
                "{:?} is not a function type",
                func.ty()
            )));
        };

        let fn_type = ret_type.fn_type(args.as_slice(), false);
        let function = self.module.add_function("sum", fn_type, None);
        // add special vmcontext param to potentially use global values

        let basic_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(basic_block);


        // declare function block level variables
        let mut variables: HashMap<String, _> = HashMap::new();
        for (i, (name, _ty)) in func.args().iter().enumerate() {
            let v = function.get_nth_param(i as u32).unwrap();
            variables.insert(name.clone(), v); 
        }

        let mut trans = FunctionTranslator::new(&mut self, variables);

        let return_value = trans.translate_expr(func.root())?;

        trans.builder().ins().return_(&[return_value]);
        trans.builder().finalize();
        println!("{}", trans.builder().display(None));
        Ok(())
    }


    pub fn real_type(self, ty: &ir::Type)
     -> Result<BasicTypeEnum<'ctx>> {
            match ty {
            ir::Type::Int => Ok(self.context.i64_type().into()),
            ir::Type::Bool => Ok(self.context.bool_type().into()),
            ir::Type::Float => Ok(self.context.f32_type().into()), 
            ir::Type::Double => Ok(self.context.f64_type().into()),
            ir::Type::Int8 => Ok(self.context.i8_type().into()),
            ir::Type::Int16 => Ok(self.context.i16_type().into()),
            ir::Type::Int32 => Ok(self.context.i32_type().into()),
            ir::Type::Int64 => Ok(self.context.i64_type().into()),
            ir::Type::Int128 => Ok(self.context.i128_type().into()),
            ir::Type::Array(_num, _inner) => {
                let inner = *_inner.clone(); 
                let t = self.real_type(&inner)?;
                Ok(t.array_type(*_num as u32).into())
            },
            ir::Type::List(_inner) => {
                let inner = *_inner.clone(); 
                let t = self.real_type(&inner)?;
                Ok(t.ptr_type(AddressSpace::default()).into())
            },
            _ => Err(CompilerError::BackendError(format!(
                "Unsupported type {:?}",
                ty
            ))),
        }
    }
}

