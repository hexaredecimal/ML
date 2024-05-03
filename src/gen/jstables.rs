
use crate::error::{CompilerError, Result};
use crate::gen::jscodegen::JSBackend;
use crate::ir::raw::{RecordType, EnumType, EnumField, Alias};
use crate::ir::sem::*;
use crate::ir::{self, Type};
use std::collections::HashMap;


#[derive(Clone)]
#[allow(unused_tuple_struct_fields)]
pub struct JSTables {
    pub records: HashMap<String, RecordType>,
    pub enums: HashMap<String, EnumType>,
    pub aliases: HashMap<String, Alias>,
}

impl JSTables {
    pub fn new() -> Self {
        Self {
            records: HashMap::new(),
            enums: HashMap::new(),
            aliases: HashMap::new()
        }
    }


    pub fn to_js_type(&self, ty: Type) -> String {
        match ty {
            Type::Any => "Object".to_string(), 
            Type::String | Type::Char => "String".to_string(), 
            Type::Int | Type::Float | Type::Double => "Number".to_string(),
            Type::Unit => "<Unit>".to_string(), 
            Type::EnumType(n, r) => format!("{n}.{r}"), 
            Type::YourType(x) => format!("{x}"), 
            Type::Lambda(x, y) => format!("{x}:-{}", y.len()), 
            _ => unreachable!()
         }
    } 

    pub fn classes(&self) -> String {
        format!(
            r#"
class Void {{}}
Void.Unit = 0; 
            "#
        )
    }


    pub fn process_enumns(&mut self, records: Vec<EnumType>, ctx: &mut SemContext) -> Result<String> {
        let mut recs: String = String::new();

        for record in records.clone() {
            let name = record.name.clone();
            let args = record.fields.clone();
            let args: Vec<String> = args
                .into_iter()
                .map(|f| {
                    match f {
                        EnumField::Rec(rec) => {
                            let recc = self.process_records(vec![rec.clone()], false, ctx, &name).unwrap();
                            let recc = recc.trim_end(); 
                            format!("{name}.{} = {recc}", rec.name)
                        }, 
                        EnumField::Id(_name) => {
                            let inspect = format!("inspect(depth, ops) {{ return this.toString() }}"); 
                            let to_string = format!("toString() {{ return '{name}'.concat('{_name}')) }}");
                            let methods = format!("\n{inspect}\n{to_string}\n");
                            format!("    {name}.{_name} = class {_name} extends {name} {{ {methods} }}")
                        }
                    }
                })
                .collect();

            let args = args.join("\n");
            let en = format!(r#"
class {name} {{}}
{args}
            "#);
            recs.push_str(&en);
        }

        Ok(recs)
    }


    pub fn process_records(&mut self, records: Vec<RecordType>, close: bool, ctx: &mut SemContext, superz: &str) -> Result<String> {
        let mut recs: String = String::new();

        for record in records.clone() {
            let name = record.name.clone();
            let args = record.fields.clone();
            let fields: Vec<String> = args.clone()
                .into_iter()
                .map(|f| {
                    let (name, ty) = f;
                    // let ty = Utils::real_type(&ty, ctx).unwrap();
                    format!("    #{}",name)
                })
                .collect();

            let raw_fields: Vec<String> = args.clone()
                .into_iter()
                .map(|f| {
                    let (name, _) = f; 
                    format!("' {name}:'.concat(this.#{name})")
                }).collect(); 

            let init: Vec<String> = args.clone()
                .into_iter()
                .map(|f| {
                    let (name, _) = f; 
                    format!("        this.#{name} = {name}")
                }).collect();


            let getter: Vec<String> = args.clone()
                .into_iter()
                .map(|f| {
                    let (name, _) = f; 
                    format!("    {name}() {{ return this.#{name}; }}")
                }).collect();

            let params: Vec<String> = args
                .into_iter()
                .map(|f| {
                    let (name, ty) = f;
                    // let ty = Utils::real_type(&ty, ctx).unwrap();
                    format!("{}",name)
                }).collect();

            let fields = fields.join("\n");
            let init = init.join("\n");
            let params = params.join(", ");
            let getters = getter.join("\n"); 
            let raw_fields = raw_fields.join("+");


            let raw_fields = raw_fields.trim();
            let inspect = format!("    inspect(depth, ops) {{ return this.toString() }}"); 
            let to_string = format!("    toString() {{ return '{name}('.concat({raw_fields}).concat(')') }}");
            let methods = format!("{inspect}\n{to_string}\n");
            let class = format!(
            r#"
class {name} extends {superz} {{
{fields}
    constructor({params}) {{
        super()
{init}
    }}
{getters}
{methods}
}}
            "#
            );

            recs.push_str(&format!("{class}"));
        }

        Ok(recs)
    }

    pub fn compile(&mut self, funcs: &[SemFunction], ctx: &mut SemContext) -> Result<String> {
        let mut s = String::new();
        for fun in funcs {
            let a = self.translate_function(fun, ctx)?;
            s.push_str(a.as_str());
            s.push('\n');
        }
        Ok(s.clone())
    }

    fn translate_function(&mut self, func: &SemFunction, ctx: &mut SemContext) -> Result<String> {
        let name: String = func.name().to_string();

        let len = func.args().len();

        let args: Vec<Result<String>> = func
            .args()
            .iter()
            .enumerate()
            .map(|(i, f)| {
                let (name, ty) = f;
                match ty {
                    ir::Type::VarArgs(inner) => {
                        if i < len - 1 {
                            let e: Result<CompilerError> =
                                Result::Err(CompilerError::BackendError(
                                    "VarArgs cannot be used in the middle of paramaters"
                                        .to_string(),
                                ));
                            println!("{:?}", e);
                            std::process::exit(1);
                        }
                        Ok(format!("...var_args"))
                    }
                    _ => {
                        Ok(format!("{name}"))
                    }
                }
            })
            .collect();

        let args: Vec<String> = args.into_iter().map(|f| f.unwrap()).collect();
        let args = args.join(", ");

        let ret_type = if let ir::Type::Function(ret, _args) = func.ty() {
            // self.clone().real_type(ret, ctx)?
        } else {
            return Err(CompilerError::BackendError(format!(
                "{:?} is not a function type",
                func.ty()
            )));
        };

        // declare function block level variables
        let mut variables: HashMap<String, Type> = HashMap::new();
        for (name, _ty) in func.args().iter() {
            let name = if name.is_empty() {
                "var_args".to_string()
            } else {
                name.clone()
            };
            variables.insert(name, _ty.clone());
        }


        let mut trans = JSBackend::new(self.clone());

        let root = func.root().clone();
        let return_value = trans.translate_expr(func.root(), &mut variables, ctx)?;

        let final_expr = match root.expr() {
            SemExpression::Block(_) | SemExpression::Lets(_,_) => {
                    let sq = return_value.clone();
                    let sq: Vec<_> = sq.split('\n').collect();
                    let len = sq.len();

                    let mut s = String::new();
                    let rest = sq.get(0..len - 1).unwrap();
                    for st in rest {
                        s.push_str(st);
                        s.push('\n');
                    }

                    let last = sq.last().unwrap();
                    s.push_str("return ");
                    s.push_str(last);
                    s.push(';');
                    s
            },

            _ => format!("return {return_value}")
        }; 

        let fx = format!("const {name} = ({args}) => {{\n{final_expr}\n}}");
        Ok(fx)
    }
}
