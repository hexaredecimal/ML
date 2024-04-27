use crate::ir::{self, Type};
use crate::error::{CompilerError, Result};
use crate::ir::raw::{RecordType, EnumType, EnumField, Alias};
use std::collections::HashMap;

pub struct Utils; 

impl Utils {
    pub fn record_contains_field(name: &String, rec: &[(String, Type)]) -> (bool, usize) {
        for (i, (n, _)) in rec.iter().enumerate() {
            if *n == *name {
                return (true, i)
            }
        }
        (false, 1_usize)
    }


    pub fn is_sys_type(ty: &ir::Type) -> bool {
        match ty {
            ir::Type::Any => true,
            ir::Type::Unit => true,
            ir::Type::String => true,
            ir::Type::Usize => true,
            ir::Type::Int => true,
            ir::Type::Char => true,
            ir::Type::Bool => true,
            ir::Type::Float => true,
            ir::Type::Double => true,
            /*
            ir::Type::Int8 => true,
            ir::Type::Int16 => true,
            ir::Type::Int32 => true,
            ir::Type::Int64 => true,
            ir::Type::Int128 => true, */
            ir::Type::Array(_num, _inner) => true,
            ir::Type::List(_inner) => true,
            _ => false,
        }
    }

    pub fn extract_record_type(name: &String, ty: &[EnumField]) -> Option<EnumField> {
        for t in ty {
            match t.clone() {
                EnumField::Rec(rec) => {
                    if rec.name == *name {
                        return Some(t.clone());
                    }
                }

                EnumField::Id(s) => {
                    if *name == s {
                        return Some(t.clone()); 
                    }
                }
            }
        }
        None
    }

    pub fn enum_type_exists(enums: &HashMap<String, EnumType>, ty: &String) -> Result<bool> {
        if !enums.contains_key(ty) {
            Err(CompilerError::BackendError(format!(
                "Invalid enum type {}",
                ty
            )))
        } else {
            Ok(true)
        }
    }

    pub fn record_type_exists(records: &HashMap<String, RecordType>, ty: &String) -> Result<bool> {
        if !records.contains_key(ty) {
            Err(CompilerError::BackendError(format!(
                "Invalid struct type {}",
                ty
            )))
        } else {
            Ok(true)
        }
    }
}
