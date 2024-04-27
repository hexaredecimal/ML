use crate::ir::{self, Type};

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
}
