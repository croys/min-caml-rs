#![allow(dead_code, unused_variables)]

use crate::ty::Type;

// 変数の名前 - variable name
#[derive(Clone, PartialEq, Debug, Hash, Eq)]
pub struct T(String);

// トップレベル関数やグローバル配列のラベル
//   - labels for top level function and global arrays
#[derive(Clone, PartialEq, Debug, Hash, Eq)]
pub struct L(String);

static mut COUNTER: u32 = 0;

pub fn genid(s: &str) -> T {
    unsafe {
        COUNTER += 1;
        T(format!("{}.{}", s, COUNTER))
    }
}

pub fn id_of_type(ty: &Type) -> &str {
    match ty {
        Type::Unit => "u",
        Type::Bool => "b",
        Type::Int => "i",
        Type::Float => "d",
        Type::Fun(_, _) => "f",
        Type::Tuple(_) => "t",
        Type::Array(_) => "a",
        Type::Var(_) => unreachable!(),
    }
}

pub fn gentmp(typ: &Type) -> T {
    unsafe {
        COUNTER += 1;
        T(format!("T{}{}", id_of_type(typ), COUNTER))
    }
}
