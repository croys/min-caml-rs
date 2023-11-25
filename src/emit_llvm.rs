//#![allow(clippy::upper_case_acronyms)]
#![allow(dead_code)]
#![allow(unused_variables)] // FIXME
#![allow(unused_imports)] // FIXME

use llvm_sys::core::LLVMVoidTypeInContext;

use crate::asm;
use crate::closure;
use crate::id;
use crate::llvm;
use crate::llvm::RefOwner;
use crate::r#virtual;
use crate::ty;

//extern crate llvm_sys;

//use libc::c_char;
use std::collections::HashMap;
//use std::ffi::CString;

pub fn ty_to_type_in_context(ctx: &llvm::Context, ty: &ty::Type) -> llvm::Type {
    use llvm_sys::core::LLVMDoubleTypeInContext;
    use llvm_sys::core::LLVMFunctionType;
    use llvm_sys::core::LLVMInt32TypeInContext;
    use llvm_sys::core::LLVMVoidTypeInContext;
    use llvm_sys::prelude::LLVMTypeRef;

    fn to_type(ctx: &llvm::Context, ty: &ty::Type) -> LLVMTypeRef {
        match ty {
            ty::Type::Unit => unsafe { LLVMVoidTypeInContext(ctx.to_ref()) },
            ty::Type::Bool => unsafe { LLVMInt32TypeInContext(ctx.to_ref()) },
            ty::Type::Int => unsafe { LLVMInt32TypeInContext(ctx.to_ref()) },
            ty::Type::Float => unsafe { LLVMDoubleTypeInContext(ctx.to_ref()) },
            ty::Type::Fun(ref arg_tys, ref ret_ty) => {
                let mut arg_tys2: Vec<LLVMTypeRef> =
                    arg_tys.iter().map(|ty| to_type(ctx, ty)).collect();
                let n: libc::c_uint = arg_tys2.len().try_into().unwrap();
                unsafe {
                    LLVMFunctionType(
                        to_type(ctx, ret_ty),
                        arg_tys2.as_mut_ptr(),
                        n,
                        0,
                    )
                }
            }
            ty::Type::Tuple(ref tys) => {
                todo!()
            }
            ty::Type::Array(ref elem_ty) => {
                todo!()
            }
            ty::Type::Var(_) => unreachable!(),
        }
    }

    let ty_ = to_type(ctx, ty);
    llvm::Type::new(ty_, false)
}

// FN to split asm::T into basic blocks of Vec<Inst>
//
// FIXME: looking more and more like we should just translate from closure
// to own LLVM-focussed intermediate rep
//

#[derive(Debug, PartialEq, Clone)]
pub enum Inst {
    Exp(asm::Exp),
    Let((id::T, ty::Type), asm::Exp),
}

// FIXME: rename all llvm wrapper classes as LLVMX and/or move to separate
// module

#[derive(Debug, PartialEq, Clone)]
pub enum BBlock {
    BBlock(id::L, Vec<Inst>),
}

fn ends_block(e: &asm::Exp) -> bool {
    use asm::Exp::*;
    matches!(
        e,
        IfEq(_, _, _, _)
            | IfLE(_, _, _, _)
            | IfGE(_, _, _, _)
            | IfLt(_, _, _, _)
            | IfGt(_, _, _, _)
            | IfFEq(_, _, _, _)
            | IfFLE(_, _, _, _)
            | IfFGE(_, _, _, _)
            | IfFLt(_, _, _, _)
            | IfFGt(_, _, _, _)
            | CallCls(_, _, _)
            | CallDir(_, _, _)
    )
}

fn to_basic_blocks(p: &asm::T, b: &mut Vec<Inst>) -> Vec<BBlock> {
    match p {
        asm::T::Ans(ref e) => b.push(Inst::Exp(e.clone())),
        asm::T::Let((ref id, ref ty), ref e, ref t) => {
            // what do we do if the let exp ends a basic block?
            // do the assignment in all branches?
            todo!()
        }
    }
    todo!()
}

// Just generate list of instructions initially, then create basic blocks
// after this?

// FIXME: need our own Inst to map all Exp to Vec<Inst> and replicate
// tree structure before creating basic blocks

// Will try just going from asm::T to ins directly initially....

// FIXME: map from ids to values
// FIXME: value name or use empty name?
fn exp_to_llvm(
    ctx: &llvm::Context,
    fun: &llvm::Value,
    bblock: &llvm::BasicBlock,
    builder: &llvm::Builder,
    e: &asm::Exp,
) -> llvm::Value {
    fn to_val(c: &mut llvm::Constant) -> llvm::Value {
        let x = llvm::Value::from_ref(c.to_ref(), c.is_owned());
        c.release();
        x
    }

    use asm::Exp::*;
    match e {
        // FIXME: call to llvm.donothing()?
        Nop => to_val(&mut llvm::Constant::int(
            &llvm::Type::int32_type_in_context(ctx),
            0,
        )),
        Set(x) => to_val(&mut llvm::Constant::int(
            &llvm::Type::int32_type_in_context(ctx),
            *x,
        )),
        SetL(ref l) =>
        // simple look up global value
        {
            todo!()
        }
        Mov(ref n) => todo!(),
        Neg(ref n) => todo!(),
        _ => todo!(),
    }
}

fn to_llvm(
    ctx: &llvm::Context,
    fun: &llvm::Value,
    bblock: &llvm::BasicBlock,
    builder: &llvm::Builder,
    p: &asm::T,
) {
    // need to maintain id -> value map
    // and pass down name
    // need last inst flag to generate ret

    match p {
        asm::T::Ans(ref e) => todo!(),
        asm::T::Let((ref id, ref ty), ref e, ref t) => {
            todo!()
        }
    }
}

#[allow(unused_variables)]
pub fn f(p: &closure::Prog) {
    let vcode = r#virtual::f(p);
    let asm::Prog::Prog(floats, fds, c) = vcode;

    let ctx = llvm::Context::new();
    let mut module =
        llvm::Module::create_with_name_in_context("mincaml_main", &ctx);

    // Create constants for doubles
    let double_ty = llvm::Type::double_type_in_context(&ctx);

    // Keep a map from labels to values

    let mut globals = HashMap::<id::L, llvm::Global>::new();

    for (ref id, ref x) in floats {
        println!("Adding float const: {} = {}", id.0, x);

        let mut const_val = llvm::Constant::real(&double_ty, *x);
        let global_val =
            llvm::Global::add_to_module(&module, id.0.as_str(), &double_ty);
        global_val.set_constant(&mut const_val);
        globals.insert(id.clone(), global_val);

        // FIXME: set names?

        // unsafe {
        //     let name = std::ffi::CString::new(id.0.as_str())
        //         .expect("unable to create id");
        //     // llvm_sys::core::LLVMSetValueName2(
        //     //     val, id.0.as_ptr() as *const i8, 8);
        //     llvm_sys::core::LLVMSetValueName2(
        //         val,
        //         name.into_raw() as *const i8,
        //         8,
        //     );
        //     LLVMDumpValue(val);
        //     println!();
        // }

        // let str = unsafe {
        //     let buf = LLVMPrintValueToString(val);
        //     let cstr_buf = std::ffi::CStr::from_ptr(buf);
        //     let res = String::from_utf8_lossy(cstr_buf.to_bytes()).into_owned();
        //     LLVMDisposeMessage(buf);
        //     res
        // };

        // let name = unsafe {
        //     let mut sz: usize = 128;
        //     let buf = LLVMGetValueName2(val, &mut sz);
        //     let cstr_buf = std::ffi::CStr::from_ptr(buf);
        //     let res = String::from_utf8_lossy(cstr_buf.to_bytes()).into_owned();
        //     //LLVMDisposeMessage(buf);
        //     res
        // };
        // println!("str = {}, name = {}", str, name);
    }
    // labels

    // FIXME: allocate global variables (min_caml_hp)

    // fundefs

    // Note the conversion to the virtual assembler separates out
    // float and non-float arguments
    //
    // Makes less sense for x86_64, but lets keep it for now.
    //
    // Options would be to modify the virutal asm, or
    // to translate to LLVM from closure::Prog directly, or via our
    // own intermediate virtual asm.
    //
    // So, we need to transform the function arg types
    //

    fn split_ty_args(ty: &ty::Type) -> ty::Type {
        match ty {
            ty::Type::Fun(ref arg_tys, ref ret_ty) => {
                let mut tys: Vec<ty::Type> = vec![];
                let mut f_tys: Vec<ty::Type> = vec![];
                for t in arg_tys {
                    if *t == ty::Type::Float {
                        f_tys.push(ty::Type::Float)
                    } else {
                        tys.push(split_ty_args(t))
                    }
                }
                tys.extend(f_tys);
                ty::Type::Fun(tys, Box::new(split_ty_args(ret_ty)))
            }
            _ => ty.clone(),
        }
    }

    // Build map of closure FunDefs to modified type
    let mut fd_tys: HashMap<id::L, ty::Type> = HashMap::new();
    let closure::Prog::Prog(c_fds, _) = p;
    for fd in c_fds {
        let (id, ty) = &fd.name;
        fd_tys.insert(id.clone(), ty.clone());
    }

    for asm::FunDef {
        ref name,
        ref args,
        ref fargs,
        body: ref exp,
        ret: ref ret_ty,
    } in fds
    {
        let f_ty = fd_tys
            .get(name)
            .unwrap_or_else(|| panic!("No type for function '{}'", name.0));

        let f_ty_ = ty_to_type_in_context(&ctx, &split_ty_args(f_ty));

        let fun_val = module.add_function(name.0.as_str(), &f_ty_);
        // FIXME: function to convert ty::Type to Type
        //let fty = ty_to_type_in_context(&ctx, ty)
    }

    module.dump();

    /*
        Notes:

            * Create module in context
            * create functions in module
            * create basic blocks in function
            * use builder to add instructions

        IR code assumes flat memory and all float constants
        treated as 1D arrays, so need to look into arrays

        values can have names, but all consutrction done by value
        - keep map from labels/ids to LLVM values

        need to wrap up value

    */

    // Random note scratchpad
    //
    // Note: MachineModule

    // LLVMGetFunctionAddress(EE, Name)
    //LLVMGetSymbolAddress(SI)

    // GenericValue

    //LLVMDisasmInstruction(DC, Bytes, BytesSize, PC, OutString, OutStringSize)
    //LLVMCreateDisasm(TripleName, DisInfo, TagType, GetOpInfo, SymbolLookUp)
}
