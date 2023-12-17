#![allow(dead_code)]
#![allow(unused_variables)] // FIXME
#![allow(clippy::too_many_arguments)]

use llvm_sys::core::LLVMGetParam;
use llvm_sys::core::LLVMPointerType;

use crate::asm;
use crate::closure;
use crate::id;
use crate::llvm;
use crate::llvm::RefOwner;
use crate::llvm::Value;
use crate::r#virtual;
use crate::ty;

//extern crate llvm_sys;

//use libc::c_char;
use std::collections::HashMap;
use std::ptr;
//use std::ffi::CString;

pub fn ty_to_ref_type_in_context(
    ctx: &llvm::Context,
    ty: &ty::Type,
) -> llvm::Type {
    let val_ty = ty_to_type_in_context(ctx, ty, false, false);
    llvm::Type::pointer_type(&val_ty, 0)
}

pub fn ty_to_type_in_context(
    ctx: &llvm::Context,
    ty: &ty::Type,
    fn_ptr: bool,
    fn_closure: bool,
) -> llvm::Type {
    use llvm_sys::core::LLVMDoubleTypeInContext;
    use llvm_sys::core::LLVMFunctionType;
    use llvm_sys::core::LLVMInt64TypeInContext;
    use llvm_sys::core::LLVMVoidTypeInContext;
    use llvm_sys::prelude::LLVMTypeRef;

    fn to_type(
        ctx: &llvm::Context,
        ty: &ty::Type,
        fn_ptr: bool,
        fn_closure: bool,
    ) -> LLVMTypeRef {
        match ty {
            ty::Type::Unit => unsafe { LLVMVoidTypeInContext(ctx.to_ref()) },
            ty::Type::Bool => unsafe { LLVMInt64TypeInContext(ctx.to_ref()) },
            ty::Type::Int => unsafe { LLVMInt64TypeInContext(ctx.to_ref()) },
            ty::Type::Float => unsafe { LLVMDoubleTypeInContext(ctx.to_ref()) },
            ty::Type::Fun(ref arg_tys, ref ret_ty) => {
                let void_ptr_ty = unsafe {
                    LLVMPointerType(LLVMVoidTypeInContext(ctx.to_ref()), 0)
                };
                let mut arg_tys2 = if fn_closure {
                    vec![void_ptr_ty]
                } else {
                    vec![]
                };
                arg_tys2.extend(
                    arg_tys.iter().map(|ty| to_type(ctx, ty, true, true)),
                );
                let n: libc::c_uint = arg_tys2.len().try_into().unwrap();
                let fun_ty = unsafe {
                    LLVMFunctionType(
                        to_type(ctx, ret_ty, true, true),
                        arg_tys2.as_mut_ptr(),
                        n,
                        0,
                    )
                };
                if fn_ptr {
                    unsafe { LLVMPointerType(fun_ty, 0) }
                } else {
                    fun_ty
                }
            }
            ty::Type::Tuple(ref tys) => {
                // Should really be a struct, but just
                // use void* for now
                unsafe {
                    LLVMPointerType(LLVMVoidTypeInContext(ctx.to_ref()), 0)
                }
            }
            ty::Type::Array(ref elem_ty) => {
                // Note: no array dimensions, so just use a pointer
                let ty_ = to_type(ctx, elem_ty, true, true);
                // unsafe { LLVMArrayType(ElementType, ElementCount)}
                unsafe { LLVMPointerType(ty_, 0) }
            }
            ty::Type::Var(_) => unreachable!(),
        }
    }

    let ty_ = to_type(ctx, ty, fn_ptr, fn_closure);
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

type GlobalMap = HashMap<id::L, (llvm::Value, llvm::Type, ty::Type)>;
type ValueMap = HashMap<id::T, (llvm::Value, ty::Type)>;

// Just generate list of instructions initially, then create basic blocks
// after this?

// FIXME: need our own Inst to map all Exp to Vec<Inst> and replicate
// tree structure before creating basic blocks

// Will try just going from asm::T to ins directly initially....

fn exp_to_llvm(
    ctx: &llvm::Context,
    globals: &GlobalMap,
    module: &llvm::Module,
    fun: &llvm::Value,
    bblock: &llvm::BasicBlock,
    builder: &llvm::Builder,
    vals: &mut ValueMap,
    e: &asm::Exp,
    expected_ty: &ty::Type,
    next_bb: &Option<&llvm::BasicBlock>,
) -> llvm::Value {
    fn get_val(
        globals: &GlobalMap,
        builder: &llvm::Builder,
        vals: &ValueMap,
        id: &id::T,
    ) -> Value {
        // FIXME: special case
        if id.0 == "min_caml_hp" {
            let (glbl, ty, ty0) = globals
                .get(&id::L(id.0.clone()))
                .unwrap_or_else(|| panic!("No value '{}'", id.0));
            let ptr = &Value::from_ref(glbl.to_ref(), false);
            let ty_ = llvm::Type::pointer_type(ty, 0);
            // FIXME: use a temp?
            builder.load2(&ty_, ptr, "")
        } else {
            let val = vals.get(id);
            match val {
                Some((v, _)) => llvm::Value::from_ref(v.to_ref(), false),
                None => {
                    let (glbl, ty, ty0) = globals
                        .get(&id::L(id.0.clone()))
                        .unwrap_or_else(|| panic!("No value '{}'", id.0));
                    let ptr = &Value::from_ref(glbl.to_ref(), false);
                    // FIXME: we need a flag to configure this behaviour
                    // might want load of global as pointer
                    // or just use of global directly..
                    let ty_ = llvm::Type::pointer_type(ty, 0);
                    // FIXME: use a temp?
                    builder.load2(&ty_, ptr, "")
                    // let ptr = Value::from_ref(glbl.to_ref(), false);
                    // ptr
                }
            }
        }
    }

    fn id_or_imm_to_value(
        ctx: &llvm::Context,
        globals: &GlobalMap,
        builder: &llvm::Builder,
        vals: &ValueMap,
        operand: &asm::IdOrImm,
    ) -> llvm::Value {
        match operand {
            asm::IdOrImm::V(ref id) => get_val(globals, builder, vals, id),
            asm::IdOrImm::C(x) => {
                let int_ty = llvm::Type::int64_type_in_context(ctx);
                llvm::constant::int(&int_ty, *x)
            }
        }
    }

    fn struct_index(
        ctx: &llvm::Context,
        globals: &GlobalMap,
        builder: &llvm::Builder,
        vals: &ValueMap,
        base: &id::T,
        idx: &asm::IdOrImm,
        step: i32,
    ) -> llvm::Value {
        // let int32_ty = llvm::Type::int32_type_in_context(ctx);
        // let step_c = llvm::constant::int(&int32_ty, step);
        // let idx_val = id_or_imm_to_value(ctx, globals, builder, vals, idx);
        // // // FIXME: probably need a temp name here
        // let offset_val = builder.mul(&idx_val, &step_c, "");
        // let base_val = get_val(globals, builder, vals, base);
        // builder.add(&base_val, &offset_val, "")
        let int64_ty = llvm::Type::int64_type_in_context(ctx);
        let step_c = llvm::constant::int(&int64_ty, step);
        let idx_val = id_or_imm_to_value(ctx, globals, builder, vals, idx);
        // // // FIXME: probably need a temp name here
        let offset_val = builder.mul(&idx_val, &step_c, "");
        let base_val = get_val(globals, builder, vals, base);
        //builder.add_nuw(&base_val, &offset_val, "")
        builder.add(&base_val, &offset_val, "")
    }

    use asm::Exp::*;
    eprintln!("Exp: {:?}", e);
    match e {
        // FIXME: call to llvm.donothing()?
        Nop => llvm::constant::int(&llvm::Type::int64_type_in_context(ctx), 0),
        Set(x) => {
            llvm::constant::int(&llvm::Type::int64_type_in_context(ctx), *x)
        }
        SetL(ref l) =>
        // simple look up global value
        {
            let (glbl, ty, ty0) = globals
                .get(l)
                .unwrap_or_else(|| panic!("No value '{}'", l.0));
            let ptr_ty = ty_to_ref_type_in_context(ctx, ty0);
            let ptr = Value::from_ref(glbl.to_ref(), false);
            eprintln!("SetL {} : {:?}", l.0, ty0);
            ptr
        }
        Mov(ref id) => {
            get_val(globals, builder, vals, id)
            // let expected_ty_ = ty_to_ref_type_in_context(ctx, expected_ty);
            // // FIXME: name is ignored and we
            // // don't get a fresh binding
            // let val = builder.bitcast(&val, &expected_ty_, "MOV");
            // //val.set_name("MOV");
            //val
            // Idea: have our own fn to return fresh values & cast
            // there is the llvm.ssa.copy intrinsic to do this
            // use load for globals, llvm.ssa.copy for local vals
        }
        Neg(ref id) => {
            let val = get_val(globals, builder, vals, id);
            builder.neg(&val, "")
        }
        Add(ref x, ref y) => {
            let x_ = get_val(globals, builder, vals, x);
            let y_ = id_or_imm_to_value(ctx, globals, builder, vals, y);
            builder.add(&x_, &y_, "")
        }
        Sub(ref x, ref y) => {
            let x_ = get_val(globals, builder, vals, x);
            let y_ = id_or_imm_to_value(ctx, globals, builder, vals, y);
            builder.sub(&x_, &y_, "")
        }
        Ld(ref base, ref idx, ref step) => {
            let ptr =
                struct_index(ctx, globals, builder, vals, base, idx, *step);
            eprintln!("Ld {}", base.0);
            // FIXME: need type of base which can be a global or a local
            // so need to store value
            // not enough... need to pass down expected type...?
            // or translate asm to our own instructions and propagate
            // type info accordingly...
            //
            // type of base is not enough. Currently the type of base is
            // Fun xxx for closures instead of a structured type for the
            // closure itself. The types are embedded in the various
            // let statements
            //
            // in any future compiler, should have explicitly typed
            // structures for closures
            //
            let expected_ty_ =
                ty_to_type_in_context(ctx, expected_ty, true, false);
            let ptr_ty = llvm::Type::pointer_type(&expected_ty_, 0);
            builder.load2(&expected_ty_, &ptr, "")
        }
        St(ref x, ref base, ref idx, ref step) => {
            let ptr =
                struct_index(ctx, globals, builder, vals, base, idx, *step);
            let x_ = get_val(globals, builder, vals, x);
            builder.store(&x_, &ptr)
        }
        FMovD(ref id) => get_val(globals, builder, vals, id),
        FNegD(ref id) => {
            let val = get_val(globals, builder, vals, id);
            builder.fneg(&val, "")
        }
        FAddD(ref x, ref y) => {
            let x_ = get_val(globals, builder, vals, x);
            let y_ = get_val(globals, builder, vals, y);
            builder.fadd(&x_, &y_, "")
        }
        FSubD(ref x, ref y) => {
            let x_ = get_val(globals, builder, vals, x);
            let y_ = get_val(globals, builder, vals, y);
            builder.fsub(&x_, &y_, "")
        }
        FMulD(ref x, ref y) => {
            let x_ = get_val(globals, builder, vals, x);
            let y_ = get_val(globals, builder, vals, y);
            builder.fmul(&x_, &y_, "")
        }
        FDivD(ref x, ref y) => {
            let x_ = get_val(globals, builder, vals, x);
            let y_ = get_val(globals, builder, vals, y);
            builder.fdiv(&x_, &y_, "")
        }
        LdDF(ref base, ref idx, ref step) => {
            let ptr =
                struct_index(ctx, globals, builder, vals, base, idx, *step);
            eprintln!("LdDF {}", base.0);
            let expected_ty_ =
                ty_to_type_in_context(ctx, expected_ty, true, false);
            let ptr_ty = llvm::Type::pointer_type(&expected_ty_, 0);
            builder.load2(&expected_ty_, &ptr, "")
        }
        StDF(ref x, ref base, ref idx, ref step) => {
            let ptr =
                struct_index(ctx, globals, builder, vals, base, idx, *step);
            let x_ = get_val(globals, builder, vals, x);
            builder.store(&x_, &ptr)
        }
        Comment(ref txt) => {
            todo!()
        }
        IfEq(ref x, _, ref e1, ref e2)
        | IfLE(ref x, _, ref e1, ref e2)
        | IfGE(ref x, _, ref e1, ref e2)
        | IfLt(ref x, _, ref e1, ref e2)
        | IfGt(ref x, _, ref e1, ref e2)
        | IfFEq(ref x, _, ref e1, ref e2)
        | IfFLE(ref x, _, ref e1, ref e2)
        | IfFGE(ref x, _, ref e1, ref e2)
        | IfFLt(ref x, _, ref e1, ref e2)
        | IfFGt(ref x, _, ref e1, ref e2) => {
            // condition
            let x_ = get_val(globals, builder, vals, x);
            let cond_id = id::genid(&id::T(String::from("brC")));
            let cond = match e {
                IfEq(_, ref y, _, _)
                | IfLE(_, ref y, _, _)
                | IfGE(_, ref y, _, _)
                | IfLt(_, ref y, _, _)
                | IfGt(_, ref y, _, _) => {
                    let y_ = id_or_imm_to_value(ctx, globals, builder, vals, y);
                    match e {
                        IfEq(_, _, _, _) => {
                            builder.icmp_eq(&x_, &y_, &cond_id.0)
                        }
                        IfLE(_, _, _, _) => {
                            builder.icmp_le(&x_, &y_, &cond_id.0)
                        }
                        IfGE(_, _, _, _) => {
                            builder.icmp_ge(&x_, &y_, &cond_id.0)
                        }
                        IfLt(_, _, _, _) => {
                            builder.icmp_lt(&x_, &y_, &cond_id.0)
                        }
                        IfGt(_, _, _, _) => {
                            builder.icmp_gt(&x_, &y_, &cond_id.0)
                        }
                        _ => unreachable!(),
                    }
                }
                IfFEq(_, ref y, _, _)
                | IfFLE(_, ref y, _, _)
                | IfFGE(_, ref y, _, _)
                | IfFLt(_, ref y, _, _)
                | IfFGt(_, ref y, _, _) => {
                    let y_ = get_val(globals, builder, vals, y);
                    match e {
                        IfFEq(_, _, _, _) => {
                            builder.fcmp_eq(&x_, &y_, &cond_id.0)
                        }
                        IfFLE(_, _, _, _) => {
                            builder.fcmp_le(&x_, &y_, &cond_id.0)
                        }
                        IfFGE(_, _, _, _) => {
                            builder.fcmp_ge(&x_, &y_, &cond_id.0)
                        }
                        IfFLt(_, _, _, _) => {
                            builder.fcmp_lt(&x_, &y_, &cond_id.0)
                        }
                        IfFGt(_, _, _, _) => {
                            builder.fcmp_gt(&x_, &y_, &cond_id.0)
                        }
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!(),
            };
            // result of the if expression
            let if_id = id::genid(&id::T(String::from("if")));
            // labels and basic blocks for each branch
            let e1_lbl = id::genid(&id::T(String::from("brT")));
            let e1_bb =
                llvm::BasicBlock::create_basic_block_in_context(ctx, &e1_lbl.0);
            let e2_lbl = id::genid(&id::T(String::from("brF")));
            let e2_bb =
                llvm::BasicBlock::create_basic_block_in_context(ctx, &e2_lbl.0);
            let end_lbl = id::genid(&id::T(String::from("brE")));
            let end_bb = llvm::BasicBlock::create_basic_block_in_context(
                ctx, &end_lbl.0,
            );
            // conditional branch
            builder.cond_br(&cond, &e1_bb, &e2_bb);

            // basic block for true e1
            e1_bb.append(fun);
            builder.position_builder_at_end(&e1_bb);
            let e1_val = to_llvm(
                ctx,
                globals,
                module,
                fun,
                &e1_bb,
                builder,
                vals,
                e1,
                expected_ty,
                &Some(&end_bb),
            );
            // Note: recursve calls can create new basic blocks
            let e1_last_bb = llvm::BasicBlock::get_last(fun);
            // - branch to end, if last instruction wasn't a branch
            if !e1_last_bb.is_terminated() {
                builder.position_builder_at_end(&e1_bb);
                builder.br(&end_bb);
            }
            //e1_bb.append(fun);

            // basic block for false e2
            e2_bb.append(fun);
            builder.position_builder_at_end(&e2_bb);
            let e2_val = to_llvm(
                ctx,
                globals,
                module,
                fun,
                &e2_bb,
                builder,
                vals,
                e2,
                expected_ty,
                &Some(&end_bb),
            );
            // Note: recursve calls can create new basic blocks
            let e2_last_bb = llvm::BasicBlock::get_last(fun);
            // - branch to end, if last instruction wasn't a branch
            if !e2_last_bb.is_terminated() {
                builder.position_builder_at_end(&e2_bb);
                builder.br(&end_bb);
            }
            //e2_bb.append(fun);

            // `end` basic block
            builder.position_builder_at_end(&end_bb);
            // - phi with results from previous blocks
            let val = if expected_ty == &ty::Type::Unit {
                // FIXME: no-op, call to llvm.donothing
                llvm::constant::int(&llvm::Type::int64_type_in_context(ctx), 0)
            } else {
                let expected_ty_ =
                    ty_to_type_in_context(ctx, expected_ty, true, false);
                builder.phi(
                    &expected_ty_,
                    &if_id.0,
                    &[(&e1_val, &e1_last_bb), (&e2_val, &e2_last_bb)],
                )
            };
            // Terminate this Basic Block
            match next_bb {
                None => (),
                Some(bb) => {
                    builder.br(bb);
                }
            }
            end_bb.append(fun);
            val
        }
        CallCls(ref id, ref args, ref fargs) => {
            let cls_val = get_val(globals, builder, vals, id);
            let void_ty = llvm::Type::void_type_in_context(ctx);
            let void_ptr_ty = llvm::Type::pointer_type(&void_ty, 0);
            // read function arg from first word in closure
            let fn_ptr = builder.load2(&void_ptr_ty, &cls_val, "");

            // call closure function with closure as first arg
            let (_, cls_ty) = vals.get(id).unwrap_or_else(|| {
                panic!("Unable to get type for closure '{}'", id.0)
            });

            // build args, first is closure
            let mut args_: Vec<&llvm::Value> = vec![&cls_val];
            for arg in args.iter().chain(fargs) {
                let (val, _) = vals
                    .get(arg)
                    .unwrap_or_else(|| panic!("No value '{}'", arg.0));
                args_.push(val);
            }
            // call function
            let cls_ty_ = ty_to_type_in_context(ctx, cls_ty, false, true);
            builder.call2(&cls_ty_, &fn_ptr, &args_, "")
        }
        CallDir(id, args, fargs) => {
            // get global for label
            let (fun_glbl, fun_ty, _) = globals
                .get(id)
                .unwrap_or_else(|| panic!("No global '{}'", id.0));
            let mut args_: Vec<&llvm::Value> = vec![];
            for arg in args.iter().chain(fargs) {
                let (val, _) = vals
                    .get(arg)
                    .unwrap_or_else(|| panic!("No value '{}'", arg.0));
                args_.push(val);
            }
            let f = llvm::Value::from_ref(fun_glbl.to_ref(), false);
            eprintln!("call2 for {}", id.0);
            builder.call2(fun_ty, &f, &args_, "")
        }
        Save(_, _) | Restore(_) => unreachable!(),
    }
}

fn to_llvm(
    ctx: &llvm::Context,
    globals: &GlobalMap,
    module: &llvm::Module,
    fun: &llvm::Value,
    bblock: &llvm::BasicBlock,
    builder: &llvm::Builder,
    vals: &mut ValueMap,
    p: &asm::T,
    expected_ty: &ty::Type,
    next_bb: &Option<&llvm::BasicBlock>,
) -> Value {
    // need to maintain id -> value map
    // and pass down name
    // need last inst flag to generate ret

    match p {
        asm::T::Ans(ref e) => {
            // FIXME: retVoid based on type...
            let val = exp_to_llvm(
                ctx,
                globals,
                module,
                fun,
                bblock,
                builder,
                vals,
                e,
                expected_ty,
                next_bb,
            );
            if val.get_name().is_none() {
                // give temp name to val
                let id = id::genid(&id::T(String::from(".ans")));
                val.set_name(id.0.as_str());
            }
            val
        }
        asm::T::Let((ref id, ref ty), ref e, ref t) => {
            // need a temporary for the intermediate?
            // FIXME: might need fresh fun, bblock, etc..
            let val = exp_to_llvm(
                ctx, globals, module, fun, bblock, builder, vals, e, ty,
                next_bb,
            );
            // FIXME: below is inefficient
            // FIXME: below captures global functions that
            // also have local binding with the same name for closures
            // (which they probably shouldn't)
            // keep a separate global variables map?
            // or a flag?
            //if globals.contains_key(&id::L(id.0.clone())) {
            // FIXME: hardcoded
            if id.0 == "min_caml_hp" {
                // "let binding" to global - need to store
                eprintln!("Store for global {}", id.0);
                let l = id::L(id.0.clone());
                let (glbl, _, _) = globals.get(&l).unwrap();
                builder.store(&val, glbl);
            } else {
                // use an alias?
                if val.get_name().unwrap_or_default().is_empty() {
                    eprintln!("Setting name to {}", id.0.as_str());
                    val.set_name(id.0.as_str());
                } else {
                    eprintln!(
                        "Warning, ignoring bind of {} to {}",
                        val.get_name().unwrap(),
                        id.0.as_str()
                    );
                }
            }
            //module.dump();
            // add to map
            vals.insert(id.clone(), (val, ty.clone()));

            // recurse
            to_llvm(
                ctx,
                globals,
                module,
                fun,
                bblock,
                builder,
                vals,
                t,
                expected_ty,
                next_bb,
            )
        }
    }
}

fn to_llvm_fun(
    ctx: &llvm::Context,
    globals: &GlobalMap,
    module: &llvm::Module,
    fun: &llvm::Value,
    bblock: &llvm::BasicBlock,
    builder: &llvm::Builder,
    vals: &mut ValueMap,
    p: &asm::T,
    expected_ty: &ty::Type,
) {
    let val = to_llvm(
        ctx,
        globals,
        module,
        fun,
        bblock,
        builder,
        vals,
        p,
        expected_ty,
        &None,
    );
    if *expected_ty == ty::Type::Unit {
        builder.ret_void();
    } else {
        builder.ret(&val);
    }
}

// Runtime functions

#[no_mangle]
pub extern "C" fn min_caml_print_int(x: i64) {
    print!("{}", x)
}

#[no_mangle]
pub extern "C" fn min_caml_print_newline() {
    println!()
}

#[no_mangle]
pub extern "C" fn min_caml_abs_float(x: f64) -> f64 {
    x.abs()
}

#[no_mangle]
pub extern "C" fn min_caml_sqrt(x: f64) -> f64 {
    x.sqrt()
}

#[no_mangle]
pub extern "C" fn min_caml_sin(x: f64) -> f64 {
    x.sin()
}

#[no_mangle]
pub extern "C" fn min_caml_cos(x: f64) -> f64 {
    x.cos()
}

#[no_mangle]
pub extern "C" fn min_caml_float_of_int(x: i64) -> f64 {
    let x_: i32 = x.try_into().unwrap();
    f64::from(x_)
}

#[no_mangle]
pub extern "C" fn min_caml_int_of_float(x: f64) -> i64 {
    x.round() as i64
}

#[no_mangle]
pub extern "C" fn min_caml_truncate(x: f64) -> i64 {
    x as i64
}

static mut MIN_CAML_HP: *mut libc::c_void = ptr::null_mut();

#[no_mangle]
pub extern "C" fn min_caml_create_array(
    sz: u64,
    val: u64,
) -> *mut libc::c_void {
    println!("Initialising array: {}, {}", sz, val);
    unsafe {
        let p = MIN_CAML_HP;
        let sz_ = sz as usize;
        let arr: &mut [u64] =
            std::slice::from_raw_parts_mut(MIN_CAML_HP as *mut u64, sz_);
        for v in arr.iter_mut().take(sz_) {
            *v = val;
        }
        MIN_CAML_HP = MIN_CAML_HP.wrapping_add(sz_ * 8);
        p
    }
}

#[no_mangle]
pub extern "C" fn min_caml_create_float_array(
    sz: u64,
    val: f64,
) -> *mut libc::c_void {
    println!("Initialising float array: {}, {}", sz, val);
    unsafe {
        let p = MIN_CAML_HP;
        let sz_ = sz as usize;
        let arr: &mut [f64] =
            std::slice::from_raw_parts_mut(MIN_CAML_HP as *mut f64, sz_);
        for v in arr.iter_mut().take(sz_) {
            *v = val;
        }
        MIN_CAML_HP = MIN_CAML_HP.wrapping_add(sz_ * 8);
        p
    }
}

// extern "C" {
//     //#[no_mangle]
//     static min_caml_hp: *const libc::c_void;
// }

// #[no_mangle]
// static min_caml_hp: *const libc::c_void = null_mut();

// #[no_mangle]
// static min_caml_hp: *mut libc::c_void = std::ptr::null();

// determine if a function is a closure or not
// Note: yet another signal we should be translating to our
// own ADT from closure::Prog
// arguably should be Option<bool>
fn is_closure(fun_id: &id::L, p: &closure::Prog) -> bool {
    fn is_clos(fun_id: &id::L, term: &closure::T) -> bool {
        use closure::T::*;
        match term {
            Unit
            | Int(_)
            | Float(_)
            | Neg(_)
            | Add(_, _)
            | Sub(_, _)
            | FNeg(_)
            | FAdd(_, _)
            | FSub(_, _)
            | FMul(_, _)
            | FDiv(_, _)
            | Var(_)
            | AppDir(_, _)
            | Tuple(_)
            | Get(_, _)
            | Put(_, _, _)
            | ExtArray(_) => false,
            IfEq(_, _, ref t_b, ref f_b)
            | IfLE(_, _, ref t_b, ref f_b)
            | IfLt(_, _, ref t_b, ref f_b)
            | IfGt(_, _, ref t_b, ref f_b)
            | IfGe(_, _, ref t_b, ref f_b) => {
                is_clos(fun_id, t_b) || is_clos(fun_id, f_b)
            }
            Let(_, ref e0, ref e1) => {
                is_clos(fun_id, e0) || is_clos(fun_id, e1)
            }
            MakeCls((ref cls_id, _), _, ref t) => {
                if cls_id.0 == fun_id.0 {
                    true
                } else {
                    is_clos(fun_id, t)
                }
            }
            AppCls(ref cls_id, _) => cls_id.0 == fun_id.0,
            LetTuple(_, _, ref e) => is_clos(fun_id, e),
        }
    }

    let closure::Prog::Prog(ref fundefs, ref main) = p;

    let mut res = false;
    for closure::FunDef {
        name: _,
        args: _,
        formal_fv: _,
        ref body,
    } in fundefs
    {
        res = res || is_clos(fun_id, body)
    }
    res || is_clos(fun_id, main)
}

#[allow(unused_variables)]
pub fn f(p: &closure::Prog) {
    // fn - build set of closures by identifying all calls to CallCls?
    // ideally want flag in FunDefs...

    let cfg = r#virtual::Config {
        int_size: 8,
        float_size: 8,
    };
    let vcode = r#virtual::f(&cfg, p);
    let sep = "--------";
    println!("{:?}\n{}", vcode, sep);
    let mut out = String::new();
    vcode.pp(&mut out, 0).expect("unable to pretty print!");
    println!("{}", out);
    println!("{}", sep);
    let asm::Prog::Prog(floats, fds, c) = vcode;

    let ctx = llvm::Context::new();
    let mut module =
        llvm::Module::create_with_name_in_context("mincaml_main", &ctx);

    // Create constants for doubles
    let double_ty = llvm::Type::double_type_in_context(&ctx);

    // Keep a map from labels to values

    let mut globals = GlobalMap::new();

    for (ref id, ref x) in floats {
        println!("Adding float const: {} = {}", id.0, x);

        let mut const_val = llvm::constant::real(&double_ty, *x);
        let global_val = module.add_global(id.0.as_str(), &double_ty);
        global_val.set_constant(&mut const_val);
        globals.insert(
            id.clone(),
            (
                global_val,
                llvm::Type::double_type_in_context(&ctx),
                ty::Type::Float,
            ),
        );

        // let str = unsafe {
        //     let buf = LLVMPrintValueToString(val);
        //     let cstr_buf = std::ffi::CStr::from_ptr(buf);
        //     let res = String::from_utf8_lossy(cstr_buf.to_bytes()).into_owned();
        //     LLVMDisposeMessage(buf);
        //     res
        // };
    }
    // labels

    // Add intrinsics

    // FIXME: Add symbols for runtime

    // FIXME: Need to add globals first, then define symbols are ee is created?

    llvm::llvm_init();

    let runtime_functions = vec![
        (
            "min_caml_print_int",
            ty::Type::Fun(vec![ty::Type::Int], Box::new(ty::Type::Unit)),
            min_caml_print_int as *const core::ffi::c_void,
        ),
        (
            "min_caml_print_newline",
            ty::Type::Fun(vec![], Box::new(ty::Type::Unit)),
            min_caml_print_newline as *const core::ffi::c_void,
        ),
        (
            "min_caml_abs_float",
            ty::Type::Fun(vec![ty::Type::Float], Box::new(ty::Type::Float)),
            min_caml_abs_float as *const core::ffi::c_void,
        ),
        (
            "min_caml_sqrt",
            ty::Type::Fun(vec![ty::Type::Float], Box::new(ty::Type::Float)),
            min_caml_sqrt as *const core::ffi::c_void,
        ),
        (
            "min_caml_sin",
            ty::Type::Fun(vec![ty::Type::Float], Box::new(ty::Type::Float)),
            min_caml_sin as *const core::ffi::c_void,
        ),
        (
            "min_caml_cos",
            ty::Type::Fun(vec![ty::Type::Float], Box::new(ty::Type::Float)),
            min_caml_cos as *const core::ffi::c_void,
        ),
        (
            "min_caml_float_of_int",
            ty::Type::Fun(vec![ty::Type::Int], Box::new(ty::Type::Float)),
            min_caml_float_of_int as *const core::ffi::c_void,
        ),
        (
            "min_caml_int_of_float",
            ty::Type::Fun(vec![ty::Type::Float], Box::new(ty::Type::Int)),
            min_caml_int_of_float as *const core::ffi::c_void,
        ),
        (
            "min_caml_truncate",
            ty::Type::Fun(vec![ty::Type::Float], Box::new(ty::Type::Int)),
            min_caml_truncate as *const core::ffi::c_void,
        ),
        (
            "min_caml_create_array",
            ty::Type::Fun(
                vec![ty::Type::Int, ty::Type::Int],
                Box::new(ty::Type::Int), // Really want address type
            ),
            min_caml_create_array as *const core::ffi::c_void,
        ),
        (
            "min_caml_create_float_array",
            ty::Type::Fun(
                vec![ty::Type::Int, ty::Type::Float],
                Box::new(ty::Type::Int), // Really want address type
            ),
            min_caml_create_float_array as *const core::ffi::c_void,
        ),
    ];

    // FIXME: need to split ty args
    for (name, ty, ptr) in runtime_functions {
        let ty_ = ty_to_type_in_context(&ctx, &ty, false, false);
        let glbl = module.add_global(name, &ty_);
        llvm::set_externally_initialized(&glbl, true);
        llvm::add_symbol(name, ptr);

        // add to globals map...
        globals.insert(id::L(name.to_string()), (glbl, ty_, ty.clone()));
    }

    let void_ty = llvm::Type::void_type_in_context(&ctx);

    // Add min_caml_hp as an external, so that we can easily observe it later
    let void_ptr_ty = llvm::Type::pointer_type(&void_ty, 0);
    let min_caml_hp_glbl = module.add_global("min_caml_hp", &void_ptr_ty);
    llvm::set_externally_initialized(&min_caml_hp_glbl, true);

    // allocate heap memory
    // move this to a struct we can shove in an Rc<>
    // to allow keeping the JIT'd code alive
    let min_caml_heap = unsafe { libc::malloc(128 * 1024 * 1024) };
    unsafe { MIN_CAML_HP = min_caml_heap };
    let min_caml_hp_ptr = unsafe { &mut MIN_CAML_HP as *mut *mut libc::c_void };
    eprintln!("min_caml_heap: {:#X}", min_caml_heap as u64);
    eprintln!("&min_caml_hp_ptr: {:#X}", min_caml_hp_ptr as u64);

    // add to globals map
    globals.insert(
        id::L(String::from("min_caml_hp")),
        (min_caml_hp_glbl, void_ptr_ty, ty::Type::Int),
    );

    let mut vals = ValueMap::new();
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
        let is_clos = is_closure(name, p);

        // For closures, we need the function type
        // to be extended with the closure parameter, and to add
        // a GetParam instruction to get the closure argument with
        // the same local name as the function.
        //
        // Note: we need to assume all function types are closures
        // as these should be the only values passed around of function type.
        // arguably, closures should be explicitly marked as such.
        //
        let f_ty = fd_tys
            .get(name)
            .unwrap_or_else(|| panic!("No type for function '{}'", name.0));

        let f_ty_ = {
            match split_ty_args(f_ty) {
                ty::Type::Fun(ref arg_tys, ref res_ty) => {
                    // add closure argument
                    let closure_ty = llvm::Type::pointer_type(&void_ty, 0);
                    let mut arg_tys_ =
                        if is_clos { vec![closure_ty] } else { vec![] };
                    for ty in arg_tys {
                        arg_tys_
                            .push(ty_to_type_in_context(&ctx, ty, true, true));
                    }
                    let arg_tys_2: Vec<&llvm::Type> = arg_tys_.iter().collect();
                    llvm::Type::function_type(
                        &ty_to_type_in_context(&ctx, res_ty, true, true),
                        &arg_tys_2,
                    )
                }
                _ => panic!("Function type expected, got: {:?}", f_ty),
            }
        };
        println!("Adding closure '{}' of type:", name.0);
        f_ty_.dump();
        println!();

        let fun_val = module.add_function(name.0.as_str(), &f_ty_);

        // set arg names and add entry to vals for each
        // note - arg names are globally unique, so
        // no need for nestend environments

        let v = if is_clos {
            vec![id::T(name.0.clone())]
        } else {
            vec![]
        };
        for (idx, id) in
            v.iter().chain(args.iter()).chain(fargs.iter()).enumerate()
        {
            let p = unsafe {
                LLVMGetParam(fun_val.to_ref(), idx.try_into().unwrap())
            };
            let p_ = Value::from_ref(p, false);
            p_.set_name(&id.0);
            vals.insert(id.clone(), (p_, f_ty.clone()));
        }

        // FIXME: need clone
        let fun_val2 = Value::from_ref(fun_val.to_ref(), false);

        globals.insert(name.clone(), (fun_val2, f_ty_, f_ty.clone()));

        // new basic block for function

        let fun_bb = llvm::BasicBlock::append_basic_block_in_context(
            &ctx, &fun_val, "entry",
        );

        let builder = llvm::Builder::create_builder_in_context(&ctx);

        builder.position_builder_at_end(&fun_bb);

        to_llvm_fun(
            &ctx, &globals, &module, &fun_val, &fun_bb, &builder, &mut vals,
            exp, ret_ty,
        );
    }

    // Generate instructions for top level expression

    let int_ty = llvm::Type::int64_type_in_context(&ctx);
    let main_arg_tys = [&void_ty];
    //let main_ty = llvm::Type::function_type(&int_ty, &main_arg_tys);
    let main_ty = llvm::Type::function_type(&void_ty, &main_arg_tys);
    let main_val = module.add_function("_main", &main_ty);

    let main_bb = llvm::BasicBlock::append_basic_block_in_context(
        &ctx, &main_val, "entry",
    );

    let builder = llvm::Builder::create_builder_in_context(&ctx);

    builder.position_builder_at_end(&main_bb);

    to_llvm_fun(
        &ctx,
        &globals,
        &module,
        &main_val,
        &main_bb,
        &builder,
        &mut vals,
        &c,
        &ty::Type::Unit,
    );

    // FIXME: need Debug for everything...
    //println!("Globals: {:?}", globals);
    module.dump();

    eprintln!("Creating execution engine...");
    let ee = llvm::ExecutionEngine::for_module(&mut module);
    // FIXME: check result...
    eprintln!("done: {:X}", ee.to_ref() as u64);

    llvm::add_symbol_mut("min_caml_hp", min_caml_hp_ptr as *mut libc::c_void);

    let main_addr = ee.function_address("_main");
    let main = unsafe {
        let main: extern "C" fn(()) -> () = std::mem::transmute(main_addr);
        main
    };
    println!("Addr for main: {:#X}", main_addr);
    let min_caml_hp_0 = unsafe { *min_caml_hp_ptr };
    eprintln!("min_caml_hp: {:#X}", unsafe { *min_caml_hp_ptr as u64 });
    main(());
    println!();
    let min_caml_hp_1 = unsafe { *min_caml_hp_ptr };
    eprintln!("min_caml_hp: {:#X}", unsafe { *min_caml_hp_ptr as u64 });
    // FIXME: slice::from_raw_parts
    let mut ptr = min_caml_hp_0;
    while ptr < min_caml_hp_1 {
        let x = unsafe { *(ptr as *const u64) };
        eprintln!("{:#016X}", x);
        ptr = ptr.wrapping_add(8);
    }

    // FIXME: dump addreses for each fn

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
