#![allow(dead_code)]
#![allow(unused_variables)] // FIXME

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

type GlobalMap = HashMap<id::L, (llvm::Global, llvm::Type, ty::Type)>;

// Just generate list of instructions initially, then create basic blocks
// after this?

// FIXME: need our own Inst to map all Exp to Vec<Inst> and replicate
// tree structure before creating basic blocks

// Will try just going from asm::T to ins directly initially....

// FIXME: map from ids to values
// FIXME: value name or use empty name?
fn exp_to_llvm(
    globals: &GlobalMap,
    ctx: &llvm::Context,
    fun: &llvm::Value,
    bblock: &llvm::BasicBlock,
    builder: &llvm::Builder,
    vals: &mut HashMap<id::T, llvm::Value>,
    e: &asm::Exp,
) -> llvm::Value {
    // FIXME: all lookups should check globals first, to
    // deal with min_caml_hp

    fn to_val(c: &mut llvm::Constant) -> llvm::Value {
        let x = llvm::Value::from_ref(c.to_ref(), c.is_owned());
        c.release();
        x
    }

    fn get_val(
        globals: &GlobalMap,
        builder: &llvm::Builder,
        vals: &HashMap<id::T, llvm::Value>,
        id: &id::T,
    ) -> Value {
        let val = vals.get(id);
        match val {
            Some(v) => llvm::Value::from_ref(v.to_ref(), false),
            None => {
                let (glbl, ty, _) = globals
                    .get(&id::L(id.0.clone()))
                    .unwrap_or_else(|| panic!("No value '{}'", id.0));
                let ptr = &Value::from_ref(glbl.to_ref(), false);
                builder.load2(ty, ptr, "")
            }
        }
    }

    fn id_or_imm_to_value(
        ctx: &llvm::Context,
        globals: &GlobalMap,
        builder: &llvm::Builder,
        vals: &HashMap<id::T, llvm::Value>,
        operand: &asm::IdOrImm,
    ) -> llvm::Value {
        match operand {
            asm::IdOrImm::V(ref id) => get_val(globals, builder, vals, id),
            asm::IdOrImm::C(x) => {
                let int_ty = llvm::Type::int32_type_in_context(ctx);
                to_val(&mut llvm::Constant::int(&int_ty, *x))
            }
        }
    }

    use asm::Exp::*;
    eprintln!("Exp: {:?}", e);
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
            let (glbl, ty, _) = globals
                .get(l)
                .unwrap_or_else(|| panic!("No value '{}'", l.0));
            let ptr = &Value::from_ref(glbl.to_ref(), false);
            builder.load2(ty, ptr, "")
        }
        Mov(ref id) => {
            // let val = vals.get(id);
            // match val {
            //     Some(ref val) => todo!(),
            //     None => {
            //         let (glbl, ty, _) = globals.get(&id::L(id.0.clone()))
            //             .unwrap_or_else(|| panic!("No value '{}'", id.0));
            //         let ptr = &Value::from_ref(glbl.to_ref(), false);
            //         builder.load2(ty, ptr, "")
            //     }
            // }
            get_val(globals, builder, vals, id)
        }
        Neg(ref id) => {
            let val = get_val(globals, builder, vals, id);
            builder.neg(&val, "")
            // let val = vals
            //     .get(id)
            //     .unwrap_or_else(|| panic!("No value '{}'", id.0));
            // builder.neg(val, "")
        }
        Add(ref x, ref y) => {
            // let x_ =
            //     vals.get(x).unwrap_or_else(|| panic!("No value '{}'", x.0));
            let x_ = get_val(globals, builder, vals, x);
            let y_ = id_or_imm_to_value(ctx, globals, builder, vals, y);
            builder.add(&x_, &y_, "")
        }
        Sub(ref x, ref y) => {
            // let x_ =
            //     vals.get(x).unwrap_or_else(|| panic!("No value '{}'", x.0));
            let x_ = get_val(globals, builder, vals, x);
            let y_ = id_or_imm_to_value(ctx, globals, builder, vals, y);
            builder.sub(&x_, &y_, "")
        }
        CallDir(id, args, fargs) => {
            // get global for label
            let (fun_glbl, fun_ty, _) = globals
                .get(id)
                .unwrap_or_else(|| panic!("No global '{}'", id.0));
            // get values for args
            let mut args_: Vec<&llvm::Value> = vec![];
            for arg in args {
                args_.push(
                    vals.get(arg)
                        .unwrap_or_else(|| panic!("No value '{}'", arg.0)),
                );
            }
            // FIXME: fargs
            let f = llvm::Value::from_ref(fun_glbl.to_ref(), false);
            eprintln!("call2 for {}", id.0);
            builder.call2(fun_ty, &f, &args_, "")
        }
        _ => {
            println!("Not done: {:?}", e);
            todo!()
        }
    }
}

// FIXME: need globals map too...
fn to_llvm(
    globals: &GlobalMap,
    ctx: &llvm::Context,
    fun: &llvm::Value,
    bblock: &llvm::BasicBlock,
    builder: &llvm::Builder,
    vals: &mut HashMap<id::T, llvm::Value>,
    p: &asm::T,
) {
    // need to maintain id -> value map
    // and pass down name
    // need last inst flag to generate ret

    match p {
        asm::T::Ans(ref e) => {
            // FIXME: retVoid based on type...
            let val = exp_to_llvm(globals, ctx, fun, bblock, builder, vals, e);
            // give temp name to val
            let id = id::genid(&id::T(String::from(".ans")));
            val.set_name(id.0.as_str());
            builder.ret(&val);
        }
        asm::T::Let((ref id, ref ty), ref e, ref t) => {
            // FIXME: need to check if id is in globals
            // and do a store
            // FIXME: might need fresh fun, bblock, etc..
            let val = exp_to_llvm(globals, ctx, fun, bblock, builder, vals, e);
            // use an alias?
            eprintln!("Setting name to {}", id.0.as_str());
            val.set_name(id.0.as_str());
            // add to map
            vals.insert(id.clone(), val);
            to_llvm(globals, ctx, fun, bblock, builder, vals, t)
        }
    }
}

// Runtime functions

#[no_mangle]
pub extern "C" fn min_caml_print_int(x: i32) {
    print!("{}", x)
}

#[no_mangle]
pub extern "C" fn min_caml_print_newline() {
    println!()
}

// extern "C" {
//     //#[no_mangle]
//     static min_caml_hp: *const libc::c_void;
// }

// #[no_mangle]
// static min_caml_hp: *const libc::c_void = null_mut();

// #[no_mangle]
// static min_caml_hp: *mut libc::c_void = std::ptr::null();

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

    let mut globals = GlobalMap::new();

    for (ref id, ref x) in floats {
        println!("Adding float const: {} = {}", id.0, x);

        let mut const_val = llvm::Constant::real(&double_ty, *x);
        let global_val =
            llvm::Global::add_to_module(&module, id.0.as_str(), &double_ty);
        global_val.set_constant(&mut const_val);
        globals.insert(
            id.clone(),
            (
                global_val,
                llvm::Type::double_type_in_context(&ctx),
                ty::Type::Float,
            ),
        );

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
    ];

    // FIXME: need to split ty args
    for (name, ty, ptr) in runtime_functions {
        let ty_ = ty_to_type_in_context(&ctx, &ty);
        let glbl = llvm::Global::add_to_module(&module, name, &ty_);
        llvm::set_externally_initialized(&glbl, true);
        llvm::add_symbol(name, ptr);

        // add to globals map...
        globals.insert(id::L(name.to_string()), (glbl, ty_, ty.clone()));
    }

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

        // FIXME: add entry to vals using:
        // LLVMGetParam(Fn, Index)

        // FIXME: generate instructions...
    }

    let void_ty = llvm::Type::void_type_in_context(&ctx);

    // Add min_caml_hp as an external, so that we can easily observe it later
    let void_ptr_ty = llvm::Type::pointer_type(&void_ty, 0);
    let min_caml_hp_glbl =
        llvm::Global::add_to_module(&module, "min_caml_hp", &void_ptr_ty);
    llvm::set_externally_initialized(&min_caml_hp_glbl, true);

    // allocate memory
    // move this to a struct we can shove in an Rc<>
    // to allow keeping the JIT'd code alive
    let mut mem = unsafe { libc::malloc(128 * 1024 * 1024) };
    let min_caml_hp = mem;
    let min_caml_hp_ptr = &mut mem as *mut *mut libc::c_void;
    eprintln!("min_caml_hp: {:#X}", min_caml_hp as u64);
    eprintln!("&min_caml_hp: {:#X}", min_caml_hp_ptr as u64);

    llvm::add_symbol("min_caml_hp", min_caml_hp_ptr as *const libc::c_void);

    // add to globals map
    globals.insert(
        id::L(String::from("min_caml_hp")),
        (min_caml_hp_glbl, void_ptr_ty, ty::Type::Int),
    );

    // Generate instructions for top level expression

    let int_ty = llvm::Type::int32_type_in_context(&ctx);
    let main_arg_tys = [&void_ty];
    //let main_ty = llvm::Type::function_type(&int_ty, &main_arg_tys);
    let main_ty = llvm::Type::function_type(&void_ty, &main_arg_tys);
    let main_val = module.add_function("_main", &main_ty);

    let main_bb = llvm::BasicBlock::append_basic_block_in_context(
        &ctx, &main_val, "entry",
    );

    let builder = llvm::Builder::create_builder_in_context(&ctx);

    builder.position_builder_at_end(&main_bb);

    let mut vals = std::collections::HashMap::<id::T, llvm::Value>::new();
    to_llvm(&globals, &ctx, &main_val, &main_bb, &builder, &mut vals, &c);

    // FIXME: need Debug for everything...
    //println!("Globals: {:?}", globals);
    module.dump();

    let ee = llvm::ExecutionEngine::for_module(&mut module);

    // FIXME: check result...
    let main_addr = ee.function_address("_main");
    let main = unsafe {
        let main: extern "C" fn(()) -> () = std::mem::transmute(main_addr);
        main
    };
    println!("Addr for main: {:#X}", main_addr);
    main(());
    println!();

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
