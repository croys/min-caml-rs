#![allow(unused_variables)] // FIXME

use crate::llvm;
use crate::llvm::{BasicBlock, Builder, Constant, Context,
    ExecutionEngine, Global, LLJITBuilder, Module, RefOwner, Type,
    llvm_init, Value,
    ThreadSafeContext, ThreadSafeModule,
    // LLJIT,
 };
extern crate llvm_sys;


//use llvm_sys::core::{LLVMConstInt};
use llvm_sys::core::{LLVMModuleCreateWithNameInContext, LLVMAddGlobal, LLVMDoubleTypeInContext, LLVMArrayType, LLVMSetGlobalConstant, LLVMSetInitializer, LLVMDumpModule, LLVMIntTypeInContext, LLVMFunctionType, LLVMAddFunction, LLVMAppendBasicBlockInContext, LLVMCreateBuilderInContext, LLVMPositionBuilderAtEnd, LLVMGetParam, LLVMBuildAdd, LLVMContextCreate };
/*, LLVMStructTypeInContext, */
use llvm_sys::core::LLVMGetGlobalContext;
//use llvm_sys::core::{LLVMModuleCreateWithName};
//use llvm_sys::disassembler::{LLVMCreateDisasm, LLVMDisasmInstruction};
use llvm_sys::execution_engine::{LLVMExecutionEngineRef, LLVMCreateExecutionEngineForModule, LLVMLinkInMCJIT, LLVMGetFunctionAddress, LLVMGetGlobalValueAddress};
//use llvm_sys::prelude::LLVMBool;
use llvm_sys::core::{LLVMConstReal, LLVMConstArray};
//use llvm_sys::object::LLVMGetSymbolAddress;
use llvm_sys::prelude::LLVMValueRef;
//use llvm_sys::LLVMValue;

//use llvm_sys::support::LLVMAddSymbol;
//use llvm_sys::target::{LLVM_InitializeNativeAsmParser};
use llvm_sys::target::{LLVM_InitializeNativeTarget, LLVM_InitializeNativeAsmPrinter};

use libc::c_char;
//use std::ffi::{CString, CStr};

#[test]
pub fn test_llvm_module2() {
    // create a module with constant, read the constant..
    let context = unsafe {
        LLVMGetGlobalContext()
    };

    let module = unsafe {
        LLVMModuleCreateWithNameInContext(
            "test_mod2\0".as_ptr() as *const c_char,
            context)
    };

    let double_ty = unsafe {
        LLVMDoubleTypeInContext(context)
    };

    let array_double_ty = unsafe {
        LLVMArrayType(double_ty, 1)
    };

    let global_var = unsafe {
        LLVMAddGlobal(
            module,
            array_double_ty,
            "dbl_arr\0".as_ptr() as *const c_char
        )
    };

    let mut x_0 = unsafe {
        LLVMConstReal(double_ty, 1.61)
    };

    let x_0_ptr = (&mut x_0) as *mut LLVMValueRef;

    let arr = unsafe {
        LLVMConstArray(double_ty, x_0_ptr, 1)
        //LLVMConstArray(double_ty, x_0_ptr as *mut LLVMValueRef , 1)
        //LLVMConstArray(double_ty, &mut x_0 as *mut LLVMValueRef , 1)
    };

    #[allow(clippy::zero_ptr)]
    unsafe {
        LLVMSetInitializer(global_var, arr);
        LLVMSetGlobalConstant(global_var, 1);

        LLVMDumpModule(module);

        LLVMLinkInMCJIT();
        LLVM_InitializeNativeTarget();
        //LLVM_InitializeNativeAsmParser();
        LLVM_InitializeNativeAsmPrinter();


        let mut ee = 0 as LLVMExecutionEngineRef;
        let mut error = 0 as *mut c_char;
        // note ownership passed to engine
        if LLVMCreateExecutionEngineForModule(&mut ee, module, &mut error) > 0 {
            panic!("Couldn't create execution engine")
        }

        //LLVMGetFunctionAddress(EE, Name)
        //LLVMGetGlobalValueAddress(EE, Name)
        let addr = LLVMGetGlobalValueAddress(ee, "dbl_arr\0".as_ptr() as *const c_char) as *const f64;
        println!("GlobalValue for dbl_arr: {:?} -> {}", addr, *addr);
    }

    // FIXME: cleanup

    //LLVMSetInitializer(GlobalVar, ConstantVal)
    //LLVMSetGlobalConstant(GlobalVar, IsConstant)
}




// LLVMRunFunction


// create module with simple fn to add 1

#[test]
pub fn test_llvm_module3() {
    // create a module with a simple function: (Int, Int) -> Int
    let context = unsafe {
        //LLVMGetGlobalContext()
        LLVMContextCreate()
    };

    let module = unsafe {
        LLVMModuleCreateWithNameInContext(
            "test_mod3\0".as_ptr() as *const c_char,
            context)
    };

    let int_ty = unsafe {
        LLVMIntTypeInContext(context, 32)
    };

    let arg_tys = [int_ty, int_ty].as_mut_ptr();

    let fun_ty = unsafe {
        LLVMFunctionType(int_ty, arg_tys, 2, 0)
    };

    let fun_val = unsafe {
        LLVMAddFunction(module, "f\0".as_ptr() as *const c_char, fun_ty)
    };

    // Add basic block to function
    let fun_bb = unsafe {
        LLVMAppendBasicBlockInContext(
            context,
            fun_val,
            "entry\0".as_ptr() as *const c_char)
    };

    // create builder & add instructions
    let builder = unsafe {
        LLVMCreateBuilderInContext(context)
    };

    unsafe {
        LLVMPositionBuilderAtEnd(builder, fun_bb);
        let arg0 = LLVMGetParam(fun_val, 0);
        let arg1 = LLVMGetParam(fun_val, 1);
        let tmp = LLVMBuildAdd(
            builder,
            arg0,
            arg1,
            "tmp\0".as_ptr() as *const c_char);
        llvm_sys::core::LLVMBuildRet(builder, tmp);
        LLVMDumpModule(module);
    }
    
    
    #[allow(clippy::zero_ptr)]
    unsafe {
        // Below should presumably only be done once per process/thread?
        LLVMLinkInMCJIT();
        LLVM_InitializeNativeTarget();
        //LLVM_InitializeNativeAsmParser();
        LLVM_InitializeNativeAsmPrinter();


        let mut ee = 0 as LLVMExecutionEngineRef;
        let mut error = 0 as *mut c_char;
        // note ownership passed to engine
        if LLVMCreateExecutionEngineForModule(&mut ee, module, &mut error) > 0 {
            panic!("Couldn't create execution engine")
        }

        let f_addr = LLVMGetFunctionAddress(ee, "f\0".as_ptr() as *const c_char);
        let f_ptr = f_addr as *const();
        let f: fn(i32, i32) -> i32 = std::mem::transmute(f_ptr);
        //let f: extern "C" fn(i32, i32) -> i32 = std::mem::transmute(f_ptr);
        //let f = f_addr as (unsafe extern "C" fn(i32, i32) -> i32);
        //let f = f_addr as extern "C" fn(i32, i32) -> i32;

        println!("Addr for f: {:?}", f_addr);
        let res = f(1, 2);
        println!("f(1,2) = : {:?}", res);

        //LLVMDisasmInstruction(DC, Bytes, BytesSize, PC, OutString, OutStringSize)
        //LLVMCreateDisasm(TripleName, DisInfo, TagType, GetOpInfo, SymbolLookUp)

        LLVMDumpModule(module);
    }

}


#[test]
pub fn test_llvm_module4() {
    // create a module with a global constant, read the constant..
    //let context = Context::get_global_context();
    let context = Context::new();

    let mut module = Module::create_with_name_in_context("test_mod4", &context);

    let double_ty = Type::double_type_in_context(&context);
    let array_double_ty = Type::array_type(&double_ty, 1);

    // FIXME: test array of arrays

    let x_0 = Constant::real(&double_ty, 1.61);
    let x_1 = Constant::real(&double_ty, 3.22);
    // FIXME: not sure if array takes ownership.
    // need to look at LLVM source and/or
    // investigate via explicit drop/dispose
    let mut arr = Constant::array(&double_ty, &[&x_0, &x_1]);

    let global_var =
        Global::add_to_module(&module, "dbl_arr", &array_double_ty);

    global_var.set_constant(&mut arr);

    module.dump();

    llvm_init();
    let ee = ExecutionEngine::for_module(&mut module);

    let addr =  ee.global_value_address("dbl_arr") as *const f64;
    unsafe {
        println!("GlobalValue for dbl_arr[0]: {:?} -> {}", addr, *addr);
        println!("GlobalValue for dbl_arr[1]: {:?} -> {}",
            addr.wrapping_add(1),
            *(addr.wrapping_add(1)));
        assert_eq!(1.61, *addr);
        assert_eq!(3.22, *(addr.wrapping_add(1)));
    }
}



#[test]
pub fn test_llvm_module5() {
    // create a module with a simple function: (Int, Int) -> Int

    let context = Context::new();
    let mut module = Module::create_with_name_in_context("test_mod5", &context);

    let int_ty = Type::int_type_in_context(&context, 32);
    let arg_tys = [&int_ty, &int_ty];
    let fun_ty = Type::function_type(&int_ty, &arg_tys);

    let fun_val = module.add_function("f", &fun_ty);

    // Add basic block to function
    let fun_bb = BasicBlock::append_basic_block_in_context(
        &context, &fun_val, "entry",
    );

    // // create builder & add instructions
    let builder = Builder::create_builder_in_context(&context);

    // call setName for each arg?

    builder.position_builder_at_end(&fun_bb);
    let arg0 = fun_val.get_param(0);
    let arg1 = fun_val.get_param(1);
    let tmp = builder.add(&arg0, &arg1, "tmp");
    let _ = builder.ret(&tmp);
    module.dump();

    llvm_init();

    let ee = ExecutionEngine::for_module(&mut module);
    let f_addr = ee.function_address("f");

    let f = unsafe {
        let f: extern "C" fn(i32, i32) -> i32 = std::mem::transmute(f_addr);
        f
    };

    println!("Addr for f: {:?}", f_addr);
    let res = f(1, 2);
    println!("f(1,2) = : {:?}", res);

    module.dump();
}


#[no_mangle]
pub extern "C" fn min_caml_test_add(a: i32, b: i32) -> i32 {
    a + b
}

#[no_mangle]
pub extern "C" fn min_caml_test_print_int(x: i32) {
    print!("{}", x)
}

#[no_mangle]
pub extern "C" fn min_caml_test_print_ln() {
    println!()
}

// Now targeting src/test/ml/print.ml as minimal first module

#[test]
pub fn test_llvm_callback() {
    let context = Context::new();
    let mut module = Module::create_with_name_in_context("test_mod6", &context);


    // Main function
    let void_ty = Type::void_type_in_context(&context);
    let int_ty = Type::int32_type_in_context(&context);
    let main_arg_tys = [&void_ty];
    let main_ty = Type::function_type(&int_ty, &main_arg_tys);

    let main_fun_val = module.add_function("main", &main_ty);

    // add global and set externally initialized
    
    let add_arg_tys = [&int_ty, &int_ty];
    let add_fun_ty = Type::function_type(&int_ty, &add_arg_tys);
    let add_fun_glbl = Global::add_to_module(&module, "add", &add_fun_ty);
    llvm::set_externally_initialized(&add_fun_glbl, true);
    let add_fun_val = Value::from_ref(add_fun_glbl.to_ref(), false);

    let print_ln_arg_tys = [];
    let print_ln_fun_ty = Type::function_type(&void_ty, &print_ln_arg_tys);
    let print_ln_fun_glbl = Global::add_to_module(
        &module,
        "print_ln",
        &print_ln_fun_ty
    );
    llvm::set_externally_initialized(&print_ln_fun_glbl, true);
    let print_ln_fun_val = Value::from_ref(print_ln_fun_glbl.to_ref(), false);

    let print_int_arg_tys = [&int_ty];
    let print_int_fun_ty = Type::function_type(&void_ty, &print_int_arg_tys);
    let print_int_fun_glbl = Global::add_to_module(
        &module,
        "print_int",
        &print_int_fun_ty
    );
    llvm::set_externally_initialized(&print_int_fun_glbl, true);
    let print_int_fun_val = Value::from_ref(print_int_fun_glbl.to_ref(), false);

    // Add basic block to main function
    let fun_bb = BasicBlock::append_basic_block_in_context(
        &context, &main_fun_val, "entry",
    );

    // // create builder & add instructions
    let builder = Builder::create_builder_in_context(&context);

    // call setName for each arg?

    builder.position_builder_at_end(&fun_bb);
    // let arg0 = main_fun_val.get_param(0);
    // let arg1 = main_fun_val.get_param(1);
    // let tmp = builder.add(&arg0, &arg1, "tmp");
    // let _ = builder.ret(&tmp);
    let zero_c = Constant::int(&int_ty, 0);
    let zero = Value::from_ref(zero_c.to_ref(), true);
    let _ = builder.call2(
        &print_ln_fun_ty,
        &print_ln_fun_val,
        &[],
        "");
    let _ = builder.call2(
        &print_int_fun_ty,
        &print_int_fun_val,
        &[&zero],
        "");
    let _ = builder.call2(
        &print_ln_fun_ty,
        &print_ln_fun_val,
        &[],
        "");
    let _ = builder.ret(&zero);

    println!();
    module.dump();


    llvm_init();

    // Create LLJIT
    let builder = LLJITBuilder::new();
    let jit = builder.create();

    // Create ExecutionSession
    //let es = jit.get_execution_session();

    //let ee = ExecutionEngine::for_module(&mut module);

    // Add local process symbols
    let add_addr = min_caml_test_add as *const core::ffi::c_void as u64;
    let print_ln_addr =
        min_caml_test_print_ln as *const core::ffi::c_void as u64;
    let print_int_addr =
        min_caml_test_print_int as *const core::ffi::c_void as u64;

    println!("Addr for min_caml_test_add: {:#X}", add_addr);
    println!("Addr for min_caml_test_print_ln: {:#X}", print_ln_addr);
    println!("Addr for min_caml_test_print_int: {:#X}", print_int_addr);

    let jd = jit.get_main_jit_dylib();
    jd.define(&jit, &[
        ("add", add_addr),
        ("print_ln", print_ln_addr),
        ("print_int", print_int_addr),
        ]);

    let add_addr_2 = jit.lookup("add");
    let print_ln_2 = jit.lookup("print_ln");
    let print_int_2 = jit.lookup("print_int");

    println!("Addr for `add` from JIT: {:#X}", add_addr_2);
    println!("Addr for `print_ln` from JIT: {:#X}", print_ln_2);
    println!("Addr for `print_int` from JIT: {:#X}", print_int_2);

    let mut tsc = ThreadSafeContext::new();
    let mut tsm = ThreadSafeModule::new(&module, &tsc);
    tsc.release(); // FIXME: above should take ownership
    //module.release();

    // Note: module.dump(); breaks here
    // ThreadSafeModule:new() takes ownership and leaves module in
    // uninitialised state.
    // Need to use LLVMOrcThreadSafeModuleWithModuleDo
    // to access the module

    let jd = jit.get_main_jit_dylib();
    jit.add_ir_module(&jd, &tsm);
    tsm.release(); // FIXME: above should take ownership

    let main_addr = jit.lookup("main");
    let main = unsafe {
        let main: extern "C" fn( () ) -> i32 = std::mem::transmute(main_addr);
        main
    };
    println!("Addr for main: {:#X}", main_addr);
    let res = main( () );
    println!("main( () ) = : {:?}", res);
}



#[test]
pub fn test_llvm_callback2() {

    let context = Context::new();
    let mut module = Module::create_with_name_in_context("test_mod7", &context);


    // Main function
    let void_ty = Type::void_type_in_context(&context);
    let int_ty = Type::int32_type_in_context(&context);
    let main_arg_tys = [&void_ty];
    let main_ty = Type::function_type(&int_ty, &main_arg_tys);

    let main_fun_val = module.add_function("main", &main_ty);

    // add global and use SetExternallyInitialized
    
    let add_arg_tys = [&int_ty, &int_ty];
    let add_fun_ty = Type::function_type(&int_ty, &add_arg_tys);
    let add_fun_glbl = Global::add_to_module(&module, "add", &add_fun_ty);
    llvm::set_externally_initialized(&add_fun_glbl, true);
    let add_fun_val = Value::from_ref(add_fun_glbl.to_ref(), false);

    let print_ln_arg_tys = [];
    let print_ln_fun_ty = Type::function_type(&void_ty, &print_ln_arg_tys);
    let print_ln_fun_glbl = Global::add_to_module(
        &module,
        "print_ln",
        &print_ln_fun_ty
    );
    llvm::set_externally_initialized(&print_ln_fun_glbl, true);
    let print_ln_fun_val = Value::from_ref(print_ln_fun_glbl.to_ref(), false);

    let print_int_arg_tys = [&int_ty];
    let print_int_fun_ty = Type::function_type(&void_ty, &print_int_arg_tys);
    let print_int_fun_glbl = Global::add_to_module(
        &module,
        "print_int",
        &print_int_fun_ty
    );
    llvm::set_externally_initialized(&print_int_fun_glbl, true);
    let print_int_fun_val = Value::from_ref(print_int_fun_glbl.to_ref(), false);

    // Add basic block to main function
    let fun_bb = BasicBlock::append_basic_block_in_context(
        &context, &main_fun_val, "entry",
    );

    // // create builder & add instructions
    let builder = Builder::create_builder_in_context(&context);

    // call setName for each arg?

    builder.position_builder_at_end(&fun_bb);
    let zero_c = Constant::int(&int_ty, 0);
    let zero = Value::from_ref(zero_c.to_ref(), true);
    let _ = builder.call2(
        &print_ln_fun_ty,
        &print_ln_fun_val,
        &[],
        "");
    let _ = builder.call2(
        &print_int_fun_ty,
        &print_int_fun_val,
        &[&zero],
        "");
    let _ = builder.call2(
        &print_ln_fun_ty,
        &print_ln_fun_val,
        &[],
        "");
    let _ = builder.ret(&zero);

    println!();
    module.dump();

    llvm_init();

    let ee = ExecutionEngine::for_module(&mut module);

    // Add local process symbols
    let add_ptr = min_caml_test_add as *const core::ffi::c_void;
    let print_ln_ptr = min_caml_test_print_ln as *const core::ffi::c_void;
    let print_int_ptr = min_caml_test_print_int as *const core::ffi::c_void;

    println!("Addr for min_caml_test_add: {:#X}", add_ptr as u64);
    println!("Addr for min_caml_test_print_ln: {:#X}", print_ln_ptr as u64);
    println!("Addr for min_caml_test_print_int: {:#X}", print_int_ptr as u64);

    llvm::add_symbol("add", add_ptr);
    llvm::add_symbol("print_ln", print_ln_ptr);
    llvm::add_symbol("print_int", print_int_ptr);

    // FIXME: try iterating over symbols
    //let add_addr_2 = ee.function_address("add");
    //let add_addr_2 = ee.global_value_address("add");
    // let print_ln_2 = jit.lookup("print_ln");
    // let print_int_2 = jit.lookup("print_int");

    //println!("Addr for `add` from JIT: {:#X}", add_addr_2);
    // println!("Addr for `print_ln` from JIT: {:#X}", print_ln_2);
    // println!("Addr for `print_int` from JIT: {:#X}", print_int_2);

    let main_addr = ee.function_address("main");
    let main = unsafe {
        let main: extern "C" fn( () ) -> i32 = std::mem::transmute(main_addr);
        main
    };
    println!("Addr for main: {:#X}", main_addr);
    let res = main( () );
    println!("main( () ) = : {:?}", res);

    module.dump();
}

#[test]
pub fn test_llvm_global_update() {
    let context = Context::new();
    let mut module = Module::create_with_name_in_context("test_mod8", &context);

    // Main function
    let void_ty = Type::void_type_in_context(&context);
    let int_ty = Type::int32_type_in_context(&context);
    let main_arg_tys = [&void_ty];
    let main_ty = Type::function_type(&int_ty, &main_arg_tys);

    let main_fun_val = module.add_function("main", &main_ty);

    // add global and use SetExternallyInitialized

    let mut mem = unsafe { libc::malloc(2 * 1024 * 1024) };

    let min_caml_hp = mem;
    let min_caml_hp_ptr = &mut mem as *mut *mut libc::c_void;
    eprintln!();
    eprintln!("min_caml_hp: {:#X}", min_caml_hp as u64);
    eprintln!("&min_caml_hp: {:#X}", min_caml_hp_ptr as u64);

    let void_ptr_ty = llvm::Type::pointer_type(&void_ty, 0);
    let min_caml_hp_glbl =
        llvm::Global::add_to_module(&module, "min_caml_hp", &void_ptr_ty);
    llvm::set_externally_initialized(&min_caml_hp_glbl, true);
    let min_caml_hp_val =
        llvm::Value::from_ref(min_caml_hp_glbl.to_ref(), false);

    // Add basic block to main function
    let fun_bb = BasicBlock::append_basic_block_in_context(
        &context, &main_fun_val, "entry",
    );

    // // create builder & add instructions
    let builder = Builder::create_builder_in_context(&context);

    // call setName for each arg?

    builder.position_builder_at_end(&fun_bb);
    let const_c = Constant::int(&int_ty, 8);
    let c_val = Value::from_ref(const_c.to_ref(), true);

    let val0 = builder.load2(&void_ptr_ty, &min_caml_hp_val, "");
    let val1 = builder.add(&val0, &c_val, "");
    let val2 = builder.store(&val1, &min_caml_hp_val);
    let val3 = builder.load2(&void_ptr_ty, &min_caml_hp_val, "");
    let val4 = builder.add(&val3, &c_val, "");
    let val5 = builder.store(&val4, &min_caml_hp_val);
    let _ = builder.ret(&c_val);

    println!();
    module.dump();

    llvm_init();

    let ee = ExecutionEngine::for_module(&mut module);

    llvm::add_symbol("min_caml_hp", min_caml_hp_ptr as *const libc::c_void);

    let main_addr = ee.function_address("main");
    let main = unsafe {
        let main: extern "C" fn( () ) -> i32 = std::mem::transmute(main_addr);
        main
    };
    println!("Addr for main: {:#X}", main_addr);
    let res = main( () );
    println!("main( () ) = : {:?}", res);

    eprintln!("min_caml_hp: {:#X}", min_caml_hp as u64);
    eprintln!("min_caml_hp_ptr: {:#X}", min_caml_hp_ptr as u64);
    let x = unsafe { *min_caml_hp_ptr };
    eprintln!("*min_caml_hp_ptr: {:#X}", x as u64);

    //module.dump();
}