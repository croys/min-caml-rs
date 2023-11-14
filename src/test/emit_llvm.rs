extern crate llvm_sys;


//use llvm_sys::core::{LLVMConstInt};
use llvm_sys::core::{LLVMModuleCreateWithNameInContext, LLVMAddGlobal, LLVMDoubleTypeInContext, LLVMArrayType, LLVMSetGlobalConstant, LLVMSetInitializer, LLVMDumpModule, LLVMIntTypeInContext, LLVMFunctionType, LLVMAddFunction, LLVMAppendBasicBlockInContext, LLVMCreateBuilderInContext, LLVMPositionBuilderAtEnd, LLVMGetParam, LLVMBuildAdd};
use llvm_sys::core::{LLVMGetGlobalContext};
//use llvm_sys::core::{LLVMModuleCreateWithName};
//use llvm_sys::disassembler::{LLVMCreateDisasm, LLVMDisasmInstruction};
use llvm_sys::execution_engine::{LLVMExecutionEngineRef, LLVMCreateExecutionEngineForModule, LLVMLinkInMCJIT, LLVMGetFunctionAddress, LLVMGetGlobalValueAddress};
//use llvm_sys::prelude::LLVMBool;
use llvm_sys::core::{LLVMConstReal, LLVMConstArray};
use llvm_sys::prelude::{LLVMValueRef};
//use llvm_sys::LLVMValue;

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
        LLVMGetGlobalContext()
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
        let f: extern "C" fn(i32, i32) -> i32 = std::mem::transmute(f_ptr);
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

