//#![allow(clippy::upper_case_acronyms)]
#![allow(dead_code)]

use crate::asm;

extern crate llvm_sys;

//use llvm_sys::core::{LLVMGetGlobalContext, LLVMConstInt, LLVMDoubleTypeInContext, LLVMDumpValue, LLVMPrintValueToString, LLVMDisposeMessage, LLVMGetValueName2, LLVMConstArray, LLVMAddGlobal, LLVMModuleCreateWithNameInContext, LLVMContextDispose};
use llvm_sys::core::{
    LLVMAddGlobal, LLVMConstArray, LLVMConstReal, LLVMContextCreate,
    LLVMContextDispose, LLVMDisposeMessage, LLVMDisposeModule, LLVMDumpModule,
    LLVMDumpValue, LLVMGetGlobalContext, LLVMGetValueName2,
    LLVMModuleCreateWithNameInContext, LLVMPrintValueToString,
    LLVMSetGlobalConstant, LLVMSetInitializer,
};
use llvm_sys::execution_engine::LLVMCreateExecutionEngineForModule;
use llvm_sys::execution_engine::{
    LLVMExecutionEngineRef, LLVMGetGlobalValueAddress,
};
//use llvm_sys::execution_engine::{LLVMGetFunctionAddress, LLVMGetGlobalValueAddress, LLVMAddModule};
//use llvm_sys::object::LLVMGetSymbolAddress;
//use llvm_sys::prelude::{/* LLVMBuilderRef, */ LLVMValueRef};
use llvm_sys::prelude::LLVMValueRef;
use llvm_sys::prelude::{LLVMContextRef, LLVMModuleRef, LLVMTypeRef};

//use llvm_sys::LLVMModule;

use llvm_sys::core::{
    LLVMArrayType, LLVMDoubleType, LLVMDoubleTypeInContext, LLVMIntType,
    LLVMIntTypeInContext,
};

use libc::c_char;
use std::ffi::CString;

// note: all classes should have ability to release/disown child objects

pub trait RefOwner<Ref> {
    fn to_ref(&self) -> Ref;
    fn release(&mut self);
}

// FIXME: wrap up context

pub struct Context {
    context: LLVMContextRef,
    owned: bool,
}

impl Drop for Context {
    fn drop(&mut self) {
        if self.owned {
            unsafe {
                LLVMContextDispose(self.context);
            }
            self.owned = false;
        }
    }
}

impl RefOwner<LLVMContextRef> for Context {
    // FIXME: to_mut_ref?
    fn to_ref(&self) -> LLVMContextRef {
        self.context
    }

    fn release(&mut self) {
        self.owned = false
    }
}

impl Context {
    pub fn new() -> Context {
        let c = unsafe { LLVMContextCreate() };
        Context {
            context: c,
            owned: false,
        }
    }

    pub fn get_global_context() -> Context {
        let c = unsafe { LLVMGetGlobalContext() };
        Context {
            context: c,
            owned: false,
        }
    }
}

pub struct Module {
    name: CString, // FIXME: do we need to keep this?
    module: LLVMModuleRef,
    owned: bool,
}

impl Drop for Module {
    fn drop(&mut self) {
        if self.owned {
            unsafe {
                LLVMDisposeModule(self.module);
            }
            self.owned = false;
        }
    }
}

impl RefOwner<LLVMModuleRef> for Module {
    fn to_ref(&self) -> LLVMModuleRef {
        self.module
    }

    fn release(&mut self) {
        self.owned = false
    }
}

impl Module {
    pub fn create_with_name_in_context(name: &str, ctx: &Context) -> Module {
        let name_ = CString::new(name).unwrap();
        let m = unsafe {
            LLVMModuleCreateWithNameInContext(
                name_.as_ptr() as *const c_char,
                ctx.to_ref(),
            )
        };
        Module {
            name: name_,
            module: m,
            owned: false,
        }
    }

    // add global

    // dump
    pub fn dump(&self) {
        unsafe {
            LLVMDumpModule(self.module);
        }
    }
}

pub struct Type {
    ty: LLVMTypeRef,
    owned: bool,
}

impl RefOwner<LLVMTypeRef> for Type {
    fn to_ref(&self) -> LLVMTypeRef {
        self.ty
    }

    // FIXME: push up
    fn release(&mut self) {
        self.owned = false
    }
}

impl Type {
    pub fn int_type(size: u32) -> Type {
        let ty = unsafe { LLVMIntType(size) };
        Type { ty, owned: true }
    }

    pub fn double_type() -> Type {
        let ty = unsafe { LLVMDoubleType() };
        Type { ty, owned: true }
    }

    // shares context of element type?
    pub fn array_type(elem_ty: &Type, size: u32) -> Type {
        let ty = unsafe { LLVMArrayType(elem_ty.to_ref(), size) };
        Type {
            ty,
            owned: elem_ty.owned,
        }
    }

    // FIXME: tuple types

    // FIXME: for tuples:
    //LLVMStructTypeInContext(C, ElementTypes, ElementCount, Packed)

    // FIXME: function types

    // Might want to separate these out to another class
    pub fn int_type_in_context(ctx: &Context, size: u32) -> Type {
        let ty = unsafe { LLVMIntTypeInContext(ctx.to_ref(), size) };
        Type { ty, owned: false }
    }

    pub fn double_type_in_context(ctx: &Context) -> Type {
        let ty = unsafe { LLVMDoubleTypeInContext(ctx.to_ref()) };
        Type { ty, owned: false }
    }
}

// FIXME: ty::Type -> LLVM type conversion

pub struct Constant {
    val: LLVMValueRef,
    owned: bool,
}

// ValueRefOwner sub-trait?

impl RefOwner<LLVMValueRef> for Constant {
    fn to_ref(&self) -> LLVMValueRef {
        self.val
    }

    // FIXME: push up
    fn release(&mut self) {
        self.owned = false
    }
}

impl Constant {
    pub fn real(ty: &Type, x: f64) -> Constant {
        let val = unsafe { LLVMConstReal(ty.to_ref(), x) };
        Constant { val, owned: true }
    }

    pub fn array(elem_ty: &Type, vals: &[&Constant]) -> Constant {
        // crete vec of poitners
        let n: libc::c_uint = vals.len().try_into().unwrap();
        let mut v: Vec<LLVMValueRef> =
            vals.iter().map(|x| x.to_ref()).collect();
        let val =
            unsafe { LLVMConstArray(elem_ty.to_ref(), v.as_mut_ptr(), n) };
        Constant { val, owned: true }
    }
}

// FIXME: globals

pub struct Global {
    val: LLVMValueRef,
    owned: bool,
}

impl RefOwner<LLVMValueRef> for Global {
    fn to_ref(&self) -> LLVMValueRef {
        self.val
    }

    // FIXME: push up
    fn release(&mut self) {
        self.owned = false
    }
}

impl Global {
    pub fn add_to_module(m: &Module, name: &str, ty: &Type) -> Global {
        let name_ = CString::new(name).unwrap();
        let val = unsafe {
            LLVMAddGlobal(
                m.to_ref(),
                ty.to_ref(),
                name_.as_ptr() as *const c_char,
            )
        };
        Global { val, owned: true }
    }

    pub fn set_constant(&self, val: &Constant) {
        unsafe {
            LLVMSetInitializer(self.to_ref(), val.to_ref());
            LLVMSetGlobalConstant(self.to_ref(), 1);
        }
    }
}

// FIXME: wrap up function

// FIXME: wrup up basic block

// FIXME: instructions

pub struct ExecutionEngine {
    engine: LLVMExecutionEngineRef,
    owned: bool,
}

impl RefOwner<LLVMExecutionEngineRef> for ExecutionEngine {
    fn to_ref(&self) -> LLVMExecutionEngineRef {
        self.engine
    }

    // FIXME: push up
    fn release(&mut self) {
        self.owned = false
    }
}

impl ExecutionEngine {
    // FIXME: return Error
    pub fn for_module(m: &mut Module) -> ExecutionEngine {
        #[allow(clippy::zero_ptr)]
        unsafe {
            let mut ee = 0 as LLVMExecutionEngineRef;
            let mut error = 0 as *mut c_char;
            if LLVMCreateExecutionEngineForModule(
                &mut ee,
                m.to_ref(),
                &mut error,
            ) > 0
            {
                // FIXME: details
                panic!("Couldn't create execution engine!")
            } else {
                m.release();
                ExecutionEngine {
                    engine: ee,
                    owned: true,
                }
            }
        }
    }

    pub fn global_value_address(&self, name: &str) -> u64 {
        let name_ = CString::new(name).unwrap();
        unsafe {
            LLVMGetGlobalValueAddress(
                self.to_ref(),
                name_.as_ptr() as *const c_char,
            )
        }
    }
}

#[allow(unused_variables)]
pub fn f(p: &asm::Prog) {
    let asm::Prog::Prog(floats, fds, c) = p;

    //let context = unsafe { LLVMGetGlobalContext() };

    // Create constants for doubles

    //LLVMConstInt(IntTy, N, SignExtend)

    let ty = unsafe {
        llvm_sys::core::LLVMDoubleType()
        // LLVMDoubleTypeInContext(context)
    };

    for (ref id, ref x) in floats {
        println!("Adding float const: {} = {}", id.0, x);

        let val = unsafe { llvm_sys::core::LLVMConstReal(ty, *x) };

        unsafe {
            let name = std::ffi::CString::new(id.0.as_str())
                .expect("unable to create id");
            // llvm_sys::core::LLVMSetValueName2(
            //     val, id.0.as_ptr() as *const i8, 8);
            llvm_sys::core::LLVMSetValueName2(
                val,
                name.into_raw() as *const i8,
                8,
            );
            LLVMDumpValue(val);
            println!();
        }

        let str = unsafe {
            let buf = LLVMPrintValueToString(val);
            let cstr_buf = std::ffi::CStr::from_ptr(buf);
            let res = String::from_utf8_lossy(cstr_buf.to_bytes()).into_owned();
            LLVMDisposeMessage(buf);
            res
        };

        let name = unsafe {
            let mut sz: usize = 128;
            let buf = LLVMGetValueName2(val, &mut sz);
            let cstr_buf = std::ffi::CStr::from_ptr(buf);
            let res = String::from_utf8_lossy(cstr_buf.to_bytes()).into_owned();
            //LLVMDisposeMessage(buf);
            res
        };
        println!("str = {}, name = {}", str, name);
    }
    // labels

    //LLVMConstReal(ty, val);

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

    //LLVMAddModule(EE, M)
    //LLVMModuleCreateWithNameInContext(ModuleID, C)

    // GenericValue
}
