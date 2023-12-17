//#![allow(clippy::upper_case_acronyms)]
#![allow(dead_code)]
#![allow(unused_variables)] // FIXME
#![allow(unused_imports)] // FIXME

use crate::asm;
use crate::closure;
use crate::id;
use crate::r#virtual;
use crate::ty;

extern crate llvm_sys;

use libc::RTEXT_FILTER_BRVLAN;
use llvm_sys::core::LLVMAddAlias2;
use llvm_sys::core::LLVMAddFunction;
use llvm_sys::core::LLVMAddIncoming;
use llvm_sys::core::LLVMAppendBasicBlockInContext;
use llvm_sys::core::LLVMAppendExistingBasicBlock;
use llvm_sys::core::LLVMBuildBitCast;
use llvm_sys::core::LLVMBuildBr;
use llvm_sys::core::LLVMBuildCall2;
use llvm_sys::core::LLVMBuildCondBr;
use llvm_sys::core::LLVMBuildFCmp;
use llvm_sys::core::LLVMBuildICmp;
use llvm_sys::core::LLVMBuildLoad2;
use llvm_sys::core::LLVMBuildMul;
use llvm_sys::core::LLVMBuildNUWAdd;
use llvm_sys::core::LLVMBuildNeg;
use llvm_sys::core::LLVMBuildPhi;
use llvm_sys::core::LLVMBuildStore;
use llvm_sys::core::LLVMCreateBasicBlockInContext;
use llvm_sys::core::LLVMCreateBuilderInContext;
use llvm_sys::core::LLVMDumpType;
use llvm_sys::core::LLVMGetInstructionOpcode;
use llvm_sys::core::LLVMGetLastBasicBlock;
use llvm_sys::core::LLVMGetLastInstruction;
use llvm_sys::core::LLVMPointerType;
use llvm_sys::core::LLVMPositionBuilderAtEnd;
use llvm_sys::core::LLVMSetAlignment;
use llvm_sys::core::LLVMVoidType;
use llvm_sys::core::LLVMVoidTypeInContext;
use llvm_sys::core::{
    LLVMAddGlobal, LLVMBuildAdd, LLVMBuildFAdd, LLVMBuildFDiv, LLVMBuildFMul,
    LLVMBuildFNeg, LLVMBuildFSub, LLVMBuildRet, LLVMBuildRetVoid, LLVMBuildSub,
    LLVMConstArray, LLVMConstInt, LLVMConstReal, LLVMContextCreate,
    LLVMContextDispose, LLVMDisposeMessage, LLVMDisposeModule, LLVMDumpModule,
    LLVMDumpValue, LLVMFunctionType, LLVMGetGlobalContext, LLVMGetParam,
    LLVMGetValueName2, LLVMModuleCreateWithNameInContext,
    LLVMPrintValueToString, LLVMSetExternallyInitialized,
    LLVMSetGlobalConstant, LLVMSetInitializer,
};
use llvm_sys::error::LLVMErrorRef;
use llvm_sys::error::LLVMErrorSuccess;
use llvm_sys::error::LLVMGetErrorMessage;
use llvm_sys::execution_engine::LLVMCreateExecutionEngineForModule;
use llvm_sys::execution_engine::{
    LLVMExecutionEngineRef, LLVMGetFunctionAddress, LLVMGetGlobalValueAddress,
    LLVMLinkInMCJIT,
};
use llvm_sys::orc2::lljit::LLVMOrcCreateLLJIT;
use llvm_sys::orc2::lljit::LLVMOrcCreateLLJITBuilder;
use llvm_sys::orc2::lljit::LLVMOrcLLJITAddLLVMIRModule;
use llvm_sys::orc2::lljit::LLVMOrcLLJITBuilderRef;
use llvm_sys::orc2::lljit::LLVMOrcLLJITGetExecutionSession;
use llvm_sys::orc2::lljit::LLVMOrcLLJITGetMainJITDylib;
use llvm_sys::orc2::lljit::LLVMOrcLLJITLookup;
use llvm_sys::orc2::lljit::LLVMOrcLLJITMangleAndIntern;
use llvm_sys::orc2::lljit::LLVMOrcLLJITRef;
use llvm_sys::orc2::LLVMJITCSymbolMapPair;
use llvm_sys::orc2::LLVMJITEvaluatedSymbol;
use llvm_sys::orc2::LLVMJITSymbolFlags;
use llvm_sys::orc2::LLVMOrcAbsoluteSymbols;
use llvm_sys::orc2::LLVMOrcCreateNewThreadSafeContext;
use llvm_sys::orc2::LLVMOrcCreateNewThreadSafeModule;
use llvm_sys::orc2::LLVMOrcDisposeThreadSafeContext;
use llvm_sys::orc2::LLVMOrcDisposeThreadSafeModule;
use llvm_sys::LLVMIntPredicate;
use llvm_sys::LLVMRealPredicate;
//use llvm_sys::orc2::LLVMOrcExecutionSessionCreateJITDylib;
use llvm_sys::orc2::LLVMOrcExecutionSessionRef;
use llvm_sys::orc2::LLVMOrcExecutorAddress;
use llvm_sys::orc2::LLVMOrcJITDylibDefine;
use llvm_sys::orc2::LLVMOrcJITDylibRef;
//use llvm_sys::orc2::LLVMOrcSymbolLookupFlags;
use llvm_sys::orc2::LLVMOrcThreadSafeContextRef;
use llvm_sys::orc2::LLVMOrcThreadSafeModuleRef;
//use llvm_sys::orc2::LLVMOrcThreadSafeModuleWithModuleDo;
use llvm_sys::prelude::LLVMBasicBlockRef;
use llvm_sys::prelude::LLVMBuilderRef;
use llvm_sys::prelude::LLVMValueRef;
use llvm_sys::prelude::{LLVMContextRef, LLVMModuleRef, LLVMTypeRef};

use llvm_sys::support::LLVMAddSymbol;
use llvm_sys::target::LLVM_InitializeNativeAsmPrinter;
use llvm_sys::target::LLVM_InitializeNativeTarget;

use llvm_sys::core::{
    LLVMArrayType, LLVMDoubleType, LLVMDoubleTypeInContext, LLVMInt32Type,
    LLVMInt32TypeInContext, LLVMInt64Type, LLVMInt64TypeInContext, LLVMIntType,
    LLVMIntTypeInContext,
};

use libc::c_char;
use std::collections::HashMap;
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

    // module should probably be mut
    pub fn add_global(&mut self, name: &str, ty: &Type) -> Value {
        let name_ = CString::new(name).unwrap();
        let val = unsafe {
            LLVMAddGlobal(
                self.to_ref(),
                ty.to_ref(),
                name_.as_ptr() as *const c_char,
            )
        };
        Value { val, owned: false }
    }

    pub fn add_function(&mut self, name: &str, ty: &Type) -> Value {
        let name_ = CString::new(name).unwrap();
        let fun = unsafe {
            LLVMAddFunction(self.to_ref(), name_.as_ptr(), ty.to_ref())
        };
        Value::from_ref(fun, false)
    }

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
    pub fn new(ty: LLVMTypeRef, owned: bool) -> Type {
        Type { ty, owned }
    }

    pub fn void_type() -> Type {
        let ty = unsafe { LLVMVoidType() };
        Type { ty, owned: true }
    }

    pub fn int_type(size: u32) -> Type {
        let ty = unsafe { LLVMIntType(size) };
        Type { ty, owned: true }
    }

    pub fn int32_type() -> Type {
        let ty = unsafe { LLVMInt32Type() };
        Type { ty, owned: true }
    }

    pub fn int64_type() -> Type {
        let ty = unsafe { LLVMInt64Type() };
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

    pub fn pointer_type(elem_ty: &Type, addr_space: u32) -> Type {
        let ty = unsafe { LLVMPointerType(elem_ty.to_ref(), addr_space) };
        Type {
            ty,
            owned: elem_ty.owned,
        }
    }

    // FIXME: tuple types

    // FIXME: for tuples:
    //LLVMStructTypeInContext(C, ElementTypes, ElementCount, Packed)

    // FIXME: function types
    pub fn function_type(res_ty: &Type, arg_tys: &[&Type]) -> Type {
        // FIXME: ownership?
        let mut arg_tys2: Vec<LLVMTypeRef> =
            arg_tys.iter().map(|ty| ty.to_ref()).collect();
        let n: libc::c_uint = arg_tys2.len().try_into().unwrap();
        let ty = unsafe {
            LLVMFunctionType(res_ty.to_ref(), arg_tys2.as_mut_ptr(), n, 0)
        };
        Type {
            ty,
            owned: res_ty.owned,
        }
    }

    // Might want to separate these out to another class

    pub fn void_type_in_context(ctx: &Context) -> Type {
        let ty = unsafe { LLVMVoidTypeInContext(ctx.to_ref()) };
        Type { ty, owned: true }
    }

    pub fn int_type_in_context(ctx: &Context, size: u32) -> Type {
        let ty = unsafe { LLVMIntTypeInContext(ctx.to_ref(), size) };
        Type { ty, owned: false }
    }

    pub fn int32_type_in_context(ctx: &Context) -> Type {
        let ty = unsafe { LLVMInt32TypeInContext(ctx.to_ref()) };
        Type { ty, owned: false }
    }

    pub fn int64_type_in_context(ctx: &Context) -> Type {
        let ty = unsafe { LLVMInt64TypeInContext(ctx.to_ref()) };
        Type { ty, owned: false }
    }

    pub fn double_type_in_context(ctx: &Context) -> Type {
        let ty = unsafe { LLVMDoubleTypeInContext(ctx.to_ref()) };
        Type { ty, owned: false }
    }

    pub fn dump(&self) {
        unsafe { LLVMDumpType(self.to_ref()) }
    }
}

pub struct Value {
    val: LLVMValueRef,
    owned: bool,
}

// ValueRefOwner sub-trait?

impl RefOwner<LLVMValueRef> for Value {
    fn to_ref(&self) -> LLVMValueRef {
        self.val
    }

    // FIXME: push up
    // FIXME: return self
    fn release(&mut self) {
        self.owned = false
    }
}

impl Value {
    pub fn from_ref(val: LLVMValueRef, owned: bool) -> Value {
        Value { val, owned }
    }

    // This is only valid for Functions, have a FunValue sub-class?
    pub fn get_param(&self, n: usize) -> Value {
        let n_: libc::c_uint = n.try_into().unwrap();
        let val = unsafe { LLVMGetParam(self.to_ref(), n_) };
        Value { val, owned: false }
    }

    pub fn set_name(&self, name: &str) {
        let name_ = std::ffi::CString::new(name).expect("unable to create id");
        unsafe {
            llvm_sys::core::LLVMSetValueName2(
                self.val,
                name_.as_ptr(),
                name.len(),
            )
        }
    }

    pub fn get_name(&self) -> Option<String> {
        unsafe {
            let mut sz = 0;
            let buf = LLVMGetValueName2(self.to_ref(), &mut sz);
            if buf.is_null() {
                None
            } else {
                let cstr = std::ffi::CStr::from_ptr(buf);
                Some(String::from_utf8_lossy(cstr.to_bytes()).into_owned())
            }
        }
    }

    // Should maybe consider internal mutability for owned?
    pub fn set_constant(&self, constant: &mut Value) {
        constant.release();
        unsafe {
            LLVMSetInitializer(self.to_ref(), constant.to_ref());
            LLVMSetGlobalConstant(self.to_ref(), 1);
        }
    }

    pub fn dump(&self) {
        unsafe { LLVMDumpValue(self.val) }
    }
}

pub mod constant {
    use llvm_sys::core::{LLVMConstPointerNull, LLVMIsAConstantPointerNull};

    use super::*;

    pub fn int(ty: &Type, x: i32) -> Value {
        let val = unsafe { LLVMConstInt(ty.to_ref(), x as u64, 1) };
        Value::from_ref(val, true)
    }

    pub fn real(ty: &Type, x: f64) -> Value {
        let val = unsafe { LLVMConstReal(ty.to_ref(), x) };
        Value::from_ref(val, true)
    }

    pub fn array(elem_ty: &Type, vals: &[&Value]) -> Value {
        // crete vec of pointers
        let n: libc::c_uint = vals.len().try_into().unwrap();
        let mut v: Vec<LLVMValueRef> =
            vals.iter().map(|x| x.to_ref()).collect();
        let val =
            unsafe { LLVMConstArray(elem_ty.to_ref(), v.as_mut_ptr(), n) };
        Value::from_ref(val, true)
    }

    pub fn pointer_null(ptr_ty: &Type) -> Value {
        let val = unsafe { LLVMConstPointerNull(ptr_ty.to_ref()) };
        Value::from_ref(val, true)
    }
}

pub struct BasicBlock {
    val: LLVMBasicBlockRef,
    owned: bool,
}

impl RefOwner<LLVMBasicBlockRef> for BasicBlock {
    fn to_ref(&self) -> LLVMBasicBlockRef {
        self.val
    }

    // FIXME: push up
    // FIXME: return self
    fn release(&mut self) {
        self.owned = false
    }
}

impl BasicBlock {
    pub fn append_basic_block_in_context(
        ctx: &Context,
        fun: &Value,
        name: &str,
    ) -> BasicBlock {
        let name_ = CString::new(name).unwrap();
        let bb = unsafe {
            LLVMAppendBasicBlockInContext(
                ctx.to_ref(),
                fun.to_ref(),
                name_.as_ptr(),
            )
        };
        BasicBlock {
            val: bb,
            owned: false,
        }
    }

    pub fn create_basic_block_in_context(
        ctx: &Context,
        name: &str,
    ) -> BasicBlock {
        let name_ = CString::new(name).unwrap();
        let bb = unsafe {
            LLVMCreateBasicBlockInContext(ctx.to_ref(), name_.as_ptr())
        };
        BasicBlock {
            val: bb,
            owned: false,
        }
    }

    pub fn append(&self, fun: &Value) {
        unsafe { LLVMAppendExistingBasicBlock(fun.to_ref(), self.to_ref()) }
    }

    // get the basic block
    pub fn get_last(fun: &Value) -> BasicBlock {
        let bb = unsafe { LLVMGetLastBasicBlock(fun.to_ref()) };
        BasicBlock {
            val: bb,
            owned: false,
        }
    }

    pub fn is_terminated(&self) -> bool {
        unsafe {
            let val = LLVMGetLastInstruction(self.to_ref());
            let op = LLVMGetInstructionOpcode(val);
            matches!(
                op,
                llvm_sys::LLVMOpcode::LLVMBr | llvm_sys::LLVMOpcode::LLVMRet
            )
        }
    }
}

// Builder

pub struct Builder {
    val: LLVMBuilderRef,
    owned: bool,
}

impl RefOwner<LLVMBuilderRef> for Builder {
    fn to_ref(&self) -> LLVMBuilderRef {
        self.val
    }

    // FIXME: push up
    // FIXME: return self
    fn release(&mut self) {
        self.owned = false
    }
}

impl Builder {
    pub fn create_builder_in_context(ctx: &Context) -> Builder {
        let builder = unsafe { LLVMCreateBuilderInContext(ctx.to_ref()) };
        Builder {
            val: builder,
            owned: false,
        }
    }

    pub fn position_builder_at_end(&self, bb: &BasicBlock) {
        unsafe { LLVMPositionBuilderAtEnd(self.to_ref(), bb.to_ref()) }
    }

    pub fn neg(&self, val: &Value, name: &str) -> Value {
        let name_ = CString::new(name).unwrap();
        let val = unsafe {
            LLVMBuildNeg(
                self.to_ref(),
                val.to_ref(),
                name_.as_ptr() as *const c_char,
            )
        };
        Value { val, owned: false }
    }

    pub fn add(&self, lhs: &Value, rhs: &Value, name: &str) -> Value {
        let name_ = CString::new(name).unwrap();
        let val = unsafe {
            LLVMBuildAdd(
                self.to_ref(),
                lhs.to_ref(),
                rhs.to_ref(),
                name_.as_ptr() as *const c_char,
            )
        };
        Value { val, owned: false }
    }

    pub fn add_nuw(&self, lhs: &Value, rhs: &Value, name: &str) -> Value {
        let name_ = CString::new(name).unwrap();
        let val = unsafe {
            LLVMBuildNUWAdd(
                self.to_ref(),
                lhs.to_ref(),
                rhs.to_ref(),
                name_.as_ptr() as *const c_char,
            )
        };
        Value { val, owned: false }
    }

    pub fn sub(&self, lhs: &Value, rhs: &Value, name: &str) -> Value {
        let name_ = CString::new(name).unwrap();
        let val = unsafe {
            LLVMBuildSub(
                self.to_ref(),
                lhs.to_ref(),
                rhs.to_ref(),
                name_.as_ptr() as *const c_char,
            )
        };
        Value { val, owned: false }
    }

    pub fn mul(&self, lhs: &Value, rhs: &Value, name: &str) -> Value {
        let name_ = CString::new(name).unwrap();
        let val = unsafe {
            LLVMBuildMul(
                self.to_ref(),
                lhs.to_ref(),
                rhs.to_ref(),
                name_.as_ptr() as *const c_char,
            )
        };
        Value { val, owned: false }
    }

    pub fn fadd(&self, lhs: &Value, rhs: &Value, name: &str) -> Value {
        let name_ = CString::new(name).unwrap();
        let val = unsafe {
            LLVMBuildFAdd(
                self.to_ref(),
                lhs.to_ref(),
                rhs.to_ref(),
                name_.as_ptr() as *const c_char,
            )
        };
        Value { val, owned: false }
    }

    pub fn fsub(&self, lhs: &Value, rhs: &Value, name: &str) -> Value {
        let name_ = CString::new(name).unwrap();
        let val = unsafe {
            LLVMBuildFSub(
                self.to_ref(),
                lhs.to_ref(),
                rhs.to_ref(),
                name_.as_ptr() as *const c_char,
            )
        };
        Value { val, owned: false }
    }

    pub fn fmul(&self, lhs: &Value, rhs: &Value, name: &str) -> Value {
        let name_ = CString::new(name).unwrap();
        let val = unsafe {
            LLVMBuildFMul(
                self.to_ref(),
                lhs.to_ref(),
                rhs.to_ref(),
                name_.as_ptr() as *const c_char,
            )
        };
        Value { val, owned: false }
    }

    pub fn fdiv(&self, lhs: &Value, rhs: &Value, name: &str) -> Value {
        let name_ = CString::new(name).unwrap();
        let val = unsafe {
            LLVMBuildFDiv(
                self.to_ref(),
                lhs.to_ref(),
                rhs.to_ref(),
                name_.as_ptr() as *const c_char,
            )
        };
        Value { val, owned: false }
    }

    pub fn fneg(&self, val: &Value, name: &str) -> Value {
        let name_ = CString::new(name).unwrap();
        let val = unsafe {
            LLVMBuildFNeg(
                self.to_ref(),
                val.to_ref(),
                name_.as_ptr() as *const c_char,
            )
        };
        Value { val, owned: false }
    }

    pub fn ret(&self, val: &Value) -> Value {
        let val = unsafe { LLVMBuildRet(self.to_ref(), val.to_ref()) };
        Value { val, owned: false }
    }

    pub fn ret_void(&self) -> Value {
        let val = unsafe { LLVMBuildRetVoid(self.to_ref()) };
        Value { val, owned: false }
    }

    pub fn call2(
        &self,
        ty: &Type,
        fun: &Value,
        args: &[&Value],
        name: &str,
    ) -> Value {
        let name_ = CString::new(name).unwrap();
        let mut args_: Vec<LLVMValueRef> =
            args.iter().map(|x| x.to_ref()).collect();
        let n: libc::c_uint = args_.len().try_into().unwrap();
        let val = unsafe {
            LLVMBuildCall2(
                self.to_ref(),
                ty.to_ref(),
                fun.to_ref(),
                args_.as_mut_ptr(),
                n,
                name_.as_ptr(),
            )
        };
        Value { val, owned: false }
    }

    pub fn load2(&self, ty: &Type, ptr: &Value, name: &str) -> Value {
        let name_ = CString::new(name).unwrap();
        let val = unsafe {
            LLVMBuildLoad2(
                self.to_ref(),
                ty.to_ref(),
                ptr.to_ref(),
                name_.as_ptr() as *const c_char,
            )
        };
        //unsafe { LLVMSetAlignment(val, 4) };
        Value { val, owned: false }
    }

    pub fn store(&self, val: &Value, ptr: &Value) -> Value {
        let val = unsafe {
            LLVMBuildStore(self.to_ref(), val.to_ref(), ptr.to_ref())
        };
        Value { val, owned: false }
    }

    pub fn bitcast(&self, val: &Value, ty: &Type, name: &str) -> Value {
        let name_ = CString::new(name).unwrap();
        let val = unsafe {
            LLVMBuildBitCast(
                self.to_ref(),
                val.to_ref(),
                ty.to_ref(),
                name_.as_ptr() as *const c_char,
            )
        };
        Value { val, owned: false }
    }

    fn icmp(
        &self,
        pred: LLVMIntPredicate,
        lhs: &Value,
        rhs: &Value,
        name: &str,
    ) -> Value {
        let name_ = CString::new(name).unwrap();
        let val = unsafe {
            LLVMBuildICmp(
                self.to_ref(),
                pred,
                lhs.to_ref(),
                rhs.to_ref(),
                name_.as_ptr() as *const c_char,
            )
        };
        Value { val, owned: false }
    }

    pub fn icmp_eq(&self, lhs: &Value, rhs: &Value, name: &str) -> Value {
        self.icmp(LLVMIntPredicate::LLVMIntEQ, lhs, rhs, name)
    }

    pub fn icmp_le(&self, lhs: &Value, rhs: &Value, name: &str) -> Value {
        self.icmp(LLVMIntPredicate::LLVMIntSLE, lhs, rhs, name)
    }

    pub fn icmp_ge(&self, lhs: &Value, rhs: &Value, name: &str) -> Value {
        self.icmp(LLVMIntPredicate::LLVMIntSGE, lhs, rhs, name)
    }

    pub fn icmp_lt(&self, lhs: &Value, rhs: &Value, name: &str) -> Value {
        self.icmp(LLVMIntPredicate::LLVMIntSLT, lhs, rhs, name)
    }

    pub fn icmp_gt(&self, lhs: &Value, rhs: &Value, name: &str) -> Value {
        self.icmp(LLVMIntPredicate::LLVMIntSGT, lhs, rhs, name)
    }

    fn fcmp(
        &self,
        pred: LLVMRealPredicate,
        lhs: &Value,
        rhs: &Value,
        name: &str,
    ) -> Value {
        let name_ = CString::new(name).unwrap();
        let val = unsafe {
            LLVMBuildFCmp(
                self.to_ref(),
                pred,
                lhs.to_ref(),
                rhs.to_ref(),
                name_.as_ptr() as *const c_char,
            )
        };
        Value { val, owned: false }
    }

    pub fn fcmp_eq(&self, lhs: &Value, rhs: &Value, name: &str) -> Value {
        self.fcmp(LLVMRealPredicate::LLVMRealOEQ, lhs, rhs, name)
    }

    pub fn fcmp_le(&self, lhs: &Value, rhs: &Value, name: &str) -> Value {
        self.fcmp(LLVMRealPredicate::LLVMRealOLE, lhs, rhs, name)
    }

    pub fn fcmp_ge(&self, lhs: &Value, rhs: &Value, name: &str) -> Value {
        self.fcmp(LLVMRealPredicate::LLVMRealOGE, lhs, rhs, name)
    }

    pub fn fcmp_lt(&self, lhs: &Value, rhs: &Value, name: &str) -> Value {
        self.fcmp(LLVMRealPredicate::LLVMRealOLT, lhs, rhs, name)
    }

    pub fn fcmp_gt(&self, lhs: &Value, rhs: &Value, name: &str) -> Value {
        self.fcmp(LLVMRealPredicate::LLVMRealOGT, lhs, rhs, name)
    }

    pub fn br(&self, bb: &BasicBlock) -> Value {
        let val = unsafe { LLVMBuildBr(self.to_ref(), bb.to_ref()) };
        Value { val, owned: false }
    }

    pub fn cond_br(
        &self,
        cond: &Value,
        true_bb: &BasicBlock,
        false_bb: &BasicBlock,
    ) -> Value {
        let val = unsafe {
            LLVMBuildCondBr(
                self.to_ref(),
                cond.to_ref(),
                true_bb.to_ref(),
                false_bb.to_ref(),
            )
        };
        Value { val, owned: false }
    }

    pub fn phi(
        &self,
        ty: &Type,
        name: &str,
        incoming: &[(&Value, &BasicBlock)],
    ) -> Value {
        let name_ = CString::new(name).unwrap();
        let val = unsafe {
            let val = LLVMBuildPhi(self.to_ref(), ty.to_ref(), name_.as_ptr());
            let mut in_vals: Vec<LLVMValueRef> =
                incoming.iter().map(|x| x.0.to_ref()).collect();
            let mut in_blocks: Vec<LLVMBasicBlockRef> =
                incoming.iter().map(|x| x.1.to_ref()).collect();
            LLVMAddIncoming(
                val,
                in_vals.as_mut_ptr(),
                in_blocks.as_mut_ptr(),
                in_vals.len() as u32,
            );
            val
        };
        Value { val, owned: false }
    }
}

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

    pub fn function_address(&self, name: &str) -> u64 {
        let name_ = CString::new(name).unwrap();
        unsafe {
            LLVMGetFunctionAddress(
                self.to_ref(),
                name_.as_ptr() as *const c_char,
            )
        }
    }
}

pub fn llvm_init() {
    unsafe {
        // FIXME: Below should presumably only be done once per process/thread?
        LLVMLinkInMCJIT();
        LLVM_InitializeNativeTarget();
        //LLVM_InitializeNativeAsmParser();
        LLVM_InitializeNativeAsmPrinter();
    }
}

// FIXME: need macros for variadic types
pub fn add_symbol_for_fn<A, B>(name: &str, f: &extern "C" fn(A) -> B) {
    let name_ = std::ffi::CString::new(name).unwrap();
    let f_ptr = f as *const extern "C" fn(A) -> B;
    let addr = f_ptr as *const core::ffi::c_void as u64;
    unsafe { LLVMAddSymbol(name_.as_ptr(), addr as *mut libc::c_void) }
}

pub fn add_symbol(name: &str, ptr: *const core::ffi::c_void) {
    let name_ = std::ffi::CString::new(name).unwrap();
    //let addr = ptr as *const core::ffi::c_void as u64;
    let addr = ptr as u64;
    unsafe { LLVMAddSymbol(name_.as_ptr(), addr as *mut libc::c_void) }
}

pub fn add_symbol_mut(name: &str, ptr: *mut core::ffi::c_void) {
    let name_ = std::ffi::CString::new(name).unwrap();
    //let addr = ptr as *const core::ffi::c_void as u64;
    let addr = ptr as u64;
    unsafe { LLVMAddSymbol(name_.as_ptr(), addr as *mut libc::c_void) }
}

pub fn set_externally_initialized(val: &Value, ext: bool) {
    unsafe {
        LLVMSetExternallyInitialized(val.to_ref(), if ext { 1 } else { 0 })
    }
}

pub struct LLJITBuilder {
    builder: LLVMOrcLLJITBuilderRef,
    owned: bool,
}

impl RefOwner<LLVMOrcLLJITBuilderRef> for LLJITBuilder {
    fn to_ref(&self) -> LLVMOrcLLJITBuilderRef {
        self.builder
    }

    // FIXME: push up
    fn release(&mut self) {
        self.owned = false
    }
}

impl LLJITBuilder {
    pub fn new() -> LLJITBuilder {
        let builder = unsafe { LLVMOrcCreateLLJITBuilder() };
        LLJITBuilder {
            builder,
            owned: true,
        }
    }

    pub fn create(&self) -> LLJIT {
        unsafe {
            let mut jit = 0 as LLVMOrcLLJITRef;
            let err = LLVMOrcCreateLLJIT(&mut jit, self.to_ref());
            if err == LLVMErrorSuccess as LLVMErrorRef {
                LLJIT { jit, owned: true }
            } else {
                let err_msg =
                    std::ffi::CStr::from_ptr(LLVMGetErrorMessage(err));
                panic!("Unable to create LLJIT, err = {:?}", err_msg)
            }
        }
    }
}

pub struct ThreadSafeContext {
    tsc: LLVMOrcThreadSafeContextRef,
    owned: bool,
}

impl RefOwner<LLVMOrcThreadSafeContextRef> for ThreadSafeContext {
    fn to_ref(&self) -> LLVMOrcThreadSafeContextRef {
        self.tsc
    }

    // FIXME: push up
    fn release(&mut self) {
        self.owned = false
    }
}

impl Drop for ThreadSafeContext {
    fn drop(&mut self) {
        if self.owned {
            unsafe { LLVMOrcDisposeThreadSafeContext(self.tsc) }
            self.owned = false;
        }
    }
}

impl ThreadSafeContext {
    pub fn new() -> ThreadSafeContext {
        let tsc = unsafe { LLVMOrcCreateNewThreadSafeContext() };
        ThreadSafeContext { tsc, owned: true }
    }

    pub fn create_in_context(ctx: &Context) -> ThreadSafeContext {
        // FIXME: not in our version of LLVM?
        unimplemented!()
    }
}

pub struct ThreadSafeModule {
    tsm: LLVMOrcThreadSafeModuleRef,
    owned: bool,
}

impl RefOwner<LLVMOrcThreadSafeModuleRef> for ThreadSafeModule {
    fn to_ref(&self) -> LLVMOrcThreadSafeModuleRef {
        self.tsm
    }

    // FIXME: push up
    fn release(&mut self) {
        self.owned = false
    }
}

impl Drop for ThreadSafeModule {
    fn drop(&mut self) {
        if self.owned {
            unsafe { LLVMOrcDisposeThreadSafeModule(self.tsm) }
            self.owned = false;
        }
    }
}

impl ThreadSafeModule {
    pub fn new(m: &Module, tsc: &ThreadSafeContext) -> ThreadSafeModule {
        let tsm = unsafe {
            LLVMOrcCreateNewThreadSafeModule(m.to_ref(), tsc.to_ref())
        };
        ThreadSafeModule { tsm, owned: true }
    }
}

pub struct JITDylib {
    jd: LLVMOrcJITDylibRef,
}

impl RefOwner<LLVMOrcJITDylibRef> for JITDylib {
    fn to_ref(&self) -> LLVMOrcJITDylibRef {
        self.jd
    }

    // FIXME: push up
    fn release(&mut self) {}
}

impl JITDylib {
    pub fn define(&self, jit: &LLJIT, syms: &[(&str, u64)]) {
        let mut syms_: Vec<LLVMJITCSymbolMapPair> = vec![];
        for (name, addr) in syms {
            let name1 = CString::new(*name).unwrap();
            let name2 = unsafe {
                LLVMOrcLLJITMangleAndIntern(jit.to_ref(), name1.as_ptr())
            };
            let pair = LLVMJITCSymbolMapPair {
                Name: name2,
                Sym: LLVMJITEvaluatedSymbol {
                    Address: *addr,
                    Flags: LLVMJITSymbolFlags {
                        GenericFlags: 0,
                        TargetFlags: 0,
                    },
                },
            };

            syms_.push(pair);
        }
        let abs_syms =
            unsafe { LLVMOrcAbsoluteSymbols(syms_.as_mut_ptr(), syms_.len()) };
        let err = unsafe { LLVMOrcJITDylibDefine(self.jd, abs_syms) };

        //LLVMOrcJITDylibDefine(JD, MU)
        //LLVMOrcAbsoluteSymbols(Syms, NumPairs)
        //LLVMCSymbolMapPair
        //LLVMJITCSymbolMapPair
        //LLVMOrcCSymbolMapPairs
        //LLVMOrcLLJITMangleAndIntern(J, UnmangledName)
        //LLVMJITCSymbolMapPair { Name: name, Sym:  };
    }

    // Dump JIT as a whole and/or symbol table?

    // FIXME: AAAAHHH
    //llvm_sys::support::LLVMAddSymbol;
    //LLVMGetSymbols
}

// Note: No Dispose method for these, so no Drop, and no need to track ownership
pub struct ExecutionSession {
    es: LLVMOrcExecutionSessionRef,
}

impl RefOwner<LLVMOrcExecutionSessionRef> for ExecutionSession {
    fn to_ref(&self) -> LLVMOrcExecutionSessionRef {
        self.es
    }

    // FIXME: push up
    fn release(&mut self) {}
}

pub struct LLJIT {
    jit: LLVMOrcLLJITRef,
    owned: bool,
}

impl RefOwner<LLVMOrcLLJITRef> for LLJIT {
    fn to_ref(&self) -> LLVMOrcLLJITRef {
        self.jit
    }

    // FIXME: push up
    fn release(&mut self) {
        self.owned = false
    }
}

// FIXME: drop

impl LLJIT {
    pub fn get_main_jit_dylib(&self) -> JITDylib {
        let jd = unsafe { LLVMOrcLLJITGetMainJITDylib(self.to_ref()) };
        JITDylib { jd }
    }

    pub fn get_execution_session(&self) -> ExecutionSession {
        let es = unsafe { LLVMOrcLLJITGetExecutionSession(self.to_ref()) };
        ExecutionSession { es }
    }

    pub fn add_ir_module(&self, jd: &JITDylib, tsm: &ThreadSafeModule) {
        unsafe {
            let err = LLVMOrcLLJITAddLLVMIRModule(
                self.jit,
                jd.to_ref(),
                tsm.to_ref(),
            );
            if err != LLVMErrorSuccess as LLVMErrorRef {
                let err_msg =
                    std::ffi::CStr::from_ptr(LLVMGetErrorMessage(err));
                panic!("Unable to add module to JIT, err = {:?}", err_msg)
            }
        }
    }

    // FIXME: mangle/intern
    pub fn lookup(&self, name: &str) -> u64 {
        unsafe {
            let mut res = 0 as LLVMOrcExecutorAddress;
            let name_ = CString::new(name).unwrap();
            let err = LLVMOrcLLJITLookup(self.jit, &mut res, name_.as_ptr());
            if err == LLVMErrorSuccess as LLVMErrorRef {
                res
            } else {
                let err_msg =
                    std::ffi::CStr::from_ptr(LLVMGetErrorMessage(err));
                panic!("Unable to find symbol '{}', err = {:?}", name, err_msg)
            }
        }
    }
}

// Random note scratchpad
//
// Note: MachineModule

// LLVMGetFunctionAddress(EE, Name)
//LLVMGetSymbolAddress(SI)

// GenericValue

//LLVMDisasmInstruction(DC, Bytes, BytesSize, PC, OutString, OutStringSize)
//LLVMCreateDisasm(TripleName, DisInfo, TagType, GetOpInfo, SymbolLookUp)
