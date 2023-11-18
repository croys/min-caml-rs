//#![allow(clippy::upper_case_acronyms)]
#![allow(dead_code)]

use crate::asm;
use crate::id;

extern crate llvm_sys;

use llvm_sys::core::LLVMAddFunction;
use llvm_sys::core::LLVMAppendBasicBlockInContext;
use llvm_sys::core::LLVMCreateBuilderInContext;
use llvm_sys::core::LLVMPositionBuilderAtEnd;
//use llvm_sys::core::{LLVMGetGlobalContext, LLVMConstInt, LLVMDoubleTypeInContext, LLVMDumpValue, LLVMPrintValueToString, LLVMDisposeMessage, LLVMGetValueName2, LLVMConstArray, LLVMAddGlobal, LLVMModuleCreateWithNameInContext, LLVMContextDispose};
use llvm_sys::core::{
    LLVMAddGlobal, LLVMBuildAdd, LLVMBuildRet, LLVMConstArray, LLVMConstReal,
    LLVMContextCreate, LLVMContextDispose, LLVMDisposeMessage,
    LLVMDisposeModule, LLVMDumpModule, LLVMDumpValue, LLVMFunctionType,
    LLVMGetGlobalContext, LLVMGetParam, LLVMGetValueName2,
    LLVMModuleCreateWithNameInContext, LLVMPrintValueToString,
    LLVMSetGlobalConstant, LLVMSetInitializer,
};
use llvm_sys::execution_engine::LLVMCreateExecutionEngineForModule;
use llvm_sys::execution_engine::{
    LLVMExecutionEngineRef, LLVMGetFunctionAddress, LLVMGetGlobalValueAddress,
    LLVMLinkInMCJIT,
};
use llvm_sys::prelude::LLVMBasicBlockRef;
//use llvm_sys::execution_engine::{LLVMGetFunctionAddress, LLVMGetGlobalValueAddress, LLVMAddModule};
//use llvm_sys::object::LLVMGetSymbolAddress;
//use llvm_sys::prelude::{/* LLVMBuilderRef, */ LLVMValueRef};
use llvm_sys::prelude::LLVMBuilderRef;
use llvm_sys::prelude::LLVMValueRef;
use llvm_sys::prelude::{LLVMContextRef, LLVMModuleRef, LLVMTypeRef};

use llvm_sys::target::LLVM_InitializeNativeAsmPrinter;
use llvm_sys::target::LLVM_InitializeNativeTarget;

//use llvm_sys::LLVMModule;

use llvm_sys::core::{
    LLVMArrayType, LLVMDoubleType, LLVMDoubleTypeInContext, LLVMIntType,
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

    // add global
    // FIXME: global type maybe not necessary, just use a value
    // and create here.

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
    pub fn int_type_in_context(ctx: &Context, size: u32) -> Type {
        let ty = unsafe { LLVMIntTypeInContext(ctx.to_ref(), size) };
        Type { ty, owned: false }
    }

    pub fn double_type_in_context(ctx: &Context) -> Type {
        let ty = unsafe { LLVMDoubleTypeInContext(ctx.to_ref()) };
        Type { ty, owned: false }
    }

    // FIXME: dump
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
    // FIXME: return self
    fn release(&mut self) {
        self.owned = false
    }
}

impl Constant {
    // FIXME: ints
    //LLVMConstInt(IntTy, N, SignExtend)

    pub fn real(ty: &Type, x: f64) -> Constant {
        let val = unsafe { LLVMConstReal(ty.to_ref(), x) };
        Constant { val, owned: true }
    }

    pub fn array(elem_ty: &Type, vals: &[&Constant]) -> Constant {
        // crete vec of pointers
        let n: libc::c_uint = vals.len().try_into().unwrap();
        let mut v: Vec<LLVMValueRef> =
            vals.iter().map(|x| x.to_ref()).collect();
        let val =
            unsafe { LLVMConstArray(elem_ty.to_ref(), v.as_mut_ptr(), n) };
        Constant { val, owned: true }
    }

    pub fn dump(&self) {
        unsafe { LLVMDumpValue(self.val) }
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
    // module should probably be mut
    pub fn add_to_module(m: &Module, name: &str, ty: &Type) -> Global {
        let name_ = CString::new(name).unwrap();
        let val = unsafe {
            LLVMAddGlobal(
                m.to_ref(),
                ty.to_ref(),
                name_.as_ptr() as *const c_char,
            )
        };
        Global { val, owned: false }
    }

    // Should maybe consider internal mutability for owned
    pub fn set_constant(&self, val: &mut Constant) {
        val.release();
        unsafe {
            LLVMSetInitializer(self.to_ref(), val.to_ref());
            LLVMSetGlobalConstant(self.to_ref(), 1);
        }
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
}

// FIXME: wrap up basic block

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

    pub fn ret(&self, val: &Value) -> Value {
        let val = unsafe { LLVMBuildRet(self.to_ref(), val.to_ref()) };
        Value { val, owned: false }
    }
}

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

#[allow(unused_variables)]
pub fn f(p: &asm::Prog) {
    let asm::Prog::Prog(floats, fds, c) = p;

    let ctx = Context::new();
    let module = Module::create_with_name_in_context("mincaml_main", &ctx);

    // Create constants for doubles
    let double_ty = Type::double_type_in_context(&ctx);

    // Keep a map from labels to values

    let mut globals = HashMap::<id::L, Global>::new();

    for (ref id, ref x) in floats {
        println!("Adding float const: {} = {}", id.0, x);

        let mut const_val = Constant::real(&double_ty, *x);
        let global_val =
            Global::add_to_module(&module, id.0.as_str(), &double_ty);
        global_val.set_constant(&mut const_val);
        globals.insert(id.clone(), global_val);

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
