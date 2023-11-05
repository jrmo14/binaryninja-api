use crate::{
    architecture::CoreArchitecture,
    basicblock::BasicBlock,
    function::{Function as BaseFunction, Location},
    rc::{Array, Ref, RefCountable},
};
use binaryninjacore_sys::*;
use std::fmt;

use super::block::Block as HighLevelBlock;
use super::{HighLevelILError, Instruction};

type Result<T> = std::result::Result<T, HighLevelILError>;

pub struct Function {
    pub handle: *mut BNHighLevelILFunction,
}

impl Function {
    pub fn arch(&self) -> CoreArchitecture {
        unsafe {
            let arch = BNGetFunctionArchitecture(BNGetHighLevelILOwnerFunction(self.handle));
            CoreArchitecture::from_raw(arch)
        }
    }

    pub fn source_function(&self) -> Result<Ref<BaseFunction>> {
        let func = unsafe { BNGetHighLevelILOwnerFunction(self.handle) };
        if func.is_null() {
            Err(HighLevelILError::NoOwnerFunction)
        } else {
            unsafe { Ok(BaseFunction::from_raw(func)) }
        }
    }

    pub fn get_current_address(&self) -> u64 {
        unsafe { BNHighLevelILGetCurrentAddress(self.handle) }
    }

    pub fn get_root_expr(&self) -> Result<Instruction> {
        let root = unsafe { BNGetHighLevelILRootExpr(self.handle) };
        Instruction::from_expr(self, root, None)
    }

    pub fn get_instruction(&self, idx: usize) -> Result<Instruction> {
        let expr = self.get_index_for_instruction(idx);
        Instruction::from_expr(self, expr, None)
    }

    pub fn get_index_for_instruction(&self, idx: usize) -> usize {
        unsafe { BNGetHighLevelILIndexForInstruction(self.handle, idx) }
    }

    pub fn get_instruction_count(&self) -> usize {
        unsafe { BNGetHighLevelILInstructionCount(self.handle) }
    }

    pub fn get_expr_count(&self) -> usize {
        unsafe { BNGetHighLevelILExprCount(self.handle) }
    }

    pub fn get_basic_blocks(&self) -> Array<BasicBlock<HighLevelBlock>> {
        unsafe {
            let mut count = 0;
            let blocks = BNGetHighLevelILBasicBlockList(self.handle, &mut count);
            let context = HighLevelBlock { function: self };
            Array::new(blocks, count, context)
        }
    }

    pub fn ssa_form(&self) -> Result<Ref<Self>> {
        let handle = unsafe { BNGetHighLevelILSSAForm(self.handle) };
        if handle.is_null() {
            Err(HighLevelILError::NoSSAForm)
        } else {
            Ok(unsafe { Ref::new(Self { handle }) })
        }
    }
    pub fn non_ssa_form(&self) -> Result<Ref<Self>> {
        let handle = unsafe { BNGetHighLevelILNonSSAForm(self.handle) };
        if handle.is_null() {
            Err(HighLevelILError::NoNonSSAForm)
        } else {
            Ok(unsafe { Ref::new(Self { handle }) })
        }
    }
}

unsafe impl Send for Function {}
unsafe impl Sync for Function {}

impl ToOwned for Function {
    type Owned = Ref<Self>;

    fn to_owned(&self) -> Self::Owned {
        unsafe { RefCountable::inc_ref(self) }
    }
}

unsafe impl RefCountable for Function {
    unsafe fn inc_ref(handle: &Self) -> Ref<Self> {
        Ref::new(Self {
            handle: BNNewHighLevelILFunctionReference(handle.handle),
        })
    }

    unsafe fn dec_ref(handle: &Self) {
        BNFreeHighLevelILFunction(handle.handle);
    }
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<hlil func handle {:p}>", self.handle)
    }
}
