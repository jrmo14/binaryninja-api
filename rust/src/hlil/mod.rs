mod block;
mod function;
mod instruction;
mod operation;

use binaryninjacore_sys::BNGetArchitectureIntrinsicName;
pub use function::Function;
pub use instruction::Instruction;

use crate::{architecture::CoreArchitecture, string::BnString};

#[derive(Debug)]
pub enum HighLevelILError {
    InvalidExprIndex(usize),
    RetrieveTokensFail,
    NoOwnerFunction,
    NoBlockForInstrIdx,
    NoSSAForm,
    NoNonSSAForm,
}

pub struct IntrinsicIndex(u32);

pub struct Intrinsic {
    pub arch: CoreArchitecture,
    pub index: IntrinsicIndex,
}

impl Intrinsic {
    pub fn new(arch: CoreArchitecture, index: u32) -> Self {
        Self {
            arch,
            index: IntrinsicIndex(index),
        }
    }

    pub fn get_name(&self) -> BnString {
        unsafe { BnString::from_raw(BNGetArchitectureIntrinsicName(self.arch.0, self.index.0)) }
    }
}
