use std::ops::Range;

use binaryninjacore_sys::BNGetHighLevelILIndexForInstruction;

use crate::basicblock::{BasicBlock, BlockContext};

use super::*;

pub struct BlockIter<'func> {
    function: &'func Function,
    range: Range<u64>,
}

impl<'func> Iterator for BlockIter<'func> {
    type Item = Instruction<'func>;
    fn next(&mut self) -> Option<Self::Item> {
        self.range.next().map(|i| {
            let expr_index =
                unsafe { BNGetHighLevelILIndexForInstruction(self.function.handle, i as usize) };
            Instruction::from_expr(self.function, expr_index, Some(i as usize)).unwrap()
        })
    }
}

pub struct Block<'func> {
    pub function: &'func Function,
}

impl<'func> BlockContext for Block<'func> {
    type Iter = BlockIter<'func>;
    type Instruction = Instruction<'func>;

    fn start(&self, block: &BasicBlock<Self>) -> Instruction<'func> {
        let index = block.raw_start() as usize;
        let expr_index =
            unsafe { BNGetHighLevelILIndexForInstruction(self.function.handle, index) };
        Instruction::from_expr(self.function, expr_index, Some(index)).unwrap()
    }

    fn iter(&self, block: &BasicBlock<Self>) -> Self::Iter {
        BlockIter {
            function: self.function,
            range: block.raw_start()..block.raw_end(),
        }
    }
}

impl<'func> Clone for Block<'func> {
    fn clone(&self) -> Self {
        Block {
            function: self.function,
        }
    }
}
