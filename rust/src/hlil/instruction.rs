use std::rc::Rc;

use super::operation::Operation;
use super::HighLevelILError;
use crate::disassembly::InstructionTextToken;
use binaryninjacore_sys::*;

use super::Function;

pub struct Instruction<'func> {
    pub operation: Rc<Operation<'func>>,
    pub source_operand: u32,
    pub size: usize,
    pub operands: [u64; 5],
    pub address: u64,
    pub function: &'func Function,
    pub expr_index: usize,
    pub instr_index: usize,
    pub as_ast: bool,
}

type Result<T> = std::result::Result<T, HighLevelILError>;

impl<'func> Instruction<'func> {
    pub fn from_expr(
        func: &'func Function,
        expr_index: usize,
        instr_index: Option<usize>,
    ) -> Result<Instruction> {
        // TODO maybe use generic for this?
        let as_ast = true;

        let instr = unsafe { BNGetHighLevelILByIndex(func.handle, expr_index, as_ast) };

        let core_instr_idx = unsafe { BNGetHighLevelILInstructionForExpr(func.handle, expr_index) };

        if core_instr_idx == !0 && instr.address == 0 && instr.size == 0 {
            return Err(HighLevelILError::InvalidExprIndex(expr_index));
        }

        let instr_index = if instr_index.is_none() {
            core_instr_idx
        } else {
            instr_index.unwrap()
        };

        Ok(Instruction {
            operation: Rc::new(Operation::from_instr(instr, func, expr_index)?),
            source_operand: instr.sourceOperand,
            size: instr.size,
            operands: instr.operands,
            address: instr.address,
            function: func,
            expr_index,
            instr_index,
            as_ast,
        })
    }

    pub fn text(&self) -> Result<String> {
        let mut count = 0;

        let mut res_lines = Vec::new();

        let lines = unsafe {
            BNGetHighLevelILExprText(
                self.function.handle,
                self.expr_index,
                true,
                &mut count,
                std::ptr::null_mut(),
            )
        };

        if lines.is_null() {
            return Err(HighLevelILError::RetrieveTokensFail);
        }

        let lines_slice = unsafe { std::slice::from_raw_parts(lines, count as usize) };

        for line in lines_slice.iter() {
            unsafe {
                res_lines.extend(
                    std::slice::from_raw_parts(line.tokens, line.count as usize)
                        .iter()
                        .map(|l| InstructionTextToken::from_raw(l).text().as_str().to_owned()),
                )
            };
        }
        Ok(res_lines.join("\n"))
    }

    pub fn visit_simple(&'func self, cb: &mut impl FnMut(&Self) -> bool) {
        let mut acc = vec![self];
        while let Some(il) = acc.pop() {
            if cb(il) {
                self.visit_children_collect(&mut acc);
            }
        }
    }

    fn visit_children_collect(&'func self, acc: &mut Vec<&'func Self>) {
        match &*self.operation {
            Operation::If { condition, .. }
            | Operation::Switch { condition, .. }
            | Operation::While { condition, .. }
            | Operation::DoWhile { condition, .. } => acc.push(condition),
            Operation::WhileSsa {
                condition_phi,
                condition,
                ..
            }
            | Operation::DoWhileSsa {
                condition_phi,
                condition,
                ..
            } => {
                acc.push(condition);
                acc.push(condition_phi);
            }
            Operation::For {
                init,
                condition,
                update,
                ..
            } => {
                acc.push(init);
                acc.push(condition);
                acc.push(update)
            }
            Operation::ForSsa {
                init,
                condition_phi,
                condition,
                update,
                ..
            } => {
                acc.push(init);
                acc.push(condition_phi);
                acc.push(condition);
                acc.push(update);
            }
            Operation::Assign { dest, src } | Operation::AssignMemSsa { dest, src, .. } => {
                acc.push(src);
                acc.push(dest);
            }

            Operation::AssignUnpack { dest, src, .. }
            | Operation::AssignUnpackMemSsa { dest, src, .. } => {
                acc.extend(dest.iter());
                acc.push(src);
            }

            Operation::VarInit { src, .. } | Operation::VarInitSsa { src, .. } => acc.push(src),

            Operation::Jump { dest } => acc.push(dest),

            Operation::Ret { src } => acc.extend(src.iter()),

            Operation::ArrayIndex { src, index } | Operation::ArrayIndexSsa { src, index, .. } => {
                acc.push(src);
                acc.push(index);
            }

            Operation::Split { high, low } => {
                acc.push(high);
                acc.push(low);
            }

            Operation::Adc { left, right, carry }
            | Operation::Sbb { left, right, carry }
            | Operation::Rlc { left, right, carry }
            | Operation::Rrc { left, right, carry } => {
                acc.push(left);
                acc.push(right);
                acc.push(carry);
            }

            Operation::SyscallSsa { params, .. }
            | Operation::Syscall { params }
            | Operation::Intrinsic { params, .. }
            | Operation::IntrinsicSsa { params, .. } => acc.extend(params.iter()),

            Operation::Call { dest, params }
            | Operation::CallSsa { dest, params, .. }
            | Operation::Tailcall { dest, params } => {
                acc.push(dest);
                acc.extend(params.iter())
            }

            Operation::StructField { src, .. }
            | Operation::DerefField { src, .. }
            | Operation::DerefSsa { src, .. }
            | Operation::DerefFieldSsa { src, .. }
            | Operation::Deref { src }
            | Operation::AddressOf { src }
            | Operation::Neg { src }
            | Operation::Not { src }
            | Operation::Sx { src }
            | Operation::Zx { src }
            | Operation::LowPart { src }
            | Operation::BoolToInt { src }
            | Operation::UnimplMem { src }
            | Operation::Fsqrt { src }
            | Operation::Fneg { src }
            | Operation::Fabs { src }
            | Operation::FloatToInt { src }
            | Operation::IntToFloat { src }
            | Operation::FloatConv { src }
            | Operation::RoundToInt { src }
            | Operation::Floor { src }
            | Operation::Ceil { src }
            | Operation::Ftrunc { src } => acc.push(src),

            Operation::Add { left, right }
            | Operation::Sub { left, right }
            | Operation::And { left, right }
            | Operation::Or { left, right }
            | Operation::Xor { left, right }
            | Operation::Lsl { left, right }
            | Operation::Lsr { left, right }
            | Operation::Asr { left, right }
            | Operation::Rol { left, right }
            | Operation::Ror { left, right }
            | Operation::Mul { left, right }
            | Operation::MuluDp { left, right }
            | Operation::MulsDp { left, right }
            | Operation::Divu { left, right }
            | Operation::DivuDp { left, right }
            | Operation::Divs { left, right }
            | Operation::DivsDp { left, right }
            | Operation::Modu { left, right }
            | Operation::ModuDp { left, right }
            | Operation::Mods { left, right }
            | Operation::ModsDp { left, right }
            | Operation::CmpE { left, right }
            | Operation::CmpNe { left, right }
            | Operation::CmpSlt { left, right }
            | Operation::CmpUlt { left, right }
            | Operation::CmpSle { left, right }
            | Operation::CmpUle { left, right }
            | Operation::CmpSge { left, right }
            | Operation::CmpUge { left, right }
            | Operation::CmpSgt { left, right }
            | Operation::CmpUgt { left, right }
            | Operation::TestBit { left, right }
            | Operation::AddOverflow { left, right }
            | Operation::Fadd { left, right }
            | Operation::Fsub { left, right }
            | Operation::Fmul { left, right }
            | Operation::Fdiv { left, right }
            | Operation::FcmpE { left, right }
            | Operation::FcmpNe { left, right }
            | Operation::FcmpLt { left, right }
            | Operation::FcmpLe { left, right }
            | Operation::FcmpGe { left, right }
            | Operation::FcmpGt { left, right }
            | Operation::FcmpO { left, right }
            | Operation::FcmpUo { left, right } => {
                acc.push(left);
                acc.push(right);
            }

            Operation::Trap { .. }
            | Operation::Const { .. }
            | Operation::ConstPtr { .. }
            | Operation::ExternPtr { .. }
            | Operation::FloatConst { .. }
            | Operation::Import { .. }
            | Operation::ConstData { .. }
            | Operation::VarDeclare { .. }
            | Operation::Var { .. }
            | Operation::VarSsa { .. }
            | Operation::VarPhi { .. }
            | Operation::MemPhi { .. }
            | Operation::Goto { .. }
            | Operation::Label { .. }
            | Operation::Case { .. }
            | Operation::Block { .. }
            | Operation::Unreachable {}
            | Operation::Break {}
            | Operation::Continue {}
            | Operation::Noret {}
            | Operation::Bp {}
            | Operation::Undef {}
            | Operation::Unimpl {}
            | Operation::Nop {} => {}
        }
    }
}
