use std::slice;
use binaryninjacore_sys::*;
use super::{HighLevelILError, Function, Instruction, Intrinsic};
use crate::types::{Variable, SSAVariable};
use crate::rc::Ref;

type Result<T> = std::result::Result<T, HighLevelILError>;            
pub enum Operation<'func> {	Nop {},
	Block {body: Vec<Instruction<'func>>},
	If {condition: Instruction<'func>, true_: Instruction<'func>, false_: Instruction<'func>},
	While {condition: Instruction<'func>, body: Instruction<'func>},
	WhileSsa {condition_phi: Instruction<'func>, condition: Instruction<'func>, body: Instruction<'func>},
	DoWhile {body: Instruction<'func>, condition: Instruction<'func>},
	DoWhileSsa {body: Instruction<'func>, condition_phi: Instruction<'func>, condition: Instruction<'func>},
	For {init: Instruction<'func>, condition: Instruction<'func>, update: Instruction<'func>, body: Instruction<'func>},
	ForSsa {init: Instruction<'func>, condition_phi: Instruction<'func>, condition: Instruction<'func>, update: Instruction<'func>, body: Instruction<'func>},
	Switch {condition: Instruction<'func>, default: Instruction<'func>, cases: Vec<Instruction<'func>>},
	Case {values: Vec<Instruction<'func>>, body: Instruction<'func>},
	Break {},
	Continue {},
	Jump {dest: Instruction<'func>},
	Ret {src: Vec<Instruction<'func>>},
	Noret {},
	Unreachable {},
	Goto {target: u64},
	Label {target: u64},
	VarDeclare {var: Variable},
	VarInit {dest: Variable, src: Instruction<'func>},
	VarInitSsa {dest: SSAVariable, src: Instruction<'func>},
	Assign {dest: Instruction<'func>, src: Instruction<'func>},
	AssignUnpack {dest: Vec<Instruction<'func>>, src: Instruction<'func>},
	AssignMemSsa {dest: Instruction<'func>, dest_memory: u64, src: Instruction<'func>, src_memory: u64},
	AssignUnpackMemSsa {dest: Vec<Instruction<'func>>, dest_memory: u64, src: Instruction<'func>, src_memory: u64},
	Var {var: Variable},
	VarSsa {var: SSAVariable},
	VarPhi {dest: SSAVariable, src: Vec<SSAVariable>},
	MemPhi {dest: u64, src: Vec<u64>},
	StructField {src: Instruction<'func>, offset: u64, member_index: u64},
	ArrayIndex {src: Instruction<'func>, index: Instruction<'func>},
	ArrayIndexSsa {src: Instruction<'func>, src_memory: u64, index: Instruction<'func>},
	Split {high: Instruction<'func>, low: Instruction<'func>},
	Deref {src: Instruction<'func>},
	DerefField {src: Instruction<'func>, offset: u64, member_index: u64},
	DerefSsa {src: Instruction<'func>, src_memory: u64},
	DerefFieldSsa {src: Instruction<'func>, src_memory: u64, offset: u64, member_index: u64},
	AddressOf {src: Instruction<'func>},
	Const {constant: u64},
	ConstPtr {constant: u64},
	ExternPtr {constant: u64, offset: u64},
	FloatConst {constant: f64},
	Import {constant: u64},
	ConstData {constant_data: u64},
	Add {left: Instruction<'func>, right: Instruction<'func>},
	Adc {left: Instruction<'func>, right: Instruction<'func>, carry: Instruction<'func>},
	Sub {left: Instruction<'func>, right: Instruction<'func>},
	Sbb {left: Instruction<'func>, right: Instruction<'func>, carry: Instruction<'func>},
	And {left: Instruction<'func>, right: Instruction<'func>},
	Or {left: Instruction<'func>, right: Instruction<'func>},
	Xor {left: Instruction<'func>, right: Instruction<'func>},
	Lsl {left: Instruction<'func>, right: Instruction<'func>},
	Lsr {left: Instruction<'func>, right: Instruction<'func>},
	Asr {left: Instruction<'func>, right: Instruction<'func>},
	Rol {left: Instruction<'func>, right: Instruction<'func>},
	Rlc {left: Instruction<'func>, right: Instruction<'func>, carry: Instruction<'func>},
	Ror {left: Instruction<'func>, right: Instruction<'func>},
	Rrc {left: Instruction<'func>, right: Instruction<'func>, carry: Instruction<'func>},
	Mul {left: Instruction<'func>, right: Instruction<'func>},
	MuluDp {left: Instruction<'func>, right: Instruction<'func>},
	MulsDp {left: Instruction<'func>, right: Instruction<'func>},
	Divu {left: Instruction<'func>, right: Instruction<'func>},
	DivuDp {left: Instruction<'func>, right: Instruction<'func>},
	Divs {left: Instruction<'func>, right: Instruction<'func>},
	DivsDp {left: Instruction<'func>, right: Instruction<'func>},
	Modu {left: Instruction<'func>, right: Instruction<'func>},
	ModuDp {left: Instruction<'func>, right: Instruction<'func>},
	Mods {left: Instruction<'func>, right: Instruction<'func>},
	ModsDp {left: Instruction<'func>, right: Instruction<'func>},
	Neg {src: Instruction<'func>},
	Not {src: Instruction<'func>},
	Sx {src: Instruction<'func>},
	Zx {src: Instruction<'func>},
	LowPart {src: Instruction<'func>},
	Call {dest: Instruction<'func>, params: Vec<Instruction<'func>>},
	CallSsa {dest: Instruction<'func>, params: Vec<Instruction<'func>>, dest_memory: u64, src_memory: u64},
	CmpE {left: Instruction<'func>, right: Instruction<'func>},
	CmpNe {left: Instruction<'func>, right: Instruction<'func>},
	CmpSlt {left: Instruction<'func>, right: Instruction<'func>},
	CmpUlt {left: Instruction<'func>, right: Instruction<'func>},
	CmpSle {left: Instruction<'func>, right: Instruction<'func>},
	CmpUle {left: Instruction<'func>, right: Instruction<'func>},
	CmpSge {left: Instruction<'func>, right: Instruction<'func>},
	CmpUge {left: Instruction<'func>, right: Instruction<'func>},
	CmpSgt {left: Instruction<'func>, right: Instruction<'func>},
	CmpUgt {left: Instruction<'func>, right: Instruction<'func>},
	TestBit {left: Instruction<'func>, right: Instruction<'func>},
	BoolToInt {src: Instruction<'func>},
	AddOverflow {left: Instruction<'func>, right: Instruction<'func>},
	Syscall {params: Vec<Instruction<'func>>},
	SyscallSsa {params: Vec<Instruction<'func>>, dest_memory: u64, src_memory: u64},
	Tailcall {dest: Instruction<'func>, params: Vec<Instruction<'func>>},
	Bp {},
	Trap {vector: u64},
	Intrinsic {intrinsic: Intrinsic, params: Vec<Instruction<'func>>},
	IntrinsicSsa {intrinsic: Intrinsic, params: Vec<Instruction<'func>>, dest_memory: u64, src_memory: u64},
	Undef {},
	Unimpl {},
	UnimplMem {src: Instruction<'func>},
	Fadd {left: Instruction<'func>, right: Instruction<'func>},
	Fsub {left: Instruction<'func>, right: Instruction<'func>},
	Fmul {left: Instruction<'func>, right: Instruction<'func>},
	Fdiv {left: Instruction<'func>, right: Instruction<'func>},
	Fsqrt {src: Instruction<'func>},
	Fneg {src: Instruction<'func>},
	Fabs {src: Instruction<'func>},
	FloatToInt {src: Instruction<'func>},
	IntToFloat {src: Instruction<'func>},
	FloatConv {src: Instruction<'func>},
	RoundToInt {src: Instruction<'func>},
	Floor {src: Instruction<'func>},
	Ceil {src: Instruction<'func>},
	Ftrunc {src: Instruction<'func>},
	FcmpE {left: Instruction<'func>, right: Instruction<'func>},
	FcmpNe {left: Instruction<'func>, right: Instruction<'func>},
	FcmpLt {left: Instruction<'func>, right: Instruction<'func>},
	FcmpLe {left: Instruction<'func>, right: Instruction<'func>},
	FcmpGe {left: Instruction<'func>, right: Instruction<'func>},
	FcmpGt {left: Instruction<'func>, right: Instruction<'func>},
	FcmpO {left: Instruction<'func>, right: Instruction<'func>},
	FcmpUo {left: Instruction<'func>, right: Instruction<'func>},
}
impl<'func> Operation<'func> {
   pub fn from_instr(instr: BNHighLevelILInstruction, func: &'func Function, expr_index: usize) -> Result<Operation> {
      let arch = func.arch();
      // Macros used to define each of the types of arguments in each operation
      macro_rules! expr {
         ($operand_index:literal) => {{
             let res = Instruction::from_expr(
                 func, 
                 instr.operands[$operand_index].try_into().unwrap(), 
                 None)?;
             res
         }}
      }

      macro_rules! float {
         ($operand_index:literal) => {{
             // Extract the value from the operand
             let res = match instr.size {
                 4 => f32::from_bits(instr.operands[$operand_index] as u32) as f64,
                 8 => f64::from_bits(instr.operands[$operand_index]),
                 _ => unreachable!()
             };
             res
         }}
      }

      macro_rules! int {
         ($operand_index:literal) => {{
             let value = instr.operands[$operand_index] ;
             let res = (value & ((1 << 63) - 1)).wrapping_sub(value & (1 << 63));
             res
         }}
      }

      macro_rules! expr_list {
         ($operand_index:literal) => {{
             // Initialize the resulting instructions vec
             let mut instrs = Vec::new();
             let mut count = 0;
   
             unsafe { 
                 // Get the pointer to instruction indexes from binja core
                 let operands = BNHighLevelILGetOperandList(func.handle, expr_index, 
                                                            $operand_index, &mut count);


                 // Get the slice from the found pointer
                 let operands_slice = slice::from_raw_parts(operands, count as usize);

                 // Create each instruction
                 for op in operands_slice {
                     let i = Instruction::from_expr(func, (*op).try_into().unwrap(), None)?;
                     instrs.push(i);
                 }

                 // Free the binja core pointer
                 BNHighLevelILFreeOperandList(operands);
             }

             instrs
         }}
      }

      macro_rules! int_list {
         ($operand_index:literal) => {{
             // Generate the int list from the binja core
             let mut count = 0;
             let mut int_list = Vec::new();
   
             unsafe { 
                 let operands = BNHighLevelILGetOperandList(func.handle, expr_index, 
                                                         $operand_index, &mut count);


                 let operands_slice = slice::from_raw_parts(operands, count as usize);

                 for i in 0..count {
                     int_list.push(operands_slice[i as usize]);
                 }

                 BNHighLevelILFreeOperandList(operands);
             }

             int_list
         }}
      }


      macro_rules! var {
         ($operand_index:literal) => {{
             let bnvar = unsafe { BNFromVariableIdentifier(instr.operands[$operand_index]) };
             let res = unsafe {Variable::from_raw(bnvar)};
             res
         }}
      }

      macro_rules! var_ssa {
         ($operand_index:literal) => {{
             let var = var!($operand_index);
             let version = instr.operands[$operand_index+1];
             let version = version.try_into().unwrap();
             SSAVariable{var, version}
         }}
      }

      macro_rules! var_ssa_list {
         ($operand_index:literal) => {{
             let mut count = 0;
             let mut vars = Vec::new();
   
             unsafe { 
                 let operands = BNHighLevelILGetOperandList(func.handle, expr_index, 
                                                            $operand_index, &mut count);


                 let operands_slice = slice::from_raw_parts(operands, count as usize);

                 for i in (0..count).step_by(2) {
                     let id      = operands_slice[i as usize];
                     let version = operands_slice[i as usize + 1];
                     let bnvar = BNFromVariableIdentifier(id);
                     let var = Variable::from_raw(bnvar);
                     let version = version.try_into().unwrap(); 
                     vars.push(SSAVariable{var, version});
                 }

                 BNHighLevelILFreeOperandList(operands);
             }

             vars
         }}
      }

      macro_rules! intrinsic {
         ($operand_index:literal) => {{
             let res = Intrinsic::new(arch.clone(), instr.operands[$operand_index] as u32);
             res
         }}
      }

      Ok(match instr.operation {
   
	BNHighLevelILOperation::HLIL_NOP => {
		Operation::Nop {
		
		}
	}
	BNHighLevelILOperation::HLIL_BLOCK => {
		let body = expr_list!(0);
		Operation::Block {
		body
		}
	}
	BNHighLevelILOperation::HLIL_IF => {
		let condition = expr!(0);
		let true_ = expr!(1);
		let false_ = expr!(2);
		Operation::If {
		condition, true_, false_
		}
	}
	BNHighLevelILOperation::HLIL_WHILE => {
		let condition = expr!(0);
		let body = expr!(1);
		Operation::While {
		condition, body
		}
	}
	BNHighLevelILOperation::HLIL_WHILE_SSA => {
		let condition_phi = expr!(0);
		let condition = expr!(1);
		let body = expr!(2);
		Operation::WhileSsa {
		condition_phi, condition, body
		}
	}
	BNHighLevelILOperation::HLIL_DO_WHILE => {
		let body = expr!(0);
		let condition = expr!(1);
		Operation::DoWhile {
		body, condition
		}
	}
	BNHighLevelILOperation::HLIL_DO_WHILE_SSA => {
		let body = expr!(0);
		let condition_phi = expr!(1);
		let condition = expr!(2);
		Operation::DoWhileSsa {
		body, condition_phi, condition
		}
	}
	BNHighLevelILOperation::HLIL_FOR => {
		let init = expr!(0);
		let condition = expr!(1);
		let update = expr!(2);
		let body = expr!(3);
		Operation::For {
		init, condition, update, body
		}
	}
	BNHighLevelILOperation::HLIL_FOR_SSA => {
		let init = expr!(0);
		let condition_phi = expr!(1);
		let condition = expr!(2);
		let update = expr!(3);
		let body = expr!(4);
		Operation::ForSsa {
		init, condition_phi, condition, update, body
		}
	}
	BNHighLevelILOperation::HLIL_SWITCH => {
		let condition = expr!(0);
		let default = expr!(1);
		let cases = expr_list!(2);
		Operation::Switch {
		condition, default, cases
		}
	}
	BNHighLevelILOperation::HLIL_CASE => {
		let values = expr_list!(0);
		let body = expr!(2);
		Operation::Case {
		values, body
		}
	}
	BNHighLevelILOperation::HLIL_BREAK => {
		Operation::Break {
		
		}
	}
	BNHighLevelILOperation::HLIL_CONTINUE => {
		Operation::Continue {
		
		}
	}
	BNHighLevelILOperation::HLIL_JUMP => {
		let dest = expr!(0);
		Operation::Jump {
		dest
		}
	}
	BNHighLevelILOperation::HLIL_RET => {
		let src = expr_list!(0);
		Operation::Ret {
		src
		}
	}
	BNHighLevelILOperation::HLIL_NORET => {
		Operation::Noret {
		
		}
	}
	BNHighLevelILOperation::HLIL_UNREACHABLE => {
		Operation::Unreachable {
		
		}
	}
	BNHighLevelILOperation::HLIL_GOTO => {
		let target = int!(0);
		Operation::Goto {
		target
		}
	}
	BNHighLevelILOperation::HLIL_LABEL => {
		let target = int!(0);
		Operation::Label {
		target
		}
	}
	BNHighLevelILOperation::HLIL_VAR_DECLARE => {
		let var = var!(0);
		Operation::VarDeclare {
		var
		}
	}
	BNHighLevelILOperation::HLIL_VAR_INIT => {
		let dest = var!(0);
		let src = expr!(1);
		Operation::VarInit {
		dest, src
		}
	}
	BNHighLevelILOperation::HLIL_VAR_INIT_SSA => {
		let dest = var_ssa!(0);
		let src = expr!(2);
		Operation::VarInitSsa {
		dest, src
		}
	}
	BNHighLevelILOperation::HLIL_ASSIGN => {
		let dest = expr!(0);
		let src = expr!(1);
		Operation::Assign {
		dest, src
		}
	}
	BNHighLevelILOperation::HLIL_ASSIGN_UNPACK => {
		let dest = expr_list!(0);
		let src = expr!(2);
		Operation::AssignUnpack {
		dest, src
		}
	}
	BNHighLevelILOperation::HLIL_ASSIGN_MEM_SSA => {
		let dest = expr!(0);
		let dest_memory = int!(1);
		let src = expr!(2);
		let src_memory = int!(3);
		Operation::AssignMemSsa {
		dest, dest_memory, src, src_memory
		}
	}
	BNHighLevelILOperation::HLIL_ASSIGN_UNPACK_MEM_SSA => {
		let dest = expr_list!(0);
		let dest_memory = int!(2);
		let src = expr!(3);
		let src_memory = int!(4);
		Operation::AssignUnpackMemSsa {
		dest, dest_memory, src, src_memory
		}
	}
	BNHighLevelILOperation::HLIL_VAR => {
		let var = var!(0);
		Operation::Var {
		var
		}
	}
	BNHighLevelILOperation::HLIL_VAR_SSA => {
		let var = var_ssa!(0);
		Operation::VarSsa {
		var
		}
	}
	BNHighLevelILOperation::HLIL_VAR_PHI => {
		let dest = var_ssa!(0);
		let src = var_ssa_list!(2);
		Operation::VarPhi {
		dest, src
		}
	}
	BNHighLevelILOperation::HLIL_MEM_PHI => {
		let dest = int!(0);
		let src = int_list!(1);
		Operation::MemPhi {
		dest, src
		}
	}
	BNHighLevelILOperation::HLIL_STRUCT_FIELD => {
		let src = expr!(0);
		let offset = int!(1);
		let member_index = int!(2);
		Operation::StructField {
		src, offset, member_index
		}
	}
	BNHighLevelILOperation::HLIL_ARRAY_INDEX => {
		let src = expr!(0);
		let index = expr!(1);
		Operation::ArrayIndex {
		src, index
		}
	}
	BNHighLevelILOperation::HLIL_ARRAY_INDEX_SSA => {
		let src = expr!(0);
		let src_memory = int!(1);
		let index = expr!(2);
		Operation::ArrayIndexSsa {
		src, src_memory, index
		}
	}
	BNHighLevelILOperation::HLIL_SPLIT => {
		let high = expr!(0);
		let low = expr!(1);
		Operation::Split {
		high, low
		}
	}
	BNHighLevelILOperation::HLIL_DEREF => {
		let src = expr!(0);
		Operation::Deref {
		src
		}
	}
	BNHighLevelILOperation::HLIL_DEREF_FIELD => {
		let src = expr!(0);
		let offset = int!(1);
		let member_index = int!(2);
		Operation::DerefField {
		src, offset, member_index
		}
	}
	BNHighLevelILOperation::HLIL_DEREF_SSA => {
		let src = expr!(0);
		let src_memory = int!(1);
		Operation::DerefSsa {
		src, src_memory
		}
	}
	BNHighLevelILOperation::HLIL_DEREF_FIELD_SSA => {
		let src = expr!(0);
		let src_memory = int!(1);
		let offset = int!(2);
		let member_index = int!(3);
		Operation::DerefFieldSsa {
		src, src_memory, offset, member_index
		}
	}
	BNHighLevelILOperation::HLIL_ADDRESS_OF => {
		let src = expr!(0);
		Operation::AddressOf {
		src
		}
	}
	BNHighLevelILOperation::HLIL_CONST => {
		let constant = int!(0);
		Operation::Const {
		constant
		}
	}
	BNHighLevelILOperation::HLIL_CONST_PTR => {
		let constant = int!(0);
		Operation::ConstPtr {
		constant
		}
	}
	BNHighLevelILOperation::HLIL_EXTERN_PTR => {
		let constant = int!(0);
		let offset = int!(1);
		Operation::ExternPtr {
		constant, offset
		}
	}
	BNHighLevelILOperation::HLIL_FLOAT_CONST => {
		let constant = float!(0);
		Operation::FloatConst {
		constant
		}
	}
	BNHighLevelILOperation::HLIL_IMPORT => {
		let constant = int!(0);
		Operation::Import {
		constant
		}
	}
	BNHighLevelILOperation::HLIL_CONST_DATA => {
		let constant_data = int!(0);
		Operation::ConstData {
		constant_data
		}
	}
	BNHighLevelILOperation::HLIL_ADD => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::Add {
		left, right
		}
	}
	BNHighLevelILOperation::HLIL_ADC => {
		let left = expr!(0);
		let right = expr!(1);
		let carry = expr!(2);
		Operation::Adc {
		left, right, carry
		}
	}
	BNHighLevelILOperation::HLIL_SUB => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::Sub {
		left, right
		}
	}
	BNHighLevelILOperation::HLIL_SBB => {
		let left = expr!(0);
		let right = expr!(1);
		let carry = expr!(2);
		Operation::Sbb {
		left, right, carry
		}
	}
	BNHighLevelILOperation::HLIL_AND => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::And {
		left, right
		}
	}
	BNHighLevelILOperation::HLIL_OR => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::Or {
		left, right
		}
	}
	BNHighLevelILOperation::HLIL_XOR => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::Xor {
		left, right
		}
	}
	BNHighLevelILOperation::HLIL_LSL => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::Lsl {
		left, right
		}
	}
	BNHighLevelILOperation::HLIL_LSR => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::Lsr {
		left, right
		}
	}
	BNHighLevelILOperation::HLIL_ASR => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::Asr {
		left, right
		}
	}
	BNHighLevelILOperation::HLIL_ROL => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::Rol {
		left, right
		}
	}
	BNHighLevelILOperation::HLIL_RLC => {
		let left = expr!(0);
		let right = expr!(1);
		let carry = expr!(2);
		Operation::Rlc {
		left, right, carry
		}
	}
	BNHighLevelILOperation::HLIL_ROR => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::Ror {
		left, right
		}
	}
	BNHighLevelILOperation::HLIL_RRC => {
		let left = expr!(0);
		let right = expr!(1);
		let carry = expr!(2);
		Operation::Rrc {
		left, right, carry
		}
	}
	BNHighLevelILOperation::HLIL_MUL => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::Mul {
		left, right
		}
	}
	BNHighLevelILOperation::HLIL_MULU_DP => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::MuluDp {
		left, right
		}
	}
	BNHighLevelILOperation::HLIL_MULS_DP => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::MulsDp {
		left, right
		}
	}
	BNHighLevelILOperation::HLIL_DIVU => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::Divu {
		left, right
		}
	}
	BNHighLevelILOperation::HLIL_DIVU_DP => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::DivuDp {
		left, right
		}
	}
	BNHighLevelILOperation::HLIL_DIVS => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::Divs {
		left, right
		}
	}
	BNHighLevelILOperation::HLIL_DIVS_DP => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::DivsDp {
		left, right
		}
	}
	BNHighLevelILOperation::HLIL_MODU => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::Modu {
		left, right
		}
	}
	BNHighLevelILOperation::HLIL_MODU_DP => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::ModuDp {
		left, right
		}
	}
	BNHighLevelILOperation::HLIL_MODS => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::Mods {
		left, right
		}
	}
	BNHighLevelILOperation::HLIL_MODS_DP => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::ModsDp {
		left, right
		}
	}
	BNHighLevelILOperation::HLIL_NEG => {
		let src = expr!(0);
		Operation::Neg {
		src
		}
	}
	BNHighLevelILOperation::HLIL_NOT => {
		let src = expr!(0);
		Operation::Not {
		src
		}
	}
	BNHighLevelILOperation::HLIL_SX => {
		let src = expr!(0);
		Operation::Sx {
		src
		}
	}
	BNHighLevelILOperation::HLIL_ZX => {
		let src = expr!(0);
		Operation::Zx {
		src
		}
	}
	BNHighLevelILOperation::HLIL_LOW_PART => {
		let src = expr!(0);
		Operation::LowPart {
		src
		}
	}
	BNHighLevelILOperation::HLIL_CALL => {
		let dest = expr!(0);
		let params = expr_list!(1);
		Operation::Call {
		dest, params
		}
	}
	BNHighLevelILOperation::HLIL_CALL_SSA => {
		let dest = expr!(0);
		let params = expr_list!(1);
		let dest_memory = int!(3);
		let src_memory = int!(4);
		Operation::CallSsa {
		dest, params, dest_memory, src_memory
		}
	}
	BNHighLevelILOperation::HLIL_CMP_E => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::CmpE {
		left, right
		}
	}
	BNHighLevelILOperation::HLIL_CMP_NE => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::CmpNe {
		left, right
		}
	}
	BNHighLevelILOperation::HLIL_CMP_SLT => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::CmpSlt {
		left, right
		}
	}
	BNHighLevelILOperation::HLIL_CMP_ULT => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::CmpUlt {
		left, right
		}
	}
	BNHighLevelILOperation::HLIL_CMP_SLE => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::CmpSle {
		left, right
		}
	}
	BNHighLevelILOperation::HLIL_CMP_ULE => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::CmpUle {
		left, right
		}
	}
	BNHighLevelILOperation::HLIL_CMP_SGE => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::CmpSge {
		left, right
		}
	}
	BNHighLevelILOperation::HLIL_CMP_UGE => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::CmpUge {
		left, right
		}
	}
	BNHighLevelILOperation::HLIL_CMP_SGT => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::CmpSgt {
		left, right
		}
	}
	BNHighLevelILOperation::HLIL_CMP_UGT => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::CmpUgt {
		left, right
		}
	}
	BNHighLevelILOperation::HLIL_TEST_BIT => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::TestBit {
		left, right
		}
	}
	BNHighLevelILOperation::HLIL_BOOL_TO_INT => {
		let src = expr!(0);
		Operation::BoolToInt {
		src
		}
	}
	BNHighLevelILOperation::HLIL_ADD_OVERFLOW => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::AddOverflow {
		left, right
		}
	}
	BNHighLevelILOperation::HLIL_SYSCALL => {
		let params = expr_list!(0);
		Operation::Syscall {
		params
		}
	}
	BNHighLevelILOperation::HLIL_SYSCALL_SSA => {
		let params = expr_list!(0);
		let dest_memory = int!(2);
		let src_memory = int!(3);
		Operation::SyscallSsa {
		params, dest_memory, src_memory
		}
	}
	BNHighLevelILOperation::HLIL_TAILCALL => {
		let dest = expr!(0);
		let params = expr_list!(1);
		Operation::Tailcall {
		dest, params
		}
	}
	BNHighLevelILOperation::HLIL_BP => {
		Operation::Bp {
		
		}
	}
	BNHighLevelILOperation::HLIL_TRAP => {
		let vector = int!(0);
		Operation::Trap {
		vector
		}
	}
	BNHighLevelILOperation::HLIL_INTRINSIC => {
		let intrinsic = intrinsic!(0);
		let params = expr_list!(1);
		Operation::Intrinsic {
		intrinsic, params
		}
	}
	BNHighLevelILOperation::HLIL_INTRINSIC_SSA => {
		let intrinsic = intrinsic!(0);
		let params = expr_list!(1);
		let dest_memory = int!(3);
		let src_memory = int!(4);
		Operation::IntrinsicSsa {
		intrinsic, params, dest_memory, src_memory
		}
	}
	BNHighLevelILOperation::HLIL_UNDEF => {
		Operation::Undef {
		
		}
	}
	BNHighLevelILOperation::HLIL_UNIMPL => {
		Operation::Unimpl {
		
		}
	}
	BNHighLevelILOperation::HLIL_UNIMPL_MEM => {
		let src = expr!(0);
		Operation::UnimplMem {
		src
		}
	}
	BNHighLevelILOperation::HLIL_FADD => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::Fadd {
		left, right
		}
	}
	BNHighLevelILOperation::HLIL_FSUB => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::Fsub {
		left, right
		}
	}
	BNHighLevelILOperation::HLIL_FMUL => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::Fmul {
		left, right
		}
	}
	BNHighLevelILOperation::HLIL_FDIV => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::Fdiv {
		left, right
		}
	}
	BNHighLevelILOperation::HLIL_FSQRT => {
		let src = expr!(0);
		Operation::Fsqrt {
		src
		}
	}
	BNHighLevelILOperation::HLIL_FNEG => {
		let src = expr!(0);
		Operation::Fneg {
		src
		}
	}
	BNHighLevelILOperation::HLIL_FABS => {
		let src = expr!(0);
		Operation::Fabs {
		src
		}
	}
	BNHighLevelILOperation::HLIL_FLOAT_TO_INT => {
		let src = expr!(0);
		Operation::FloatToInt {
		src
		}
	}
	BNHighLevelILOperation::HLIL_INT_TO_FLOAT => {
		let src = expr!(0);
		Operation::IntToFloat {
		src
		}
	}
	BNHighLevelILOperation::HLIL_FLOAT_CONV => {
		let src = expr!(0);
		Operation::FloatConv {
		src
		}
	}
	BNHighLevelILOperation::HLIL_ROUND_TO_INT => {
		let src = expr!(0);
		Operation::RoundToInt {
		src
		}
	}
	BNHighLevelILOperation::HLIL_FLOOR => {
		let src = expr!(0);
		Operation::Floor {
		src
		}
	}
	BNHighLevelILOperation::HLIL_CEIL => {
		let src = expr!(0);
		Operation::Ceil {
		src
		}
	}
	BNHighLevelILOperation::HLIL_FTRUNC => {
		let src = expr!(0);
		Operation::Ftrunc {
		src
		}
	}
	BNHighLevelILOperation::HLIL_FCMP_E => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::FcmpE {
		left, right
		}
	}
	BNHighLevelILOperation::HLIL_FCMP_NE => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::FcmpNe {
		left, right
		}
	}
	BNHighLevelILOperation::HLIL_FCMP_LT => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::FcmpLt {
		left, right
		}
	}
	BNHighLevelILOperation::HLIL_FCMP_LE => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::FcmpLe {
		left, right
		}
	}
	BNHighLevelILOperation::HLIL_FCMP_GE => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::FcmpGe {
		left, right
		}
	}
	BNHighLevelILOperation::HLIL_FCMP_GT => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::FcmpGt {
		left, right
		}
	}
	BNHighLevelILOperation::HLIL_FCMP_O => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::FcmpO {
		left, right
		}
	}
	BNHighLevelILOperation::HLIL_FCMP_UO => {
		let left = expr!(0);
		let right = expr!(1);
		Operation::FcmpUo {
		left, right
		}
	}
})}
	pub fn name(&self) -> String {
		match self {Operation::Nop{..} => "Nop".to_string(),
Operation::Block{..} => "Block".to_string(),
Operation::If{..} => "If".to_string(),
Operation::While{..} => "While".to_string(),
Operation::WhileSsa{..} => "WhileSsa".to_string(),
Operation::DoWhile{..} => "DoWhile".to_string(),
Operation::DoWhileSsa{..} => "DoWhileSsa".to_string(),
Operation::For{..} => "For".to_string(),
Operation::ForSsa{..} => "ForSsa".to_string(),
Operation::Switch{..} => "Switch".to_string(),
Operation::Case{..} => "Case".to_string(),
Operation::Break{..} => "Break".to_string(),
Operation::Continue{..} => "Continue".to_string(),
Operation::Jump{..} => "Jump".to_string(),
Operation::Ret{..} => "Ret".to_string(),
Operation::Noret{..} => "Noret".to_string(),
Operation::Unreachable{..} => "Unreachable".to_string(),
Operation::Goto{..} => "Goto".to_string(),
Operation::Label{..} => "Label".to_string(),
Operation::VarDeclare{..} => "VarDeclare".to_string(),
Operation::VarInit{..} => "VarInit".to_string(),
Operation::VarInitSsa{..} => "VarInitSsa".to_string(),
Operation::Assign{..} => "Assign".to_string(),
Operation::AssignUnpack{..} => "AssignUnpack".to_string(),
Operation::AssignMemSsa{..} => "AssignMemSsa".to_string(),
Operation::AssignUnpackMemSsa{..} => "AssignUnpackMemSsa".to_string(),
Operation::Var{..} => "Var".to_string(),
Operation::VarSsa{..} => "VarSsa".to_string(),
Operation::VarPhi{..} => "VarPhi".to_string(),
Operation::MemPhi{..} => "MemPhi".to_string(),
Operation::StructField{..} => "StructField".to_string(),
Operation::ArrayIndex{..} => "ArrayIndex".to_string(),
Operation::ArrayIndexSsa{..} => "ArrayIndexSsa".to_string(),
Operation::Split{..} => "Split".to_string(),
Operation::Deref{..} => "Deref".to_string(),
Operation::DerefField{..} => "DerefField".to_string(),
Operation::DerefSsa{..} => "DerefSsa".to_string(),
Operation::DerefFieldSsa{..} => "DerefFieldSsa".to_string(),
Operation::AddressOf{..} => "AddressOf".to_string(),
Operation::Const{..} => "Const".to_string(),
Operation::ConstPtr{..} => "ConstPtr".to_string(),
Operation::ExternPtr{..} => "ExternPtr".to_string(),
Operation::FloatConst{..} => "FloatConst".to_string(),
Operation::Import{..} => "Import".to_string(),
Operation::ConstData{..} => "ConstData".to_string(),
Operation::Add{..} => "Add".to_string(),
Operation::Adc{..} => "Adc".to_string(),
Operation::Sub{..} => "Sub".to_string(),
Operation::Sbb{..} => "Sbb".to_string(),
Operation::And{..} => "And".to_string(),
Operation::Or{..} => "Or".to_string(),
Operation::Xor{..} => "Xor".to_string(),
Operation::Lsl{..} => "Lsl".to_string(),
Operation::Lsr{..} => "Lsr".to_string(),
Operation::Asr{..} => "Asr".to_string(),
Operation::Rol{..} => "Rol".to_string(),
Operation::Rlc{..} => "Rlc".to_string(),
Operation::Ror{..} => "Ror".to_string(),
Operation::Rrc{..} => "Rrc".to_string(),
Operation::Mul{..} => "Mul".to_string(),
Operation::MuluDp{..} => "MuluDp".to_string(),
Operation::MulsDp{..} => "MulsDp".to_string(),
Operation::Divu{..} => "Divu".to_string(),
Operation::DivuDp{..} => "DivuDp".to_string(),
Operation::Divs{..} => "Divs".to_string(),
Operation::DivsDp{..} => "DivsDp".to_string(),
Operation::Modu{..} => "Modu".to_string(),
Operation::ModuDp{..} => "ModuDp".to_string(),
Operation::Mods{..} => "Mods".to_string(),
Operation::ModsDp{..} => "ModsDp".to_string(),
Operation::Neg{..} => "Neg".to_string(),
Operation::Not{..} => "Not".to_string(),
Operation::Sx{..} => "Sx".to_string(),
Operation::Zx{..} => "Zx".to_string(),
Operation::LowPart{..} => "LowPart".to_string(),
Operation::Call{..} => "Call".to_string(),
Operation::CallSsa{..} => "CallSsa".to_string(),
Operation::CmpE{..} => "CmpE".to_string(),
Operation::CmpNe{..} => "CmpNe".to_string(),
Operation::CmpSlt{..} => "CmpSlt".to_string(),
Operation::CmpUlt{..} => "CmpUlt".to_string(),
Operation::CmpSle{..} => "CmpSle".to_string(),
Operation::CmpUle{..} => "CmpUle".to_string(),
Operation::CmpSge{..} => "CmpSge".to_string(),
Operation::CmpUge{..} => "CmpUge".to_string(),
Operation::CmpSgt{..} => "CmpSgt".to_string(),
Operation::CmpUgt{..} => "CmpUgt".to_string(),
Operation::TestBit{..} => "TestBit".to_string(),
Operation::BoolToInt{..} => "BoolToInt".to_string(),
Operation::AddOverflow{..} => "AddOverflow".to_string(),
Operation::Syscall{..} => "Syscall".to_string(),
Operation::SyscallSsa{..} => "SyscallSsa".to_string(),
Operation::Tailcall{..} => "Tailcall".to_string(),
Operation::Bp{..} => "Bp".to_string(),
Operation::Trap{..} => "Trap".to_string(),
Operation::Intrinsic{..} => "Intrinsic".to_string(),
Operation::IntrinsicSsa{..} => "IntrinsicSsa".to_string(),
Operation::Undef{..} => "Undef".to_string(),
Operation::Unimpl{..} => "Unimpl".to_string(),
Operation::UnimplMem{..} => "UnimplMem".to_string(),
Operation::Fadd{..} => "Fadd".to_string(),
Operation::Fsub{..} => "Fsub".to_string(),
Operation::Fmul{..} => "Fmul".to_string(),
Operation::Fdiv{..} => "Fdiv".to_string(),
Operation::Fsqrt{..} => "Fsqrt".to_string(),
Operation::Fneg{..} => "Fneg".to_string(),
Operation::Fabs{..} => "Fabs".to_string(),
Operation::FloatToInt{..} => "FloatToInt".to_string(),
Operation::IntToFloat{..} => "IntToFloat".to_string(),
Operation::FloatConv{..} => "FloatConv".to_string(),
Operation::RoundToInt{..} => "RoundToInt".to_string(),
Operation::Floor{..} => "Floor".to_string(),
Operation::Ceil{..} => "Ceil".to_string(),
Operation::Ftrunc{..} => "Ftrunc".to_string(),
Operation::FcmpE{..} => "FcmpE".to_string(),
Operation::FcmpNe{..} => "FcmpNe".to_string(),
Operation::FcmpLt{..} => "FcmpLt".to_string(),
Operation::FcmpLe{..} => "FcmpLe".to_string(),
Operation::FcmpGe{..} => "FcmpGe".to_string(),
Operation::FcmpGt{..} => "FcmpGt".to_string(),
Operation::FcmpO{..} => "FcmpO".to_string(),
Operation::FcmpUo{..} => "FcmpUo".to_string()}}}