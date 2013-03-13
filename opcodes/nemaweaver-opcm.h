/* nemaweaver-opcm.h -- Header used in nemaweaver-opc.h

   Copyright 2009 Free Software Foundation, Inc.

   This file is part of the GNU opcodes library.

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   It is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with this file; see the file COPYING.  If not, write to the
   Free Software Foundation, 51 Franklin Street - Fifth Floor, Boston,
   MA 02110-1301, USA.  */


#ifndef NEMAWEAVER_OPCM
#define NEMAWEAVER_OPCM


enum nemaweaver_instr
{
/* Begin generated code: opcode names */
nwop_sll,
nwop_srl,
nwop_rotr,
nwop_sra,
nwop_sllv,
nwop_srlv,
nwop_rotrv,
nwop_srav,
nwop_mul,
nwop_mulu,
nwop_addu,
nwop_subu,
nwop_mulhiu,
nwop_mulhi,
nwop_and,
nwop_or,
nwop_xor,
nwop_nor,
nwop_slt,
nwop_sltu,
nwop_wsbh,
nwop_sbext,
nwop_shext,
nwop_movt,
nwop_movf,
nwop_movz,
nwop_movn,
nwop_bltz,
nwop_bltzal,
nwop_bgez,
nwop_bgezal,
nwop_beq,
nwop_bneq,
nwop_blez,
nwop_bgtz,
nwop_bc1t,
nwop_bc1f,
nwop_jr,
nwop_jalr,
nwop_j,
nwop_jal,
nwop_addiu,
nwop_slti,
nwop_sltiu,
nwop_andi,
nwop_ori,
nwop_xori,
nwop_lui,
nwop_add_s,
nwop_sub_s,
nwop_mul_s,
nwop_frcp,
nwop_sqrt_s,
nwop_abs_s,
nwop_neg_s,
nwop_madd_s,
nwop_msub_s,
nwop_mfc1,
nwop_mtc1,
nwop_mov_s,
nwop_movf_s,
nwop_movt_s,
nwop_movz_s,
nwop_movn_s,
nwop_trunc_w_s,
nwop_cvt_s_w,
nwop_c_f_s,
nwop_c_un_s,
nwop_c_eq_s,
nwop_c_ueq_s,
nwop_c_olt_s,
nwop_c_ult_s,
nwop_c_ole_s,
nwop_c_ule_s,
nwop_mov_vec,
nwop_movf_vec,
nwop_movt_vec,
nwop_movz_v2,
nwop_movn_v2,
nwop_add_v2,
nwop_sub_v2,
nwop_mul_v2,
nwop_abs_v2,
nwop_neg_v2,
nwop_madd_v2,
nwop_add_v4,
nwop_sub_v4,
nwop_mul_v4,
nwop_abs_v4,
nwop_neg_v4,
nwop_madd_v4,
nwop_f_extr2,
nwop_f_ins2,
nwop_vbuild2,
nwop_f_ins4,
nwop_f_extr4,
nwop_vbuild4,
nwop_fset,
nwop_pixout,
nwop_sample,
nwop_pixin,
nwop_waket,
nwop_tmove,
nwop_invalid,
nwop_fork,
nwop_yield,
nwop_barrier,
nwop_wr_hwr,
nwop_rd_hwr,
nwop_c_f_h,
nwop_c_un_h,
nwop_c_eq_h,
nwop_c_ueq_h,
nwop_c_olt_h,
nwop_c_ult_h,
nwop_c_ole_h,
nwop_c_ule_h,
nwop_cvt_h_s,
nwop_cvt_s_h,
nwop_fset_hf,
nwop_mov_h,
nwop_ld_hf,
nwop_add_h,
nwop_sub_h,
nwop_mul_h,
nwop_frcp_h,
nwop_neg_h,
nwop_st_hf,
nwop_lb,
nwop_lh,
nwop_lw,
nwop_lbu,
nwop_lhu,
nwop_sb,
nwop_sh,
nwop_sw,
nwop_lwxc1,
nwop_swxc1,
nwop_lwc1,
nwop_swc1,
nwop_lwx_v2,
nwop_swx_v2,
nwop_ld_vec,
nwop_st_vec,
/* End generated code: opcode names */
    invalid_inst
};


//------------------------------------------------------
enum nemaweaver_instr_type
{
    arithmetic_inst,
    logical_inst,
    mult_inst,
    div_inst,
    branch_inst,
    return_inst,
    immediate_inst,
    special_inst,
    memory_load_inst,
    memory_store_inst,
    barrel_shift_inst,
    anyware_inst,
    alias_and_warn
};

#define INST_WORD_SIZE 4

/* Gen purpose regs go from 0 to 31.  */
/* Mask is reg num - max_reg_num, ie reg_num - 32 in this case.  */

#define REG_PC_MASK  0x8000
#define REG_MSR_MASK 0x8001
#define REG_EAR_MASK 0x8003
#define REG_ESR_MASK 0x8005
#define REG_FSR_MASK 0x8007
#define REG_BTR_MASK 0x800b
#define REG_EDR_MASK 0x800d
#define REG_PVR_MASK 0xa000

#define REG_PID_MASK   0x9000
#define REG_ZPR_MASK   0x9001
#define REG_TLBX_MASK  0x9002
#define REG_TLBLO_MASK 0x9003
#define REG_TLBHI_MASK 0x9004
#define REG_TLBSX_MASK 0x9005

#define MIN_REGNUM 0
#define MAX_REGNUM 31

#define MIN_PVR_REGNUM 0
#define MAX_PVR_REGNUM 15

#define REG_PC  32    /* PC.  */
#define REG_MSR 33    /* Machine status reg.  */
#define REG_EAR 35    /* Exception reg.  */
#define REG_ESR 37    /* Exception reg.  */
#define REG_FSR 39    /* FPU Status reg.  */
#define REG_BTR 43    /* Branch Target reg.  */
#define REG_EDR 45    /* Exception reg.  */
#define REG_PVR 40960 /* Program Verification reg.  */

#define REG_PID   36864 /* MMU: Process ID reg.  */
#define REG_ZPR   36865 /* MMU: Zone Protect reg.  */
#define REG_TLBX  36866 /* MMU: TLB Index reg.  */
#define REG_TLBLO 36867 /* MMU: TLB Low reg.  */
#define REG_TLBHI 36868 /* MMU: TLB High reg.  */
#define REG_TLBSX 36869 /* MMU: TLB Search Index reg.  */

/* Alternate names for gen purpose regs.  */
#define REG_SP     1 /* stack pointer.  */
#define REG_ROSDP  2 /* read-only small data pointer.  */
#define REG_RWSDP 13 /* read-write small data pointer.  */

/* Assembler Register - Used in Delay Slot Optimization.  */
#define REG_AS    18
#define REG_ZERO  0

#define RD_LOW  21 /* Low bit for RD.  */
#define RA_LOW  16 /* Low bit for RA.  */
#define RB_LOW  11 /* Low bit for RB.  */
#define IMM_LOW  0 /* Low bit for immediate.  */

#define RD_MASK  0x03E00000
#define RA_MASK  0x001F0000
#define RB_MASK  0x0000F800
#define IMM_MASK 0x0000FFFF

/* Imm mask for barrel shifts.  */
#define IMM5_MASK 0x0000001F

/* FSL imm mask for get, put instructions.  */
#define  RFSL_MASK 0x000000F

/* Imm mask for msrset, msrclr instructions.  */
#define  IMM15_MASK 0x00007FFF

#endif /* NEMAWEAVER-OPCM */
