/* nemaweaver-opc.h -- MicroBlaze Opcodes

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


#ifndef NEMAWEAVER_OPC
#define NEMAWEAVER_OPC

#include "nemaweaver-opcm.h"

/* New instn type for barrel shift imms. */
#define INST_TYPE_RD_R1_IMM5        13
#define INST_TYPE_RD_RFSL           14
#define INST_TYPE_R1_RFSL           15

/* New insn type for insn cache.  */
#define INST_TYPE_RD_R1_SPECIAL     16

/* New insn type for msrclr, msrset insns.  */
#define INST_TYPE_RD_IMM15          17

/* New insn type for tuqula rd - addik rd, r0, 42.  */
#define INST_TYPE_RD                18

/* New insn type for t*put.  */
#define INST_TYPE_RFSL              19
#define INST_TYPE_NONE              25



/* Instructions where the label address is resolved as a PC offset
   (for branch label).  */
#define INST_PC_OFFSET 1
/* Instructions where the label address is resolved as an absolute
   value (for data mem or abs address).  */
#define INST_NO_OFFSET 0

#define OPCODE_MAST_MM    0xFC0007FF


#define OPCODE_MASK_H     0xFC000000  /* High 6 bits only.  */
#define OPCODE_MASK_H1    0xFFE00000  /* High 11 bits.      */
#define OPCODE_MASK_H2    0xFC1F0000  /* High 6 and bits 20-16.  */
#define OPCODE_MASK_H12   0xFFFF0000  /* High 16.  */
#define OPCODE_MASK_H4    0xFC0007FF  /* High 6 and low 11 bits.  */
#define OPCODE_MASK_H13S  0xFFE0EFF0  /* High 11 and 15:1 bits and last nibble of last byte for spr.  */
#define OPCODE_MASK_H23S  0xFC1FC000  /* High 6, 20-16 and 15:1 bits and last nibble of last byte for spr.  */
#define OPCODE_MASK_H34   0xFC00FFFF  /* High 6 and low 16 bits.  */
#define OPCODE_MASK_H14   0xFFE007FF  /* High 11 and low 11 bits.  */
#define OPCODE_MASK_H24   0xFC1F07FF  /* High 6, bits 20-16 and low 11 bits.  */
#define OPCODE_MASK_H124  0xFFFF07FF  /* High 16, and low 11 bits.  */
#define OPCODE_MASK_H1234 0xFFFFFFFF  /* All 32 bits.  */
#define OPCODE_MASK_H3    0xFC000600  /* High 6 bits and bits 21, 22.  */
#define OPCODE_MASK_H32   0xFC00FC00  /* High 6 bits and bit 16-21.  */
#define OPCODE_MASK_H34B  0xFC0000FF  /* High 6 bits and low 8 bits.  */
#define OPCODE_MASK_H34C  0xFC0007E0  /* High 6 bits and bits 21-26.  */

/* New Mask for msrset, msrclr insns.  */
#define OPCODE_MASK_H23N  0xFC1F8000  /* High 6 and bits 11 - 16.  */

#define DELAY_SLOT    1
#define NO_DELAY_SLOT 0

#define MAX_OPCODES 280

/* Immediate types */
#define ARG_TYPE_IMM     0x10
#define ARG_TYPE_UIMM    0x01

/* Register types */
#define ARG_TYPE_REG        0x08
#define ARG_TYPE_REG_FLOAT  0x04
#define ARG_TYPE_REG_VECTOR 0x02
#define ARG_TYPE_INV        0x00

/* The maximum registers an opcode can have as args. */
#define ARG_MAX_REGS 5
#define ARG_MAX 5

/* Build an int with thes 5 bit values */
#define OP_BUILD5(a0, a1, a2, a3, a4) (((a0)&0x1f)<<0 | ((a1)&0x1f)<<5 | ((a2)&0x1f)<<10 | ((a3)&0x1f)<<15 | ((a4)&0x1f)<<20)
/* Get a field from the above */
#define OP_BREAD5(i, bitfield) (((bitfield)>>((i)*5)) & 0x1f)
/* Get an op's mask */
#define ARG_MASK(i, op)  ((1<<OP_BREAD5(i, op->arg_mask))-1)
#define ARG_SHIFT(i, op) (OP_BREAD5(i, op->arg_shift))
#define ARG_TYPE(i, op)  (OP_BREAD5(i, op->arg_type))
#define ARG_UIMM(op) (ARG_TYPE(op->imm_arg, op) & ARG_TYPE_UIMM)

/* Get immediate position in bytes. Note that imm_size shouls be ceiled */
#define DIV_CEIL(x,n) (((x)%(n)) ? ((x)/(n)+1) : ((x)/(n)))
#define IMM_SIZE_BITS(op) (OP_BREAD5((op)->imm_arg, (op)->arg_mask))
#define IMM_SIZE(op) DIV_CEIL(IMM_SIZE_BITS(op),8)
#define IMM_BIT_POS(op) (ARG_SHIFT((op)->imm_arg, (op)) + IMM_SIZE(op))
#define IMM_POS(op) (IMM_BIT_POS(op) / 8)

#define FLOAT_REG(t) (((t) & ARG_TYPE_REG_FLOAT) && 1)
#define VECTOR_REG(t) (((t) & ARG_TYPE_REG_VECTOR) && 1)

struct op_code_struct
{
    char * name;

    //  short inst_type;            /* Registers and immediate values involved.  */
    unsigned long arg_mask;	/* A series of 5bit numbers of the
				 * length of each mask, this also
				 * tells us how many args exist */
    unsigned long arg_shift;	/* A series of 5bit numbers of the shift of each arg */
    unsigned long arg_type;	/* ARG_TYPE_REG or ARG_TYPE_IMM */
    short inst_offset_type;     /* Immediate vals offset from PC? (= 1 for branches).  */
    short delay_slots;          /* Info about delay slots needed after this instr. */
    short imm_arg;	        /* Which argument is the imm, -1 if none (used to be immval_mask) */
    unsigned long bit_sequence; /* All the fixed bits for the op are set and
				   all the variable bits (reg names, imm vals)
				   are set to 0.  */
    unsigned long opcode_mask;  /* Which bits define the opcode.  */
    enum nemaweaver_instr instr;
    enum nemaweaver_instr_type instr_type;
    /* More info about output format here.  */
} opcodes[MAX_OPCODES] =
{
    /* Begin generated code: opcodes */
{"sll", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 16, 6, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, 0, 0), INST_NO_OFFSET, 0, 2, 0x0, 0xffe0003f, nwop_sll, anyware_inst},
{"srl", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 16, 6, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, 0, 0), INST_NO_OFFSET, 0, 2, 0x2, 0xffe0003f, nwop_srl, anyware_inst},
{"rotr", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 16, 6, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, 0, 0), INST_NO_OFFSET, 0, 2, 0x200002, 0xffe0003f, nwop_rotr, anyware_inst},
{"sra", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 16, 6, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, 0, 0), INST_NO_OFFSET, 0, 2, 0x3, 0xffe0003f, nwop_sra, anyware_inst},
{"sllv", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 16, 21, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_REG, 0, 0), INST_NO_OFFSET, 0, -1, 0x4, 0xfc0007ff, nwop_sllv, anyware_inst},
{"srlv", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 16, 21, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_REG, 0, 0), INST_NO_OFFSET, 0, -1, 0x6, 0xfc0007ff, nwop_srlv, anyware_inst},
{"rotrv", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 16, 21, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_REG, 0, 0), INST_NO_OFFSET, 0, -1, 0x46, 0xfc0007ff, nwop_rotrv, anyware_inst},
{"srav", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 16, 21, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_REG, 0, 0), INST_NO_OFFSET, 0, -1, 0x7, 0xfc0007ff, nwop_srav, anyware_inst},
{"mul", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 21, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_REG, 0, 0), INST_NO_OFFSET, 0, -1, 0x70000002, 0xfc0007ff, nwop_mul, anyware_inst},
{"mulu", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 21, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_REG, 0, 0), INST_NO_OFFSET, 0, -1, 0x59, 0xfc0007ff, nwop_mulu, anyware_inst},
{"addu", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 21, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_REG, 0, 0), INST_NO_OFFSET, 0, -1, 0x21, 0xfc0007ff, nwop_addu, anyware_inst},
{"subu", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 21, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_REG, 0, 0), INST_NO_OFFSET, 0, -1, 0x23, 0xfc0007ff, nwop_subu, anyware_inst},
{"mulhiu", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 21, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_REG, 0, 0), INST_NO_OFFSET, 0, -1, 0x259, 0xfc0007ff, nwop_mulhiu, anyware_inst},
{"mulhi", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 21, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_REG, 0, 0), INST_NO_OFFSET, 0, -1, 0x70000003, 0xfc0007ff, nwop_mulhi, anyware_inst},
{"and", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 21, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_REG, 0, 0), INST_NO_OFFSET, 0, -1, 0x24, 0xfc0007ff, nwop_and, anyware_inst},
{"or", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 21, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_REG, 0, 0), INST_NO_OFFSET, 0, -1, 0x25, 0xfc0007ff, nwop_or, anyware_inst},
{"xor", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 21, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_REG, 0, 0), INST_NO_OFFSET, 0, -1, 0x26, 0xfc0007ff, nwop_xor, anyware_inst},
{"nor", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 21, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_REG, 0, 0), INST_NO_OFFSET, 0, -1, 0x27, 0xfc0007ff, nwop_nor, anyware_inst},
{"slt", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 21, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_REG, 0, 0), INST_NO_OFFSET, 0, -1, 0x2a, 0xfc0007ff, nwop_slt, anyware_inst},
{"sltu", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 21, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_REG, 0, 0), INST_NO_OFFSET, 0, -1, 0x2b, 0xfc0007ff, nwop_sltu, anyware_inst},
{"wsbh", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 16, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x7c0000a0, 0xffe007ff, nwop_wsbh, anyware_inst},
{"sbext", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 16, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x7c000420, 0xffe007ff, nwop_sbext, anyware_inst},
{"shext", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 16, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x7c000620, 0xffe007ff, nwop_shext, anyware_inst},
{"movt", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x10001, 0xfc1f07ff, nwop_movt, anyware_inst},
{"movf", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x1, 0xfc1f07ff, nwop_movf, anyware_inst},
{"movz", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 21, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_REG, 0, 0), INST_NO_OFFSET, 0, -1, 0xa, 0xfc0007ff, nwop_movz, anyware_inst},
{"movn", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 21, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_REG, 0, 0), INST_NO_OFFSET, 0, -1, 0xb, 0xfc0007ff, nwop_movn, anyware_inst},
{"bltz", OP_BUILD5(5, 16, 0, 0, 0), OP_BUILD5(21, 0, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_IMM, 0, 0, 0), INST_PC_OFFSET, 0, 1, 0x4000000, 0xfc1f0000, nwop_bltz, anyware_inst},
{"bltzal", OP_BUILD5(5, 16, 0, 0, 0), OP_BUILD5(21, 0, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_IMM, 0, 0, 0), INST_PC_OFFSET, 0, 1, 0x4100000, 0xfc1f0000, nwop_bltzal, anyware_inst},
{"bgez", OP_BUILD5(5, 16, 0, 0, 0), OP_BUILD5(21, 0, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_IMM, 0, 0, 0), INST_PC_OFFSET, 0, 1, 0x4010000, 0xfc1f0000, nwop_bgez, anyware_inst},
{"bgezal", OP_BUILD5(5, 16, 0, 0, 0), OP_BUILD5(21, 0, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_IMM, 0, 0, 0), INST_PC_OFFSET, 0, 1, 0x4110000, 0xfc1f0000, nwop_bgezal, anyware_inst},
{"beq", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, 0, 0), INST_PC_OFFSET, 0, 2, 0x10000000, 0xfc000000, nwop_beq, anyware_inst},
{"bneq", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, 0, 0), INST_PC_OFFSET, 0, 2, 0x14000000, 0xfc000000, nwop_bneq, anyware_inst},
{"blez", OP_BUILD5(5, 16, 0, 0, 0), OP_BUILD5(21, 0, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_IMM, 0, 0, 0), INST_PC_OFFSET, 0, 1, 0x18000000, 0xfc1f0000, nwop_blez, anyware_inst},
{"bgtz", OP_BUILD5(5, 16, 0, 0, 0), OP_BUILD5(21, 0, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_IMM, 0, 0, 0), INST_PC_OFFSET, 0, 1, 0x1c000000, 0xfc1f0000, nwop_bgtz, anyware_inst},
{"bc1t", OP_BUILD5(16, 0, 0, 0, 0), OP_BUILD5(0, 0, 0, 0, 0), OP_BUILD5(ARG_TYPE_IMM, 0, 0, 0, 0), INST_PC_OFFSET, 0, 0, 0x45010000, 0xffff0000, nwop_bc1t, anyware_inst},
{"bc1f", OP_BUILD5(16, 0, 0, 0, 0), OP_BUILD5(0, 0, 0, 0, 0), OP_BUILD5(ARG_TYPE_IMM, 0, 0, 0, 0), INST_PC_OFFSET, 0, 0, 0x45000000, 0xffff0000, nwop_bc1f, anyware_inst},
{"jr", OP_BUILD5(5, 0, 0, 0, 0), OP_BUILD5(21, 0, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, 0, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x8, 0xfc1fffff, nwop_jr, anyware_inst},
{"jalr", OP_BUILD5(5, 0, 0, 0, 0), OP_BUILD5(21, 0, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, 0, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0xf809, 0xfc1fffff, nwop_jalr, anyware_inst},
{"j", OP_BUILD5(26, 0, 0, 0, 0), OP_BUILD5(0, 0, 0, 0, 0), OP_BUILD5(ARG_TYPE_IMM, 0, 0, 0, 0), INST_NO_OFFSET, 0, 0, 0x8000000, 0xfc000000, nwop_j, anyware_inst},
{"jal", OP_BUILD5(26, 0, 0, 0, 0), OP_BUILD5(0, 0, 0, 0, 0), OP_BUILD5(ARG_TYPE_IMM, 0, 0, 0, 0), INST_NO_OFFSET, 0, 0, 0xc000000, 0xfc000000, nwop_jal, anyware_inst},
{"addiu", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, 0, 0), INST_NO_OFFSET, 0, 2, 0x24000000, 0xfc000000, nwop_addiu, anyware_inst},
{"slti", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, 0, 0), INST_NO_OFFSET, 0, 2, 0x28000000, 0xfc000000, nwop_slti, anyware_inst},
{"sltiu", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, 0, 0), INST_NO_OFFSET, 0, 2, 0x2c000000, 0xfc000000, nwop_sltiu, anyware_inst},
{"andi", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, 0, 0), INST_NO_OFFSET, 0, 2, 0x30000000, 0xfc000000, nwop_andi, anyware_inst},
{"ori", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, 0, 0), INST_NO_OFFSET, 0, 2, 0x34000000, 0xfc000000, nwop_ori, anyware_inst},
{"xori", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, 0, 0), INST_NO_OFFSET, 0, 2, 0x38000000, 0xfc000000, nwop_xori, anyware_inst},
{"lui", OP_BUILD5(5, 16, 0, 0, 0), OP_BUILD5(16, 0, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_IMM, 0, 0, 0), INST_NO_OFFSET, 0, 1, 0x3c000000, 0xffe00000, nwop_lui, anyware_inst},
{"add.s", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(6, 11, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0), INST_NO_OFFSET, 0, -1, 0x46000000, 0xffe0003f, nwop_add_s, anyware_inst},
{"sub.s", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(6, 11, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0), INST_NO_OFFSET, 0, -1, 0x46000001, 0xffe0003f, nwop_sub_s, anyware_inst},
{"mul.s", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(6, 11, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0), INST_NO_OFFSET, 0, -1, 0x46000002, 0xffe0003f, nwop_mul_s, anyware_inst},
{"frcp", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(6, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x44000004, 0xffff003f, nwop_frcp, anyware_inst},
{"sqrt.s", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(6, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x46000004, 0xffff003f, nwop_sqrt_s, anyware_inst},
{"abs.s", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(6, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x46000005, 0xffff003f, nwop_abs_s, anyware_inst},
{"neg.s", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(6, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x46000007, 0xffff003f, nwop_neg_s, anyware_inst},
{"madd.s", OP_BUILD5(5, 5, 5, 5, 0), OP_BUILD5(6, 21, 11, 16, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0), INST_NO_OFFSET, 0, -1, 0x4c000020, 0xfc00003f, nwop_madd_s, anyware_inst},
{"msub.s", OP_BUILD5(5, 5, 5, 5, 0), OP_BUILD5(6, 21, 11, 16, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0), INST_NO_OFFSET, 0, -1, 0x4c000028, 0xfc00003f, nwop_msub_s, anyware_inst},
{"mfc1", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(16, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x44000000, 0xffe007ff, nwop_mfc1, anyware_inst},
{"mtc1", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(16, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x44800000, 0xffe007ff, nwop_mtc1, anyware_inst},
{"mov.s", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(6, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x46000006, 0xffff003f, nwop_mov_s, anyware_inst},
{"movf.s", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(6, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x46000011, 0xffff003f, nwop_movf_s, anyware_inst},
{"movt.s", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(6, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x46010011, 0xffff003f, nwop_movt_s, anyware_inst},
{"movz.s", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(6, 11, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG, 0, 0), INST_NO_OFFSET, 0, -1, 0x46000012, 0xffe0003f, nwop_movz_s, anyware_inst},
{"movn.s", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(6, 11, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG, 0, 0), INST_NO_OFFSET, 0, -1, 0x46000013, 0xffe0003f, nwop_movn_s, anyware_inst},
{"trunc.w.s", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(6, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x4600000d, 0xffff003f, nwop_trunc_w_s, anyware_inst},
{"cvt.s.w", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(6, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x46800020, 0xffff003f, nwop_cvt_s_w, anyware_inst},
{"c.f.s", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 16, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x46000030, 0xffe007ff, nwop_c_f_s, anyware_inst},
{"c.un.s", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 16, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x46000031, 0xffe007ff, nwop_c_un_s, anyware_inst},
{"c.eq.s", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 16, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x46000032, 0xffe007ff, nwop_c_eq_s, anyware_inst},
{"c.ueq.s", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 16, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x46000033, 0xffe007ff, nwop_c_ueq_s, anyware_inst},
{"c.olt.s", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 16, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x46000034, 0xffe007ff, nwop_c_olt_s, anyware_inst},
{"c.ult.s", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 16, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x46000035, 0xffe007ff, nwop_c_ult_s, anyware_inst},
{"c.ole.s", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 16, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x46000036, 0xffe007ff, nwop_c_ole_s, anyware_inst},
{"c.ule.s", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 16, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x46000037, 0xffe007ff, nwop_c_ule_s, anyware_inst},
{"c.lt.s", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 16, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x4600003c, 0xffe007ff, nwop_c_lt_s, anyware_inst},
{"c.le.s", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 16, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x4600003e, 0xffe007ff, nwop_c_le_s, anyware_inst},
{"mov.ps", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(6, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x46c00006, 0xffff003f, nwop_mov_ps, anyware_inst},
{"movf.ps", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(6, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x46c00011, 0xffff003f, nwop_movf_ps, anyware_inst},
{"movt.ps", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(6, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x46c10011, 0xffff003f, nwop_movt_ps, anyware_inst},
{"movz.ps", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(6, 11, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG, 0, 0), INST_NO_OFFSET, 0, -1, 0x46c00012, 0xffe0003f, nwop_movz_ps, anyware_inst},
{"movn.ps", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(6, 11, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG, 0, 0), INST_NO_OFFSET, 0, -1, 0x46c00013, 0xffe0003f, nwop_movn_ps, anyware_inst},
{"add.v2", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(6, 11, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0), INST_NO_OFFSET, 0, -1, 0x46c00000, 0xffe0003f, nwop_add_v2, anyware_inst},
{"sub.ps", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(6, 11, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0), INST_NO_OFFSET, 0, -1, 0x46c00001, 0xffe0003f, nwop_sub_ps, anyware_inst},
{"mul.ps", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(6, 11, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0), INST_NO_OFFSET, 0, -1, 0x46c00002, 0xffe0003f, nwop_mul_ps, anyware_inst},
{"abs.ps", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(6, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x46c00005, 0xffff003f, nwop_abs_ps, anyware_inst},
{"neg.ps", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(6, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x46c00007, 0xffff003f, nwop_neg_ps, anyware_inst},
{"madd.ps", OP_BUILD5(5, 5, 5, 5, 0), OP_BUILD5(6, 21, 11, 16, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0), INST_NO_OFFSET, 0, -1, 0x4c000026, 0xfc00003f, nwop_madd_ps, anyware_inst},
{"f_extr2", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(16, 11, 6, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_IMM, 0, 0), INST_NO_OFFSET, 0, 2, 0x77c00007, 0xffe0003f, nwop_f_extr2, anyware_inst},
{"vbuild2", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(6, 11, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_VECTOR, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0), INST_NO_OFFSET, 0, -1, 0x7780000b, 0xffe0003f, nwop_vbuild2, anyware_inst},
{"fset", OP_BUILD5(5, 16, 0, 0, 0), OP_BUILD5(16, 0, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_IMM, 0, 0, 0), INST_NO_OFFSET, 0, 1, 0x77e00000, 0xffe00000, nwop_fset, anyware_inst},
{"f_ins2", OP_BUILD5(5, 5, 5, 5, 0), OP_BUILD5(16, 11, 21, 6, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_IMM, 0), INST_NO_OFFSET, 0, 3, 0x7c000007, 0xfc00003f, nwop_f_ins2, anyware_inst},
{"waket", OP_BUILD5(5, 0, 0, 0, 0), OP_BUILD5(21, 0, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, 0, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x408, 0xfc1fffff, nwop_waket, anyware_inst},
{"tmove", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(16, 21, 6, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, 0, 0), INST_NO_OFFSET, 0, 2, 0x7c000014, 0xfc00f83f, nwop_tmove, anyware_inst},
{"invalid", OP_BUILD5(0, 0, 0, 0, 0), OP_BUILD5(0, 0, 0, 0, 0), OP_BUILD5(0, 0, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0xadbeef, 0xffffffff, nwop_invalid, anyware_inst},
{"fork", OP_BUILD5(5, 5, 5, 5, 0), OP_BUILD5(11, 21, 16, 6, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, 0), INST_NO_OFFSET, 0, 3, 0x7c000008, 0xfc00003f, nwop_fork, anyware_inst},
{"yield", OP_BUILD5(5, 0, 0, 0, 0), OP_BUILD5(21, 0, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, 0, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x7c000009, 0xfc1fffff, nwop_yield, anyware_inst},
{"barrier", OP_BUILD5(5, 0, 0, 0, 0), OP_BUILD5(6, 0, 0, 0, 0), OP_BUILD5(ARG_TYPE_IMM, 0, 0, 0, 0), INST_NO_OFFSET, 0, 0, 0x70000018, 0xfffff83f, nwop_barrier, anyware_inst},
{"c.f.h", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 16, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x46800030, 0xffe007ff, nwop_c_f_h, anyware_inst},
{"c.un.h", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 16, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x46800031, 0xffe007ff, nwop_c_un_h, anyware_inst},
{"c.eq.h", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 16, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x46800032, 0xffe007ff, nwop_c_eq_h, anyware_inst},
{"c.ueq.h", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 16, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x46800033, 0xffe007ff, nwop_c_ueq_h, anyware_inst},
{"c.olt.h", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 16, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x46800034, 0xffe007ff, nwop_c_olt_h, anyware_inst},
{"c.ult.h", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 16, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x46800035, 0xffe007ff, nwop_c_ult_h, anyware_inst},
{"c.ole.h", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 16, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x46800036, 0xffe007ff, nwop_c_ole_h, anyware_inst},
{"c.ule.h", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 16, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x46800037, 0xffe007ff, nwop_c_ule_h, anyware_inst},
{"cvt.h.s", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(6, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x46a00020, 0xffff003f, nwop_cvt_h_s, anyware_inst},
{"cvt.s.h", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(6, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x46a00024, 0xffff003f, nwop_cvt_s_h, anyware_inst},
{"fset_hf", OP_BUILD5(5, 16, 0, 0, 0), OP_BUILD5(16, 0, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_IMM, 0, 0, 0), INST_NO_OFFSET, 0, 1, 0xb3e00000, 0xffe00000, nwop_fset_hf, anyware_inst},
{"mov.h", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(6, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0, 0), INST_NO_OFFSET, 0, -1, 0x46a00006, 0xffff003f, nwop_mov_h, anyware_inst},
{"ld_hf", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, 0, 0), INST_NO_OFFSET, 0, 2, 0x98000000, 0xfc000000, nwop_ld_hf, anyware_inst},
{"add.h", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(6, 11, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0), INST_NO_OFFSET, 0, -1, 0x46a00000, 0xffe0003f, nwop_add_h, anyware_inst},
{"sub.h", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(6, 11, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0), INST_NO_OFFSET, 0, -1, 0x46a00001, 0xffe0003f, nwop_sub_h, anyware_inst},
{"mul.h", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(6, 11, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, 0, 0), INST_NO_OFFSET, 0, -1, 0x46a00002, 0xffe0003f, nwop_mul_h, anyware_inst},
{"st_hf", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, 0, 0), INST_NO_OFFSET, 0, 2, 0xa8000000, 0xfc000000, nwop_st_hf, anyware_inst},
{"lb", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, 0, 0), INST_NO_OFFSET, 0, 2, 0x80000000, 0xfc000000, nwop_lb, anyware_inst},
{"lh", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, 0, 0), INST_NO_OFFSET, 0, 2, 0x84000000, 0xfc000000, nwop_lh, anyware_inst},
{"lw", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, 0, 0), INST_NO_OFFSET, 0, 2, 0x8c000000, 0xfc000000, nwop_lw, anyware_inst},
{"lbu", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, 0, 0), INST_NO_OFFSET, 0, 2, 0x90000000, 0xfc000000, nwop_lbu, anyware_inst},
{"lhu", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, 0, 0), INST_NO_OFFSET, 0, 2, 0x94000000, 0xfc000000, nwop_lhu, anyware_inst},
{"sb", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, 0, 0), INST_NO_OFFSET, 0, 2, 0xa0000000, 0xfc000000, nwop_sb, anyware_inst},
{"sh", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, 0, 0), INST_NO_OFFSET, 0, 2, 0xa4000000, 0xfc000000, nwop_sh, anyware_inst},
{"sw", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, 0, 0), INST_NO_OFFSET, 0, 2, 0xac000000, 0xfc000000, nwop_sw, anyware_inst},
{"lwxc1", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(6, 21, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG, ARG_TYPE_REG, 0, 0), INST_NO_OFFSET, 0, -1, 0x4c000000, 0xfc00f83f, nwop_lwxc1, anyware_inst},
{"swxc1", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 21, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG, ARG_TYPE_REG, 0, 0), INST_NO_OFFSET, 0, -1, 0x4c000008, 0xfc0007ff, nwop_swxc1, anyware_inst},
{"lwc1", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG, ARG_TYPE_IMM, 0, 0), INST_NO_OFFSET, 0, 2, 0xc4000000, 0xfc000000, nwop_lwc1, anyware_inst},
{"swc1", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG, ARG_TYPE_IMM, 0, 0), INST_NO_OFFSET, 0, 2, 0xe4000000, 0xfc000000, nwop_swc1, anyware_inst},
{"lwx_v2", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(6, 21, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG, ARG_TYPE_REG, 0, 0), INST_NO_OFFSET, 0, -1, 0x4c000001, 0xfc00f83f, nwop_lwx_v2, anyware_inst},
{"swx_v2", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 21, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG, ARG_TYPE_REG, 0, 0), INST_NO_OFFSET, 0, -1, 0x4c000009, 0xfc0007ff, nwop_swx_v2, anyware_inst},
{"ld_v2", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_FLOAT, ARG_TYPE_REG, ARG_TYPE_IMM, 0, 0), INST_NO_OFFSET, 0, 2, 0xd4000000, 0xfc000000, nwop_ld_v2, anyware_inst},
{"st_v2", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG | ARG_TYPE_REG_VECTOR, ARG_TYPE_REG, ARG_TYPE_IMM, 0, 0), INST_NO_OFFSET, 0, 2, 0xf4000000, 0xfc000000, nwop_st_v2, anyware_inst},
    /* End generated code: opcodes */
    {"nop", 0, 0, 0, 0, 0, -1, 0, 0, 0, alias_and_warn},
    {"", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
};


// Register aliases
struct spl_regiser
{
    char name[20];
    unsigned value;
} special_registers[] = {
    {"ZERO",0},
    {"AT",1},
    {"2",2},
    {"3",3},
    {"4",4},
    {"5",5},
    {"6",6},
    {"7",7},
    {"8",8},
    {"9",9},
    {"10",10},
    {"11",11},
    {"12",12},
    {"13",13},
    {"14",14},
    {"15",15},
    {"16",16},
    {"17",17},
    {"18",18},
    {"19",19},
    {"20",20},
    {"21",21},
    {"22",22},
    {"23",23},
    {"24",24},
    {"25",25},
    {"26",26},
    {"27",27},
    {"GP",28},
    {"SP",29},
    {"FP",30},
    {"RA",31},
    /// NemaCore Single precision HPU Registers
    {"H0",0},
    {"H1",1},
    {"H2",2},
    {"H3",3},
    {"H4",4},
    {"H5",5},
    {"H6",6},
    {"H7",7},
    {"H8",8},
    {"H9",9},
    {"H10",10},
    {"H11",11},
    {"H12",12},
    {"H13",13},
    {"H14",14},
    {"H15",15},
    {"H16",16},
    {"H17",17},
    {"H18",18},
    {"H19",19},
    {"H20",20},
    {"H21",21},
    {"H22",22},
    {"H23",23},
    {"H24",24},
    {"H25",25},
    {"H26",26},
    {"H27",27},
    {"H28",28},
    {"H29",29},
    {"H30",30},
    {"H31",31},

    /// NemaCore Single precision FPU Registers
    {"F0",0},
    {"F1",1},
    {"F2",2},
    {"F3",3},
    {"F4",4},
    {"F5",5},
    {"F6",6},
    {"F7",7},
    {"F8",8},
    {"F9",9},
    {"F10",10},
    {"F11",11},
    {"F12",12},
    {"F13",13},
    {"F14",14},
    {"F15",15},
    {"F16",16},
    {"F17",17},
    {"F18",18},
    {"F19",19},
    {"F20",20},
    {"F21",21},
    {"F22",22},
    {"F23",23},
    {"F24",24},
    {"F25",25},
    {"F26",26},
    {"F27",27},
    {"F28",28},
    {"F29",29},
    {"F30",30},
    {"F31",31},

    /// NemaCore Vector Registers
    {"F0",0},
    {"F2",2},
    {"F4",4},
    {"F6",6},
    {"F8",8},
    {"F10",10},
    {"F12",12},
    {"F14",14},
    {"F16",16},
    {"F18",18},
    {"F20",20},
    {"F22",22},
    {"F24",24},
    {"F26",26},
    {"F28",28},
    {"F30",30},

    {"", 0}
};


/* Prefix for register names.  */
char register_prefix[]         = "r";
char F_register_prefix[]       = "f";
char V_register_prefix[]       = "vV";

char special_register_prefix[] = "spr";
char fsl_register_prefix[]     = "rfsl";
char pvr_register_prefix[]     = "rpvr";

/* #defines for valid immediate range.  */
#define MIN_IMM(op)  (0)
#define MAX_IMM(op)  ((1<<(IMM_SIZE_BITS(op)+1)) - 1)

#define MIN_IMM15 ((int) 0x0000)
#define MAX_IMM15 ((int) 0x7fff)

/* Begin generated code: invalid opcode */
#define INVALID_INST 0xadbeef
/* End generated code: invalid opcode */

extern char* arg_prefix(unsigned rtype);

/* Debuging function */
#define fdd(format, arg...) printf("FD DEBUG: " format "\n", ##arg)


#define SKIP_WORD(s) do{ while ((*s<='z' && *s>='a') || (*s>='A' && *s<='Z')  || (*s>='0' && *s<='9')) \
	    ++ s; }while(0)
#define SKIP_WORD_OR_COMMA(s) do{ while ((*s<='z' && *s>='a') || (*s>='A' && *s<='Z') || (*s>='0' && *s<='9') || *s==',') \
	    if (*(s++) == ',') break; }while(0)

#endif /* NEMAWEAVER_OPC */
