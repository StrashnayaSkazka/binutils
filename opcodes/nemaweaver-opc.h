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

#define SKIP_RANGE_CHECKS

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

#define ARG_IS(opcode, i, val) (OP_BREAD5((i), (opcode)->arg_type) == (unsigned)(val))

/* Make sure we loose no information from the chopping. */
#define JUMP_LENGTH_CHECK(val, pcr) (((pcr) && ((unsigned)(val)&(1<<31))) ? (((unsigned)(val)>>27) == 0x1f) : (((unsigned)(val)>>27) == 0x00) )

/* Begin generated code: type constants */
#define ARG_TYPE_REG 0x1
#define ARG_TYPE_REG_FLOAT 0x2
#define ARG_TYPE_REG_VECTOR 0x3
#define ARG_TYPE_REG_HALF 0x4
#define ARG_TYPE_IMM 0x5
#define ARG_TYPE_UIMM 0x6
#define ARG_TYPE_SIMM 0x7
#define ARG_TYPE_INV 0x8
/* End generated code: type constants */


/* The maximum registers an opcode can have as args. */
#define ARG_MAX_REGS 5
#define ARG_MAX 5

/* Begin generated code: type checks */
#define REG_TYPE(t) ((t) == ARG_TYPE_REG)
#define REG_FLOAT_TYPE(t) ((t) == ARG_TYPE_REG_FLOAT)
#define REG_VECTOR_TYPE(t) ((t) == ARG_TYPE_REG_VECTOR)
#define REG_HALF_TYPE(t) ((t) == ARG_TYPE_REG_HALF)
#define IMM_TYPE(t) ((t) == ARG_TYPE_IMM)
#define UIMM_TYPE(t) ((t) == ARG_TYPE_UIMM)
#define SIMM_TYPE(t) ((t) == ARG_TYPE_SIMM)
#define INV_TYPE(t) ((t) == ARG_TYPE_INV)
/* End generated code: type checks */

/* Begin generated code: type groups */
#define IMMEDIATE_TYPE(t) (IMM_TYPE(t) || UIMM_TYPE(t) || SIMM_TYPE(t))
#define REGISTER_TYPE(t) (REG_TYPE(t) || REG_FLOAT_TYPE(t) || REG_VECTOR_TYPE(t) || REG_HALF_TYPE(t))
#define INVALID_TYPE(t) (INV_TYPE(t))
#define UNSIGNED_TYPE(t) (IMM_TYPE(t) || UIMM_TYPE(t))
/* End generated code: type groups */

/* Begin generated code: argument groups */
#define IMMEDIATE_ARG(arg_i, opcode) (IMMEDIATE_TYPE( OP_BREAD5((arg_i), (opcode)->arg_type) ))
#define REGISTER_ARG(arg_i, opcode) (REGISTER_TYPE( OP_BREAD5((arg_i), (opcode)->arg_type) ))
#define INVALID_ARG(arg_i, opcode) (INVALID_TYPE( OP_BREAD5((arg_i), (opcode)->arg_type) ))
#define UNSIGNED_ARG(arg_i, opcode) (UNSIGNED_TYPE( OP_BREAD5((arg_i), (opcode)->arg_type) ))
/* End generated code: argument groups */

#define PCREL_IMM(opc) ((opc)->inst_offset_type == INST_PC_OFFSET)
#define SIGNED_IMM(opc) (!UNSIGNED_ARG((opc)->imm_arg, (opc)) || PCREL_IMM(opc))

/* Build an int with thes 5 bit values */
#define OP_BUILD5(a0, a1, a2, a3, a4) (((a0)&0x1f)<<0 | ((a1)&0x1f)<<5 | ((a2)&0x1f)<<10 | ((a3)&0x1f)<<15 | ((a4)&0x1f)<<20)
/* Get a field from the above */
#define OP_BREAD5(i, bitfield) (((bitfield)>>((i)*5)) & 0x1f)
/* Get an op's mask */
#define ARG_MASK(i, op)  ((1<<OP_BREAD5((i), (op)->arg_mask))-1)
#define ARG_SHIFT(i, op) (OP_BREAD5((i), (op)->arg_shift))
#define ARG_TYPE(i, op)  (OP_BREAD5((i), (op)->arg_type))
#define ARG_UIMM(op) (ARG_TYPE((op)->imm_arg, (op)) & ARG_TYPE_UIMM)

/* Get immediate position in bytes. Note that imm_size shouls be ceiled */
#define DIV_CEIL(x,n) (((x)%(n)) ? ((x)/(n)+1) : ((x)/(n)))
#define IMM_SIZE_BITS(op) (OP_BREAD5((op)->imm_arg, (op)->arg_mask))
#define IMM_SIZE(op) DIV_CEIL(IMM_SIZE_BITS(op),8)
#define IMM_BIT_POS(op) (ARG_SHIFT((op)->imm_arg, (op)) + IMM_SIZE(op))
#define IMM_POS(op) (IMM_BIT_POS(op) / 8)

static struct op_code_struct
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
}  opcodes[MAX_OPCODES] __attribute__((unused)) =
{
    /* Begin generated code: opcodes */
{"sll", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 16, 6, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, 2, 0x0, 0xffe0003f, nwop_sll, anyware_inst},
{"srl", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 16, 6, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, 2, 0x2, 0xffe0003f, nwop_srl, anyware_inst},
{"rotr", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 16, 6, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, 2, 0x200002, 0xffe0003f, nwop_rotr, anyware_inst},
{"sra", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 16, 6, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, 2, 0x3, 0xffe0003f, nwop_sra, anyware_inst},
{"sllv", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 16, 21, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x4, 0xfc0007ff, nwop_sllv, anyware_inst},
{"srlv", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 16, 21, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x6, 0xfc0007ff, nwop_srlv, anyware_inst},
{"rotrv", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 16, 21, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46, 0xfc0007ff, nwop_rotrv, anyware_inst},
{"srav", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 16, 21, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x7, 0xfc0007ff, nwop_srav, anyware_inst},
{"mul", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 21, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x70000002, 0xfc0007ff, nwop_mul, anyware_inst},
{"mulu", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 21, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x59, 0xfc0007ff, nwop_mulu, anyware_inst},
{"addu", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 21, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x21, 0xfc0007ff, nwop_addu, anyware_inst},
{"subu", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 21, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x23, 0xfc0007ff, nwop_subu, anyware_inst},
{"mulhiu", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 21, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x259, 0xfc0007ff, nwop_mulhiu, anyware_inst},
{"mulhi", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 21, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x70000003, 0xfc0007ff, nwop_mulhi, anyware_inst},
{"and", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 21, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x24, 0xfc0007ff, nwop_and, anyware_inst},
{"or", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 21, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x25, 0xfc0007ff, nwop_or, anyware_inst},
{"xor", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 21, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x26, 0xfc0007ff, nwop_xor, anyware_inst},
{"nor", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 21, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x27, 0xfc0007ff, nwop_nor, anyware_inst},
{"slt", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 21, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x2a, 0xfc0007ff, nwop_slt, anyware_inst},
{"sltu", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 21, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x2b, 0xfc0007ff, nwop_sltu, anyware_inst},
{"wsbh", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 16, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x7c0000a0, 0xffe007ff, nwop_wsbh, anyware_inst},
{"sbext", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 16, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x7c000420, 0xffe007ff, nwop_sbext, anyware_inst},
{"shext", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 16, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x7c000620, 0xffe007ff, nwop_shext, anyware_inst},
{"movt", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x10001, 0xfc1f07ff, nwop_movt, anyware_inst},
{"movf", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x1, 0xfc1f07ff, nwop_movf, anyware_inst},
{"movz", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 21, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0xa, 0xfc0007ff, nwop_movz, anyware_inst},
{"movn", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 21, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0xb, 0xfc0007ff, nwop_movn, anyware_inst},
{"bltz", OP_BUILD5(5, 16, 0, 0, 0), OP_BUILD5(21, 0, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_PC_OFFSET, 0, 1, 0x4000000, 0xfc1f0000, nwop_bltz, anyware_inst},
{"bltzal", OP_BUILD5(5, 16, 0, 0, 0), OP_BUILD5(21, 0, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_PC_OFFSET, 0, 1, 0x4100000, 0xfc1f0000, nwop_bltzal, anyware_inst},
{"bgez", OP_BUILD5(5, 16, 0, 0, 0), OP_BUILD5(21, 0, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_PC_OFFSET, 0, 1, 0x4010000, 0xfc1f0000, nwop_bgez, anyware_inst},
{"bgezal", OP_BUILD5(5, 16, 0, 0, 0), OP_BUILD5(21, 0, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_PC_OFFSET, 0, 1, 0x4110000, 0xfc1f0000, nwop_bgezal, anyware_inst},
{"jal_a", OP_BUILD5(26, 0, 0, 0, 0), OP_BUILD5(0, 0, 0, 0, 0), OP_BUILD5(ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, 0, 0x68000000, 0xfc000000, nwop_jal_a, anyware_inst},
{"beq", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV), INST_PC_OFFSET, 0, 2, 0x10000000, 0xfc000000, nwop_beq, anyware_inst},
{"bneq", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV), INST_PC_OFFSET, 0, 2, 0x14000000, 0xfc000000, nwop_bneq, anyware_inst},
{"blez", OP_BUILD5(5, 16, 0, 0, 0), OP_BUILD5(21, 0, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_PC_OFFSET, 0, 1, 0x18000000, 0xfc1f0000, nwop_blez, anyware_inst},
{"bgtz", OP_BUILD5(5, 16, 0, 0, 0), OP_BUILD5(21, 0, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_PC_OFFSET, 0, 1, 0x1c000000, 0xfc1f0000, nwop_bgtz, anyware_inst},
{"bc1t", OP_BUILD5(16, 0, 0, 0, 0), OP_BUILD5(0, 0, 0, 0, 0), OP_BUILD5(ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_PC_OFFSET, 0, 0, 0x45010000, 0xffff0000, nwop_bc1t, anyware_inst},
{"bc1f", OP_BUILD5(16, 0, 0, 0, 0), OP_BUILD5(0, 0, 0, 0, 0), OP_BUILD5(ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_PC_OFFSET, 0, 0, 0x45000000, 0xffff0000, nwop_bc1f, anyware_inst},
{"jr", OP_BUILD5(5, 0, 0, 0, 0), OP_BUILD5(21, 0, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x8, 0xfc1fffff, nwop_jr, anyware_inst},
{"jalr", OP_BUILD5(5, 0, 0, 0, 0), OP_BUILD5(21, 0, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0xf809, 0xfc1fffff, nwop_jalr, anyware_inst},
{"j", OP_BUILD5(26, 0, 0, 0, 0), OP_BUILD5(0, 0, 0, 0, 0), OP_BUILD5(ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, 0, 0x8000000, 0xfc000000, nwop_j, anyware_inst},
{"jal", OP_BUILD5(26, 0, 0, 0, 0), OP_BUILD5(0, 0, 0, 0, 0), OP_BUILD5(ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_PC_OFFSET, 0, 0, 0xc000000, 0xfc000000, nwop_jal, anyware_inst},
{"addiu", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, 2, 0x24000000, 0xfc000000, nwop_addiu, anyware_inst},
{"slti", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, 2, 0x28000000, 0xfc000000, nwop_slti, anyware_inst},
{"sltiu", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, 2, 0x2c000000, 0xfc000000, nwop_sltiu, anyware_inst},
{"andi", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, 2, 0x30000000, 0xfc000000, nwop_andi, anyware_inst},
{"ori", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, 2, 0x34000000, 0xfc000000, nwop_ori, anyware_inst},
{"xori", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, 2, 0x38000000, 0xfc000000, nwop_xori, anyware_inst},
{"lui", OP_BUILD5(5, 16, 0, 0, 0), OP_BUILD5(16, 0, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_SIMM, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, 1, 0x3c000000, 0xffe00000, nwop_lui, anyware_inst},
{"add.s", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(6, 11, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG_FLOAT, ARG_TYPE_REG_FLOAT, ARG_TYPE_REG_FLOAT, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46000000, 0xffe0003f, nwop_add_s, anyware_inst},
{"sub.s", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(6, 11, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG_FLOAT, ARG_TYPE_REG_FLOAT, ARG_TYPE_REG_FLOAT, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46000001, 0xffe0003f, nwop_sub_s, anyware_inst},
{"mul.s", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(6, 11, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG_FLOAT, ARG_TYPE_REG_FLOAT, ARG_TYPE_REG_FLOAT, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46000002, 0xffe0003f, nwop_mul_s, anyware_inst},
{"frac.s", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(6, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_FLOAT, ARG_TYPE_REG_FLOAT, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x44000003, 0xffff003f, nwop_frac_s, anyware_inst},
{"frcp", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(6, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_FLOAT, ARG_TYPE_REG_FLOAT, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x44000004, 0xffff003f, nwop_frcp, anyware_inst},
{"sqrt.s", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(6, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_FLOAT, ARG_TYPE_REG_FLOAT, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46000004, 0xffff003f, nwop_sqrt_s, anyware_inst},
{"abs.s", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(6, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_FLOAT, ARG_TYPE_REG_FLOAT, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46000005, 0xffff003f, nwop_abs_s, anyware_inst},
{"neg.s", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(6, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_FLOAT, ARG_TYPE_REG_FLOAT, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46000007, 0xffff003f, nwop_neg_s, anyware_inst},
{"floor.w.s", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(6, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_FLOAT, ARG_TYPE_REG_FLOAT, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46000008, 0xffff003f, nwop_floor_w_s, anyware_inst},
{"ceil.w.s", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(6, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_FLOAT, ARG_TYPE_REG_FLOAT, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46000009, 0xffff003f, nwop_ceil_w_s, anyware_inst},
{"round.w.s", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(6, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_FLOAT, ARG_TYPE_REG_FLOAT, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x4600000a, 0xffff003f, nwop_round_w_s, anyware_inst},
{"madd.s", OP_BUILD5(5, 5, 5, 5, 0), OP_BUILD5(6, 21, 11, 16, 0), OP_BUILD5(ARG_TYPE_REG_FLOAT, ARG_TYPE_REG_FLOAT, ARG_TYPE_REG_FLOAT, ARG_TYPE_REG_FLOAT, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x4c000020, 0xfc00003f, nwop_madd_s, anyware_inst},
{"msub.s", OP_BUILD5(5, 5, 5, 5, 0), OP_BUILD5(6, 21, 11, 16, 0), OP_BUILD5(ARG_TYPE_REG_FLOAT, ARG_TYPE_REG_FLOAT, ARG_TYPE_REG_FLOAT, ARG_TYPE_REG_FLOAT, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x4c000028, 0xfc00003f, nwop_msub_s, anyware_inst},
{"mfc1", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(16, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG_FLOAT, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x44000000, 0xffe007ff, nwop_mfc1, anyware_inst},
{"mtc1", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 16, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_FLOAT, ARG_TYPE_REG, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x44800000, 0xffe007ff, nwop_mtc1, anyware_inst},
{"mov.s", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(6, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_FLOAT, ARG_TYPE_REG_FLOAT, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46000006, 0xffff003f, nwop_mov_s, anyware_inst},
{"movf.s", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(6, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_FLOAT, ARG_TYPE_REG_FLOAT, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46000011, 0xffff003f, nwop_movf_s, anyware_inst},
{"movt.s", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(6, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_FLOAT, ARG_TYPE_REG_FLOAT, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46010011, 0xffff003f, nwop_movt_s, anyware_inst},
{"movz.s", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(6, 11, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG_FLOAT, ARG_TYPE_REG_FLOAT, ARG_TYPE_REG, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46000012, 0xffe0003f, nwop_movz_s, anyware_inst},
{"movn.s", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(6, 11, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG_FLOAT, ARG_TYPE_REG_FLOAT, ARG_TYPE_REG, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46000013, 0xffe0003f, nwop_movn_s, anyware_inst},
{"trunc.w.s", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(6, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_FLOAT, ARG_TYPE_REG_FLOAT, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x4600000d, 0xffff003f, nwop_trunc_w_s, anyware_inst},
{"cvt.s.w", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(6, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_FLOAT, ARG_TYPE_REG_FLOAT, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46800020, 0xffff003f, nwop_cvt_s_w, anyware_inst},
{"c.f.s", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 16, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_FLOAT, ARG_TYPE_REG_FLOAT, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46000030, 0xffe007ff, nwop_c_f_s, anyware_inst},
{"c.un.s", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 16, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_FLOAT, ARG_TYPE_REG_FLOAT, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46000031, 0xffe007ff, nwop_c_un_s, anyware_inst},
{"c.eq.s", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 16, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_FLOAT, ARG_TYPE_REG_FLOAT, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46000032, 0xffe007ff, nwop_c_eq_s, anyware_inst},
{"c.ueq.s", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 16, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_FLOAT, ARG_TYPE_REG_FLOAT, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46000033, 0xffe007ff, nwop_c_ueq_s, anyware_inst},
{"c.olt.s", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 16, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_FLOAT, ARG_TYPE_REG_FLOAT, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46000034, 0xffe007ff, nwop_c_olt_s, anyware_inst},
{"c.ult.s", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 16, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_FLOAT, ARG_TYPE_REG_FLOAT, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46000035, 0xffe007ff, nwop_c_ult_s, anyware_inst},
{"c.ole.s", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 16, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_FLOAT, ARG_TYPE_REG_FLOAT, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46000036, 0xffe007ff, nwop_c_ole_s, anyware_inst},
{"c.ule.s", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 16, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_FLOAT, ARG_TYPE_REG_FLOAT, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46000037, 0xffe007ff, nwop_c_ule_s, anyware_inst},
{"mov.vec", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(6, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_VECTOR, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46c00006, 0xffff003f, nwop_mov_vec, anyware_inst},
{"movf.vec", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(6, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_VECTOR, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46c00011, 0xffff003f, nwop_movf_vec, anyware_inst},
{"movt.vec", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(6, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_VECTOR, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46c10011, 0xffff003f, nwop_movt_vec, anyware_inst},
{"movz.vec", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(6, 11, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_VECTOR, ARG_TYPE_REG, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46c00012, 0xffe0003f, nwop_movz_vec, anyware_inst},
{"movn.vec", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(6, 11, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_VECTOR, ARG_TYPE_REG, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46c00013, 0xffe0003f, nwop_movn_vec, anyware_inst},
{"add.v2", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(6, 11, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_VECTOR, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46c00000, 0xffe0003f, nwop_add_v2, anyware_inst},
{"sub.v2", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(6, 11, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_VECTOR, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46c00001, 0xffe0003f, nwop_sub_v2, anyware_inst},
{"mul.v2", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(6, 11, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_VECTOR, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46c00002, 0xffe0003f, nwop_mul_v2, anyware_inst},
{"abs.v2", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(6, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_VECTOR, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46c00005, 0xffff003f, nwop_abs_v2, anyware_inst},
{"neg.v2", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(6, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_VECTOR, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46c00007, 0xffff003f, nwop_neg_v2, anyware_inst},
{"madd.v2", OP_BUILD5(5, 5, 5, 5, 0), OP_BUILD5(6, 21, 11, 16, 0), OP_BUILD5(ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_VECTOR, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x4c000026, 0xfc00003f, nwop_madd_v2, anyware_inst},
{"add.v4", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(6, 11, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_VECTOR, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46400000, 0xffe0003f, nwop_add_v4, anyware_inst},
{"sub.v4", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(6, 11, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_VECTOR, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46400001, 0xffe0003f, nwop_sub_v4, anyware_inst},
{"mul.v4", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(6, 11, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_VECTOR, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46400002, 0xffe0003f, nwop_mul_v4, anyware_inst},
{"abs.v4", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(6, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_VECTOR, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46400005, 0xffff003f, nwop_abs_v4, anyware_inst},
{"neg.v4", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(6, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_VECTOR, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46400007, 0xffff003f, nwop_neg_v4, anyware_inst},
{"madd.v4", OP_BUILD5(5, 5, 5, 5, 0), OP_BUILD5(6, 21, 11, 16, 0), OP_BUILD5(ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_VECTOR, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x4c000027, 0xfc00003f, nwop_madd_v4, anyware_inst},
{"f_extr2", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(16, 11, 6, 0, 0), OP_BUILD5(ARG_TYPE_REG_FLOAT, ARG_TYPE_REG_VECTOR, ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, 2, 0x77c00007, 0xffe0003f, nwop_f_extr2, anyware_inst},
{"f_ins2", OP_BUILD5(5, 5, 5, 5, 0), OP_BUILD5(16, 11, 21, 6, 0), OP_BUILD5(ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_FLOAT, ARG_TYPE_IMM, ARG_TYPE_INV), INST_NO_OFFSET, 0, 3, 0x7c000007, 0xfc00003f, nwop_f_ins2, anyware_inst},
{"vbuild2", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(6, 11, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_FLOAT, ARG_TYPE_REG_FLOAT, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x7780000b, 0xffe0003f, nwop_vbuild2, anyware_inst},
{"f_ins4", OP_BUILD5(5, 5, 5, 5, 0), OP_BUILD5(16, 11, 21, 6, 0), OP_BUILD5(ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_HALF, ARG_TYPE_IMM, ARG_TYPE_INV), INST_NO_OFFSET, 0, 3, 0x70000009, 0xfc00003f, nwop_f_ins4, anyware_inst},
{"f_extr4", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(16, 11, 6, 0, 0), OP_BUILD5(ARG_TYPE_REG_HALF, ARG_TYPE_REG_VECTOR, ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, 2, 0x77400007, 0xffe0003f, nwop_f_extr4, anyware_inst},
{"vbuild4", OP_BUILD5(5, 5, 5, 5, 0), OP_BUILD5(6, 21, 11, 16, 0), OP_BUILD5(ARG_TYPE_REG_HALF, ARG_TYPE_REG_HALF, ARG_TYPE_REG_HALF, ARG_TYPE_REG_HALF, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x7800000b, 0xfc00003f, nwop_vbuild4, anyware_inst},
{"fset", OP_BUILD5(5, 16, 0, 0, 0), OP_BUILD5(16, 0, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_FLOAT, ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, 1, 0x77e00000, 0xffe00000, nwop_fset, anyware_inst},
{"pixout", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(21, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG_VECTOR, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x7000000c, 0xfc1f07ff, nwop_pixout, anyware_inst},
{"waket", OP_BUILD5(5, 0, 0, 0, 0), OP_BUILD5(21, 0, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x408, 0xfc1fffff, nwop_waket, anyware_inst},
{"tmove", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(16, 21, 6, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, 2, 0x7c000014, 0xfc00f83f, nwop_tmove, anyware_inst},
{"invalid", OP_BUILD5(0, 0, 0, 0, 0), OP_BUILD5(0, 0, 0, 0, 0), OP_BUILD5(ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0xadbeef, 0xffffffff, nwop_invalid, anyware_inst},
{"fork", OP_BUILD5(5, 5, 5, 5, 0), OP_BUILD5(11, 21, 16, 6, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, ARG_TYPE_INV), INST_NO_OFFSET, 0, 3, 0x7c000008, 0xfc00003f, nwop_fork, anyware_inst},
{"yield", OP_BUILD5(5, 0, 0, 0, 0), OP_BUILD5(21, 0, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x7c000009, 0xfc1fffff, nwop_yield, anyware_inst},
{"barrier", OP_BUILD5(5, 0, 0, 0, 0), OP_BUILD5(6, 0, 0, 0, 0), OP_BUILD5(ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, 0, 0x70000018, 0xfffff83f, nwop_barrier, anyware_inst},
{"wr_hwr", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(21, 16, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x60000000, 0xfc00ffff, nwop_wr_hwr, anyware_inst},
{"rd_hwr", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(21, 16, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x64000000, 0xfc00ffff, nwop_rd_hwr, anyware_inst},
{"magic", OP_BUILD5(5, 0, 0, 0, 0), OP_BUILD5(6, 0, 0, 0, 0), OP_BUILD5(ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, 0, 0xdead0000, 0xfffff83f, nwop_magic, anyware_inst},
{"c.f.h", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 16, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_HALF, ARG_TYPE_REG_HALF, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46800030, 0xffe007ff, nwop_c_f_h, anyware_inst},
{"c.un.h", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 16, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_HALF, ARG_TYPE_REG_HALF, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46800031, 0xffe007ff, nwop_c_un_h, anyware_inst},
{"c.eq.h", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 16, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_HALF, ARG_TYPE_REG_HALF, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46800032, 0xffe007ff, nwop_c_eq_h, anyware_inst},
{"c.ueq.h", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 16, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_HALF, ARG_TYPE_REG_HALF, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46800033, 0xffe007ff, nwop_c_ueq_h, anyware_inst},
{"c.olt.h", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 16, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_HALF, ARG_TYPE_REG_HALF, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46800034, 0xffe007ff, nwop_c_olt_h, anyware_inst},
{"c.ult.h", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 16, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_HALF, ARG_TYPE_REG_HALF, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46800035, 0xffe007ff, nwop_c_ult_h, anyware_inst},
{"c.ole.h", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 16, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_HALF, ARG_TYPE_REG_HALF, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46800036, 0xffe007ff, nwop_c_ole_h, anyware_inst},
{"c.ule.h", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(11, 16, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_HALF, ARG_TYPE_REG_HALF, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46800037, 0xffe007ff, nwop_c_ule_h, anyware_inst},
{"cvt.h.s", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(6, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_FLOAT, ARG_TYPE_REG_HALF, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46a00020, 0xffff003f, nwop_cvt_h_s, anyware_inst},
{"cvt.s.h", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(6, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_HALF, ARG_TYPE_REG_FLOAT, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46a00024, 0xffff003f, nwop_cvt_s_h, anyware_inst},
{"fset_hf", OP_BUILD5(5, 16, 0, 0, 0), OP_BUILD5(16, 0, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_HALF, ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, 1, 0xb3e00000, 0xffe00000, nwop_fset_hf, anyware_inst},
{"mov.h", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(6, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_HALF, ARG_TYPE_REG_HALF, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46a00006, 0xffff003f, nwop_mov_h, anyware_inst},
{"ld_hf", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_HALF, ARG_TYPE_REG, ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, 2, 0x98000000, 0xfc000000, nwop_ld_hf, anyware_inst},
{"add.h", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(6, 11, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG_HALF, ARG_TYPE_REG_HALF, ARG_TYPE_REG_HALF, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46a00000, 0xffe0003f, nwop_add_h, anyware_inst},
{"sub.h", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(6, 11, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG_HALF, ARG_TYPE_REG_HALF, ARG_TYPE_REG_HALF, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46a00001, 0xffe0003f, nwop_sub_h, anyware_inst},
{"mul.h", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(6, 11, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG_HALF, ARG_TYPE_REG_HALF, ARG_TYPE_REG_HALF, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46a00002, 0xffe0003f, nwop_mul_h, anyware_inst},
{"frcp.h", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(6, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_HALF, ARG_TYPE_REG_HALF, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46a00004, 0xffff003f, nwop_frcp_h, anyware_inst},
{"neg.h", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(6, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_HALF, ARG_TYPE_REG_HALF, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x46a00007, 0xffff003f, nwop_neg_h, anyware_inst},
{"st_hf", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_HALF, ARG_TYPE_REG, ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, 2, 0xa8000000, 0xfc000000, nwop_st_hf, anyware_inst},
{"lb", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, 2, 0x80000000, 0xfc000000, nwop_lb, anyware_inst},
{"lh", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, 2, 0x84000000, 0xfc000000, nwop_lh, anyware_inst},
{"lw", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, 2, 0x8c000000, 0xfc000000, nwop_lw, anyware_inst},
{"lbu", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, 2, 0x90000000, 0xfc000000, nwop_lbu, anyware_inst},
{"lhu", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, 2, 0x94000000, 0xfc000000, nwop_lhu, anyware_inst},
{"sb", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, 2, 0xa0000000, 0xfc000000, nwop_sb, anyware_inst},
{"sh", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, 2, 0xa4000000, 0xfc000000, nwop_sh, anyware_inst},
{"sw", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, 2, 0xac000000, 0xfc000000, nwop_sw, anyware_inst},
{"lwxc1", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(6, 21, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG_FLOAT, ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x4c000000, 0xfc00f83f, nwop_lwxc1, anyware_inst},
{"swxc1", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 21, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG_FLOAT, ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x4c000008, 0xfc0007ff, nwop_swxc1, anyware_inst},
{"lwc1", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_FLOAT, ARG_TYPE_REG, ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, 2, 0xc4000000, 0xfc000000, nwop_lwc1, anyware_inst},
{"swc1", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_FLOAT, ARG_TYPE_REG, ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, 2, 0xe4000000, 0xfc000000, nwop_swc1, anyware_inst},
{"lwx_vec", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(6, 21, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG_VECTOR, ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x4c000001, 0xfc00f83f, nwop_lwx_vec, anyware_inst},
{"swx_vec", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(11, 21, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG_VECTOR, ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x4c000009, 0xfc0007ff, nwop_swx_vec, anyware_inst},
{"ld_vec", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_VECTOR, ARG_TYPE_REG, ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, 2, 0xd4000000, 0xfc000000, nwop_ld_vec, anyware_inst},
{"st_vec", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_VECTOR, ARG_TYPE_REG, ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, 2, 0xf4000000, 0xfc000000, nwop_st_vec, anyware_inst},
{"f_ins4f", OP_BUILD5(5, 5, 5, 5, 0), OP_BUILD5(16, 11, 21, 6, 0), OP_BUILD5(ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_FLOAT, ARG_TYPE_IMM, ARG_TYPE_INV), INST_NO_OFFSET, 0, 3, 0x7c00000a, 0xfc00003f, nwop_f_ins4f, anyware_inst},
{"vbuild4f", OP_BUILD5(5, 5, 5, 5, 0), OP_BUILD5(6, 21, 11, 16, 0), OP_BUILD5(ARG_TYPE_REG_FLOAT, ARG_TYPE_REG_FLOAT, ARG_TYPE_REG_FLOAT, ARG_TYPE_REG_FLOAT, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x7c00003f, 0xfc00003f, nwop_vbuild4f, anyware_inst},
{"f_extr4f", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(16, 11, 6, 0, 0), OP_BUILD5(ARG_TYPE_REG_FLOAT, ARG_TYPE_REG_VECTOR, ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, 2, 0x77c00009, 0xffe0003f, nwop_f_extr4f, anyware_inst},
{"ld_lvec", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_VECTOR, ARG_TYPE_REG, ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, 2, 0xd8000000, 0xfc000000, nwop_ld_lvec, anyware_inst},
{"st_lvec", OP_BUILD5(5, 5, 16, 0, 0), OP_BUILD5(16, 21, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_VECTOR, ARG_TYPE_REG, ARG_TYPE_IMM, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, 2, 0xf8000000, 0xfc000000, nwop_st_lvec, anyware_inst},
{"add.v4f", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(6, 11, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_VECTOR, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x47400000, 0xffe0003f, nwop_add_v4f, anyware_inst},
{"sub.v4f", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(6, 11, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_VECTOR, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x47400001, 0xffe0003f, nwop_sub_v4f, anyware_inst},
{"mul.v4f", OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(6, 11, 16, 0, 0), OP_BUILD5(ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_VECTOR, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x47400002, 0xffe0003f, nwop_mul_v4f, anyware_inst},
{"abs.v4f", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(6, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_VECTOR, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x47400005, 0xffff003f, nwop_abs_v4f, anyware_inst},
{"neg.v4f", OP_BUILD5(5, 5, 0, 0, 0), OP_BUILD5(6, 11, 0, 0, 0), OP_BUILD5(ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_VECTOR, ARG_TYPE_INV, ARG_TYPE_INV, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x47400007, 0xffff003f, nwop_neg_v4f, anyware_inst},
{"madd.v4f", OP_BUILD5(5, 5, 5, 5, 0), OP_BUILD5(6, 21, 11, 16, 0), OP_BUILD5(ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_VECTOR, ARG_TYPE_REG_VECTOR, ARG_TYPE_INV), INST_NO_OFFSET, 0, -1, 0x4c000029, 0xfc00003f, nwop_madd_v4f, anyware_inst},
    /* End generated code: opcodes */
    {"", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
};


// Register aliases
static struct spl_regiser
{
    char name[20];
    unsigned value;
} special_registers[] __attribute__ ((unused)) = {
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
    {"H32",32},
    {"H33",33},
    {"H34",34},
    {"H35",35},
    {"H36",36},
    {"H37",37},
    {"H38",38},
    {"H39",39},
    {"H40",40},
    {"H41",41},
    {"H42",42},
    {"H43",43},
    {"H44",44},
    {"H45",45},
    {"H46",46},
    {"H47",47},
    {"H48",48},
    {"H49",49},
    {"H50",50},
    {"H51",51},
    {"H52",52},
    {"H53",53},
    {"H54",54},
    {"H55",55},
    {"H56",56},
    {"H57",57},
    {"H58",58},
    {"H59",59},
    {"H60",60},
    {"H61",61},
    {"H62",62},
    {"H63",63},

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


/* Begin generated code: type prefixes */
static char reg_prefix[] __attribute__ ((unused)) = "r";
static char reg_float_prefix[] __attribute__ ((unused)) = "f";
static char reg_vector_prefix[] __attribute__ ((unused)) = "v";
static char reg_half_prefix[] __attribute__ ((unused)) = "h";
static char imm_prefix[] __attribute__ ((unused)) = "";
static char uimm_prefix[] __attribute__ ((unused)) = "";
static char simm_prefix[] __attribute__ ((unused)) = "";
static char inv_prefix[] __attribute__ ((unused)) = "";
/* End generated code: type prefixes */

/* #defines for valid immediate range.  */
#define MIN_IMM(op)  (SIGNED_IMM(op) ? ((-1)<<(IMM_SIZE_BITS(op) - 1)) : (0))
#define MAX_IMM(op)  (SIGNED_IMM(op) ? ((1<<(IMM_SIZE_BITS(op) - 1)) - 1) : ((1<<(IMM_SIZE_BITS(op)+1)) - 1))

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

#define FORCE_OPCODE_BITFIELD(u,o) (( (u)&(~((o)->opcode_mask)) ) | (o)->bit_sequence)

#endif /* NEMAWEAVER_OPC */
