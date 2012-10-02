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


#define INST_TYPE_RD_R1_R2           0
#define INST_TYPE_RD_R1_IMM          1
#define INST_TYPE_RD_R1_UNSIGNED_IMM 2
#define INST_TYPE_RD_R1              3
#define INST_TYPE_RD_R2              4
#define INST_TYPE_RD_IMM             5
#define INST_TYPE_R2                 6
#define INST_TYPE_R1_R2              7
#define INST_TYPE_R1_IMM             8
#define INST_TYPE_IMM                9
#define INST_TYPE_SPECIAL_R1        10
#define INST_TYPE_RD_SPECIAL        11
#define INST_TYPE_R1                12
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
#define ARG_TYPE_REG     0x08
#define ARG_TYPE_REG_SPL 0x04
#define ARG_TYPE_INV     0x00

/* The maximum registers an opcode can have as args. */
#define ARG_MAX_REGS 5
#define ARG_MAX 5

/* Build an int with thes 5 bit values */
#define OP_BUILD5(a0, a1, a2, a3, a4) (((a0)&0x1f)<<0 | ((a1)&0x1f)<<1 | ((a2)&0x1f)<<2 | ((a3)&0x1f)<<3 | ((a4)&0x1f)<<4)
/* Get a field from the above */
#define OP_BREAD5(i, bitfield) (((bitfield)>>(i)) & 0x1f)
/* Get an op's mask */
#define ARG_MASK(i, op)  ((1<<OP_BREAD5(i, op->arg_mask))-1)
#define ARG_SHIFT(i, op) (OP_BREAD5(i, op->arg_shift))
#define ARG_TYPE(i, op)  (OP_BREAD5(i, op->arg_type))
#define ARG_UIMM(op) (ARG_TYPE(op->imm_arg, op) & ARG_TYPE_UIMM)

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

    {"sll",   OP_BUILD5(11, 16, 6, 0, 0), OP_BUILD5(5, 5, 5, 0, 0), OP_BUILD5(ARG_TYPE_REG, ARG_TYPE_REG, ARG_TYPE_REG, 0, 0), INST_NO_OFFSET, NO_DELAY_SLOT, -1,  0x00000000, OPCODE_MAST_MM, sll,    barrel_shift_inst },
    {"", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
};

/* Prefix for register names.  */
char register_prefix[]         = "r";
char F_register_prefix[]       = "f";

char special_register_prefix[] = "spr";
char fsl_register_prefix[]     = "rfsl";
char pvr_register_prefix[]     = "rpvr";

#define arg_prefix(type) ((type) & ARG_TYPE_REG ? register_prefix: "")

/* #defines for valid immediate range.  */
#define MIN_IMM  ((int) 0x80000000)
#define MAX_IMM  ((int) 0x7fffffff)

#define MIN_IMM15 ((int) 0x0000)
#define MAX_IMM15 ((int) 0x7fff)

#endif /* NEMAWEAVER_OPC */
