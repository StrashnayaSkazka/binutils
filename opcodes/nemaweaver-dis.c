/* Disassemble Think Silicon nemaweaver instructions.

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


#include "sysdep.h"
#define STATIC_TABLE
#define DEFINE_TABLE

#include "dis-asm.h"
#include <strings.h>
#include "nemaweaver-opc.h"
#include "nemaweaver-dis.h"

#define GET_ARGUMENT(i, instr, op)   (get_field (instr, ARG_MASK(i, op), ARG_SHIFT(i, op), ARG_TYPE(i,op)))

/* Return a textual representation of the reg */
static char *
get_field (unsigned long instr, unsigned long mask, unsigned short low, unsigned type)
{
    char tmpstr[25];
    int x = (int)((instr >> low) & mask);

    if (mask+1 == 1<<26) {
	x <<= 2;
    }

    if (arg_prefix(type)[0])
	sprintf (tmpstr, "%c%d", *arg_prefix(type), x);
    else
	sprintf (tmpstr, "0x%x", (unsigned)x);

    return (strdup (tmpstr));
}

/* This is not used but it will be. */
/* static char * */
/* get_field_special (long instr, struct op_code_struct * op) */
/* { */
/*     char tmpstr[25]; */
/*     char spr[6]; */

/*     switch ((((instr & IMM_MASK) >> IMM_LOW) ^ op->immval_mask)) */
/*     { */
/*     case REG_MSR_MASK : */
/* 	strcpy (spr, "msr"); */
/* 	break; */
/*     case REG_PC_MASK : */
/* 	strcpy (spr, "pc"); */
/* 	break; */
/*     case REG_EAR_MASK : */
/* 	strcpy (spr, "ear"); */
/* 	break; */
/*     case REG_ESR_MASK : */
/* 	strcpy (spr, "esr"); */
/* 	break; */
/*     case REG_FSR_MASK : */
/* 	strcpy (spr, "fsr"); */
/* 	break; */
/*     case REG_BTR_MASK : */
/* 	strcpy (spr, "btr"); */
/* 	break; */
/*     case REG_EDR_MASK : */
/* 	strcpy (spr, "edr"); */
/* 	break; */
/*     case REG_PID_MASK : */
/* 	strcpy (spr, "pid"); */
/* 	break; */
/*     case REG_ZPR_MASK : */
/* 	strcpy (spr, "zpr"); */
/* 	break; */
/*     case REG_TLBX_MASK : */
/* 	strcpy (spr, "tlbx"); */
/* 	break; */
/*     case REG_TLBLO_MASK : */
/* 	strcpy (spr, "tlblo"); */
/* 	break; */
/*     case REG_TLBHI_MASK : */
/* 	strcpy (spr, "tlbhi"); */
/* 	break; */
/*     case REG_TLBSX_MASK : */
/* 	strcpy (spr, "tlbsx"); */
/* 	break; */
/*     default : */
/* 	if (((((instr & IMM_MASK) >> IMM_LOW) ^ op->immval_mask) & 0xE000) */
/* 	    == REG_PVR_MASK) */
/*         { */
/* 	    sprintf (tmpstr, "%spvr%d", register_prefix, */
/* 		     (unsigned short)(((instr & IMM_MASK) >> IMM_LOW) */
/* 				      ^ op->immval_mask) ^ REG_PVR_MASK); */
/* 	    return (strdup (tmpstr)); */
/*         } */
/* 	else */
/* 	    strcpy (spr, "pc"); */
/* 	break; */
/*     } */

/*     sprintf (tmpstr, "%s%s", register_prefix, spr); */
/*     return (strdup (tmpstr)); */
/* } */

static unsigned long read_insn_nemaweaver (bfd_vma memaddr,
					   struct disassemble_info *info,
					   struct op_code_struct **opr)
{
    unsigned char       ibytes[4];
    int                 status;
    struct op_code_struct * op;
    unsigned long inst;

    /* Pull the inst from the blob. */
    status = info->read_memory_func (memaddr, ibytes, 4, info);

    if (status != 0)
    {
	info->memory_error_func (status, memaddr, info);
	return INVALID_INST;
    }

    if (info->endian == BFD_ENDIAN_BIG)
	inst = (ibytes[0] << 24) | (ibytes[1] << 16) | (ibytes[2] << 8) | ibytes[3];
    else if (info->endian == BFD_ENDIAN_LITTLE)
	inst = (ibytes[3] << 24) | (ibytes[2] << 16) | (ibytes[1] << 8) | ibytes[0];
    else
	abort ();

    /* Just a linear search of the table.  */
    for (op = opcodes; op->name != 0; op ++)
	if (op->bit_sequence == (inst & op->opcode_mask))
	    break;

    /* We also have the struct. */
    *opr = op;
    return inst;
}

/* Use read insn_nemaweaver to read and print_func to print. */
int print_insn_nemaweaver (bfd_vma memaddr, struct disassemble_info * info)
{
    fprintf_ftype       print_func = info->fprintf_func;
    void *              stream = info->stream;
    unsigned long       inst, prev_inst;
    struct op_code_struct * op, *pop;
    int                 immval = 0;
    bfd_boolean         immfound = FALSE;
    static bfd_vma      prev_insn_addr = -1; /* Init the prev insn addr.  */
    static int          prev_insn_vma = -1;  /* Init the prev insn vma.  */
    int                 curr_insn_vma = info->buffer_vma;
    int i;

    info->bytes_per_chunk = 4;

    inst = read_insn_nemaweaver (memaddr, info, &op);

    if (inst == INVALID_INST) {
	return -1;
    }

    /* If we are continuing from the last expanded command. */
    if (prev_insn_vma == curr_insn_vma)
    {
    	if (memaddr-(info->bytes_per_chunk) == prev_insn_addr)
        {
    	    prev_inst = read_insn_nemaweaver (prev_insn_addr, info, &pop);
    	    if (prev_inst == INVALID_INST) {
    		return -1;
    	    }
    	    /* This code seems quite legit... I dont see why it is
    	     * commented out */
    	    //if (pop->instr == imm)
    	    //  {
    	    //    immval = (get_int_field_imm (prev_inst) << 16) & 0xffff0000;
    	    //    immfound = TRUE;
    	    //  }
    	    //else
    	    //  {
    	    immval = 0;
    	    immfound = FALSE;
    	    //  }
    	}
    }

    /* Make curr insn as prev insn.  */
    prev_insn_addr = memaddr;
    prev_insn_vma  = curr_insn_vma;

    if ( strlen(op->name) == 0)
	print_func (stream, ".word\t0x%08x", inst);
    else
    {
	print_func (stream, "%s", op->name);

	for (i=0 ; ARG_TYPE(i, op); i++) {
	    print_func (stream, " %s", GET_ARGUMENT(i, inst, op));
	    if (ARG_TYPE(i+1, op))
		print_func (stream, ",");
	}
    }
    /* Say how many bytes we consumed.  */
    return 4;
}

enum nemaweaver_instr
get_insn_nemaweaver (long inst,
  		     bfd_boolean *isunsignedimm,
  		     enum nemaweaver_instr_type *insn_type,
  		     short *delay_slots)
{
    struct op_code_struct * op;
    *isunsignedimm = FALSE;

    /* Just a linear search of the table.  */
    for (op = opcodes; op->name != 0; op ++)
	if (op->bit_sequence == (inst & op->opcode_mask))
	    break;

    if (op->name == 0)
	return invalid_inst;
    else
    {
	*isunsignedimm = ARG_UIMM(op);
	*insn_type = op->instr_type;
	*delay_slots = op->delay_slots;
	return op->instr;
    }
}
