/* Disassemble Xilinx nemaweaver instructions.

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

#ifndef NEMAWEAVER_DIS_H
#define NEMAWEAVER_DIS_H 1

extern enum nemaweaver_instr nemaweaver_decode_insn (long, int *, int *,
						     int *, int *);
extern enum nemaweaver_instr get_insn_nemaweaver (long, bfd_boolean *,
						  enum nemaweaver_instr_type *,
  		     				  short *);

#endif /* nemaweaver-dis.h */
