/* tc-nemaweaver.c -- Assemble code for Xilinx MicroBlaze

   Copyright 2009, 2010 Free Software Foundation.

   This file is part of GAS, the GNU Assembler.

   GAS is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GAS is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GAS; see the file COPYING.  If not, write to the Free
   Software Foundation, 51 Franklin Street - Fifth Floor, Boston, MA
   02110-1301, USA.  */

#include <stdio.h>
#include <assert.h>
#include "as.h"
#include "bfd.h"
#include "subsegs.h"
#define DEFINE_TABLE
#include "../opcodes/nemaweaver-opc.h"
#include "../opcodes/nemaweaver-opcm.h"
#include "safe-ctype.h"
#include <string.h>
#include <dwarf2dbg.h>
#include "aout/stab_gnu.h"
#include "struc-symbol.h"

#ifndef streq
#define streq(a,b) (strcmp (a, b) == 0)
#endif

void nemaweaver_generate_symbol (char *sym);

/* Several places in this file insert raw instructions into the
   object. They should generate the instruction
   and then use these four macros to crack the instruction value into
   the appropriate byte values.  */
#define	INST_BYTE0(x)  (target_big_endian ? (((x) >> 24) & 0xFF) : ( (x)        & 0xFF))
#define	INST_BYTE1(x)  (target_big_endian ? (((x) >> 16) & 0xFF) : (((x) >> 8)  & 0xFF))
#define	INST_BYTE2(x)  (target_big_endian ? (((x) >> 8)  & 0xFF) : (((x) >> 16) & 0xFF))
#define	INST_BYTE3(x)  (target_big_endian ? ( (x)        & 0xFF) : (((x) >> 24) & 0xFF))

/* Options from the command line. */
#define OPTION_LITTLE_ENDIAN OPTION_MD_BASE+1
#define OPTION_BIG_ENDIAN OPTION_MD_BASE+2

/* This array holds the chars that always start a comment.  If the
   pre-processor is disabled, these aren't very useful.  */
const char comment_chars[] = "#";

const char line_separator_chars[] = ";";

/* This array holds the chars that only start a comment at the beginning of
   a line.  */
const char line_comment_chars[] = "#";

const int md_reloc_size = 8; /* Size of relocation record.  */

/* Chars that can be used to separate mant
   from exp in floating point numbers.  */
const char EXP_CHARS[] = "eE";

/* Chars that mean this number is a floating point constant
   As in 0f12.456
   or    0d1.2345e12.  */
const char FLT_CHARS[] = "rRsSfFdDxXpP";

/* INST_PC_OFFSET and INST_NO_OFFSET are 0 and 1.  */
#define UNDEFINED_PC_OFFSET  2
#define DEFINED_ABS_SEGMENT  3
#define DEFINED_PC_OFFSET    4
#define DEFINED_RO_SEGMENT   5
#define DEFINED_RW_SEGMENT   6
#define LARGE_DEFINED_PC_OFFSET 7
#define GOT_OFFSET           8
#define PLT_OFFSET           9
#define GOTOFF_OFFSET        10


/* Initialize the relax table.  */
const relax_typeS md_relax_table[] =
{
    {          1,          1,                0, 0 },  /*  0: Unused.  */
    {          1,          1,                0, 0 },  /*  1: Unused.  */
    {          1,          1,                0, 0 },  /*  2: Unused.  */
    {          1,          1,                0, 0 },  /*  3: Unused.  */
    {      32767,     -32768,   INST_WORD_SIZE, LARGE_DEFINED_PC_OFFSET }, /* 4: DEFINED_PC_OFFSET.  */
    {          1,          1,                0, 0 },                      /*  5: Unused.  */
    {          1,          1,                0, 0 },                      /*  6: Unused.  */
    { 0x7fffffff, 0x80000000, INST_WORD_SIZE*2, 0 },  /*  7: LARGE_DEFINED_PC_OFFSET.  */
    { 0x7fffffff, 0x80000000, INST_WORD_SIZE*2, 0 },  /*  8: GOT_OFFSET.  */
    { 0x7fffffff, 0x80000000, INST_WORD_SIZE*2, 0 },  /*  9: PLT_OFFSET.  */
    { 0x7fffffff, 0x80000000, INST_WORD_SIZE*2, 0 },  /* 10: GOTOFF_OFFSET.  */
};

static struct hash_control * opcode_hash_control;	/* Opcode mnemonics.  */

static segT sbss_segment = 0; 	/* Small bss section.  */
/* static segT sbss2_segment = 0; 	/\* Section not used.  *\/ */
/* static segT sdata_segment = 0; 	/\* Small data section.  *\/ */
/* static segT sdata2_segment = 0; /\* Small read-only section.  *\/ */
static segT rodata_segment = 0; /* read-only section.  */

/* Generate a symbol for stabs information.  */

void
nemaweaver_generate_symbol (char *sym)
{
#define NEMAWEAVER_FAKE_LABEL_NAME "XL0\001"
    static int nemaweaver_label_count;
    sprintf (sym, "%sL%d", NEMAWEAVER_FAKE_LABEL_NAME, nemaweaver_label_count);
    ++nemaweaver_label_count;
}

/* Handle the section changing pseudo-ops. */

static void nemaweaver_s_text (int ignore ATTRIBUTE_UNUSED)
{
#ifdef OBJ_ELF
    obj_elf_text (ignore);
#else
    s_text (ignore);
#endif
}

static void nemaweaver_s_data (int ignore ATTRIBUTE_UNUSED)
{
#ifdef OBJ_ELF
    obj_elf_change_section (".data", SHT_PROGBITS, SHF_ALLOC+SHF_WRITE, 0, 0, 0, 0);
#else
    s_data (ignore);
#endif
}

/* Things in the .sdata segment are always considered to be in the small data section.  */

static void nemaweaver_s_sdata (int ignore ATTRIBUTE_UNUSED)
{
#ifdef OBJ_ELF
    obj_elf_change_section (".sdata", SHT_PROGBITS, SHF_ALLOC+SHF_WRITE, 0, 0, 0, 0);
#else
    s_data (ignore);
#endif
}

/* Pseudo op to make file scope bss items.  */

static void nemaweaver_s_lcomm (int xxx ATTRIBUTE_UNUSED)
{
    char *name;
    char c;
    char *p;
    offsetT size;
    symbolS *symbolP;
    offsetT align;
    char *pfrag;
    int align2;
    segT    current_seg    = now_seg;
    subsegT current_subseg = now_subseg;

    name = input_line_pointer;
    c = get_symbol_end ();

    /* Just after name is now '\0'.  */
    p = input_line_pointer;
    *p = c;
    SKIP_WHITESPACE ();
    if (*input_line_pointer != ',')
    {
	as_bad (_("Expected comma after symbol-name: rest of line ignored."));
	ignore_rest_of_line ();
	return;
    }

    input_line_pointer++;		/* skip ',' */
    if ((size = get_absolute_expression ()) < 0)
    {
	as_warn (_(".COMMon length (%ld.) <0! Ignored."), (long) size);
	ignore_rest_of_line ();
	return;
    }

    /* The third argument to .lcomm is the alignment.  */
    if (*input_line_pointer != ',')
	align = 8;
    else
    {
	++input_line_pointer;
	align = get_absolute_expression ();
	if (align <= 0)
	{
	    as_warn (_("ignoring bad alignment"));
	    align = 8;
	}
    }

    *p = 0;
    symbolP = symbol_find_or_make (name);
    *p = c;

    if (S_IS_DEFINED (symbolP) && ! S_IS_COMMON (symbolP))
    {
	as_bad (_("Ignoring attempt to re-define symbol `%s'."),
		S_GET_NAME (symbolP));
	ignore_rest_of_line ();
	return;
    }

    if (S_GET_VALUE (symbolP) && S_GET_VALUE (symbolP) != (valueT) size)
    {
	as_bad (_("Length of .lcomm \"%s\" is already %ld. Not changed to %ld."),
		S_GET_NAME (symbolP),
		(long) S_GET_VALUE (symbolP),
		(long) size);

	ignore_rest_of_line ();
	return;
    }

    /* Allocate_bss.  */
    if (align)
    {
	/* Convert to a power of 2 alignment.  */
	for (align2 = 0; (align & 1) == 0; align >>= 1, ++align2);
	if (align != 1)
	{
	    as_bad (_("Common alignment not a power of 2"));
	    ignore_rest_of_line ();
	    return;
	}
    }
    else
	align2 = 0;

    record_alignment (current_seg, align2);
    subseg_set (current_seg, current_subseg);
    if (align2)
	frag_align (align2, 0, 0);
    if (S_GET_SEGMENT (symbolP) == current_seg)
	symbol_get_frag (symbolP)->fr_symbol = 0;
    symbol_set_frag (symbolP, frag_now);
    pfrag = frag_var (rs_org, 1, 1, (relax_substateT) 0, symbolP, size,
		      (char *) 0);
    *pfrag = 0;
    S_SET_SIZE    (symbolP, size);
    S_SET_SEGMENT (symbolP, current_seg);
    subseg_set (current_seg, current_subseg);
    demand_empty_rest_of_line ();
}

static void
nemaweaver_s_rdata (int localvar)
{
#ifdef OBJ_ELF
    if (localvar == 0)
    {
	/* rodata.  */
	obj_elf_change_section (".rodata", SHT_PROGBITS, SHF_ALLOC, 0, 0, 0, 0);
	if (rodata_segment == 0)
	    rodata_segment = subseg_new (".rodata", 0);
    }
    else
    {
	/* 1 .sdata2.  */
	obj_elf_change_section (".sdata2", SHT_PROGBITS, SHF_ALLOC, 0, 0, 0, 0);
    }
#else
    s_data (ignore);
#endif
}

static void
nemaweaver_s_bss (int localvar)
{
#ifdef OBJ_ELF
    if (localvar == 0) /* bss.  */
	obj_elf_change_section (".bss", SHT_NOBITS, SHF_ALLOC+SHF_WRITE, 0, 0, 0, 0);
    else if (localvar == 1)
    {
	/* sbss.  */
	obj_elf_change_section (".sbss", SHT_NOBITS, SHF_ALLOC+SHF_WRITE, 0, 0, 0, 0);
	if (sbss_segment == 0)
	    sbss_segment = subseg_new (".sbss", 0);
    }
#else
    s_data (ignore);
#endif
}

/* endp_p is always 1 as this func is called only for .end <funcname>
   This func consumes the <funcname> and calls regular processing
   s_func(1) with arg 1 (1 for end). */

static void
nemaweaver_s_func (int end_p ATTRIBUTE_UNUSED)
{
    *input_line_pointer = get_symbol_end ();
    s_func (1);
}

/* Handle the .weakext pseudo-op as defined in Kane and Heinrich.  */

static void
nemaweaver_s_weakext (int ignore ATTRIBUTE_UNUSED)
{
    char *name;
    int c;
    symbolS *symbolP;
    expressionS exp;

    name = input_line_pointer;
    c = get_symbol_end ();
    symbolP = symbol_find_or_make (name);
    S_SET_WEAK (symbolP);
    *input_line_pointer = c;

    SKIP_WHITESPACE ();

    if (!is_end_of_line[(unsigned char) *input_line_pointer])
    {
	if (S_IS_DEFINED (symbolP))
	{
	    as_bad ("Ignoring attempt to redefine symbol `%s'.",
		    S_GET_NAME (symbolP));
	    ignore_rest_of_line ();
	    return;
	}

	if (*input_line_pointer == ',')
	{
	    ++input_line_pointer;
	    SKIP_WHITESPACE ();
	}

	expression (&exp);
	if (exp.X_op != O_symbol)
	{
	    as_bad ("bad .weakext directive");
	    ignore_rest_of_line ();
	    return;
	}
	symbol_set_value_expression (symbolP, &exp);
    }

    demand_empty_rest_of_line ();
}


static void
nemaweaver_s_set (int xxx ATTRIBUTE_UNUSED)
{
    char *name = input_line_pointer, ch;

    while (!is_end_of_line[(unsigned char) *input_line_pointer])
	++input_line_pointer;
    ch = *input_line_pointer;
    *input_line_pointer = '\0';

    /* if (strcmp(name, "<paramname>")) { *actions* } else ..*/
    if (strchr (name, ',')) {
	/* Generic ".set" directive; use the generic handler.  */
	*input_line_pointer = ch;
	input_line_pointer = name;
	s_set (0);
	return;
    } else {
	;/* as_warn (_("Tried to set unrecognized symbol: %s\n"), name); */
    }
    *input_line_pointer = ch;
    demand_empty_rest_of_line ();
}

/* This table describes all the machine specific pseudo-ops the assembler
   has to support.  The fields are:
   Pseudo-op name without dot
   Function to call to execute this pseudo-op
   Integer arg to pass to the function.  */
/* If the pseudo-op is not found in this table, it searches in the obj-elf.c,
   and then in the read.c table.  */
const pseudo_typeS md_pseudo_table[] =
{
    {"bss", nemaweaver_s_bss, 0},
    {"data", nemaweaver_s_data, 0},
    {"data16", cons, 2},     /* Same as hword.  */
    {"data32", cons, 4},     /* Same as word.  */
    {"data8", cons, 1},      /* Same as byte.  */
    {"end", nemaweaver_s_func, 1}, /* Treat end as function end point.  */
    {"ent", s_func, 0}, /* Treat ent as function entry point.  */
    {"fpu", s_ignore, 0},
    {"frame", s_ignore, 0},
    {"gpword", s_rva, 4}, /* gpword label => store resolved label address in data section.  */
    {"lcomm", nemaweaver_s_lcomm, 1},
    {"mask", s_ignore, 0}, /* Emitted by gcc.  */
    {"rodata", nemaweaver_s_rdata, 0},
    {"sbss", nemaweaver_s_bss, 1},
    {"sdata", nemaweaver_s_sdata, 0},
    {"sdata2", nemaweaver_s_rdata, 1},
    {"text", nemaweaver_s_text, 0},
    {"weakext", nemaweaver_s_weakext, 0},
    {"word", cons, 4},

    {"eabi_attribute", s_ignore, 0}, 	/* Emitted by ARM */
    {"syntax", s_ignore, 0}, 	/* Emited by ARM */
    {"zero", s_ignore, 0}, 	/* Emitted by ARM */

    {"fmask", s_ignore, 0},  	/* Emited my MIPS */

    {"set", nemaweaver_s_set, 0}, /* Modified MIPS */
    {NULL, NULL, 0}
};

/* This function is called once, at assembler startup time.  This should
   set up all the tables, etc that the MD part of the assembler needs.  */

void
md_begin (void)
{
    struct op_code_struct * opcode;

    opcode_hash_control = hash_new ();

    /* Insert unique names into hash table.  */
    for (opcode = opcodes; opcode->name; opcode ++)
	hash_insert (opcode_hash_control, opcode->name, (char *) opcode);
}

struct spl_regiser
{
    char name[20];
    unsigned value;
} special_registers[] = {
    {"zero", 0},
    {"", 0}
};


/* Parse a reg name */
static char* parse_reg (char* s, unsigned* reg, unsigned rtype)
{
    struct spl_regiser *spl;
    char* rprefix = arg_prefix(rtype);

    if (*rprefix == 0) {
	as_fatal (_("Tried to parse an immediate as a register. Check binutils setup of opcodes."));
	return s;
    }

    /* Strip leading whitespace.  */
    while (ISSPACE (* s))
	++ s;

    /* First check for special registers */
    for (spl=special_registers; spl->name[0] != 0; spl++) {
	if (strncasecmp(spl->name, s, strlen(spl->name)) == 0) {
	    *reg = spl->value;
	    return s + strlen(spl->name);
	}
    }

    *reg = 0;
    /* Look for normal registers. */
    if (strchr(rprefix, *s)) {
	char* cursor = s;
	if (ISDIGIT(*(++cursor)) ) {
	    while (ISDIGIT(*cursor)) {
		*reg *= 10;
		*reg += *cursor-'0';
		cursor++;
	    }
	    return cursor;
	}
    } else {
	as_bad (_("Expected one of '%s' as prfix, instead got %c (rest of line: %s)"), rprefix, *s, s);
	SKIP_WORD(s);
	return s;
    }
    as_bad (_("Expected register but instead got %s.6"), s);
    SKIP_WORD(s);
    return s;
}

static char *parse_exp (char *s, expressionS *e)
{
    char *save;
    char *new_pointer;

    /* Skip whitespace.  */
    while (ISSPACE (* s))
	++ s;

    save = input_line_pointer;
    input_line_pointer = s;

    expression (e);

    if (e->X_op == O_absent)
	as_fatal (_("missing operand"));

    new_pointer = input_line_pointer;
    input_line_pointer = save;

    return new_pointer;
}

/* Symbol modifiers (@GOT, @PLT, @GOTOFF).  */
#define IMM_GOT    1
#define IMM_PLT    2
#define IMM_GOTOFF 3

#define IMM_LOWER16 1
#define IMM_HIGHER16 2

static symbolS * GOT_symbol;

#define GOT_SYMBOL_NAME "_GLOBAL_OFFSET_TABLE_"

static unsigned short
parse_imm_flags (char* s)
{
    if (strncmp("lower16",s, sizeof("lower16") -1) == 0) {
	return IMM_LOWER16;
    }
    if (strncmp("higher16",s, sizeof("higher16") -1) == 0) {
	return IMM_HIGHER16;
    }
    as_warn(_("Check your imm prefixes in '%s'"),s);
    return 0;
}

/* Skip lexical whitespaces. I this scoe commas are considered spaces aswell. */
static char* nemaweaver_skip_lspaces(char* s)
{
    char * it;
    for (it = s; *it == ' ' || *it == '\t' || *it == ','; it++);
    return it;
}

static char *
parse_imm(char * s, expressionS * e)
{
    char* it;
    e->X_md = 0;

    for (it = s = nemaweaver_skip_lspaces(s); !IS_SPACE_OR_NUL(*it); it++) {
	if (*it == '%') {
	    if (s != it) e->X_md |= parse_imm_flags(s);
	    s = it+1;
	}
    }

    /* s is now clean of prefixes. */
    s = parse_exp (s, e);	/* XXX: X_md is now our sophisticated version ;) */
    if (e->X_op == O_symbol) {
	e->X_add_symbol->bsym->udata.i = e->X_md;
    }

    return s;
}

static unsigned int
imm_value(const expressionS* e, enum bfd_reloc_code_real rel, int min, int max)
{
    unsigned ret = 0;
    if (e->X_md & IMM_HIGHER16 && e->X_md & IMM_LOWER16)
	as_fatal(_("you can either get the higher16 OR lower16."));
    if (e->X_md & IMM_HIGHER16) {
	return e->X_add_number >> 16;
    } else if (e->X_md & IMM_LOWER16 || rel == BFD_RELOC_16) {
	return e->X_add_number & 0xffff;
    }

    if (rel == BFD_RELOC_NEMAWEAVER_26_JUMP) {
	ret = (e->X_add_number >> 2);
    }

    if ((e->X_op != O_constant && e->X_op != O_symbol))
	as_fatal (_("operand %d is neither a constant nor a symbol. Its X_op is %d"),
		  (int) ret, (int)e->X_op);
    else if ((e->X_op == O_constant) && ((int) ret < min
					 || (int) ret > max))
	as_fatal (_("operand must be absolute in range 0x%x..0x%x, not 0x%x (original value: 0x%x)"),
		  min, max, ret, (unsigned) e->X_add_number);

    return ret;
}

static char *
check_got (int * got_type, int * got_len)
{
    char *new_pointer;
    char *atp;
    char *past_got;
    int first, second;
    char *tmpbuf;

/* Find the start of "@GOT" or "@PLT" suffix (if any).  */
    for (atp = input_line_pointer; *atp != '@'; atp++)
	if (is_end_of_line[(unsigned char) *atp])
	    return NULL;

    if (strncmp (atp + 1, "GOTOFF", 5) == 0)
    {
	*got_len = 6;
	*got_type = IMM_GOTOFF;
    }
    else if (strncmp (atp + 1, "GOT", 3) == 0)
    {
	*got_len = 3;
	*got_type = IMM_GOT;
    }
    else if (strncmp (atp + 1, "PLT", 3) == 0)
    {
	*got_len = 3;
	*got_type = IMM_PLT;
    }
    else
	return NULL;

    if (!GOT_symbol)
	GOT_symbol = symbol_find_or_make (GOT_SYMBOL_NAME);

    first = atp - input_line_pointer;

    past_got = atp + *got_len + 1;
    for (new_pointer = past_got; !is_end_of_line[(unsigned char) *new_pointer++];)
	;
    second = new_pointer - past_got;
    tmpbuf = xmalloc (first + second + 2); /* One extra byte for ' ' and one for NUL.  */
    memcpy (tmpbuf, input_line_pointer, first);
    tmpbuf[first] = ' '; /* @GOTOFF is replaced with a single space.  */
    memcpy (tmpbuf + first + 1, past_got, second);
    tmpbuf[first + second + 1] = '\0';

    return tmpbuf;
}

/* Not called */
__attribute__((unused))
static void
parse_cons_expression_nemaweaver (expressionS *exp, unsigned int size)
{
    /* We do not have 4byte immediates. */
    if (size == 4)
    {
	/* Handle @GOTOFF et.al.  */
	char *save, *gotfree_copy;
	int got_len, got_type;

	save = input_line_pointer;
	gotfree_copy = check_got (& got_type, & got_len);
	if (gotfree_copy)
	    input_line_pointer = gotfree_copy;

	expression (exp);

	if (gotfree_copy)
	{
	    exp->X_md = got_type;
	    input_line_pointer = save + (input_line_pointer - gotfree_copy)
		+ got_len;
	    free (gotfree_copy);
	}
    }
    else
	expression (exp);
}

/* Here we decide which fixups can be adjusted to make them relative to
   the beginning of the section instead of the symbol.  Basically we need
   to make sure that the dynamic relocations are done correctly, so in
   some cases we force the original symbol to be used.  */

/* int */
/* tc_nemaweaver_fix_adjustable (struct fix *fixP) */
/* { */
/*     if (GOT_symbol && fixP->fx_subsy == GOT_symbol) */
/* 	return 0; */

/*     if (fixP->fx_r_type == BFD_RELOC_NEMAWEAVER_64_GOTOFF */
/* 	|| fixP->fx_r_type == BFD_RELOC_NEMAWEAVER_32_GOTOFF */
/* 	|| fixP->fx_r_type == BFD_RELOC_NEMAWEAVER_64_GOT */
/* 	|| fixP->fx_r_type == BFD_RELOC_NEMAWEAVER_64_PLT) */
/* 	return 0; */

/*     return 1; */
/* } */

/* Tell how to relocate the result of the expression. */

static enum bfd_reloc_code_real
get_relocation_type (expressionS* e, struct op_code_struct *op)
{
    if (!IMM_SIZE(op)) {
	return BFD_RELOC_NONE;
    }

    if (IMM_SIZE_BITS(op) == 5) {
	/* We should never need to relocate this. */
	return BFD_RELOC_NONE;
    } else if (IMM_SIZE(op) == 2) {
	if (e->X_md & IMM_LOWER16)
	    return op->inst_offset_type ? BFD_RELOC_NEMAWEAVER_32_LO_PCREL : BFD_RELOC_NEMAWEAVER_32_LO;
	else if (e->X_md & IMM_HIGHER16)
	    return op->inst_offset_type ? BFD_RELOC_NEMAWEAVER_32_HI_PCREL : BFD_RELOC_NEMAWEAVER_32_HI;
	else
	    /* Return 16bit relocation. */
	    /* Not reached for now */
	    return BFD_RELOC_16;
    } else if (IMM_SIZE(op) == 4) {
	return BFD_RELOC_NEMAWEAVER_26_JUMP;
    }
    /* Not reached */
    as_fatal(_("No suitable relocation type found."));
}

void md_assemble(char * str)
{
    char * op_start;
    char * op_end;
    struct op_code_struct * opcode;
    char * output = NULL;
    int nlen = 0;
    unsigned long inst;
    unsigned argument, amask, ashift;
    unsigned reg[ARG_MAX_REGS];
    unsigned isize;
    unsigned arg_index;
    unsigned reg_index;
    expressionS exp;
    char name[20];

    /* Drop leading whitespace.  */
    while (ISSPACE (* str))
	str ++;

    /* Find the op code end. (the opcode name) */
    for (op_start = op_end = str;
	 *op_end && !is_end_of_line[(unsigned char) *op_end] && *op_end != ' ';
	 op_end++)
    {
	name[nlen] = op_start[nlen];
	nlen++;
	if (nlen == sizeof (name) - 1)
	    break;
    }

    name [nlen] = 0;

    if (nlen == 0)
    {
	as_bad (_("can't find opcode "));
	return;
    }

    /* Get the opcode struct. */
    opcode = (struct op_code_struct *) hash_find (opcode_hash_control, name);
    if (opcode == NULL)
    {
	as_bad (_("unknown opcode \"%s\""), name);
	return;
    }


    /* Unmasked bit sequence. */
    inst = opcode->bit_sequence;
    isize = 4;
    reg_index = 0;
    output = frag_more(isize);
    frag_now->fr_opcode = opcode->name;

    /* Read the arguments. */
    for (arg_index = 0; OP_BREAD5(arg_index, opcode->arg_type) != ARG_TYPE_INV && arg_index < ARG_MAX; arg_index++) {
	if (OP_BREAD5(arg_index, opcode->arg_type) & ARG_TYPE_REG) {
	    /* The argument is a register. */
	    if (strcmp (op_end, "")) {
		op_end = parse_reg (op_end + 1, reg + reg_index, OP_BREAD5(arg_index, opcode->arg_type));
		reg_index++;
	    } else {
		as_fatal (_("Error in statement syntax"));
		reg[reg_index] = 0;
	    }

	    argument = reg[reg_index-1];
	} else if (OP_BREAD5(arg_index, opcode->arg_type) & ARG_TYPE_IMM) {
	    unsigned short offset_of_fix = IMM_POS (opcode);
	    unsigned short fix_size = IMM_SIZE (opcode);

	    /* The argument is an imm value */
	    if (strcmp (op_end, ""))
		op_end = parse_imm (op_end, &exp);
	    else
		as_fatal (_("Error in statement syntax"));

	    if (exp.X_op != O_constant)
		fix_new_exp (frag_now,
			     output - frag_now->fr_literal + offset_of_fix, /* where the command in this frag begins + the offset that we want to use */
			     fix_size,
			     &exp,
			     opcode->inst_offset_type,
			     get_relocation_type(&exp, opcode));

	    /* ATTENTION: max imm should always ceil in order to pass jumps (right shifted 26bits). */
	    argument = imm_value(&exp, get_relocation_type(&exp, opcode), MIN_IMM(opcode), MAX_IMM(opcode));
	} else {
	    argument = 0;
	}

	amask = ARG_MASK(arg_index,opcode);
	ashift = ARG_SHIFT(arg_index, opcode);
	inst |= (argument & amask) << ashift;
    }

    /* Clean up whitespaces. */
    while (ISSPACE (* op_end))
	op_end ++;
    if (*op_end) as_warn(_("Ignoring last part of the line '%s'."), op_end);

    /* From opcode and read arguments create a bitfield. */
    output[0] = INST_BYTE0 (inst);
    output[1] = INST_BYTE1 (inst);
    output[2] = INST_BYTE2 (inst);
    output[3] = INST_BYTE3 (inst);

#ifdef OBJ_ELF
    dwarf2_emit_insn (4);
#endif
}

symbolS *
md_undefined_symbol (char * name ATTRIBUTE_UNUSED)
{
    return NULL;
}

/* Various routines to kill one day.  */
/* Equal to MAX_PRECISION in atof-ieee.c */
#define MAX_LITTLENUMS 6

/* Turn a string in input_line_pointer into a floating point constant of type
   type, and store the appropriate bytes in *litP.  The number of LITTLENUMS
   emitted is stored in *sizeP.  An error message is returned, or NULL on OK.*/
char *
md_atof (int type, char * litP, int * sizeP)
{
    int prec;
    LITTLENUM_TYPE words[MAX_LITTLENUMS];
    int    i;
    char * t;

    switch (type)
    {
    case 'f':
    case 'F':
    case 's':
    case 'S':
	prec = 2;
	break;

    case 'd':
    case 'D':
    case 'r':
    case 'R':
	prec = 4;
	break;

    case 'x':
    case 'X':
	prec = 6;
	break;

    case 'p':
    case 'P':
	prec = 6;
	break;

    default:
	*sizeP = 0;
	return _("Bad call to MD_NTOF()");
    }

    t = atof_ieee (input_line_pointer, type, words);

    if (t)
	input_line_pointer = t;

    *sizeP = prec * sizeof (LITTLENUM_TYPE);

    if (! target_big_endian)
    {
	for (i = prec - 1; i >= 0; i--)
	{
	    md_number_to_chars (litP, (valueT) words[i],
				sizeof (LITTLENUM_TYPE));
	    litP += sizeof (LITTLENUM_TYPE);
	}
    }
    else
	for (i = 0; i < prec; i++)
	{
	    md_number_to_chars (litP, (valueT) words[i],
				sizeof (LITTLENUM_TYPE));
	    litP += sizeof (LITTLENUM_TYPE);
	}

    return NULL;
}

const char * md_shortopts = "";

struct option md_longopts[] =
{
    {"EB", no_argument, NULL, OPTION_BIG_ENDIAN},
    {"EL", no_argument, NULL, OPTION_LITTLE_ENDIAN},
    { NULL, no_argument, NULL, 0}
};

size_t md_longopts_size = sizeof (md_longopts);

int md_short_jump_size;

void
md_create_short_jump (char * ptr ATTRIBUTE_UNUSED,
		      addressT from_Nddr ATTRIBUTE_UNUSED,
		      addressT to_Nddr ATTRIBUTE_UNUSED,
		      fragS * frag ATTRIBUTE_UNUSED,
		      symbolS * to_symbol ATTRIBUTE_UNUSED)
{
    as_fatal (_("failed sanity check: short_jump"));
}

void
md_create_long_jump (char * ptr ATTRIBUTE_UNUSED,
		     addressT from_Nddr ATTRIBUTE_UNUSED,
		     addressT to_Nddr ATTRIBUTE_UNUSED,
		     fragS * frag ATTRIBUTE_UNUSED,
		     symbolS * to_symbol ATTRIBUTE_UNUSED)
{
    as_fatal (_("failed sanity check: long_jump"));
}

/* Called in cvt_frag_to_fill which is called inside size_seg. Size
 * seg basically works through this to inform the segments of their
 * own sizes. Since we have a fix now fr_var is fr_var is useless.
 *
 * This creates fixes
 *
 * This is called only if we have an unresolved symbol that we must create a fix for.
 *
 * XXX: we do not need to grow all we */

void
md_convert_frag (bfd * abfd ATTRIBUTE_UNUSED,
		 segT sec ATTRIBUTE_UNUSED,
		 fragS * fragP)
{
    fixS *fixP;
    enum bfd_reloc_code_real relocation_type;

    switch (fragP->fr_symbol->bsym->udata.i) {
    case IMM_LOWER16:
	relocation_type = BFD_RELOC_NEMAWEAVER_32_LO;
	break;
    case IMM_HIGHER16:
	relocation_type = BFD_RELOC_NEMAWEAVER_32_HI;
	break;
    default:
	relocation_type = 0;
    }

    fixP = fix_new (fragP,	/* Fragment (fx_frag) */
		    fragP->fr_fix, /* Where to put the fix realtive to the beginning (fx_offset) */
		    fragP->fr_var, /* How big is the fix (fx_size) */
		    fragP->fr_symbol, /* The symbol to substitute (fx_addsy) */
		    fragP->fr_offset, /* the offset (fx_offset) */
		    fragP->fr_subtype, /* are we PC relative? (fx_pcrel) */
		    relocation_type); /* Reloc type (fx_r_type) */
    fragP->fr_fix += 4;
    fragP->fr_var = 0;
}

/* static void swap(char* c1, char* c2) */
/* { */
/*     char tmp = *c1; */
/*     *c1 = *c2; */
/*     *c2 = tmp; */
/* } */

/* Applies the desired value to the specified location.
   Also sets up addends for 'rela' type relocations. This is called for fixes */
void
md_apply_fix (fixS *   fixP,
	      valueT * valp,
	      segT     segment)
{
    char *       buf  = fixP->fx_where + fixP->fx_frag->fr_literal;
    char *buf0, *buf1, *buf2, *buf3;
    /* char *       file = fixP->fx_file ? fixP->fx_file : _("unknown"); */
    /* Note: use offsetT because it is signed, valueT is unsigned.  */
    offsetT      val  = (offsetT) * valp;
    /* struct op_code_struct * opcode1; */
    /* unsigned long inst1; */

    /* fixP->fx_offset is supposed to be set up correctly for all
       symbol relocations.  */
    if (fixP->fx_addsy == NULL)
    {
	if (!fixP->fx_pcrel)
	    fixP->fx_offset = val; /* Absolute relocation.  */
	else
	    fprintf (stderr, "NULL symbol PC-relative relocation? offset = %08x, val = %08x\n",
		     (unsigned int) fixP->fx_offset, (unsigned int) val);
    }

    /* If we aren't adjusting this fixup to be against the section
       symbol, we need to adjust the value.  */
    if (fixP->fx_addsy != NULL)
    {
	if (S_IS_WEAK (fixP->fx_addsy)
	    || (symbol_used_in_reloc_p (fixP->fx_addsy)
		&& (((bfd_get_section_flags (stdoutput,
					     S_GET_SEGMENT (fixP->fx_addsy))
		      & SEC_LINK_ONCE) != 0)
		    || !strncmp (segment_name (S_GET_SEGMENT (fixP->fx_addsy)),
				 ".gnu.linkonce",
				 sizeof (".gnu.linkonce") - 1))))
	{
	    val -= S_GET_VALUE (fixP->fx_addsy);
	    if (val != 0 && ! fixP->fx_pcrel)
	    {
		/* In this case, the bfd_install_relocation routine will
		   incorrectly add the symbol value back in.  We just want
		   the addend to appear in the object file.
		   FIXME: If this makes VALUE zero, we're toast.  */
		val -= S_GET_VALUE (fixP->fx_addsy);
	    }
	}
    }

    /* If the fix is relative to a symbol which is not defined, or not
       in the same segment as the fix, we cannot resolve it here.  */
    /* fixP->fx_addsy is NULL if valp contains the entire relocation.  */
    if (fixP->fx_addsy != NULL
	&& (!S_IS_DEFINED (fixP->fx_addsy)
	    || (S_GET_SEGMENT (fixP->fx_addsy) != segment)))
    {
	fixP->fx_done = 0;
#ifdef OBJ_ELF
	/* For ELF we can just return and let the reloc that will be generated
	   take care of everything.  For COFF we still have to insert 'val'
	   into the insn since the addend field will be ignored.  */
	/* return; */
#endif
    }
    /* All fixups in the text section must be handled in the linker.  */
    else if (segment->flags & SEC_CODE)
	fixP->fx_done = 0;
    else if (!fixP->fx_pcrel && fixP->fx_addsy != NULL)
	fixP->fx_done = 0;
    else
	fixP->fx_done = 1;

    /* For endianness control. Make sure buf0 points at the MSB and
     * buf3 points to the least significant byte. */
    if (target_big_endian) {
	buf0 = buf; buf1 = buf+1; buf2 = buf+2; buf3 = buf+3;
    } else {
	buf0 = buf+3; buf1 = buf+2; buf2 = buf+1; buf3 = buf;
    }

    switch (fixP->fx_r_type)
    {
    case BFD_RELOC_NEMAWEAVER_32_HI:
	val >>= 16;
	/* Fall through */
    case BFD_RELOC_NEMAWEAVER_32_LO:
    case BFD_RELOC_16:
    case BFD_RELOC_NEMAWEAVER_32_LO_PCREL:
	*buf2 = (val >> 8) & 0xff;
	*buf3 = val & 0xff;
	break;
    case BFD_RELOC_NEMAWEAVER_26_JUMP:
	if (val >> 28) {
	    as_fatal(_("Too long jump."));
	}

	val >>= 2; 		/* 32bits -> 30bits, now only use the 3 LSB */
	/* Fall through. */
    case BFD_RELOC_32:
	/* We assume that val plays well with buffer (aka they have 0s
	 * at the areas they are not supposed to write.) */
	*buf0 |= (val >> 24) & 0xff;
	*buf1 |= (val >> 16) & 0xff;
	*buf2 |= (val >> 8) & 0xff;
	*buf3 |= val & 0xff;
	break;
    default:
	as_bad(_("Unrecognized reloc type."));
    }

    if (fixP->fx_addsy == NULL)
    {
	/* This fixup has been resolved.  Create a reloc in case the linker
	   moves code around due to relaxing.  */
	fixP->fx_r_type = BFD_RELOC_NONE;
	fixP->fx_addsy = section_symbol (absolute_section);
    }
    return;
}

/* Never run. */
void
md_operand (expressionS * expressionP)
{
    /* Ignore leading hash symbol, if present.  */
    if (*input_line_pointer == '#')
    {
	input_line_pointer ++;
	expression (expressionP);
    }
}


/* Doesnt seem to get called. */
/* called just before address relaxation, return the length by which a
   fragment must grow to reach it's destination. This should always be
   0. Note that this makes some initializations in the frag aswell. */

int
md_estimate_size_before_relax (fragS * fragP ATTRIBUTE_UNUSED,
			       segT segment_type ATTRIBUTE_UNUSED)
{
    return 4;
}
/*     sbss_segment = bfd_get_section_by_name (stdoutput, ".sbss"); */
/*     sbss2_segment = bfd_get_section_by_name (stdoutput, ".sbss2"); */
/*     sdata_segment = bfd_get_section_by_name (stdoutput, ".sdata"); */
/*     sdata2_segment = bfd_get_section_by_name (stdoutput, ".sdata2"); */

/*     switch (fragP->fr_subtype) */
/*     { */
/*     case INST_PC_OFFSET: */
/* 	/\* Used to be a PC-relative branch.  *\/ */
/* 	if (!fragP->fr_symbol) */
/*         { */
/* 	    /\* We know the abs value: Should never happen.  *\/ */
/* 	    as_bad (_("Absolute PC-relative value in relaxation code.  Assembler error.....")); */
/* 	    abort (); */
/*         } */
/* 	else if ((S_GET_SEGMENT (fragP->fr_symbol) == segment_type)) */
/*         { */
/* 	    fragP->fr_subtype = DEFINED_PC_OFFSET; */
/* 	    /\* Don't know now whether we need an imm instruction.  *\/ */
/* 	    fragP->fr_var = INST_WORD_SIZE; */
/*         } */
/* 	else if (S_IS_DEFINED (fragP->fr_symbol) */
/* 		 && (((S_GET_SEGMENT (fragP->fr_symbol))->flags & SEC_CODE) == 0)) */
/*         { */
/* 	    /\* Cannot have a PC-relative branch to a diff segment.  *\/ */
/* 	    as_bad (_("PC relative branch to label %s which is not in the instruction space"), */
/* 		    S_GET_NAME (fragP->fr_symbol)); */
/* 	    fragP->fr_subtype = UNDEFINED_PC_OFFSET; */
/* 	    fragP->fr_var = INST_WORD_SIZE*2; */
/*         } */
/* 	else */
/* 	{ */
/* 	    fragP->fr_subtype = UNDEFINED_PC_OFFSET; */
/* 	    fragP->fr_var = INST_WORD_SIZE*2; */
/* 	} */
/* 	break; */

/*     case INST_NO_OFFSET: */
/* 	/\* Used to be a reference to somewhere which was unknown.  *\/ */
/* 	if (fragP->fr_symbol) */
/*         { */
/* 	    if (fragP->fr_opcode == NULL) */
/* 	    { */
/* 		/\* Used as an absolute value.  *\/ */
/* 		fragP->fr_subtype = DEFINED_ABS_SEGMENT; */
/* 		/\* Variable part does not change.  *\/ */
/* 		fragP->fr_var = INST_WORD_SIZE*2; */
/*             } */
/* 	    else if (streq (fragP->fr_opcode, str_nemaweaver_ro_anchor)) */
/* 	    { */
/* 		/\* It is accessed using the small data read only anchor.  *\/ */
/* 		if ((S_GET_SEGMENT (fragP->fr_symbol) == &bfd_com_section) */
/* 		    || (S_GET_SEGMENT (fragP->fr_symbol) == sdata2_segment) */
/* 		    || (S_GET_SEGMENT (fragP->fr_symbol) == sbss2_segment) */
/* 		    || (! S_IS_DEFINED (fragP->fr_symbol))) */
/* 		{ */
/* 		    fragP->fr_subtype = DEFINED_RO_SEGMENT; */
/* 		    fragP->fr_var = INST_WORD_SIZE; */
/*                 } */
/* 		else */
/* 		{ */
/* 		    /\* Variable not in small data read only segment accessed */
/* 		       using small data read only anchor.  *\/ */
/* 		    char *file = fragP->fr_file ? fragP->fr_file : _("unknown"); */

/* 		    as_bad_where (file, fragP->fr_line, */
/* 				  _("Variable is accessed using small data read " */
/* 				    "only anchor, but it is not in the small data " */
/* 				    "read only section")); */
/* 		    fragP->fr_subtype = DEFINED_RO_SEGMENT; */
/* 		    fragP->fr_var = INST_WORD_SIZE; */
/*                 } */
/*             } */
/* 	    else if (streq (fragP->fr_opcode, str_nemaweaver_rw_anchor)) */
/* 	    { */
/* 		if ((S_GET_SEGMENT (fragP->fr_symbol) == &bfd_com_section) */
/* 		    || (S_GET_SEGMENT (fragP->fr_symbol) == sdata_segment) */
/* 		    || (S_GET_SEGMENT (fragP->fr_symbol) == sbss_segment) */
/* 		    || (!S_IS_DEFINED (fragP->fr_symbol))) */
/* 	        { */
/* 		    /\* It is accessed using the small data read write anchor.  *\/ */
/* 		    fragP->fr_subtype = DEFINED_RW_SEGMENT; */
/* 		    fragP->fr_var = INST_WORD_SIZE; */
/*                 } */
/* 		else */
/* 		{ */
/* 		    char *file = fragP->fr_file ? fragP->fr_file : _("unknown"); */

/* 		    as_bad_where (file, fragP->fr_line, */
/* 				  _("Variable is accessed using small data read " */
/* 				    "write anchor, but it is not in the small data " */
/* 				    "read write section")); */
/* 		    fragP->fr_subtype = DEFINED_RW_SEGMENT; */
/* 		    fragP->fr_var = INST_WORD_SIZE; */
/*                 } */
/*             } */
/* 	    else */
/* 	    { */
/* 		as_bad (_("Incorrect fr_opcode value in frag.  Internal error.....")); */
/* 		abort (); */
/*             } */
/* 	} */
/* 	else */
/* 	{ */
/* 	    /\* We know the abs value: Should never happen.  *\/ */
/* 	    as_bad (_("Absolute value in relaxation code.  Assembler error.....")); */
/* 	    abort (); */
/* 	} */
/* 	break; */

/*     case UNDEFINED_PC_OFFSET: */
/*     case LARGE_DEFINED_PC_OFFSET: */
/*     case DEFINED_ABS_SEGMENT: */
/*     case GOT_OFFSET: */
/*     case PLT_OFFSET: */
/*     case GOTOFF_OFFSET: */
/* 	fragP->fr_var = INST_WORD_SIZE*2; */
/* 	break; */
/*     case DEFINED_RO_SEGMENT: */
/*     case DEFINED_RW_SEGMENT: */
/*     case DEFINED_PC_OFFSET: */
/* 	fragP->fr_var = INST_WORD_SIZE; */
/* 	break; */
/*     default: */
/* 	abort (); */
/*     } */

/*     return fragP->fr_var; */
/* } */

/* Put number into target byte order.  */

void
md_number_to_chars (char * ptr, valueT use, int nbytes)
{
    if (target_big_endian)
	number_to_chars_bigendian (ptr, use, nbytes);
    else
	number_to_chars_littleendian (ptr, use, nbytes);
}

/* Round up a section size to the appropriate boundary.  */

valueT
md_section_align (segT segment ATTRIBUTE_UNUSED, valueT size)
{
    return size;			/* Byte alignment is fine.  */
}


/* We do not handle pc-relative imms inteligently so this doesnt get
 * called. */
/* The location from which a PC relative jump should be calculated,
   given a PC relative reloc.  */

long
md_pcrel_from_section (fixS * fixp, segT sec ATTRIBUTE_UNUSED)
{
#ifdef OBJ_ELF
    /* If the symbol is undefined or defined in another section
       we leave the add number alone for the linker to fix it later.
       Only account for the PC pre-bump (No PC-pre-bump on the Microblaze). */

    if (fixp->fx_addsy != (symbolS *) NULL
	&& (!S_IS_DEFINED (fixp->fx_addsy)
	    || (S_GET_SEGMENT (fixp->fx_addsy) != sec)))
	return 0;
    else
    {
	/* The case where we are going to resolve things... */
	if (fixp->fx_r_type == BFD_RELOC_64_PCREL)
	    return  fixp->fx_where + fixp->fx_frag->fr_address + INST_WORD_SIZE;
	else
	    return  fixp->fx_where + fixp->fx_frag->fr_address;
    }
#endif
}


#define F(SZ,PCREL)		(((SZ) << 1) + (PCREL))
#define MAP(SZ,PCREL,TYPE)	case F (SZ, PCREL): code = (TYPE); break


/* Generate a reloc for a fixup.  */

arelent *
tc_gen_reloc (asection *seg ATTRIBUTE_UNUSED, fixS *fixP)
{
    arelent *reloc;

    reloc = (arelent *) xmalloc (sizeof (arelent));

    reloc->sym_ptr_ptr = (asymbol **) xmalloc (sizeof (asymbol *));
    *reloc->sym_ptr_ptr = symbol_get_bfdsym (fixP->fx_addsy);
    reloc->address = fixP->fx_frag->fr_address + fixP->fx_where;
    reloc->howto = bfd_reloc_type_lookup (stdoutput, fixP->fx_r_type);
    if (reloc->howto == (reloc_howto_type *) NULL)
    {
	as_bad_where (fixP->fx_file, fixP->fx_line,
		      _("reloc %d not supported by object file format. (Could not find a good howto. Symbol name is: %s)"),
		      (int) fixP->fx_r_type, fixP->fx_addsy->bsym->name);
	return NULL;
    }
    reloc->addend = fixP->fx_offset;

    return reloc;
}

/* Return 0 on failure to find the right option. These options are
 * defined above in md_longopt */
int
md_parse_option (int c, char * arg ATTRIBUTE_UNUSED)
{
    switch (c)
    {
    case OPTION_LITTLE_ENDIAN:
	target_big_endian = 0;
	break;
    case OPTION_BIG_ENDIAN:
	target_big_endian = 1;
	break;
    default:
	return 0;
    }
    return 1;
}

void
md_show_usage (FILE * stream ATTRIBUTE_UNUSED)
{
    fprintf(stream, _("NemaWeaver: no options\n"));
}

/* This fnctionaluty should be in md_assemble. Actually it should be
 * mived here. */
/* Create a fixup for a cons expression.  If parse_cons_expression_nemaweaver
   found a machine specific op in an expression,
   then we create relocs accordingly. */
/* Turn the expression into a fixup if  */
void
cons_fix_new_nemaweaver (fragS * frag,
			 int where,
			 int size,
			 expressionS *exp)
{
    bfd_reloc_code_real_type r = 0;

    if ((exp->X_op == O_subtract) && (exp->X_add_symbol) &&
	(exp->X_op_symbol) && (now_seg != absolute_section) && (size == 4)
	&& (!S_IS_LOCAL (exp->X_op_symbol))) {
	if (exp->X_md == IMM_HIGHER16)
	    r = BFD_RELOC_NEMAWEAVER_32_HI;
	else if (exp->X_md == IMM_LOWER16)
	    r = BFD_RELOC_NEMAWEAVER_32_LO;
	else
	    as_bad(_("A 32bit symbol is being pushed into a smaller space."));
    } else {
	switch (size)
	{
	case 1:
	    r = BFD_RELOC_8;
	    break;
	case 2:
	    r = BFD_RELOC_16;
	    break;
	case 4:
	    /* I am not sure if relocation refers to the entire
	     * instruction or just the fixup. */
	    /* as_bad (_("unsupported BFD relocation size %u"), size); */
	    r = BFD_RELOC_32;
	    break;
	case 8:
	    as_bad (_("unsupported BFD relocation size %u"), size);
	    r = BFD_RELOC_64;
	    break;
	default:
	    as_bad (_("unsupported BFD relocation size %u"), size);
	    r = BFD_RELOC_32;
	    break;
	}
    }
    fix_new_exp (frag, where, size, exp, 0, r);
}

char* arg_prefix(unsigned rtype)
{
    if (rtype & ARG_TYPE_REG)
	if (FLOAT_REG(rtype)) {
	    if (VECTOR_REG(rtype))
	        return V_register_prefix+1;
	    else
		return F_register_prefix;
	} else {
	    if (VECTOR_REG(rtype))
		return V_register_prefix;
	    else
		return register_prefix;
	}
    else
	return "";
}
