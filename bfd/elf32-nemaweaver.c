/* Xilinx MicroBlaze-specific support for 32-bit ELF

   Copyright 2009, 2010, 2011 Free Software Foundation, Inc.

   This file is part of BFD, the Binary File Descriptor library.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the
   Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
   Boston, MA 02110-1301, USA.  */


int dbg = 0;

#include "bfd.h"
#include "sysdep.h"
#include "bfdlink.h"
#include "libbfd.h"
#include "elf-bfd.h"
#include "elf/nemaweaver.h"
#include <assert.h>

#define	USE_RELA	/* Only USE_REL is actually significant, but this is
			   here are a reminder...  */
#define INST_WORD_SIZE 4

#define FUNC_UNUSED __attribute__((unused))

static int ro_small_data_pointer = 0;
static int rw_small_data_pointer = 0;

static reloc_howto_type * nemaweaver_elf_howto_table [(int) R_NEMAWEAVER_max];

static reloc_howto_type nemaweaver_elf_howto_raw[] =
{
    /* This reloc does nothing.  */
    HOWTO (R_NEMAWEAVER_NONE,	/* Type.  */
	   0,			/* Rightshift.  */
	   2,			/* Size (0 = byte, 1 = short, 2 = long).  */
	   32,			/* Bitsize.  */
	   FALSE,		/* PC_relative.  */
	   0,			/* Bitpos.  */
	   complain_overflow_bitfield,  /* Complain on overflow.  */
	   NULL,                  /* Special Function.  */
	   "R_NEMAWEAVER_NONE", 	/* Name.  */
	   FALSE,		/* Partial Inplace.  */
	   0,			/* Source Mask.  */
	   0,			/* Dest Mask.  */
	   FALSE),		/* PC relative offset?  */

    HOWTO (R_NEMAWEAVER_26_JUMP_PCREL,   	/* Type.  */
	   2,			/* Rightshift.  */
	   2,			/* Size (0 = byte, 1 = short, 2 = long).  */
	   26,			/* Bitsize.  */
	   TRUE,			/* PC_relative.  */
	   0,			/* Bitpos.  */
	   complain_overflow_signed, /* Complain on overflow.  */
	   bfd_elf_generic_reloc,	/* Special Function.  */
	   "R_NEMAWEAVER_26_JUMP_PCREL", 	/* Name.  */
	   FALSE,		/* Partial Inplace.  */
	   0,			/* Source Mask.  */
	   0x03ffffff,		/* Dest Mask.  */
	   TRUE), 		/* PC relative offset?  */

    /* TODO: add a jumb fixup for 26bits of address. Fix it if broken */
    HOWTO (R_NEMAWEAVER_26_JUMP,   	/* Type.  */
	   2,			/* Rightshift.  */
	   2,			/* Size (0 = byte, 1 = short, 2 = long).  */
	   26,			/* Bitsize.  */
	   FALSE,			/* PC_relative.  */
	   0,			/* Bitpos.  */
	   complain_overflow_signed, /* Complain on overflow.  */
	   bfd_elf_generic_reloc,	/* Special Function.  */
	   "R_NEMAWEAVER_26_JUMP", 	/* Name.  */
	   FALSE,		/* Partial Inplace.  */
	   0,			/* Source Mask.  */
	   0x03ffffff,		/* Dest Mask.  */
	   FALSE), 		/* PC relative offset?  */

    /* 16 bit jumps/branches */
    HOWTO (R_NEMAWEAVER_16_JUMP_PCREL,   	/* Type.  */
	   2,			/* Rightshift.  */
	   1,			/* Size (0 = byte, 1 = short, 2 = long).  */
	   16,			/* Bitsize.  */
	   TRUE,			/* PC_relative.  */
	   0,			/* Bitpos.  */
	   complain_overflow_signed, /* Complain on overflow.  */
	   bfd_elf_generic_reloc,	/* Special Function.  */
	   "R_NEMAWEAVER_16_JUMP_PCREL", 	/* Name.  */
	   FALSE,		/* Partial Inplace.  */
	   0,			/* Source Mask.  */
	   0x0000ffff,		/* Dest Mask.  */
	   TRUE), 		/* PC relative offset?  */


    /* The high half of a PCREL 32 bit relocation.  */
    HOWTO (R_NEMAWEAVER_32_PCREL_HI,   	/* Type.  */
	   16,			/* Rightshift.  */
	   2,			/* Size (0 = byte, 1 = short, 2 = long).  */
	   16,			/* Bitsize.  */
	   TRUE,			/* PC_relative.  */
	   0,			/* Bitpos.  */
	   complain_overflow_signed, /* Complain on overflow.  */
	   bfd_elf_generic_reloc,	/* Special Function.  */
	   "R_NEMAWEAVER_32_PCREL_HI", 	/* Name.  */
	   FALSE,		/* Partial Inplace.  */
	   0,			/* Source Mask.  */
	   0x0000ffff,		/* Dest Mask.  */
	   TRUE), 		/* PC relative offset?  */

    /* The low half of a PCREL 32 bit relocation.  */
    HOWTO (R_NEMAWEAVER_32_PCREL_LO,   	/* Type.  */
	   0,			/* Rightshift.  */
	   2,			/* Size (0 = byte, 1 = short, 2 = long).  */
	   16,			/* Bitsize.  */
	   TRUE,			/* PC_relative.  */
	   0,			/* Bitpos.  */
	   complain_overflow_signed, /* Complain on overflow.  */
	   bfd_elf_generic_reloc,	/* Special Function.  */
	   "R_NEMAWEAVER_32_PCREL_LO", 	/* Name.  */
	   FALSE,		/* Partial Inplace.  */
	   0,			/* Source Mask.  */
	   0x0000ffff,		/* Dest Mask.  */
	   TRUE), 		/* PC relative offset?  */

    /* A 32 bit absolute relocation.  */
    HOWTO (R_NEMAWEAVER_32,		/* type */
	   0,			/* rightshift */
	   2,			/* size (0 = byte, 1 = short, 2 = long) */
	   32,			/* bitsize */
	   FALSE,			/* pc_relative */
	   0,			/* bitpos */
	   complain_overflow_bitfield, /* complain_on_overflow */
	   bfd_elf_generic_reloc,	/* special_function */
	   "R_NEMAWEAVER_32",		/* name */
	   FALSE,			/* partial_inplace */
	   0x00000000,		/* src_mask */
	   0xffffffff,		/* dst_mask */
	   FALSE),		/* pcrel_offset */

    /* The low half of a 32 bit relocation.  */
    HOWTO (R_NEMAWEAVER_32_LO_SIGNED,	/* Type.  */
	   0,			/* Rightshift.  */
	   2,			/* Size (0 = byte, 1 = short, 2 = long).  */
	   16,			/* Bitsize.  */
	   FALSE,		/* PC_relative.  */
	   0,			/* Bitpos.  */
	   complain_overflow_dont,	/* Complain on overflow.  */
	   bfd_elf_generic_reloc,	/* Special Function.  */
	   "R_NEMAWEAVER_32_LO_SIGNED",	/* Name.  */
	   FALSE,		/* Partial Inplace.  */
	   0,			/* Source Mask.  */
	   0x0000ffff,		/* Dest Mask.  */
	   FALSE), 		/* PC relative offset?  */

    /* The high half of a 32 bit relocation */
    HOWTO (R_NEMAWEAVER_32_HI_SIGNED,	/* Type.  */
	   16,			/* Rightshift.  */
	   1,			/* Size (0 = byte, 1 = short, 2 = long).  */
	   16,			/* Bitsize.  */
	   FALSE,		/* PC_relative.  */
	   0,			/* Bitpos.  */
	   complain_overflow_signed,	/* Complain on overflow.  */
	   bfd_elf_generic_reloc,	/* Special Function.  */
	   "R_NEMAWEAVER_32_HI_SIGNED",	/* Name.  */
	   FALSE,		/* Partial Inplace.  */
	   0,			/* Source Mask.  */
	   0x0000ffff,		/* Dest Mask.  */
	   FALSE), 		/* PC relative offset?  */

    /* The low half of a 32 bit relocation.  */
    HOWTO (R_NEMAWEAVER_32_LO,   /* Type.  */
	   0,			/* Rightshift.  */
	   2,			/* Size (0 = byte, 1 = short, 2 = long).  */
	   16,			/* Bitsize.  */
	   FALSE,		/* PC_relative.  */
	   0,			/* Bitpos.  */
	   complain_overflow_dont, /* Complain on overflow.  */
	   bfd_elf_generic_reloc,/* Special Function.  */
	   "R_NEMAWEAVER_32_LO", /* Name.  */
	   FALSE,		/* Partial Inplace.  */
	   0,			/* Source Mask.  */
	   0x0000ffff,		/* Dest Mask.  */
	   FALSE), 		/* PC relative offset?  */

    /* The high half of a 32 bit relocation */
    HOWTO (R_NEMAWEAVER_32_HI,   /* Type.  */
	   16,			/* Rightshift.  */
	   1,			/* Size (0 = byte, 1 = short, 2 = long).  */
	   16,			/* Bitsize.  */
	   FALSE,		/* PC_relative.  */
	   0,			/* Bitpos.  */
	   complain_overflow_signed, /* Complain on overflow.  */
	   bfd_elf_generic_reloc,/* Special Function.  */
	   "R_NEMAWEAVER_32_HI", /* Name.  */
	   FALSE,		/* Partial Inplace.  */
	   0,			/* Source Mask.  */
	   0x0000ffff,		/* Dest Mask.  */
	   FALSE), 		/* PC relative offset?  */

    /* GNU extension to record C++ vtable hierarchy.  */
    HOWTO (R_NEMAWEAVER_GNU_VTINHERIT, /* Type.  */
	   0,                     /* Rightshift.  */
	   2,                     /* Size (0 = byte, 1 = short, 2 = long).  */
	   0,                     /* Bitsize.  */
	   FALSE,                 /* PC_relative.  */
	   0,                     /* Bitpos.  */
	   complain_overflow_dont,/* Complain on overflow.  */
	   NULL,                  /* Special Function.  */
	   "R_NEMAWEAVER_GNU_VTINHERIT", /* Name.  */
	   FALSE,                 /* Partial Inplace.  */
	   0,                     /* Source Mask.  */
	   0,                     /* Dest Mask.  */
	   FALSE),                /* PC relative offset?  */

    /* GNU extension to record C++ vtable member usage.  */
    HOWTO (R_NEMAWEAVER_GNU_VTENTRY,   /* Type.  */
	   0,                     /* Rightshift.  */
	   2,                     /* Size (0 = byte, 1 = short, 2 = long).  */
	   0,                     /* Bitsize.  */
	   FALSE,                 /* PC_relative.  */
	   0,                     /* Bitpos.  */
	   complain_overflow_dont,/* Complain on overflow.  */
	   _bfd_elf_rel_vtable_reloc_fn,  /* Special Function.  */
	   "R_NEMAWEAVER_GNU_VTENTRY", /* Name.  */
	   FALSE,                 /* Partial Inplace.  */
	   0,                     /* Source Mask.  */
	   0,                     /* Dest Mask.  */
	   FALSE),                /* PC relative offset?  */
};

#ifndef NUM_ELEM
#define NUM_ELEM(a) (sizeof (a) / sizeof (a)[0])
#endif

/* Initialize the nemaweaver_elf_howto_table, so that linear accesses can be done.  */

static void
nemaweaver_elf_howto_init (void)
{
    unsigned int i;

    for (i = NUM_ELEM (nemaweaver_elf_howto_raw); i--;)
    {
	unsigned int type;

	type = nemaweaver_elf_howto_raw[i].type;

	BFD_ASSERT (type < NUM_ELEM (nemaweaver_elf_howto_table));

	nemaweaver_elf_howto_table [type] = & nemaweaver_elf_howto_raw [i];
    }
}

static reloc_howto_type *
nemaweaver_elf_reloc_type_lookup (bfd * abfd ATTRIBUTE_UNUSED,
				  bfd_reloc_code_real_type code)
{
    enum elf_nemaweaver_reloc_type nemaweaver_reloc = R_NEMAWEAVER_NONE;

    switch (code)
    {
    case BFD_RELOC_NONE:
	nemaweaver_reloc = R_NEMAWEAVER_NONE;
	break;
    case BFD_RELOC_NEMAWEAVER_32_HI_PCREL:
	nemaweaver_reloc = R_NEMAWEAVER_32_PCREL_HI;
	break;
    case BFD_RELOC_NEMAWEAVER_32_LO_PCREL:
	nemaweaver_reloc = R_NEMAWEAVER_32_PCREL_LO;
	break;
    case BFD_RELOC_NEMAWEAVER_16_JUMP_PCREL:
	nemaweaver_reloc = R_NEMAWEAVER_16_JUMP_PCREL;
	break;
    case BFD_RELOC_NEMAWEAVER_26_JUMP_PCREL:
	nemaweaver_reloc = R_NEMAWEAVER_26_JUMP_PCREL;
	break;
    case BFD_RELOC_NEMAWEAVER_26_JUMP:
	nemaweaver_reloc = R_NEMAWEAVER_26_JUMP;
	break;
    case BFD_RELOC_NEMAWEAVER_32_HI_SIGNED:
	nemaweaver_reloc = R_NEMAWEAVER_32_HI_SIGNED;
	break;
    case BFD_RELOC_NEMAWEAVER_32_LO_SIGNED:
	nemaweaver_reloc = R_NEMAWEAVER_32_LO_SIGNED;
	break;
    case BFD_RELOC_NEMAWEAVER_32_HI:
	nemaweaver_reloc = R_NEMAWEAVER_32_HI;
	break;
    case BFD_RELOC_NEMAWEAVER_32_LO:
	nemaweaver_reloc = R_NEMAWEAVER_32_LO;
	break;
    case BFD_RELOC_32:
	nemaweaver_reloc = R_NEMAWEAVER_32;
	break;
    case BFD_RELOC_16:
	nemaweaver_reloc = R_NEMAWEAVER_32_LO; /* Treat 16bit stuff as lower parts of 32bits. */
	break;
    case BFD_RELOC_VTABLE_INHERIT:
	nemaweaver_reloc = R_NEMAWEAVER_GNU_VTINHERIT;
	break;
    case BFD_RELOC_VTABLE_ENTRY:
	nemaweaver_reloc = R_NEMAWEAVER_GNU_VTENTRY;
	break;
    default:
	return (reloc_howto_type *) NULL;
    }

    if (!nemaweaver_elf_howto_table [R_NEMAWEAVER_32])
	/* Initialize howto table if needed.  */
	nemaweaver_elf_howto_init ();

    return nemaweaver_elf_howto_table [(int) nemaweaver_reloc];
};

static reloc_howto_type *
nemaweaver_elf_reloc_name_lookup (bfd *abfd ATTRIBUTE_UNUSED,
				  const char *r_name)
{
    unsigned int i;

    for (i = 0; i < NUM_ELEM (nemaweaver_elf_howto_raw); i++)
	if (nemaweaver_elf_howto_raw[i].name != NULL
	    && strcasecmp (nemaweaver_elf_howto_raw[i].name, r_name) == 0)
	    return &nemaweaver_elf_howto_raw[i];

    return NULL;
}

/* Set the howto pointer for a RCE ELF reloc.  */

static void
nemaweaver_elf_info_to_howto (bfd * abfd ATTRIBUTE_UNUSED,
			      arelent * cache_ptr,
			      Elf_Internal_Rela * dst)
{
    if (!nemaweaver_elf_howto_table [R_NEMAWEAVER_32])
	/* Initialize howto table if needed.  */
	nemaweaver_elf_howto_init ();

    BFD_ASSERT (ELF32_R_TYPE (dst->r_info) < (unsigned int) R_NEMAWEAVER_max);

    cache_ptr->howto = nemaweaver_elf_howto_table [ELF32_R_TYPE (dst->r_info)];
}

/* Nemaweaver ELF local labels start with 'L.' or '$L', not '.L'.  */

static bfd_boolean
nemaweaver_elf_is_local_label_name (bfd *abfd, const char *name)
{
    if (name[0] == 'L' && name[1] == '.')
	return TRUE;

    if (name[0] == '$' && name[1] == 'L')
	return TRUE;

    /* With gcc, the labels go back to starting with '.', so we accept
       the generic ELF local label syntax as well.  */
    return _bfd_elf_is_local_label_name (abfd, name);
}

/* The nemaweaver linker (like many others) needs to keep track of
   the number of relocs that it decides to copy as dynamic relocs in
   check_relocs for each symbol. This is so that it can later discard
   them if they are found to be unnecessary.  We store the information
   in a field extending the regular ELF linker hash table.  */

struct elf32_mb_dyn_relocs
{
    struct elf32_mb_dyn_relocs *next;

    /* The input section of the reloc.  */
    asection *sec;

    /* Total number of relocs copied for the input section.  */
    bfd_size_type count;

    /* Number of pc-relative relocs copied for the input section.  */
    bfd_size_type pc_count;
};

/* ELF linker hash entry.  */

struct elf32_mb_link_hash_entry
{
    struct elf_link_hash_entry elf;

    /* Track dynamic relocs copied for this symbol.  */
    struct elf32_mb_dyn_relocs *dyn_relocs;

};

#define elf32_mb_hash_entry(ent) ((struct elf32_mb_link_hash_entry *)(ent))

/* ELF linker hash table.  */

struct elf32_mb_link_hash_table
{
    struct elf_link_hash_table elf;

    /* Short-cuts to get to dynamic linker sections.  */
    asection *sgot;
    asection *sgotplt;
    asection *srelgot;
    asection *splt;
    asection *srelplt;
    asection *sdynbss;
    asection *srelbss;

    /* Small local sym to section mapping cache.  */
    struct sym_cache sym_sec;
};

/* Get the ELF linker hash table from a link_info structure.  */

#define elf32_mb_hash_table(p)						\
    (elf_hash_table_id ((struct elf_link_hash_table *) ((p)->hash))	\
     == NEMAWEAVER_ELF_DATA ? ((struct elf32_mb_link_hash_table *) ((p)->hash)) : NULL)

/* Create an entry in a nemaweaver ELF linker hash table.  */

static struct bfd_hash_entry *
link_hash_newfunc (struct bfd_hash_entry *entry,
		   struct bfd_hash_table *table,
		   const char *string)
{
    /* Allocate the structure if it has not already been allocated by a
       subclass.  */
    if (entry == NULL)
    {
	entry = bfd_hash_allocate (table,
				   sizeof (struct elf32_mb_link_hash_entry));
	if (entry == NULL)
	    return entry;
    }

    /* Call the allocation method of the superclass.  */
    entry = _bfd_elf_link_hash_newfunc (entry, table, string);
    if (entry != NULL)
    {
	struct elf32_mb_link_hash_entry *eh;

	eh = (struct elf32_mb_link_hash_entry *) entry;
	eh->dyn_relocs = NULL;
    }

    return entry;
}

/* Create a mb ELF linker hash table.  */

static struct bfd_link_hash_table *
nemaweaver_elf_link_hash_table_create (bfd *abfd)
{
    struct elf32_mb_link_hash_table *ret;
    bfd_size_type amt = sizeof (struct elf32_mb_link_hash_table);

    ret = (struct elf32_mb_link_hash_table *) bfd_zmalloc (amt);
    if (ret == NULL)
	return NULL;

    if (!_bfd_elf_link_hash_table_init (&ret->elf, abfd, link_hash_newfunc,
					sizeof (struct elf32_mb_link_hash_entry),
					NEMAWEAVER_ELF_DATA))
    {
	free (ret);
	return NULL;
    }

    return &ret->elf.root;
}

/* Set the values of the small data pointers.  */

FUNC_UNUSED
static void
nemaweaver_elf_final_sdp (struct bfd_link_info *info)
{
    struct bfd_link_hash_entry *h;

    h = bfd_link_hash_lookup (info->hash, RO_SDA_ANCHOR_NAME, FALSE, FALSE, TRUE);
    if (h != (struct bfd_link_hash_entry *) NULL
	&& h->type == bfd_link_hash_defined)
	ro_small_data_pointer = (h->u.def.value
				 + h->u.def.section->output_section->vma
				 + h->u.def.section->output_offset);

    h = bfd_link_hash_lookup (info->hash, RW_SDA_ANCHOR_NAME, FALSE, FALSE, TRUE);
    if (h != (struct bfd_link_hash_entry *) NULL
	&& h->type == bfd_link_hash_defined)
	rw_small_data_pointer = (h->u.def.value
				 + h->u.def.section->output_section->vma
				 + h->u.def.section->output_offset);
}

/* This code is taken from elf32-m32r.c
   There is some attempt to make this function usable for many architectures,
   both USE_REL and USE_RELA ['twould be nice if such a critter existed],
   if only to serve as a learning tool.

   The RELOCATE_SECTION function is called by the new ELF backend linker
   to handle the relocations for a section.

   The relocs are always passed as Rela structures; if the section
   actually uses Rel structures, the r_addend field will always be
   zero.

   This function is responsible for adjust the section contents as
   necessary, and (if using Rela relocs and generating a
   relocatable output file) adjusting the reloc addend as
   necessary.

   This function does not have to worry about setting the reloc
   address or the reloc symbol index.

   LOCAL_SYMS is a pointer to the swapped in local symbols.

   LOCAL_SECTIONS is an array giving the section in the input file
   corresponding to the st_shndx field of each local symbol.

   The global hash table entry for the global symbols can be found
   via elf_sym_hashes (input_bfd).

   When generating relocatable output, this function must handle
   STB_LOCAL/STT_SECTION symbols specially.  The output symbol is
   going to be the section symbol corresponding to the output
   section, which means that the addend must be adjusted
   accordingly.  */

static bfd_boolean
nemaweaver_elf_relocate_section (bfd *output_bfd,
			         struct bfd_link_info *info,
			         bfd *input_bfd,
			         asection *input_section,
			         bfd_byte *contents,
			         Elf_Internal_Rela *relocs,
			         Elf_Internal_Sym *local_syms,
			         asection **local_sections)
{
    struct elf32_mb_link_hash_table *htab;
    Elf_Internal_Shdr *symtab_hdr = &elf_tdata (input_bfd)->symtab_hdr;
    struct elf_link_hash_entry **sym_hashes = elf_sym_hashes (input_bfd);
    Elf_Internal_Rela *rel, *relend;
    /* Assume success.  */
    bfd_boolean ret = TRUE;
    asection *sreloc;
    bfd_vma *local_got_offsets;

    if (!nemaweaver_elf_howto_table[R_NEMAWEAVER_max-1])
	nemaweaver_elf_howto_init ();

    htab = elf32_mb_hash_table (info);
    if (htab == NULL)
	return FALSE;

    local_got_offsets = elf_local_got_offsets (input_bfd);

    sreloc = elf_section_data (input_section)->sreloc;

    rel = relocs;
    relend = relocs + input_section->reloc_count;
    for (; rel < relend; rel++)
    {
	int r_type;
	reloc_howto_type *howto;
	unsigned long r_symndx;
	bfd_vma addend = rel->r_addend;
	bfd_vma offset = rel->r_offset;
	struct elf_link_hash_entry *h;
	Elf_Internal_Sym *sym;
	asection *sec;
	const char *sym_name;
	bfd_reloc_status_type r = bfd_reloc_ok;
	const char *errmsg = NULL;
	bfd_boolean unresolved_reloc = FALSE;

	h = NULL;
	r_type = ELF32_R_TYPE (rel->r_info);

	/* Bounds check */
	if (r_type < 0 || r_type >= (int) R_NEMAWEAVER_max)
	{
	    (*_bfd_error_handler) (_("%s: unknown relocation type %d"),
				   bfd_get_filename (input_bfd), (int) r_type);
	    bfd_set_error (bfd_error_bad_value);
	    ret = FALSE;
	    continue;
	}

	howto = nemaweaver_elf_howto_table[r_type];
	r_symndx = ELF32_R_SYM (rel->r_info);

	if (info->relocatable)
	{
	    /* Runtime relocation */
	    /* This is a relocatable link.  We don't have to change
	       anything, unless the reloc is against a section symbol,
	       in which case we have to adjust according to where the
	       section symbol winds up in the output section.  */
	    sec = NULL;
	    if (r_symndx >= symtab_hdr->sh_info)
		/* External symbol.  */
		continue;

	    /* Local symbol.  */
	    sym = local_syms + r_symndx;
	    sym_name = "<local symbol>";
	    /* STT_SECTION: symbol is associated with a section.  */
	    if (ELF_ST_TYPE (sym->st_info) != STT_SECTION)
		/* Symbol isn't associated with a section.  Nothing to do.  */
		continue;

	    sec = local_sections[r_symndx];
	    addend += sec->output_offset + sym->st_value;
#ifndef USE_REL
	    /* This can't be done for USE_REL because it doesn't mean anything
	       and elf_link_input_bfd asserts this stays zero.  */
	    /* rel->r_addend = addend; */
#endif

#ifndef USE_REL
	    /* Addends are stored with relocs.  We're done.  */
	    continue;
#else /* USE_REL */
	    /* If partial_inplace, we need to store any additional addend
	       back in the section.  */
	    if (!howto->partial_inplace)
		continue;
	    /* ??? Here is a nice place to call a special_function like handler.  */
	    r = _bfd_relocate_contents (howto, input_bfd, addend,
					contents + offset);
#endif /* USE_REL */
	}
	else
	{
	    /* Compile time relocation */
	    bfd_vma relocation;

	    /* This is a final link.  */
	    sym = NULL;
	    sec = NULL;
	    unresolved_reloc = FALSE;

	    if (r_symndx < symtab_hdr->sh_info)
	    {
		/* Local symbol.  */
		sym = local_syms + r_symndx;
		sec = local_sections[r_symndx];
		if (sec == 0)
		    continue;
		sym_name = "<local symbol>";
		relocation = _bfd_elf_rela_local_sym (output_bfd, sym, &sec, rel);
		/* r_addend may have changed if the reference section was
		   a merge section.  */
		addend = rel->r_addend;
	    }
	    else
	    {
		/* External symbol.  */
		bfd_boolean warned ATTRIBUTE_UNUSED;

		RELOC_FOR_GLOBAL_SYMBOL (info, input_bfd, input_section, rel,
					 r_symndx, symtab_hdr, sym_hashes,
					 h, sec, relocation,
					 unresolved_reloc, warned);
		sym_name = h->root.root.string;
	    }


	    /* Sanity check the address.  */
	    if (offset > bfd_get_section_limit (input_bfd, input_section))
	    {
		r = bfd_reloc_outofrange;
		goto check_reloc;
	    }

	    if ((r_type == R_NEMAWEAVER_32_PCREL_HI || r_type == R_NEMAWEAVER_32_HI_SIGNED) && (relocation & 0x8000)) {
		relocation += 0x10000;
	    }

	    switch ((int) r_type)
	    {
		// Add relocations' special stuff here.
	    case R_NEMAWEAVER_16_JUMP_PCREL:
		offset--;
		/* Fall through. */
	    default :
		r = _bfd_final_link_relocate (howto, input_bfd, input_section,
					      contents, offset,
					      relocation, addend);
		break;
	    }
	}

    check_reloc:

	if (r != bfd_reloc_ok)
	{
	    /* FIXME: This should be generic enough to go in a utility.  */
	    const char *name;

	    if (h != NULL)
		name = h->root.root.string;
	    else
	    {
		name = (bfd_elf_string_from_elf_section
			(input_bfd, symtab_hdr->sh_link, sym->st_name));
		if (name == NULL || *name == '\0')
		    name = bfd_section_name (input_bfd, sec);
	    }

	    if (errmsg != NULL)
		goto common_error;

	    switch (r)
	    {
	    case bfd_reloc_overflow:
		if (!((*info->callbacks->reloc_overflow)
		      (info, (h ? &h->root : NULL), name, howto->name,
		       (bfd_vma) 0, input_bfd, input_section, offset)))
		    return FALSE;
		break;

	    case bfd_reloc_undefined:
		if (!((*info->callbacks->undefined_symbol)
		      (info, name, input_bfd, input_section, offset, TRUE)))
		    return FALSE;
		break;

	    case bfd_reloc_outofrange:
		errmsg = _("internal error: out of range error");
		goto common_error;

	    case bfd_reloc_notsupported:
		errmsg = _("internal error: unsupported relocation error");
		goto common_error;

	    case bfd_reloc_dangerous:
		errmsg = _("internal error: dangerous error");
		goto common_error;

	    default:
		errmsg = _("internal error: unknown error");
		/* Fall through.  */
	    common_error:
		if (!((*info->callbacks->warning)
		      (info, errmsg, name, input_bfd, input_section, offset)))
		    return FALSE;
		break;
	    }
	}
    }

    return ret;
}

/* Calculate fixup value for reference.  */

FUNC_UNUSED
static int
calc_fixup (bfd_vma addr, asection *sec)
{
    int i, fixup = 0;

    if (sec == NULL || sec->relax == NULL)
	return 0;

    /* Look for addr in relax table, total fixup value.  */
    for (i = 0; i < sec->relax_count; i++)
    {
	if (addr <= sec->relax[i].addr)
	    break;
	fixup += sec->relax[i].size;
    }

    return fixup;
}

/* Return the section that should be marked against GC for a given
   relocation.  */

static asection *
nemaweaver_elf_gc_mark_hook (asection *sec,
			     struct bfd_link_info * info,
     			     Elf_Internal_Rela * rel,
     			     struct elf_link_hash_entry * h,
     			     Elf_Internal_Sym * sym)
{
    if (h != NULL)
	switch (ELF32_R_TYPE (rel->r_info))
	{
	case R_NEMAWEAVER_GNU_VTINHERIT:
	case R_NEMAWEAVER_GNU_VTENTRY:
	    return NULL;
	}

    return _bfd_elf_gc_mark_hook (sec, info, rel, h, sym);
}

/* Update the got entry reference counts for the section being removed.  */

FUNC_UNUSED
static bfd_boolean
nemaweaver_elf_gc_sweep_hook (bfd * abfd ATTRIBUTE_UNUSED,
     			      struct bfd_link_info * info ATTRIBUTE_UNUSED,
     			      asection * sec ATTRIBUTE_UNUSED,
     			      const Elf_Internal_Rela * relocs ATTRIBUTE_UNUSED)
{
    return TRUE;
}

/* PIC support.  */

#define PLT_ENTRY_SIZE 16

#define PLT_ENTRY_WORD_0  0xb0000000          /* "imm 0".  */
#define PLT_ENTRY_WORD_1  0xe9940000          /* "lwi r12,r20,0" - relocated to lwi r12,r20,func@GOT.  */
#define PLT_ENTRY_WORD_1_NOPIC  0xe9800000    /* "lwi r12,r0,0" - non-PIC object.  */
#define PLT_ENTRY_WORD_2  0x98186000          /* "brad r12".  */
#define PLT_ENTRY_WORD_3  0x80000000          /* "nop".  */

/* Create .got, .gotplt, and .rela.got sections in DYNOBJ, and set up
   shortcuts to them in our hash table.  */
FUNC_UNUSED
static bfd_boolean
create_got_section (bfd *dynobj, struct bfd_link_info *info)
{
    struct elf32_mb_link_hash_table *htab;

    if (! _bfd_elf_create_got_section (dynobj, info))
	return FALSE;
    htab = elf32_mb_hash_table (info);
    if (htab == NULL)
	return FALSE;

    htab->sgot = bfd_get_section_by_name (dynobj, ".got");
    htab->sgotplt = bfd_get_section_by_name (dynobj, ".got.plt");
    if (!htab->sgot || !htab->sgotplt)
	return FALSE;

    htab->srelgot = bfd_make_section (dynobj, ".rela.got");
    if (htab->srelgot == NULL
	|| ! bfd_set_section_flags (dynobj, htab->srelgot, SEC_ALLOC
				    | SEC_LOAD
				    | SEC_HAS_CONTENTS
				    | SEC_IN_MEMORY
				    | SEC_LINKER_CREATED
				    | SEC_READONLY)
	|| ! bfd_set_section_alignment (dynobj, htab->srelgot, 2))
	return FALSE;
    return TRUE;
}

/* Look through the relocs for a section during the first phase.  */

static bfd_boolean
nemaweaver_elf_check_relocs (bfd * abfd,
			     struct bfd_link_info * info,
     			     asection * sec,
			     const Elf_Internal_Rela * relocs)
{
    Elf_Internal_Shdr *           symtab_hdr;
    struct elf_link_hash_entry ** sym_hashes;
    struct elf_link_hash_entry ** sym_hashes_end;
    const Elf_Internal_Rela *     rel;
    const Elf_Internal_Rela *     rel_end;
    struct elf32_mb_link_hash_table *htab;
    /* asection *sreloc = NULL; */

    if (info->relocatable)
	return TRUE;

    htab = elf32_mb_hash_table (info);
    if (htab == NULL)
	return FALSE;

    symtab_hdr = & elf_tdata (abfd)->symtab_hdr;
    sym_hashes = elf_sym_hashes (abfd);
    sym_hashes_end = sym_hashes + symtab_hdr->sh_size / sizeof (Elf32_External_Sym);
    if (!elf_bad_symtab (abfd))
	sym_hashes_end -= symtab_hdr->sh_info;

    rel_end = relocs + sec->reloc_count;

    for (rel = relocs; rel < rel_end; rel++)
    {
	unsigned int r_type;
	struct elf_link_hash_entry * h;
	unsigned long r_symndx;

	r_symndx = ELF32_R_SYM (rel->r_info);
	r_type = ELF32_R_TYPE (rel->r_info);

	if (r_symndx < symtab_hdr->sh_info)
	    h = NULL;
	else
	    h = sym_hashes [r_symndx - symtab_hdr->sh_info];

	switch (r_type)
        {
	    /* This relocation describes the C++ object vtable hierarchy.
	       Reconstruct it for later use during GC.  */
        case R_NEMAWEAVER_GNU_VTINHERIT:
	    if (!bfd_elf_gc_record_vtinherit (abfd, sec, h, rel->r_offset))
		return FALSE;
	    break;

	    /* This relocation describes which C++ vtable entries are actually
	       used.  Record for later use during GC.  */
        case R_NEMAWEAVER_GNU_VTENTRY:
	    if (!bfd_elf_gc_record_vtentry (abfd, sec, h, rel->r_addend))
		return FALSE;
	    break;
        }
    }

    return TRUE;
}

/* Not used */
/* Copy the extra info we tack onto an elf_link_hash_entry.  */

static void
nemaweaver_elf_copy_indirect_symbol (struct bfd_link_info *info,
				     struct elf_link_hash_entry *dir,
				     struct elf_link_hash_entry *ind)
{
    struct elf32_mb_link_hash_entry *edir, *eind;

    edir = (struct elf32_mb_link_hash_entry *) dir;
    eind = (struct elf32_mb_link_hash_entry *) ind;

    if (eind->dyn_relocs != NULL)
    {
	if (edir->dyn_relocs != NULL)
	{
	    struct elf32_mb_dyn_relocs **pp;
	    struct elf32_mb_dyn_relocs *p;

	    if (ind->root.type == bfd_link_hash_indirect)
		abort ();

	    /* Add reloc counts against the weak sym to the strong sym
	       list.  Merge any entries against the same section.  */
	    for (pp = &eind->dyn_relocs; (p = *pp) != NULL; )
	    {
		struct elf32_mb_dyn_relocs *q;

		for (q = edir->dyn_relocs; q != NULL; q = q->next)
		    if (q->sec == p->sec)
		    {
			q->pc_count += p->pc_count;
			q->count += p->count;
			*pp = p->next;
			break;
		    }
		if (q == NULL)
		    pp = &p->next;
	    }
	    *pp = edir->dyn_relocs;
	}

	edir->dyn_relocs = eind->dyn_relocs;
	eind->dyn_relocs = NULL;
    }

    _bfd_elf_link_hash_copy_indirect (info, dir, ind);
}
FUNC_UNUSED
/* Not used */
static bfd_boolean
nemaweaver_elf_adjust_dynamic_symbol (struct bfd_link_info *info,
				      struct elf_link_hash_entry *h)
{
    struct elf32_mb_link_hash_table *htab;
    struct elf32_mb_link_hash_entry * eh;
    struct elf32_mb_dyn_relocs *p;
    asection *sdynbss, *s;
    unsigned int power_of_two;
    bfd *dynobj;

    htab = elf32_mb_hash_table (info);
    if (htab == NULL)
	return FALSE;

    /* If this is a function, put it in the procedure linkage table.  We
       will fill in the contents of the procedure linkage table later,
       when we know the address of the .got section.  */
    if (h->type == STT_FUNC
	|| h->needs_plt)
    {
	if (h->plt.refcount <= 0
	    || SYMBOL_CALLS_LOCAL (info, h)
	    || (ELF_ST_VISIBILITY (h->other) != STV_DEFAULT
		&& h->root.type == bfd_link_hash_undefweak))
	{
	    /* This case can occur if we saw a PLT reloc in an input
	       file, but the symbol was never referred to by a dynamic
	       object, or if all references were garbage collected.  In
	       such a case, we don't actually need to build a procedure
	       linkage table, and we can just do a PC32 reloc instead.  */
	    h->plt.offset = (bfd_vma) -1;
	    h->needs_plt = 0;
	}

	return TRUE;
    }
    else
	/* It's possible that we incorrectly decided a .plt reloc was
	   needed for an R_NEMAWEAVER_64_PCREL reloc to a non-function sym in
	   check_relocs.  We can't decide accurately between function and
	   non-function syms in check-relocs;  Objects loaded later in
	   the link may change h->type.  So fix it now.  */
	h->plt.offset = (bfd_vma) -1;

    /* If this is a weak symbol, and there is a real definition, the
       processor independent code will have arranged for us to see the
       real definition first, and we can just use the same value.  */
    if (h->u.weakdef != NULL)
    {
	BFD_ASSERT (h->u.weakdef->root.type == bfd_link_hash_defined
		    || h->u.weakdef->root.type == bfd_link_hash_defweak);
	h->root.u.def.section = h->u.weakdef->root.u.def.section;
	h->root.u.def.value = h->u.weakdef->root.u.def.value;
	return TRUE;
    }

    /* This is a reference to a symbol defined by a dynamic object which
       is not a function.  */

    /* If we are creating a shared library, we must presume that the
       only references to the symbol are via the global offset table.
       For such cases we need not do anything here; the relocations will
       be handled correctly by relocate_section.  */
    if (info->shared)
	return TRUE;

    /* If there are no references to this symbol that do not use the
       GOT, we don't need to generate a copy reloc.  */
    if (!h->non_got_ref)
	return TRUE;

    /* If -z nocopyreloc was given, we won't generate them either.  */
    if (info->nocopyreloc)
    {
	h->non_got_ref = 0;
	return TRUE;
    }

    eh = (struct elf32_mb_link_hash_entry *) h;
    for (p = eh->dyn_relocs; p != NULL; p = p->next)
    {
	s = p->sec->output_section;
	if (s != NULL && (s->flags & SEC_READONLY) != 0)
	    break;
    }

    /* If we didn't find any dynamic relocs in read-only sections, then
       we'll be keeping the dynamic relocs and avoiding the copy reloc.  */
    if (p == NULL)
    {
	h->non_got_ref = 0;
	return TRUE;
    }

    /* We must allocate the symbol in our .dynbss section, which will
       become part of the .bss section of the executable.  There will be
       an entry for this symbol in the .dynsym section.  The dynamic
       object will contain position independent code, so all references
       from the dynamic object to this symbol will go through the global
       offset table.  The dynamic linker will use the .dynsym entry to
       determine the address it must put in the global offset table, so
       both the dynamic object and the regular object will refer to the
       same memory location for the variable.  */

    /* We must generate a R_NEMAWEAVER_COPY reloc to tell the dynamic linker
       to copy the initial value out of the dynamic object and into the
       runtime process image.  */
    dynobj = elf_hash_table (info)->dynobj;
    BFD_ASSERT (dynobj != NULL);
    if ((h->root.u.def.section->flags & SEC_ALLOC) != 0)
    {
	htab->srelbss->size += sizeof (Elf32_External_Rela);
	h->needs_copy = 1;
    }

    /* We need to figure out the alignment required for this symbol.  I
       have no idea how ELF linkers handle this.  */
    power_of_two = bfd_log2 (h->size);
    if (power_of_two > 3)
	power_of_two = 3;

    sdynbss = htab->sdynbss;
    /* Apply the required alignment.  */
    sdynbss->size = BFD_ALIGN (sdynbss->size, (bfd_size_type) (1 << power_of_two));
    if (power_of_two > bfd_get_section_alignment (dynobj, sdynbss))
    {
	if (! bfd_set_section_alignment (dynobj, sdynbss, power_of_two))
	    return FALSE;
    }

    /* Define the symbol as being at this point in the section.  */
    h->root.u.def.section = sdynbss;
    h->root.u.def.value = sdynbss->size;

    /* Increment the section size to make room for the symbol.  */
    sdynbss->size += h->size;
    return TRUE;
}

/* Not used */
/* Allocate space in .plt, .got and associated reloc sections for
   dynamic relocs.  */

static bfd_boolean
allocate_dynrelocs (struct elf_link_hash_entry *h, void * dat)
{
    struct bfd_link_info *info;
    struct elf32_mb_link_hash_table *htab;
    struct elf32_mb_link_hash_entry *eh;
    struct elf32_mb_dyn_relocs *p;

    if (h->root.type == bfd_link_hash_indirect)
	return TRUE;

    info = (struct bfd_link_info *) dat;
    htab = elf32_mb_hash_table (info);
    if (htab == NULL)
	return FALSE;

    if (htab->elf.dynamic_sections_created
	&& h->plt.refcount > 0)
    {
	/* Make sure this symbol is output as a dynamic symbol.
	   Undefined weak syms won't yet be marked as dynamic.  */
	if (h->dynindx == -1
	    && !h->forced_local)
        {
	    if (! bfd_elf_link_record_dynamic_symbol (info, h))
		return FALSE;
        }

	if (WILL_CALL_FINISH_DYNAMIC_SYMBOL (1, info->shared, h))
        {
	    asection *s = htab->splt;

	    /* The first entry in .plt is reserved.  */
	    if (s->size == 0)
		s->size = PLT_ENTRY_SIZE;

	    h->plt.offset = s->size;

	    /* If this symbol is not defined in a regular file, and we are
	       not generating a shared library, then set the symbol to this
	       location in the .plt.  This is required to make function
	       pointers compare as equal between the normal executable and
	       the shared library.  */
	    if (! info->shared
		&& !h->def_regular)
            {
		h->root.u.def.section = s;
		h->root.u.def.value = h->plt.offset;
            }

	    /* Make room for this entry.  */
	    s->size += PLT_ENTRY_SIZE;

	    /* We also need to make an entry in the .got.plt section, which
	       will be placed in the .got section by the linker script.  */
	    htab->sgotplt->size += 4;

	    /* We also need to make an entry in the .rel.plt section.  */
	    htab->srelplt->size += sizeof (Elf32_External_Rela);
        }
	else
        {
	    h->plt.offset = (bfd_vma) -1;
	    h->needs_plt = 0;
        }
    }
    else
    {
	h->plt.offset = (bfd_vma) -1;
	h->needs_plt = 0;
    }

    if (h->got.refcount > 0)
    {
	asection *s;

	/* Make sure this symbol is output as a dynamic symbol.
	   Undefined weak syms won't yet be marked as dynamic.  */
	if (h->dynindx == -1
	    && !h->forced_local)
        {
	    if (! bfd_elf_link_record_dynamic_symbol (info, h))
		return FALSE;
        }

	s = htab->sgot;
	h->got.offset = s->size;
	s->size += 4;
	htab->srelgot->size += sizeof (Elf32_External_Rela);
    }
    else
	h->got.offset = (bfd_vma) -1;

    eh = (struct elf32_mb_link_hash_entry *) h;
    if (eh->dyn_relocs == NULL)
	return TRUE;

    /* In the shared -Bsymbolic case, discard space allocated for
       dynamic pc-relative relocs against symbols which turn out to be
       defined in regular objects.  For the normal shared case, discard
       space for pc-relative relocs that have become local due to symbol
       visibility changes.  */

    if (info->shared)
    {
	if (h->def_regular
	    && (h->forced_local
		|| info->symbolic))
	{
	    struct elf32_mb_dyn_relocs **pp;

	    for (pp = &eh->dyn_relocs; (p = *pp) != NULL; )
	    {
		p->count -= p->pc_count;
		p->pc_count = 0;
		if (p->count == 0)
		    *pp = p->next;
		else
		    pp = &p->next;
	    }
	}
    }
    else
    {
	/* For the non-shared case, discard space for relocs against
	   symbols which turn out to need copy relocs or are not
	   dynamic.  */

	if (!h->non_got_ref
	    && ((h->def_dynamic
		 && !h->def_regular)
		|| (htab->elf.dynamic_sections_created
		    && (h->root.type == bfd_link_hash_undefweak
			|| h->root.type == bfd_link_hash_undefined))))
	{
	    /* Make sure this symbol is output as a dynamic symbol.
	       Undefined weak syms won't yet be marked as dynamic.  */
	    if (h->dynindx == -1
		&& !h->forced_local)
	    {
		if (! bfd_elf_link_record_dynamic_symbol (info, h))
		    return FALSE;
	    }

	    /* If that succeeded, we know we'll be keeping all the
	       relocs.  */
	    if (h->dynindx != -1)
		goto keep;
	}

	eh->dyn_relocs = NULL;

    keep: ;
    }

    /* Finally, allocate space.  */
    for (p = eh->dyn_relocs; p != NULL; p = p->next)
    {
	asection *sreloc = elf_section_data (p->sec)->sreloc;
	sreloc->size += p->count * sizeof (Elf32_External_Rela);
    }

    return TRUE;
}

/* Set the sizes of the dynamic sections.  */
FUNC_UNUSED
static bfd_boolean
nemaweaver_elf_size_dynamic_sections (bfd *output_bfd ATTRIBUTE_UNUSED,
				      struct bfd_link_info *info)
{
    struct elf32_mb_link_hash_table *htab;
    bfd *dynobj;
    asection *s;
    bfd *ibfd;

    htab = elf32_mb_hash_table (info);
    if (htab == NULL)
	return FALSE;

    dynobj = htab->elf.dynobj;
    BFD_ASSERT (dynobj != NULL);

    /* Set up .got offsets for local syms, and space for local dynamic
       relocs.  */
    for (ibfd = info->input_bfds; ibfd != NULL; ibfd = ibfd->link_next)
    {
	bfd_signed_vma *local_got;
	bfd_signed_vma *end_local_got;
	bfd_size_type locsymcount;
	Elf_Internal_Shdr *symtab_hdr;
	asection *srel;

	if (bfd_get_flavour (ibfd) != bfd_target_elf_flavour)
	    continue;

	for (s = ibfd->sections; s != NULL; s = s->next)
	{
	    struct elf32_mb_dyn_relocs *p;

	    for (p = ((struct elf32_mb_dyn_relocs *)
		      elf_section_data (s)->local_dynrel);
		 p != NULL;
		 p = p->next)
	    {
		if (!bfd_is_abs_section (p->sec)
		    && bfd_is_abs_section (p->sec->output_section))
		{
		    /* Input section has been discarded, either because
		       it is a copy of a linkonce section or due to
		       linker script /DISCARD/, so we'll be discarding
		       the relocs too.  */
		}
		else if (p->count != 0)
		{
		    srel = elf_section_data (p->sec)->sreloc;
		    srel->size += p->count * sizeof (Elf32_External_Rela);
		    if ((p->sec->output_section->flags & SEC_READONLY) != 0)
			info->flags |= DF_TEXTREL;
		}
	    }
	}

	local_got = elf_local_got_refcounts (ibfd);
	if (!local_got)
	    continue;

	symtab_hdr = &elf_tdata (ibfd)->symtab_hdr;
	locsymcount = symtab_hdr->sh_info;
	end_local_got = local_got + locsymcount;
	s = htab->sgot;
	srel = htab->srelgot;

	for (; local_got < end_local_got; ++local_got)
        {
	    if (*local_got > 0)
            {
		*local_got = s->size;
		s->size += 4;
		if (info->shared)
		    srel->size += sizeof (Elf32_External_Rela);
            }
	    else
		*local_got = (bfd_vma) -1;
        }
    }

    /* Allocate global sym .plt and .got entries, and space for global
       sym dynamic relocs.  */
    elf_link_hash_traverse (elf_hash_table (info), allocate_dynrelocs, info);

    if (elf_hash_table (info)->dynamic_sections_created)
    {
	/* Make space for the trailing nop in .plt.  */
	if (htab->splt->size > 0)
	    htab->splt->size += 4;
    }

    /* The check_relocs and adjust_dynamic_symbol entry points have
       determined the sizes of the various dynamic sections.  Allocate
       memory for them.  */
    for (s = dynobj->sections; s != NULL; s = s->next)
    {
	const char *name;
	bfd_boolean strip = FALSE;

	if ((s->flags & SEC_LINKER_CREATED) == 0)
	    continue;

	/* It's OK to base decisions on the section name, because none
	   of the dynobj section names depend upon the input files.  */
	name = bfd_get_section_name (dynobj, s);

	if (strncmp (name, ".rela", 5) == 0)
        {
	    if (s->size == 0)
            {
		/* If we don't need this section, strip it from the
		   output file.  This is to handle .rela.bss and
		   .rela.plt.  We must create it in
		   create_dynamic_sections, because it must be created
		   before the linker maps input sections to output
		   sections.  The linker does that before
		   adjust_dynamic_symbol is called, and it is that
		   function which decides whether anything needs to go
		   into these sections.  */
		strip = TRUE;
            }
	    else
            {
		/* We use the reloc_count field as a counter if we need
		   to copy relocs into the output file.  */
		s->reloc_count = 0;
            }
        }
	else if (s != htab->splt && s != htab->sgot && s != htab->sgotplt)
        {
	    /* It's not one of our sections, so don't allocate space.  */
	    continue;
        }

	if (strip)
        {
	    s->flags |= SEC_EXCLUDE;
	    continue;
        }

	/* Allocate memory for the section contents.  */
	/* FIXME: This should be a call to bfd_alloc not bfd_zalloc.
	   Unused entries should be reclaimed before the section's contents
	   are written out, but at the moment this does not happen.  Thus in
	   order to prevent writing out garbage, we initialise the section's
	   contents to zero.  */
	s->contents = (bfd_byte *) bfd_zalloc (dynobj, s->size);
	if (s->contents == NULL && s->size != 0)
	    return FALSE;
    }

    if (elf_hash_table (info)->dynamic_sections_created)
    {
	/* Add some entries to the .dynamic section.  We fill in the
	   values later, in nemaweaver_elf_finish_dynamic_sections, but we
	   must add the entries now so that we get the correct size for
	   the .dynamic section.  The DT_DEBUG entry is filled in by the
	   dynamic linker and used by the debugger.  */
#define add_dynamic_entry(TAG, VAL)			\
	_bfd_elf_add_dynamic_entry (info, TAG, VAL)

	if (info->executable)
        {
	    if (!add_dynamic_entry (DT_DEBUG, 0))
		return FALSE;
        }

	if (!add_dynamic_entry (DT_RELA, 0)
	    || !add_dynamic_entry (DT_RELASZ, 0)
	    || !add_dynamic_entry (DT_RELAENT, sizeof (Elf32_External_Rela)))
	    return FALSE;

	if (htab->splt->size != 0)
        {
	    if (!add_dynamic_entry (DT_PLTGOT, 0)
		|| !add_dynamic_entry (DT_PLTRELSZ, 0)
		|| !add_dynamic_entry (DT_PLTREL, DT_RELA)
		|| !add_dynamic_entry (DT_JMPREL, 0)
		|| !add_dynamic_entry (DT_BIND_NOW, 1))
		return FALSE;
        }

	if (info->flags & DF_TEXTREL)
        {
	    if (!add_dynamic_entry (DT_TEXTREL, 0))
		return FALSE;
        }
    }
#undef add_dynamic_entry
    return TRUE;
}

/* Finish up dynamic symbol handling.  We set the contents of various
   dynamic sections here.  */
FUNC_UNUSED
static bfd_boolean
nemaweaver_elf_finish_dynamic_symbol (bfd *output_bfd,
				      struct bfd_link_info *info,
				      struct elf_link_hash_entry *h,
				      Elf_Internal_Sym *sym)
{
    struct elf32_mb_link_hash_table *htab;

    htab = elf32_mb_hash_table (info);
    if (htab == NULL)
	return FALSE;

    if (h->plt.offset != (bfd_vma) -1)
    {
	asection *splt;
	asection *srela;
	asection *sgotplt;
	Elf_Internal_Rela rela;
	bfd_byte *loc;
	bfd_vma plt_index;
	bfd_vma got_offset;
	bfd_vma got_addr;

	/* This symbol has an entry in the procedure linkage table.  Set
	   it up.  */
	BFD_ASSERT (h->dynindx != -1);

	splt = htab->splt;
	srela = htab->srelplt;
	sgotplt = htab->sgotplt;
	BFD_ASSERT (splt != NULL && srela != NULL && sgotplt != NULL);

	plt_index = h->plt.offset / PLT_ENTRY_SIZE - 1; /* first entry reserved.  */
	got_offset = (plt_index + 3) * 4; /* 3 reserved ???  */
	got_addr = got_offset;

	/* For non-PIC objects we need absolute address of the GOT entry.  */
	if (!info->shared)
	    got_addr += htab->sgotplt->output_section->vma + sgotplt->output_offset;

	/* Fill in the entry in the procedure linkage table.  */
	bfd_put_32 (output_bfd, PLT_ENTRY_WORD_0 + ((got_addr >> 16) & 0xffff),
		    splt->contents + h->plt.offset);
	if (info->shared)
	    bfd_put_32 (output_bfd, PLT_ENTRY_WORD_1 + (got_addr & 0xffff),
			splt->contents + h->plt.offset + 4);
	else
	    bfd_put_32 (output_bfd, PLT_ENTRY_WORD_1_NOPIC + (got_addr & 0xffff),
			splt->contents + h->plt.offset + 4);
	bfd_put_32 (output_bfd, (bfd_vma) PLT_ENTRY_WORD_2,
		    splt->contents + h->plt.offset + 8);
	bfd_put_32 (output_bfd, (bfd_vma) PLT_ENTRY_WORD_3,
		    splt->contents + h->plt.offset + 12);

	/* Any additions to the .got section??? */
	/*      bfd_put_32 (output_bfd,
		splt->output_section->vma + splt->output_offset + h->plt.offset + 4,
		sgotplt->contents + got_offset); */

	/* Fill in the entry in the .rela.plt section.  */
	rela.r_offset = (sgotplt->output_section->vma
			 + sgotplt->output_offset
			 + got_offset);
	rela.r_info = ELF32_R_INFO (h->dynindx, R_NEMAWEAVER_JUMP_SLOT);
	rela.r_addend = 0;
	loc = srela->contents;
	loc += plt_index * sizeof (Elf32_External_Rela);
	bfd_elf32_swap_reloca_out (output_bfd, &rela, loc);

	if (!h->def_regular)
        {
	    /* Mark the symbol as undefined, rather than as defined in
	       the .plt section.  Zero the value.  */
	    sym->st_shndx = SHN_UNDEF;
	    sym->st_value = 0;
        }
    }

    if (h->got.offset != (bfd_vma) -1)
    {
	asection *sgot;
	asection *srela;
	Elf_Internal_Rela rela;
	bfd_byte *loc;

	/* This symbol has an entry in the global offset table.  Set it
	   up.  */

	sgot = htab->sgot;
	srela = htab->srelgot;
	BFD_ASSERT (sgot != NULL && srela != NULL);

	rela.r_offset = (sgot->output_section->vma
			 + sgot->output_offset
			 + (h->got.offset &~ (bfd_vma) 1));

	/* If this is a -Bsymbolic link, and the symbol is defined
	   locally, we just want to emit a RELATIVE reloc.  Likewise if
	   the symbol was forced to be local because of a version file.
	   The entry in the global offset table will already have been
	   initialized in the relocate_section function.  */
	if (info->shared
	    && (info->symbolic || h->dynindx == -1)
	    && h->def_regular)
        {
	    asection *sec = h->root.u.def.section;
	    rela.r_info = ELF32_R_INFO (0, R_NEMAWEAVER_REL);
	    rela.r_addend = (h->root.u.def.value
			     + sec->output_section->vma
			     + sec->output_offset);
        }
	else
        {
	    rela.r_info = ELF32_R_INFO (h->dynindx, R_NEMAWEAVER_GLOB_DAT);
	    rela.r_addend = 0;
        }

	bfd_put_32 (output_bfd, (bfd_vma) 0,
		    sgot->contents + (h->got.offset &~ (bfd_vma) 1));
	loc = srela->contents;
	loc += srela->reloc_count++ * sizeof (Elf32_External_Rela);
	bfd_elf32_swap_reloca_out (output_bfd, &rela, loc);
    }

    if (h->needs_copy)
    {
	asection *s;
	Elf_Internal_Rela rela;
	bfd_byte *loc;

	/* This symbols needs a copy reloc.  Set it up.  */

	BFD_ASSERT (h->dynindx != -1);

	s = bfd_get_section_by_name (h->root.u.def.section->owner,
				     ".rela.bss");
	BFD_ASSERT (s != NULL);

	rela.r_offset = (h->root.u.def.value
			 + h->root.u.def.section->output_section->vma
			 + h->root.u.def.section->output_offset);
	rela.r_info = ELF32_R_INFO (h->dynindx, R_NEMAWEAVER_COPY);
	rela.r_addend = 0;
	loc = s->contents + s->reloc_count++ * sizeof (Elf32_External_Rela);
	bfd_elf32_swap_reloca_out (output_bfd, &rela, loc);
    }

    /* Mark some specially defined symbols as absolute.  */
    if (strcmp (h->root.root.string, "_DYNAMIC") == 0
	|| strcmp (h->root.root.string, "_GLOBAL_OFFSET_TABLE_") == 0
	|| strcmp (h->root.root.string, "_PROCEDURE_LINKAGE_TABLE_") == 0)
	sym->st_shndx = SHN_ABS;

    return TRUE;
}


static bfd_boolean
nemaweaver_elf_create_dynamic_sections (bfd *dynobj, struct bfd_link_info *info)
{
    struct elf32_mb_link_hash_table *htab;

    htab = elf32_mb_hash_table (info);
    if (htab == NULL)
	return FALSE;

    if (!htab->sgot && !create_got_section (dynobj, info))
	return FALSE;

    if (!_bfd_elf_create_dynamic_sections (dynobj, info))
	return FALSE;

    htab->splt = bfd_get_section_by_name (dynobj, ".plt");
    htab->srelplt = bfd_get_section_by_name (dynobj, ".rela.plt");
    htab->sdynbss = bfd_get_section_by_name (dynobj, ".dynbss");
    if (!info->shared)
	htab->srelbss = bfd_get_section_by_name (dynobj, ".rela.bss");

    if (!htab->splt || !htab->srelplt || !htab->sdynbss
	|| (!info->shared && !htab->srelbss))
	abort ();

    return TRUE;
}

/* Finish up the dynamic sections.  */
FUNC_UNUSED
static bfd_boolean
nemaweaver_elf_finish_dynamic_sections (bfd *output_bfd,
					struct bfd_link_info *info)
{
    bfd *dynobj;
    asection *sdyn, *sgot;
    struct elf32_mb_link_hash_table *htab;

    htab = elf32_mb_hash_table (info);
    if (htab == NULL)
	return FALSE;

    dynobj = htab->elf.dynobj;

    sdyn = bfd_get_section_by_name (dynobj, ".dynamic");

    if (htab->elf.dynamic_sections_created)
    {
	asection *splt;
	Elf32_External_Dyn *dyncon, *dynconend;

	splt = bfd_get_section_by_name (dynobj, ".plt");
	BFD_ASSERT (splt != NULL && sdyn != NULL);

	dyncon = (Elf32_External_Dyn *) sdyn->contents;
	dynconend = (Elf32_External_Dyn *) (sdyn->contents + sdyn->size);
	for (; dyncon < dynconend; dyncon++)
        {
	    Elf_Internal_Dyn dyn;
	    const char *name;
	    bfd_boolean size;

	    bfd_elf32_swap_dyn_in (dynobj, dyncon, &dyn);

	    switch (dyn.d_tag)
            {
            case DT_PLTGOT:   name = ".got.plt"; size = FALSE; break;
            case DT_PLTRELSZ: name = ".rela.plt"; size = TRUE; break;
            case DT_JMPREL:   name = ".rela.plt"; size = FALSE; break;
            case DT_RELA:     name = ".rela.dyn"; size = FALSE; break;
            case DT_RELASZ:   name = ".rela.dyn"; size = TRUE; break;
            default:	  name = NULL; size = FALSE; break;
            }

	    if (name != NULL)
            {
		asection *s;

		s = bfd_get_section_by_name (output_bfd, name);
		if (s == NULL)
		    dyn.d_un.d_val = 0;
		else
                {
		    if (! size)
			dyn.d_un.d_ptr = s->vma;
		    else
			dyn.d_un.d_val = s->size;
                }
		bfd_elf32_swap_dyn_out (output_bfd, &dyn, dyncon);
            }
        }

	/* Clear the first entry in the procedure linkage table,
	   and put a nop in the last four bytes.  */
	if (splt->size > 0)
        {
	    memset (splt->contents, 0, PLT_ENTRY_SIZE);
	    bfd_put_32 (output_bfd, (bfd_vma) 0x80000000 /* nop.  */,
			splt->contents + splt->size - 4);
        }

	elf_section_data (splt->output_section)->this_hdr.sh_entsize = 4;
    }

    /* Set the first entry in the global offset table to the address of
       the dynamic section.  */
    sgot = bfd_get_section_by_name (dynobj, ".got.plt");
    if (sgot && sgot->size > 0)
    {
	if (sdyn == NULL)
	    bfd_put_32 (output_bfd, (bfd_vma) 0, sgot->contents);
	else
	    bfd_put_32 (output_bfd,
			sdyn->output_section->vma + sdyn->output_offset,
			sgot->contents);
	elf_section_data (sgot->output_section)->this_hdr.sh_entsize = 4;
    }

    if (htab->sgot && htab->sgot->size > 0)
	elf_section_data (htab->sgot->output_section)->this_hdr.sh_entsize = 4;

    return TRUE;
}

/* Hook called by the linker routine which adds symbols from an object
   file.  We use it to put .comm items in .sbss, and not .bss.  */
FUNC_UNUSED
static bfd_boolean
nemaweaver_elf_add_symbol_hook (bfd *abfd,
			        struct bfd_link_info *info,
			        Elf_Internal_Sym *sym,
			        const char **namep ATTRIBUTE_UNUSED,
			        flagword *flagsp ATTRIBUTE_UNUSED,
			        asection **secp,
			        bfd_vma *valp)
{
    if (sym->st_shndx == SHN_COMMON
	&& !info->relocatable
	&& sym->st_size <= elf_gp_size (abfd))
    {
	/* Common symbols less than or equal to -G nn bytes are automatically
	   put into .sbss.  */
	*secp = bfd_make_section_anyway (abfd, ".sbss");
	if (*secp == NULL
	    || ! bfd_set_section_flags (abfd, *secp, SEC_IS_COMMON))
	    return FALSE;

	*valp = sym->st_size;
    }

    return TRUE;
}


#define TARGET_BIG_SYM          bfd_elf32_nemaweaver_vec
#define TARGET_BIG_NAME		"elf32-nemaweaver"

#define TARGET_LITTLE_SYM          bfd_elf32_nemaweaver_little_vec
#define TARGET_LITTLE_NAME	   "elf32-nemaweaver-little"

#define ELF_ARCH		bfd_arch_nemaweaver
#define ELF_TARGET_ID		NEMAWEAVER_ELF_DATA
#define ELF_MACHINE_CODE	EM_NEMAWEAVER
#define ELF_MACHINE_ALT1	EM_NEMAWEAVER_OLD
#define ELF_MAXPAGESIZE		0x4   		/* 4k, if we ever have 'em.  */
#define elf_info_to_howto	nemaweaver_elf_info_to_howto
#define elf_info_to_howto_rel	NULL

#define bfd_elf32_bfd_reloc_type_lookup		nemaweaver_elf_reloc_type_lookup
#define bfd_elf32_bfd_is_local_label_name       nemaweaver_elf_is_local_label_name
#define elf_backend_relocate_section		nemaweaver_elf_relocate_section
#define bfd_elf32_bfd_reloc_name_lookup		nemaweaver_elf_reloc_name_lookup

#define elf_backend_gc_mark_hook		nemaweaver_elf_gc_mark_hook
#define elf_backend_check_relocs                nemaweaver_elf_check_relocs
#define elf_backend_copy_indirect_symbol        nemaweaver_elf_copy_indirect_symbol
#define bfd_elf32_bfd_link_hash_table_create    nemaweaver_elf_link_hash_table_create
#define elf_backend_can_gc_sections		1
#define elf_backend_can_refcount    		1
#define elf_backend_want_got_plt    		1
#define elf_backend_plt_readonly    		1
#define elf_backend_got_header_size 		12
#define elf_backend_rela_normal     		1

#define elf_backend_adjust_dynamic_symbol       nemaweaver_elf_adjust_dynamic_symbol
#define elf_backend_create_dynamic_sections     nemaweaver_elf_create_dynamic_sections
#define elf_backend_finish_dynamic_sections     nemaweaver_elf_finish_dynamic_sections
#define elf_backend_finish_dynamic_symbol       nemaweaver_elf_finish_dynamic_symbol
#define elf_backend_size_dynamic_sections       nemaweaver_elf_size_dynamic_sections
#define elf_backend_add_symbol_hook		nemaweaver_elf_add_symbol_hook

#include "elf32-target.h"
