/* Copyright (C) 1991 Free Software Foundation, Inc.
   
This file is part of GLD, the Gnu Linker.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/*
 * $Id$ 
*/

/* 
   This module writes out the final image by reading sections from the
   input files, relocating them and writing them out

   There are two main paths through this module, one for normal
   operation and one for partial linking. 

   During  normal operation, raw section data is read along with the
   associated relocation information, the relocation info applied and
   the section data written out on a section by section basis.

   When partially linking, all the relocation records are read to work
   out how big the output relocation vector will be. Then raw data is
   read, relocated and written section by section.

   Written by Steve Chamberlain steve@cygnus.com

*/


#include "bfd.h"
#include "sysdep.h"

#include "ldlang.h"
#include "ld.h"
#include "ldwrite.h"
#include "ldmisc.h"
#include "ldsym.h"
#include "ldgram.h"




/* Static vars for do_warnings and subroutines of it */
int list_unresolved_refs;	/* List unresolved refs */
int list_warning_symbols;	/* List warning syms */
int list_multiple_defs;		/* List multiple definitions */
extern int errno;
extern char *sys_errlist[];

extern unsigned int undefined_global_sym_count;

extern bfd *output_bfd;

extern struct lang_output_section_statement_struct * create_object_symbols;

extern char lprefix;

#ifdef __STDC__
void lang_for_each_statement(void (*func)());
#else /* __STDC__ */
void lang_for_each_statement();
#endif /* __STDC__ */

extern bfd_size_type largest_section;
ld_config_type config;

extern unsigned int global_symbol_count;

boolean trace_files;

static void 
DEFUN(perform_relocation,(input_bfd,
			  input_section,
			  data,
			  symbols),
      bfd *input_bfd AND
      asection *input_section AND
      PTR data AND
      asymbol **symbols)
{
  static asymbol *error_symbol = (asymbol *)NULL;
  static unsigned int error_count = 0;
#define MAX_ERRORS_IN_A_ROW 5
  bfd_size_type reloc_size = bfd_get_reloc_upper_bound(input_bfd, input_section);

  arelent **reloc_vector = (arelent **)ldmalloc(reloc_size);
  arelent **parent;
  bfd *ob = output_bfd;
  asection *os = input_section->output_section;
  if (config.relocateable_output == false) ob = (bfd *)NULL;

  input_section->_cooked_size = input_section->_raw_size;
  input_section->reloc_done = 1;
  
  if (bfd_canonicalize_reloc(input_bfd, 
			     input_section,
			     reloc_vector,
			     symbols) )
    {
      for (parent = reloc_vector; *parent; parent++) 
	{

	  bfd_reloc_status_type r=
	    bfd_perform_relocation(input_bfd,
				   *parent,
				   data,
				   input_section, 
				   ob);

	  if (r == bfd_reloc_ok) {
	    if (ob != (bfd *)NULL) {
	      /* A parital link, so keep the relocs */

	      /* Add to each relocation the offset of where it lives
		 in the output section */
/*	      (*parent)->address += input_section->output_offset;*/

	      os->orelocation[os->reloc_count] = *parent;
	      os->reloc_count++;
	    }
	  }
	  else
	    {
	      asymbol *s;
	      arelent *p = *parent;

	      if (ob != (bfd *)NULL) {
		/* A parital link, so keep the relocs */
		os->orelocation[os->reloc_count] = *parent;
		os->reloc_count++;
	      }

	      if (p->sym_ptr_ptr != (asymbol **)NULL) {
		s = *(p->sym_ptr_ptr);
	      }
	      else {
		s = (asymbol *)NULL;
	      }
	      switch (r)
		{
		case bfd_reloc_undefined:
		  /* We remember the symbol, and never print more than
		     a reasonable number of them in a row */
		  if (s == error_symbol) {
		    error_count++;
		  }
		  else {
		    error_count = 0;
		    error_symbol = s;
		  }
		  if (error_count < MAX_ERRORS_IN_A_ROW) {
		    einfo("%C: undefined reference to `%T'\n",
			  input_bfd, input_section, symbols,
			  (*parent)->address, s);
		    config.make_executable = false;
		  }
		  else if (error_count == MAX_ERRORS_IN_A_ROW) {
		    einfo("%C: more undefined references to `%T' follow\n",
			  input_bfd, input_section,
			  symbols, (*parent)->address, s);
		  }		    
		  else {
		    /* Don't print any more */
		  }
		  break;
		case bfd_reloc_dangerous: 
		  einfo("%B: relocation may be wrong `%T'\n",
			input_bfd, s);
		  break;
		case bfd_reloc_outofrange:
		  einfo("%B:%s relocation address out of range %T (%V)\n",
			input_bfd, input_section->name, s, p->address); 
		  break;
		case bfd_reloc_overflow:
		  einfo("%B:%s relocation overflow in %T reloc type %d\n",
			input_bfd, input_section->name, s, p->howto->type);
		  break;
		default:
		  einfo("%F%B: relocation error, symbol `%T'\n",
			input_bfd, s);
		  break;
		}
	    }
	}
    }
  free((char *)reloc_vector);
}






PTR data_area;

static void
DEFUN(copy_and_relocate,(statement),
      lang_statement_union_type *statement)
{
  switch (statement->header.type) {
  case lang_fill_statement_enum: 
      {
#if 0
      bfd_byte play_area[SHORT_SIZE];
      unsigned int i;
      bfd_putshort(output_bfd, statement->fill_statement.fill, play_area);
      /* Write out all entire shorts */
      for (i = 0;
	   i < statement->fill_statement.size - SHORT_SIZE + 1;
	   i+= SHORT_SIZE)
	{
	  bfd_set_section_contents(output_bfd,
				   statement->fill_statement.output_section,
				   play_area,
				   statement->data_statement.output_offset +i,
				   SHORT_SIZE);

	}

      /* Now write any remaining byte */
      if (i < statement->fill_statement.size) 
	{
	  bfd_set_section_contents(output_bfd,
				   statement->fill_statement.output_section,
				   play_area,
				   statement->data_statement.output_offset +i,
				   1);

	}
#endif
    }
    break;
  case lang_data_statement_enum:
    {
      bfd_vma value = statement->data_statement.value;
      bfd_byte play_area[LONG_SIZE];
      unsigned int size = 0;
      switch (statement->data_statement.type) {
      case LONG:
	bfd_put_32(output_bfd, value,  play_area);
	size = LONG_SIZE;
	break;
      case SHORT:
	bfd_put_16(output_bfd, value,  play_area);
	size = SHORT_SIZE;
	break;
      case BYTE:
	bfd_put_8(output_bfd, value,  play_area);
	size = BYTE_SIZE;
	break;
      }
      
      bfd_set_section_contents(output_bfd,
			       statement->data_statement.output_section,
			       play_area,
			       statement->data_statement.output_vma,
			       size);
			       
			       


    }
    break;
  case lang_input_section_enum:
      {

	asection *i  = statement->input_section.section;
	asection *output_section = i->output_section;
	lang_input_statement_type *ifile =
	  statement->input_section.ifile;
	if (ifile->just_syms_flag == false) {
	  bfd *inbfd = ifile->the_bfd;

	  if (output_section->flags & SEC_LOAD &&
	      output_section->flags & SEC_ALLOC
	      && bfd_get_section_size_before_reloc(i) != 0) 
	      {
		if(bfd_get_section_contents(inbfd,
					    i,
					    data_area,
					    (file_ptr)0,
					    bfd_get_section_size_before_reloc(i)) == false) 
		    {
		      einfo("%F%B error reading section contents %E\n", inbfd);
		    }
		/* Set the reloc bit */
		perform_relocation (inbfd,  i,  data_area, ifile->asymbols);


		if(bfd_set_section_contents(output_bfd,
					    output_section,
					    data_area,
					    (file_ptr)i->output_offset,
					    bfd_get_section_size_after_reloc(i)) == false) 
		    {
		      einfo("%F%B error writing section contents of %E\n",
			    output_bfd);
		    }

	      }
	}

      }
    break;

  default:
    /* All the other ones fall through */
    ;

  }
}

void
DEFUN_VOID(write_norel)
{
  /* Output the text and data segments, relocating as we go.  */
  lang_for_each_statement(copy_and_relocate);
}


static void 
DEFUN(read_relocs,(abfd, section, symbols),
      bfd *abfd AND
      asection *section AND
      asymbol **symbols)
{
  /* Work out the output section ascociated with this input section */
  asection *output_section = section->output_section;

  bfd_size_type reloc_size = bfd_get_reloc_upper_bound(abfd, section);
  arelent **reloc_vector = (arelent **)ldmalloc(reloc_size);

  if (bfd_canonicalize_reloc(abfd, 
			     section,
			     reloc_vector,
			     symbols)) {
    output_section->reloc_count   += section->reloc_count;
  }
}


static  void
DEFUN_VOID(write_rel)
{
  /*
     Run through each section of each file and work work out the total
     number of relocation records which will finally be in each output
     section 
     */

  LANG_FOR_EACH_INPUT_SECTION
    (statement, abfd, section,
     (read_relocs(abfd, section, statement->asymbols)));



  /*
     Now run though all the output sections and allocate the space for
     all the relocations
     */
  LANG_FOR_EACH_OUTPUT_SECTION
    (section, 
     (section->orelocation =
      (arelent **)ldmalloc((bfd_size_type)(sizeof(arelent **)*
				    section->reloc_count)),
      section->reloc_count = 0,
     section->flags |= SEC_HAS_CONTENTS));


  /*
     Copy the data, relocating as we go
     */
  lang_for_each_statement(copy_and_relocate);
}

void
DEFUN(ldwrite, (write_map),
      boolean write_map)
{
  data_area = (PTR) ldmalloc(largest_section);
  if (config.relocateable_output == true)
      {
	write_rel();
      }
  else 
      {
	write_relaxnorel(output_bfd);
      }
  free(data_area);
  /* Output the symbol table (both globals and locals).  */

  /* Print a map, if requested.  */

  if (write_map) {
    ldsym_print_symbol_table ();
    lang_map(stdout);
  }

  ldsym_write ();

}
    
