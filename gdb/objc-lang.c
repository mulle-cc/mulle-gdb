/* Objective-C language support routines for GDB, the GNU debugger.

   Copyright (C) 2002-2023 Free Software Foundation, Inc.

   Contributed by Apple Computer, Inc.
   Written by Michael Snyder.

   This file is part of GDB.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

#include "defs.h"
#include "symtab.h"
#include "gdbtypes.h"
#include "expression.h"
#include "parser-defs.h"
#include "language.h"
#include "varobj.h"
#include "c-lang.h"
#include "objc-lang.h"
#include "complaints.h"
#include "value.h"
#include "symfile.h"
#include "objfiles.h"
#include "target.h"
#include "gdbcore.h"
#include "gdbcmd.h"
#include "frame.h"
#include "gdbsupport/gdb_regex.h"
#include "regcache.h"
#include "block.h"
#include "infcall.h"
#include "valprint.h"
#include "cli/cli-utils.h"
#include "c-exp.h"

#include <ctype.h>

#ifdef __cplusplus
# include <algorithm>
#endif


//#define DEBUG_VERBOSE 1 // (defined( DEBUG) & 1)
//#if DEBUG_VERBOSE
//# warning "DEBUG_VERBOSE defined"
//#endif

//
// This file has been changed to work with mulle-objc
//
// When compiled with #ifndef __cplusplus, then large parts of the code
// poisoned by C++ is removed and the remaining functions can be used
// in a test in mulle-objc-runtime/test-debugger/20_gdb/simulate-gdb
// as is. Just copy it there.
//

struct objc_object {
  CORE_ADDR isa;
};

struct objc_class {
  CORE_ADDR isa;
  CORE_ADDR super_class;
  CORE_ADDR name;
//  long version;
//  long info;
  long allocation_size;
//  CORE_ADDR ivars;
  CORE_ADDR methods;
//  CORE_ADDR cache;
  CORE_ADDR infra_class;
  CORE_ADDR universe;
//  CORE_ADDR protocols;
  unsigned long   classid;
  unsigned long   inheritance;
};

struct objc_super {
  unsigned long   classid;
  unsigned long   methodid;
};

struct objc_method {
  unsigned long   sel;
  CORE_ADDR name;
  CORE_ADDR types;
  CORE_ADDR imp;
};

static const registry<objfile>::key<unsigned int> objc_objfile_data;

/* Lookup a structure type named "struct NAME", visible in lexical
   block BLOCK.  If NOERR is nonzero, return zero if NAME is not
   suitably defined.  */

struct symbol *
lookup_struct_typedef (const char *name, const struct block *block, int noerr)
{
  struct symbol *sym;

  sym = lookup_symbol (name, block, STRUCT_DOMAIN, 0).symbol;

  if (sym == NULL)
    {
      if (noerr)
	return 0;
      else
	error (_("No struct type named %s."), name);
    }
  if (sym->type ()->code () != TYPE_CODE_STRUCT)
    {
      if (noerr)
	return 0;
      else
	error (_("This context has class, union or enum %s, not a struct."),
	       name);
    }
  return sym;
}

CORE_ADDR
lookup_objc_class (struct gdbarch *gdbarch, const char *classname)
{
  struct type *char_type = builtin_type (gdbarch)->builtin_char;
  struct value * function, *classval;

  if (! target_has_execution ())
    {
      /* Can't call into inferior to lookup class.  */
      return 0;
    }

  if (lookup_minimal_symbol("objc_lookUpClass", 0, 0).minsym)
    function = find_function_in_inferior("objc_lookUpClass", NULL);
  else if (lookup_minimal_symbol ("objc_lookup_class", 0, 0).minsym)
    function = find_function_in_inferior("objc_lookup_class", NULL);
  else
    {
      complaint (_("no way to lookup Objective-C classes"));
      return 0;
    }

  classval = value_string (classname, strlen (classname) + 1, char_type);
  classval = value_coerce_array (classval);
  return (CORE_ADDR) value_as_long (call_function_by_hand (function,
							   NULL,
							   classval));
}

CORE_ADDR
lookup_child_selector (struct gdbarch *gdbarch, const char *selname)
{
  struct type *char_type = builtin_type (gdbarch)->builtin_char;
  struct value * function, *selstring;

  if (! target_has_execution ())
    {
      /* Can't call into inferior to lookup selector.  */
      return 0;
    }

  if (lookup_minimal_symbol("sel_getUid", 0, 0).minsym)
    function = find_function_in_inferior("sel_getUid", NULL);
  else if (lookup_minimal_symbol ("sel_get_any_uid", 0, 0).minsym)
    function = find_function_in_inferior("sel_get_any_uid", NULL);
  else
    {
      complaint (_("no way to lookup Objective-C selectors"));
      return 0;
    }

  selstring = value_coerce_array (value_string (selname,
						strlen (selname) + 1,
						char_type));
  return value_as_long (call_function_by_hand (function, NULL, selstring));
}

struct value * 
value_nsstring (struct gdbarch *gdbarch, const char *ptr, int len)
{
  struct type *char_type = builtin_type (gdbarch)->builtin_char;
  struct value *stringValue[3];
  struct value *function, *nsstringValue;
  struct symbol *sym;
  struct type *type;

  if (!target_has_execution ())
    return 0;		/* Can't call into inferior to create NSString.  */

  stringValue[2] = value_string(ptr, len, char_type);
  stringValue[2] = value_coerce_array(stringValue[2]);
  /* _NSNewStringFromCString replaces "istr" after Lantern2A.  */
  if (lookup_minimal_symbol("_NSNewStringFromCString", 0, 0).minsym)
    {
      function = find_function_in_inferior("_NSNewStringFromCString", NULL);
      nsstringValue = call_function_by_hand(function, NULL, stringValue[2]);
    }
  else if (lookup_minimal_symbol("istr", 0, 0).minsym)
    {
      function = find_function_in_inferior("istr", NULL);
      nsstringValue = call_function_by_hand(function, NULL, stringValue[2]);
    }
  else if (lookup_minimal_symbol("+[NSString stringWithCString:]", 0, 0).minsym)
    {
      function
	= find_function_in_inferior("+[NSString stringWithCString:]", NULL);
      type = builtin_type (gdbarch)->builtin_long;

      stringValue[0] = value_from_longest
	(type, lookup_objc_class (gdbarch, "NSString"));
      stringValue[1] = value_from_longest
	(type, lookup_child_selector (gdbarch, "stringWithCString:"));
      nsstringValue = call_function_by_hand(function, NULL, stringValue);
    }
  else
    error (_("NSString: internal error -- no way to create new NSString"));

  sym = lookup_struct_typedef("NSString", 0, 1);
  if (sym == NULL)
    sym = lookup_struct_typedef("NXString", 0, 1);
  if (sym == NULL)
    type = builtin_type (gdbarch)->builtin_data_ptr;
  else
    type = lookup_pointer_type(sym->type ());

  nsstringValue->deprecated_set_type (type);
  return nsstringValue;
}

/* Class representing the Objective-C language.  */

class objc_language : public language_defn
{
public:
  objc_language ()
    : language_defn (language_objc)
  { /* Nothing.  */ }

  /* See language.h.  */

  const char *name () const override
  { return "objective-c"; }

  /* See language.h.  */

  const char *natural_name () const override
  { return "Objective-C"; }

  /* See language.h.  */

  const std::vector<const char *> &filename_extensions () const override
  {
    static const std::vector<const char *> extensions = { ".m", ".aam" };
    return extensions;
  }

  /* See language.h.  */
  void language_arch_info (struct gdbarch *gdbarch,
			   struct language_arch_info *lai) const override
  {
    c_language_arch_info (gdbarch, lai);
  }

  /* See language.h.  */
  bool sniff_from_mangled_name
       (const char *mangled, gdb::unique_xmalloc_ptr<char> *demangled)
       const override
  {
    *demangled = demangle_symbol (mangled, 0);
    return *demangled != NULL;
  }

  /* See language.h.  */

  gdb::unique_xmalloc_ptr<char> demangle_symbol (const char *mangled,
						 int options) const override;

  /* See language.h.  */

  bool can_print_type_offsets () const override
  {
    return true;
  }

  /* See language.h.  */

  void print_type (struct type *type, const char *varstring,
		   struct ui_file *stream, int show, int level,
		   const struct type_print_options *flags) const override
  {
    c_print_type (type, varstring, stream, show, level, la_language, flags);
  }

  /* See language.h.  */

  CORE_ADDR skip_trampoline (const frame_info_ptr &frame,
			     CORE_ADDR stop_pc) const override
  {
    struct gdbarch *gdbarch = get_frame_arch (frame);
    CORE_ADDR real_stop_pc;
    CORE_ADDR method_stop_pc;

#if DEBUG_VERBOSE
    fprintf( stderr, "*>* START OF TRAMPOLINE QUERY\n");
#endif
    /* Determine if we are currently in the Objective-C dispatch function.
       If so, get the address of the method function that the dispatcher
       would call and use that as the function to step into instead.  Also
       skip over the trampoline for the function (if any).  This is better
       for the user since they are only interested in stepping into the
       method function anyway.  */

    real_stop_pc = gdbarch_skip_trampoline_code (gdbarch, frame, stop_pc);

    if (real_stop_pc != 0)
      find_objc_msgcall (real_stop_pc, &method_stop_pc);
    else
      find_objc_msgcall (stop_pc, &method_stop_pc);

#if DEBUG_VERBOSE
    fprintf( stderr, "*<* END OF TRAMPOLINE QUERY\n");
#endif

    if (method_stop_pc)
      {
         if( method_stop_pc == (CORE_ADDR) -1)
         {
            real_stop_pc = frame_unwind_caller_pc( frame);
         }
         else
         {
         	real_stop_pc = gdbarch_skip_trampoline_code
         	  (gdbarch, frame, method_stop_pc);
         	if (real_stop_pc == 0)
         	  real_stop_pc = method_stop_pc;
         }
      }

    return real_stop_pc;
  }

  /* See language.h.  */

  const char *name_of_this () const override
  { return "self"; }

  /* See language.h.  */

  enum macro_expansion macro_expansion () const override
  { return macro_expansion_c; }
};

/* See declaration of objc_language::demangle_symbol above.  */

gdb::unique_xmalloc_ptr<char>
objc_language::demangle_symbol (const char *mangled, int options) const
{
  char *demangled, *cp;

  if (mangled[0] == '_'
      && (mangled[1] == 'i' || mangled[1] == 'c')
      && mangled[2] == '_')
    {
      cp = demangled = (char *) xmalloc (strlen (mangled) + 2);

      if (mangled[1] == 'i')
	*cp++ = '-';		/* for instance method */
      else
	*cp++ = '+';		/* for class    method */

      *cp++ = '[';		/* opening left brace  */
      strcpy(cp, mangled+3);	/* Tack on the rest of the mangled name.  */

      while (*cp != '\0' && *cp == '_')
	cp++;			/* Skip any initial underbars in class
				   name.  */

      cp = strchr(cp, '_');
      if (cp == nullptr)	/* Find first non-initial underbar.  */
	{
	  xfree(demangled);	/* not mangled name */
	  return nullptr;
	}
      if (cp[1] == '_')		/* Easy case: no category name.    */
	{
	  *cp++ = ' ';		/* Replace two '_' with one ' '.   */
	  strcpy(cp, mangled + (cp - demangled) + 2);
	}
      else
	{
	  *cp++ = '(';		/* Less easy case: category name.  */
	  cp = strchr(cp, '_');
	  if (cp == nullptr)
	    {
	      xfree(demangled);	/* not mangled name */
	      return nullptr;
	    }
	  *cp++ = ')';
	  *cp++ = ' ';		/* Overwriting 1st char of method name...  */
	  strcpy(cp, mangled + (cp - demangled));	/* Get it back.  */
	}

      while (*cp != '\0' && *cp == '_')
	cp++;			/* Skip any initial underbars in
				   method name.  */

      for (; *cp != '\0'; cp++)
	if (*cp == '_')
	  *cp = ':';		/* Replace remaining '_' with ':'.  */

      *cp++ = ']';		/* closing right brace */
      *cp++ = 0;		/* string terminator */
      return gdb::unique_xmalloc_ptr<char> (demangled);
    }
  else
    return nullptr;	/* Not an objc mangled name.  */
}

/* Single instance of the class representing the Objective-C language.  */
/*
 * MEMO: in the constructor this adds itself to an array of languages...
 */
static objc_language objc_language_defn;

/*
 * ObjC:
 * Following functions help construct Objective-C message calls.
 */

struct selname		/* For parsing Objective-C.  */
  {
    struct selname *next;
    char *msglist_sel;
    int msglist_len;
  };

static int msglist_len;
static struct selname *selname_chain;
static char *msglist_sel;

void
start_msglist(void)
{
  struct selname *newobj = XNEW (struct selname);

  newobj->next = selname_chain;
  newobj->msglist_len = msglist_len;
  newobj->msglist_sel = msglist_sel;
  msglist_len = 0;
  msglist_sel = (char *)xmalloc(1);
  *msglist_sel = 0;
  selname_chain = newobj;
}

void
add_msglist(struct stoken *str, int addcolon)
{
  char *s;
  const char *p;
  int len, plen;

  if (str == 0)			/* Unnamed arg, or...  */
    {
      if (addcolon == 0)	/* variable number of args.  */
	{
	  msglist_len++;
	  return;
	}
      p = "";
      plen = 0;
    }
  else
    {
      p = str->ptr;
      plen = str->length;
    }
  len = plen + strlen(msglist_sel) + 2;
  s = (char *)xmalloc(len);
  strcpy(s, msglist_sel);
  strncat(s, p, plen);
  xfree(msglist_sel);
  msglist_sel = s;
  if (addcolon)
    {
      s[len-2] = ':';
      s[len-1] = 0;
      msglist_len++;
    }
  else
    s[len-2] = '\0';
}

int
end_msglist (struct parser_state *ps)
{
  int val = msglist_len;
  struct selname *sel = selname_chain;
  char *p = msglist_sel;
  CORE_ADDR selid;

  std::vector<expr::operation_up> args = ps->pop_vector (val);
  expr::operation_up target = ps->pop ();

  selname_chain = sel->next;
  msglist_len = sel->msglist_len;
  msglist_sel = sel->msglist_sel;
  selid = lookup_child_selector (ps->gdbarch (), p);
  if (!selid)
    error (_("Can't find selector \"%s\""), p);

  ps->push_new<expr::objc_msgcall_operation> (selid, std::move (target),
					      std::move (args));

  xfree(p);
  xfree(sel);

  return val;
}

/*
 * Function: specialcmp (const char *a, const char *b)
 *
 * Special strcmp: treats ']' and ' ' as end-of-string.
 * Used for qsorting lists of objc methods (either by class or selector).
 */

static int
specialcmp (const char *a, const char *b)
{
  while (*a && *a != ' ' && *a != ']' && *b && *b != ' ' && *b != ']')
    {
      if (*a != *b)
	return *a - *b;
      a++, b++;
    }
  if (*a && *a != ' ' && *a != ']')
    return  1;		/* a is longer therefore greater.  */
  if (*b && *b != ' ' && *b != ']')
    return -1;		/* a is shorter therefore lesser.  */
  return    0;		/* a and b are identical.  */
}

/*
 * Function: compare_selectors (const void *, const void *)
 *
 * Comparison function for use with qsort.  Arguments are symbols or
 * msymbols Compares selector part of objc method name alphabetically.
 */

static int
compare_selectors (const void *a, const void *b)
{
  const char *aname, *bname;

  aname = (*(struct symbol **) a)->print_name ();
  bname = (*(struct symbol **) b)->print_name ();
  if (aname == NULL || bname == NULL)
    error (_("internal: compare_selectors(1)"));

  aname = strchr(aname, ' ');
  bname = strchr(bname, ' ');
  if (aname == NULL || bname == NULL)
    error (_("internal: compare_selectors(2)"));

  return specialcmp (aname+1, bname+1);
}

/*
 * Function: selectors_info (regexp, from_tty)
 *
 * Implements the "Info selectors" command.  Takes an optional regexp
 * arg.  Lists all objective c selectors that match the regexp.  Works
 * by grepping thru all symbols for objective c methods.  Output list
 * is sorted and uniqued.
 */

static void
info_selectors_command (const char *regexp, int from_tty)
{
  const char            *name;
  char                  *val;
  int                    matches = 0;
  int                    maxlen  = 0;
  int                    ix;
  char                   myregexp[2048];
  char                   asel[256];
  struct symbol        **sym_arr;
  int                    plusminus = 0;

  if (regexp == NULL)
    strcpy(myregexp, ".*]");	/* Null input, match all objc methods.  */
  else
    {
      if (*regexp == '+' || *regexp == '-')
	{ /* User wants only class methods or only instance methods.  */
	  plusminus = *regexp++;
	  while (*regexp == ' ' || *regexp == '\t')
	    regexp++;
	}
      if (*regexp == '\0')
	strcpy(myregexp, ".*]");
      else
	{
	  /* Allow a few extra bytes because of the strcat below.  */
	  if (sizeof (myregexp) < strlen (regexp) + 4)
	    error (_("Regexp is too long: %s"), regexp);
	  strcpy(myregexp, regexp);
	  if (myregexp[strlen(myregexp) - 1] == '$') /* end of selector */
	    myregexp[strlen(myregexp) - 1] = ']';    /* end of method name */
	  else
	    strcat(myregexp, ".*]");
	}
    }

  if (regexp != NULL)
    {
      val = re_comp (myregexp);
      if (val != 0)
	error (_("Invalid regexp (%s): %s"), val, regexp);
    }

  /* First time thru is JUST to get max length and count.  */
  for (objfile *objfile : current_program_space->objfiles ())
    {
      for (minimal_symbol *msymbol : objfile->msymbols ())
	{
	  QUIT;
	  name = msymbol->natural_name ();
	  if (name
	      && (name[0] == '-' || name[0] == '+')
	      && name[1] == '[')		/* Got a method name.  */
	    {
	      /* Filter for class/instance methods.  */
	      if (plusminus && name[0] != plusminus)
		continue;
	      /* Find selector part.  */
	      name = (char *) strchr (name+2, ' ');
	      if (name == NULL)
		{
		  complaint (_("Bad method name '%s'"),
			     msymbol->natural_name ());
		  continue;
		}
	      if (regexp == NULL || re_exec(++name) != 0)
		{
		  const char *mystart = name;
		  const char *myend   = strchr (mystart, ']');

		  if (myend && (myend - mystart > maxlen))
		    maxlen = myend - mystart;	/* Get longest selector.  */
		  matches++;
		}
	    }
	}
    }
  if (matches)
    {
      gdb_printf (_("Selectors matching \"%s\":\n\n"), 
		  regexp ? regexp : "*");

      sym_arr = XALLOCAVEC (struct symbol *, matches);
      matches = 0;
      for (objfile *objfile : current_program_space->objfiles ())
	{
	  for (minimal_symbol *msymbol : objfile->msymbols ())
	    {
	      QUIT;
	      name = msymbol->natural_name ();
	      if (name &&
		  (name[0] == '-' || name[0] == '+') &&
		  name[1] == '[')		/* Got a method name.  */
		{
		  /* Filter for class/instance methods.  */
		  if (plusminus && name[0] != plusminus)
		    continue;
		  /* Find selector part.  */
		  name = (char *) strchr(name+2, ' ');
		  if (regexp == NULL || re_exec(++name) != 0)
		    sym_arr[matches++] = (struct symbol *) msymbol;
		}
	    }
	}

      qsort (sym_arr, matches, sizeof (struct minimal_symbol *),
	     compare_selectors);
      /* Prevent compare on first iteration.  */
      asel[0] = 0;
      for (ix = 0; ix < matches; ix++)	/* Now do the output.  */
	{
	  char *p = asel;

	  QUIT;
	  name = sym_arr[ix]->natural_name ();
	  name = strchr (name, ' ') + 1;
	  if (p[0] && specialcmp(name, p) == 0)
	    continue;		/* Seen this one already (not unique).  */

	  /* Copy selector part.  */
	  while (*name && *name != ']')
	    *p++ = *name++;
	  *p++ = '\0';
	  /* Print in columns.  */
	  puts_tabular(asel, maxlen + 1, 0);
	}
      begin_line();
    }
  else
    gdb_printf (_("No selectors matching \"%s\"\n"),
		regexp ? regexp : "*");
}

/*
 * Function: compare_classes (const void *, const void *)
 *
 * Comparison function for use with qsort.  Arguments are symbols or
 * msymbols Compares class part of objc method name alphabetically.
 */

static int
compare_classes (const void *a, const void *b)
{
  const char *aname, *bname;

  aname = (*(struct symbol **) a)->print_name ();
  bname = (*(struct symbol **) b)->print_name ();
  if (aname == NULL || bname == NULL)
    error (_("internal: compare_classes(1)"));

  return specialcmp (aname+1, bname+1);
}

/*
 * Function: classes_info(regexp, from_tty)
 *
 * Implements the "info classes" command for objective c classes.
 * Lists all objective c classes that match the optional regexp.
 * Works by grepping thru the list of objective c methods.  List will
 * be sorted and uniqued (since one class may have many methods).
 * BUGS: will not list a class that has no methods.
 */

static void
info_classes_command (const char *regexp, int from_tty)
{
  const char            *name;
  char                  *val;
  int                    matches = 0;
  int                    maxlen  = 0;
  int                    ix;
  char                   myregexp[2048];
  char                   aclass[256];
  struct symbol        **sym_arr;

  if (regexp == NULL)
    strcpy(myregexp, ".* ");	/* Null input: match all objc classes.  */
  else
    {
      /* Allow a few extra bytes because of the strcat below.  */
      if (sizeof (myregexp) < strlen (regexp) + 4)
	error (_("Regexp is too long: %s"), regexp);
      strcpy(myregexp, regexp);
      if (myregexp[strlen(myregexp) - 1] == '$')
	/* In the method name, the end of the class name is marked by ' '.  */
	myregexp[strlen(myregexp) - 1] = ' ';
      else
	strcat(myregexp, ".* ");
    }

  if (regexp != NULL)
    {
      val = re_comp (myregexp);
      if (val != 0)
	error (_("Invalid regexp (%s): %s"), val, regexp);
    }

  /* First time thru is JUST to get max length and count.  */
  for (objfile *objfile : current_program_space->objfiles ())
    {
      for (minimal_symbol *msymbol : objfile->msymbols ())
	{
	  QUIT;
	  name = msymbol->natural_name ();
	  if (name &&
	      (name[0] == '-' || name[0] == '+') &&
	      name[1] == '[')			/* Got a method name.  */
	    if (regexp == NULL || re_exec(name+2) != 0)
	      {
		/* Compute length of classname part.  */
		const char *mystart = name + 2;
		const char *myend   = strchr (mystart, ' ');

		if (myend && (myend - mystart > maxlen))
		  maxlen = myend - mystart;
		matches++;
	      }
	}
    }
  if (matches)
    {
      gdb_printf (_("Classes matching \"%s\":\n\n"), 
		  regexp ? regexp : "*");
      sym_arr = XALLOCAVEC (struct symbol *, matches);
      matches = 0;
      for (objfile *objfile : current_program_space->objfiles ())
	{
	  for (minimal_symbol *msymbol : objfile->msymbols ())
	    {
	      QUIT;
	      name = msymbol->natural_name ();
	      if (name &&
		  (name[0] == '-' || name[0] == '+') &&
		  name[1] == '[') /* Got a method name.  */
		if (regexp == NULL || re_exec(name+2) != 0)
		  sym_arr[matches++] = (struct symbol *) msymbol;
	    }
	}

      qsort (sym_arr, matches, sizeof (struct minimal_symbol *),
	     compare_classes);
      /* Prevent compare on first iteration.  */
      aclass[0] = 0;
      for (ix = 0; ix < matches; ix++)	/* Now do the output.  */
	{
	  char *p = aclass;

	  QUIT;
	  name = sym_arr[ix]->natural_name ();
	  name += 2;
	  if (p[0] && specialcmp(name, p) == 0)
	    continue;	/* Seen this one already (not unique).  */

	  /* Copy class part of method name.  */
	  while (*name && *name != ' ')
	    *p++ = *name++;
	  *p++ = '\0';
	  /* Print in columns.  */
	  puts_tabular(aclass, maxlen + 1, 0);
	}
      begin_line();
    }
  else
    gdb_printf (_("No classes matching \"%s\"\n"), regexp ? regexp : "*");
}

static char *
parse_selector (char *method, char **selector)
{
  char *s1 = NULL;
  char *s2 = NULL;
  int found_quote = 0;

  char *nselector = NULL;

  gdb_assert (selector != NULL);

  s1 = method;

  s1 = skip_spaces (s1);
  if (*s1 == '\'')
    {
      found_quote = 1;
      s1++;
    }
  s1 = skip_spaces (s1);

  nselector = s1;
  s2 = s1;

  for (;;)
    {
      if (isalnum (*s2) || (*s2 == '_') || (*s2 == ':'))
	*s1++ = *s2;
      else if (isspace (*s2))
	;
      else if ((*s2 == '\0') || (*s2 == '\''))
	break;
      else
	return NULL;
      s2++;
    }
  *s1++ = '\0';

  s2 = skip_spaces (s2);
  if (found_quote)
    {
      if (*s2 == '\'')
	s2++;
      s2 = skip_spaces (s2);
    }

  if (selector != NULL)
    *selector = nselector;

  return s2;
}

static char *
parse_method (char *method, char *type, char **theclass,
	      char **category, char **selector)
{
  char *s1 = NULL;
  char *s2 = NULL;
  int found_quote = 0;

  char ntype = '\0';
  char *nclass = NULL;
  char *ncategory = NULL;
  char *nselector = NULL;

  gdb_assert (type != NULL);
  gdb_assert (theclass != NULL);
  gdb_assert (category != NULL);
  gdb_assert (selector != NULL);

  s1 = method;

  s1 = skip_spaces (s1);
  if (*s1 == '\'')
    {
      found_quote = 1;
      s1++;
    }
  s1 = skip_spaces (s1);

  if ((s1[0] == '+') || (s1[0] == '-'))
    ntype = *s1++;

  s1 = skip_spaces (s1);

  if (*s1 != '[')
    return NULL;
  s1++;

  nclass = s1;
  while (isalnum (*s1) || (*s1 == '_'))
    s1++;

  s2 = s1;
  s2 = skip_spaces (s2);

  if (*s2 == '(')
    {
      s2++;
      s2 = skip_spaces (s2);
      ncategory = s2;
      while (isalnum (*s2) || (*s2 == '_'))
	s2++;
      *s2++ = '\0';
    }

  /* Truncate the class name now that we're not using the open paren.  */
  *s1++ = '\0';

  nselector = s2;
  s1 = s2;

  for (;;)
    {
      if (isalnum (*s2) || (*s2 == '_') || (*s2 == ':'))
	*s1++ = *s2;
      else if (isspace (*s2))
	;
      else if (*s2 == ']')
	break;
      else
	return NULL;
      s2++;
    }
  *s1++ = '\0';
  s2++;

  s2 = skip_spaces (s2);
  if (found_quote)
    {
      if (*s2 != '\'')
	return NULL;
      s2++;
      s2 = skip_spaces (s2);
    }

  if (type != NULL)
    *type = ntype;
  if (theclass != NULL)
    *theclass = nclass;
  if (category != NULL)
    *category = ncategory;
  if (selector != NULL)
    *selector = nselector;

  return s2;
}

static void
find_methods (char type, const char *theclass, const char *category,
	      const char *selector,
	      std::vector<const char *> *symbol_names)
{
  const char *symname = NULL;

  char ntype = '\0';
  char *nclass = NULL;
  char *ncategory = NULL;
  char *nselector = NULL;

  static char *tmp = NULL;
  static unsigned int tmplen = 0;

  gdb_assert (symbol_names != NULL);

  for (objfile *objfile : current_program_space->objfiles ())
    {
      unsigned int *objc_csym;

      /* The objfile_csym variable counts the number of ObjC methods
	 that this objfile defines.  We save that count as a private
	 objfile data.	If we have already determined that this objfile
	 provides no ObjC methods, we can skip it entirely.  */

      unsigned int objfile_csym = 0;

      objc_csym = objc_objfile_data.get (objfile);
      if (objc_csym != NULL && *objc_csym == 0)
	/* There are no ObjC symbols in this objfile.  Skip it entirely.  */
	continue;

      for (minimal_symbol *msymbol : objfile->msymbols ())
	{
	  QUIT;

	  /* Check the symbol name first as this can be done entirely without
	     sending any query to the target.  */
	  symname = msymbol->natural_name ();
	  if (symname == NULL)
	    continue;

	  if ((symname[0] != '-' && symname[0] != '+') || (symname[1] != '['))
	    /* Not a method name.  */
	    continue;

	  objfile_csym++;

	  /* Now that thinks are a bit sane, clean up the symname.  */
	  while ((strlen (symname) + 1) >= tmplen)
	    {
	      tmplen = (tmplen == 0) ? 1024 : tmplen * 2;
	      tmp = (char *) xrealloc (tmp, tmplen);
	    }
	  strcpy (tmp, symname);

	  if (parse_method (tmp, &ntype, &nclass,
			    &ncategory, &nselector) == NULL)
	    continue;

	  if ((type != '\0') && (ntype != type))
	    continue;

	  if ((theclass != NULL)
	      && ((nclass == NULL) || (strcmp (theclass, nclass) != 0)))
	    continue;

	  if ((category != NULL) &&
	      ((ncategory == NULL) || (strcmp (category, ncategory) != 0)))
	    continue;

	  if ((selector != NULL) &&
	      ((nselector == NULL) || (strcmp (selector, nselector) != 0)))
	    continue;

	  symbol_names->push_back (symname);
	}

      if (objc_csym == NULL)
	objc_csym = objc_objfile_data.emplace (objfile, objfile_csym);
      else
	/* Count of ObjC methods in this objfile should be constant.  */
	gdb_assert (*objc_csym == objfile_csym);
    }
}

/* Uniquify a vector of strings.  */

static void
uniquify_strings (std::vector<const char *> *strings)
{
  if (strings->empty ())
    return;

  std::sort (strings->begin (), strings->end (), compare_cstrings);
  strings->erase (std::unique (strings->begin (), strings->end (), streq),
		  strings->end ());
}

/*
 * Function: find_imps (const char *selector, struct symbol **sym_arr)
 *
 * Input:  a string representing a selector
 *         a pointer to an array of symbol pointers
 *         possibly a pointer to a symbol found by the caller.
 *
 * Output: number of methods that implement that selector.  Side
 * effects: The array of symbol pointers is filled with matching syms.
 *
 * By analogy with function "find_methods" (symtab.c), builds a list
 * of symbols matching the ambiguous input, so that "decode_line_2"
 * (symtab.c) can list them and ask the user to choose one or more.
 * In this case the matches are objective c methods
 * ("implementations") matching an objective c selector.
 *
 * Note that it is possible for a normal (c-style) function to have
 * the same name as an objective c selector.  To prevent the selector
 * from eclipsing the function, we allow the caller (decode_line_1) to
 * search for such a function first, and if it finds one, pass it in
 * to us.  We will then integrate it into the list.  We also search
 * for one here, among the minsyms.
 *
 * NOTE: if NUM_DEBUGGABLE is non-zero, the sym_arr will be divided
 *       into two parts: debuggable (struct symbol) syms, and
 *       non_debuggable (struct minimal_symbol) syms.  The debuggable
 *       ones will come first, before NUM_DEBUGGABLE (which will thus
 *       be the index of the first non-debuggable one).
 */

const char *
find_imps (const char *method, std::vector<const char *> *symbol_names)
{
  char type = '\0';
  char *theclass = NULL;
  char *category = NULL;
  char *selector = NULL;

  char *buf = NULL;
  char *tmp = NULL;

  int selector_case = 0;

  gdb_assert (symbol_names != NULL);

  buf = (char *) alloca (strlen (method) + 1);
  strcpy (buf, method);
  tmp = parse_method (buf, &type, &theclass, &category, &selector);

  if (tmp == NULL)
    {
      strcpy (buf, method);
      tmp = parse_selector (buf, &selector);

      if (tmp == NULL)
	return NULL;

      selector_case = 1;
    }

  find_methods (type, theclass, category, selector, symbol_names);

  /* If we hit the "selector" case, and we found some methods, then
     add the selector itself as a symbol, if it exists.  */
  if (selector_case && !symbol_names->empty ())
    {
      struct symbol *sym = lookup_symbol (selector, NULL, VAR_DOMAIN,
					  0).symbol;

      if (sym != NULL)
	symbol_names->push_back (sym->natural_name ());
      else
	{
	  struct bound_minimal_symbol msym
	    = lookup_minimal_symbol (selector, 0, 0);

	  if (msym.minsym != NULL)
	    symbol_names->push_back (msym.minsym->natural_name ());
	}
    }

  uniquify_strings (symbol_names);

  return method + (tmp - buf);
}

static void
print_object_command (const char *args, int from_tty)
{
  struct value *object, *function, *description;
  CORE_ADDR string_addr, object_addr;
  int i = 0;
  gdb_byte c = 0;

  if (!args || !*args)
    error (
"The 'print-object' command requires an argument (an Objective-C object)");

  {
    expression_up expr = parse_expression (args);

    object = expr->evaluate (builtin_type (expr->gdbarch)->builtin_data_ptr);
  }

  /* Validate the address for sanity.  */
  object_addr = value_as_long (object);
  if( ! (object_addr & 0x1)) // don't do this if its TPS
     read_memory (object_addr, &c, 1);

  function = find_function_in_inferior ("_NSPrintForDebugger", NULL);
  if (function == NULL)
    error (_("Unable to locate _NSPrintForDebugger in child process"));

  description = call_function_by_hand (function, NULL, object);

  string_addr = value_as_long (description);
  if (string_addr == 0)
    error (_("object returns null description"));

  read_memory (string_addr + i++, &c, 1);
  if (c != 0)
    do
      { /* Read and print characters up to EOS.  */
	QUIT;
	gdb_printf ("%c", c);
	read_memory (string_addr + i++, &c, 1);
      } while (c != 0);
  else
    gdb_printf(_("<object returns empty description>"));
  gdb_printf ("\n");
}


void _initialize_objc_language ();
void
_initialize_objc_language ()
{
  add_info ("selectors", info_selectors_command,
       _("All Objective-C selectors, or those matching REGEXP."));
  add_info ("classes", info_classes_command,
       _("All Objective-C classes, or those matching REGEXP."));
  cmd_list_element *print_object_cmd
    = add_com ("print-object", class_vars, print_object_command,
      _         ("Ask an Objective-C object to print itself."));
  add_com_alias ("po", print_object_cmd, class_vars, 1);
}


#if DEBUG_VERBOSE
static size_t  read_c_string(struct gdbarch *gdbarch, CORE_ADDR addr, char *buf, size_t n)
{
   char       *sentinel;
   char       *p;
   gdb_byte   c;

   if( ! n)
      return;

   if( ! addr)
   {
      if( n < 5)
         strcpy( buf, "NULL");
      else
         buf[ 0] = 0;
      return;
   }

   p        = buf;
   sentinel = &p[ n];
   while( p < sentinel)
   {
     read_memory( addr++, &c, 1);
     *p++ = c;
     if( ! c)
      return( p - buf);
   }
   buf[ n - 1] = 0;
   return( p - buf);
}
#endif

static void
read_objc_method (struct gdbarch *gdbarch, CORE_ADDR addr,
		  struct objc_method *method)
{
  enum bfd_endian byte_order = gdbarch_byte_order (gdbarch);
  // @mulle-gdb@ fix size >
  int   len;

#if DEBUG_VERBOSE
  fprintf( stderr, "%s :: %p\n", __PRETTY_FUNCTION__, (void *) addr);
#endif

  len           = gdbarch_ptr_bit( gdbarch) / 8;

  // this is really a uint32_t so.... lets read 4 only but assume ptr
  // alignment, runtime should make this explicit!
  method->sel   = read_memory_unsigned_integer (addr, 4, byte_order);
  addr += len;
  method->types = read_memory_unsigned_integer( addr, len, byte_order);
  addr += len;
  method->name  = read_memory_unsigned_integer( addr, len, byte_order);
  addr += len;

  addr += len; // skip bits
  method->imp   = read_memory_unsigned_integer( addr, len, byte_order);

#if DEBUG_VERBOSE
  {
     char   buf[ 256];

     fprintf( stderr, "%s :: sel   = %p\n", __PRETTY_FUNCTION__, (void *) method->sel );
     read_c_string( gdbarch, method->types, buf, sizeof( buf));
     fprintf( stderr, "%s :: types = %s\n", __PRETTY_FUNCTION__, buf);
     read_c_string( gdbarch, method->name, buf, sizeof( buf));
     fprintf( stderr, "%s :: name  = %s\n", __PRETTY_FUNCTION__, buf);
     fprintf( stderr, "%s :: imp   = %p\n", __PRETTY_FUNCTION__, (void *) method->imp);
  }
#endif
}

static unsigned long
read_objc_methlist_nmethods (struct gdbarch *gdbarch, CORE_ADDR addr)
{
  enum bfd_endian byte_order = gdbarch_byte_order (gdbarch);
  // @mulle-gdb@ fix size >
  int   len;

  len = gdbarch_ptr_bit( gdbarch) / 8;
  return read_memory_unsigned_integer( addr, len, byte_order);
  // @mulle-obcj@ fix size <
}

static void
read_objc_methlist_method (struct gdbarch *gdbarch, CORE_ADDR addr,
			   unsigned long num, struct objc_method *method)
{
  gdb_assert (num < read_objc_methlist_nmethods (gdbarch, addr));
  int   len;

#if DEBUG_VERBOSE
  fprintf( stderr, "%s :: %p %ld\n", __PRETTY_FUNCTION__, (void *) addr, num);
#endif

  len   = gdbarch_ptr_bit( gdbarch) / 8;
  addr += len * 2;
  read_objc_method (gdbarch, addr + (5 * len * num), method);
}


static CORE_ADDR
read_universe( void)
{
   struct bound_minimal_symbol universe_sym;
   CORE_ADDR universe;

   universe_sym = lookup_bound_minimal_symbol("mulle_objc_defaultuniverse");
   if( ! universe_sym.minsym)
    universe_sym = lookup_bound_minimal_symbol("_mulle_objc_defaultuniverse");

   if( ! universe_sym.minsym)
   {
#if DEBUG_VERBOSE
    fprintf( stderr, "%s :: universe not found\n", __PRETTY_FUNCTION__);
#endif
    return ( 0);
   }

   universe = universe_sym.value_address();
   return( universe);
}


struct runtime_version_loadbits
{
   uint32_t  version;
   uint32_t  loadbits; // 0 for < 0.24
};


static struct runtime_version_loadbits
read_runtime_version_loadbits(struct gdbarch *gdbarch)
{
  enum bfd_endian byte_order = gdbarch_byte_order (gdbarch);
  int   len;
  struct runtime_version_loadbits   info = { 0 };

#if DEBUG_VERBOSE
   fprintf( stderr, "%s\n", __PRETTY_FUNCTION__);
#endif
   CORE_ADDR universe;

   universe = read_universe();
   if( ! universe)
      return( info);

   CORE_ADDR version_p;
   ULONGEST version;

   len        = gdbarch_ptr_bit( gdbarch) / 8;
   version_p  = universe;
   version_p += len; // skip cache

   version = read_memory_unsigned_integer( version_p, len, byte_order);
   info.version = (uint32_t) (uintptr_t) version;

   if( info.version < 24)
   {
      CORE_ADDR loadbits_p;
      ULONGEST loadbits;

      loadbits_p = version_p + len; // skip cache
      loadbits = read_memory_unsigned_integer( loadbits_p, len, byte_order);
      info.loadbits = (uint32_t) (uintptr_t) loadbits;
   }
   return( info);
}



/* x86_64: magic offsets, address arithmetic in mulle_objc_classpair
 * see: "mulle-objc-runtime/test-compiler-runtime/gdb"
 *
 * These offsets change for each release though. We need a way to get the
 * current runtime version and then use the proper offsets. Or put the offsets
 * into the universe! Alternative the classpair, contains these offsets...
 *
 * pair.infraclass      = 16
 * pair.metaclass       = 480
 * pair.protocolclasses = 800
 * i686: magic offsets
 * pair.infraclass      = 8
 * pair.metaclass       = 248
 * pair.protocolclasses = 412
 */


struct gdb_objc_runtime_offsets
{
   int infraclass;
   int metaclass;
   int protocolclasses;
};

struct gdb_objc_runtime_version_arch_offsets
{
   struct
   {
      int  major;
      int  minor;
   } version;
   struct gdb_objc_runtime_offsets  b64;
   struct gdb_objc_runtime_offsets  b32;
};


static struct gdb_objc_runtime_offsets *
   get_version_arch_offsets( int major, int minor, int bits, int tao)
{
   static struct gdb_objc_runtime_version_arch_offsets  runtime_offsets[] =
   {
      { { 0, 18 }, { 16, 496, 840 }, { 8, 248, 420 } },
      { { 0, 19 }, { 16, 496, 848 }, { 8, 248, 424 } },
      { { 0, 20 }, { 16, 480, 800 }, { 8, 248, 412 } },
      { { 0, 24 }, { 16, 480, 792 }, { 8, 248, 412 } }
   };
#define n_runtime_offsets (sizeof( runtime_offsets) / sizeof( runtime_offsets[ 0]))

   static struct gdb_objc_runtime_version_arch_offsets  tao_runtime_offsets[] =
   {
      { { 0, 24 }, { 32, 512, 824 }, { 16, 272, 436 } }
   };
#define n_tao_runtime_offsets (sizeof( tao_runtime_offsets) / sizeof( tao_runtime_offsets[ 0]))

   int   i;
   int   n;
   struct gdb_objc_runtime_version_arch_offsets  *p;

   if( tao)
   {
      n = n_tao_runtime_offsets;
      p = tao_runtime_offsets;
   }
   else
   {
      n = n_runtime_offsets;
      p = runtime_offsets;
   }


   for( i = 0; i < n; i++)
   {
      if( p->version.major == major && p->version.minor == minor)
      {
         switch( bits)
         {
         case 64 : return( &runtime_offsets[ i].b64);
         case 32 : return( &runtime_offsets[ i].b32);
         }
         break;
      }
      ++p;
   }
   return( NULL);
}

static struct  runtime_version_loadbits  runtime_version_loadbits_info;

static uint32_t  mulle_objc_runtime_version( struct gdbarch *gdbarch)
{
   if( ! runtime_version_loadbits_info.version)
      runtime_version_loadbits_info = read_runtime_version_loadbits( gdbarch);
   return( runtime_version_loadbits_info.version);
}


static uint32_t  mulle_objc_runtime_loadbits( struct gdbarch *gdbarch)
{
   if( ! runtime_version_loadbits_info.version)
      runtime_version_loadbits_info = read_runtime_version_loadbits( gdbarch);
   return( runtime_version_loadbits_info.loadbits);
}


static int  mulle_objc_runtime_tao( struct gdbarch *gdbarch)
{
   uint32_t   loadbits;

   loadbits = mulle_objc_runtime_loadbits( gdbarch);
   return( (loadbits & 0x10) ? 1 :0 );
}



static struct gdb_objc_runtime_offsets  *
   gdb_runtime_offset_arch( struct gdbarch *gdbarch, int bits)
{
   uint32_t   version;
   uint32_t   major, minor;
   uint32_t   tao;

   version = mulle_objc_runtime_version( gdbarch);
   major   = (version >> 20);
   minor   = (version >> 8) & (1024-1);
   tao     = mulle_objc_runtime_tao( gdbarch);
   return( get_version_arch_offsets( major, minor, bits, tao));
}




static void
read_objc_object (struct gdbarch *gdbarch, CORE_ADDR addr,
		  struct objc_object *object)
{
  enum bfd_endian byte_order = gdbarch_byte_order (gdbarch);
  int   len;
  int   tpsIndex;

#if DEBUG_VERBOSE
  fprintf( stderr, "%s :: %p\n", __PRETTY_FUNCTION__, (void *) addr);
#endif
  len = gdbarch_ptr_bit( gdbarch) / 8;

  if( ! (addr & 0x1))
  {
     addr = addr - len;
     object->isa = read_memory_unsigned_integer( addr, len, byte_order);
     return;
  }
   // it's a TPS instance! Get the mulle_objc_defaultuniverse
   CORE_ADDR universe;
   CORE_ADDR tpsTable;

   universe = read_universe();
   if( ! universe)
   {
      // fprintf( stderr, "%s :: universe not found\n", __PRETTY_FUNCTION__);
      memset( object, 0, sizeof( *object));
      return;
   }


   // fprintf( stderr, "%s :: universe=%p\n", __PRETTY_FUNCTION__, (void *) universe);
   // now get to TPS table
   tpsTable  = universe;
   tpsTable += len;     // skip cache
   tpsTable += 2 * len; // skip version + path
   if( mulle_objc_runtime_tao( gdbarch)) // skip loadbits
      tpsTable += len;

   tpsTable += 6 * (3 * len); // skip 6 hashmaps
   tpsTable += 3 * (3 * len); // skip 3 pointerarrays

   // arrive at "taggedpointers"
   // fprintf( stderr, "%s :: tpsTable=%p\n", __PRETTY_FUNCTION__, (void *) tpsTable);
   tpsIndex = addr & (len == 8 ? 0x7 : 0x3);
   // fprintf( stderr, "%s :: tpsIndex=%d\n", __PRETTY_FUNCTION__, tpsIndex);

   object->isa = read_memory_unsigned_integer( tpsTable + tpsIndex * len, len, byte_order);
}


static void
read_objc_super (struct gdbarch *gdbarch, CORE_ADDR addr,
		 struct objc_super *super)
{
  enum bfd_endian byte_order = gdbarch_byte_order (gdbarch);
  int   len;

  // fprintf( stderr, "%s :: %p\n", __PRETTY_FUNCTION__, (void *) addr);

  len = gdbarch_ptr_bit( gdbarch) / 8;
  // clearly non optimally alignment reading...
  super->classid  = read_memory_unsigned_integer (addr + 2 * len, 4, byte_order);
  // fprintf( stderr, "%s :: classid=%p\n", __PRETTY_FUNCTION__, (void *) super->classid);
  super->methodid = read_memory_unsigned_integer (addr + 2 * len + 4, 4, byte_order);
  // fprintf( stderr, "%s :: methodid=%p\n", __PRETTY_FUNCTION__, (void *) super->methodid);
};


static CORE_ADDR
search_hashtable(struct gdbarch *gdbarch, CORE_ADDR addr,
                 CORE_ADDR search)
{
  enum bfd_endian byte_order = gdbarch_byte_order (gdbarch);
// @mulle-gdb@ fix isa offset >
  int   len;

  // fprintf( stderr, "%s :: %p\n", __PRETTY_FUNCTION__, (void *) addr);

  len = gdbarch_ptr_bit( gdbarch) / 8;

  CORE_ADDR  storage;

  storage = read_memory_unsigned_integer( addr, len, byte_order);
  // fprintf( stderr, "%s :: storage=%p\n", __PRETTY_FUNCTION__,  (void *) storage);
  if( ! storage)
     return( 0);

  CORE_ADDR  nhashs;

  nhashs = read_memory_unsigned_integer( storage, len, byte_order);
  // fprintf( stderr, "%s :: nhashs=%p\n", __PRETTY_FUNCTION__,  (void *) nhashs);
  if( ! nhashs)
     return( 0);

  CORE_ADDR  key;
  CORE_ADDR  value;
  CORE_ADDR  mask;
  CORE_ADDR  entries;
  CORE_ADDR  endEntries;

  mask   = read_memory_unsigned_integer( storage + len, len, byte_order);
  // fprintf( stderr, "%s :: mask=%p\n", __PRETTY_FUNCTION__,  (void *) mask);

  entries    = storage + len * 2;
  endEntries = entries + len * 2 * mask;
  while( entries <= endEntries)
  {
    key      = read_memory_unsigned_integer( entries, len, byte_order);
    entries += len;
    //if( key)
    //   fprintf( stderr, "%s :: key=%p\n", __PRETTY_FUNCTION__, (void *) key);

    if( search == key)
    {
       value = read_memory_unsigned_integer( entries, len, byte_order);
       // fprintf( stderr, "%s :: value=%p\n", __PRETTY_FUNCTION__, (void *)  value);
       return( value);
    }
    entries += len;
  }
  return( 0);
}


// @mulle-gdb@ read class
// struct _mulle_objc_cachepivot
// {
//    mulle_atomic_pointer_t   entries; // for atomic XCHG with pointer indirection
// };
// struct _mulle_objc_methodcachepivot
// {
//    struct _mulle_objc_cachepivot   pivot; // for atomic XCHG with pointer indirection
// };
// struct _mulle_objc_class
// {
//    struct _mulle_objc_methodcachepivot     cachepivot;  // DON'T MOVE
//
//    /* ^^^ keep above like this, or change mulle_objc_fastmethodtable fault (void **)[ 0] */
//
//    // keep name, superclass, allocationsize in this order for gdb debugging
//    struct _mulle_objc_class                *superclass;      // keep here for debugger (void **)[ 1]
//    char                                    *name;            // offset (void **)[ 2]
//    uintptr_t                               allocationsize;   // instancesize + header   (void **)[ 3]
//
//    struct _mulle_objc_infraclass           *infraclass;
//    struct _mulle_objc_universe             *universe;
//
//    struct mulle_concurrent_pointerarray    methodlists;
//
//    //
//    // TODO: we could have a pointer to the load class and get the id
//    //       information that way. (wouldn't save a lot though)
//    //       or move to classpair
//    //
//    mulle_objc_classid_t                    classid;
//    mulle_objc_classid_t                    superclassid;
//
//    // TODO: general storage mechanism for KVC, needed in meta ? move to classpair ?
//    uint16_t                                inheritance;
//    uint16_t                                preloads;
//
//    // vvv - from here on the debugger doesn't care
static void
read_objc_class (struct gdbarch *gdbarch, CORE_ADDR addr,
		 struct objc_class *theclass)
{
  enum bfd_endian byte_order = gdbarch_byte_order (gdbarch);
// @mulle-gdb@  >
  int   len;

#if DEBUG_VERBOSE
  fprintf( stderr, "%s :: %p\n", __PRETTY_FUNCTION__, (void *) addr);
#endif

  len = gdbarch_ptr_bit( gdbarch) / 8;

  // isa = &[ -1]
  theclass->isa = read_memory_unsigned_integer( addr - len, len, byte_order);

  addr += len; // skip cachepivot

  // &[ 1]
  theclass->super_class = read_memory_unsigned_integer( addr, len, byte_order);
  addr += len;

  theclass->name = read_memory_unsigned_integer( addr, len, byte_order);
  addr += len;

#if DEBUG_VERBOSE
  if( theclass->name)
  {
     char   buf[ 256];

     read_c_string( gdbarch, theclass->name, buf, sizeof( buf));
     fprintf( stderr, "%s :: name = %s\n", __PRETTY_FUNCTION__, buf);
  }
#endif


  theclass->allocation_size = read_memory_unsigned_integer( addr, len, byte_order);
  addr += len;

  theclass->infra_class = read_memory_unsigned_integer( addr, len, byte_order);
  addr += len;

  theclass->universe = read_memory_unsigned_integer( addr, len, byte_order);
  addr += len;

  // fprintf( stderr, "%s :: &method_lists=%p\n", __PRETTY_FUNCTION__, (void *) addr);

  theclass->methods = read_memory_unsigned_integer( addr, len, byte_order);
  addr += len * 3;

  theclass->classid = read_memory_unsigned_integer(addr, 4, byte_order);
  addr += 4;

//  theclass->superclassid = read_memory_unsigned_integer(addr, 4, byte_order);
  addr += 4;

  theclass->inheritance = read_memory_unsigned_integer(addr, 2, byte_order);
//  addr += 2;

// fprintf( stderr, "%s :: method_lists->storage.storage=%p\n", __PRETTY_FUNCTION__, (void *) theclass->methods);
//  theclass->ivars = read_memory_unsigned_integer (addr + 24, 4, byte_order);
//  theclass->cache = read_memory_unsigned_integer (addr + 32, 4, byte_order);
//  theclass->protocols = read_memory_unsigned_integer (addr + 36, 4, byte_order);
// @mulle-gdb@  <

  // small sanity checks
  if( theclass->inheritance >= 0x20 ||
      theclass->classid == 0 || theclass->classid == -1 ||
      theclass->allocation_size < 0)
  {
#if DEBUG_VERBOSE
     fprintf( stderr, "%s :: class looks broken", __PRETTY_FUNCTION__);
#endif

      memset( theclass, 0, sizeof( *theclass));
  }
}


enum
{
   MULLE_OBJC_CLASS_DONT_INHERIT_SUPERCLASS          = 0x01,
   MULLE_OBJC_CLASS_DONT_INHERIT_CATEGORIES          = 0x02,
   MULLE_OBJC_CLASS_DONT_INHERIT_PROTOCOLS           = 0x04,
   MULLE_OBJC_CLASS_DONT_INHERIT_PROTOCOL_CATEGORIES = 0x08,
   MULLE_OBJC_CLASS_DONT_INHERIT_PROTOCOL_META       = 0x10,
   MULLE_OBJC_CLASS_DONT_INHERIT_CLASS               = 0x20
};

static unsigned long
read_objc_pointerarray_count(struct gdbarch *gdbarch, CORE_ADDR addr)
{
  enum bfd_endian byte_order = gdbarch_byte_order (gdbarch);
  // @mulle-gdb@ fix size >
  int   len;

  len = gdbarch_ptr_bit( gdbarch) / 8;
  return read_memory_unsigned_integer( addr, len, byte_order);
  // @mulle-obcj@ fix size <
}


static CORE_ADDR
read_objc_pointerarray_entry(struct gdbarch *gdbarch,
                             CORE_ADDR addr,
                             unsigned long num)
{
  enum bfd_endian byte_order = gdbarch_byte_order (gdbarch);

  gdb_assert (num < read_objc_methlist_nmethods (gdbarch, addr));
  int   len;

  // fprintf( stderr, "%s :: %p %ld\n", __PRETTY_FUNCTION__, (void *) addr, num);

  len = gdbarch_ptr_bit( gdbarch) / 8;
  addr += len * 2;  // entrie
  return read_memory_unsigned_integer( addr + (num * len), len, byte_order);
}


static inline CORE_ADDR
metaclass_of_infraclass( struct gdbarch *gdbarch,
                         CORE_ADDR infraAddr)
{
  int   bits;
  struct gdb_objc_runtime_offsets  *offsets;

  bits    = gdbarch_ptr_bit( gdbarch);
  offsets = gdb_runtime_offset_arch( gdbarch, bits);
  if( ! offsets)
     return( 0);
  // assert( bits == 64 || bits == 32);
  return( infraAddr + offsets->metaclass - offsets->infraclass);
}


// static inline CORE_ADDR
// infraclass_of_metaclass( struct gdbarch *gdbarch,
//                          CORE_ADDR metaAddr)
// {
//   if( gdbarch_ptr_bit( gdbarch) == 64)
//      return( metaAddr - 480 + 16);
//   return( metaAddr - 248 + 8);
// }


//
// just after the metaclass we run into the classpair struct with
// the protocolclasses conveniently at this point. It contains
// a pointer to the storage, which we read
static inline CORE_ADDR
protocolclass_array_of_metaclass( struct gdbarch *gdbarch,
                                  CORE_ADDR metaAddr)
{
  enum  bfd_endian byte_order = gdbarch_byte_order (gdbarch);
  int   len;
  int   bits;
  struct gdb_objc_runtime_offsets  *offsets;

  bits    = gdbarch_ptr_bit( gdbarch);
  offsets = gdb_runtime_offset_arch( gdbarch, bits);
  if( ! offsets)
     return( 0);

  len  = bits / 8;
  return( read_memory_unsigned_integer( metaAddr +
            offsets->protocolclasses -
            offsets->metaclass, len, byte_order));
}


static CORE_ADDR
search_superclass( struct gdbarch *gdbarch,
                   CORE_ADDR classAddr,
                   struct objc_class *p_class,
                   CORE_ADDR inheritance)
{
   struct objc_class   superclass_str;
   CORE_ADDR           supercls;

#if DEBUG_VERBOSE
  fprintf( stderr, "%s :: %p %p\n", __PRETTY_FUNCTION__, (void *) classAddr, (void *)inheritance);
#endif

   if( inheritance & MULLE_OBJC_CLASS_DONT_INHERIT_SUPERCLASS)
   {
#if DEBUG_VERBOSE
      fprintf( stderr, "%s :: not inheriting from superclass!\n", __PRETTY_FUNCTION__);
#endif
      return( 0);
   }

   supercls = p_class->super_class;
   if( ! p_class->infra_class)     // is a infra, (so has no infra companion, only meta)
      return( supercls);

   read_objc_class( gdbarch, supercls, &superclass_str);
   if( superclass_str.isa == 0)
      return( 0);
   if( superclass_str.infra_class) // is superclass a meta
      return( supercls);

   if( ! (inheritance & MULLE_OBJC_CLASS_DONT_INHERIT_PROTOCOL_META))
   {
      // Ok we'd be transitioning from metaclass to infraclass
      // Use protocolclass if available
      CORE_ADDR   protocolclassesAddr;
      CORE_ADDR   protoclassAddr;
      CORE_ADDR   protoclassMetaAddr;
      unsigned long  i, n;

      protocolclassesAddr = protocolclass_array_of_metaclass( gdbarch, classAddr);
      if( ! protocolclassesAddr)
         return( 0);
      // get storage


      // first entry after meta is then the protocol classes array
      // in class pair (nota reverse enumeration here)
      n = read_objc_pointerarray_count( gdbarch, protocolclassesAddr);
      for( i = 0; i < n; i++)
      {
         protoclassAddr = read_objc_pointerarray_entry( gdbarch,
                                                        protocolclassesAddr,
                                                        i);
         if( ! protoclassAddr)
            break;

         protoclassMetaAddr = metaclass_of_infraclass( gdbarch, protoclassAddr);
         if( ! protoclassMetaAddr)
            break;
         if( protoclassMetaAddr != classAddr)
         {
#if DEBUG_VERBOSE
            fprintf( stderr, "%s :: found protocolclass %p\n", __PRETTY_FUNCTION__, (void *) protoclassMetaAddr);
#endif

            supercls = protoclassMetaAddr;
            break;
         }
      }
   }
   return( supercls);
}


// @mulle-gdb@ we are traversing at first a pointer to  a
// struct _mulle_concurrent_pointerarraystorage
// {
//    mulle_atomic_pointer_t   n;
//    uintptr_t                size;
//
//    mulle_atomic_pointer_t   entries[ 1];
// };


//
// TODO: need to reimplement the complete searching logic from
//       the runtime here....
//
static CORE_ADDR
find_implementation_in_methodlist(struct gdbarch *gdbarch,
                                  CORE_ADDR mlist,
                                  CORE_ADDR sel)
{
  unsigned long nmethods;
  unsigned long i;
  struct objc_method meth_str;

#if DEBUG_VERBOSE
  fprintf( stderr, "%s :: %p %p\n", __PRETTY_FUNCTION__, (void *) mlist, (void *)sel);
#endif

  nmethods = read_objc_methlist_nmethods (gdbarch, mlist);
#if DEBUG_VERBOSE
  fprintf( stderr, "%s :: nmethods=%p\n", __PRETTY_FUNCTION__, (void *) nmethods);
#endif
  for (i = 0; i < nmethods; i++)
  {
     read_objc_methlist_method( gdbarch, mlist, i, &meth_str);

     if (meth_str.sel == sel)
     {
        /* FIXME: hppa arch was doing a pointer dereference
           here.  There needs to be a better way to do that.  */
        // fprintf( stderr, "%s match on %p\n", __PRETTY_FUNCTION__, (void *) meth_str.sel);
        return meth_str.imp;
     }
   }
   return( 0);
}


static CORE_ADDR
find_implementation_in_methodlist_array(struct gdbarch *gdbarch,
                                        CORE_ADDR methodlist_array,
                                        CORE_ADDR inheritance,
                                        CORE_ADDR sel)
{
   CORE_ADDR   mlistnum;
   CORE_ADDR   mlist;
   CORE_ADDR   found;


#if DEBUG_VERBOSE
   fprintf( stderr, "%s :: %p %p\n", __PRETTY_FUNCTION__, (void *) methodlist_array, (void *)sel);
#endif
   mlistnum = read_objc_pointerarray_count( gdbarch, methodlist_array);


#if DEBUG_VERBOSE
   fprintf( stderr, "%s :: mlistnum=%ld\n", __PRETTY_FUNCTION__, (long) mlistnum);
#endif
   if( ! mlistnum)  // can't happen
      return( 0);

   if( inheritance & MULLE_OBJC_CLASS_DONT_INHERIT_CATEGORIES)
       mlistnum = 1;

   // reverse enumerate
   while( mlistnum)
   {
     --mlistnum;
     if( (inheritance & MULLE_OBJC_CLASS_DONT_INHERIT_CLASS) && mlistnum == 0)
        break;

     mlist = read_objc_pointerarray_entry( gdbarch, methodlist_array, mlistnum);
#if DEBUG_VERBOSE
     fprintf( stderr, "%s :: mlist=%p\n", __PRETTY_FUNCTION__, (void *) mlist);
#endif
     if (mlist == 0)
       break;

     found = find_implementation_in_methodlist( gdbarch, mlist, sel);
     if( found)
        return( found);
   }
   return( 0);
}


// static struct _mulle_objc_method  *
//    _mulle_objc_class_protocol_search_method( struct _mulle_objc_class *cls,
//                                              struct _mulle_objc_searcharguments *search,
//                                              unsigned int inheritance,
//                                              struct _mulle_objc_searchresult *result,
//                                              enum internal_search_mode *mode)
// {
//    struct _mulle_objc_classpair                        *pair;
//    struct _mulle_objc_infraclass                       *infra;
//    struct _mulle_objc_class                            *walk_cls;
//    struct _mulle_objc_infraclass                       *proto_cls;
//    struct _mulle_objc_infraclass                       *next_proto_cls;
//    struct _mulle_objc_protocolclassreverseenumerator   rover;
//    struct _mulle_objc_method                           *found;
//    struct _mulle_objc_method                           *method;
//    int                                                 is_meta;
//
//    found        = MULLE_OBJC_METHOD_SEARCH_FAIL;
//    pair         = _mulle_objc_class_get_classpair( cls);
//    infra        = _mulle_objc_classpair_get_infraclass( pair);
//    is_meta      = _mulle_objc_class_is_metaclass( cls);
//    inheritance |= MULLE_OBJC_CLASS_DONT_INHERIT_SUPERCLASS;
//
//    rover          = _mulle_objc_classpair_reverseenumerate_protocolclasses( pair);
//    next_proto_cls = _mulle_objc_protocolclassreverseenumerator_next( &rover);
//    while( proto_cls = next_proto_cls)
//    {
//       next_proto_cls = _mulle_objc_protocolclassreverseenumerator_next( &rover);
//       if( proto_cls == infra)
//          continue;
//
//       walk_cls = _mulle_objc_infraclass_as_class( proto_cls);
//       if( is_meta)
//          walk_cls = _mulle_objc_metaclass_as_class( _mulle_objc_infraclass_get_metaclass( proto_cls));
//
//       method = __mulle_objc_class_search_method( walk_cls,
//                                                  search,
//                                                  inheritance | walk_cls->inheritance,
//                                                  result,
//                                                  mode);
//       if( method == MULLE_OBJC_METHOD_SEARCH_FAIL)
//          continue;
//
//       if( ! method)
//       {
//          found = NULL;
//          break;
//       }
//
//       if( found != MULLE_OBJC_METHOD_SEARCH_FAIL)
//       {
//          result->error = EEXIST;
//          found = NULL;
//          break;
//       }
//
//       found = method;
//
//       if( ! _mulle_objc_descriptor_is_hidden_override_fatal( &method->descriptor))
//          break;
//    }
//    _mulle_objc_protocolclassreverseenumerator_done( &rover);
//
//    return( found);
// }


static CORE_ADDR
find_implementation_from_class (struct gdbarch *gdbarch,
                                CORE_ADDR addr,
                                CORE_ADDR sel,
                                long inheritance,
                                long startClassid);

static CORE_ADDR
find_implementation_from_protocol_classes( struct gdbarch *gdbarch,
                                           CORE_ADDR addr,
                                           struct objc_class *p_class,
                                           CORE_ADDR sel,
                                           long inheritance,
                                           long startClassid)
{
   CORE_ADDR           found;
   CORE_ADDR           infraAddr;
   CORE_ADDR           metaAddr;
   CORE_ADDR           protocolclassesAddr;
   CORE_ADDR           protoclassAddr;
   unsigned long       i, n;
   int                 is_meta;
   struct objc_class   class_str;

#if DEBUG_VERBOSE
   fprintf( stderr, "%s :: %p %p (%s)\n", __PRETTY_FUNCTION__, (void *) addr, (void *) sel, p_class->infra_class ? "is meta" : "is infra");
#endif
   if( p_class->infra_class)  // is meta
   {
      is_meta   = 1;
      infraAddr = p_class->infra_class;
      metaAddr  = addr;
   }
   else
   {
      is_meta   = 0;
      infraAddr = addr;
      metaAddr  = metaclass_of_infraclass( gdbarch, infraAddr);
      if( ! metaAddr)
         return( 0);

      read_objc_class( gdbarch, metaAddr, &class_str);
      if( class_str.isa == 0)
         return( 0);
      p_class  = &class_str;
   }

   // get classpair
   // where is it
   // offset
   protocolclassesAddr = protocolclass_array_of_metaclass( gdbarch, metaAddr);
   if( ! protocolclassesAddr)
   {
#if DEBUG_VERBOSE
      fprintf( stderr, "%s :: failed to acquire protocol classes from meta\n", __PRETTY_FUNCTION__);
#endif
      return( 0);
   }
   // storage

   // first entry after meta is then the protocol classes array
   // in class pair (nota reverse enumeration here)
   n = read_objc_pointerarray_count( gdbarch, protocolclassesAddr);
   for( i = n; i;)
   {
      --i;
      protoclassAddr = read_objc_pointerarray_entry( gdbarch, protocolclassesAddr, i);
      if( protoclassAddr == infraAddr)
         continue;

      if( ! protoclassAddr)
         break;

      if( is_meta)
      {
         protoclassAddr = metaclass_of_infraclass( gdbarch, protoclassAddr);
         if( ! protoclassAddr)
            return( 0);
      }

      // just look through local list and don't walk
      found = find_implementation_from_class( gdbarch, protoclassAddr, sel, 0xFFFF, startClassid);
      if( found)
         return( found);
   }
   return( 0);
}


static CORE_ADDR
find_implementation_from_class (struct gdbarch *gdbarch,
			                       CORE_ADDR addr,
                                CORE_ADDR sel,
                                long inheritance,
                                long startClassid)
{
  CORE_ADDR classAddr;
  CORE_ADDR found;
  struct objc_class class_str;

#if DEBUG_VERBOSE
  fprintf( stderr, "%s :: %p %p\n", __PRETTY_FUNCTION__, (void *) addr, (void *)sel);
#endif

  classAddr = addr;
  while( classAddr != 0)
  {
      read_objc_class( gdbarch, classAddr, &class_str);
      if( class_str.isa == 0)
      {
#if DEBUG_VERBOSE
         fprintf( stderr, "%s :: could not read class\n", __PRETTY_FUNCTION__);
#endif
         return( 0);
      }

      if( inheritance == -1)
         inheritance = class_str.inheritance;

      // ignore everthing until and including startClassid is found
      if( startClassid)
      {
         if( class_str.classid == startClassid)
            startClassid = 0;
      }
      else
      {
         found = find_implementation_in_methodlist_array( gdbarch, class_str.methods, inheritance, sel);
         if( found)
            return( found);
      }

      if( ! (inheritance & MULLE_OBJC_CLASS_DONT_INHERIT_PROTOCOLS))
      {
          CORE_ADDR  tmp;

          tmp = inheritance;
          if( inheritance & MULLE_OBJC_CLASS_DONT_INHERIT_PROTOCOL_CATEGORIES)
             tmp |= MULLE_OBJC_CLASS_DONT_INHERIT_CATEGORIES;

          found = find_implementation_from_protocol_classes( gdbarch,
                                                             classAddr,
                                                             &class_str,
                                                             sel,
                                                             tmp,
                                                             startClassid);
          if( found)
            return( found);
      }
      else
      {
#if DEBUG_VERBOSE
        fprintf( stderr, "%s :: not inheriting protocol classes!\n", __PRETTY_FUNCTION__);
#endif
      }

      classAddr = search_superclass( gdbarch, classAddr, &class_str, inheritance);
      inheritance = -1; // use superclass inheritance
  }

#if DEBUG_VERBOSE
  fprintf( stderr, "%s :: could not find method in class hierarchy\n", __PRETTY_FUNCTION__);
#endif
  return 0;
}


static CORE_ADDR
find_implementation (struct gdbarch *gdbarch,
          		      CORE_ADDR object, CORE_ADDR sel)
{
  struct objc_object ostr;

#if DEBUG_VERBOSE
  fprintf( stderr, "%s :: %p %p\n", __PRETTY_FUNCTION__, (void *) object, (void *)sel);
#endif
  if (object == 0)
    return 0;
  read_objc_object (gdbarch, object, &ostr);
#if DEBUG_VERBOSE
  fprintf( stderr, "%s :: isa=%p\n", __PRETTY_FUNCTION__, (void *) ostr.isa);
#endif
  if (ostr.isa == 0)
    return 0;

  return find_implementation_from_class (gdbarch, ostr.isa, sel, -1, 0);
}


#ifndef __cplusplus
CORE_ADDR
objc_find_implementation_from_class( struct gdbarch *gdbarch,
                                     CORE_ADDR classAddr,
                                     CORE_ADDR sel,
                                     long inheritance,
                                     long classid)
{
   return( find_implementation_from_class( gdbarch, classAddr, sel, inheritance, classid));
}
#else

// not part of the test

/* The data structure 'methcalls' is used to detect method calls (thru
 * ObjC runtime lib functions objc_msgSend, objc_msgSendSuper, etc.),
 * and ultimately find the method being called.
 */

struct objc_methcall {
  const char *name;
 /* Return instance method to be called.  */
  int (*stop_at) (CORE_ADDR, CORE_ADDR *);
  /* Start of pc range corresponding to method invocation.  */
  CORE_ADDR begin;
  /* End of pc range corresponding to method invocation.  */
  CORE_ADDR end;
};

static int resolve_msgsend (CORE_ADDR pc, CORE_ADDR *new_pc);
//static int resolve_msgsend_stret (CORE_ADDR pc, CORE_ADDR *new_pc);
static int resolve_msgsend_super (CORE_ADDR pc, CORE_ADDR *new_pc);
//static int resolve_msgsend_super_stret (CORE_ADDR pc, CORE_ADDR *new_pc);
static int resolve_msgsend_class (CORE_ADDR pc, CORE_ADDR *new_pc);

//
// the inline function might appear with no optimization compilation
// the MULLE_C_ALWAYS_INLINE is not super-respected by compilers
//
static struct objc_methcall methcalls[] = {
//  { "_objc_msgSend", resolve_msgsend, 0, 0},
//  { "_objc_msgSend_stret", resolve_msgsend_stret, 0, 0},
//  { "_objc_msgSendSuper", resolve_msgsend_super, 0, 0},
//  { "_objc_msgSendSuper_stret", resolve_msgsend_super_stret, 0, 0},
//  { "_objc_getClass", NULL, 0, 0},
//  { "mulle_objc_object_call", resolve_msgsend, 0, 0},

// TODO: mulle_objc_object_call is found like this only I don't know why
  { "mulle_objc_object_call", resolve_msgsend, 0, 0},
  { "_mulle_objc_object_call", resolve_msgsend, 0, 0},
  { "mulle_objc_object_call_inline_full", resolve_msgsend, 0, 0},
  { "mulle_objc_object_call_inline", resolve_msgsend, 0, 0},
  { "mulle_objc_object_call_inline_minimal", resolve_msgsend, 0, 0},
  { "mulle_objc_object_call_inline_partial", resolve_msgsend, 0, 0},
  { "mulle_objc_object_call2", resolve_msgsend, 0, 0},
  { "mulle_objc_object_call_variablemethodid", resolve_msgsend, 0, 0},
  { "mulle_objc_object_call_variablemethodid_inline", resolve_msgsend, 0, 0},
  { "mulle_objc_object_call_variablemethodid_inline_full", resolve_msgsend, 0, 0},
  { "mulle_objc_object_call_class", resolve_msgsend_class, 0, 0},
  { "mulle_objc_object_call_class_needcache", resolve_msgsend_class, 0, 0},
  { "mulle_objc_object_supercall", resolve_msgsend_super, 0, 0},
  { "mulle_objc_object_supercall2", resolve_msgsend_super, 0, 0},
  { "mulle_objc_object_supercall_inline_full", resolve_msgsend_super, 0, 0},
  { "mulle_objc_object_supercall_inline", resolve_msgsend_super, 0, 0},
  { "mulle_objc_object_supercall_inline_partial", resolve_msgsend_super, 0, 0},
  { "mulle_objc_global_lookup_infraclass_nofail", NULL, 0, 0 },

  { "_mulle_objc_object_call", resolve_msgsend, 0, 0},
  { "__mulle_objc_object_call", resolve_msgsend, 0, 0},
  { "_mulle_objc_object_call_inline_full", resolve_msgsend, 0, 0},
  { "_mulle_objc_object_call_inline", resolve_msgsend, 0, 0},
  { "_mulle_objc_object_call_inline_minimal", resolve_msgsend, 0, 0},
  { "_mulle_objc_object_call_inline_partial", resolve_msgsend, 0, 0},
  { "_mulle_objc_object_call2", resolve_msgsend, 0, 0},
  { "_mulle_objc_object_call_variablemethodid", resolve_msgsend, 0, 0},
  { "_mulle_objc_object_call_variablemethodid_inline", resolve_msgsend, 0, 0},
  { "_mulle_objc_object_call_variablemethodid_inline_full", resolve_msgsend, 0, 0},
  { "_mulle_objc_object_call_class", resolve_msgsend_class, 0, 0},
  { "_mulle_objc_object_call_class_needcache", resolve_msgsend_class, 0, 0},
  { "_mulle_objc_object_supercall", resolve_msgsend_super, 0, 0},
  { "_mulle_objc_object_supercall_inline_full", resolve_msgsend_super, 0, 0},
  { "_mulle_objc_object_supercall_inline", resolve_msgsend_super, 0, 0},
  { "_mulle_objc_object_supercall_inline_partial", resolve_msgsend_super, 0, 0},
  { "_mulle_objc_global_lookup_infraclass_nofail", NULL, 0, 0 }
//  { "_objc_getMetaClass", NULL, 0, 0}
};

#define nmethcalls (sizeof (methcalls) / sizeof (methcalls[0]))


/* The following function, "find_objc_msgsend", fills in the data
 * structure "objc_msgs" by finding the addresses of each of the
 * (currently four) functions that it holds (of which objc_msgSend is
 * the first).  This must be called each time symbols are loaded, in
 * case the functions have moved for some reason.
 */

static void
find_objc_msgsend (void)
{
   unsigned int i;

#if DEBUG_VERBOSE
   fprintf( stderr, "%s :: looking for message sender\n",
                __PRETTY_FUNCTION__);
#endif

   for (i = 0; i < nmethcalls; i++)
   {
      struct bound_minimal_symbol func;

      /* Try both with and without underscore.  */
      func = lookup_bound_minimal_symbol (methcalls[i].name);
      if ((func.minsym == NULL) && (methcalls[i].name[0] == '_'))
      {
        func = lookup_bound_minimal_symbol (methcalls[i].name + 1);
      }

      if (func.minsym == NULL)
      {
#if DEBUG_VERBOSE
        fprintf( stderr, "%s :: did not find \"%s\"\n",
                      __PRETTY_FUNCTION__, methcalls[i].name);
#endif
        methcalls[i].begin = 0;
        methcalls[i].end = 0;
        continue;
      }

      methcalls[i].begin = func.value_address ();
      methcalls[i].end = minimal_symbol_upper_bound (func);
#if DEBUG_VERBOSE
      fprintf( stderr, "%s :: found \"%s\" at %p-%p\n",
                        __PRETTY_FUNCTION__,
                        methcalls[i].name,
                        (void *) methcalls[i].begin,
                        (void *) methcalls[i].end);
#endif
   }
}


/* find_objc_msgcall (replaces pc_off_limits)
 *
 * ALL that this function now does is to determine whether the input
 * address ("pc") is the address of one of the Objective-C message
 * dispatch functions (mainly objc_msgSend or objc_msgSendSuper), and
 * if so, it returns the address of the method that will be called.
 *
 * The old function "pc_off_limits" used to do a lot of other things
 * in addition, such as detecting shared library jump stubs and
 * returning the address of the shlib function that would be called.
 * That functionality has been moved into the gdbarch_skip_trampoline_code and
 * IN_SOLIB_TRAMPOLINE macros, which are resolved in the target-
 * dependent modules.
 */

static int
find_objc_msgcall_submethod (int (*f) (CORE_ADDR, CORE_ADDR *),
              CORE_ADDR pc,
              CORE_ADDR *new_pc)
{
  try
    {
      if (f (pc, new_pc) == 0)
      {
#if DEBUG_VERBOSE
         fprintf( stderr, "%s :: found endpoint %p -> %p\n",
                        __PRETTY_FUNCTION__, (void *) pc, (void *) *new_pc);
#endif

         return 1;
      }
    }
  catch (const gdb_exception_error &ex)
    {
#if DEBUG_VERBOSE
      fprintf( stderr, "%s :: booom shakalaka\n", __PRETTY_FUNCTION__);
#endif
      exception_fprintf (gdb_stderr, ex,
          "Unable to determine target of "
          "Objective-C method call (ignoring):\n");
    }
#if DEBUG_VERBOSE
  fprintf( stderr, "%s :: did not find %p (lookup: %p)\n", __PRETTY_FUNCTION__, (void *) pc, (void *) f);
#endif
  return 0;
}


int
find_objc_msgcall (CORE_ADDR pc, CORE_ADDR *new_pc)
{
  unsigned int i;

  find_objc_msgsend();
  if (new_pc != NULL)
    {
      *new_pc = 0;
    }

#if DEBUG_VERBOSE
  fprintf( stderr, "%s :: %p\n", __PRETTY_FUNCTION__, (void *) pc);
#endif
  for (i = 0; i < nmethcalls; i++)
    if ((pc >= methcalls[i].begin) && (pc < methcalls[i].end))
    {
#if DEBUG_VERBOSE
      fprintf( stderr, "%s :: match %p\n", __PRETTY_FUNCTION__, (void *) methcalls[i].begin);
#endif
      if (methcalls[i].stop_at != NULL)
        return find_objc_msgcall_submethod (methcalls[i].stop_at,
                        pc, new_pc);
      // if we have no stop at, its probably calls lookup which we consider
      // boring
#if DEBUG_VERBOSE
      fprintf( stderr, "%s :: but no stop at known\n", __PRETTY_FUNCTION__);
#endif
      *new_pc = (CORE_ADDR) -1;
      return( 1);
    }

   return 0;
}



static int
resolve_msgsend (CORE_ADDR pc, CORE_ADDR *new_pc)
{
  frame_info_ptr frame = get_current_frame ();
  struct gdbarch *gdbarch = get_frame_arch (frame);
  struct type *ptr_type = builtin_type (gdbarch)->builtin_data_ptr;

  CORE_ADDR object;
  CORE_ADDR sel;
  CORE_ADDR res;

#if DEBUG_VERBOSE
  fprintf( stderr, "%s :: %p (%p)\n", __PRETTY_FUNCTION__, (void *) pc, frame);
#endif
  object = gdbarch_fetch_pointer_argument (gdbarch, frame, 0, ptr_type);

#if DEBUG_VERBOSE
  fprintf( stderr, "%s :: object=%p\n", __PRETTY_FUNCTION__, (void *) object);
#endif
  sel = gdbarch_fetch_pointer_argument (gdbarch, frame, 1, ptr_type);

#if DEBUG_VERBOSE
  fprintf( stderr, "%s :: sel=%p\n", __PRETTY_FUNCTION__, (void *) sel);
#endif
  res = find_implementation (gdbarch, object, sel);
  if (new_pc != 0)
    *new_pc = res;
  if (res == 0)
    return 1;
  return 0;
}

// @mulle-gdb@ comment out resolve_msgsend_stret >
//static int
//resolve_msgsend_stret (CORE_ADDR pc, CORE_ADDR *new_pc)
//{
//  frame_info_ptr frame = get_current_frame ();
//  struct gdbarch *gdbarch = get_frame_arch (frame);
//  struct type *ptr_type = builtin_type (gdbarch)->builtin_func_ptr;
//
//  CORE_ADDR object;
//  CORE_ADDR sel;
//  CORE_ADDR res;
//
//  object = gdbarch_fetch_pointer_argument (gdbarch, frame, 1, ptr_type);
//  sel = gdbarch_fetch_pointer_argument (gdbarch, frame, 2, ptr_type);
//
//  res = find_implementation (gdbarch, object, sel);
//  if (new_pc != 0)
//    *new_pc = res;
//  if (res == 0)
//    return 1;
//  return 0;
//}
// @mulle-gdb@ comment out resolve_msgsend_stret <

static int
resolve_msgsend_super (CORE_ADDR pc, CORE_ADDR *new_pc)
{
  frame_info_ptr frame = get_current_frame ();
  struct gdbarch *gdbarch = get_frame_arch (frame);
  struct type *ptr_type = builtin_type (gdbarch)->builtin_data_ptr;
  struct objc_super  ssup;

  CORE_ADDR superid;

  // super is a little tricky, we need to lookup the class via
  // the universe...

//  object  = gdbarch_fetch_pointer_argument (gdbarch, frame, 0, ptr_type);
//  sel     = gdbarch_fetch_pointer_argument (gdbarch, frame, 1, ptr_type);
#if DEBUG_VERBOSE
  fprintf( stderr, "%s :: %p (%p)\n", __PRETTY_FUNCTION__, (void *) pc, frame);
#endif

  superid = gdbarch_fetch_pointer_argument (gdbarch, frame, 3, ptr_type);
#if DEBUG_VERBOSE
  fprintf( stderr, "%s :: superid=%p\n", __PRETTY_FUNCTION__, (void *) superid);
#endif
  if( ! superid)
     return( 0);

  CORE_ADDR universe;
  CORE_ADDR superTable;
  CORE_ADDR classTable;

  universe = read_universe();
  if( ! universe)
     return( 0);

  int   len;

  len = gdbarch_ptr_bit( gdbarch) / 8;

  //    _mulle_concurrent_hashmap_lookup
#if DEBUG_VERBOSE
  fprintf( stderr, "%s :: universe=%p\n", __PRETTY_FUNCTION__, (void *) universe);
#endif
      // now get to TPS table
  classTable  = universe;
  classTable += len; // skip cache
  classTable += 2 * len; // skip version + path
  if( mulle_objc_runtime_tao( gdbarch)) // skip loadbits
   classTable += len;
  superTable  = classTable;
  superTable += 4 * (3 * len); // skip 4 hashmaps

  CORE_ADDR superInfo;

  superInfo = search_hashtable( gdbarch, superTable, superid);
  if( ! superInfo)
     return( 0);

  read_objc_super( gdbarch, superInfo, &ssup);
  if( ssup.classid == 0 || ssup.methodid == 0)
    return 0;

  CORE_ADDR classAddr;

  classAddr = search_hashtable( gdbarch, classTable, ssup.classid);
  if( ! classAddr)
     return( 0);

  CORE_ADDR res;

  res = find_implementation_from_class( gdbarch, classAddr, ssup.methodid, -1, ssup.classid);
  if (new_pc != 0)
    *new_pc = res;
  if (res == 0)
    return 1;
  return 0;
}


static int
resolve_msgsend_class (CORE_ADDR pc, CORE_ADDR *new_pc)
{
  frame_info_ptr frame = get_current_frame ();
  struct gdbarch *gdbarch = get_frame_arch (frame);
  struct type *ptr_type = builtin_type (gdbarch)->builtin_data_ptr;

  CORE_ADDR classAddr;
  CORE_ADDR methodid;

//  object  = gdbarch_fetch_pointer_argument (gdbarch, frame, 0, ptr_type);
  methodid = gdbarch_fetch_pointer_argument (gdbarch, frame, 1, ptr_type);
  if( ! methodid)
     return( 0);

#if DEBUG_VERBOSE
  fprintf( stderr, "%s :: %p (%p)\n", __PRETTY_FUNCTION__, (void *) pc, frame);
#endif

  classAddr = gdbarch_fetch_pointer_argument (gdbarch, frame, 3, ptr_type);
#if DEBUG_VERBOSE
  fprintf( stderr, "%s :: classAddr=%p\n", __PRETTY_FUNCTION__, (void *) classAddr);
#endif
  if( ! classAddr)
     return( 0);

  CORE_ADDR res;

  res = find_implementation_from_class( gdbarch, classAddr, methodid, -1, 0);
  if (new_pc != 0)
    *new_pc = res;
  if (res == 0)
    return 1;
  return 0;
}


//static int
//resolve_msgsend_super_stret (CORE_ADDR pc, CORE_ADDR *new_pc)
//{
//  struct frame_info *frame = get_current_frame ();
//  struct gdbarch *gdbarch = get_frame_arch (frame);
//  struct type *ptr_type = builtin_type (gdbarch)->builtin_func_ptr;
//
//  struct objc_super sstr;
//
//  CORE_ADDR super;
//  CORE_ADDR sel;
//  CORE_ADDR res;
//
//  super = gdbarch_fetch_pointer_argument (gdbarch, frame, 1, ptr_type);
//  sel = gdbarch_fetch_pointer_argument (gdbarch, frame, 2, ptr_type);
//
//  read_objc_super (gdbarch, super, &sstr);
//  if (sstr.theclass == 0)
//    return 0;
//
//  res = find_implementation_from_class (gdbarch, sstr.theclass, sel);
//  if (new_pc != 0)
//    *new_pc = res;
//  if (res == 0)
//    return 1;
//  return 0;
//}
#endif
