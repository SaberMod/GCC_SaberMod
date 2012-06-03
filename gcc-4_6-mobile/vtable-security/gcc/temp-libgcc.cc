#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <execinfo.h>

#include "threaded-hash.h"
#include "vtv_memory_pool/vtvmalloc.h"

#ifndef __cplusplus
#error "This file must be compiled with a C++ compiler"
#endif

static int debug_hash = 1;
static int debug_functions = 0;
static int debug_register_pairs = 0;
static int row_length = 30;
static FILE *log_file_fp =NULL;

void *
__VLTChangePermission (char *arg1, int len)
{
  const char *perm = (const char *) arg1;

  if (debug_functions)
    {
      if (strncmp (perm, "rw", 2) == 0)
	fprintf (stdout, "Changing VLT permisisons to Read-Write.\n");
      else if (strncmp (perm, "ro", 2) == 0)
	fprintf (stdout, "Changing VLT permissions to Read-only.\n");
      else
	fprintf (stdout, "Unrecognized permission string: %s\n", perm);
    }

  if (strncmp (perm, "rw", 2) == 0)
    {
      VTV_malloc_init ();
      VTV_unprotect ();
    }
  else if (strncmp (perm, "ro", 2) == 0)
    VTV_protect ();

  if (debug_register_pairs)
    {
      if (strncmp (perm, "rw", 2) == 0)
	{
	  if (!log_file_fp)
	    log_file_fp = fopen ("/tmp/vlt_register_pairs.log", "a");
	}
      /*  -- If we close the log file here, we can't access if in
	  __VerifyVtablePointer. --
      else
	{
	  if (log_file_fp)
	    fclose (log_file_fp);
	}
      */
    }

  return NULL;
}

typedef int * vptr;

  /* For some reason, when the char * names get passed into these
     functions, they are missing the '\0' at the end; therefore we
     also pass in the length of the string and make sure, when writing
     out the names, that we only write out the correct number of
     bytes. */
void
log_register_pairs (FILE *fp, const char *format_string_dummy, int format_arg1,
		    int format_arg2, char *base_var_name, char *vtable_name,
		    vptr vtbl_ptr)
{
  char format_string[50];

  /* format_string needs to contain something like "%.10s" (for
     example) to write a vtable_name that is of length
     10. Unfortunately the length varies with every name, so we need to
     generate a new format string, with the correct length, EACH TIME.
     That is what the 'format_string_dummy' parameter is for.  It
     contains something like '%%.%ds', and we then use that plus the
     length argument to generate the correct format_string, to allow
     us to write out the string that is missing the '\0' at it's
     end. */

  sprintf (format_string, format_string_dummy, format_arg1, format_arg2);

  fprintf (fp, format_string, base_var_name, vtable_name, vtbl_ptr);
}

  /* For some reason, when the char * names get passed into these
     functions, they are missing the '\0' at the end; therefore we
     also pass in the length of the string and make sure, when writing
     out the names, that we only write out the correct number of
     bytes. */
void
print_debugging_message (const char *format_string_dummy, int format_arg1,
			 int format_arg2,
			 char *str_arg1, char *str_arg2)
{
  char format_string[50];

  /* format_string needs to contain something like "%.10s" (for
     example) to write a vtable_name that is of length
     10. Unfortunately the length varies with every name, so we need to
     generate a new format string, with the correct length, EACH TIME.
     That is what the 'format_string_dummy' parameter is for.  It
     contains something like '%%.%ds', and we then use that plus the
     length argument to generate the correct format_string, to allow
     us to write out the string that is missing the '\0' at it's
     end. */

  sprintf (format_string, format_string_dummy, format_arg1, format_arg2);

  fprintf (stdout, format_string, str_arg1, str_arg2);
}

void *
__VLTRegisterPair (void **data_pointer, void *test_value,
		   char *base_ptr_var_name, int len1, char *vtable_name,
		   int len2)
{
  struct vlt_hashtable **base_vtbl_row_ptr = (struct vlt_hashtable **) data_pointer;
  vptr vtbl_ptr = (vptr) test_value;

  if ((*base_vtbl_row_ptr) == NULL)
    *base_vtbl_row_ptr = vlt_hash_init_table ();

  if (base_ptr_var_name && vtable_name && debug_functions)
    print_debugging_message ("Registering %%.%ds : %%.%ds\n", len1, len2,
			     base_ptr_var_name, vtable_name);
  if (debug_register_pairs)
    {
      if (!log_file_fp)
	log_file_fp = fopen ("/tmp/vlt_register_pairs.log", "a");
      log_register_pairs (log_file_fp, "Registering %%.%ds : %%.%ds (%%p)\n",
			  len1, len2,
			  base_ptr_var_name, vtable_name, vtbl_ptr);
      if (*base_vtbl_row_ptr == NULL)
	fprintf (log_file_fp, "  vtable map variable is NULL.\n");
    }

  vlt_hash_insert (*base_vtbl_row_ptr, test_value);
}

static void PrintStackTrace()
{
  #define STACK_DEPTH 20
  void * callers[STACK_DEPTH];
  int actual_depth = backtrace(callers, STACK_DEPTH);
  char ** symbols = backtrace_symbols(callers, actual_depth);
  if (symbols == NULL )
    {
      fprintf(stderr, "Could not get backtrace\n");
      return;
    }

  for (int i = 0; i < actual_depth; i++)
    fprintf(stderr, "%s\n", symbols[i]);

  free(symbols);
}

void *
__VerifyVtablePointer (void **data_pointer, void *test_value,
		       char *base_vtbl_var_name, int len1, char *vtable_name,
		       int len2)
{
  struct vlt_hashtable **base_vtbl_ptr = (vlt_hashtable **) data_pointer;
  vptr obj_vptr = (vptr) test_value;
  static bool first_time = true;

  if ((*data_pointer) == NULL)
    return test_value;

  if (first_time && debug_hash)
    {
      dump_hashing_statistics ();
      first_time = false;
    }

  if (vlt_hash_find ((*base_vtbl_ptr), test_value))
    {
      if (debug_functions)
	fprintf (stdout, "Verified object vtable pointer = 0x%lx\n", obj_vptr);
    }
  else
    {
      /* The data structure is not NULL, but we failed to find our
         object's vtpr in it.  Write out information and call abort.*/
      if (base_vtbl_var_name && vtable_name)
	print_debugging_message ("Looking for %%.%ds in %%.%ds \n", len2, len1,
				 vtable_name, base_vtbl_var_name);
      fprintf (stderr, "FAILED to verify object vtable pointer=0x%lx!!!\n",
               obj_vptr);
      dump_table_to_vtbl_map_file (*base_vtbl_ptr, 1, base_vtbl_var_name,
                                   len1);
      /* Eventually we should call __stack_chk_fail (or something similar)
         rather than just abort.  */
      PrintStackTrace();
      abort ();
    }

  return test_value;
}
