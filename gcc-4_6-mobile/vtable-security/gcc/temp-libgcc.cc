#include <stdlib.h>
#include <stdio.h>
#include <string.h>


extern "C" {

int debug_functions = 0;
int debug_register_pairs = 0;
int row_length = 30;
FILE *log_file_fp =NULL;

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

struct tree_node {
  vptr value;
  struct tree_node *left;
  struct tree_node *right;
};

void
dump_binary_tree (FILE *fp, struct tree_node *root)
{
  if (!root)
    return;

  if (root->left)
    dump_binary_tree (fp, root->left);

  fprintf (fp, "%p\n", root->value);

  if (root->right)
    dump_binary_tree (fp, root->right);
}

void
dump_binary_tree_to_file (struct tree_node *root, char *var_name, int name_len)
{
  char *filename = (char *) malloc ((name_len + 11) * sizeof (char));
  char *real_name = (char *) malloc ((name_len + 1) * sizeof (char));
  FILE *dump_file_fp = NULL;

  strncpy (real_name, var_name, name_len);
  real_name[name_len] = '\0';

  sprintf (filename, "/tmp/%s.log", real_name);

  dump_file_fp = fopen (filename, "w");
  if (dump_file_fp)
    {
      dump_binary_tree (dump_file_fp, root);
      fclose (dump_file_fp);
    }

  free (real_name);
  free (filename);
}

bool
vlt_binary_tree_search (struct tree_node *root, vptr value)
{
  if (!root)
    return false;

  if (root->value == value)
    return true;
  else if (value < root->value)
    return vlt_binary_tree_search (root->left, value);
  else
    return vlt_binary_tree_search (root->right, value);

  return false;
}

/* Keeps track of whether the current call to binary_tree_insert is for
   the very tip-top root of the binary tree or not.  */

static bool root_insert = false;  

void
vlt_binary_tree_insert (struct tree_node **root, vptr value)
{
  struct tree_node *new_node;

  if (!(*root))
    {
      new_node = (struct tree_node *) malloc (sizeof (struct tree_node));
      new_node->value = value;
      new_node->left = NULL;
      new_node->right = NULL;
      (*root) = new_node;
      if (log_file_fp && root_insert)
	fprintf (log_file_fp, "     Assigning %p to new vtable_map var.\n",
		 new_node);
    }
  else if (value < (*root)->value)
    {
      root_insert = false;
      vlt_binary_tree_insert (&((*root)->left), value);
    }
  else if (value > (*root)->value)
    {
      root_insert = false;
      vlt_binary_tree_insert (&((*root)->right), value);
    }

  return;
}

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
__VLTRegisterPair (struct tree_node **base_vtbl_row_ptr, vptr vtbl_ptr, 
		   char *base_ptr_var_name, int len1, char *vtable_name, 
		   int len2)
{
  if (base_ptr_var_name && vtable_name && debug_functions)
    print_debugging_message ("Registering %%.%ds : %%.%ds\n", len1, len2,
			     base_ptr_var_name, vtable_name);
  if (debug_register_pairs)
    {
      if (!log_file_fp)
	log_file_fp = fopen ("/tmp/vlt_register_pairs.log", "a");
      log_register_pairs (log_file_fp, "Registering %%.%ds : %%.%ds (%p)\n", 
			  len1, len2,
			  base_ptr_var_name, vtable_name, vtbl_ptr);
      if (*base_vtbl_row_ptr == NULL)
	fprintf (log_file_fp, "  vtable map variable is NULL.\n");
    }

  root_insert = true;
  vlt_binary_tree_insert (base_vtbl_row_ptr, vtbl_ptr);
  root_insert = false;
}

void *
__VerifyVtablePointer (struct tree_node **base_vtbl_ptr, vptr obj_vptr, 
		       char *base_vtbl_var_name, int len1, char *vtable_name, 
		       int len2)
{
  /* The two lines below are not really right; they are there, for
     now, to deal with calls that happen during _init, possibly before
     the correct __VLTRegisterPair call has been made.  */
  if (*base_vtbl_ptr == NULL)
    return obj_vptr;

  if (vlt_binary_tree_search ((*base_vtbl_ptr), obj_vptr))
    {
      if (debug_functions)
	fprintf (stdout, "Verified object vtable pointer.\n");
    }
  else
    {
      /* The data structure is not NULL, but we failed to find our
         object's vtpr in it.  Write out information and call abort.*/
      if (base_vtbl_var_name && vtable_name)
	print_debugging_message ("Looking for %%.%ds in %%.%ds\n", len2, len1, 
				 vtable_name, base_vtbl_var_name);
      fprintf (stderr, "FAILED to verify object vtable pointer!!\n");
      dump_binary_tree_to_file (*base_vtbl_ptr, base_vtbl_var_name, len1);
      /* Eventually we should call __stack_chk_fail (or something similar)
         rather than just abort.  */
      abort ();
    }

  return obj_vptr;
}

}  /* Extern C */
