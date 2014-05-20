/* Gcc offline profile processing tool support. */
/* Compile this one with gcc.  */
/* Copyright (C) 2014 Free Software Foundation, Inc.
   Contributed by Rong Xu <xur@google.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "intl.h"
#include "hashtab.h"
#include "diagnostic.h"
#include "version.h"
#include "gcov-io.h"
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <ftw.h>
#include <getopt.h>
#include "params.h"
#include <string.h>

extern int gcov_profile_merge (struct gcov_info*, struct gcov_info*, int, int);
extern int gcov_profile_normalize (struct gcov_info*, gcov_type);
extern int gcov_profile_scale (struct gcov_info*, float, int, int);
extern struct gcov_info* gcov_read_profile_dir (const char*, int);
extern void gcov_exit (void);
extern void set_gcov_list (struct gcov_info *);
extern void gcov_set_verbose (void);
extern void set_use_existing_grouping (void);
extern void set_use_modu_list (void);
extern void lipo_set_substitute_string (const char *);

/* The following defines are needed by dyn-ipa.c.
   They will also be emitted by the compiler with -fprofile-generate,
   which means this file cannot be compiled with -fprofile-generate
   -- otherwise we get duplicated defintions.
   Make the defines weak to link with other objects/libraries
   that potentially compiled with -fprofile-generate.  */

__attribute__ ((weak)) gcov_unsigned_t __gcov_lipo_grouping_algorithm;
__attribute__ ((weak)) gcov_unsigned_t __gcov_lipo_merge_modu_edges;
__attribute__ ((weak)) gcov_unsigned_t __gcov_lipo_weak_inclusion;
__attribute__ ((weak)) gcov_unsigned_t __gcov_lipo_max_mem;
__attribute__ ((weak)) gcov_unsigned_t __gcov_lipo_comdat_algorithm;
__attribute__ ((weak)) gcov_unsigned_t __gcov_lipo_random_group_size;
__attribute__ ((weak)) gcov_unsigned_t __gcov_lipo_cutoff;
__attribute__ ((weak)) gcov_unsigned_t __gcov_lipo_random_seed;
__attribute__ ((weak)) gcov_unsigned_t __gcov_lipo_dump_cgraph;
__attribute__ ((weak)) gcov_unsigned_t __gcov_lipo_propagate_scale;

static int verbose;

/* Remove file NAME if it has a gcda suffix. */

static int
unlink_gcda_file (const char *name,
                  const struct stat *status ATTRIBUTE_UNUSED,
                  int type ATTRIBUTE_UNUSED,
                  struct FTW *ftwbuf ATTRIBUTE_UNUSED)
{
  int ret = 0;
  int len = strlen (name);
  int len1 = strlen (GCOV_DATA_SUFFIX);

  if (len > len1 && !strncmp (len -len1 + name, GCOV_DATA_SUFFIX, len1))
    remove (name);

  if (ret)
    {
      fnotice (stderr, "error in removing %s\n", name);
      exit (FATAL_EXIT_CODE);
    }

  return ret;
}

/* Remove the gcda files in PATH recursively.  */

static int
unlink_profile_dir (const char *path)
{
    return nftw(path, unlink_gcda_file, 64, FTW_DEPTH | FTW_PHYS);
}

/* Merging profile D1 and D2 with weight as W1 and W2, respectively.
   The result profile is written to directory OUT.
   Return 0 on success.  */

static int
profile_merge (const char *d1, const char *d2, const char *out, int w1, int w2)
{
  char *pwd;
  int ret;
  struct gcov_info * d1_profile;
  struct gcov_info * d2_profile;


  d1_profile = gcov_read_profile_dir (d1, 0);
  if (!d1_profile)
    return 1;

  if (d2)
    {
      d2_profile = gcov_read_profile_dir (d2, 0);
      if (!d2_profile)
        return 1;

      /* The actual merge: we overwrite to d1_profile.  */
      ret = gcov_profile_merge (d1_profile, d2_profile, w1, w2);

      if (ret)
        return ret;
    }

  /* Output new profile.  */
  unlink_profile_dir (out);
  mkdir (out, 0755);
  pwd = getcwd (NULL, 0);
  gcc_assert (pwd);
  ret = chdir (out);
  gcc_assert (ret == 0);

  set_gcov_list (d1_profile);
  gcov_exit ();

  ret = chdir (pwd);
  free (pwd);
  return 0;
}

/* Usage message for profile merge.  */

static void
print_merge_usage_message (int error_p)
{
  FILE *file = error_p ? stderr : stdout;

  fnotice (file, "  merge [options] <dir1> <dir2>         Merge coverage file contents\n");
  fnotice (file, "    -v, --verbose                       Verbose mode\n");
  fnotice (file, "    -o, --output <dir>                  Output directory\n");
  fnotice (file, "    -w, --weight <w1,w2>                Set weights (float point values)\n");
}

static const struct option merge_options[] =
{
  { "verbose",                no_argument,       NULL, 'v' },
  { "output",                 required_argument, NULL, 'o' },
  { "weight",                 required_argument, NULL, 'w' },
  { 0, 0, 0, 0 }
};

/* Print merge usage and exit.  */

static void
merge_usage (void)
{
  fnotice (stderr, "Merge subcomand usage:");
  print_merge_usage_message (true);
  exit (FATAL_EXIT_CODE);
}

/* Driver for profile merge sub-command.  */

static int
do_merge (int argc, char **argv)
{
  int opt;
  int ret;
  const char *output_dir = 0;
  int w1 = 1, w2 = 1;

  optind = 0;
  while ((opt = getopt_long (argc, argv, "vo:w:", merge_options, NULL)) != -1)
    {
      switch (opt)
        {
        case 'v':
          verbose = 1;
          gcov_set_verbose ();
          break;
        case 'o':
          output_dir = optarg;
          break;
        case 'w':
          sscanf (optarg, "%d,%d", &w1, &w2);
          if (w1 < 0 || w2 < 0)
            {
              fnotice (stderr, "weights need to be non-negative\n");
              exit (FATAL_EXIT_CODE);
            }
          break;
        default:
          merge_usage ();
        }
    }

  if (output_dir == NULL)
    output_dir = "merged_profile";

  if (argc - optind == 2)
    ret = profile_merge (argv[optind], argv[optind+1], output_dir, w1, w2);
  else
    merge_usage ();

  return ret;
}

/* If N_VAL is no-zero, normalize the profile by setting the largest counter
   counter value to N_VAL and scale others counters proportionally.
   Otherwise, multiply the all counters by SCALE.  */

static int
profile_rewrite (const char *d1, const char *out, long long n_val,
                 float scale, int n, int d)
{
  char *pwd;
  int ret;
  struct gcov_info * d1_profile;


  d1_profile = gcov_read_profile_dir (d1, 0);
  if (!d1_profile)
    return 1;

  /* Output new profile.  */
  unlink_profile_dir (out);
  mkdir (out, 0755);
  pwd = getcwd (NULL, 0);
  gcc_assert (pwd);
  ret = chdir (out);
  gcc_assert (ret == 0);

  if (n_val)
    gcov_profile_normalize (d1_profile, (gcov_type) n_val);
  else
    gcov_profile_scale (d1_profile, scale, n, d);

  set_gcov_list (d1_profile);
  gcov_exit ();

  ret = chdir (pwd);
  free (pwd);
  return 0;
}

/* This is the hashtab entry to store a name and mod_id pair. */
typedef struct {
  const char *name;
  unsigned id;
} mod_name_id;

/* Hash and comparison functions for strings.  */

static unsigned
mod_name_id_htab_hash (const void *s_p)
{
  const char *s = ((const mod_name_id *) s_p)->name;
  return (*htab_hash_string) (s);
}

static int
mod_name_id_hash_eq (const void *s1_p, const void *s2_p)
{
  return strcmp (((const mod_name_id *) s1_p)->name,
                 ((const mod_name_id *) s2_p)->name) == 0;
}

static htab_t mod_name_id_hash_table;

/* Look up an entry in the hash table. STRING is the module name.
   CREATE controls to insert to htab or not.
   If (*ID_P != 0), we write (*ID_P) to htab.
   If (*ID_P == 0), we write module_id to (*ID_P).
   return 1 if an entry is found and otherwise 0.  */

static int
module_name_hash_lookup (const char *string, unsigned *id_p, int create)
{
  void **e;
  mod_name_id t;

  t.name = string;
  e = htab_find_slot (mod_name_id_hash_table, &t,
                      create ? INSERT : NO_INSERT);
  if (e == NULL)
    return 0;
  if (*e == NULL)
    {
      *e = XNEW (mod_name_id *);
      (*(mod_name_id **)e)->name = xstrdup (string);
    }
  if (id_p)
    {
      if (*id_p != 0)
        (*(mod_name_id **)e)->id = *id_p;
      else
        *id_p = (*(mod_name_id **)e)->id;
    }
  return 1;
}

/* Return 1 if NAME is of a source type that LIPO targets.
   Return 0 otherwise.  */

static int
is_lipo_source_type (char *name)
{
  char *p;

  if (strcasestr (name, ".c") ||
      strcasestr (name, ".cc") ||
      strcasestr (name, ".cpp") ||
      strcasestr (name, ".c++"))
    return 1;

  /* Replace ".proto" with ".pb.cc". Since the two strings have the same
     length, we simplfy do a strcpy.  */
  if ((p = strcasestr (name, ".proto")) != NULL)
    {
      strcpy (p, ".pb.cc");
      return 1;
    }

  return 0;
}

/* Convert/process the names from dependence query to a
   stardard format. Return NULL if this is not a lipo
   target source. */

static char *
lipo_process_name_string (char *name)
{
  char *p;

  if (name == NULL)
    return NULL;
  if (strlen (name) == 0)
    return NULL;

  if (!is_lipo_source_type (name))
    return NULL;

  /* Overwrite ':' with '/'.  */
  if ((p = strchr (name, ':')) != NULL)
    *p = '/';

  /* Remove "//".  */
  if (name[0] == '/' && name[1] =='/')
    name += 2;

  return name;
}

/* Store the list of source modules in INPUT_FILE to internal hashtab.  */

static int
lipo_process_modu_list (const char *input_file)
{
  FILE *fd;
  char *line = NULL;
  size_t linecap = 0;
  char *name;

  set_use_modu_list ();

  if ((fd = fopen (input_file, "r")) == NULL)
    {
      fnotice (stderr, "Cannot open %s\n", input_file);
      return -1;
    }

  /* Read all the modules */
  while (getline (&line, &linecap, fd) != -1)
    {
      name = strtok (line, " \t\n");
      name = lipo_process_name_string (name);
      if (name)
        module_name_hash_lookup (name, 0, 1);

      free (line);
      line = NULL;
    }

  return 0;
}

#define GENFILE_PREFIX "/genfiles/"

/* Return 1 if module NAME is available to be used in the target
   profile.  CREATE controls to insert to htab or not.
   If (*ID_P != 0), we write (*ID_P) to htab.
   If (*ID_P == 0), we write module_id to (*ID_P).
   return 1 if an entry is found and otherwise 0.  */

int
is_module_available (const char *name, unsigned *id_p, int create)
{
  char *buf, *p;
  int ret;

  if (mod_name_id_hash_table == NULL)
    return 1;

  buf = xstrdup (name);
  /* Remove genfile string.  */
  if ((p = strstr (buf, GENFILE_PREFIX)) != NULL)
    p += strlen (GENFILE_PREFIX);
  else
    p = buf;

   ret = module_name_hash_lookup (p, id_p, create);
   free (buf);
   return ret;
}

/* Return module_ident for module NAME.
   return 0 if the module NAME is not available.  */

int
get_module_id_from_name (const char *name)
{
  unsigned mod_id = 0;
  if (is_module_available (name, &mod_id, 0) == 1)
    return mod_id;
  return 0;
}

/* Usage function for profile rewrite.  */

static void
print_rewrite_usage_message (int error_p)
{
  FILE *file = error_p ? stderr : stdout;

  fnotice (file, "  rewrite [options] <dir>               Rewrite coverage file contents\n");
  fnotice (file, "    -v, --verbose                       Verbose mode\n");
  fnotice (file, "    -o, --output <dir>                  Output directory\n");
  fnotice (file, "    -l, --modu_list <file>              Only use the modules in this file\n");
  fnotice (file, "    -r, --path_substr_replace <str>     Replace string in path\n");
  fnotice (file, "    -s, --scale <float or simple-frac>  Scale the profile counters\n");
  fnotice (file, "    -u, --use_imports_file <file>       Use the grouping in import files.\n");
  fnotice (file, "    -n, --normalize <long long>         Normalize the profile\n");
}

static const struct option rewrite_options[] =
{
  { "verbose",                no_argument,       NULL, 'v' },
  { "output",                 required_argument, NULL, 'o' },
  { "modu_list",              required_argument, NULL, 'l' },
  { "string",                 required_argument, NULL, 'r' },
  { "scale",                  required_argument, NULL, 's' },
  { "use_imports_file",       no_argument,       NULL, 'u' },
  { "normalize",              required_argument, NULL, 'n' },
  { 0, 0, 0, 0 }
};

/* Print profile rewrite usage and exit.  */

static void
rewrite_usage (void)
{
  fnotice (stderr, "Rewrite subcommand usage:");
  print_rewrite_usage_message (true);
  exit (FATAL_EXIT_CODE);
}

/* Driver for profile rewrite sub-command. */

static int
do_rewrite (int argc, char **argv)
{
  int opt;
  int ret;
  const char *output_dir = 0;
  long long normalize_val = 0;
  float scale = 1.0;
  int numerator = -1;
  int denominator = -1;

  mod_name_id_hash_table = htab_create (500, mod_name_id_htab_hash,
                                        mod_name_id_hash_eq, NULL);

  optind = 0;
  while ((opt = getopt_long (argc, argv, "vo:l:r:s:un:", rewrite_options, NULL)) != -1)
    {
      switch (opt)
        {
        case 'v':
          verbose = 1;
          gcov_set_verbose ();
          break;
        case 'o':
          output_dir = optarg;
          break;
        case 'l':
          lipo_process_modu_list (optarg);
          break;
        case 'r':
          lipo_set_substitute_string (optarg);
          break;
        case 'u':
          set_use_existing_grouping ();
          break;
        case 'n':
          if (scale != 1.0)
            {
              fnotice (stderr, "scaling cannot co-exist with normalization\n");
              scale = 1.0;
            }
          normalize_val = atoll (optarg);
          break;
        case 's':
          ret = 0;
          if (strstr (optarg, "/"))
            {
              ret = sscanf (optarg, "%d/%d", &numerator, &denominator);
              if (ret == 2)
                {
                  gcc_assert (numerator >= 0);
                  gcc_assert (denominator > 0);
                  scale = 0.0;
                }
            }
          if (ret != 2)
            {
              ret = sscanf (optarg, "%f", &scale);
              gcc_assert (ret == 1);
            }

          if (scale < 0.0)
            {
              fnotice (stderr, "scale needs to be non-negative\n");
              exit (FATAL_EXIT_CODE);
            }
          if (normalize_val != 0)
            {
              fnotice (stderr, "normalization cannot co-exist with scaling\n");
              normalize_val = 0;
            }
          break;
        default:
          rewrite_usage ();
        }
    }

  if (output_dir == NULL)
    output_dir = "rewrite_profile";

  if (argc - optind == 1)
    {
      if (denominator > 0)
        ret = profile_rewrite (argv[optind],  output_dir, 0, 0.0, numerator, denominator);
      else
        ret = profile_rewrite (argv[optind],  output_dir, normalize_val, scale, 0, 0);
    }
  else
    rewrite_usage ();

  return ret;
}

/* Print a usage message and exit.  If ERROR_P is nonzero, this is an error,
   otherwise the output of --help.  */

static void
print_usage (int error_p)
{
  FILE *file = error_p ? stderr : stdout;
  int status = error_p ? FATAL_EXIT_CODE : SUCCESS_EXIT_CODE;

  fnotice (file, "Usage: %s [OPTION]... SUB_COMMAND [OPTION]...\n\n", progname);
  fnotice (file, "Offline tool to handle gcda counts.\n\n");
  fnotice (file, "  -h, --help                            Print this help, then exit\n");
  fnotice (file, "  -v, --version                         Print version number, then exit\n");
  fnotice (file, "  -A, --lipo_algorithm <0|1>            Choose LIPO module grouping algorithm\n");
  fnotice (file, "  -E, --lipo_merge_edge                 Merge module edges in LIPO module grouping\n");
  fnotice (file, "  -W, --lipo_weak_inclusion             Don't force strict inclusion in grouping\n");
  fnotice (file, "  -C, --lipo_cutoff <0..100>            Set LIPO module grouping cutoff\n");
  fnotice (file, "  -M, --lipo_max_memory <int>           Set the max memory in LIPO module grouping\n");
  fnotice (file, "  -F, --lipo_comdat_algorithm <0|1|2|3> Set the COMDAT fixup algorithm\n");
  fnotice (file, "  -R, --lipo_random_group_size <int>    Set LIPO random grouping size\n");
  fnotice (file, "  -S, --lipo_random_group_seed <int>    Set LIPO random grouping seed\n");
  fnotice (file, "  -D, --lipo_dump_cgraph                Dump dynamic call graph\n");
  fnotice (file, "  -P, --lipo_propagate_scale            Set LIPO propagate scale to true\n");
  fnotice (file, "\n");
  print_merge_usage_message (error_p);
  print_rewrite_usage_message (error_p);
  fnotice (file, "\nFor bug reporting instructions, please see:\n%s.\n",
           bug_report_url);
  exit (status);
}

/* Print version information and exit.  */

static void
print_version (void)
{
  fnotice (stdout, "%s %s%s\n", progname, pkgversion_string, version_string);
  fnotice (stdout, "Copyright %s 2014 Free Software Foundation, Inc.\n",
           _("(C)"));
  fnotice (stdout,
           _("This is free software; see the source for copying conditions.\n"
             "There is NO warranty; not even for MERCHANTABILITY or \n"
             "FITNESS FOR A PARTICULAR PURPOSE.\n\n"));
  exit (SUCCESS_EXIT_CODE);
}

static const struct option options[] =
{
  { "help",                   no_argument,       NULL, 'h' },
  { "version",                no_argument,       NULL, 'v' },
  { "lipo_algorithm",         required_argument, NULL, 'A' },
  { "lipo_merge_edge",        no_argument,       NULL, 'E' },
  { "lipo_weak_inclusion",    no_argument,       NULL, 'W' },
  { "lipo_cutoff",            required_argument, NULL, 'C' },
  { "lipo_max_memory",        required_argument, NULL, 'M' },
  { "lipo_comdat_algorithm",  required_argument, NULL, 'F' },
  { "lipo_random_group_size", required_argument, NULL, 'R' },
  { "lipo_random_group_seed", required_argument, NULL, 'S' },
  { "lipo_dump_cgraph",       no_argument,       NULL, 'D' },
  { "lipo_propagate_scale",   no_argument,       NULL, 'P' },
  { 0, 0, 0, 0 }
};

/* Process args, return index to first non-arg.  */

static int
process_args (int argc, char **argv)
{
  int opt;
  int ret;

  while ((opt = getopt_long (argc, argv, "+hvA:EWC:M:R:S:DP", options, NULL)) != -1)
    {
      switch (opt)
        {
        case 'h':
          print_usage (false);
          /* Print_usage will exit.  */
        case 'v':
          print_version ();
          /* Print_version will exit.  */
        case 'E':
          __gcov_lipo_merge_modu_edges = 1;
          break;
        case 'W':
          __gcov_lipo_weak_inclusion = 1;
          break;
        case 'D':
          __gcov_lipo_dump_cgraph = 1;
          break;
        case 'P':
          __gcov_lipo_propagate_scale = 1;
          break;
        case 'A':
          sscanf (optarg, "%d", &ret);
          if (ret != 0 && ret != 1)
            {
              fnotice (stderr, "LIPO grouping algorithm can only be 0 or 1\n");
              exit (-1);
            }
          __gcov_lipo_grouping_algorithm = ret;
          break;
        case 'R':
          sscanf (optarg, "%d", &ret);
          if (ret < 1)
            {
              fnotice (stderr, "LIPO random group size needs to be positive\n");
              exit (-1);
            }
          __gcov_lipo_random_group_size = ret;
          break;
        case 'S':
          sscanf (optarg, "%d", &ret);
          __gcov_lipo_random_seed = ret;;
          break;
        case 'M':
          sscanf (optarg, "%d", &ret);
          if (ret < 0)
            {
              fnotice (stderr, "LIPO max-memory size needs to be positive\n");
              exit (-1);
            }
          __gcov_lipo_max_mem = ret;
          break;
        case 'F':
          sscanf (optarg, "%d", &ret);
          if (ret < 0)
            {
              fnotice (stderr,
                       "LIPO COMDAT fixup algorithm needs to be positive\n");
              exit (-1);
            }
          __gcov_lipo_comdat_algorithm = ret;
          break;
        case 'C':
          sscanf (optarg, "%d", &ret);
          if (ret < 0 || ret > 100)
            {
              fnotice (stderr, "LIPO cutoff value range is [0, 100]\n");
              exit (-1);
            }
          __gcov_lipo_cutoff = ret;;
          break;
        default:
          print_usage (true);
          /* Print_usage will exit.  */
        }
    }

  return optind;
}

/* Get the default param value from params.def.  */

#define GET_DEFAULT_PARAM_VALUE(p) compiler_params[p].default_value
static void
set_lipo_default_params (void)
{
  __gcov_lipo_grouping_algorithm = GET_DEFAULT_PARAM_VALUE (PARAM_LIPO_GROUPING_ALGORITHM);
  __gcov_lipo_merge_modu_edges   = GET_DEFAULT_PARAM_VALUE (PARAM_LIPO_MERGE_MODU_EDGES);
  __gcov_lipo_weak_inclusion     = GET_DEFAULT_PARAM_VALUE (PARAM_LIPO_WEAK_INCLUSION);
  __gcov_lipo_max_mem            = GET_DEFAULT_PARAM_VALUE (PARAM_MAX_LIPO_MEMORY);
  __gcov_lipo_comdat_algorithm   = GET_DEFAULT_PARAM_VALUE (PARAM_LIPO_COMDAT_ALGORITHM);
  __gcov_lipo_random_group_size  = GET_DEFAULT_PARAM_VALUE (PARAM_LIPO_RANDOM_GROUP_SIZE);
  __gcov_lipo_cutoff             = GET_DEFAULT_PARAM_VALUE (PARAM_LIPO_CUTOFF);
  __gcov_lipo_random_seed        = GET_DEFAULT_PARAM_VALUE (PARAM_LIPO_RANDOM_SEED);
  __gcov_lipo_dump_cgraph        = GET_DEFAULT_PARAM_VALUE (PARAM_LIPO_DUMP_CGRAPH);
  __gcov_lipo_propagate_scale    = GET_DEFAULT_PARAM_VALUE (PARAM_LIPO_PROPAGATE_SCALE);
}

/* Main function for gcov-tool.  */

int
main (int argc, char **argv)
{
  const char *p;
  const char *sub_command;

  p = argv[0] + strlen (argv[0]);
  while (p != argv[0] && !IS_DIR_SEPARATOR (p[-1]))
    --p;
  progname = p;

  xmalloc_set_program_name (progname);

  /* Unlock the stdio streams.  */
  unlock_std_streams ();

  gcc_init_libintl ();

  diagnostic_initialize (global_dc, 0);

  /* Handle response files.  */
  expandargv (&argc, &argv);

  /* Register the language-independent parameters.  */
  global_init_params ();
  finish_params ();
  set_lipo_default_params ();

  process_args (argc, argv);
  if (optind >= argc)
    print_usage (true);

  sub_command = argv[optind];

  if (!strcmp (sub_command, "merge"))
    return do_merge (argc - optind, argv + optind);
  else if (!strcmp (sub_command, "rewrite"))
    return do_rewrite (argc - optind, argv + optind);

  print_usage (true);
}
