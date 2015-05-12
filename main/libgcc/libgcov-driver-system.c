/* Routines required for instrumenting a program.  */
/* Compile this one with gcc.  */
/* Copyright (C) 1989-2014 Free Software Foundation, Inc.

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

/* A utility function for outputing errors.  */

static int __attribute__((format(printf, 1, 2)))
gcov_error (const char *fmt, ...)
{
  int ret;
  va_list argp;
  va_start (argp, fmt);
  ret = vfprintf (stderr, fmt, argp);
  va_end (argp);
  return ret;
}

/* Make sure path component of the given FILENAME exists, create
   missing directories. FILENAME must be writable.
   Returns zero on success, or -1 if an error occurred.  */

static int
create_file_directory (char *filename)
{
#if !defined(TARGET_POSIX_IO) && !defined(_WIN32)
  (void) filename;
  return -1;
#else
  char *s;

  s = filename;

  if (HAS_DRIVE_SPEC(s))
    s += 2;
  if (IS_DIR_SEPARATOR(*s))
    ++s;
  for (; *s != '\0'; s++)
    if (IS_DIR_SEPARATOR(*s))
      {
        char sep = *s;
        *s  = '\0';

        /* Try to make directory if it doesn't already exist.  */
        if (access (filename, F_OK) == -1
#ifdef TARGET_POSIX_IO
            && mkdir (filename, 0755) == -1
#else
            && mkdir (filename) == -1
#endif
            /* The directory might have been made by another process.  */
            && errno != EEXIST)
          {
            gcov_error ("profiling:%s:Cannot create directory\n", filename);
            *s = sep;
            return -1;
          };

        *s = sep;
      };
  return 0;
#endif
}

static void
allocate_filename_struct (struct gcov_filename *gf)
{
  const char *gcov_prefix;
  size_t prefix_length;
  int strip = 0;

  {
    /* Check if the level of dirs to strip off specified. */
    char *tmp = getenv("GCOV_PREFIX_STRIP");
    if (tmp)
      {
        strip = atoi (tmp);
        /* Do not consider negative values. */
        if (strip < 0)
          strip = 0;
      }
  }
  gf->strip = strip;

  /* Get file name relocation prefix.  Non-absolute values are ignored. */
  gcov_prefix = getenv("GCOV_PREFIX");
  prefix_length = gcov_prefix ? strlen (gcov_prefix) : 0;
  
  /* Remove an unnecessary trailing '/' */
  if (prefix_length && IS_DIR_SEPARATOR (gcov_prefix[prefix_length - 1]))
    prefix_length--;

  /* If no prefix was specified and a prefix stip, then we assume
     relative.  */
  if (!prefix_length && gf->strip)
    {
      gcov_prefix = ".";
      prefix_length = 1;
    }
  gf->prefix = prefix_length;

  /* Allocate and initialize the filename scratch space.  */
  gf->filename = (char *) xmalloc (gf->max_length + prefix_length + 2);
  if (prefix_length)
    memcpy (gf->filename, gcov_prefix, prefix_length);
}

static int
gcov_open_by_filename (char *gi_filename)
{
  if (!gcov_open (gi_filename))
    {
      /* Open failed likely due to missed directory.
         Create directory and retry to open file. */
      if (create_file_directory (gi_filename))
        {
          fprintf (stderr, "profiling:%s:Skip\n", gi_filename);
          return -1;
        }
      if (!gcov_open (gi_filename))
        {
          fprintf (stderr, "profiling:%s:Cannot open\n", gi_filename);
          return -1;
        }
    }
  return 0;
}

#define GCOV_GET_FILENAME gcov_strip_leading_dirs

/* Strip GCOV_PREFIX_STRIP levels of leading '/' from FILENAME and
   put the result into GI_FILENAME_UP.  */

static void
gcov_strip_leading_dirs (int prefix_length, int gcov_prefix_strip,
      			 const char *filename, char *gi_filename_up)
{
  /* Avoid to add multiple drive letters into combined path.  */
  if (prefix_length != 0 && HAS_DRIVE_SPEC(filename))
    filename += 2;

  /* Build relocated filename, stripping off leading
     directories from the initial filename if requested. */
  if (gcov_prefix_strip > 0)
    {
      const char *probe = filename;
      int level;

      /* Remove a leading separator, without counting it.  */
      if (IS_DIR_SEPARATOR (*probe))
	probe++;

      /* Skip selected directory levels.  If we fall off the end, we
	 keep the final part.  */
      for (level = gcov_prefix_strip; *probe && level; probe++)
        if (IS_DIR_SEPARATOR (*probe))
          {
            filename = probe;
            level--;
          }
    }

  /* Update complete filename with stripped original. */
  if (prefix_length != 0 && !IS_DIR_SEPARATOR (*filename))
    {
      /* If prefix is given, add directory separator.  */
      strcpy (gi_filename_up, "/");
      strcpy (gi_filename_up + 1, filename);
    }
  else
    strcpy (gi_filename_up, filename);
}

/* Open a gcda file specified by GI_FILENAME.
   Return -1 on error.  Return 0 on success.  */

static int
gcov_exit_open_gcda_file (struct gcov_info *gi_ptr, struct gcov_filename *gf)
{
  int gcov_prefix_strip;
  size_t prefix_length;
  char *gi_filename_up;

  gcov_prefix_strip = gf->strip;
  gi_filename_up = gf->filename + gf->prefix;
  prefix_length = gf->prefix;

  GCOV_GET_FILENAME (prefix_length, gcov_prefix_strip, gi_ptr->filename,
                     gi_filename_up);

  return gcov_open_by_filename (gf->filename);
}
