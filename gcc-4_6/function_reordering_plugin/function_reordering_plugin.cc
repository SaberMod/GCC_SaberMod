/* Function re-ordering plugin for gold.
   Copyright (C) 2011 Free Software Foundation, Inc.
   Contributed by Sriraman Tallam (tmsriram@google.com).

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* This plugin should be invoked only when callgraph edge profile
   information is available in the object files generated using the
   compiler flag -fcallgraph-profiles-sections.

   This plugin reads the callgraph sections and constructs an annotated
   callgraph.  It then repeatedly groups sections that are connected by
   hot edges and passes the new function layout to the linker.

   This plugin dumps the final layout order of the functions in a file
   called "final_layout.txt".  To change the output file, pass the new
   file name with --plugin-opt.  */

#if HAVE_STDINT_H
#include <stdint.h>
#endif
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <elf.h>
#include "config.h"
#include "plugin-api.h"
#include "callgraph.h"

extern "C"
enum ld_plugin_status claim_file_hook (const struct ld_plugin_input_file *file,
                                       int *claimed);
extern "C"
enum ld_plugin_status all_symbols_read_hook ();
static ld_plugin_get_input_section_count get_input_section_count = NULL;
static ld_plugin_get_input_section_type get_input_section_type = NULL;
static ld_plugin_get_input_section_name get_input_section_name = NULL;
static ld_plugin_get_input_section_contents get_input_section_contents = NULL;
static ld_plugin_update_section_order update_section_order = NULL;
static ld_plugin_allow_section_ordering allow_section_ordering = NULL;


/* The file where the final function order will be stored.
   Default is "./final_layout.txt"
   To specify a different file, use --plugin-opt,<filename>  */

std::string out_file ("./final_layout.txt");

bool is_api_exist = false;

/* Plugin entry point.  */
extern "C"
enum ld_plugin_status
onload (struct ld_plugin_tv *tv)
{
  struct ld_plugin_tv *entry;
  for (entry = tv; entry->tv_tag != LDPT_NULL; ++entry)
    {
      switch (entry->tv_tag)
        {
        case LDPT_API_VERSION:
          break;
        case LDPT_GOLD_VERSION:
          break;
        case LDPT_OPTION:
	  out_file.assign (entry->tv_u.tv_string);
          break;
        case LDPT_REGISTER_CLAIM_FILE_HOOK:
          assert ((*entry->tv_u.tv_register_claim_file) (claim_file_hook) == LDPS_OK);
          break;
	case LDPT_REGISTER_ALL_SYMBOLS_READ_HOOK:
          assert ((*entry->tv_u.tv_register_all_symbols_read) (all_symbols_read_hook) == LDPS_OK);
          break;
        case LDPT_GET_INPUT_SECTION_COUNT:
          get_input_section_count = *entry->tv_u.tv_get_input_section_count;
          break;
        case LDPT_GET_INPUT_SECTION_TYPE:
          get_input_section_type = *entry->tv_u.tv_get_input_section_type;
          break;
        case LDPT_GET_INPUT_SECTION_NAME:
          get_input_section_name = *entry->tv_u.tv_get_input_section_name;
          break;
        case LDPT_GET_INPUT_SECTION_CONTENTS:
          get_input_section_contents = *entry->tv_u.tv_get_input_section_contents;
          break;
	case LDPT_UPDATE_SECTION_ORDER:
	  update_section_order = *entry->tv_u.tv_update_section_order;
	  break;
	case LDPT_ALLOW_SECTION_ORDERING:
	  allow_section_ordering = *entry->tv_u.tv_allow_section_ordering;
	  break;
        default:
          break;
        }
    }

  if (get_input_section_count != NULL
      && get_input_section_type != NULL
      && get_input_section_name != NULL
      && get_input_section_contents != NULL
      && update_section_order != NULL
      && allow_section_ordering != NULL)
    is_api_exist = true;

  return LDPS_OK;
}

static bool is_ordering_specified = false;

inline static bool is_prefix_of (const char *prefix, const char *str)
{
  return strncmp (prefix, str, strlen (prefix)) == 0;
}


/* This function is called by the linker for every new object it encounters.  */

extern "C"
enum ld_plugin_status
claim_file_hook (const struct ld_plugin_input_file *file, int *claimed)
{
  unsigned int count = 0;
  ld_plugin_section section;

  /* Silently return if the plugin APIs are not supported.  */
  if (!is_api_exist)
    return LDPS_OK;

  if (!is_ordering_specified)
    {
      /* Inform the linker to prepare for section reordering.  */
      (*allow_section_ordering) ();
      is_ordering_specified = true;
    }

  (*get_input_section_count) (file->handle, &count);

  for (unsigned int shndx = 0; shndx < count; ++shndx)
    {
      unsigned int type = SHT_NULL;
      section.handle = file->handle;
      section.shndx = shndx;
      (*get_input_section_type) (section, &type);
      if (type != SHT_PROGBITS)
        continue;

      char *name = NULL;
      (*get_input_section_name) (section, &name);
      if (type == SHT_PROGBITS && is_prefix_of (".text.", name))
        {
	  /* Length of '.text.' is 6.  */
          map_section_name_to_index (name + 6, file->handle, shndx);
        }
      else if (is_prefix_of (".note.callgraph.text", name))
        {
	  /* Process callgraph sections.  */
          unsigned char *section_contents_ptr = NULL;
          size_t length;
          (*get_input_section_contents) (section,
		(const unsigned char **)&section_contents_ptr,
		&length);
	  unsigned char *section_contents;
	  section_contents = (unsigned char *) malloc (length);
	  memcpy (section_contents, section_contents_ptr, length);
          parse_callgraph_section_contents (section_contents, (unsigned int)length);
        }
      else if (name != NULL)
        free (name);
    }

  return LDPS_OK;
}

/* This function is called by the linker after all the symbols have been read.
   At this stage, it is fine to tell the linker the desired function order.  */

extern "C"
enum ld_plugin_status
all_symbols_read_hook (void)
{
  void **handles;
  unsigned int *shndx;
  ld_plugin_section *section_list;

  /* Silently return if the plugin APIs are not supported.  */
  if (!is_api_exist)
    return LDPS_OK;

  find_pettis_hansen_function_layout ();
  fprintf (stderr, "Writing final layout to file : %s\n", out_file.c_str ());
  unsigned int num_entries = get_layout (out_file.c_str (), &handles, &shndx);
  section_list = new ld_plugin_section[num_entries];

  for (int i = 0; i < num_entries; i++)
    {
      section_list[i].handle = handles[i];
      section_list[i].shndx = shndx[i];
    }

  /* Pass the new order of functions to the linker.  */
  update_section_order (section_list, num_entries);
  delete [] section_list;
  return LDPS_OK;
}
