/* Interprocedural constant propagation
   Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010, 2011
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* Virtual Table Pointer Security.  */

#ifndef TREE_VTABLE_VERIFY_H
#define TREE_VTABLE_VERIFY_H

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "timevar.h"
#include "cpplib.h"
#include "tree.h"
#include "hashtab.h"
#include "sbitmap.h"

extern unsigned num_vtable_map_nodes;
extern bool any_verification_calls_generated;

struct vtable_registration
{
  tree vtable_decl;
  unsigned max_offsets;
  unsigned cur_offset;
  unsigned *offsets;
};

struct vtv_graph_node {
  tree class_type;
  unsigned class_uid;
  unsigned max_parents;
  unsigned max_children;
  unsigned num_parents;
  unsigned num_children;
  unsigned num_processed_children;
  struct vtv_graph_node **parents;
  struct vtv_graph_node **children;
  sbitmap descendants;
};

struct vtbl_map_node {
  tree vtbl_map_decl;
  tree class_name;
  struct vtv_graph_node *class_info;
  unsigned uid;
  struct vtbl_map_node *next, *prev;
  htab_t registered;
  bool is_used;
};

extern struct vtbl_map_node *vtbl_map_nodes;
extern struct vtbl_map_node **vtbl_map_nodes_array;

extern struct vtbl_map_node *vtbl_map_get_node (const_tree);
extern struct vtbl_map_node *vtbl_map_node (tree);
extern void vtbl_map_node_class_insert (struct vtbl_map_node *, unsigned);
extern bool vtbl_map_node_registration_find (struct vtbl_map_node *,
                                             tree, unsigned);
extern void vtbl_map_node_registration_insert (struct vtbl_map_node *,
                                               tree, unsigned);

#endif /* TREE_VTABLE_VERIFY_H */
