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

/* The function decl used to create calls to __VLTVtableVerify.  It must
   be global because it needs to be initialized in the C++ front end, but
   used in the middle end (in the vtable verification pass).  */

extern tree verify_vtbl_ptr_fndecl;

/* Global variable keeping track of how many vtable map variables we
   have created. */
extern unsigned num_vtable_map_nodes;

/* Global variable that records whether or not any vtable verification
   calls have been generated.  */
extern bool any_verification_calls_generated;

/* Keep track of how many virtual calls we are actually verifying.  */
extern int total_num_virtual_calls;
extern int total_num_verified_vcalls;

/* Each vtable map variable corresponds to a virtual class.  Each
   vtable map variable has a hash table associated with it, that keeps
   track of the vtable pointers for which we have generated a call to
   __VLTRegisterPair (with the current vtable map variable).  This is
   the hash table node that is used for each entry in this hash table
   of vtable pointers.

   Sometimes there are multiple valid vtable pointer entries that use
   the same vtable pointer decl with different offsets.  Therefore,
   for each vtable pointer in the hash table, there is also an array
   of offsets used with that vtable. */

struct vtable_registration
{
  tree vtable_decl;            /* The var decl of the vtable.                */
  unsigned max_offsets;        /* The allocated size of the offsets array.   */
  unsigned cur_offset;         /* The next availabe entry in the offsets
				  array.                                     */
  unsigned *offsets;           /* The offsets array.                         */
};

/*  This struct is used to represent the class hierarchy information
    that we need.  Each vtable map variable has an associated class
    hierarchy node (struct vtv_graph_node).  Note: In this struct,
    'children' means immediate descendants in the class hierarchy;
    'descendant' means any descendant however many levels deep. */

struct vtv_graph_node {
  tree class_type;                  /* The record_type of the class.         */
  unsigned class_uid;               /* A unique, monotonically
                                       ascending id for class node.
                                       Each vtable map node also has
                                       an id.  The class uid is the
                                       same as the vtable map node id
                                       for nodes corresponding to the
                                       same class.  */
  unsigned max_parents;             /* Allocated size of the parents array.  */
  unsigned max_children;            /* Allocated size of the children array. */
  unsigned num_parents;             /* # of entries in the parents array.    */
  unsigned num_children;            /* # of entries in the children array.   */
  unsigned num_processed_children;  /* # of children for whom we have
                                       computed the class hierarchy
                                       transitive closure.                   */
  struct vtv_graph_node **parents;  /* Array of parents in the graph.        */
  struct vtv_graph_node **children; /* Array of children in the graph.       */
  sbitmap descendants;              /* Bitmap representing all this node's
				       descendants in the graph.             */
};

/* This is the node used for our hashtable of vtable map variable
   information.  When we create a vtable map variable (var decl) we
   put it into one of these nodes; create a corresponding
   vtv_graph_node for our class hierarchy info and store that in this
   node; generate a unique (monotonically ascending) id for both the
   vtbl_map_node and the vtv_graph_node; and insert the node into
   THREE data structures (to make it easy to find in several different
   ways): 1). A hash table ("vtbl_map_hash" in tree-vtable-verify.c).
   This gives us an easy way to check to see if we already have a node
   for the vtable map variable or not.  2).  A linked list of all
   vtbl_map_nodes ("vtbl_map_nodes") for easy iteration through all of
   them; and 3). An array of vtbl_map_nodes, where the array index
   corresponds to the unique id of the vtbl_map_node, which gives us
   an easy way to use bitmaps to represent and find the vtable map
   nodes.  */

struct vtbl_map_node {
  tree vtbl_map_decl;                 /* The var decl for the vtable map
					 variable.                           */
  tree class_name;                    /* The DECL_ASSEMBLER_NAME of the
					 class.                              */
  struct vtv_graph_node *class_info;  /* Our class hierarchy info for the
					 class.                              */
  unsigned uid;                       /* The unique id for the vtable map
					 variable.                           */
  struct vtbl_map_node *next, *prev;  /* Pointers for the linked list
					 structure.                          */
  htab_t registered;     /* Hashtable of vtable pointers for which we have
			    generated a _VLTRegisterPair call with this vtable
			    map variable.                                    */
  bool is_used;          /* Boolean indicating if we used this vtable map
			    variable in a call to __VLTVerifyVtablePointer.  */
};

/* The global linked list of vtbl_map_nodes.  */
extern struct vtbl_map_node *vtbl_map_nodes;

/* The global array of vtbl_map_nodes.  */
extern struct vtbl_map_node **vtbl_map_nodes_array;

extern struct vtbl_map_node *vtbl_map_get_node (tree);
extern struct vtbl_map_node *find_or_create_vtbl_map_node (tree);
extern void vtbl_map_node_class_insert (struct vtbl_map_node *, unsigned);
extern bool vtbl_map_node_registration_find (struct vtbl_map_node *,
                                             tree, unsigned);
extern bool vtbl_map_node_registration_insert (struct vtbl_map_node *,
                                               tree, unsigned);

#endif /* TREE_VTABLE_VERIFY_H */
