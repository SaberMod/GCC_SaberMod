/* Callgraph implementation.
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

#include "callgraph.h"
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <hashtab.h>

/*****************************************************************************/
/* section_map hashtable definition and helpers. */

/* Maps section name to its corresponding object handle and section index.  */
static htab_t section_map = NULL;

/* Hashtable helper for section_map htab.  */
static hashval_t
section_map_htab_hash_descriptor (const void *p)
{
  const Section_id *s = (const Section_id *)p;
  const char *name = s->name;
  return htab_hash_string(name);
}

/* Hashtable helper for section_map htab.  */
static int
section_map_htab_eq_descriptor (const void *p1, const void *p2)
{
  const Section_id *s1 = (const Section_id *)p1;
  const char *c1 = s1->name;
  const char *c2 = (const char *)p2;

  return (strcmp (c1, c2) == 0);
}
/*****************************************************************************/


/*****************************************************************************/
/* function_map hashtable definition and helpers.
   Maps function name to a unique Node.  */
static htab_t function_map = NULL;
static unsigned int last_function_id = 0;

/* Hashtable helper for function_map htab.  */
static hashval_t
function_map_htab_hash_descriptor (const void *p)
{
  const Node *s = (const Node *)p;
  const char *name = s->name;
  return htab_hash_string(name);
}

/* Hashtable helper for section_map htab.  */
static int
function_map_htab_eq_descriptor (const void *p1, const void *p2)
{
  const Node *s1 = (const Node *)p1;
  const char *c1 = s1->name;
  const char *c2 = (const char *)p2;

  return (strcmp (c1, c2) == 0);
}
/*****************************************************************************/

/*****************************************************************************/
/* edge_map hashtable definition and helpers.
   Maps two node ids to a unique edge.  */
static htab_t edge_map = NULL;

static inline hashval_t
edge_hash_function (unsigned int id1, unsigned int id2)
{
  return (id1 << 16) | id2;
}

/* Hashtable helper for edge_map htab.  */
static hashval_t
edge_map_htab_hash_descriptor (const void *p)
{
  Edge *e = (Edge *) p;
  return edge_hash_function (e->first_function->id, e->second_function->id);
}

/* Hashtable helper for edge_map htab.  */
static int
edge_map_htab_eq_descriptor (const void *p1, const void *p2)
{
  Edge *e1 = (Edge *) p1;
  Raw_edge *r1 = (Raw_edge *) p2;
  return ((e1->first_function->id == r1->n1->id)
	  && (e1->second_function->id == r1->n2->id));
}


/*****************************************************************************/

/* Chain of all the created nodes.  */
Node *node_chain = NULL;
/* Number of nodes that correspond to functions which will be reordered.  */
unsigned int num_real_nodes = 0;
/* Chain of all edges in the merged callgraph.  */
Edge *active_edges = NULL;
/* Chain of all the merged edges.  */
Edge *inactive_edges = NULL;

/* Initial value of number of functions to allocate hash tables.  */
const int NUM_FUNCTIONS = 100;

/* Reads off the next string from the char stream CONTENTS and updates
   READ_LENGTH to the length of the string read.  The value of CONTENTS
   is updated to start at the next string.  */

static char *
get_next_string (char **contents, unsigned int *read_length)
{
  char *s = *contents;
  *read_length = strlen (*contents) + 1;
  *contents += *read_length;
  return s;
}

/* Add an EDGE to the list of edges in the call graph.  */

static void
add_edge_to_list (Edge *edge)
{
  assert (edge != NULL);
  edge->next = active_edges;
  if (active_edges != NULL)
    active_edges->prev = edge;
  active_edges = edge;
}

/* Remove the edge from the list of edges in the call graph. This is done
   when the nodes corresponding to this edge are merged.  */

static void
remove_edge_from_list (Edge * curr_edge)
{
  assert (curr_edge != NULL);
  if (curr_edge->prev != NULL)
    curr_edge->prev->next = curr_edge->next;
  if (curr_edge->next != NULL)
    curr_edge->next->prev = curr_edge->prev;
  if (active_edges == curr_edge)
    active_edges = curr_edge->next;
  curr_edge->next = NULL;
  curr_edge->prev = NULL;

  /* Add to inactive edges to be freed later.  */
  curr_edge->next = inactive_edges;
  inactive_edges = curr_edge;
  return;
}

/* Adds the WEIGHT value to the edge count of CALLER and CALLEE.  */

static void
update_edge (Node *n1, Node *n2, unsigned int weight)
{
  void **slot;
  Raw_edge re, *r;
  Edge *e;

  if (n1->id == n2->id)
    return;
  if (weight == 0)
    return;

  if (edge_map == NULL)
    {
      edge_map = htab_create ((NUM_FUNCTIONS * 2),
			      edge_map_htab_hash_descriptor,
			      edge_map_htab_eq_descriptor , NULL);
      assert (edge_map != NULL);
    }

  r = &re;
  init_raw_edge (r, n1, n2);
  slot = htab_find_slot_with_hash (edge_map, r,
				   edge_hash_function (r->n1->id, r->n2->id),
				   INSERT);
  if (*slot == NULL)
    {
      e = make_edge (r->n1, r->n2, weight);
      *slot = e;
      add_edge_to_list (e);
    }
  else
    {
      e = *slot;
      e->weight += weight;
    }
}

/* Create a unique node for a function.  */

static Node *
get_function_node (char *name)
{
  void **slot = NULL;
  Node *node;

  if (function_map == NULL)
    {
      function_map = htab_create (NUM_FUNCTIONS,
				  function_map_htab_hash_descriptor,
				  function_map_htab_eq_descriptor , NULL);
      assert (function_map != NULL);
    }

  slot = htab_find_slot_with_hash (function_map, name, htab_hash_string (name),
				   INSERT);

  if (*slot == NULL)
    {
      node = make_node (last_function_id, name);
      /* Chain the node to the node_chain.  */
      node->next = node_chain;
      node_chain = node;
      *slot = node;
      last_function_id++;
    }
  else
    {
      node = (Node *)*slot;
    }
  return node;
}

/* Dumper funcction to print the list of functions that will be considered for
   re-ordering.  */

void
dump_functions ()
{
  Node *node = node_chain;
  while (node)
  {
    if (node->is_real_node)
      fprintf (stderr, "Dumping function %s\n", node->name);
    node = node->next;
  }
}

/* Dump all the edges existing in the callgraph.  */

void dump_edges (FILE *fp)
{
  Edge *it;
  for (it = active_edges;
       it != NULL;
       it = it->next)
    {
      fprintf (fp,"# %s ---- (%u)---- %s\n",
               it->first_function->name,
	       it->weight,
               it->second_function->name);
    }
}

/* HEADER_LEN is the length of string 'Function '.  */
const int HEADER_LEN = 9;

/* Parse the section contents of ".gnu.callgraph.text"  sections and create
   call graph edges with appropriate weights. The section contents have the
   following format :
   Function  <caller_name>
   <callee_1>
   <edge count between caller and callee_1>
   <callee_2>
   <edge count between caller and callee_2>
   ....  */
void
parse_callgraph_section_contents (unsigned char *section_contents,
				  unsigned int length)
{
  char *contents;
  char *caller;
  unsigned int read_length = 0, curr_length = 0;
  Node *caller_node;

   /* First string in contents is 'Function <function-name>'.  */
  assert (length > 0);
  contents = (char*) (section_contents);
  caller = get_next_string (&contents, &read_length);
  assert (read_length > HEADER_LEN);
  caller = caller + HEADER_LEN;
  curr_length = read_length;
  caller_node = get_function_node (caller);
  num_real_nodes++;

  while (curr_length < length)
    {
      /* Read callee, weight tuples.  */
      char *callee;
      char *weight_str;
      unsigned int weight;
      Node *callee_node;

      callee = get_next_string (&contents, &read_length);
      curr_length += read_length;
      callee_node = get_function_node (callee);

      assert (curr_length < length);
      weight_str = get_next_string (&contents, &read_length);
      weight = atoi (weight_str);
      curr_length += read_length;
      update_edge (caller_node, callee_node, weight);
    }
}

/* Traverse the list of edges and find the edge with the maximum weight.  */

static Edge *
find_max_edge ()
{
  Edge *it, *max_edge;

  if (active_edges == NULL)
    return NULL;

  max_edge = active_edges;
  assert (!active_edges->is_merged);

  it = active_edges->next;
  for (;it != NULL; it = it->next)
    {
      assert (!it->is_merged);
      if (edge_lower (max_edge , it))
          max_edge = it;
    }

  return max_edge;
}

/* Change the EDGE from OLD_NODE to KEPT_NODE to be between NEW_NODE
   and KEPT_NODE.  */

static void
merge_edge (Edge *edge, Node *new_node, Node *old_node,
            Node *kept_node)
{
  void **slot;
  Raw_edge re, *r;

  r = &re;
  init_raw_edge (r, new_node, kept_node);
  slot = htab_find_slot_with_hash (edge_map, r,
				   edge_hash_function (r->n1->id, r->n2->id),
				   INSERT);

  if (*slot == NULL)
    {
      reset_functions (edge, new_node, kept_node);
      *slot = edge;
      add_edge_to_node (new_node, edge);
    }
  else
    {
      Edge *new_edge = *slot;
      new_edge->weight += edge->weight;
      edge->is_merged = 1;
      remove_edge_from_list (edge);
    }
}

/* Merge the two nodes in this EDGE. The new node's edges are the union of
   the edges of the original nodes.  */

static void
collapse_edge (Edge * edge)
{
  Edge_list *it;
  Node *kept_node = edge->first_function;
  Node *merged_node = edge->second_function;

  /* Go through all merged_node edges and merge with kept_node.  */
  for (it = merged_node->edge_list; it != NULL; it = it->next)
    {
      Node *other_node = NULL;
      Edge *this_edge = it->edge;
      if (this_edge->is_merged)
        continue;
      if (this_edge == edge)
        continue;
      assert (this_edge->first_function->id == merged_node->id
              || this_edge->second_function->id == merged_node->id);
      other_node = (this_edge->first_function->id
		    == merged_node->id)
		   ? this_edge->second_function
                   : this_edge->first_function;
      merge_edge (this_edge, kept_node, merged_node, other_node);
    }

  merge_node (kept_node, merged_node);
  edge->is_merged = 1;
  remove_edge_from_list (edge);
}

/* Make node N a real node if it can be reordered, that is, its .text
   section is available.  */
static void set_node_type (Node *n)
{
  void *slot;
  char *name = n->name;
  slot = htab_find_with_hash (section_map, name, htab_hash_string (name));
  if (slot != NULL)
    set_as_real_node (n);
}

void
find_pettis_hansen_function_layout (FILE *fp)
{
  Node *n_it;
  Edge *it;

  assert (node_chain != NULL);
  assert (active_edges != NULL);
  assert (fp != NULL);

  dump_edges (fp);

  /* Go over all the nodes and set it as real node only if a corresponding
     function section exists.  */
  for (n_it = node_chain; n_it != NULL; n_it = n_it->next)
    set_node_type (n_it);

  /* Set edge types. A WEAK_EDGE has one of its nodes corresponding to a
     function that cannot be re-ordered.  */
  for (it = active_edges; it != NULL; it = it->next)
    set_edge_type (it);

  it = find_max_edge ();
  while (it != NULL)
    {
      collapse_edge (it);
      it = find_max_edge ();
    }
}

/* Maps the function name corresponding to section SECTION_NAME to the
   object handle and the section index.  */

void
map_section_name_to_index (char *section_name, void *handle, int shndx)
{
  void **slot;
  const char *sections[] = {".text.hot.",
			    ".text.unlikely.",
			    ".text.cold.",
			    ".text.startup.",
			    ".text." };
  char *function_name = NULL;
  int i;

  for (i = 0; i < ARRAY_SIZE (sections); ++i)
    {
      if (is_prefix_of (sections[i], section_name))
        {
          function_name = section_name + strlen (sections[i]);
	  break;
        }
    }
  assert (function_name != NULL);

  /* Allocate section_map.  */
  if (section_map == NULL)
    {
      section_map = htab_create (NUM_FUNCTIONS,
				 section_map_htab_hash_descriptor,
				 section_map_htab_eq_descriptor , NULL);
      assert (section_map != NULL);
    }

  slot = htab_find_slot_with_hash (section_map, function_name,
				   htab_hash_string (function_name),
				   INSERT);
  if (*slot == NULL)
    *slot = make_section_id (function_name, section_name, handle, shndx);
}

static void
write_out_node (FILE *fp, char *name,
		void **handles, unsigned int *shndx, int position)
{
  void *slot;
  slot = htab_find_with_hash (section_map, name, htab_hash_string (name));
  if (slot != NULL)
    {
      Section_id *s = (Section_id *)slot;
      handles[position] = s->handle;
      shndx[position] = s->shndx;
      fprintf (fp, "%s\n", s->full_name);
      /* No more use of full_name  */
      free (s->full_name);
    }
}

/* Visit each node and print the chain of merged nodes to the file.  */

unsigned int
get_layout (FILE *fp, void*** handles,
            unsigned int** shndx)
{
  Node *it;
  int position = 0;

  assert (fp != NULL);

  *handles = XNEWVEC (void *, num_real_nodes);
  *shndx = XNEWVEC (unsigned int, num_real_nodes);

  /* Dump edges to the final reordering file.  */

  for (it = node_chain; it != NULL; it = it->next)
    {
      Node *node;
      if (it->is_merged)
        continue;
      if (it->is_real_node)
        {
	  write_out_node (fp, it->name, *handles, *shndx, position);
	  position++;
        }
      node = it->merge_next;
      while (node != NULL)
        {
          if (node->is_real_node)
	    {
	      write_out_node (fp, node->name, *handles, *shndx, position);
	      position++;
	    }
          node = node->merge_next;
	}
    }
  return position;
}

/* Free all heap objects.  */

void
cleanup ()
{
  Node *node;

  /* Free all nodes and edge_lists.  */
  for (node = node_chain; node != NULL; )
    {
      Node *next_node = node->next;
      Edge_list *it;
      for (it = node->edge_list; it != NULL; )
        {
          Edge_list *next_it = it->next;
          free (it);
          it = next_it;
        }
      free (node);
      node = next_node;
    }

  /* Free all active_edges.  */
  free_edge_chain (active_edges);

  /* Free all inactive edges.  */
  free_edge_chain (inactive_edges);

  /* Delete all htabs.  */
  htab_delete (section_map);
  htab_delete (function_map);
  htab_delete (edge_map);
}

/* Check if the callgraph is empty.  */
unsigned int
is_callgraph_empty ()
{
  if (active_edges == NULL)
    return 1;
  return 0;
}
