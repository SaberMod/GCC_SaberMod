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
#include <iostream>
#include <fstream>

/* List of functions or nodes in the call graph. A new node is created for
   each function at the beginning.  */
Function_list function_list;
/* Maps function names to their id.  */
Function_map function_map;

/* Maps the node id tuple (Raw_edge) to the Edge object.  */
Edge_map edge_map;
/* List of edges in the call graph.  */
Edge_ptr active_edges = NULL;

/* Maps section name to its corresponding object handle and section index.  */
Section_map section_map;

/* Return a std::pair, tuple of the node id's.  */

static Raw_edge
get_raw_edge (unsigned int func_id_1, unsigned int func_id_2)
{
  assert (func_id_1 != func_id_2);
  if (func_id_1 < func_id_2)
    return Raw_edge (func_id_1, func_id_2);
  else
    return Raw_edge (func_id_2, func_id_1);
}

/* Add an EDGE to the list of edges in the call graph.  */

static void
add_edge_to_list (Edge_ptr edge)
{
  assert (edge != NULL);
  edge->set_next (active_edges);
  if (active_edges != NULL)
    active_edges->set_prev (edge);
  active_edges = edge;
}

/* Remove the edge from the list of edges in the call graph. This is done
   when the nodes corresponding to this edge are merged.  */

static void
remove_edge_from_list (Edge_ptr curr_edge)
{
  assert (curr_edge != NULL);
  if (curr_edge->get_prev () != NULL)
    curr_edge->get_prev ()->set_next (curr_edge->get_next ());
  if (curr_edge->get_next () != NULL)
    curr_edge->get_next ()->set_prev (curr_edge->get_prev ());
  if (active_edges == curr_edge)
    active_edges = curr_edge->get_next ();
  curr_edge->set_next (NULL);
  curr_edge->set_prev (NULL);
  return;
}

/* Adds the WEIGHT value to the edge count of CALLER and CALLEE.  */

static void
update_edge (unsigned int caller, unsigned int callee, unsigned int weight)
{
  if (caller == callee)
    return;
  if (weight == 0)
    return;
  Raw_edge r = get_raw_edge (caller, callee);
  Edge_map::iterator it = edge_map.find (r);
  if (it == edge_map.end ())
    {
      Edge_ptr edge = new Edge (function_list[caller], function_list[callee],
                               weight);
      add_edge_to_list (edge);
      edge_map[r] = edge;
      return;
    }
  Edge_ptr edge = it->second;
  edge->add_weight (weight);
  return;
}

/* Adds a function to the function_list if it is a new function. Returns the
   unique identifier corresponding to the function, which is its index in
   the function_list vector.  */

static unsigned int
get_function_index (std::string fnname)
{
  Function_map::iterator it = function_map.find (fnname);
  if (it == function_map.end ())
    {
      unsigned int position = function_list.size ();
      function_map[fnname] = position;
      Node *node = new Node (position, fnname);
      function_list.push_back (node);
      return position;
    }
  return it->second;
}

/* Dumper funcction to print the list of functions that will be considered for
   re-ordering.  */

void
dump_functions ()
{
  for (Function_list::iterator it = function_list.begin ();
       it != function_list.end ();
       ++it)
    {
      if ( (*it)->is_real_node ())
        fprintf (stderr,"Dumping %s\n", ( (*it)->name ()).c_str ());
    }
}

/* Dumper function to print the list of edges in the call graph.  */

void
dump_edges ()
{
  fprintf (stderr,"active_edges = %p\n", active_edges);
  for (Edge_ptr it = active_edges;
       it != NULL;
       it = it->get_next ())
    {
      fprintf (stderr," (%p<--Edge %p : [%s ---- (%u)---- %s]-->%p) ",
              it->get_prev (), it,
              it->first_function ()->name ().c_str (),
	      it->weight (),
              it->second_function ()->name ().c_str (), it->get_next ());
    }
  fprintf (stderr, "\n");
}

/* Reads off the next string from the char stream CONTENTS and updates
   READ_LENGTH to the length of the string read.  The value of CONTENTS
   is updated to start at the next string.  */

static std::string
get_next_string (char **contents, unsigned int *read_length)
{
  std::string s (*contents);
  *read_length = strlen (*contents) + 1;
  *contents += *read_length;
  return s;
}

/* HEADER_LEN is the length of string 'Function '.  */
const int HEADER_LEN = 9;

/* Parse the section contents of ".note.callgraph.text"  sections and create
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
   /* First string in contents is 'Function <function-name>'.  */
  assert (length > 0);
  char *contents = (char*) (section_contents);
  unsigned int read_length = 0, curr_length = 0;
  std::string caller = get_next_string (&contents, &read_length);
  caller = caller.substr (HEADER_LEN);
  curr_length = read_length;
  unsigned int caller_id = get_function_index (caller);
  /* Real nodes are nodes that correspond to .text sections found.  These will
     definitely be sorted.  */
  function_list[caller_id]->set_as_real_node ();

  while (curr_length < length)
    {
      /* Read callee, weight tuples.  */
      std::string callee = get_next_string (&contents, &read_length);
      curr_length += read_length;
      unsigned int callee_id = get_function_index (callee);

      assert (curr_length < length);
      std::string weight_str = get_next_string (&contents, &read_length);
      unsigned int weight = atoi (weight_str.c_str ());
      curr_length += read_length;
      update_edge (caller_id, callee_id, weight);
    }
}

/* Traverse the list of edges and find the edge with the maximum weight.  */

static Edge_ptr
find_max_edge ()
{
  if (active_edges == NULL)
    return NULL;
  Edge_ptr it, max_edge;
  max_edge = active_edges;
  assert (!active_edges->is_merged ());

  it = active_edges->get_next ();
  for (;it != NULL; it = it->get_next ())
    {
      assert (!it->is_merged ());
      if (*max_edge < *it)
          max_edge = it;
    }
  return max_edge;
}

/* Change the EDGE from OLD_NODE to KEPT_NODE to be between NEW_NODE
   and KEPT_NODE.  */

static void
merge_edge (Edge_ptr edge, Node *new_node, Node *old_node,
           Node *kept_node)
{
  Raw_edge r = get_raw_edge (new_node->id (), kept_node->id ());
  Edge_map::iterator it = edge_map.find (r);
  if (it == edge_map.end ())
    {
      /* Reuse the edge rather than creating a new one.  */
      edge->set_functions (new_node, kept_node);
      edge_map[r] = edge;
      new_node->add_edge (edge);
      return;
    }
  else
    {
      Edge_ptr new_edge = it->second;
      new_edge->add_weight (edge->weight ());
      edge->set_merged ();
      remove_edge_from_list (edge);
    }

  /* Erase the old edge from the map.  */
  Raw_edge old_r = get_raw_edge (old_node->id (), kept_node->id ());
  edge_map.erase (old_r);
  return;
}

/* Merge the two nodes in this EDGE. The new node's edges are the union of
   the edges of the original nodes.  */

static void
collapse_edge (Edge_ptr edge)
{
  Node *kept_node = edge->first_function ();
  Node *merged_node = edge->second_function ();

  /* Go through all merged_node edges and merge with kept_node.  */
  for (Edge_list::iterator it = merged_node->edge_list ().begin ();
       it != merged_node->edge_list ().end (); ++it)
    {
      Edge *this_edge = *it;
      if (this_edge->is_merged ())
        continue;
      if (this_edge == edge)
        continue;
      assert (this_edge->first_function ()->id () == merged_node->id ()
              || this_edge->second_function ()->id () == merged_node->id ());
      Node *other_node = (this_edge->first_function ()->id ()
			  == merged_node->id ())
			 ? this_edge->second_function ()
                         : this_edge->first_function ();
      merge_edge (this_edge, kept_node, merged_node, other_node);
    }

  kept_node->merge_node (merged_node);
  edge->set_merged ();
  remove_edge_from_list (edge);
}

static void
write_out_node (std::ofstream& out, const std::string& name,
                    void **handles, unsigned int *shndx, int position)
{
  Section_map::iterator it = section_map.find (name);
  if (it != section_map.end ())
  {
    handles[position] = (it->second).first;
    shndx[position] = (it->second).second;
    out<<".text."<<name<<"\n";
  }
  else
  {
    std::cerr<<"Not found :"<<name<<"\n";
  }
}

/* Visit each node and print the chain of merged nodes to the file.  */

unsigned int
get_layout (const char *out_filename, void*** handles,
            unsigned int** shndx)
{
  std::ofstream out;
  out.open (out_filename, std::ios_base::trunc);
  if (!out)
    {
      fprintf (stderr, "Unable to open %s\n", out_filename);
      exit (0);
    }
  *handles = new void*[function_list.size ()];
  *shndx = new unsigned int[function_list.size ()];
  int position = 0;

  for (Function_list::iterator it = function_list.begin ();
       it != function_list.end ();
       ++it)
    {
      if ((*it)->is_merged ())
        continue;
      if ((*it)->is_real_node ())
        {
	  write_out_node (out, (*it)->name (), *handles, *shndx, position);
          position++;
        }
      Node *node = (*it)->get_merge_next ();
      while (node != NULL)
        {
          if (node->is_real_node ())
	    {
	      write_out_node (out, node->name (), *handles, *shndx, position);
              position++;
	    }
          node = node->get_merge_next ();
        }
    }

  /* All the sorted functions should appear at the front.  This glob puts the
     non-listed functions to the end.  */
  out<<".text.*\n";
  out.close ();
  return position;
}

/* Driver function that iteratively finds the edge with the maximum weight and
   merges the nodes.  */

void
find_pettis_hansen_function_layout ()
{
  /* Set edge types. A WEAK_EDGE has one of its nodes corresponding to a
     function that cannot be re-ordered.  */
  for (Edge_ptr it = active_edges; it != NULL;
       it = it->get_next ())
      it->set_edge_type ();

  Edge_ptr edge = find_max_edge ();
  while (edge != NULL)
    {
      collapse_edge (edge);
      edge = find_max_edge ();
    }
}

void
map_section_name_to_index (char *name, void *handle, int shndx)
{
  std::string secn_name (name);
  Section_map::iterator it = section_map.find (secn_name);
  if (it == section_map.end ())
    section_map[secn_name] = Section_id (handle, shndx);
}
