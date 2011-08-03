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

#ifndef CALLGRAPH_H
#define CALLGRAPH_H

#include <vector>
#include <string>
#include <map>
#include <list>
#include <iostream>
#include <stdio.h>
#include <assert.h>

class Node;
class Edge;

/* Represents a node on the call graph.  */
class Node
{
 public:
  Node (unsigned int id, std::string name)
    : id_ (id), name_ (name), merge_next_ (NULL), edge_list_ (),
      last_merge_node_ (this), is_merged_ (false), is_real_node_ (false)
  { }

  /* Returns the unique id for this node.  */
  unsigned int id () const
  { return this->id_; }

  /* Name of the function corresponding to this node.  */
  const std::string&
  name () const
  { return this->name_; }

  /* Add a new edge to the list of edges with this node.  */
  void add_edge (Edge *edge)
  { edge_list_.push_back (edge); }

  /* Returns the list of edges that contain this node.  */
  std::list<Edge*>& edge_list ()
  { return edge_list_; }

  /* Returns true if this node has been merged onto an other node.  */
  bool is_merged () const
  { return this->is_merged_; }

  /* Chain the nodes that are merged. Maintain a pointer to the last
     node in the chain.  After merging at the end, the last node in the
     current chain is the last node in the chain of the merged node.  */
  void merge_node (Node *mergee)
  {
    last_merge_node_->merge_next_ = mergee;
    last_merge_node_ = mergee->last_merge_node_;
    mergee->is_merged_ = true;
  }

  /* Get the next node in the chain of merged nodes.  */
  Node *get_merge_next () const
  { return this->merge_next_; }

  /* True if the function corresponding to this node can be re-ordered.  */
  bool is_real_node () const
  { return this->is_real_node_; }

  void set_as_real_node ()
  { this->is_real_node_ = true; }

 private:
  unsigned int id_;
  std::string name_;
  /* Pointer to the next node in the chain of merged nodes.  */
  Node *merge_next_;
  /* List of edges with this node.  */
  std::list<Edge*> edge_list_;
  /* Pointer to the last node in the chain of merged nodes.  */
  Node *last_merge_node_;
  bool is_merged_;
  bool is_real_node_;
};

/* WEAK if one of the nodes is not real. STRONG if both
   nodes are real.  */
typedef enum edge_type_
{
  WEAK_EDGE,
  STRONG_EDGE
} Edge_type;

/* Represents an edge in the call graph.  */
class Edge
{
 public:
  Edge (Node *function_1,
        Node *function_2,
       unsigned int weight)
    : weight_ (weight), next_ (NULL), prev_ (NULL), is_merged_ (false)
  {
    this->set_functions (function_1, function_2);
    function_1->add_edge (this);
    function_2->add_edge (this);
  }

  /* Assigns the two nodes that make up this edge.  */
  void set_functions (Node *function_1, Node *function_2)
  {
    /* No self edges.  */
    assert (function_1->id () != function_2->id ());
    if (function_1->id () < function_2->id ())
      {
        first_function_ = function_1;
	second_function_ = function_2;
      }
    else
      {
        first_function_ = function_2;
	second_function_ = function_1;
      }
  }

  /* The first node that makes up this edge.  */
  Node *first_function () const
  { return this->first_function_; }

  /* The second node that makes up this edge.  */
  Node *second_function () const
  { return this->second_function_; }

  /* Returns the weight of this edge.  */
  unsigned int weight () const
  { return this->weight_; }

  /* Increments the weight of this edge by WEIGHT.  */
  void add_weight (unsigned int weight)
  {  this->weight_ += weight; }

  /* Compares the weight of two edges. If the types of the edges are
     different, then the STRONG_EDGE is always bigger.  */
  bool operator< (const Edge& edge)
  {
    if (this->edge_type () == edge.edge_type ())
      return this->weight_ < edge.weight_;

    if (this->edge_type () == STRONG_EDGE)
      return false;

    return true;
  }

  /* Gets the next edge in a chain of edges.  */
  Edge *get_next () const
  { return this->next_; }

  /* Sets the next edge in a chain of edges.  */
  void set_next (Edge *edge)
  { this->next_ = edge; }

  /* Gets the previous edge in a chain of edges.  */
  Edge *get_prev () const
  { return this->prev_; }

  /* Sets the previous edge in a chain of edges.  */
  void set_prev (Edge *edge)
  { this->prev_ = edge; }

  /* True is the nodes corresponding to this edge have been merged.  */
  bool is_merged () const
  { return this->is_merged_; }

  /* Set that the nodes corresponding to this edge have been merged.  */
  void set_merged ()
  { this->is_merged_ = true; }

  Edge_type edge_type () const
  { return edge_type_; }

  /* A WEAK_EDGE has one of its nodes corresponding to a function
  that cannot be re-ordered. Functions that can be re-ordered
  are marked as real nodes.  */
  void
  set_edge_type ()
  {
    if (this->first_function_->is_real_node ()
        && this->second_function_->is_real_node ())
        this->edge_type_ = STRONG_EDGE;
    else
        this->edge_type_ = WEAK_EDGE;
  }

 private:

  Node *first_function_;
  Node *second_function_;
  unsigned int weight_;
  Edge *next_;
  Edge *prev_;
  /* True if the nodes corresponding to this edge have been merged.  */
  bool is_merged_;
  Edge_type edge_type_;
};


/* List of Functions or Nodes.  */
typedef std::vector<Node*> Function_list;

/* Maps a function name to the id in the Function_list.  */
typedef std::map<std::string, unsigned int> Function_map;

typedef Edge *Edge_ptr;
typedef std::list<Edge*> Edge_list;
typedef std::pair<unsigned int, unsigned int> Raw_edge;
typedef std::map<Raw_edge, Edge*> Edge_map;

typedef std::pair<void*, int> Section_id;
typedef std::map<std::string, std::pair<void*, int> > Section_map;

void
parse_callgraph_section_contents (unsigned char *contents, unsigned int length);

/* Maps the section name to its corresponding object handle
   and the section index.  */
void
map_section_name_to_index (char *name, void *handle, int shndx);

void
dump_functions ();

void
dump_edges ();

void
find_pettis_hansen_function_layout ();

unsigned int
get_layout (const char *out_filename, void ***handles, unsigned int **shndx);

#endif
