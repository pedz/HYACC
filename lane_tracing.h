/*
   This file is part of Hyacc, a LR(0)/LALR(1)/LR(1)/LR(k) parser generator.
   Copyright (C) 2008, 2009 Xin Chen. chenx@hawaii.edu

   Hyacc is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   Hyacc is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Hyacc; if not, write to the Free Software Foundation, Inc.,
   51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */

#ifndef _LANE_TRACING_H_
#define _LANE_TRACING_H_

#include "y.h"

/*
 * lane_tracing.h
 *
 * Used by lane_tracing.c and lrk.c only.
 *
 * @Author: Xin Chen
 * @Created on: 7/26/2008
 * @Last modified: 3/24/2009
 */


#define DEBUG_EdgePushing 0


/*
 * A macro to determine if a config is the goal production
 * of state 0. Used for getting the "$end" context
 * before testA() in lane-tracing.
 * Single it out and put here, so it's easier to understand.
 */
#define IS_GOAL(o) (o->owner->state_no == 0 && o->ruleID == 0)


/** Data structure for the state-combination in phase 2 table. START */

typedef struct _llist_int llist_int; /* linked list of states */
struct _llist_int {
  int n;
  llist_int * next;
};

/*
 * Similar to llist_int, but has two int fields.
 * Used by LT_cluster only.
 */
typedef struct _llist_int2 llist_int2; /* linked list of states */
struct _llist_int2 {
  int n1;
  int n2;
  llist_int2 * next;
};

typedef struct _llist_context_set llist_context_set;
struct _llist_context_set {
  Configuration * config; // in INC order of config->ruleID.
  SymbolList ctxt; // in INC order of symbol.
  llist_context_set * next;
};


/*
 * to get a State pointer from the state_no, use
 * states_new_array->state_list[state_no].
 */
typedef struct _LT_tbl_entry LT_tbl_entry;
struct _LT_tbl_entry {
  BOOL processed; // whether this entry was processed during regeration.
  int from_state;
  llist_context_set * ctxt_set; // in INC order of config->ruleID.
  llist_int * to_states; // in INC order of state_no.
  LT_tbl_entry * next;
};


/*
 * For state combining purpose.
 */
typedef struct _LT_cluster LT_cluster;
struct _LT_cluster {
  BOOL pairwise_disjoint;
  llist_int2 * states; // in INC order of state_no.
  llist_context_set * ctxt_set; // in INC order of config->ruleID.
  LT_cluster * next;
};


extern LT_cluster * all_clusters;


/* Functions */

extern LT_tbl_entry * LT_tbl_entry_find(State * from);
extern LT_cluster * find_actual_containing_cluster(int state_no);
extern void cluster_dump(LT_cluster * c);
extern llist_int * llist_int_add_inc(llist_int * list, int n);
extern int cluster_contain_state(LT_cluster * c, int state_no);
extern llist_int2 * llist_int2_find_n2(llist_int2 * list, int n2);
extern void llist_int_dump(llist_int * list);


/*
 * Data structures in lrk.c
 */


/*
 * For conflicting lanes' head states and associated conflicting contexts.
 */
typedef struct laneHeadState laneHead;
struct laneHeadState {
  State * s; // conflicting lane head state.
  SymbolList contexts; // list of conflicting contexts.
  laneHead * next;
};


/*
 * For (conflict_config, lane_end_config) pairs.
 */
typedef struct _ConfigPairNode ConfigPairNode;
struct _ConfigPairNode {
  Configuration * end; // conflict_config
  Configuration * start; // lane_start_config
  ConfigPairNode * next;
};
typedef ConfigPairNode * ConfigPairList;

extern ConfigPairList lane_head_tail_pairs;


/*
 * For parsing table extention on LR(k).
 */

typedef struct _LRk_contextListNode LRk_contextListNode;
struct _LRk_contextListNode {
  int k; // level of k in LR(k).
  SymbolList context; // context symbols.
  int context_count;  // basically this is useless but keep it here.
  LRk_contextListNode * next; // to next context list of higher k in LR(k)
};


typedef struct _LRk_configListNode LRk_configListNode;
struct _LRk_configListNode {
  Configuration * config;
  LRk_contextListNode * ctxt_list;
  LRk_configListNode * next;
};


typedef struct _LRk_PT_entry LRk_PT_entry;
struct _LRk_PT_entry {
  int state;
  SymbolTblNode * token;
  LRk_configListNode * cfg_list;
  int conflict_count; // 0 if no conflict. can be > 1, but counted as 1
                      // when report in statistics.
  LRk_PT_entry * next;
};

LRk_PT_entry * LRk_PT; // extension parsing table for LR(k).


/*
 * Functions in lane_tracing.c
 */
extern laneHead * trace_back(
    Configuration * c0, Configuration * c, laneHead * lh_list);
extern void trace_back_lrk(
    Configuration * c0, Configuration * c);
extern void trace_back_lrk_clear(
    Configuration * c0, Configuration * c);


/*
 * Functions in lrk.c
 */
extern void lane_tracing_LR_k();

/*
 * Functions in lrk_util.c
 */
extern ConfigPairList ConfigPairList_combine(
         ConfigPairList t, ConfigPairList s);
extern ConfigPairList ConfigPairList_insert(ConfigPairList list,
    Configuration * conflict_config, Configuration * lane_start_config);
extern void ConfigPairList_dump(ConfigPairList list);
extern ConfigPairNode * ConfigPairList_find(
    ConfigPairList list, Configuration * conflict_config);
extern void ConfigPairList_destroy(ConfigPairList list);

// Set - a linked list of objects.
typedef struct _Object_item Object_item;
struct _Object_item {
  void * object;
  Object_item * next;
};
typedef Object_item Set;
extern Set * Set_insert(Set * set, void * object);
extern Object_item * Set_find(Set * set, void * object);
extern Set * Set_delete(Set * set, void * object);
extern void Set_dump(Set * set, void (* set_item_dump)(void *));


// List - a single linked list.
typedef struct {
  int count;
  Object_item * head;
  Object_item * tail;
} List; 
extern List * List_create();
extern void List_insert_tail(List * t, void * object);
extern void List_destroy(List * t);
extern void List_dump(List * t, void (* list_item_dump)(void *));

extern void print_symbolList(void * object);


//
// LRk_P_T - LR(k) parsing table.
//
#define CONST_CONFLICT_SYMBOL -10000010
typedef struct _LRk_P_T_row LRk_P_T_row;
struct _LRk_P_T_row {
  int state;
  SymbolNode * token;
  ConfigPairNode ** row;
  LRk_P_T_row * next;
};

#define LRk_P_T_INIT_SIZE 10
#define LRk_P_T_INC 10

// LR(k) parsing table.
typedef struct {
  int k; // k in LR(k).
  int row_count; // number of rows.
  LRk_P_T_row * rows;
} LRk_P_T;

extern LRk_P_T * LRk_P_T_create(int k);
extern void LRk_P_T_dump(LRk_P_T * t);
extern LRk_P_T_row * LRk_P_T_find(LRk_P_T * t, 
         int state, SymbolTblNode * token, int * found);
extern ConfigPairNode * LRk_P_T_getEntry(LRk_P_T * t, int state,
         SymbolTblNode * token, SymbolTblNode * col_token, int * exist);
extern BOOL LRk_P_T_addReduction(LRk_P_T * t, 
         int state, SymbolTblNode * token,
         SymbolTblNode * s, Configuration * c, Configuration * c_tail);

//
// LR(k) parsing table array.
//
typedef struct {
  LRk_P_T ** array;
  int max_k; // number of entries in array is max_k - 1.
  int size;  // 0 <= max_k - 2 <= size - 1
} LRk_P_T_Array;

extern LRk_P_T_Array * lrk_pt_array; // defined in lrk.c
  
extern LRk_P_T_Array * LRk_P_T_Array_create();
extern void LRk_P_T_Array_add(LRk_P_T_Array * a, LRk_P_T * t);
extern LRk_P_T * LRk_P_T_Array_get(LRk_P_T_Array * a, int k);
extern void LRk_P_T_Array_dump(LRk_P_T_Array * a);
extern void LRk_P_T_Array_dump_FILE(LRk_P_T_Array * a);


//
// for (configuration, conflict context) pair
//
typedef struct {
  Configuration * c;
  SymbolList ctxt;  // conflict context symbols
  Configuration * tail;
} cfg_ctxt;

extern cfg_ctxt * cfg_ctxt_create(
         Configuration * c, SymbolList s, Configuration * tail);
extern void cfg_ctxt_destroy(cfg_ctxt * cc);
extern void cfg_ctxt_dump(cfg_ctxt * cc);

// for LR(k) theads.
extern List * lrk_theads(SymbolList alpha, int k);

// in the lane_tracing of edge_pushing.
extern BOOL IN_EDGE_PUSHING_LANE_TRACING;
extern Configuration * cur_red_config;
extern SymbolList EDGE_PUSHING_CONTEXT_GENERATED;

#endif
