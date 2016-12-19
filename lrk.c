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

/*
 * lrk.c
 *
 * Functions for LR(k).
 *
 * @Author: Xin Chen
 * @Created on: 11/25/2008
 * @Last modified: 11/25/2008
 * @Copyright: 2008, 2009
 */

#include "y.h"
#include "lane_tracing.h"

#define DEBUG_LRK 0

/** For (conflict_config, lane_end_config) pairs. */
ConfigPairList lane_head_tail_pairs;

/** LR(k) parsing table array. */
LRk_P_T_Array * lrk_pt_array;


static void print_int(void * object) { printf("%d\n", * ((int *) object)); }
static void print_string(void * object) { printf("%s\n", (char *) object); }
void print_symbolList(void * object)
{
  SymbolNode * n = (SymbolNode *) object;
  writeSymbolList(n, "");
}
static void print_cfg_ctxt(void * object)
{
  Configuration * c;
  SymbolList n;
  cfg_ctxt * cc;
  if (NULL == object) return;

  cc = (cfg_ctxt *) object;
  c = cc->c;
  n = cc->ctxt;

  if (c == NULL) { puts("c is NULL"); return; }
  if (n == NULL) { puts("n is NULL"); return; }

  printf("%d.%d: ", c->owner->state_no, c->ruleID);
  writeSymbolList(n, "");
}

static void test_int()
{
  Set * s;
  int i, j, k;

  puts("test_int");
  i = 1; j = 2; k = 3;
  s = NULL;
  s = Set_insert(s, (void *) & i);

  Set_dump(s, & print_int);
  s = Set_insert(s, (void *) & j);
  s = Set_insert(s, (void *) & j);
  s = Set_delete(s, (void *) & i);
  s = Set_delete(s, (void *) & j);
  Set_dump(s, & print_int);
}


static void test_str()
{
  Set * s;
  char * s1 = "s1"; 
  char * s2 = "s2";
  char * s3 = "s3";
  puts("test_str");
  s = NULL;

  s = Set_insert(s, (void *) s1);
  s = Set_insert(s, (void *) s2);
  s = Set_insert(s, (void *) s3);
  s = Set_delete(s, (void *) s3);
  Set_dump(s, & print_string);
}

static void test_lrk_theads()
{
  SymbolList alpha;
  SymbolNode * t;
  List * th;

  puts("test_lrk_theads(), on G_thead.");
  t = alpha = createSymbolNode(hashTbl_find("X")); 
  t->next = createSymbolNode(hashTbl_find("Y")); 
  t = t->next;
  t->next = createSymbolNode(hashTbl_find("Z"));
  t = t->next;
  t->next = createSymbolNode(hashTbl_find("U"));
  t = t->next;

  th = lrk_theads(alpha, 2);
  List_dump(th, & print_symbolList);
  exit(0);
}


static void dump_conflict_list(Conflict * c)
{
  for (; c != NULL; c = c->next) {
    printf("state %d, token %s, actions: [%d, %d], decision: %d\n",
           c->state, c->lookahead->symbol,
           c->r, c->s, c->decision);
  }
}


void test_dump_P_conflict()
{
  int i, len;
  len = states_new_array->state_count;
  printf("dump_P_conflict(). total states: %d\n", len);

  for (i = 0; i < len; i ++) {
    if (states_new_array->rr_count[i] > 0) {
      printf("state %d rr_count: %d\n", i, states_new_array->rr_count[i]);
      dump_conflict_list(states_new_array->conflict_list[i]);
    }
  }
}


// return the length of the symbollist.
int get_lrk_theads_len(SymbolList s)
{
  SymbolNode * t;
  int len = 0;
  
  for (t = s; t != NULL; t = t->next) {
    len ++;
  }
  return len;
}


// return the last symbol node.
SymbolNode * get_last_symbol(SymbolList s)
{
  SymbolNode * t;
  if (NULL == s) return NULL;

  for (t = s; t->next != NULL; t = t->next) { }

  return t;
}


static Configuration * get_start_config_from_tail(Configuration * c)
{
  ConfigPairNode * n = ConfigPairList_find(lane_head_tail_pairs, c);
  if (NULL == n) return NULL;
  else return n->start;
}


//
// If configuration cc->c is not in set, insert cc to set.
// Otherwise, compare cc->ctxt symbol list and add in new symbols
// to the object in set.
//
static Set * insert_cfg_ctxt_to_set(cfg_ctxt * cc, Set * st)
{
  Object_item * o;
  cfg_ctxt * s;

  if (st == NULL) {
    st = Set_insert(st, (void *) cc);
    return st;
  }

  for (o = st; o != NULL; o = o->next) {
    s = (cfg_ctxt *) o->object;
    if (s->c == cc->c) {
      s->ctxt = combineIncSymbolList(s->ctxt, cc->ctxt);
      return st;
    }
  }

  // now o is NULL
  st = Set_insert(st, (void *) cc);
  return st;
}


/*
 * Note that dummy is set_c2. Use it as global variable instead of passed in
 * to overcome the lack of head pointer problem.
 *
 * Insert c->ruleID to entry col at LR(MAX_K) table row (state_no, token).
 * for each token in token_list (context list).
 */
static Set * insert_LRk_PT(int state_no, SymbolNode * token_list, 
    SymbolTblNode * col, Configuration * c, Configuration * c_tail, 
    Set * set_c2)
{
  cfg_ctxt * cc;
  SymbolNode * token;
  LRk_P_T * pt; // LR(k) parsing table where k = MAX_K.
  ConfigPairNode * c0;
  BOOL exist;

  // create LR(MAX_K) parsing table if it does not exist yet.
  if (NULL == (pt = LRk_P_T_Array_get(lrk_pt_array, MAX_K))) {
    pt = LRk_P_T_create(MAX_K);
    LRk_P_T_Array_add(lrk_pt_array, pt);
  }

  for (token = token_list; token != NULL; token = token->next) {
    c0 = LRk_P_T_getEntry(pt, state_no, token->snode, col, & exist);
    if (exist == FALSE || c0 == NULL) { // no conflict.
      LRk_P_T_addReduction(pt, state_no, token->snode, col, c, c_tail);
    } else { // exist is TRUE, and c0 != NULL.
      cc = cfg_ctxt_create(c, createSymbolNode(col), c_tail);
      set_c2 = insert_cfg_ctxt_to_set(cc, set_c2);
      if (c0->end == (Configuration *) CONST_CONFLICT_SYMBOL) {
        // do nothing.
      } else {
        cc = cfg_ctxt_create(c0->start, createSymbolNode(col), c0->end);
        set_c2 = insert_cfg_ctxt_to_set(cc, set_c2);
        // set this entry to CONST_CONFLICT_SYMBOL.
        LRk_P_T_addReduction(pt, state_no, token->snode, col, c, c_tail);
      }
    }
  }

  return set_c2;
}


static void lrk_config_lane_tracing(Configuration * c)
{
  EDGE_PUSHING_CONTEXT_GENERATED = NULL;
  IN_EDGE_PUSHING_LANE_TRACING = TRUE;
  lane_tracing_reduction(c);

  // pretend that c is a reduce configuration.
  c->LANE_END = 0;
  cur_red_config = c;

  // get LANE_END configs and add to lane_head_tail_pairs list.
  trace_back_lrk(NULL, c);

  // clear the LANE_CON flag of configurations on conflicting lanes.
  trace_back_lrk_clear(NULL, c);

  c->LANE_END = 1; // recover the value of c->LANE_END.
  // note that the value of cur_red_config does not need recovery.
}


/*
 * @Return: the conflict symbol list of configuration c in INC order.
 */
static SymbolList getConfigConflictContext(Configuration * c)
{
  Conflict * n; // conflict list.
  SymbolList contxt;
  SymbolNode * ret_list = NULL;

  if (NULL == c) return ret_list;

  n = states_new_array->conflict_list[c->owner->state_no];
  for (; n != NULL; n = n->next) {
    if (n->r < 0 && n->s < 0) { // is r/r conflict.
      for (contxt = c->context->nContext; 
           contxt != NULL; contxt = contxt->next) {
        if (n->lookahead == contxt->snode) {
          ret_list = insertIncSymbolList(ret_list, n->lookahead);
        }
      }
    }
  }

  printf("conflict symbol for config %d.%d", c->owner->state_no, c->ruleID);
  writeSymbolList(ret_list, "");

  return ret_list;
}


static Set * fill_set_c2(ConfigPairNode * n, 
        Configuration * c, Configuration * c_tail, 
        int k1, Set * set_c2, int state_no, cfg_ctxt * cc)
{
  SymbolNode * sn;
  // n->start is the next level lane head.
  n->start->z = c->z + k1 - 1; 
  printf("next level lane head: %d.%d. z = %d + %d - 1 = %d\n",
         n->start->owner->state_no, n->start->ruleID,
         c->z, k1, 
         n->start->z);
  sn = EDGE_PUSHING_CONTEXT_GENERATED;
  for(; sn != NULL; sn = sn->next) {
    set_c2 = insert_LRk_PT(state_no, cc->ctxt, sn->snode,
                           n->start, c_tail, set_c2);
  }

  return set_c2;
}


/*
 * @Input: inadequate state no.: state_no.
 */
static void edge_pushing(int state_no)
{
  cfg_ctxt * cc;
  Set * set_c;
  Set * set_c2;
  Object_item * si;
  List * phi;
  Object_item * x;
  SymbolList x_str;
  int i, len, k, k1, x_len;
  Configuration * c, * c_tail;
  ConfigPairNode * n, * tmp;
  State * s;
  if (NULL == (s = states_new_array->state_list[state_no])) return;

  printf("\nedge_pushing on state %d\n", state_no);

  k = 1;
  set_c = set_c2 = NULL;

  // note, need to get conflict symbols for each final config also.

  for (i = 0, len = s->config_count; i < len; i ++) {
    c = s->config[i];
    if (TRUE == isFinalConfiguration(c)) {
      // check if this final config's context contains conflict symbol,
      // if so add it to set_c, together with the conflict symbol(s).
      cc = cfg_ctxt_create(get_start_config_from_tail(c), 
                           getConfigConflictContext(c), c);
      c->z = 0;
      set_c = Set_insert(set_c, (void *) cc);
    }
  }

  while (set_c != NULL) {
    k ++;
    MAX_K = k; // K for LR(K).
    printf("while loop: k = %d\n", k);

    for (si = set_c; si != NULL; si = si->next) {
      cc = (cfg_ctxt *) si->object;
      c = (Configuration *) cc->c;
      if (NULL == c) { printf("? c is NULL"); continue; }
      c_tail = (Configuration *) cc->tail;

      k1 = k - c->z; ///////////!!!!!!!!

      if (c->nMarker == NULL) {
        puts("Error: c->nMarker is NULL. ");
        continue;
      }
      phi = lrk_theads(c->nMarker->next, k1);
      if (phi == NULL) { 
        //puts("phi is NULL. should not!"); 
        continue;
      }

      for (x = phi->head; x != NULL; x = x->next) {
        x_str = (SymbolNode *) x->object;
        x_len = get_lrk_theads_len(x_str);
        if (x_len == k1) {
          set_c2 = insert_LRk_PT(state_no, cc->ctxt, 
            get_last_symbol(x_str)->snode, c, c_tail, set_c2);
        } else if (x_len == k1 - 1) {
          n = ConfigPairList_find(lane_head_tail_pairs, c);
          if (n != NULL) { // found in cache.
            puts("found in cache");
            // this list is in INC order of c.
            for (; n != NULL; n = n->next) {
              if (c != n->end) break;
              set_c2 = fill_set_c2(n, c, c_tail, k1, set_c2, state_no, cc);
            }
          } else {
            tmp = lane_head_tail_pairs; // store the old list.
            lane_head_tail_pairs = NULL;
            lrk_config_lane_tracing(c);
            for (n = lane_head_tail_pairs; n != NULL; n = n->next) {
              set_c2 = fill_set_c2(n, c, c_tail, k1, set_c2, state_no, cc);
            }
            // now combine the two list.
            if (NULL == lane_head_tail_pairs) {
              lane_head_tail_pairs = tmp;
            } else {
              lane_head_tail_pairs = 
                ConfigPairList_combine(lane_head_tail_pairs, tmp); 
              ConfigPairList_destroy(tmp);
            }
            tmp = NULL; // this is not really necessary.

          }

        }
      }
    }

    set_c = set_c2;
    set_c2 = NULL;
  }
}


/*
 * Remove r/r conflict nodes from list.
 */
static void removeRRConflictFromList(int state_no)
{
  Conflict * c, * c_prev;
  int i;

  c_prev = NULL;
  c = states_new_array->conflict_list[state_no];
  while (NULL != c) {
    if (c->r < 0 && c->s < 0) { // remove this node.
      if(c_prev == NULL) { // remove at head.
        states_new_array->conflict_list[state_no] = c->next;
        destroyConflictNode(c);
        c = states_new_array->conflict_list[state_no];
      } else { // remove in the middle.
        c_prev->next = c->next;
        destroyConflictNode(c);
        c = c_prev->next;
      }
      states_new_array->rr_count[state_no] --;
      rr_count --;
      continue;
    }

    c = c->next;
  }

  // if list is empty, remove this conflict.
  if (states_new_array->conflict_list[state_no] == NULL) {
    for (i = 0; i < states_inadequate->count; i ++) {
      if (state_no == states_inadequate->states[i]) {
        states_inadequate->states[i] = -1;
        states_inadequate->count_unresolved --;
      }
    }
  }
}


/*
 * Update LR(1) parsing table.
 * 1) set entry to -10000010.
 * 2) remove this conflict from conflict list.
 */
static void update_LR1_ParsingTable(int state_no)
{
  Conflict * c;
  c = states_new_array->conflict_list[state_no];
  for (; c != NULL; c = c->next) {
    updateAction(getCol(c->lookahead), state_no, -10000010);
  }

  // remove r/r conflict node from list.
  removeRRConflictFromList(state_no);
}


void lane_tracing_LR_k()
{
  int i, ct, ct_rr, state_no;
  if (0 == states_inadequate->count_unresolved) return;
  LRk_PT = NULL; // parsing table extension.
  MAX_K ++; // increment K for LR(k).
  lrk_pt_array = LRk_P_T_Array_create();

  printf("\n------------- lane_tracing_LR_k ------------- ");
  puts("lane head/tail pairs:"); ConfigPairList_dump(lane_head_tail_pairs);

  ct = states_inadequate->count;
  for (i = 0; i < ct; i ++) {
    state_no = states_inadequate->states[i];
    ct_rr = states_new_array->rr_count[state_no];

    if (state_no >= 0 && ct_rr > 0) {
      edge_pushing(state_no);

      // update corresonding entry in LR(1) parsing table.
      update_LR1_ParsingTable(state_no);
    }
  }
}
