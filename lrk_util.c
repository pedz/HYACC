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
 * lrk_util.c
 *
 * Utility functions for LR(k).
 *
 * @Author: Xin Chen
 * @Created on: 11/25/2008
 * @Last modified: 11/25/2008
 * @Copyright: 2008
 */

#include "y.h"
#include "lane_tracing.h"



//////////////////////////////////////////////////////////////////
// Functions for ConfigPairNode. START.
//////////////////////////////////////////////////////////////////
  
static ConfigPairNode * ConfigPairNode_create(
    Configuration * conflict_config, Configuration * lane_start_config)
{
  ConfigPairNode * n;
  HYY_NEW(n, ConfigPairNode, 1);
  n->end = conflict_config;
  n->start = lane_start_config;
  n->next = NULL;
  return n;
} 


static void ConfigPairNode_destroy(ConfigPairNode * n)
{
  free(n);
}


/*
 * compare function: 1 - greater than, 0 - equal, -1 - less than.
 */
static int ConfigPair_cmp(Configuration * end1, Configuration * start1,
    Configuration * end2, Configuration * start2)
{
  int cmp;

  cmp = start1->owner->state_no - start2->owner->state_no;
  if (cmp > 0) return 1;
  if (cmp < 0) return -1;
  // else, == 0.

  cmp = start1->ruleID - start2->ruleID;
  if (cmp > 0) return 1;
  if (cmp < 0) return -1;
  // else, == 0.

  cmp = end1->owner->state_no - end2->owner->state_no;
  if (cmp > 0) return 1;
  if (cmp < 0) return -1;
  // else, == 0.

  cmp = end1->ruleID - end2->ruleID;
  if (cmp > 0) return 1;
  if (cmp < 0) return -1;
  // else, == 0.

  return 0;
}


/*
 * Combine list s(ource) to list t(arget).
 */
ConfigPairList ConfigPairList_combine(ConfigPairList t, ConfigPairList s)
{
  ConfigPairNode * n;
  if (s == NULL) return t;
  if (t == NULL) return s;

  for (n = s; n != NULL; n = n->next) {
    t = ConfigPairList_insert(t, n->end, n->start);
  }

  return t;
}


/*
 * Insert in INC order of:
 *   conflict_config's state_no and ruleID,
 *   lane_start_config's state_no and ruleID.
 */
ConfigPairList ConfigPairList_insert(ConfigPairList list,
    Configuration * conflict_config, Configuration * lane_start_config)
{
  ConfigPairNode * n, * n_prev, * m;
  int cmp;

  if (list == NULL) {
    return ConfigPairNode_create(conflict_config, lane_start_config);
  }

  for (n_prev = NULL, n = list; n != NULL; n_prev = n, n = n->next) {
    cmp = ConfigPair_cmp(n->end, n->start, 
                         conflict_config, lane_start_config);
    if (cmp < 0) {
      continue;
    } else if (cmp == 0) {
      // existing config pair.
      return list;
    } else { // cmp > 0, insert to list between n and n_prev.
      m = ConfigPairNode_create(conflict_config, lane_start_config);
      if (n_prev == NULL) { // insert at start
        m->next = list;
        list = m;
      } else { // insert in the middle
        n_prev->next = m;
        m->next = n;
      }
      return list;
    }
  }

  // n is NULL. insert at the end.
  n_prev->next = ConfigPairNode_create(conflict_config, lane_start_config);
  return list;
}


static void ConfigPairNode_dump(ConfigPairNode * n)
{
  printf("(%d.%d -> %d.%d)",
         n->start->owner->state_no, n->start->ruleID,
         n->end->owner->state_no, n->end->ruleID);

  if (n->start->owner->PASS_THRU == 1) {
    printf(" PASS_THRU. ");
  }
}


void ConfigPairList_dump(ConfigPairList list)
{
  ConfigPairNode * n;

  puts("--ConfigPairList--");
  for (n = list; n != NULL; n = n->next) {
    ConfigPairNode_dump(n);
    puts("");
  }
}


/*
 * Note that more than one LANE_END configurations could be found.
 */
ConfigPairNode * ConfigPairList_find(
    ConfigPairList list, Configuration * conflict_config)
{
  ConfigPairNode * n;
  for (n = list; n != NULL; n = n->next) {
    if (n->end == conflict_config) {
      return n;
    }
  }
#if DEBUG_EdgePushing
  puts("not found");
#endif
  return NULL;
} 


void ConfigPairList_destroy(ConfigPairList list)
{
  ConfigPairNode * n, * tmp;

  n = list;
  while (n != NULL) {
    tmp = n;
    n = n->next;
    ConfigPairNode_destroy(tmp);
  }
}

  
//////////////////////////////////////////////////////////////////
// Functions for ConfigPairNode. END.
//////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////
// Functions for Set. START.
//////////////////////////////////////////////////////////////////


/*
 * Create new set object.
 */
static Object_item * Object_item_new(void * object)
{
  Object_item * s;
  HYY_NEW(s, Object_item, 1);
  s->object = object;
  s->next = NULL;
  return s;
}

static void Object_item_destroy(Object_item * s)
{
  free(s);
}

/*
 * Insert if not exist.
 * NOTE: "not exist" means the object, but if "not exist" means 
 * the contents of the object, then a separate function should 
 * be written for this.
 */
Set * Set_insert(Set * set, void * object)
{
  Set * s;

  if (set == NULL) { return Object_item_new(object); }

  // else, set is not NULL.
  for (s = set; s->next != NULL; s = s->next) {
    if (s->object == object) return set; // exists already.
  }

  // now s->next is NULL.
  if (s->object == object) { return set; }
  else { s->next = Object_item_new(object); }

  return set;
}


Object_item * Set_find(Set * set, void * object)
{
  Set * s;

  for (s = set; s != NULL; s = s->next) {
    if (s->object == object) return s;
  }

  return NULL;
}


Set * Set_delete(Set * set, void * object)
{
  Set * s, * s_prev;

  if (set == NULL) { return NULL; }
  
  // else, set is not NULL.
  for (s_prev = NULL, s = set; s != NULL; s_prev = s, s = s->next) {
    if (s->object == object) {
      if (s_prev == NULL) {
        s_prev = s;
        s = s->next;
        Object_item_destroy(s_prev);
        return s;
      } else {
        s_prev->next = s->next;
        Object_item_destroy(s);
        return set;
      }
    }
  }

  return set;
}


/*
 * A function pointer is passed in. This function dumps the set item.
 */
void Set_dump(Set * set, void (* set_item_dump)(void *))
{
  Set * s;

  if (set == NULL) {
    puts("(set is empty)");
    return;
  }

  for (s = set; s != NULL; s = s->next) {
    (* set_item_dump)(s->object);
  }
}


//////////////////////////////////////////////////////////////////
// Functions for Set. END.
//////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////
// Functions for List. START.
//////////////////////////////////////////////////////////////////


// create an empty list.
List * List_create()
{
  List * t;
  HYY_NEW(t, List, 1);
  t->head = NULL;
  t->tail = NULL;
  t->count = 0;
  return t;
}


// insert new object at tail of list t, 
// without checking if the object already exists.
void List_insert_tail(List * t, void * object)
{
  if (NULL == object || NULL == t) return;

  if (t->head == NULL) {
    t->head = t->tail = Object_item_new(object);
  } else {
    t->tail->next = Object_item_new(object);
    t->tail = t->tail->next;
  }

  t->count ++;
}


void List_destroy(List * t)
{
  Object_item * o, * tmp;

  if (NULL == t) return;
  if ((o = t->head) != NULL) {
    while (o != NULL) {
      tmp = o;
      o = o->next;
      Object_item_destroy(tmp);
    }
  }

  free(t); 
}


void List_dump(List * t, void (* list_item_dump)(void *))
{
  int i;
  Object_item * s;
  
  if (t == NULL || t->head == NULL) {
    puts("(list is empty)");
    return; 
  }

  printf("list count: %d\n", t->count);

  i = 0;
  for (s = t->head; s != NULL; s = s->next) {
    printf("%d ", ++ i);
    (* list_item_dump)(s->object);
  }
}


//////////////////////////////////////////////////////////////////
// Functions for List. END.
//////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////
// Functions for LR(k) table. START.
//////////////////////////////////////////////////////////////////

// create a parsing table.
LRk_P_T * LRk_P_T_create(int k)
{
  LRk_P_T * t;
  HYY_NEW(t, LRk_P_T, 1);
  t->k = k;
  t->row_count = 0;
  t->rows = NULL;
  return t;
}


void LRk_P_T_dump(LRk_P_T * t)
{
  LRk_P_T_row * r;
  ConfigPairNode * n;
  Configuration * c;
  int i;

  if (NULL == t) return;
  if (NULL == t->rows) {
    printf("(empty LR(%d) parsing table)\n", t->k);
    return;
  }

  for (r = t->rows; r != NULL; r = r->next) {
    printf("[%d, %s] ", r->state, r->token->snode->symbol);
    for (i = 0; i < ParsingTblCols; i ++) {
      n = r->row[i];
      if (n == NULL) {
        printf("0 ");
      } else {
        c = n->end; ///start;
        if (c == (Configuration *) CONST_CONFLICT_SYMBOL) {
          printf("X ");
        } else {
          printf("%d ", n->end->ruleID);
        }
      }
    }
    puts("");
  }
  puts("");
}


/*
 * Same as LRk_P_T_dump() but write a file.
 */
void LRk_P_T_dump_FILE(LRk_P_T * t, FILE * fp)
{
  LRk_P_T_row * r;
  ConfigPairNode * n;
  Configuration * c;
  int i;

  if (NULL == fp) {
    puts("LRk_P_T_dump_FILE error: fp is closed.");
    return;
  }
  if (NULL == t) return;
  if (NULL == t->rows) {
    fprintf(fp, "(empty LR(%d) parsing table)\n", t->k);
    return;
  }

  for (r = t->rows; r != NULL; r = r->next) {
    fprintf(fp, "[%d, %s] ", r->state, r->token->snode->symbol);
    for (i = 0; i < ParsingTblCols; i ++) {
      n = r->row[i];
      if (n == NULL) {
        fprintf(fp, "0 ");
      } else {
        c = n->end; ///start;
        if (c == (Configuration *) CONST_CONFLICT_SYMBOL) {
          fprintf(fp, "X ");
        } else {
          fprintf(fp, "%d ", n->end->ruleID);
        }
      }
    }
    fprintf(fp, "\n");
  }
  fprintf(fp, "\n");
}


/*
 * @Return: found - TRUE if found, FALSE if not.
 *          If found is TRUE, return the row.
 *          otherwise, return the row before the insertion point.
 */
LRk_P_T_row * LRk_P_T_find(LRk_P_T * t, 
    int state, SymbolTblNode * token, int * found)
{
  LRk_P_T_row * r, * r_prev;
  int cmp;

  * found = FALSE;
  if (NULL == t) return NULL;

  r = t->rows;
  if (NULL == r) return NULL;

  for (r_prev = NULL; r != NULL; r_prev = r, r = r->next) {
    if (r->state < state) continue;
    else if (r->state > state) break; // not found
    else { // found same state.
      cmp = strcmp(r->token->snode->symbol, token->symbol);
      if (cmp < 0) continue;
      else if (cmp > 0) break;
      else {
        * found = TRUE;
        return r;
      }
    }
  }

  return r_prev;
}


/*
 * Pre-assumption: t is not NULL.
 * Insert the new row after r.
 * @Return: the inserted new row.
 */
static LRk_P_T_row * LRk_P_T_addRow(LRk_P_T * t, LRk_P_T_row * r_prev, 
    int state, SymbolTblNode * token)
{
  LRk_P_T_row * r;
  int i;

  HYY_NEW(r, LRk_P_T_row, 1);
  r->state = state;
  r->token = createSymbolNode(token);

  HYY_NEW(r->row, ConfigPairNode *, ParsingTblCols);
  for (i = 0; i < ParsingTblCols; i ++) {
    r->row[i] = NULL; // initialize to NULL.
  }

  r->next = NULL;
  if (r_prev != NULL) {
    r->next = r_prev->next;
    r_prev->next = r;
  } else {
    t->rows = r;
  }

  t->row_count ++;
  return r;
}


/*
 * Get the entry [(state, token), col_token] in t.
 *
 * Assumptions: t != NULL.
 */
ConfigPairNode * LRk_P_T_getEntry(LRk_P_T * t, int state,
    SymbolTblNode * token, SymbolTblNode * col_token, int * exist)
{
  int index, found;
  LRk_P_T_row * r;

  * exist = 0;
  if (t == NULL) return NULL; // parsing table not exist.

  r = LRk_P_T_find(t, state, token, & found);
  if (found == FALSE) return NULL; // row not exist in t.

  * exist = 1; // this entry exists.
  index = getCol(col_token);

  return r->row[index];
} 


/*
 * For row on (state, token), there is a reduce action for symbol s.
 * New entries are inserted in INC order of state, then token.
 *
 * ruleID can be accessed as c->ruleID.
 *
 * Return: TRUE is confilct occurs, FALSE otherwise.
 */
BOOL LRk_P_T_addReduction(LRk_P_T * t, 
    int state, SymbolTblNode * token, 
    SymbolTblNode * s, Configuration * c, Configuration * c_tail)
{
  int index, found;
  Configuration * prev_entry;
  ConfigPairNode * n;
  LRk_P_T_row * r;
  if (NULL == t) return FALSE;

  r = LRk_P_T_find(t, state, token, & found);

  if (found == FALSE) { // insert new entry
    r = LRk_P_T_addRow(t, r, state, token);
  }

  // now add the reduce action on token s.
  index = getCol(s);
  n = r->row[index];
  if (n == NULL) {
    r->row[index] = ConfigPairNode_create(c_tail, c);
    return FALSE;
  } else {
    prev_entry = n->end; ///start;
    if (prev_entry == (Configuration *) CONST_CONFLICT_SYMBOL) {
      printf("row [%d, %s] r/r conflict: CONFLICT_LABEL:%d\n",
        r->state, r->token->snode->symbol, c->ruleID);
    } else if (prev_entry == c_tail) {
      // same config, do nothing.
    } else {
      printf("row [%d, %s] r/r conflict: %d:%d\n", 
        r->state, r->token->snode->symbol, prev_entry->ruleID, c->ruleID);
      r->row[index]->end = (Configuration *) CONST_CONFLICT_SYMBOL;
    }
    return TRUE;
  }
}


//////////////////////////////////////////////////////////////////
// Functions for LR(k) table. END.
//////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////
// Functions for LR(k) table array. START.
//////////////////////////////////////////////////////////////////

LRk_P_T_Array * LRk_P_T_Array_create()
{
  LRk_P_T_Array * a;
  int i;
  HYY_NEW(a, LRk_P_T_Array, 1);
  a->size = 10; // initial max number of entries: 10.
  HYY_NEW(a->array, LRk_P_T *, a->size);
  for (i = 0; i < a->size; i ++) { // initialize to NULL.
    a->array[i] = NULL;
  }
  a->max_k = 1;
  return a;
}


/*
 * Add t to a.
 */
void LRk_P_T_Array_add(LRk_P_T_Array * a, LRk_P_T * t)
{
  if (NULL == a || NULL == t) return;

  if (a->max_k >= a->size + 1) { // expand a->array.
    HYY_EXPAND(a->array, LRk_P_T *, a->size * 2);
    a->size = a->size * 2;
  }
  if (a->array[t->k - 2] == NULL) {
    a->array[t->k - 2] = t;
    if (a->max_k < t->k) {
      a->max_k = t->k;
    }
  } else {
    printf("Error: LR(%d) table already exists\n", t->k);
  }
}


/*
 * Get the LR(k) parsing table for k.
 */
LRk_P_T * LRk_P_T_Array_get(LRk_P_T_Array * a, int k)
{
  if (k < 2 || k > a->max_k) return NULL;
  if (a->array[k - 2] == NULL) {
    printf("Warning: LR(%d) table is empty\n", k);
    return NULL;
  } else {
    return a->array[k - 2];
  }
}


static void writeParsingTblColHdr()
{
  int i;
  printf("--Parsing Table Column Header [Total: %d]--\n", ParsingTblCols);
  for (i = 0; i < ParsingTblCols; i ++) {
    printf("%s ", ParsingTblColHdr[i]->symbol);
  }
  puts("");
}


static void writeParsingTblColHdr_FILE(FILE * fp)
{
  int i;
  fprintf(fp, "--Parsing Table Column Header [Total: %d]--\n", ParsingTblCols);
  for (i = 0; i < ParsingTblCols; i ++) {
    fprintf(fp, "%s ", ParsingTblColHdr[i]->symbol);
  }
  fprintf(fp, "\n");
}


void LRk_P_T_Array_dump(LRk_P_T_Array * a)
{
  int i;
  if (NULL == a) return;

  writeParsingTblColHdr();
  printf("===LRk_P_T_Array_dump [max_k = %d]===\n", a->max_k);

  for (i = 2; i <= a->max_k; i ++) {
    printf("LR(%d) p.t.\n", i);
    LRk_P_T_dump(a->array[i - 2]);
  }

  printf("====================================\n");
}


/*
 * Dump to disk.
 */ 
void LRk_P_T_Array_dump_FILE(LRk_P_T_Array * a)
{
  int i;
  FILE * fp;
  if (NULL == a) return;

  if ((fp = fopen("y.lrk", "w")) == NULL) {
    puts("cannot open file y.lrk to write");
    return;
  }

  writeParsingTblColHdr_FILE(fp);
  fprintf(fp, "===LRk_P_T_Array_dump [max_k = %d]===\n", a->max_k);

  for (i = 2; i <= a->max_k; i ++) {
    fprintf(fp, "LR(%d) p.t.\n", i);
    LRk_P_T_dump_FILE(a->array[i - 2], fp);
  }

  fclose(fp);
}


//////////////////////////////////////////////////////////////////
// Functions for LR(k) table array. END.
//////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////
// Functions for struct cfg_ctxt. START.
//////////////////////////////////////////////////////////////////


cfg_ctxt * cfg_ctxt_create(Configuration * c, SymbolList s, 
    Configuration * tail)
{
  cfg_ctxt * cc;
  HYY_NEW(cc, cfg_ctxt, 1);
  cc->c = c;
  cc->ctxt = s;
  cc->tail = tail;
  return cc;
}


void cfg_ctxt_destroy(cfg_ctxt * cc)
{
  free(cc);
}


void cfg_ctxt_dump(cfg_ctxt * cc)
{
  SymbolList a;
  if (NULL == cc) return;
  printf("cfg_ctxt: %d.%d { ", cc->c->owner->state_no, cc->c->ruleID);
  for (a = cc->ctxt; a != NULL; a = a->next) {
    printf("%s ", a->snode->symbol);
  }
  printf("}");
  printf("[tail: ");
  if (cc->tail != NULL) {
    printf("%d.%d", cc->tail->owner->state_no, cc->tail->ruleID);
  }
  printf("]");
  puts("");
}


//////////////////////////////////////////////////////////////////
// Functions for struct cfg_ctxt. END.
//////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////
// Functions for LR(k) theads. START.
//////////////////////////////////////////////////////////////////

#define DEBUG_LRK_THEADS_CYCLE 1
#define DEBUG_LRK_THEADS 0


//
// Get the head of string alpha up to the k-th symbol that 
// does not vanish, or the entire alpha if less than k symbols
// do not vanish.
// Note: k >= 2.
// @Return: a COPY of the head of the original string is returned.
//
SymbolList getStringWithKNonVanishSymbol(SymbolList alpha, int k)
{
  SymbolNode * cpy, * cpy_tail, * pt; // copy.
  int i; // count of non-vanish symbols.

  if (NULL == alpha || k <= 0) return NULL;

  pt = alpha;
  cpy = cpy_tail = createSymbolNode(pt->snode);
  i = (isVanishSymbol(pt->snode) == FALSE) ? 1 : 0;
  if (i == k) return cpy;

  for (pt = alpha->next; pt != NULL; pt = pt->next) {
    cpy_tail->next = createSymbolNode(pt->snode);
    cpy_tail = cpy_tail->next;
    if (isVanishSymbol(pt->snode) == FALSE) i ++;
    if (i == k) break;
  }

  return cpy;
}


//
// n is a node in list new_list, 
// replace symbol n->next with the RHS of rule numbered ruleID.
// return the new list.
//
SymbolNode * replaceWithRHS(
    SymbolList new_list, SymbolNode * n_prev, int ruleID)
{
  SymbolList rhs;
  SymbolNode * rhs_tail, * tmp;

  rhs = cloneSymbolList(grammar.rules[ruleID]->nRHS_head);
  if (rhs == NULL) {
    rhs_tail = NULL;
    // in this case, rhs is empty list, 
    // just remove n_prev->next from new_list.
    if (NULL == n_prev) {
      tmp = new_list;
      new_list = new_list->next;
    } else {
      tmp = n_prev->next;
      n_prev->next = tmp->next;
    }
    freeSymbolNode(tmp);
    return new_list;
  }

  // else, rhs is NOT NULL.
  for (rhs_tail = rhs; rhs_tail->next != NULL; rhs_tail = rhs_tail->next) ;
 
  if (n_prev == NULL) { // replace the first symbol with rhs list.
    tmp = new_list;
    rhs_tail->next = tmp->next;
    freeSymbolNode(tmp);
    new_list = rhs;
  } else { // replace symbol n_prev->next in the middle.
    tmp = n_prev->next;
    rhs_tail->next = tmp->next;
    freeSymbolNode(tmp);
    n_prev->next = rhs;
  }

  return new_list;
}


BOOL isSameSymbolList(SymbolList a, SymbolList b)
{
  for (; (a != NULL) && (b != NULL); a = a->next, b = b->next) {
    if (a->snode != b->snode) return FALSE;
  }
  if ((NULL == a) && (NULL == b)) return TRUE;
  else return FALSE;
}


// assumption: new_list != NULL, t != NULL.
BOOL lrk_thead_in_list(List * t, SymbolList new_list)
{
  Object_item * o;
  if (NULL == t || new_list == NULL) {
    puts("lrk_thead_in_list ERROR: t or nwe_list is NULL");
    exit(1);
  }

  for (o = t->head; o != NULL; o = o->next) {
    if (TRUE == isSameSymbolList(new_list, (SymbolList) o->object)) {
      return TRUE;
    }
  }

  return FALSE;
}


//
// Assumption: k >= 2.
// Truncate list s so it contains up to k non-vanishable symbols.
//
static SymbolList lrk_theads_truncate_list_by_k(SymbolList s, int k)
{
  SymbolNode * t, * tmp;
  int i;
  if(NULL == s) return NULL;

  i = 0;
  for (t = s; t != NULL; t = t->next) {
    if (t->snode->vanishable == FALSE) i ++;
    if (i >= k) { // truncate from after this point.
      //puts("--Yes truncate");
      tmp = t->next;
      t->next = NULL;
      if (NULL != tmp) { freeSymbolNodeList(tmp); }
      break;
    }
  }

  return s;
}


//
// Add to the end of list the result of applying all possible
// productions to the j-th symbol, omitting existing strings,
// and truncate until it contains no more than k non-vanishable
// symbols.
//
void addDerivatives(List * t, Object_item * o, int j, int k)
{
  SymbolList new_list;
  SymbolNode * m, * n, * n_prev; 
  RuleIDNode * r;
  int i;

  // get the (j)-th symbol.
  m = (SymbolList) o->object; 
  for (i = 0; i < j; i ++) { 
    if (NULL == m) { return; }
    m = m->next; 
  }
  if (NULL == m) { return; }

  for (r = m->snode->ruleIDList; r != NULL; r = r->next) {
    new_list = cloneSymbolList((SymbolNode *) o->object);
    // get the (j-1)-th symbol and store as n_prev.
    n_prev = NULL;
    n = new_list;
    for (i = 0; i < j; i ++) {
      n_prev = n;
      n = n->next;
    }

    new_list = replaceWithRHS(new_list, n_prev, r->ruleID);
    // assumption: new_list != NULL, t != NULL.
    if (NULL != new_list) {
      new_list = lrk_theads_truncate_list_by_k(new_list, k);
      if (lrk_thead_in_list(t, new_list) == FALSE) {
        List_insert_tail(t, (void *) new_list);
      }
    } // end if
  } // end for
}


//
// Assumption: s.length >= j.
//
// Return TRUE is the j-th symbol of s exists and is Non-terminal.
// otherwise return FALSE.
//
BOOL j_th_symbol_is_NT(SymbolList s, int j)
{
  int i;

  for (i = 0; i < j; i ++) {
    if (s == NULL) return FALSE;
    s = s->next;
  }
  if (s == NULL) return FALSE;

  return (s->snode->type == _NONTERMINAL);
}


// 
// Remove from list t all strings whose j-th symbol is non-terminal.
// 
static void lrk_theads_rm_nt(List * t, int j)
{
  Object_item * o, * o_prev;

#if DEBUG_LRK_THEADS
  puts("\nlrk_theads_rm_nt:");
#endif

  if (NULL == t || NULL == t->head) return;

  o_prev = NULL;
  o = t->head;
  while (o != NULL) {
    if (TRUE == j_th_symbol_is_NT((SymbolList) o->object, j)) {
      t->count --;
      // remove o.
      if (o_prev == NULL) {
        t->head = o->next;
        if (t->head == NULL) { t->tail = NULL; }
        Object_item_destroy(o);
        o = t->head;
      } else {
        o_prev->next = o->next;
        if (o_prev->next == NULL) { t->tail = o_prev; }
        Object_item_destroy(o);
        o = o_prev->next;
      }
    } else {
      o_prev = o;
      o = o->next;
    }
  }
}


//
// Assumption: k >= 2.
//
// Returns TRUE if the first k symbols are s are all terminals.
// Otherwise returns FALSE.
//
static BOOL k_heads_are_T(SymbolList s, int k, int * len)
{
  int i;
  SymbolList s0 = s;
  * len = -1;

  if (NULL == s) return FALSE;

  for (i = 0; (i < k) && (NULL != s); i ++) {
    if (s->snode->type == _NONTERMINAL) return FALSE;
    s = s->next;
  }

  * len = i;

  // s contains less than k symbols, and these are all terminals.
  if ((i < k) && (NULL == s)) {
    return FALSE;
  }

  return TRUE;
}


//
// Remove from t all strings whose k-heads consist entirely
// of terminals, and add the k-heads to set t_heads;
//
static void lrk_theads_rm_theads(List * t, int k, List * t_heads)
{
  Object_item * o, * o_prev;
  int len;
  
  if (NULL == t || NULL == t->head) return;
  
  o_prev = NULL;
  o = t->head;
  while (o != NULL) {
    if (TRUE == k_heads_are_T((SymbolList) o->object, k, 
                              & len) ||
        (len > 0 && len < k)) {
      t->count --;
      if (len < k) { 
        // k'-thead, where k' < k. do nothing
        if (len == k - 1) { 
          List_insert_tail(t_heads, o->object);
        }
      } else if (FALSE == lrk_thead_in_list(t_heads, (SymbolList) o->object)) {
        // k-thead.
        List_insert_tail(t_heads, o->object);
      }
      // remove o.
      if (o_prev == NULL) {
        t->head = o->next;
        if (t->head == NULL) { t->tail = NULL; }
        Object_item_destroy(o);
        o = t->head;
      } else {
        o_prev->next = o->next;
        if (o_prev->next == NULL) { t->tail = o_prev; }
        Object_item_destroy(o);
        o = o_prev->next;
      }
    } else {
      o_prev = o;
      o = o->next;
    }
  }
}


/*
 * Find theads of length k for string alpha.
 * This is a set of strings.
 */
List * lrk_theads(SymbolList alpha, int k)
{
  int j;
  SymbolList s;
  List * t;
  List * t_heads;
  Object_item * o;

  if (NULL == alpha) return NULL;

  s = getStringWithKNonVanishSymbol(alpha, k);

  t_heads = List_create(); // set of LR(k) theads.
  t = List_create();
  List_insert_tail(t, (void *) s);

  for (j = 0; j < k; j ++) {
    o = t->head;
    while (o != NULL) {
      addDerivatives(t, o, j, k);
      o = o->next;
    }

    lrk_theads_rm_nt(t, j);
    lrk_theads_rm_theads(t, k, t_heads);
  }

  return t_heads;
}

//////////////////////////////////////////////////////////////////
// Functions for LR(k) theads. END.
//////////////////////////////////////////////////////////////////
