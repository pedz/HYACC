/* 
   This file is part of Hyacc, a LR(0)/LALR(1)/LR(1)/LR(k) parser generator.
   Copyright (C) 2007, 2008, 2009 Xin Chen. chenx@hawaii.edu

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
 * gen_compiler.c 
 *
 * Contains functions to generate a compiler.
 *
 * @Author: Xin Chen
 * @Date started: 10/16/2006
 * @Last modified: 3/21/2007
 */

#include "y.h"
#include "lane_tracing.h"

extern char * y_tab_c;
extern char * y_tab_h;

static FILE * fp_yacc; // for yacc input file.
static FILE * fp; // pointer to output file y.tab.c
static FILE * fp_h; // for y.tab.h

static int n_line; // count number of lines in yacc input file.
static int n_col;

#define MAX_RULE_LENGTH 0xfffff

char *yystype_definition = "typedef int YYSTYPE;";
static char *yystype_format =
  "#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED\n"
  "%s\n"
  "#define YYSTYPE_IS_DECLARED 1\n"
  "#endif\n"
  ;

extern void addCharToSymbol(int c);
extern char * ysymbol; // token symbol.
extern int ysymbol_pt;
extern int ysymbol_size;

static void prepare_outfile()
{
  if ((fp = fopen(y_tab_c, "w")) == NULL) {
    printf("Cannot open output file %s\n", y_tab_c);
    exit(0);
  }

  if (USE_HEADER_FILE == FALSE) return;
  if ((fp_h = fopen(y_tab_h, "w")) == NULL) {
    printf("Cannot open output file %s\n", y_tab_h);
    fclose(fp);
    exit(0);
  }
}


static void my_perror(char * msg, int c)
{
  printf("\nerror [line %d, col %d]: invalid char '%c'. %s\n",
         n_line, n_col, c, msg);
  exit(1);
}


#define print_break() { fprintf(fp, "break;\n"); }


/*
 * Token - terminal symbols.
 */
void writeTokens()
{
  int i;
  SymbolNode * a;
  for (a = tokens, i = 0; a != NULL; a = a->next, i ++) {
    printf("token %d: %s\n", i + 1, a->snode->symbol);
  }
}


/*
 *  Write all terminal tokens that are not quoted, and not "error".
 */
void writeTokensToCompilerFile()
{
  int i, index = 0;
  SymbolNode * a;
  
  fprintf(fp, "\n/* tokens */\n\n");
  if (USE_HEADER_FILE) fprintf(fp_h, "\n/* tokens */\n\n");

  for (a = tokens, i = 0; a != NULL; a = a->next, i ++) {
    if (a->snode->TP->is_quoted == TRUE ||
        strcmp(a->snode->symbol, strError) == 0) continue;

    fprintf(fp, "#define %s %d\n", a->snode->symbol, index + 257);
    if (USE_HEADER_FILE)
      fprintf(fp_h, "#define %s %d\n", a->snode->symbol, index + 257);

    index ++;
  }

  fprintf(fp, yystype_format, yystype_definition);
  if (USE_HEADER_FILE) {
    fprintf(fp_h, yystype_format, yystype_definition);
    fprintf(fp_h, "\nextern YYSTYPE yylval;\n");
  }
}


/*
 *  Get code declarations from section 1, write to 
 *  y.tab.c, and write token declarations too.
 */
void processYaccFile_section1()
{
  int c, last_c = '\n', last_last_c;
  BOOL IS_CODE = FALSE;

  n_line = n_col = 1;

  while ((c = getc(fp_yacc)) != EOF) {

    if (last_c == '\n' && c == '%') {
      IS_CODE = FALSE;
    } else if (last_last_c == '\n' && last_c == '%') {
      if (c == '%') {
        break; // end of section1.
      } else if (c == '{') {
        IS_CODE = TRUE;
      } else if (c == '}') {
        IS_CODE = FALSE;
      }
    } else if (IS_CODE == TRUE) {
      putc(c, fp); // write to output file.
    }

    last_last_c = last_c;
    last_c = c;

    n_col ++;
    if (c == '\n') { n_line ++; n_col = 1; }
  }

  //writeTokens();
  writeTokensToCompilerFile();
}


/*
 * rewind to section 2.
 */
static void goto_section2()
{
  int c, last_c = 0, last_last_c = '\n';

  fseek(fp_yacc, 0L, 0); // go to the beginning of file fp.
  n_line = 1;
  while ((c = getc(fp_yacc)) != EOF) {
    if (c == '\n') n_line ++;
    if (last_last_c == '\n' && 
        last_c == '%' && c == '%') break; // end section 2.

    last_last_c = last_c;
    last_c = c;
  }
}


/*
 * Pass section 2, go to section 3.
 * Presumption: finished section 1, entering section 2.
 */
static void goto_section3()
{
  int c, last_c = 0, last_last_c = '\n';

  while ((c = getc(fp_yacc)) != EOF) {
    if (c == '\n') n_line ++;
    if (last_last_c == '\n' && last_c == '%' && c == '%') {
      break; // end section 2.
    }
    last_last_c = last_c;
    last_c = c;
  }
}

static SymbolTblNode *find_full_rule(int rule_count, int dollar_number)
{
  Production *rule;
  SymbolNode *node;
  SymbolTblNode *sym;
  int full_rule;
  int i;

  for (full_rule = rule_count; full_rule < grammar.rule_count; ++full_rule) {

    if ((rule = grammar.rules[full_rule]) &&
	(node = rule->nLHS) &&
	(sym = node->snode)) {
      if (strncmp("$$", sym->symbol, 2)) /* node symbol starting with $$ we continue */
	break;
    } else {
      printf("Malformed grammar rule at index %d\n", full_rule);
      exit(1);
    }
  }

  if (dollar_number == MAX_RULE_LENGTH)
    return sym;

  for (i = 1, node = rule->nRHS_head; i < dollar_number && node; ++i, node = node->next);
  if (i != dollar_number) {
    printf("Rule %d terminated before %d RHS\n", full_rule, dollar_number);
    exit(1);
  }
  if (!(sym = node->snode)) {
    printf("Malformed grammar rule %d RHS %d had no symbol table node\n", full_rule, dollar_number);
    exit(1);
  }
  return sym;
}

/*
 * Basically, this has the same structure as function
 *   processYaccFileInput_section2(int c)
 * in parsetYaccInput.c.
 *
 * The purpose here is to extract the code for semantic 
 * actions of rules.
 */
static void processYaccFile_section2(char * filename)
{
  YACC_STATE state = LHS;
  int CODE_LEVEL;
  BOOL READING_SYMBOL = FALSE;
  BOOL READING_NUMBER = FALSE;
  BOOL READING_TYPE = FALSE;
  int dollar_number = 0;
  int c, last_c = 0, last_last_c = 0;
  int rule_count = 0;
  BOOL END_OF_CODE = FALSE; // for mid-production action.
  char *explicit_type = 0;
  SymbolTblNode *sym;
  char *token_type;
  static char * padding = "        ";

  while ((c = getc(fp_yacc)) != EOF) {
    if (last_c == '%' && c == '%') break; // end section 2.

    switch (state) {
      case LHS:
        if (isspace(c) && READING_SYMBOL == FALSE) { 
          // do nothing, skip space.
        } else if (c == ':') {
          READING_SYMBOL = FALSE;
          state = RHS;
        } else if (isspace(c)) { // finish reading symbol
          READING_SYMBOL = FALSE;
          state = COLON;
        } else if (last_c == '/' && c =='*') {
          state = LHS_COMMENT;
          if (READING_SYMBOL == TRUE) {
            READING_SYMBOL = FALSE;
            rule_count --;
          }
        } else if (c == '/') { // do nothing.
        } else if (c == ';') { // do nothing.
        } else if (! isspace(c)) {
          if (READING_SYMBOL == FALSE) { 
            rule_count ++;
            READING_SYMBOL = TRUE;
          }
        }
        break;
      case LHS_COMMENT:
        if (last_c == '*' && c == '/') {
          state = LHS;
        }
        break;
      case COLON:
        if (c == ':') {
          state = RHS;
        } else if (c == '/') {
          // do nothing
        } else if (last_c == '/' && c == '*') {
          state = COLON_COMMENT;
        } else if (! isspace(c)) {
          my_perror("error: state COLON. ", c);
        }
        break;
      case COLON_COMMENT:
        if (last_c == '*' && c == '/') {
          state = COLON;
        }
        break;
      case RHS:
        if (isspace(c)) {
          // end of a symbol. do nothing. 
        } else if (c == '\'') {
          state = TERMINAL;
          if (END_OF_CODE == TRUE) {
            rule_count ++;
            print_break();
            END_OF_CODE = FALSE;
          }

        } else if (c == ';') {
          state = LHS; // end of a rule.
          if (END_OF_CODE == TRUE) { print_break(); }
          END_OF_CODE = FALSE;
        } else if (c == '|') { // end of a rule
          rule_count ++;
          if (END_OF_CODE == TRUE) { print_break(); }
          END_OF_CODE = FALSE;
        } else if (c == '{') { // code for rule #rule_count
          state = CODE;
          CODE_LEVEL = 1;

          if (END_OF_CODE == FALSE) {
            fprintf(fp, "\n%s  case %d:\n", padding, rule_count);
            if (USE_LINES) 
              fprintf(fp, "# line %d \"%s\"\n", n_line, filename);
          }
          putc('{', fp);

        } else if (last_c == '/' && c == '*') {
          state = COMMENT;
        } else if (last_c == '/' && c == '/') {
          state = COMMENT2;
        } else if (c == '/') {
          // do nothing
        } else if (c == ':') {
          my_perror("You may miss a ';' in the last rule.", c);
        } else {
          //reading a symbol. do nothing
          if (END_OF_CODE == TRUE) {
            rule_count ++;
            print_break();
            END_OF_CODE = FALSE;
          }
        }
        break;
      case TERMINAL:
        // avoid '\'' and '\\'.
        if (c == '\'' && (last_c != '\\' || last_last_c == '\\')) {
          state = RHS;
        } else if (! isspace(c)) {
          // reading a symbol. do nothing.
        }
        break;
      case CODE: // the meat. write code to yacc output file.
	if (READING_TYPE) {
	  if (c == '>') {
	    READING_TYPE = FALSE;
	    c = '$';
	    addCharToSymbol('\0');
	    explicit_type = ysymbol;
	  } else {
	    addCharToSymbol(c);
	  }
	} else if (last_c == '$' && c == '<') {
	  ysymbol_pt = 0;
	  READING_TYPE = TRUE;
	} else if (last_c != '$' && c == '$') {
          // do nothing, this may be a special character.
        } else if (last_c == '$' && c == '$') {

	  if (explicit_type) {
	    token_type = explicit_type;
	    explicit_type = 0;
	  } else {
	    sym = find_full_rule(rule_count, MAX_RULE_LENGTH);
	    token_type = sym->token_type;
	  }
	  if (token_type)
	    fprintf(fp, "(yyval.%s)", token_type);
	  else
	    fprintf(fp, "yyval");

        } else if (last_c == '$' && isdigit(c)) {
          READING_NUMBER = TRUE; 
          dollar_number = (c - 48) + 10 * dollar_number;
        } else if (READING_NUMBER == TRUE && isdigit(c)) {
          dollar_number = (c - 48) + 10 * dollar_number;
        } else if (READING_NUMBER == TRUE && ! isdigit(c)) {
	  int RHS_count = grammar.rules[rule_count]->RHS_count;
	  
	  if (explicit_type) {
	    token_type = explicit_type;
	    explicit_type = 0;
	  } else {
	    sym = find_full_rule(rule_count, dollar_number);
	    token_type = sym->token_type;
	  }
	  if (token_type)
	    fprintf(fp, "(yypvt[%d].%s)", dollar_number - RHS_count, token_type);
	  else
	    fprintf(fp, "yypvt[%d]", dollar_number - RHS_count);

          putc(c, fp);
          READING_NUMBER = FALSE;
          dollar_number = 0;
        } else {
          putc(c, fp);
        }

        if (c == '\"') {
          state = CODE_DOUBLE_QUOTE;
        } else if (c == '\'') {
          state = CODE_SINGLE_QUOTE;
        } else if (c == '*' && last_c == '/') {
          state = CODE_COMMENT;
        } else if (c == '/' && last_c == '/') {
          state = CODE_COMMENT2;
        } else if (c == '}' && CODE_LEVEL == 1) {
          fprintf(fp, " "); // print_break();
          state = RHS;
          //!!
          END_OF_CODE = TRUE; // end of a section of code.

        } else if (c == '{') {
          CODE_LEVEL ++;
        } else if (c == '}') {
          CODE_LEVEL --;
        } else {
          // do nothing.
        }
        break;
      case CODE_DOUBLE_QUOTE:
        putc(c, fp);
        if (c == '\"' && last_c != '\\') state = CODE;
        break;
      case CODE_SINGLE_QUOTE:
        putc(c, fp);
        if (c == '\'') state = CODE;
        break;
      case CODE_COMMENT:
        putc(c, fp);
        if (c == '/' && last_c == '*') state = CODE;
        break;
      case CODE_COMMENT2:
        putc(c, fp);
        if (c == '\n') state = CODE;
        break;
      case COMMENT: 
        if (last_c == '*' && c =='/') state = RHS;
        break;
      case COMMENT2:
        if (last_c == '\n') state = RHS;
        break;
      default:
        break;
    } // end switch.

    //putc(c, stdout);
    last_last_c = last_c;
    last_c = c;

    n_col ++;
    if (c == '\n') { n_line ++; n_col = 1; }
  }
}


void processYaccFile_section3()
{
  int c;
  while((c = getc(fp_yacc)) != EOF) {
    putc(c, fp);
  }
}


/*
 * Copy code from resource files into output file.
 */
void copy_src_file(char * filename)
{
  int c;
  FILE * fp_src;
  if ((fp_src = fopen(filename, "r")) == NULL) {
    printf("error: can't open file %s\n", filename);
    exit(1);
  }
  while((c = getc(fp_src)) != EOF) putc(c, fp);
  fclose(fp_src);
}


/*
 * This function will return the position of $A 
 * to the end of yaccpar.
 * This is for the purpose of inserting code 
 * associated with reductions.
 */
void copy_yaccpar_file_1(char * filename)
{
  int c, last_c = 0;
  FILE * fp_src;
  if ((fp_src = fopen(filename, "r")) == NULL) {
    printf("error: can't open file %s\n", filename);
    exit(1);
  }

  while((c = getc(fp_src)) != EOF) {
    if ((last_c == '$' && c == 'A')) break;
    putc(c, fp);
    last_c = c;
  }
  fseek(fp, -1L, SEEK_CUR); // write head reverse 1 byte to remove '$'.
  fclose(fp_src);
}


void copy_yaccpar_file_2(char * filename)
{
  int c, last_c = 0;
  FILE * fp_src;
  if ((fp_src = fopen(filename, "r")) == NULL) {
    printf("error: can't open file %s\n", filename);
    exit(1);
  }

  while((c = getc(fp_src)) != EOF) {
    if (last_c == '$' && c == 'A') break;
    last_c = c;
  }
  while((c = getc(fp_src)) != EOF) putc(c, fp);

  fclose(fp_src);
}


///////////////////////////////////////////////////////
// Functions to print parsing table arrays. START.
///////////////////////////////////////////////////////

static char * indent = "    "; // indentation.


int getIndexInTokensArray(SymbolTblNode * s)
{
  SymbolNode * a;
  int i;
  for (a = tokens, i = 0; a != NULL; a = a->next, i ++) {
    if (s == a->snode) return i;
  }
  return -1;
}


/*
 * Returns the index of the given symbol in the
 * non-terminal array of the given grammar.
 * Used in gen_compiler.c.
 */
int getNonTerminalIndex(SymbolTblNode * snode)
{
  SymbolNode * a;
  int i = 0;

  for (a = grammar.non_terminal_list; a != NULL; a = a->next) {
    if (snode == a->snode) return i;
    i ++;
  }
  //printf("getNonTerminalIndex warning: ");
  //printf("%s not found!\n", symbol);
  return -1;
}


/*
 * yyr1[i] represents index of non-terminal symbol
 * on the LHS of reduction i, or a terminal symbol
 * if use unit-production-removal and in step 3
 * the LHS are replaced with leaf terminals in the 
 * multi-rooted trees.
 */
void print_yyr1()
{
  int i;
  fprintf(fp, "static YYCONST yytabelem yyr1[] = {\n");
  // First rule is always "$accept : ...".
  fprintf(fp, "%6d,", 0);
 
  if (USE_REMOVE_UNIT_PRODUCTION) {
    int index;
    for (i = 1; i < grammar.rule_count; i ++) {
      //printf("rule %d lhs: %s\n", i, grammar.rules[i]->LHS);
      index = grammar.rules[i]->nLHS->snode->value;
      fprintf(fp, "%6d", index);
      if (i < grammar.rule_count - 1) fprintf(fp, ",");
      if ((i - 9) % 10 == 0) fprintf(fp, "\n");
    }
    fprintf(fp, "};\n");
    return;
  }

  for (i = 1; i < grammar.rule_count; i ++) {
    fprintf(fp, "%6d", (-1) * getNonTerminalIndex(
                        grammar.rules[i]->nLHS->snode));
    if (i < grammar.rule_count - 1) fprintf(fp, ",");
    if ((i - 9) % 10 == 0) fprintf(fp, "\n");
  }

  fprintf(fp, "};\n");
}


/* 
 * yyr2[0] is a dummy field, and is alwasy 0.
 * for i >= 1, yyr2[i] defines the following for grammar rule i:
 *   let x be the number of symbols on the RHS of rule i,
 *   let y indicate whether there is any code associated 
 *     with this rule (y = 1 for yes, y = 0 for no).
 *   then yyr2[i] = (x << 1) + y;
 */
void print_yyr2()
{
  int i;
  fprintf(fp, "static YYCONST yytabelem yyr2[] = {\n");
  fprintf(fp, "%6d,", 0);
  for (i = 1; i < grammar.rule_count; i ++) {
    fprintf(fp, "%6d", (grammar.rules[i]->RHS_count << 1) +
                       grammar.rules[i]->hasCode);
    if (i < grammar.rule_count - 1) fprintf(fp, ",");
    if ( (i - 9) % 10 == 0) fprintf(fp, "\n");
  }
  fprintf(fp, "};\n");
}


void print_yynonterminals()
{
  int i;
  SymbolNode * a;
  fprintf(fp, "yytoktype yynts[] = {\n");

  i = 1; // ignore first nonterminal: $accept.
  for (a = grammar.non_terminal_list->next; a != NULL; a = a->next) {
    fprintf(fp, "\t\"%s\",\t-%d,\n", a->snode->symbol, i);
    i ++;
  }
  fprintf(fp, "\t\"-unknown-\", 1  /* ends search */\n");
  fprintf(fp, "};\n");
}


/*
 * Print terminal tokens.
 */
void print_yytoks()
{
  int i, index;
  SymbolNode * a;

  index = 0;

  fprintf(fp, "yytoktype yytoks[] = {\n");
  for (a = tokens, i = 0; a != NULL; a = a->next, i ++) {
    if (strcmp(a->snode->symbol, strError) == 0) continue;

    if (strlen(a->snode->symbol) == 2 && 
        a->snode->symbol[0] == '\\') { // escape sequence
      fprintf(fp, "\t\"\\\\%s\",\t%d,\n", a->snode->symbol,
                                        a->snode->value);
    } else {
      fprintf(fp, "\t\"%s\",\t%d,\n", a->snode->symbol, 
                                      a->snode->value);
    }
  }

  fprintf(fp, "\t\"-unknown-\", -1  /* ends search */\n");
  fprintf(fp, "};\n");
}


/*
 * Print reductions.
 */
void print_yyreds()
{
  int i, j;
  char * symbol;
  SymbolNode * a;

  fprintf(fp, "char * yyreds[] = {\n");
  fprintf(fp, "\t\"-no such reduction-\"\n");
  for (i = 1; i < grammar.rule_count; i ++) {
    fprintf(fp, "\t\"%s : ", grammar.rules[i]->nLHS->snode->symbol); 
  
    a = grammar.rules[i]->nRHS_head;
    for (j = 0; j < grammar.rules[i]->RHS_count; j ++) {
      if (j > 0) fprintf(fp, " ");

      if (j > 0) a = a->next;
      symbol = a->snode->symbol;

      if (strlen(symbol) == 1 ||
          (strlen(symbol) == 2 && symbol[0] == '\\') ) {
        fprintf(fp, "'%s'", symbol);
      } else {
        fprintf(fp, "%s", symbol);
      }
    }
    fprintf(fp, "\", \n");
  }
  fprintf(fp, "};\n");
}


void print_yytoken()
{
  int i;
  SymbolNode * a;

  fprintf(fp, "int yytoken[] = {\n");
  for (a = tokens, i = 0; a != NULL; a = a->next, i ++) {
    fprintf(fp, "\t\"%s\",\t%d,\n", a->snode->symbol, 257 + i);
  }

  fprintf(fp, "\t\"-unknown-\", -1  /* ends search */\n");
  fprintf(fp, "};\n");
}


void print_parsing_tbl_entry(char action, int state_no, int * count)
{
  BOOL isEntry = FALSE;
  if (action == 's' || action == 'g') {
    fprintf(fp, "%d, ", state_no); // state to go.
    isEntry = TRUE;
  } else if (action == 'r') {
    fprintf(fp, "-%d, ", state_no); // no. of reduction.
    isEntry = TRUE;
  } else if (action == 'a') {
    fprintf(fp, "0, ");
    isEntry = TRUE;
  }

  if (isEntry) {
    (* count) ++;
    if ( (* count) % 10 == 0 && (* count) != 0) fprintf(fp, "\n");
  }
}


/*
 * For the actions in a parsing table.
 * if an action yyptblact[i] is positive, it's a shift/goto;
 * if it is negative, it's a reduce;
 * if it's zero, it's accept.
 */
void print_parsing_tbl()
{
  int i, j, row, col;
  int col_size = ParsingTblCols;
  char action;
  int state_no;
  int * rowoffset = (int *) malloc(sizeof(int) * ParsingTblRows);
  int rowoffset_pt = 0;
  int count = 0;
  SymbolTblNode * n;

  fprintf(fp, "static YYCONST yytabelem yyptblact[] = {\n"); 

  if (USE_REMOVE_UNIT_PRODUCTION) {
    i = 0;
    for (row = 0; row < ParsingTblRows; row ++) {
      if (isReachableState(row)) {

#if USE_REM_FINAL_STATE
        if (final_state_list[row] < 0) {
          print_parsing_tbl_entry('s', final_state_list[row], &count);
          * (rowoffset + rowoffset_pt) = count;
          rowoffset_pt ++;
          continue; 
        }
#endif
        for (col = 0; col < ParsingTblCols; col ++) {
          n = ParsingTblColHdr[col];
          if (isGoalSymbol(n) == FALSE && isParentSymbol(n) == FALSE) {
            getAction(n->type, col, row, & action, & state_no);

            if (action == 's' || action == 'g')
              state_no = getActualState(state_no);
            //printf("%c%d\t", action, state_no);
            print_parsing_tbl_entry(action, state_no, &count);
          } // end of if.
        }

        * (rowoffset + rowoffset_pt) = count;
        rowoffset_pt ++;
        //printf("\n");
      } // end of if.
    }
  } else {
    for (i = 0; i < ParsingTblRows; i ++) {

#if USE_REM_FINAL_STATE
    if (final_state_list[i] < 0) {
      print_parsing_tbl_entry('s', final_state_list[i], &count);
      * (rowoffset + rowoffset_pt) = count;
      rowoffset_pt ++;
      continue;
    }
#endif

      for (j = 0; j < ParsingTblCols; j ++) {
        getAction(ParsingTblColHdr[j]->type, 
                  j, i, & action, & state_no);
        //printf("%c%d, ", action, state_no);
        print_parsing_tbl_entry(action, state_no, &count);
      }

      * (rowoffset + rowoffset_pt) = count;
      rowoffset_pt ++;
      //printf("\n");
    } // end of for.
  }

  fprintf(fp, "-10000000};\n\n"); // -10000000 is space filler

  fprintf(fp, "static YYCONST yytabelem yyrowoffset[] = {\n0, ");
  for (i = 0; i < rowoffset_pt; i ++) {
    fprintf(fp, "%d", *(rowoffset + i));
    if (i < rowoffset_pt - 1) fprintf(fp, ", ");
    if (i % 10 == 0 && i != 0) fprintf(fp, "\n");
  }
  fprintf(fp, "};\n\n"); // NOTE: the last entry is (yyptbl.size - 1).

}


void print_parsing_tbl_col_entry(
       char action, int token_value, int * count)
{
  BOOL isEntry = FALSE;
  if (action == 's' || action == 'g' || action == 'r' || action == 'a') {
    fprintf(fp, "%d, ", token_value); // state to go.
    isEntry = TRUE;
  }

  if (isEntry) {
    (* count) ++;
    if ( (* count) % 10 == 0 && (* count) != 0) fprintf(fp, "\n");
  }
}


/*
 * For the tokens upon which action are taken.
 * If it's between 0 - 255, it's an ascii char;
 * if it's 256, it's 'error'; 
 * if it's > 256, it's a token;
 * if it's < 0, it's a non-terminal.
 */
void print_parsing_tbl_col()
{
  int i, j, row, col, count = 0;
  int col_size = ParsingTblCols;
  char action;
  int state;
  SymbolTblNode * n;

  fprintf(fp, "static YYCONST yytabelem yyptbltok[] = {\n"); 

  if (USE_REMOVE_UNIT_PRODUCTION) {
    i = 0;
    for (row = 0; row < ParsingTblRows; row ++) {
      if (isReachableState(row)) {

#if USE_REM_FINAL_STATE
      if (final_state_list[row] < 0) {
        print_parsing_tbl_col_entry('r', -10000001, &count);
        continue;
      }
#endif
        for (col = 0; col < ParsingTblCols; col ++) {
          n = ParsingTblColHdr[col];
          if (isGoalSymbol(n) == FALSE && isParentSymbol(n) == FALSE) {
            getAction(n->type, col, row, & action, & state);
            print_parsing_tbl_col_entry(action, n->value, &count);
          } // end of if.
        } // end of for.
      } // end of if.
    }
  } else {
    for (i = 0; i < ParsingTblRows; i ++) {

#if USE_REM_FINAL_STATE
      if (final_state_list[i] < 0) { // is a final state.
        // -10000001 labels a final state's col entry
        print_parsing_tbl_col_entry('r', -10000001, &count);
        continue;
      }
#endif
      for (j = 0; j < ParsingTblCols; j ++) {
        n = ParsingTblColHdr[j];
        getAction(n->type, j, i, & action, & state);
        print_parsing_tbl_col_entry(action, n->value, &count);
      }
    } // end of for.
  }

  fprintf(fp, "-10000000};\n\n"); // -10000000 is space filler
}


/*
 * Find those states that only have a single reduce action.
 * Refer: Pager July, 72', Tech Rpt PE 259. Measure 3.
 */
void getFinalStates()
{
  int i, j;
  fprintf(fp, "static YYCONST yytabelem yyfs[] = {\n");
  fprintf(fp, "%d", final_state_list[0]);
  j = 0;
  for (i = 1; i < ParsingTblRows; i ++) {
    if (USE_REMOVE_UNIT_PRODUCTION) {
      if (isReachableState(i) == FALSE) continue;
    }

    fprintf(fp, ", ");
    if ((++ j) % 10 == 0) fprintf(fp, "\n");
    fprintf(fp, "%d", final_state_list[i]);
  }
  fprintf(fp, "};\n\n");
}


BOOL useLRK()
{
  return USE_LR_K == TRUE &&
         (lrk_pt_array != NULL && lrk_pt_array->max_k >= 2);
}


void write_LRK_table_arrays()
{
  int i, j;
  LRk_P_T * t;
  LRk_P_T_row * r;

  fprintf(fp, "/*\n * For LR(k) parsing tables.\n */\n");

  // yy_lrk_k.
  fprintf(fp, "\n/* Max K in LR(k). */\n");
  fprintf(fp, "static YYCONST yytabelem yy_lrk_k = %d;\n", 
          lrk_pt_array->max_k);
  
  // yy_lrk_rows[].
  fprintf(fp, "\n/* Number of rows in each LR(k) parsing table. */\n");
  fprintf(fp, "static YYCONST yytabelem yy_lrk_rows[] = {");
  for (i = 2; i <= lrk_pt_array->max_k; i ++) {
    //printf("write LRK table arrays: i = %d\n", i);
    if (i > 2) fprintf(fp, ", ");
    fprintf(fp, "%d", lrk_pt_array->array[i - 2]->row_count);
  }
  fprintf(fp, "};\n");

  // yy_lrk_cols
  fprintf(fp, "\n/* yyPTC_count + 2 */\n");
  fprintf(fp, "static YYCONST yytabelem yy_lrk_cols = %d;\n", 
          ParsingTblCols + 2);

  // yy_lrk_r[].
  fprintf(fp, "\n/* Values in each LR(k) parsing table. */\n");
  fprintf(fp, "static YYCONST yytabelem yy_lrk_r[] = {\n");
  for (i = 2; i <= lrk_pt_array->max_k; i ++) {
    t = lrk_pt_array->array[i - 2];
    for (r = t->rows; r != NULL; r = r->next) {
      fprintf(fp, "  %d, %d, ", r->state, r->token->snode->value);
      for (j = 0; j < ParsingTblCols; j ++) {
        if (r->row[j] != NULL) {
          if (r->row[j]->end == (Configuration *) CONST_CONFLICT_SYMBOL) {
            fprintf(fp, "%d, %d, ", j, -2);
          } else {
            fprintf(fp, "%d, %d, ", j, r->row[j]->end->ruleID);
          }
        }
      }
      if (i == lrk_pt_array->max_k && r->next == NULL) {
        fprintf(fp, "-1");
      } else {
        fprintf(fp, "-1, ");
      }
      fprintf(fp, "\n");
    }
    if (i < lrk_pt_array->max_k) fprintf(fp, "\n");
  }
  fprintf(fp, "};\n");

  // CONST_ACC.
  fprintf(fp, "\n#define CONST_ACC -10000000 ");
  fprintf(fp, "/* for ACC in parsing table. */\n");

  // yyPTC[].
  fprintf(fp, "\n/* Values of parsing table column tokens. */\n");
  fprintf(fp, "static YYCONST yytabelem yyPTC[] = {\n");
  for (i = 0; i < ParsingTblCols; i ++) {
    if (i > 0) fprintf(fp, ", ");
    if (i % 10 == 0) {
      if (i > 0) fprintf(fp, "\n");
      fprintf(fp, "  ");
    }
    if (strcmp("$accept", ParsingTblColHdr[i]->symbol) == 0) {
      fprintf(fp, "CONST_ACC");
    } else if (strcmp("$end", ParsingTblColHdr[i]->symbol) == 0) {
      fprintf(fp, "%d", 0);
    } else {
      fprintf(fp, "%d", ParsingTblColHdr[i]->value);
    }
  }
  fprintf(fp, "\n};\n");

  fprintf(fp, "\n\n");
}


/*
 * write the generated parsing table into the arrays
 * used by the driver code.
 */
void write_parsing_table_arrays()
{
  getFinalStates();

  print_parsing_tbl_col(); // yytbltok[]
  print_parsing_tbl();     // yytblact[], yyrowoffset[]

  print_yyr1(); // yyr1[]
  print_yyr2(); // yyr2[]

  if (useLRK() == FALSE) {
    fprintf(fp, "\n#ifdef YYDEBUG\n\n");
    fprintf(fp, "typedef struct {char *t_name; int t_val;} yytoktype;\n\n");
    print_yynonterminals(); // yynts[]. nonterminals.

    print_yytoks(); // yytoks[]. tokens.
    print_yyreds(); // yyreds[]. Productions of grammar.
    fprintf(fp, "#endif /* YYDEBUG */\n\n");

  } else { // use LR(k).
    fprintf(fp, "typedef struct {char *t_name; int t_val;} yytoktype;\n\n");
    print_yynonterminals(); // yynts[]. nonterminals.
    print_yytoks(); // yytoks[]. tokens.

    fprintf(fp, "\n#ifdef YYDEBUG\n\n");
    print_yyreds(); // yyreds[]. Productions of grammar.
    fprintf(fp, "#endif /* YYDEBUG */\n\n");

    write_LRK_table_arrays();
  }
}


///////////////////////////////////////////////////////
// Functions to print parsing table arrays. END.
///////////////////////////////////////////////////////


void write_special_info()
{
  fprintf(fp, "\nYYSTYPE yylval;\n");
  if (USE_YYDEBUG) { fprintf(fp, "\n#define YYDEBUG 1\n"); }
}


/*
 * Do this is use LR(k).
 */
void get_LRK_HYACC_PATH()
{
  if (useLRK()) {
    char * tmp;
    puts("lrk used");
    HYY_NEW(tmp, char, strlen(HYACC_PATH) + 2);
    strcpy(tmp, HYACC_PATH);
    strcat(tmp, "k"); // hyaccpark
    HYACC_PATH = tmp;

    printf("LR(k) HYACC_PATH: %s\n", HYACC_PATH);
  }
}


void generate_compiler(char * infile)
{
  if ((fp_yacc = fopen(infile, "r")) == NULL) {
    printf("error: can't open file %s\n", infile);
    exit(1);
  }

  prepare_outfile(); // open output compiler file.

  if (USE_LINES) 
    fprintf(fp, "\n# line 1 \"%s\"\n", infile); 
  processYaccFile_section1(); // declaration section.

  write_special_info();

  goto_section3();

  if (USE_LINES) 
    fprintf(fp, "\n# line %d \"%s\"\n", n_line, infile); 

  processYaccFile_section3(); // code section.

  fprintf(fp, "\n#define YYCONST const\n");
  fprintf(fp, "typedef int yytabelem;\n\n");
  write_parsing_table_arrays();

  get_LRK_HYACC_PATH(); /* do this if LR(k) is used */

  copy_yaccpar_file_1(HYACC_PATH);
  goto_section2();
  processYaccFile_section2(infile); // get reduction code.
  copy_yaccpar_file_2(HYACC_PATH);

  freeSymbolNodeList(tokens);

  fclose(fp);
  if (USE_HEADER_FILE) fclose(fp_h);
  fclose(fp_yacc);

}
