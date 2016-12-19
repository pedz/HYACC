/* 
   This file is part of Hyacc, a LR(0)/LALR(1)/LR(1)/LR(k) parser generator.
   Copyright (C) 2007 Xin Chen. chenx@hawaii.edu

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
 * inst.c
 *
 * Used by makefile when calling 'make install'.
 * This will generate a new hyacc_path.c file given the
 * installation path, which is passed to main() as a 
 * command line argument by makefile.
 *
 * @compile: gcc inst.c -o inst
 *
 * @Author: Xin Chen
 * @Created on: Feb 10, 2007
 * @Last updated: Feb 10, 2007
 */

#include <stdio.h>
#include <stdlib.h> /* system, exit. */

static char * s1 = 
"/*\n\
 * Generated by inst.c \n\
 * This file is used to retrieve files hyaccpar and hyaccmanpage.\n\
 * This file is updated each time hyacc is installed.\n\
 */\n\n\
#include \"y.h\"\n\n\
void show_manpage()\n\
{\n\
  system(\"less ";

static char * s2 = 
"/lib/hyacc/hyaccmanpage\");\n\
}\n\n\
char * HYACC_PATH = \n\
\"";

static char * s3 = "/lib/hyacc/hyaccpar\";\n\n";

int main(int argc, char ** argv) {
  FILE * fp;
  char * filename;

  if (argc == 1) return 0;

  filename = "hyacc_path.tmp";
  if ((fp = fopen(filename, "w")) == NULL) {
    printf("inst.c: cannot open file %s\n", filename);
    exit(0);
  }

  fprintf(fp, "%s%s%s%s%s", s1, argv[1], s2, argv[1], s3);
  fclose(fp);

  system("mv -f hyacc_path.tmp hyacc_path.c");

  return 0;
}

