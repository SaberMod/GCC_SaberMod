/* { dg-do compile } */
/* { dg-require-effective-target mpx } */
/* { dg-options "-fcheck-pointer-bounds -mmpx -O2 -fdump-tree-strlen" } */
/* { dg-final { scan-tree-dump "memcpy.chkp" "strlen" } } */
/* { dg-final { cleanup-tree-dump "strlen" } } */

#include "string.h"

char *test (char *str1, char *str2)
{
  size_t len = strlen (str2);
  strcpy (str1, str2);
  return str1 + len;
}
