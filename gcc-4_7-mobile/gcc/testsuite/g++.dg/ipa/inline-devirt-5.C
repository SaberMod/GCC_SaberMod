/* Verify that the inliner makes good decisions and the example
   is optimized to 4 printf()s in main().  */
// { dg-do compile }
// { dg-options "-O2 -fdump-tree-optimized"  }

#include <stdio.h>

typedef unsigned char(*Calculable)(void);
typedef Calculable(*CalculateStrategy)(void);

unsigned char one() { return 1; }
Calculable oneStrategy() { return one; }
unsigned char two() { return 2; }
Calculable twoStrategy() { return two; }

int main()
{
	printf("%d\n", oneStrategy()());
	printf("+1: %d\n", oneStrategy()() + 1);
	printf("%d\n", twoStrategy()());
	printf("+1: %d\n", twoStrategy()() + 1);
	return 0;
}

// { dg-final { scan-tree-dump "printf \\(\"%d\\\\n\", 1\\);" "optimized" } }
// { dg-final { scan-tree-dump "printf \\(\"\\+1: %d\\\\n\", 2\\);" "optimized" } }
// { dg-final { scan-tree-dump "printf \\(\"%d\\\\n\", 2\\);" "optimized" } }
// { dg-final { scan-tree-dump "printf \\(\"\\+1: %d\\\\n\", 3\\);" "optimized" } }
// { dg-final { cleanup-tree-dump "optimized" } }
